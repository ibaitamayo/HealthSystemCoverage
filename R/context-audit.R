# Context metadata is resolved once per profile, never by translated literals.
# Multiple root memberships remain unknown rather than resolved by CASE order.
resolve_visit_hierarchy <- function(edges) {
  empty<-data.frame(concept_id=numeric(),setting=character(),root_ids=character(),ambiguous=logical())
  if(!nrow(edges))return(empty)
  roots<-c(`9201`="inpatient",`9202`="outpatient_unspecified",`9203`="emergency")
  groups<-split(edges$ancestor_concept_id,edges$descendant_concept_id)
  do.call(rbind,lapply(names(groups),function(key) {
    ids<-sort(unique(as.double(groups[[key]])))
    data.frame(concept_id=as.double(key),
      setting=if(length(ids)==1L)unname(roots[as.character(ids)]) else "other_or_unknown",
      root_ids=paste(ids,collapse=";"),ambiguous=length(ids)>1L)
  }))
}

validate_dispensing_metadata <- function(requested,metadata) {
  checked<-metadata[match(requested,as.double(metadata$concept_id)),,drop=FALSE]
  checked$concept_id<-requested
  checked$validation_status<-ifelse(is.na(checked$domain_id),"unresolved",
    ifelse(checked$domain_id!="Type Concept","wrong_domain",ifelse(!is.na(checked$invalid_reason),"invalid","usable")))
  checked
}

prepare_context_config <- function(con,config) {
  if(isTRUE(config$context_prepared))return(config)
  config<-prepare_source_context(con,config)
  s<-config$cdm_schema
  hierarchy<-data.frame(concept_id=numeric(),setting=character(),root_ids=character(),ambiguous=logical())
  available<-table_exists(con,s,"concept")&&table_exists(con,s,"concept_ancestor")
  if(available) {
    c<-qualified_table(con,s,"concept");a<-qualified_table(con,s,"concept_ancestor")
    edges<-DBI::dbGetQuery(con,paste0("SELECT DISTINCT a.ancestor_concept_id,a.descendant_concept_id FROM ",a,
      " a JOIN ",c," d ON d.concept_id=a.descendant_concept_id JOIN ",c,
      " r ON r.concept_id=a.ancestor_concept_id WHERE a.ancestor_concept_id IN (9201,9202,9203)",
      " AND d.domain_id='Visit' AND d.invalid_reason IS NULL AND r.domain_id='Visit' AND r.invalid_reason IS NULL"))
    hierarchy<-resolve_visit_hierarchy(edges)
  }
  attr(config$source_aliases,"visit_hierarchy_map")<-hierarchy
  config$visit_hierarchy_available<-available
  requested<-config$community_dispensing_type_ids
  if(is.null(requested))requested<-c(38000175,38000176)
  config$dispensing_vocabulary_available<-table_exists(con,s,"concept")
  checked<-data.frame(concept_id=numeric(),concept_name=character(),domain_id=character(),invalid_reason=character())
  if(length(requested)&&config$dispensing_vocabulary_available)
    checked<-DBI::dbGetQuery(con,paste0("SELECT concept_id,concept_name,domain_id,invalid_reason FROM ",
      qualified_table(con,s,"concept")," WHERE concept_id IN (",paste(requested,collapse=","),")"))
  checked<-validate_dispensing_metadata(requested,checked)
  config$dispensing_type_evidence<-checked
  config$resolved_dispensing_type_ids<-as.double(checked$concept_id[checked$validation_status=="usable"])
  expired<-if(config$dispensing_vocabulary_available)DBI::dbGetQuery(con,paste0(
    "SELECT concept_id FROM ",qualified_table(con,s,"concept")," WHERE concept_id=32218 AND domain_id='Visit' AND invalid_reason IS NULL")) else data.frame(concept_id=numeric())
  config$resolved_death_discharge_ids<-as.double(expired$concept_id)
  config$context_prepared<-TRUE
  config
}

context_scope_sql <- function(con,config,table,alias,cohort=NULL,person_scope=NULL,filtered=TRUE) {
  date<-event_date_fields()[[table]][1]
  predicate<-"TRUE"
  if(!is.null(person_scope))predicate<-paste0("(",person_scope,")")
  if(!is.null(cohort))predicate<-paste0(predicate," AND EXISTS (SELECT 1 FROM (",cohort_sql(con,cohort),
    ") cw WHERE cw.subject_id=",alias,".person_id AND ",alias,".",date," BETWEEN cw.cohort_start_date AND cw.cohort_end_date)")
  if(filtered&&!is.null(config$event_date_min))predicate<-paste0(predicate," AND ",
    event_date_predicate(con,config,table,alias,table_columns(con,config$cdm_schema,table)))
  predicate
}

alias_winner_case <- function(con,aliases) {
  priority<-c("primary_care_ehr","emergency_care","home_hospitalisation","palliative_care",
    "mental_health","maternal_prenatal","inpatient_hospital","specialist_outpatient")
  clauses<-character()
  for(type in priority)for(i in which(aliases$source_type==type))
    clauses<-c(clauses,paste0("WHEN lower(concat_ws(' ',v.visit_source_value,cs.place_of_service_source_value)) ~ ",
      DBI::dbQuoteString(con,aliases$pattern[i])," THEN ",i))
  if(!length(clauses))return("NULL::integer")
  structured<-structured_visit_clauses(con,aliases)
  # An alias is a winning rule only when all structured rules fail.
  structured<-sub(" THEN .*?$"," THEN NULL::integer",structured)
  paste("CASE",paste(c(structured,clauses),collapse=" "),"ELSE NULL::integer END")
}

probe_context_audit <- function(con,config,cohort=NULL,person_scope=NULL) {
  config<-prepare_context_config(con,config)
  s<-config$cdm_schema;tab<-function(t)qualified_table(con,s,t)
  out<-list(visit_hierarchy=attr(config$source_aliases,"visit_hierarchy_map"),
    dispensing_type_rules=config$dispensing_type_evidence,inventory=config$source_inventory,
    cdm_source=config$source_metadata,source_date_validation=config$source_date_validation)
  have_visits<-all(vapply(c("visit_occurrence","person","care_site","concept"),function(t)table_exists(con,s,t),logical(1)))
  if(have_visits) {
    baseline<-config$source_aliases[FALSE,,drop=FALSE]
    attr(baseline,"visit_concept_map")<-attr(config$source_aliases,"visit_concept_map")
    attr(baseline,"visit_hierarchy_map")<-attr(config$source_aliases,"visit_hierarchy_map")
    joins<-paste0(" LEFT JOIN ",tab("care_site")," cs ON cs.care_site_id=v.care_site_id ")
    reclassified<-DBI::dbGetQuery(con,paste0("SELECT ",visit_setting_case(con,baseline)," AS baseline_setting,",
      visit_setting_case(con,config$source_aliases)," AS configured_setting,",alias_winner_case(con,config$source_aliases),
      " AS alias_index,count(*)::bigint AS records,count(DISTINCT v.person_id)::bigint AS persons FROM ",tab("visit_occurrence"),
      " v JOIN ",tab("person")," p ON p.person_id=v.person_id ",joins," WHERE ",
      context_scope_sql(con,config,"visit_occurrence","v",cohort,person_scope)," GROUP BY 1,2,3"))
    reclassified$records<-as.double(reclassified$records);reclassified$persons<-as.double(reclassified$persons)
    reclassified$reclassified<-reclassified$baseline_setting!=reclassified$configured_setting
    reclassified$alias_pattern<-config$source_aliases$pattern[reclassified$alias_index]
    reclassified$alias_source_type<-config$source_aliases$source_type[reclassified$alias_index]
    out$visit_reclassification<-reclassified
    settings<-sort(unique(c(reclassified$baseline_setting,reclassified$configured_setting)))
    out$visit_setting_comparison<-data.frame(logical_setting=settings,
      baseline_records=vapply(settings,function(x)sum(reclassified$records[reclassified$baseline_setting==x]),0),
      configured_records=vapply(settings,function(x)sum(reclassified$records[reclassified$configured_setting==x]),0))
    distributions<-list(data.frame(source_table="visit_occurrence",logical_setting=reclassified$configured_setting,records=reclassified$records))
    for(t in c("condition_occurrence","drug_exposure","procedure_occurrence"))if(table_exists(con,s,t)) {
      # Same event scopes/filters as the coverage counts, including unlinked rows.
      z<-DBI::dbGetQuery(con,paste0("SELECT CASE WHEN v.visit_occurrence_id IS NULL THEN 'unlinked_or_unknown' ELSE ",
        visit_setting_case(con,config$source_aliases)," END AS logical_setting,count(*)::bigint AS records FROM ",tab(t),
        " d JOIN ",tab("person")," p ON p.person_id=d.person_id LEFT JOIN ",tab("visit_occurrence"),
        " v ON v.visit_occurrence_id=d.visit_occurrence_id AND v.person_id=d.person_id ",joins," WHERE ",
        context_scope_sql(con,config,t,"d",cohort,person_scope)," GROUP BY 1"))
      z$records<-as.double(z$records);z$source_table<-rep(t,nrow(z))
      distributions[[length(distributions)+1L]]<-z[,c("source_table","logical_setting","records")]
    }
    dist<-do.call(rbind,distributions)
    out$setting_distribution<-if(nrow(dist))stats::aggregate(records~source_table+logical_setting,dist,sum) else dist
  }
  if(table_exists(con,s,"drug_exposure")&&table_exists(con,s,"concept")&&
     "drug_type_concept_id"%in%table_columns(con,s,"drug_exposure")) {
    out$drug_types<-DBI::dbGetQuery(con,paste0("SELECT d.drug_type_concept_id,c.concept_name,c.domain_id,c.invalid_reason,",
      "count(*)::bigint AS records,count(DISTINCT d.person_id)::bigint AS persons FROM ",tab("drug_exposure"),
      " d JOIN ",tab("person")," p ON p.person_id=d.person_id LEFT JOIN ",tab("concept"),
      " c ON c.concept_id=d.drug_type_concept_id WHERE ",context_scope_sql(con,config,"drug_exposure","d",cohort,person_scope),
      " GROUP BY 1,2,3,4"))
    out$drug_types$records<-as.double(out$drug_types$records);out$drug_types$persons<-as.double(out$drug_types$persons)
    out$drug_types$community_dispensing_candidate<-out$drug_types$drug_type_concept_id%in%config$resolved_dispensing_type_ids
  }
  # Dates of clinical event starts, not prospective drug ends or birth dates.
  dates<-list()
  for(t in names(event_date_fields()))if(table_exists(con,s,t)) {
    date<-event_date_fields()[[t]][1]
    if(!date%in%table_columns(con,s,t))next
    bound<-if(length(config$database_cutoff)==1L&&!is.na(config$database_cutoff))config$database_cutoff else Sys.Date()
    scope<-context_scope_sql(con,config,t,"d",cohort,person_scope,filtered=FALSE)
    q<-paste0("SELECT max(d.",date,") AS raw_max_event_date,max(d.",date,") FILTER (WHERE d.",date,"<=",
      DBI::dbQuoteString(con,as.character(bound)),"::date) AS max_observed_event_date FROM ",tab(t),
      " d JOIN ",tab("person")," p ON p.person_id=d.person_id WHERE ",scope)
    z<-DBI::dbGetQuery(con,q);z$source_table<-t;dates[[t]]<-z
  }
  if(length(dates))out$event_date_maxima<-do.call(rbind,dates)
  out<-c(out,probe_extended_sources(con,config,cohort))
  out$scope<-if(is.null(cohort))"selected_people_all_event_dates; setting/type distributions after active event filters" else "selected_cohort_event_windows; setting/type distributions after active event filters"
  out
}

context_alerts <- function(audit,config) {
  out<-list()
  small<-function(n)!is.na(n)&&n>0&&n<config$privacy_min_cell
  show_count<-function(n)if(small(n))"[suppressed]" else as.character(n)
  add<-function(rule,component,message,n=NA_real_,fraction=NA_real_) {
    if(small(n))fraction<-NA_real_
    out[[length(out)+1L]]<<-data.frame(rule_id=rule,component_id=component,records=n,fraction=fraction,
      message=message,scope=if(is.null(audit$scope))"selected_profile" else audit$scope)
  }
  r<-audit$visit_reclassification
  setting_components<-c(primary_care="primary_care_encounters",inpatient="inpatient_admissions",emergency="emergency_care",
    primary_care_emergency="primary_care_emergency",emergency_inpatient="emergency_care",
    specialist_outpatient="specialist_outpatient_encounters",home_hospitalisation="home_hospitalisation",palliative="palliative_care",
    mental_health="mental_health",maternal_prenatal="maternal_prenatal_care")
  if(!is.null(r)&&nrow(r))for(setting in intersect(names(setting_components),unique(r$baseline_setting))) {
    before<-sum(r$records[r$baseline_setting==setting]);after<-sum(r$records[r$configured_setting==setting])
    if(before>0&&after==0) {
      patterns<-unique(r$alias_pattern[r$baseline_setting==setting&r$reclassified])
      add("alias_emptied_component",setting_components[[setting]],paste0("Aliases reclassified all ",show_count(before),
        " baseline visit records out of this setting. Winning patterns: ",paste(patterns,collapse="; "),
        ". Zero configured matches do not establish absence."),before)
    }
  }
  d<-audit$setting_distribution
  if(!is.null(d)&&nrow(d))for(t in unique(d$source_table)) {
    z<-d[d$source_table==t,];n<-sum(z$records);unknown<-sum(z$records[z$logical_setting%in%c("other_or_unknown","unlinked_or_unknown","setting_conflict","emergency_unspecified")])
    conflict<-sum(z$records[z$logical_setting=="setting_conflict"])
    if(conflict>0)add("structured_setting_conflict","__PROFILE__",paste0(t,": ",show_count(conflict),
      " records have contradictory coded visit/establishment settings. They are not forced into primary or hospital settings; see setting_distribution.csv."),conflict)
    threshold<-if(is.null(config$unknown_setting_threshold))0.20 else config$unknown_setting_threshold
    if(n>0&&unknown/n>threshold)add("unclassified_setting_fraction","__PROFILE__",
      paste0(t,": ",if(small(unknown)||small(n))"[suppressed]" else signif(100*unknown/n,4),"% lack an unambiguous classified setting (threshold ",100*threshold,
        "%). Reliability of setting-dependent components is capped at low; classified records are not assumed representative of the remainder."),unknown,unknown/n)
  }
  maxima<-audit$event_date_maxima
  if(!is.null(maxima)&&any(!is.na(maxima$max_observed_event_date))) {
    latest<-max(as.Date(maxima$max_observed_event_date),na.rm=TRUE)
    lag<-as.numeric(config$database_cutoff-latest)
    threshold<-if(is.null(config$cutoff_lag_days))183 else config$cutoff_lag_days
    if(identical(config$database_cutoff_source,"Sys.Date() fallback")&&length(lag)==1L&&!is.na(lag)&&lag>threshold)
      add("cutoff_lag","__PROFILE__",paste0("Sys.Date() cutoff ",config$database_cutoff," is ",lag,
        " days after the latest observed event start not beyond the cutoff (",latest,
        "; threshold ",threshold," days). Supply a verified extraction/ETL date. This lag is not evidence of current data capture."))
  }
  if(!isTRUE(config$visit_hierarchy_available))add("visit_hierarchy_unavailable","__PROFILE__",
    "Visit hierarchy unavailable: only exact root concepts, provider mappings and aliases can classify settings.")
  validation<-audit$source_date_validation
  if(!is.null(validation)&&nrow(validation)&&any(validation$status!="consistent_with_observed_starts"))
    add("source_metadata_date_not_validated","__PROFILE__",paste0("Declared source date was not validated: ",paste(unique(validation$status),collapse="; "),
      ". Contradictory source metadata is not used to exclude events; the effective cutoff and its origin are reported separately."))
  structure<-audit$note_structure
  if(!is.null(structure)&&nrow(structure)&&structure$unexpected_columns[1]>0)
    add("unexpected_note_columns","clinical_notes","NOTE contains unexpected columns. Their names/content are not exported or interpreted; malformed import headers require source/ETL review.")
  if(!is.null(audit$drug_types))add("dispensing_scope","community_dispensings",
    "Drug-type provenance was examined. Generic EHR records do not provide the OMOP distinction needed for dispensing analysis. No off-model text search or recovery is attempted. Recognised pharmacy/mail-order types are reported under the declared definition.")
  rules<-audit$dispensing_type_rules
  if(!is.null(rules)&&"validation_status"%in%names(rules)&&any(rules$validation_status!="usable"))
    add("dispensing_rules_not_resolved","community_dispensings",
      "Some requested dispensing types are missing, inactive or outside Type Concept in this instance. Only validated types were queried; see dispensing_type_rules.csv. An unavailable rule is not evidence of absence.")
  if(length(out))do.call(rbind,out) else data.frame(rule_id=character(),component_id=character(),records=numeric(),fraction=numeric(),message=character(),scope=character())
}

apply_context_quality <- function(components,audit,config) {
  if(is.null(audit))return(components)
  alerts<-context_alerts(audit,config)
  if(!nrow(alerts))return(components)
  for(i in seq_len(nrow(components))) {
    id<-components$component_id[i]
    hit<-alerts$component_id%in%c(id,"__PROFILE__")
    if(any(hit))components$issues[i]<-paste(c(components$issues[i][!is.na(components$issues[i])],alerts$message[hit]),collapse=" ")
    if(any(alerts$rule_id=="unclassified_setting_fraction")&&id%in%visit_dependent_components()&&
       components$reliability[i]%in%c("high","moderate","low","unusable"))
      components$reliability[i]<-cap_reliability(components$reliability[i],"low")
  }
  components
}

write_context_audit <- function(audit,config,path) {
  if(is.null(audit))return(invisible(NULL))
  dest<-file.path(path,"context_audit");dir.create(dest,showWarnings=FALSE)
  for(n in names(audit))if(is.data.frame(audit[[n]]))utils::write.csv(
    suppress_small_cells(audit[[n]],config$privacy_min_cell),file.path(dest,paste0(n,".csv")),row.names=FALSE,na="")
  utils::write.csv(suppress_small_cells(context_alerts(audit,config),config$privacy_min_cell),file.path(dest,"alerts.csv"),row.names=FALSE,na="")
  dates<-audit$event_date_maxima
  latest<-if(!is.null(dates)&&any(!is.na(dates$max_observed_event_date)))as.character(max(as.Date(dates$max_observed_event_date),na.rm=TRUE)) else NA_character_
  utils::write.csv(data.frame(database_cutoff=if(length(config$database_cutoff))as.character(config$database_cutoff) else NA_character_,
    cutoff_source=config$database_cutoff_source,max_observed_event_date=latest,
    cutoff_lag_days=config$cutoff_lag_days,unknown_setting_threshold=config$unknown_setting_threshold,
    scope=if(is.null(audit$scope))"selected_profile" else audit$scope),file.path(dest,"parameters.csv"),row.names=FALSE,na="")
}

context_audit_html <- function(audit,config) {
  if(is.null(audit))return("")
  alerts<-context_alerts(audit,config)
  messages<-if(nrow(alerts))paste0("<li>",html_escape(alerts$message),"</li>",collapse="") else "<li>No context alert triggered; this is not a completeness guarantee.</li>"
  dates<-audit$event_date_maxima
  latest<-if(!is.null(dates)&&any(!is.na(dates$max_observed_event_date)))as.character(max(as.Date(dates$max_observed_event_date),na.rm=TRUE)) else "Unknown"
  cutoff<-if(length(config$database_cutoff)==1L)as.character(config$database_cutoff) else "Not set (date-max audit uses today's date only)"
  tables<-vapply(intersect(c("cdm_source","source_date_validation","inventory","visit_setting_comparison","type_distributions",
    "discharge_destinations","preceding_visit_links","mortality_episode_evidence","cost_layer","provider_specialties","location_coverage","note_structure"),names(audit)),function(n) {
    d<-audit[[n]];if(!is.data.frame(d)||!nrow(d))return("")
    d<-suppress_small_cells(d,config$privacy_min_cell)
    rows<-vapply(seq_len(nrow(d)),function(i)paste0("<tr>",paste(vapply(d[i,,drop=FALSE],function(x)paste0("<td>",html_escape(x),"</td>"),""),collapse=""),"</tr>"),"")
    paste0("<details><summary>",html_escape(n),"</summary><div style='overflow:auto'><table><tr>",paste0("<th>",html_escape(names(d)),"</th>",collapse=""),
      "</tr>",paste(rows,collapse=""),"</table></div></details>")
  },"")
  paste0("<section style='margin:24px'><h2>Classification and time context</h2><p>Database cutoff: ",
    html_escape(cutoff)," (",html_escape(config$database_cutoff_source),
    "). Latest observed event start not beyond cutoff: ",html_escape(latest),
    ". Thresholds: unknown setting fraction ",config$unknown_setting_threshold,
    "; cutoff lag ",config$cutoff_lag_days," days. Prospective drug end dates are excluded from the latest-start comparison.</p><ul>",
    messages,"</ul><p>Evidence below and context_audit CSV files distinguish event-scoped counts from instance metadata. Discharge markers are not automatically confirmed deaths; preceding links are not merged episodes. Person-grain findings cannot be summed as records. No source-language outcome is interpreted without an explicit map. These audits do not change person eligibility.</p>",
    paste(tables,collapse=""),"</section>")
}
