mapping_spec <- function() list(
  condition_occurrence=c("condition_concept_id","condition_source_concept_id","condition_source_value","Condition"),
  drug_exposure=c("drug_concept_id","drug_source_concept_id","drug_source_value","Drug"),
  measurement=c("measurement_concept_id","measurement_source_concept_id","measurement_source_value","Measurement"),
  observation=c("observation_concept_id","observation_source_concept_id","observation_source_value","Observation"),
  procedure_occurrence=c("procedure_concept_id","procedure_source_concept_id","procedure_source_value","Procedure"),
  device_exposure=c("device_concept_id","device_source_concept_id","device_source_value","Device"))

mapping_counts <- function() c("mapping_records","valid_standard_records","source_coded_records",
  "source_resolved_records","source_crosswalked_records","assigned_target_crosswalked_records",
  "crosswalk_ambiguous_records","crosswalk_origin_unspecified_records","auditable_records")

quality_fields <- function() c(mapping_counts(),"mapped_fraction","source_coded_fraction",
  "source_resolved_fraction","source_crosswalked_fraction","assigned_target_crosswalked_fraction",
  "auditable_fraction","provenance_reliability","mapping_audit_policy")

# No clinical text inference or any-vocabulary concept-code search. Crosswalk
# entries declare vocabulary; unknown event origin remains explicitly limited.
mapping_event_flags <- function(con,config,table,columns,local_map_available=FALSE) {
  z<-mapping_spec()[[table]]
  defaults<-c(is_mapping_record="FALSE",valid_standard="FALSE",source_coded="FALSE",
    source_resolved="FALSE",source_crosswalked="FALSE",assigned_target_crosswalked="FALSE",
    crosswalk_ambiguous="FALSE",crosswalk_origin_unspecified="FALSE",auditable="FALSE")
  if(is.null(z)||!all(z[1:3]%in%columns))return(defaults)
  target<-paste0("d.",z[1]);source<-paste0("d.",z[2]);value<-paste0("d.",z[3])
  concept<-qualified_table(con,config$cdm_schema,"concept")
  valid<-paste0("EXISTS (SELECT 1 FROM ",concept," mc WHERE mc.concept_id=",target,
    " AND mc.standard_concept='S' AND mc.domain_id=",DBI::dbQuoteString(con,z[4])," AND mc.invalid_reason IS NULL)")
  coded<-paste0("COALESCE(",source,"<>0,false)")
  resolved<-paste0("(",coded," AND EXISTS (SELECT 1 FROM ",concept," sc WHERE sc.concept_id=",source,"))")
  candidate<-"FALSE";local<-"FALSE";ambiguous<-"FALSE";unspecified<-"FALSE"
  if(local_map_available) {
    # Joined one-row-per-code aggregate, never a many-to-many event join.
    candidate<-"(sm.source_code IS NOT NULL)"
    local<-paste0("COALESCE(",target,"=ANY(sm.target_ids),false)")
    ambiguous<-paste0("(",local," AND (sm.target_count>1 OR sm.vocabulary_count>1))")
    origin<-config$source_vocabularies[table]
    if(length(origin)!=1L||is.na(origin))unspecified<-local
  }
  c(is_mapping_record="TRUE",valid_standard=valid,source_coded=coded,
    source_resolved=resolved,source_crosswalked=candidate,assigned_target_crosswalked=local,
    crosswalk_ambiguous=ambiguous,
    crosswalk_origin_unspecified=unspecified,
    auditable=paste0("(",resolved," OR ",local,")"))
}

# Aggregate all flags over exactly the same component/window as the counts.
coverage_aggregate_sql <- function(alias="e") {
  a<-paste0(alias,".");keep<-paste0("NOT ",a,"event_excluded")
  count<-function(predicate,name)paste0("count(*) FILTER (WHERE ",predicate,")::bigint AS ",name)
  map<-c(mapping_records="is_mapping_record",valid_standard_records="valid_standard",
    source_coded_records="source_coded",source_resolved_records="source_resolved",
    source_crosswalked_records="source_crosswalked",assigned_target_crosswalked_records="assigned_target_crosswalked",
    crosswalk_ambiguous_records="crosswalk_ambiguous",crosswalk_origin_unspecified_records="crosswalk_origin_unspecified",auditable_records="auditable")
  fields<-c(paste0("count(DISTINCT ",a,"person_id) FILTER (WHERE ",keep,")::bigint AS persons"),
    count(keep,"records"),paste0("min(",a,"event_date) FILTER (WHERE ",keep,") AS start_date"),
    paste0("max(",a,"event_date) FILTER (WHERE ",keep,") AS end_date"),
    vapply(names(map),function(n)count(paste0(keep," AND ",a,map[[n]]),n),""))
  rules<-c("before_min","after_cutoff","before_birth","end_before_start")
  for(rule in rules)for(action in c("retained","excluded","prior")) {
    previous<-rules[seq_len(match(rule,rules)-1L)]
    prior<-if(length(previous))paste0("(",paste0(a,previous,collapse=" OR "),")") else "FALSE"
    hit<-paste0(a,rule," AND ",if(action=="retained")keep else paste0(a,"event_excluded AND ",if(action=="excluded")"NOT " else "",prior))
    fields<-c(fields,count(hit,paste0(rule,"_",action,"_records")),
      paste0("count(DISTINCT ",a,"person_id) FILTER (WHERE ",hit,")::bigint AS ",rule,"_",action,"_persons"))
  }
  fields<-c(fields,count(paste0(keep," AND ",a,"is_mapping_record AND NOT ",a,"auditable"),"source_unresolved_records"),
    paste0("count(DISTINCT ",a,"person_id) FILTER (WHERE ",keep," AND ",a,"is_mapping_record AND NOT ",a,"auditable)::bigint AS source_unresolved_persons"))
  for(flag in c("crosswalk_ambiguous","crosswalk_origin_unspecified"))fields<-c(fields,
    paste0("count(DISTINCT ",a,"person_id) FILTER (WHERE ",keep," AND ",a,flag,")::bigint AS ",flag,"_persons"))
  paste(fields,collapse=", ")
}

mapping_quality <- function(z,config) {
  policy<-if(is.null(config$mapping_audit_policy))"require_local" else config$mapping_audit_policy
  result<-list(mapped_fraction=NA_real_,source_coded_fraction=NA_real_,source_resolved_fraction=NA_real_,
    source_crosswalked_fraction=NA_real_,assigned_target_crosswalked_fraction=NA_real_,
    auditable_fraction=NA_real_,provenance_reliability="unknown",mapping_audit_policy=policy,
    reliability_cap=NA_character_,quality_issue=NA_character_)
  if(!all(mapping_counts()%in%names(z))||anyNA(z[,mapping_counts(),drop=FALSE])||z$mapping_records==0)return(result)
  n<-as.double(z$mapping_records)
  result$mapped_fraction<-as.double(z$valid_standard_records)/n
  result$source_coded_fraction<-as.double(z$source_coded_records)/n
  result$source_resolved_fraction<-as.double(z$source_resolved_records)/n
  result$source_crosswalked_fraction<-as.double(z$source_crosswalked_records)/n
  result$assigned_target_crosswalked_fraction<-as.double(z$assigned_target_crosswalked_records)/n
  result$auditable_fraction<-as.double(z$auditable_records)/n
  missing<-n-as.double(z$auditable_records)
  result$provenance_reliability<-if(missing==0)"moderate" else if(missing==n&&policy=="require_local")"unusable" else "low"
  crosswalk_only<-as.double(z$auditable_records)-as.double(z$source_resolved_records)
  if(missing>0) {
    result$reliability_cap<-result$provenance_reliability
    result$quality_issue<-"Mapping provenance is not fully documented by resolved source concepts or exact assigned targets in a declared-vocabulary crosswalk. This is not proof of invalid clinical semantics. See scoped mapping findings and fractions."
  } else if(crosswalk_only>0) {
    result$provenance_reliability<-if(z$crosswalk_ambiguous_records>0||z$crosswalk_origin_unspecified_records>0)"low" else "moderate"
    result$reliability_cap<-result$provenance_reliability
    result$quality_issue<-"Provenance is documented through a crosswalk. Multiple target/vocabulary candidates may prevent determination from the event alone; no clinical mapping error is inferred. See crosswalk audit."
  }
  result
}

cap_reliability <- function(value,cap) {
  if(is.na(cap))return(value)
  levels<-c("unusable","low","moderate","high")
  if(!value%in%levels)return(cap)
  levels[min(match(value,levels),match(cap,levels))]
}

enrich_coverage_stats <- function(measured,config) {
  if(is.null(measured)||!nrow(measured))return(measured)
  for(i in seq_len(nrow(measured))) {
    q<-mapping_quality(measured[i,,drop=FALSE],config)
    for(nm in names(q))measured[[nm]][i]<-q[[nm]]
  }
  measured
}

# A zero from a setting classifier is not evidence of absent care: aliases and
# unrecognised visit concepts can move or hide the matching records. Explicit
# concept rules replace the setting definition and have their own scoped zero.
visit_dependent_components <- function() c(
  "primary_care_encounters","inpatient_admissions","emergency_care",
  "specialist_outpatient_encounters","home_hospitalisation","palliative_care",
  "mental_health","maternal_prenatal_care","primary_care_diagnoses",
  "inpatient_diagnoses","specialist_outpatient_diagnoses",
  "outpatient_prescriptions","primary_care_procedures","inpatient_procedures",
  "hospital_medications")

zero_coverage_status <- function(node,config,assessed_children=NULL) {
  ids<-node
  member<-coverage_membership()
  if(node%in%c(member$group_id,member$area_id)) {
    ids<-member$component_id[member$group_id==node|member$area_id==node]
    if(!is.null(assessed_children))ids<-intersect(ids,assessed_children)
  }
  explicit<-if(is.null(config$component_rules))character() else config$component_rules$component_id
  if(any(ids%in%setdiff(visit_dependent_components(),explicit)))"unknown" else "not_detected"
}

zero_setting_issue <- function() paste(
  "No records matched the configured visit-setting classification after active filters.",
  "Unrecognised settings or alias reassignment cannot be distinguished from absence;",
  "zero matches do not establish absence of this care component.")

apply_coverage_quality <- function(components,measured,config) {
  if(is.null(measured)||!nrow(measured))return(components)
  for(i in seq_len(nrow(components))) {
    z<-measured[measured$node_id==components$component_id[i],,drop=FALSE]
    if(nrow(z)!=1L)next
    # DBI bigint columns can be bit64::integer64. Subassignment into a numeric
    # column copies their underlying bits unless explicitly converted.
    for(nm in intersect(c("persons","records","start_date","end_date",mapping_counts()),names(z)))
      components<-do.call(set_component,c(list(x=components,
        component_id=components$component_id[i]),stats::setNames(list(z[[nm]][1]),nm)))
    q<-mapping_quality(z,config)
    for(nm in intersect(names(q),quality_fields()))components[[nm]][i]<-q[[nm]]
    if(all(c("mapping_records","valid_standard_records")%in%names(z))&&z$mapping_records>0) {
      # Preserve reviewed semantic findings; missing source provenance alone
      # must never manufacture detected_misclassified.
      if(!components$status[i]%in%c("detected_misclassified","detected_unstructured"))
        components$status[i]<-if(z$valid_standard_records==z$mapping_records)"detected_structured" else "detected_partial"
      base<-if(z$valid_standard_records==z$mapping_records)"moderate" else "low"
      reviewed_ids<-if(is.null(config$adjudications))character() else config$adjudications$component_id
      components$reliability[i]<-if(components$component_id[i]%in%reviewed_ids)cap_reliability(components$reliability[i],base) else base
    }
    if(z$records==0) {
      components$status[i]<-zero_coverage_status(components$component_id[i],config)
      components$reliability[i]<-"unknown"
    }
    temporal<-intersect(c("before_min_retained_records","after_cutoff_retained_records","before_birth_retained_records","end_before_start_retained_records"),names(z))
    retained_defects<-sum(as.double(unlist(z[1,temporal,drop=FALSE],use.names=FALSE)),na.rm=TRUE)
    if(length(temporal)&&retained_defects>0)
      components$reliability[i]<-cap_reliability(components$reliability[i],"low")
    # Recompute the existing date-based assessment for direct visits/deaths;
    # excluded raw anomalies must not masquerade as retained defects.
    direct<-c("primary_care_encounters","inpatient_admissions","emergency_care","specialist_outpatient_encounters",
      "home_hospitalisation","palliative_care","mental_health","maternal_prenatal_care","mortality_all_cause")
    reviewed_ids<-if(is.null(config$adjudications))character() else config$adjudications$component_id
    if(length(temporal)&&components$component_id[i]%in%direct&&!components$component_id[i]%in%reviewed_ids)
      components$reliability[i]<-if(z$records>0)if(retained_defects>0)"low" else "moderate" else "unknown"
    components$reliability[i]<-cap_reliability(components$reliability[i],q$reliability_cap)
    # Historical raw diagnostics must not be appended as if present in these
    # filtered counts. Structured findings carry the actual action and scope.
    reviewed<-config$adjudications
    reviewed_issue<-if(!is.null(reviewed)&&"issues"%in%names(reviewed))reviewed$issues[reviewed$component_id==components$component_id[i]] else character()
    reviewed_issue<-reviewed_issue[!is.na(reviewed_issue)]
    if(!components$component_id[i]%in%c("population_registry","demographics","geographic_assignment","longitudinal_follow_up"))
      components$issues[i]<-paste(c("Counts and quality measures use the selected component/window after active filters; raw diagnostics are separate. Recorded presence does not establish complete capture or service provenance.",
        if(z$records==0&&components$status[i]=="unknown")zero_setting_issue(),
        reviewed_issue,q$quality_issue[!is.na(q$quality_issue)]),collapse=" ")
  }
  components
}

empty_findings <- function() data.frame(component_id=character(),area_id=character(),rule_id=character(),
  scope=character(),action=character(),in_scope=logical(),excluded_by_filter=logical(),
  rule_order=integer(),incremental_exclusion=logical(),records=numeric(),persons=numeric())

coverage_findings <- function(measured,config) {
  out<-list()
  if(is.null(measured)||!nrow(measured))return(empty_findings())
  add<-function(z,rule,action,n,p,order=NA_integer_) {
    if(is.na(n)||n==0)return()
    out[[length(out)+1L]]<<-data.frame(component_id=z$node_id,
      area_id=if("area_id"%in%names(z))z$area_id else "__ALL__",rule_id=rule,
      scope=if(is.null(config$analysis_population))"component_definition_all_persons" else "component_definition_and_selected_cohort_window",action=action,
      in_scope=action=="retained",excluded_by_filter=action%in%c("excluded_by_filter","excluded_by_prior_rule"),
      rule_order=order,incremental_exclusion=action=="excluded_by_filter",
      records=as.double(n),persons=as.double(p))
  }
  for(i in seq_len(nrow(measured))) {
    z<-measured[i,,drop=FALSE]
    rules<-c("before_min","after_cutoff","before_birth","end_before_start")
    for(rule in rules)for(action in c("retained","excluded","prior")) {
      n<-paste0(rule,"_",action,"_records");p<-paste0(rule,"_",action,"_persons")
      if(all(c(n,p)%in%names(z)))add(z,
        if(rule=="before_birth")paste0(rule,"_",if(is.null(config$birth_date_rule))"year" else config$birth_date_rule) else rule,
        if(action=="excluded")"excluded_by_filter" else if(action=="prior")"excluded_by_prior_rule" else "retained",z[[n]],z[[p]],match(rule,rules))
    }
    if(all(c("source_unresolved_records","source_unresolved_persons")%in%names(z)))
      add(z,"mapping_source_not_auditable","retained",z$source_unresolved_records,z$source_unresolved_persons)
    for(flag in c("crosswalk_ambiguous","crosswalk_origin_unspecified")) {
      n<-paste0(flag,"_records");p<-paste0(flag,"_persons")
      if(all(c(n,p)%in%names(z)))add(z,paste0("mapping_",flag),"retained",z[[n]],z[[p]])
    }
  }
  if(length(out))do.call(rbind,out) else empty_findings()
}

findings_html <- function(findings,privacy_min_cell) {
  if(is.null(findings)||!nrow(findings))return("")
  f<-suppress_small_cells(findings,privacy_min_cell)
  headers<-names(f)
  rows<-vapply(seq_len(nrow(f)),function(i)paste0("<tr>",paste(vapply(f[i,,drop=FALSE],function(v)paste0("<td>",html_escape(v),"</td>"),""),collapse=""),"</tr>"),"")
  paste0("<section style='margin:24px;overflow:auto'><h2>Scoped quality findings</h2><p>Reasons and people may overlap. Exclusion order: lower bound, upper bound, birth. Reversed intervals are diagnosed, not independently excluded by this rule. Only incremental_exclusion=true rows count additional exclusions; excluded_by_prior_rule documents overlaps and must not be summed again. Excluded findings are outside the filtered counts. Small cells are suppressed.</p><table><thead><tr>",
    paste0("<th>",html_escape(headers),"</th>",collapse=""),"</tr></thead><tbody>",paste(rows,collapse=""),"</tbody></table></section>")
}

mapping_provenance_html <- function(components,config) {
  if(is.null(components)||!nrow(components)||!"mapping_records"%in%names(components))return("")
  d<-components[!is.na(components$mapping_records)&components$mapping_records>0,,drop=FALSE]
  if(!nrow(d))return("")
  d<-suppress_small_cells(d,config$privacy_min_cell)
  columns<-intersect(c("component_id","records","mapping_records","mapped_fraction","source_coded_fraction","source_resolved_fraction",
    "source_crosswalked_fraction","assigned_target_crosswalked_fraction","auditable_fraction","provenance_reliability","reliability","mapping_audit_policy"),names(d))
  d<-d[,columns,drop=FALSE]
  rows<-vapply(seq_len(nrow(d)),function(i)paste0("<tr>",paste(vapply(d[i,,drop=FALSE],function(v)paste0("<td>",html_escape(v),"</td>"),""),collapse=""),"</tr>"),"")
  paste0("<section style='margin:24px;overflow:auto'><h2>Component mapping provenance</h2><p>Fractions use mapping_records, after active event filters, not the population denominator. A source code present in a crosswalk is different from the assigned target being documented there. Candidate entries do not prove clinical validity; source concepts may be absent legitimately. Policy: ",html_escape(config$mapping_audit_policy),".</p><table><tr>",
    paste0("<th>",html_escape(names(d)),"</th>",collapse=""),"</tr>",paste(rows,collapse=""),"</table></section>")
}
