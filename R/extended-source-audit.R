probe_type_distributions <- function(con,config,cohort=NULL) {
  fields<-c(visit_occurrence="visit_type_concept_id",condition_occurrence="condition_type_concept_id",
    drug_exposure="drug_type_concept_id",measurement="measurement_type_concept_id",observation="observation_type_concept_id",
    procedure_occurrence="procedure_type_concept_id",note="note_type_concept_id",death="death_type_concept_id",
    device_exposure="device_type_concept_id",specimen="specimen_type_concept_id")
  out<-list();s<-config$cdm_schema
  for(t in names(fields)) {
    if(!table_exists(con,s,t)||!fields[[t]]%in%table_columns(con,s,t))next
    q<-paste0("SELECT d.",fields[[t]]," AS type_concept_id,c.concept_name,c.domain_id,c.invalid_reason,count(*)::bigint AS records FROM ",
      qualified_table(con,s,t)," d JOIN ",qualified_table(con,s,"person")," p ON p.person_id=d.person_id LEFT JOIN ",
      qualified_table(con,s,"concept")," c ON c.concept_id=d.",fields[[t]]," WHERE ",
      context_scope_sql(con,config,t,"d",cohort)," GROUP BY 1,2,3,4")
    z<-DBI::dbGetQuery(con,q);z$records<-as.double(z$records);z$source_table<-rep(t,nrow(z))
    z$scope<-rep(if(is.null(cohort))"all_persons_after_active_event_filters" else "selected_cohort_windows_after_active_event_filters",nrow(z));out[[t]]<-z
  }
  if(length(out))do.call(rbind,out) else data.frame()
}

death_discharge_grain_case <- function(con,config,v="v") {
  clauses<-character()
  # Coded UB04 Expired outcome, validated in the instance before use; no source
  # string is interpreted without a provider-declared map.
  map<-config$discharge_outcome_map
  if(!is.null(map))for(i in seq_len(nrow(map)))clauses<-c(clauses,paste0("WHEN ",v,".visit_concept_id=",map$visit_concept_id[i],
    " AND ",v,".discharged_to_source_value=",DBI::dbQuoteString(con,map$source_value[i])," THEN ",DBI::dbQuoteString(con,map$grain[i])))
  if(length(config$resolved_death_discharge_ids))clauses<-c(clauses,paste0("WHEN ",v,".discharged_to_concept_id IN (",
    paste(config$resolved_death_discharge_ids,collapse=","),") THEN 'event'"))
  if(!length(clauses))return("NULL::text")
  paste("CASE",paste(clauses,collapse=" "),"ELSE NULL::text END")
}

probe_discharge_evidence <- function(con,config,cohort=NULL) {
  s<-config$cdm_schema;tab<-function(t)qualified_table(con,s,t)
  if(!table_exists(con,s,"visit_occurrence"))return(list())
  cols<-table_columns(con,s,"visit_occurrence")
  if(!"discharged_to_concept_id"%in%cols)return(list())
  join<-paste0(" FROM ",tab("visit_occurrence")," v JOIN ",tab("person")," p ON p.person_id=v.person_id ")
  scope<-context_scope_sql(con,config,"visit_occurrence","v",cohort)
  out<-list(discharge_destinations=DBI::dbGetQuery(con,paste0("SELECT v.visit_concept_id,v.discharged_to_concept_id,c.concept_name,",
    "c.domain_id,count(*)::bigint AS records,count(DISTINCT v.person_id)::bigint AS persons",join,
    " LEFT JOIN ",tab("concept")," c ON c.concept_id=v.discharged_to_concept_id WHERE ",scope," GROUP BY 1,2,3,4")))
  grain<-death_discharge_grain_case(con,config)
  if(table_exists(con,s,"death")&&"discharged_to_source_value"%in%cols) {
    base<-paste0("SELECT v.visit_occurrence_id,v.visit_concept_id,v.person_id,v.visit_start_date,v.visit_end_date,",grain," AS grain,",
      "EXISTS (SELECT 1 FROM ",tab("death")," d WHERE d.person_id=v.person_id) AS has_death,",
      "EXISTS (SELECT 1 FROM ",tab("death")," d WHERE d.person_id=v.person_id AND d.death_date=v.visit_end_date) AS exact_death_date",join,
      " WHERE ",scope," AND (",grain,") IS NOT NULL")
    out$mortality_episode_evidence<-DBI::dbGetQuery(con,paste0("WITH b AS (",base,
      "),summary AS (SELECT visit_concept_id,grain,count(*)::bigint AS source_records,count(DISTINCT visit_occurrence_id)::bigint AS distinct_visit_records,",
      "count(DISTINCT person_id)::bigint AS persons,count(DISTINCT person_id) FILTER (WHERE has_death)::bigint AS persons_with_death,",
      "count(*) FILTER (WHERE exact_death_date)::bigint AS exact_date_records FROM b GROUP BY 1,2),",
      "per_person AS (SELECT visit_concept_id,grain,person_id,count(*) AS n,count(DISTINCT visit_start_date) AS dates FROM b GROUP BY 1,2,3),",
      "repeat_summary AS (SELECT visit_concept_id,grain,max(n)::bigint AS max_records_per_person,max(dates)::bigint AS max_dates_per_person FROM per_person GROUP BY 1,2)",
      " SELECT * FROM summary JOIN repeat_summary USING(visit_concept_id,grain)"))
  }
  if("preceding_visit_occurrence_id"%in%cols) {
    # A link is not a merged episode. Keep identity and temporal integrity visible.
    out$preceding_visit_links<-DBI::dbGetQuery(con,paste0("SELECT v.visit_concept_id,prev.visit_concept_id AS preceding_concept_id,",
      "(prev.visit_occurrence_id IS NOT NULL) AS preceding_exists,COALESCE(prev.person_id=v.person_id,false) AS same_person,",
      "CASE WHEN prev.visit_occurrence_id IS NULL THEN 'missing_previous' WHEN prev.person_id<>v.person_id THEN 'different_person' ",
      "WHEN prev.visit_start_date>v.visit_start_date THEN 'reversed_start_order' WHEN v.visit_start_date-prev.visit_end_date BETWEEN 0 AND 1 THEN 'adjacent_0_1_days' ",
      "WHEN v.visit_start_date<prev.visit_end_date THEN 'overlapping' ELSE 'separated_or_unknown' END AS temporal_relation,",
      "count(*)::bigint AS records,count(DISTINCT v.person_id)::bigint AS persons",join,
      " LEFT JOIN ",tab("visit_occurrence")," prev ON prev.visit_occurrence_id=v.preceding_visit_occurrence_id WHERE ",scope,
      " AND v.preceding_visit_occurrence_id IS NOT NULL GROUP BY 1,2,3,4,5"))
  }
  for(n in names(out)) {
    for(col in names(out[[n]]))if(inherits(out[[n]][[col]],"integer64"))out[[n]][[col]]<-as.double(out[[n]][[col]])
    out[[n]]$scope<-rep(if(is.null(cohort))"all_persons_after_active_event_filters" else "selected_cohort_windows_after_active_event_filters",nrow(out[[n]]))
  }
  out
}

probe_extended_sources <- function(con,config,cohort=NULL) {
  out<-probe_discharge_evidence(con,config,cohort)
  out$type_distributions<-probe_type_distributions(con,config,cohort)
  s<-config$cdm_schema
  if(table_exists(con,s,"provider"))out$provider_specialties<-DBI::dbGetQuery(con,paste0(
    "SELECT pr.specialty_concept_id,c.concept_name,c.domain_id,count(*)::bigint AS providers FROM ",qualified_table(con,s,"provider"),
    " pr LEFT JOIN ",qualified_table(con,s,"concept")," c ON c.concept_id=pr.specialty_concept_id GROUP BY 1,2,3"))
  if(table_exists(con,s,"cost")) {
    fields<-intersect(c("total_cost","total_charge","total_paid","paid_patient_copay","paid_by_patient"),table_columns(con,s,"cost"))
    counts<-vapply(fields,function(f)paste0("count(*) FILTER (WHERE ",qid(con,f)," IS NOT NULL)::bigint AS ",f,"_records"),"")
    out$cost_layer<-DBI::dbGetQuery(con,paste0("SELECT cost_domain_id,count(*)::bigint AS records",
      if(length(counts))paste0(",",paste(counts,collapse=","))," FROM ",qualified_table(con,s,"cost")," GROUP BY 1"))
  }
  if(table_exists(con,s,"location")) {
    fields<-intersect(c("county","city","zip","country_source_value"),table_columns(con,s,"location"))
    if(length(fields))out$location_coverage<-DBI::dbGetQuery(con,paste0("SELECT count(*)::bigint AS locations,",
      paste(vapply(fields,function(f)paste0("count(*) FILTER (WHERE NULLIF(btrim(",qid(con,f),"),'') IS NOT NULL)::bigint AS ",f,"_records"),""),collapse=","),
      " FROM ",qualified_table(con,s,"location")))
  }
  # Do not publish unexpected column names: malformed headers may contain data.
  if(table_exists(con,s,"note")) {
    expected<-c("note_id","person_id","note_date","note_datetime","note_type_concept_id","note_class_concept_id","note_title",
      "note_text","encoding_concept_id","language_concept_id","provider_id","visit_occurrence_id","visit_detail_id","note_source_value","note_event_id","note_event_field_concept_id")
    cols<-table_columns(con,s,"note")
    out$note_structure<-data.frame(expected_columns=length(expected),present_expected_columns=sum(expected%in%cols),unexpected_columns=sum(!cols%in%expected))
  }
  for(n in intersect(c("provider_specialties","cost_layer","location_coverage","note_structure"),names(out)))
    out[[n]]$scope<-rep("instance_metadata_not_cohort_counts",nrow(out[[n]]))
  # Instance metadata tables above are not restricted to the cohort. Event evidence is.
  out
}

context_person_findings <- function(audit,config) {
  z<-audit$mortality_episode_evidence
  if(is.null(z)||!nrow(z))return(empty_findings())
  out<-empty_findings()
  for(i in seq_len(nrow(z)))out<-rbind(out,data.frame(component_id="mortality_all_cause",area_id="__ALL__",
    rule_id=paste0("declared_discharge_marker_",z$visit_concept_id[i]),scope=paste0("discharge_marker_evidence_not_confirmed_mortality:",
      if(is.null(config$analysis_population))"all_persons_active_filters" else "selected_cohort_windows_active_filters"),
    action="source_evidence",in_scope=FALSE,excluded_by_filter=FALSE,rule_order=NA_integer_,incremental_exclusion=FALSE,
    records=if(z$grain[i]=="event")as.double(z$source_records[i]) else NA_real_,persons=as.double(z$persons[i]),
    grain=z$grain[i],count_unit=if(z$grain[i]=="event")"visit_record" else if(z$grain[i]=="person")"person" else "undetermined"))
  out
}
