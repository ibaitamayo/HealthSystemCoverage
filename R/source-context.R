expected_cdm_tables <- function() unique(c("person","observation_period",names(event_date_fields()),
  "provider","care_site","location","cost","drug_era","condition_era","dose_era",
  "visit_detail","fact_relationship","episode","episode_event","payer_plan_period",
  "note_nlp","drug_strength","metadata","cdm_source","concept","concept_ancestor","source_to_concept_map"))

prepare_source_context <- function(con,config) {
  inventory<-probe_inventory(con,config)
  config$table_states<-inventory[,c("table_name","table_state"),drop=FALSE]
  config$source_inventory<-inventory
  config$cost_domains<-if("cost"%in%inventory$table_name[inventory$table_state=="nonempty"]&&
    "cost_domain_id"%in%table_columns(con,config$cdm_schema,"cost"))
    as.character(DBI::dbGetQuery(con,paste0("SELECT DISTINCT cost_domain_id FROM ",qualified_table(con,config$cdm_schema,"cost")))$cost_domain_id) else character()
  source<-data.frame()
  s<-config$cdm_schema
  if(table_exists(con,s,"cdm_source")) {
    keep<-intersect(c("cdm_source_name","cdm_source_abbreviation","cdm_holder","source_release_date",
      "cdm_release_date","cdm_version","vocabulary_version"),table_columns(con,s,"cdm_source"))
    if(length(keep))source<-DBI::dbGetQuery(con,paste0("SELECT ",paste(qid(con,keep),collapse=",")," FROM ",qualified_table(con,s,"cdm_source")))
  }
  config$source_metadata<-source
  config$source_date_validation<-data.frame(candidate_date=as.Date(NA),status="missing",contradiction_table=NA_character_)
  if("source_release_date"%in%names(source)) {
    candidates<-unique(as.Date(source$source_release_date[!is.na(source$source_release_date)]))
    if(length(candidates)==1L) {
      candidate<-candidates[1]
      low<-if(is.null(config$event_date_min))as.Date("1900-01-01") else config$event_date_min
      status<-if(candidate>Sys.Date()||candidate<low)"outside_plausible_bounds" else "consistent_with_observed_starts"
      contradiction<-NA_character_
      if(status=="consistent_with_observed_starts")for(t in names(event_date_fields())) {
        if(!t%in%inventory$table_name[inventory$table_state=="nonempty"])next
        date<-event_date_fields()[[t]][1]
        if(!date%in%table_columns(con,s,t))next
        exists<-DBI::dbGetQuery(con,paste0("SELECT EXISTS (SELECT 1 FROM ",qualified_table(con,s,t),
          " d WHERE d.",date,">",DBI::dbQuoteString(con,as.character(candidate)),"::date AND d.",date,
          "<=CURRENT_DATE) AS hit"))$hit[1]
        if(isTRUE(exists)){status<-"contradicted_by_observed_starts";contradiction<-t;break}
      }
      config$source_date_validation<-data.frame(candidate_date=candidate,status=status,contradiction_table=contradiction)
      if(status=="consistent_with_observed_starts"&&identical(config$database_cutoff_source,"Sys.Date() fallback")) {
        config$database_cutoff<-candidate
        config$database_cutoff_source<-"cdm_source.source_release_date (validated)"
      }
    } else if(length(candidates)>1L)config$source_date_validation$status<-"ambiguous_multiple_dates"
  }
  config
}

domain_for_component <- function(id) {
  mapping<-c(primary_care_procedures="procedure_occurrence",inpatient_procedures="procedure_occurrence",
    primary_care_diagnoses="condition_occurrence",inpatient_diagnoses="condition_occurrence",
    specialist_outpatient_diagnoses="condition_occurrence",outpatient_prescriptions="drug_exposure",
    community_dispensings="drug_exposure",hospital_medications="drug_exposure",clinical_notes="note",
    costs_and_copayment="cost")
  if(id%in%names(mapping))unname(mapping[id]) else if(id%in%c(visit_dependent_components(),"hospital_encounters_unspecified"))"visit_occurrence" else NA_character_
}

domain_is_empty <- function(id,config) {
  table<-domain_for_component(id);states<-config$table_states
  !is.na(table)&&!is.null(states)&&table%in%states$table_name[states$table_state=="empty"]
}

apply_source_component_quality <- function(components,config) {
  for(i in seq_len(nrow(components))) {
    id<-components$component_id[i]
    if(!is.na(components$records[i])&&components$records[i]>0&&id=="clinical_notes") {
      components$status[i]<-"detected_unstructured"
      components$issues[i]<-paste(components$issues[i],"Note rows with non-null note_text; text completeness and clinical meaning are not inferred. Unexpected source columns are not read.")
    }
    if(id=="costs_and_copayment")components$issues[i]<-paste(components$issues[i],
      "This is the costs layer. The legacy component ID does not imply copayment; patient-payment fields are audited separately.")
    if(id=="maternal_prenatal_care")components$issues[i]<-paste(components$issues[i],
      "Provider-linked obstetrics/gynaecology demonstrates service representation, not pregnancy or prenatal eligibility.")
    if(id%in%c("mental_health","maternal_prenatal_care")&&!is.na(components$records[i])&&components$records[i]>0&&
       components$status[i]=="detected_structured") {
      components$status[i]<-"detected_partial"
      components$issues[i]<-paste(components$issues[i],"Encounter/service evidence does not establish capture of all diagnoses, treatments or specialised activities in this component.")
    }
    if(id%in%c("emergency_care","inpatient_admissions","inpatient_diagnoses","inpatient_procedures"))components$issues[i]<-paste(components$issues[i],
      "Combined ER/inpatient records contribute under their coded meaning. They are not split into synthetic episodes; emergency and inpatient component counts overlap and must not be summed as admissions.")
    if(!is.na(components$records[i])&&components$records[i]==0&&domain_is_empty(id,config)) {
      components$status[i]<-"not_detected"
      components$issues[i]<-paste("The source domain table exists and contains no rows, independently of setting classification.",components$issues[i])
    }
  }
  components
}
