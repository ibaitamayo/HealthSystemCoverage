# Current, traceable definitions. Each SELECT returns person, source record and date.
coverage_event_queries <- function(con, config, cohort=NULL,apply_event_filter=TRUE) {
  s <- config$cdm_schema
  tab <- function(t) qualified_table(con, s, t)
  present <- function(t) table_exists(con, s, t)
  queries <- list()
  column_cache<-new.env(parent=emptyenv())
  columns<-function(t) {
    if(!exists(t,column_cache,inherits=FALSE))assign(t,table_columns(con,s,t),column_cache)
    get(t,column_cache,inherits=FALSE)
  }
  map_available<-present("source_to_concept_map") && all(c("source_code","source_vocabulary_id","target_concept_id","invalid_reason")%in%columns("source_to_concept_map"))
  scope_sql <- if(is.null(cohort))NULL else cohort_sql(con,cohort)
  put <- function(cid, sql) queries[[cid]] <<- sql
  person_join <- paste0(" JOIN ", tab("person"), " p ON p.person_id=d.person_id ")
  event <- function(table, key, date, predicate = "TRUE", extra = "") {
    dates<-event_date_exclusions(con,config,table,columns=columns(table))
    ordered_dates<-intersect(event_date_fields()[[table]],columns(table))
    reversed<-if(length(ordered_dates)>1L)paste0("COALESCE(d.",ordered_dates[-1],"<d.",ordered_dates[1],",false)",collapse=" OR ") else "FALSE"
    mapping<-mapping_event_flags(con,config,table,columns(table),map_available)
    ms<-mapping_spec()[[table]]
    has_crosswalk<-map_available&&!is.null(ms)&&all(ms[1:3]%in%columns(table))
    if(has_crosswalk)extra<-paste0(extra," LEFT JOIN hsc_crosswalk sm ON sm.source_code=d.",ms[3]," ")
    if(apply_event_filter && !is.null(config$event_date_min) && table %in% names(event_date_fields()))
      predicate <- paste0("(",predicate,") AND ",event_date_predicate(con,config,table,
        columns=columns(table)))
    if(!is.null(cohort)) {
      window <- if(is.null(date)) "TRUE" else if(table=="observation_period")
        "d.observation_period_start_date<=cw.cohort_end_date AND d.observation_period_end_date>=cw.cohort_start_date" else
        paste0("d.",date," BETWEEN cw.cohort_start_date AND cw.cohort_end_date")
      predicate <- paste0("(",predicate,") AND EXISTS (SELECT 1 FROM (",scope_sql,
        ") cw WHERE cw.subject_id=d.person_id AND ",window,")")
    }
    sql<-paste0("SELECT d.person_id, '", table, "'::text AS source_table, d.", key,
      "::text AS record_id, ", if (is.null(date)) "NULL::date" else paste0("d.", date),
      " AS event_date, ",paste0(unname(mapping)," AS ",names(mapping),collapse=", "),
      ", (",dates$before,") AS before_min, (",dates$after,") AS after_cutoff, (",dates$before_birth,") AS before_birth, (",reversed,") AS end_before_start, (",dates$any,") AS event_excluded FROM ", tab(table), " d", person_join, extra,
      " WHERE ", predicate)
    if(has_crosswalk) {
      cw<-qualified_table(con,s,"source_to_concept_map")
      vocab<-config$source_vocabularies[table]
      restrict<-if(length(vocab)==1L&&!is.na(vocab))paste0(" AND source_vocabulary_id=",DBI::dbQuoteString(con,unname(vocab))) else ""
      sql<-paste0("WITH hsc_crosswalk AS MATERIALIZED (SELECT source_code,array_agg(DISTINCT target_concept_id) AS target_ids,count(DISTINCT target_concept_id) AS target_count,count(DISTINCT source_vocabulary_id) AS vocabulary_count FROM ",cw,
        " WHERE invalid_reason IS NULL AND NULLIF(btrim(source_vocabulary_id),'') IS NOT NULL",restrict," GROUP BY source_code) ",sql)
    }
    sql
  }
  if (!present("person")) return(queries)
  for (cid in c("population_registry", "demographics"))
    put(cid, event("person", "person_id", NULL))
  if (present("location")) put("geographic_assignment", event("person", "person_id", NULL,
    "d.location_id IS NOT NULL", paste0(" JOIN ", tab("location"), " loc ON loc.location_id=d.location_id")))
  if (present("observation_period")) put("longitudinal_follow_up",
    event("observation_period", "observation_period_id", "observation_period_start_date"))
  if (present("visit_occurrence") && present("care_site") && present("concept")) {
    setting <- visit_setting_case(con, config$source_aliases)
    classified <- paste0("SELECT v.visit_occurrence_id, v.person_id, ", setting,
      " AS logical_setting FROM ", tab("visit_occurrence"), " v LEFT JOIN ",
      tab("care_site"), " cs ON cs.care_site_id=v.care_site_id")
    linkage <- paste0(" JOIN (", classified,
      ") cv ON cv.visit_occurrence_id=d.visit_occurrence_id AND cv.person_id=d.person_id ")
    visit_map <- c(primary_care_encounters="primary_care", inpatient_admissions="inpatient",
      emergency_care="emergency", specialist_outpatient_encounters="specialist_outpatient",
      home_hospitalisation="home_hospitalisation", palliative_care="palliative",
      mental_health="mental_health", maternal_prenatal_care="maternal_prenatal")
    for (cid in names(visit_map)) put(cid, event("visit_occurrence", "visit_occurrence_id", "visit_start_date",
      paste0("cv.logical_setting=", DBI::dbQuoteString(con, visit_map[[cid]])), linkage))
    domain_map <- list(
      condition_occurrence=list(key="condition_occurrence_id", date="condition_start_date",
        ids=c(primary_care_diagnoses="primary_care", inpatient_diagnoses="inpatient", specialist_outpatient_diagnoses="specialist_outpatient")),
      drug_exposure=list(key="drug_exposure_id", date="drug_exposure_start_date",
        ids=c(outpatient_prescriptions="primary_care")),
      procedure_occurrence=list(key="procedure_occurrence_id", date="procedure_date",
        ids=c(primary_care_procedures="primary_care", inpatient_procedures="inpatient")))
    for (t in names(domain_map)) if (present(t)) {
      z <- domain_map[[t]]
      for (cid in names(z$ids)) put(cid, event(t, z$key, z$date,
        paste0("cv.logical_setting=", DBI::dbQuoteString(con, z$ids[[cid]])), linkage))
    }
    if (present("drug_exposure")) put("hospital_medications", event("drug_exposure", "drug_exposure_id",
      "drug_exposure_start_date", "cv.logical_setting IN ('inpatient','emergency','specialist_outpatient','palliative')", linkage))
  }
  if (present("death")) {
    put("mortality_all_cause", event("death", "person_id", "death_date"))
    cols <- table_columns(con, s, "death")
    predicates <- character()
    if ("cause_concept_id" %in% cols) predicates <- c(predicates, "COALESCE(d.cause_concept_id,0)<>0")
    if ("cause_source_value" %in% cols) predicates <- c(predicates, "NULLIF(btrim(d.cause_source_value),'') IS NOT NULL")
    if (length(predicates)) put("mortality_cause", event("death", "person_id", "death_date", paste(predicates,collapse=" OR ")))
  }
  # Optional explicit concept lists, never lexical searches. Definitions are exported.
  rules <- config$component_rules
  allowed <- list(
    measurement=c("measurement_id","measurement_date","measurement_concept_id"),
    observation=c("observation_id","observation_date","observation_concept_id"),
    specimen=c("specimen_id","specimen_date","specimen_concept_id"),
    drug_exposure=c("drug_exposure_id","drug_exposure_start_date","drug_concept_id"),
    procedure_occurrence=c("procedure_occurrence_id","procedure_date","procedure_concept_id"))
  if (!is.null(rules) && nrow(rules)) {
    stopifnot(all(c("component_id","table","concept_id") %in% names(rules)))
    for (cid in unique(rules$component_id)) {
      stopifnot(cid %in% health_system_spec()$component_id)
      rr <- rules[rules$component_id==cid,,drop=FALSE]
      pieces <- character()
      for (t in unique(rr$table)) {
        if (!t %in% names(allowed)) stop("Unsupported explicit-rule table: ",t)
        if (!present(t)) next
        ids <- as.double(rr$concept_id[rr$table==t])
        stopifnot(length(ids)>0,!anyNA(ids),all(is.finite(ids)),all(ids==floor(ids)),all(ids>=0))
        a <- allowed[[t]]
        pieces <- c(pieces, event(t,a[1],a[2],paste0("d.",a[3]," IN (",paste(format(ids,scientific=FALSE,trim=TRUE),collapse=","),")")))
      }
      if (length(pieces)) put(cid,paste(pieces,collapse=" UNION ALL "))
    }
  }
  queries
}

probe_coverage <- function(con, config, cohort=NULL) {
  queries <- coverage_event_queries(con, config, cohort,apply_event_filter=FALSE)
  if (!length(queries)) return(data.frame())
  member <- coverage_membership()
  nodes <- c(as.character(member$component_id), unique(member$group_id), unique(member$area_id))
  results <- list()
  for (node in nodes) {
    ids <- if (node %in% member$component_id) node else
      member$component_id[member$group_id==node | member$area_id==node]
    ids <- intersect(ids,names(queries))
    if (!length(ids)) next
    message("Coverage: ", node)
    # UNION deduplicates both people and records shared by multiple definitions.
    events <- paste(vapply(queries[ids],function(q) paste0("(",q,")"),""),collapse=" UNION ")
    sql <- paste0("SELECT ",coverage_aggregate_sql()," FROM (",events,") e")
    z <- DBI::dbGetQuery(con,sql)
    z$persons <- as.double(z$persons);z$records <- as.double(z$records)
    z$node_id <- node
    z$assessed_children <- paste(ids,collapse=";")
    results[[node]] <- z
  }
  enrich_coverage_stats(do.call(rbind,results),config)
}
