#' Create a health-system profile configuration
#'
#' @param cdm_schema OMOP CDM schema.
#' @param source_aliases Optional data frame with columns `source_type` and
#'   `pattern`. Patterns are case-insensitive PostgreSQL regular expressions.
#' @param privacy_min_cell Minimum cell count shown in exports.
#' @param database_cutoff Optional known ETL/database-generation date. With event
#'   filtering enabled, NULL initially resolves to Sys.Date(). Profiling can
#'   adopt a validated, non-contradictory CDM_SOURCE extraction date instead.
#' @param dbms Database engine. The first implementation supports PostgreSQL.
#' @return A validated configuration list.
#' @export
new_profile_config <- function(cdm_schema,
                               source_aliases = NULL,
                               adjudications = NULL,
                               privacy_min_cell = 5L,
                               database_cutoff = NULL,
                               dbms = "postgresql",
                               component_rules = NULL,
                               source_name = NULL,
                               visit_concept_map = NULL,
                               event_date_min = NULL,
                               birth_date_rule = c("year", "date", "off"),
                               mapping_audit_policy = c("require_local", "report"),
                               source_vocabularies = NULL,
                               crosswalk_domains = NULL,
                               unknown_setting_threshold = 0.20,
                               cutoff_lag_days = 183L,
                               community_dispensing_type_ids = c(38000175,38000176),
                               discharge_outcome_map = NULL,
                               provider_specialty_map = NULL) {
  if(is.null(provider_specialty_map))provider_specialty_map<-data.frame(
    component_id=c("mental_health","maternal_prenatal_care"),specialty_concept_id=c(38004469,38004461))
  stopifnot(is.data.frame(provider_specialty_map),all(c("component_id","specialty_concept_id")%in%names(provider_specialty_map)),
    !anyNA(provider_specialty_map),is.numeric(provider_specialty_map$specialty_concept_id),
    all(provider_specialty_map$component_id%in%c("mental_health","maternal_prenatal_care")),
    all(is.finite(provider_specialty_map$specialty_concept_id)),all(provider_specialty_map$specialty_concept_id>0),
    all(provider_specialty_map$specialty_concept_id==floor(provider_specialty_map$specialty_concept_id)))
  if(!is.null(discharge_outcome_map)) {
    stopifnot(is.data.frame(discharge_outcome_map),all(c("visit_concept_id","source_value","grain")%in%names(discharge_outcome_map)),
      is.numeric(discharge_outcome_map$visit_concept_id),!anyNA(discharge_outcome_map),
      all(discharge_outcome_map$visit_concept_id>=0),
      all(discharge_outcome_map$visit_concept_id==floor(discharge_outcome_map$visit_concept_id)),
      all(nzchar(discharge_outcome_map$source_value)),all(discharge_outcome_map$grain%in%c("event","person","unknown")),
      !anyDuplicated(discharge_outcome_map[,c("visit_concept_id","source_value")]))
  }
  stopifnot(is.numeric(unknown_setting_threshold),length(unknown_setting_threshold)==1L,
    is.finite(unknown_setting_threshold),unknown_setting_threshold>=0,unknown_setting_threshold<=1,
    is.numeric(cutoff_lag_days),length(cutoff_lag_days)==1L,is.finite(cutoff_lag_days),cutoff_lag_days>=0,
    is.numeric(community_dispensing_type_ids),!anyNA(community_dispensing_type_ids),
    all(is.finite(community_dispensing_type_ids)),all(community_dispensing_type_ids>0),
    all(community_dispensing_type_ids==floor(community_dispensing_type_ids)))
  birth_date_rule <- match.arg(birth_date_rule)
  mapping_audit_policy <- match.arg(mapping_audit_policy)
  if(!is.null(source_vocabularies) &&
     (!is.character(source_vocabularies)||is.null(names(source_vocabularies))||
      anyNA(source_vocabularies)||any(!nzchar(source_vocabularies))||
      any(!nzchar(names(source_vocabularies)))||anyDuplicated(names(source_vocabularies))))
    stop("source_vocabularies must be a named character vector: OMOP table -> source vocabulary ID")
  if(!is.null(crosswalk_domains)&&(!is.data.frame(crosswalk_domains)||
    !all(c("source_vocabulary_id","expected_domain")%in%names(crosswalk_domains))||
    anyNA(crosswalk_domains[,c("source_vocabulary_id","expected_domain")])||
    any(!nzchar(crosswalk_domains$source_vocabulary_id))||any(!nzchar(crosswalk_domains$expected_domain))))
    stop("crosswalk_domains requires source_vocabulary_id and expected_domain")
  stopifnot(is.character(cdm_schema), length(cdm_schema) == 1L, nzchar(cdm_schema))
  stopifnot(is.numeric(privacy_min_cell), length(privacy_min_cell) == 1L,
            privacy_min_cell >= 0)
  cutoff_source <- if(is.null(database_cutoff)) "not_set" else "provided"
  if (!is.null(event_date_min)) {
    if(is.null(database_cutoff)) {
      database_cutoff <- Sys.Date()
      cutoff_source <- "Sys.Date() fallback"
    }
    event_date_min <- as.Date(event_date_min)
    cutoff <- as.Date(database_cutoff)
    if (length(event_date_min)!=1L || anyNA(event_date_min) ||
        length(cutoff)!=1L || anyNA(cutoff) || cutoff<event_date_min)
      stop("Event filtering requires valid ordered event_date_min and database_cutoff dates")
  }
  if (is.null(source_aliases)) source_aliases <- default_source_aliases()
  required <- c("source_type", "pattern")
  if (!all(required %in% names(source_aliases))) {
    stop("source_aliases must contain: ", paste(required, collapse = ", "))
  }
  if (!is.null(visit_concept_map)) {
    stopifnot(all(c("concept_id", "setting") %in% names(visit_concept_map)),
              is.numeric(visit_concept_map$concept_id),!anyNA(visit_concept_map$concept_id),
              all(is.finite(visit_concept_map$concept_id)),all(visit_concept_map$concept_id>=0),
              all(visit_concept_map$concept_id==floor(visit_concept_map$concept_id)),
              !anyNA(visit_concept_map$setting),all(nzchar(visit_concept_map$setting)))
    keys<-"concept_id"
    if("place_of_service_concept_id"%in%names(visit_concept_map)) {
      p<-visit_concept_map$place_of_service_concept_id
      stopifnot(is.numeric(p),all(is.na(p)|(is.finite(p)&p>=0&p==floor(p))))
      keys<-c(keys,"place_of_service_concept_id")
    }
    stopifnot(!anyDuplicated(visit_concept_map[,keys,drop=FALSE]))
    attr(source_aliases, "visit_concept_map") <- visit_concept_map
  }
  if (!is.null(adjudications)) {
    required_adjudication <- c("component_id", "status", "reliability",
                               "fitness_for_research", "evidence", "issues")
    if (!all(required_adjudication %in% names(adjudications))) {
      stop("adjudications must contain: ",
           paste(required_adjudication, collapse = ", "))
    }
    unknown <- setdiff(adjudications$component_id,
                       health_system_spec()$component_id)
    if (length(unknown)) {
      stop("Unknown component_id in adjudications: ",
           paste(unique(unknown), collapse = ", "))
    }
  }
  structure(
    list(
      cdm_schema = cdm_schema,
      source_aliases = source_aliases,
      adjudications = adjudications,
      privacy_min_cell = as.integer(privacy_min_cell),
      database_cutoff = as.Date(database_cutoff),
      database_cutoff_source = cutoff_source,
      event_date_min = event_date_min,
      birth_date_rule = birth_date_rule,
      mapping_audit_policy = mapping_audit_policy,
      source_vocabularies = source_vocabularies,
      crosswalk_domains = crosswalk_domains,
      unknown_setting_threshold = unknown_setting_threshold,
      cutoff_lag_days = cutoff_lag_days,
      community_dispensing_type_ids = unique(community_dispensing_type_ids),
      discharge_outcome_map = discharge_outcome_map,
      provider_specialty_map = unique(provider_specialty_map),
      dbms = tolower(dbms),
      component_rules = component_rules,
      source_name = source_name
    ),
    class = "health_system_profile_config"
  )
}

default_source_aliases <- function() {
  data.frame(source_type = character(), pattern = character(),
             stringsAsFactors = FALSE)
}
