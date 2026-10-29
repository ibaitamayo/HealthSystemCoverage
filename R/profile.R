classify_visit_rows <- function(x, aliases) {
  text <- tolower(paste(
    x$visit_source_value, x$visit_concept_name,
    x$place_source_value, x$place_concept_name
  ))
  out <- rep("other_or_unknown", nrow(x))
  order <- c(
    primary_care_ehr = "primary_care",
    emergency_care = "emergency",
    home_hospitalisation = "home_hospitalisation",
    palliative_care = "palliative",
    mental_health = "mental_health",
    maternal_prenatal = "maternal_prenatal",
    inpatient_hospital = "inpatient",
    specialist_outpatient = "specialist_outpatient"
  )
  for (source_type in names(order)) {
    patterns <- aliases$pattern[aliases$source_type == source_type]
    if (!length(patterns)) next
    hit <- Reduce(`|`, lapply(patterns, grepl, x = text, ignore.case = TRUE,
                              perl = TRUE))
    out[hit & out == "other_or_unknown"] <- unname(order[[source_type]])
  }
  out
}

summarise_setting <- function(visits, setting) {
  x <- visits[visits$logical_setting == setting, , drop = FALSE]
  if (!nrow(x)) return(NULL)
  list(
    records = sum(as.double(x$records), na.rm = TRUE),
    persons = if (nrow(x) == 1L) as.double(x$persons) else NA_real_,
    start_date = min(as.Date(x$start_date), na.rm = TRUE),
    end_date = max(as.Date(x$end_date), na.rm = TRUE),
    invalid_order_records = sum(as.double(x$invalid_order_records), na.rm = TRUE),
    future_date_records = sum(as.double(x$future_date_records), na.rm = TRUE),
    pre1900_records = sum(as.double(x$pre1900_records), na.rm = TRUE)
  )
}

evaluate_core_components <- function(spec, evidence) {
  out <- empty_component_result(spec)
  if (!is.null(evidence$person$data)) {
    p <- evidence$person$data[1, ]
    out <- set_component(out, "population_registry", status = "detected_structured",
                         reliability = "moderate", persons = p$persons,
                         records = p$persons,
                         evidence = "OMOP person records are present")
    out <- set_component(out, "demographics", status = "detected_partial",
                         reliability = "moderate", persons = p$persons,
                         records = p$persons,
                         evidence = "Birth year and gender are populated",
                         issues = "Race/ethnicity and source-field completeness require separate interpretation")
  }
  if (!is.null(evidence$observation_period$data)) {
    op <- evidence$observation_period$data[1, ]
    status <- if (op$periods > 0) "detected_structured" else "not_detected"
    out <- set_component(out, "longitudinal_follow_up", status = status,
                         reliability = if (op$invalid_periods == 0) "moderate" else "low",
                         persons = op$persons, records = op$periods,
                         start_date = as.Date(op$start_date), end_date = as.Date(op$end_date),
                         evidence = "OMOP observation periods")
  }
  if (!is.null(evidence$visit_settings$data) && nrow(evidence$visit_settings$data)) {
    v <- evidence$visit_settings$data
    mapping <- c(
      primary_care = "primary_care_encounters",
      specialist_outpatient = "specialist_outpatient_encounters",
      emergency = "emergency_care", inpatient = "inpatient_admissions",
      home_hospitalisation = "home_hospitalisation", palliative = "palliative_care",
      mental_health = "mental_health", maternal_prenatal = "maternal_prenatal"
    )
    for (setting in names(mapping)) {
      z <- summarise_setting(v, setting)
      if (is.null(z)) next
      out <- set_component(out, mapping[[setting]], status = "detected_structured",
                           reliability = if (z$future_date_records > 0 ||
                                               z$invalid_order_records > 0) "low" else "moderate",
                           persons = z$persons,
                           records = z$records, start_date = z$start_date,
                           end_date = z$end_date,
                           evidence = paste("Visit records classified as", setting),
                           issues = if (z$future_date_records > 0 ||
                                         z$invalid_order_records > 0) {
                             paste(z$future_date_records, "future-dated and",
                                   z$invalid_order_records,
                                   "reverse-order visit records")
                           } else NA_character_)
    }
  }
  domain_component <- function(domain_data, setting, component_id,
                               evidence_label) {
    if (is.null(domain_data) || !nrow(domain_data)) return(NULL)
    z <- domain_data[domain_data$logical_setting == setting, , drop = FALSE]
    if (!nrow(z)) {
      return(list(component_id = component_id, status = "unknown",
                  reliability = "unknown", persons = 0, records = 0,
                  evidence = evidence_label,
                  issues = zero_setting_issue()))
    }
    valid <- sum(z$valid_standard_records, na.rm = TRUE)
    records <- sum(z$records, na.rm = TRUE)
    source_values <- sum(z$source_value_records, na.rm = TRUE)
    status <- if (valid == records && records > 0) {
      "detected_structured"
    } else if (valid > 0) {
      "detected_partial"
    } else if (source_values > 0) {
      "detected_partial"
    } else {
      "detected_partial"
    }
    list(component_id = component_id, status = status,
         reliability = if (valid == records) "moderate" else "low",
         persons = sum(z$persons, na.rm = TRUE), records = records,
         start_date = min(as.Date(z$start_date), na.rm = TRUE),
         end_date = max(as.Date(z$end_date), na.rm = TRUE),
         mapped_fraction = if (records) valid / records else NA_real_,
         evidence = evidence_label,
         issues = if (valid < records) {
           paste(records - valid,
                 "records are not valid standard concepts in the expected domain")
         } else NA_character_)
  }
  condition_map <- c(primary_care = "primary_care_diagnoses",
                     specialist_outpatient = "specialist_outpatient_diagnoses",
                     inpatient = "inpatient_diagnoses")
  if (!is.null(evidence$conditions$data)) {
    for (setting in names(condition_map)) {
      z <- domain_component(evidence$conditions$data, setting,
                            condition_map[[setting]],
                            paste("Condition records directly linked to", setting,
                                  "visits"))
      if (!is.null(z)) out <- do.call(set_component, c(list(x = out), z))
    }
  }
  if (!is.null(evidence$drugs$data)) {
    z <- domain_component(evidence$drugs$data, "primary_care",
                          "outpatient_prescriptions",
                          "Drug records directly linked to primary-care visits; this does not by itself prove dispensing")
    if (!is.null(z)) out <- do.call(set_component, c(list(x = out), z))
    hospital_settings <- c("inpatient", "emergency", "specialist_outpatient",
                           "palliative")
    dz <- evidence$drugs$data[evidence$drugs$data$logical_setting %in%
                              hospital_settings, , drop = FALSE]
    if (nrow(dz)) {
      records <- sum(dz$records, na.rm = TRUE)
      valid <- sum(dz$valid_standard_records, na.rm = TRUE)
      out <- set_component(out, "hospital_medications",
                           status = if (valid == records) "detected_structured" else "detected_partial",
                           reliability = if (valid == records) "moderate" else "low",
                           persons = NA_real_, records = records,
                           start_date = min(as.Date(dz$start_date), na.rm = TRUE),
                           end_date = max(as.Date(dz$end_date), na.rm = TRUE),
                           mapped_fraction = if (records) valid / records else NA_real_,
                           evidence = "Drug records directly linked to hospital-care visits",
                           issues = "A hospital visit link does not distinguish administration, prescription and dispensing")
    }
  }
  if (!is.null(evidence$death$data) && nrow(evidence$death$data)) {
    d <- evidence$death$data[1, ]
    status <- if (d$deaths > 0) "detected_structured" else "not_detected"
    out <- set_component(out, "mortality_all_cause", status = status,
                         reliability = if (d$pre1900_deaths > 0 ||
                                              d$future_deaths > 0 ||
                                              d$before_birth_deaths > 0) "low" else "moderate",
                         persons = d$persons,
                         records = d$deaths, start_date = as.Date(d$start_date),
                         end_date = as.Date(d$end_date), evidence = "OMOP death records",
                         issues = if (d$pre1900_deaths > 0 ||
                                       d$future_deaths > 0 ||
                                       d$before_birth_deaths > 0) {
                           paste(d$pre1900_deaths, "pre-1900 deaths;",
                                 d$future_deaths, "future deaths;",
                                 d$before_birth_deaths, "deaths before birth year")
                         } else NA_character_)
    causes <- sum(d$mapped_causes, d$source_causes, na.rm = TRUE)
    out <- set_component(out, "mortality_cause",
                         status = if (causes > 0) "detected_partial" else "not_detected",
                         reliability = if (causes > 0) "moderate" else "unknown",
                         persons = if (causes > 0) d$persons else 0,
                         records = causes,
                         evidence = "Mapped or source cause-of-death fields")
  }
  if (!is.null(evidence$death_setting$data) &&
      nrow(evidence$death_setting$data)) {
    ds <- evidence$death_setting$data[1, ]
    out <- set_component(
      out, "mortality_in_hospital", status = "detected_partial",
      reliability = "low", persons = ds$inpatient_date_overlap,
      records = ds$inpatient_date_overlap,
      evidence = paste(ds$inpatient_date_overlap,
                       "death dates overlap an inpatient visit"),
      issues = "Date overlap is compatible with, but does not prove, in-hospital death; discharge disposition and source provenance must be assessed"
    )
    outside_candidates <- if ("no_inpatient_or_hospice_overlap" %in% names(ds))
      as.double(ds$no_inpatient_or_hospice_overlap) else NA_real_
    out <- set_component(
      out, "mortality_outside_hospital", status = "unknown",
      reliability = "low", persons = outside_candidates,
      records = outside_candidates,
      evidence = paste(outside_candidates,
                       "deaths do not overlap inpatient or hospice-coded visits"),
      issues = "Lack of a same-day hospital overlap is not a verified place-of-death source"
    )
  }
  out
}

#' Profile logical health-system coverage in an OMOP database
#'
#' @param con Open DBI connection.
#' @param config Configuration returned by [new_profile_config()].
#' @param modules Probe modules to run.
#' @return An object of class `health_system_profile` containing aggregate data.
#' @export
profile_health_system <- function(con, config,
                                  modules = c("inventory", "person",
                                              "observation_period", "visits",
                                              "conditions", "drugs", "death",
                                              "note_schema", "coverage", "source_crosswalk")) {
  assert_connection(con)
  if (!inherits(config, "health_system_profile_config")) {
    stop("config must be created with new_profile_config()")
  }
  if (config$dbms != "postgresql") {
    stop("Currently supports PostgreSQL only")
  }
  schema <- config$cdm_schema
  probes <- list(config = config)
  if("source_crosswalk"%in%modules)probes$source_crosswalk<-safe_probe("source_crosswalk",function()audit_source_crosswalk(con,config))
  if ("event_dates" %in% modules) probes$event_dates <- list(name="event_dates",status="ok",data=review_event_dates(con,config))
  if (!is.null(config$event_date_min) && !"coverage" %in% modules)
    stop("Event date filtering requires the coverage module for filtered quantitative results")
  if ("inventory" %in% modules) probes$inventory <- safe_probe("inventory", function() probe_inventory(con, config))
  if ("person" %in% modules && table_exists(con, schema, "person")) probes$person <- safe_probe("person", function() probe_person(con, config))
  if ("observation_period" %in% modules && table_exists(con, schema, "observation_period")) probes$observation_period <- safe_probe("observation_period", function() probe_observation_period(con, config))
  if ("visits" %in% modules && table_exists(con, schema, "visit_occurrence")) probes$visits <- safe_probe("visits", function() probe_visits(con, config))
  if ("visits" %in% modules && table_exists(con, schema, "visit_occurrence")) probes$visit_settings <- safe_probe("visit_settings", function() probe_visit_settings(con, config))
  if ("conditions" %in% modules && table_exists(con, schema, "condition_occurrence")) probes$conditions <- safe_probe("conditions", function() probe_domain_by_visit_source(con, config, "condition_occurrence", "condition_occurrence_id", "condition_start_date", "condition_concept_id", "condition_source_concept_id", "condition_source_value", "Condition"))
  if ("drugs" %in% modules && table_exists(con, schema, "drug_exposure")) probes$drugs <- safe_probe("drugs", function() probe_domain_by_visit_source(con, config, "drug_exposure", "drug_exposure_id", "drug_exposure_start_date", "drug_concept_id", "drug_source_concept_id", "drug_source_value", "Drug"))
  if ("death" %in% modules) probes$death <- safe_probe("death", function() probe_death(con, config))
  if ("death" %in% modules) probes$death_setting <- safe_probe("death_setting", function() probe_death_setting(con, config))
  if ("note_schema" %in% modules) probes$note_schema <- safe_probe("note_schema", function() probe_note_schema(con, config))
  if ("coverage" %in% modules) probes$coverage <- safe_probe("coverage", function() probe_coverage(con, config))
  if (!is.null(config$event_date_min) && identical(probes$coverage$status,"error"))
    stop("Filtered coverage query failed; no unfiltered fallback is permitted")
  components <- evaluate_core_components(health_system_spec(), probes)
  components <- apply_adjudications(components, config$adjudications)
  # Quantitative output is refreshed from this run, independently of reviewed interpretation.
  components<-apply_coverage_quality(components,probes$coverage$data,config)
  errors <- do.call(rbind, lapply(probes[vapply(probes, is.list, logical(1))], function(x) {
    if (is.null(x$status) || x$status != "error") return(NULL)
    data.frame(probe = x$name, error = x$error, stringsAsFactors = FALSE)
  }))
  structure(
    list(
      generated_at = Sys.time(), config = config,
      components = components, evidence = probes,
      findings = rbind(coverage_findings(probes$coverage$data,config),
        if(!is.null(probes$source_crosswalk$data$findings))probes$source_crosswalk$data$findings else empty_findings()),
      probe_errors = errors
    ),
    class = "health_system_profile"
  )
}
