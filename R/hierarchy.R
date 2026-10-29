# Fixed functional catalogue; source-specific recognition is configured separately.
coverage_groups <- function() {
  defs <- list(
    c("community", "primary_care", "Primary Care", "primary_care_encounters", "primary_care_diagnoses", "primary_care_procedures", "primary_care_nursing"),
    c("community", "community_pharmacy", "Community Medicines", "outpatient_prescriptions", "community_dispensings"),
    c("community", "home_care", "Hospital at Home", "home_hospitalisation"),
    c("hospital", "outpatient", "Outpatient Clinics", "specialist_outpatient_encounters", "specialist_outpatient_diagnoses"),
    c("hospital", "emergency", "Emergency", "emergency_care"),
    c("hospital", "inpatient", "Inpatient Care", "inpatient_admissions", "inpatient_diagnoses", "inpatient_procedures"),
    c("hospital", "surgery", "Surgery", "surgery"),
    c("hospital", "hospital_pharmacy", "Hospital Medicines", "hospital_medications"),
    c("diagnostics", "laboratory", "Laboratory", "laboratory"),
    c("diagnostics", "microbiology", "Microbiology", "microbiology"),
    c("diagnostics", "imaging", "Imaging", "radiology_imaging"),
    c("diagnostics", "pathology", "Pathology", "histopathology"),
    c("diagnostics", "blood_bank", "Blood Management", "blood_management"),
    c("public_health", "screening", "Organised Screening", "cancer_screening_breast", "cancer_screening_cervical", "cancer_screening_colorectal"),
    c("public_health", "immunisation", "Immunisation", "vaccinations"),
    c("public_health", "occupational_health", "Occupational Health", "occupational_health"),
    c("public_health", "surveillance", "Epidemiological Surveillance", "epidemiological_surveillance"),
    c("public_health", "registries", "Disease Registries", "disease_registries"),
    c("shared", "population", "Population & Follow-up", "population_registry", "demographics", "geographic_assignment", "longitudinal_follow_up", "system_exit_attrition"),
    c("shared", "mortality", "Mortality", "mortality_all_cause", "mortality_cause", "mortality_in_hospital", "mortality_outside_hospital"),
    c("other_care", "palliative", "Palliative Care", "palliative_care"),
    c("other_care", "mental_health", "Mental Health", "mental_health"),
    c("other_care", "maternal", "Maternal & Prenatal Care", "maternal_prenatal_care"),
    c("other_care", "social", "Social Care", "social_care"),
    c("shared", "lifestyle", "Lifestyle", "lifestyle"),
    c("shared", "costs", "Costs & Copayment", "costs_and_copayment")
  )
  out <- data.frame(area = vapply(defs, `[`, "", 1),
                    group_id = vapply(defs, `[`, "", 2),
                    title = vapply(defs, `[`, "", 3), stringsAsFactors = FALSE)
  out$components <- lapply(defs, function(x) x[-(1:3)])
  out
}

coverage_area_names <- function() c(
  community = "Community Care", hospital = "Hospital Care",
  diagnostics = "Diagnostic Services", public_health = "Public Health & Prevention",
  other_care = "Other Care Modalities", shared = "System-wide Information")

coverage_membership <- function() {
  groups <- coverage_groups()
  do.call(rbind, lapply(seq_len(nrow(groups)), function(i) {
    data.frame(component_id = groups$components[[i]],
               group_id = paste0("group_", groups$group_id[i]),
               area_id = paste0("area_", groups$area[i]), stringsAsFactors = FALSE)
  }))
}

#' Build hierarchical coverage results
#' @param profile A health_system_profile object.
#' @return A data frame with area, component-group and subcomponent rows.
#' @export
coverage_results <- function(profile) {
  groups <- coverage_groups()
  components <- profile$components
  denominator <- if (!is.null(profile$evidence$person$data))
    as.double(profile$evidence$person$data$persons[1]) else NA_real_
  measured <- if (!is.null(profile$evidence$coverage$data))
    profile$evidence$coverage$data else data.frame()
  measured <- suppress_small_cells(measured, profile$config$privacy_min_cell)
  rows <- list()
  add <- function(id, parent, level, label, area, group, leaf_ids, leaf = NULL) {
    child <- components[components$component_id %in% leaf_ids, , drop = FALSE]
    m <- if (nrow(measured)) measured[measured$node_id == id, , drop = FALSE] else data.frame()
    has <- nrow(m) == 1L
    # Counts always come from current SQL, never from a previous reviewed CSV.
    persons <- if (has) as.double(m$persons[1]) else NA_real_
    records <- if (has) as.double(m$records[1]) else NA_real_
    status <- if (!is.null(leaf)) leaf$status else map_group_status(child$status)
    ev <- if (!is.null(leaf)) leaf$evidence else "Union of the assessed child definitions; people and source records deduplicated in SQL."
    issues <- if (!is.null(leaf)) leaf$issues else "Counts cover assessed components only; unassessed children are not assumed absent."
    rows[[length(rows) + 1L]] <<- data.frame(
      node_id = id, parent_id = parent, level = level, label = label,
      area = area, group = group, status = status,
      persons = persons, population = denominator,
      percentage = if (has && !is.na(persons) && !is.na(denominator) && denominator > 0)
        100 * persons / denominator else NA_real_,
      records = records,
      start_date = if (has) as.character(m$start_date[1]) else NA_character_,
      end_date = if (has) as.character(m$end_date[1]) else NA_character_,
      measure = if(is.null(profile$config$analysis_population))
        "People with at least one record matching an assessed definition / all persons in the CDM" else
        "People with at least one matching record inside selected cohort intervals / distinct selected cohort persons",
      measurement_status = if (!has) "not_assessed" else if (is.na(persons)) "suppressed" else "measured",
      scope = if (level == "Subcomponent") "configured_definition" else "assessed_components_only",
      reliability = if (!is.null(leaf)) leaf$reliability else cap_reliability("mixed",if(has&&"reliability_cap"%in%names(m))m$reliability_cap[1] else NA_character_),
      fitness_for_research = if (!is.null(leaf)) leaf$fitness_for_research else "see_children",
      mapped_fraction = if(has&&"mapped_fraction"%in%names(m))m$mapped_fraction[1] else if (!is.null(leaf)) leaf$mapped_fraction else NA_real_,
      source_coded_fraction = if(has&&"source_coded_fraction"%in%names(m))m$source_coded_fraction[1] else NA_real_,
      source_resolved_fraction = if(has&&"source_resolved_fraction"%in%names(m))m$source_resolved_fraction[1] else NA_real_,
      source_crosswalked_fraction = if(has&&"source_crosswalked_fraction"%in%names(m))m$source_crosswalked_fraction[1] else NA_real_,
      assigned_target_crosswalked_fraction = if(has&&"assigned_target_crosswalked_fraction"%in%names(m))m$assigned_target_crosswalked_fraction[1] else NA_real_,
      auditable_fraction = if(has&&"auditable_fraction"%in%names(m))m$auditable_fraction[1] else NA_real_,
      mapping_records = if(has&&"mapping_records"%in%names(m))m$mapping_records[1] else NA_real_,
      source_coded_records = if(has&&"source_coded_records"%in%names(m))m$source_coded_records[1] else NA_real_,
      source_resolved_records = if(has&&"source_resolved_records"%in%names(m))m$source_resolved_records[1] else NA_real_,
      auditable_records = if(has&&"auditable_records"%in%names(m))m$auditable_records[1] else NA_real_,
      source_crosswalked_records = if(has&&"source_crosswalked_records"%in%names(m))m$source_crosswalked_records[1] else NA_real_,
      assigned_target_crosswalked_records = if(has&&"assigned_target_crosswalked_records"%in%names(m))m$assigned_target_crosswalked_records[1] else NA_real_,
      mapping_audit_policy = if(has&&"mapping_audit_policy"%in%names(m))m$mapping_audit_policy[1] else NA_character_,
      provenance_reliability = if(has&&"provenance_reliability"%in%names(m))m$provenance_reliability[1] else "unknown",
      evidence = ev, issues = issues, stringsAsFactors = FALSE)
  }
  area_names <- coverage_area_names()
  for (area in names(area_names)) {
    gs <- groups[groups$area == area, , drop = FALSE]
    add(paste0("area_", area), "", "Area", area_names[[area]], area_names[[area]], "", unlist(gs$components))
    for (i in seq_len(nrow(gs))) {
      gid <- paste0("group_", gs$group_id[i])
      add(gid, paste0("area_", area), "Component", gs$title[i], area_names[[area]], gs$title[i], gs$components[[i]])
      for (cid in gs$components[[i]]) {
        leaf <- components[components$component_id == cid, , drop = FALSE]
        if (!nrow(leaf)) next
        add(cid, gid, "Subcomponent", leaf$description, area_names[[area]], gs$title[i], cid, leaf)
      }
    }
  }
  do.call(rbind, rows)
}
