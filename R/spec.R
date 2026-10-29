#' Logical health-system coverage specification
#'
#' Returns the system-oriented catalogue used by the profiler. It deliberately
#' avoids local application names and OMOP table names in its component labels.
#'
#' @return A data frame with one row per logical health-system component.
#' @export
health_system_spec <- function() {
  rows <- list(
    c("population_registry", "population", "all", "population register", "Administrative population denominator and stable person identity"),
    c("demographics", "population", "all", "population register", "Birth, sex and other demographic characteristics"),
    c("geographic_assignment", "population", "all", "population register", "Residence and assignment to a reference care area"),
    c("longitudinal_follow_up", "population", "all", "population register", "Observable entry, continuous follow-up and end of follow-up"),
    c("system_exit_attrition", "population", "all", "population register", "Deaths, transfers, loss of entitlement and other exits from the system"),
    c("primary_care_encounters", "care_delivery", "primary_care", "primary care EHR", "Adult, paediatric, nursing and home primary-care contacts"),
    c("primary_care_emergency", "care_delivery", "primary_care", "primary care EHR", "Urgent encounters at coded primary-care centres"),
    c("primary_care_diagnoses", "diagnoses", "primary_care", "primary care EHR", "Structured or recoverable diagnoses originating in primary care"),
    c("primary_care_procedures", "procedures", "primary_care", "primary care EHR", "Procedures and interventions performed in primary care"),
    c("primary_care_nursing", "measurements", "primary_care", "primary care EHR", "Nursing interventions and vital signs in primary care"),
    c("specialist_outpatient_encounters", "care_delivery", "specialist_outpatient", "specialist outpatient EHR", "Specialist consultations outside an admission"),
    c("specialist_outpatient_diagnoses", "diagnoses", "specialist_outpatient", "specialist outpatient EHR", "Diagnoses originating in specialist outpatient care"),
    c("emergency_care", "care_delivery", "emergency", "emergency care EHR", "Emergency department contacts and their clinical content"),
    c("emergency_care_unspecified", "care_delivery", "unknown", "emergency care EHR", "Urgent encounters whose primary-care or hospital setting is not identified"),
    c("hospital_encounters_unspecified", "care_delivery", "hospital", "hospital EHR", "Coded hospital contacts without an identified inpatient or outpatient modality"),
    c("inpatient_admissions", "care_delivery", "inpatient", "hospital EHR/discharge abstract", "Recorded inpatient or combined emergency/inpatient contacts; not deduplicated admissions"),
    c("inpatient_diagnoses", "diagnoses", "inpatient", "hospital discharge abstract", "Diagnoses linked to inpatient or combined emergency/inpatient contacts"),
    c("inpatient_procedures", "procedures", "inpatient", "hospital EHR/discharge abstract", "Diagnostic and therapeutic procedures during admission"),
    c("home_hospitalisation", "care_delivery", "home_hospitalisation", "hospital EHR", "Hospital-at-home episodes"),
    c("palliative_care", "care_delivery", "palliative", "hospital or community EHR", "Palliative and hospice care contacts"),
    c("mental_health", "care_delivery", "mental_health", "mental health EHR", "Mental-health encounters, diagnoses and treatment"),
    c("maternal_prenatal_care", "care_delivery", "maternal_prenatal", "maternal care EHR", "Obstetric/gynaecological contacts; prenatal activities require specific evidence"),
    c("surgery", "procedures", "surgery", "hospital EHR/discharge abstract", "Major and minor surgical activity"),
    c("laboratory", "diagnostics", "all", "laboratory information system", "Biochemistry and haematology orders and results"),
    c("microbiology", "diagnostics", "all", "microbiology information system", "Microbiology tests, organisms and susceptibility results"),
    c("histopathology", "diagnostics", "all", "anatomical pathology system", "Specimens, histology, cytology and pathology diagnoses"),
    c("radiology_imaging", "diagnostics", "all", "radiology information system", "Imaging procedures and reports"),
    c("blood_management", "clinical_support", "all", "blood management system", "Donations, transfusions and blood products"),
    c("outpatient_prescriptions", "medicines", "outpatient", "outpatient prescribing system", "Prescribed product, date, dose, duration and prescriber"),
    c("community_dispensings", "medicines", "outpatient", "community dispensing system", "Dispensed product, date, packages and prescription linkage"),
    c("hospital_medications", "medicines", "hospital", "hospital pharmacy system", "Inpatient and specialist-care medication use"),
    c("vaccinations", "prevention", "all", "immunisation registry", "Vaccine, dose, date, location, manufacturer and batch"),
    c("cancer_screening_breast", "screening", "population", "screening programme", "Invitation, test, result and follow-up for breast cancer screening"),
    c("cancer_screening_cervical", "screening", "population", "screening programme", "Invitation, test, result and follow-up for cervical cancer screening"),
    c("cancer_screening_colorectal", "screening", "population", "screening programme", "Invitation, test, result and follow-up for colorectal cancer screening"),
    c("disease_registries", "registries", "all", "disease registry", "Condition-specific registries and their linkage"),
    c("mortality_all_cause", "mortality", "all", "population or mortality register", "Date of death regardless of place"),
    c("mortality_cause", "mortality", "all", "mortality register", "Underlying and contributing causes of death"),
    c("mortality_in_hospital", "mortality", "inpatient", "hospital discharge abstract", "Deaths identified from hospital episodes or discharge status"),
    c("mortality_outside_hospital", "mortality", "community", "population or mortality register", "Deaths not observable solely from hospital activity"),
    c("social_care", "social", "all", "social care system", "Social-work contacts, interventions and outcomes"),
    c("lifestyle", "risk_factors", "all", "clinical EHR", "Smoking, alcohol intake and physical activity"),
    c("costs_and_copayment", "economics", "all", "administrative and costing systems", "Resource costs; the legacy component ID does not imply copayment"),
    c("clinical_notes", "documentation", "all", "clinical EHR", "Clinical documentation recorded as unstructured notes"),
    c("epidemiological_surveillance", "prevention", "population", "public health surveillance", "Population-level epidemiological surveillance"),
    c("occupational_health", "prevention", "all", "occupational health system", "Occupational exposures, surveillance and interventions")
  )
  x <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(x) <- c("component_id", "dimension", "setting", "expected_source_type", "description")
  x
}

system_map_groups <- function() {
  data.frame(
    area = c(
      rep("community", 4), rep("hospital", 4), rep("shared", 4)
    ),
    group_id = c(
      "primary_care", "community_medicines", "specialist_outpatient",
      "prevention_screening", "emergency", "inpatient_episode",
      "hospital_medicines", "specialised_pathways", "population_follow_up",
      "diagnostics", "mortality", "system_support"
    ),
    title = c(
      "Primary care", "Community pharmacy", "Outpatient specialist care",
      "Prevention and screening", "Emergency care", "Hospital stay",
      "Hospital medicines", "Specialised pathways", "Population and follow-up",
      "Diagnostics", "Mortality", "System support"
    ),
    icon = c(
      "clinic", "pharmacy", "consultation", "prevention", "emergency",
      "bed", "hospital_drug", "pathway", "population", "diagnostics",
      "mortality", "support"
    ),
    components = I(list(
      c("primary_care_encounters", "primary_care_emergency", "primary_care_diagnoses",
        "primary_care_procedures", "primary_care_nursing"),
      c("outpatient_prescriptions", "community_dispensings"),
      c("specialist_outpatient_encounters", "specialist_outpatient_diagnoses"),
      c("vaccinations", "cancer_screening_breast",
        "cancer_screening_cervical", "cancer_screening_colorectal"),
      c("emergency_care","emergency_care_unspecified"),
      c("inpatient_admissions", "inpatient_diagnoses",
        "inpatient_procedures", "surgery"),
      c("hospital_medications"),
      c("home_hospitalisation", "palliative_care", "mental_health",
        "maternal_prenatal_care"),
      c("population_registry", "demographics", "geographic_assignment",
        "longitudinal_follow_up", "system_exit_attrition"),
      c("laboratory", "microbiology", "histopathology",
        "radiology_imaging", "blood_management"),
      c("mortality_all_cause", "mortality_cause", "mortality_in_hospital",
        "mortality_outside_hospital"),
      c("disease_registries", "social_care", "lifestyle","clinical_notes","hospital_encounters_unspecified",
        "costs_and_copayment")
    )),
    stringsAsFactors = FALSE
  )
}
