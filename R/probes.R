probe_inventory <- function(con, config) {
  schema <- config$cdm_schema
  sql <- paste0(
    "SELECT c.relname AS table_name, c.reltuples::bigint AS estimated_rows, ",
    "pg_total_relation_size(c.oid)::bigint AS total_bytes ",
    "FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace ",
    "WHERE n.nspname=", DBI::dbQuoteString(con, schema),
    " AND c.relkind='r' ORDER BY c.relname"
  )
  DBI::dbGetQuery(con, sql)
}

probe_person <- function(con, config) {
  tab <- qualified_table(con, config$cdm_schema, "person")
  DBI::dbGetQuery(con, paste0(
    "SELECT count(*)::bigint AS persons, ",
    "min(year_of_birth)::integer AS min_birth_year, ",
    "max(year_of_birth)::integer AS max_birth_year, ",
    "count(*) FILTER (WHERE gender_concept_id=0)::bigint AS unmapped_gender, ",
    "count(*) FILTER (WHERE race_concept_id=0)::bigint AS unmapped_race, ",
    "count(*) FILTER (WHERE ethnicity_concept_id=0)::bigint AS unmapped_ethnicity ",
    "FROM ", tab
  ))
}

probe_observation_period <- function(con, config) {
  tab <- qualified_table(con, config$cdm_schema, "observation_period")
  DBI::dbGetQuery(con, paste0(
    "SELECT count(*)::bigint AS periods, count(DISTINCT person_id)::bigint AS persons, ",
    "min(observation_period_start_date) AS start_date, ",
    "max(observation_period_end_date) AS end_date, ",
    "count(*) FILTER (WHERE observation_period_end_date < observation_period_start_date)::bigint AS invalid_periods ",
    "FROM ", tab
  ))
}

probe_visits <- function(con, config) {
  s <- config$cdm_schema
  v <- qualified_table(con, s, "visit_occurrence")
  cpt <- qualified_table(con, s, "concept")
  cs <- qualified_table(con, s, "care_site")
  DBI::dbGetQuery(con, paste0(
    "SELECT COALESCE(v.visit_source_value,'[missing]') AS visit_source_value, ",
    "v.visit_concept_id, COALESCE(vc.concept_name,'[missing]') AS visit_concept_name, ",
    "COALESCE(cs.place_of_service_source_value,'[missing]') AS place_source_value, ",
    "COALESCE(pc.concept_name,'[missing]') AS place_concept_name, ",
    "count(*)::bigint AS records, count(DISTINCT v.person_id)::bigint AS persons, ",
    "min(v.visit_start_date) AS start_date, max(v.visit_end_date) AS end_date ",
    "FROM ", v, " v LEFT JOIN ", cpt, " vc ON vc.concept_id=v.visit_concept_id ",
    "LEFT JOIN ", cs, " cs ON cs.care_site_id=v.care_site_id ",
    "LEFT JOIN ", cpt, " pc ON pc.concept_id=cs.place_of_service_concept_id ",
    "GROUP BY 1,2,3,4,5 ORDER BY records DESC"
  ))
}

probe_visit_settings <- function(con, config) {
  s <- config$cdm_schema
  v <- qualified_table(con, s, "visit_occurrence")
  cpt <- qualified_table(con, s, "concept")
  cs <- qualified_table(con, s, "care_site")
  setting <- visit_setting_case(con, config$source_aliases)
  DBI::dbGetQuery(con, paste0(
    "WITH classified AS (SELECT v.person_id, v.visit_start_date, ",
    "v.visit_end_date, ", setting, " AS logical_setting FROM ", v, " v ",
    "LEFT JOIN ", cpt, " vc ON vc.concept_id=v.visit_concept_id ",
    "LEFT JOIN ", cs, " cs ON cs.care_site_id=v.care_site_id ",
    "LEFT JOIN ", cpt, " pc ON pc.concept_id=cs.place_of_service_concept_id) ",
    "SELECT logical_setting, count(*)::bigint AS records, ",
    "count(DISTINCT person_id)::bigint AS persons, ",
    "min(visit_start_date) AS start_date, max(visit_end_date) AS end_date ",
    ", count(*) FILTER (WHERE visit_start_date > visit_end_date)::bigint AS invalid_order_records ",
    ", count(*) FILTER (WHERE visit_start_date > CURRENT_DATE OR visit_end_date > CURRENT_DATE)::bigint AS future_date_records ",
    ", count(*) FILTER (WHERE visit_start_date < DATE '1900-01-01' OR visit_end_date < DATE '1900-01-01')::bigint AS pre1900_records ",
    "FROM classified GROUP BY logical_setting ORDER BY records DESC"
  ))
}

probe_domain_by_visit_source <- function(con, config, table, id_column,
                                         date_column, concept_column,
                                         source_concept_column,
                                         source_value_column,
                                         expected_domain) {
  s <- config$cdm_schema
  d <- qualified_table(con, s, table)
  v <- qualified_table(con, s, "visit_occurrence")
  cpt <- qualified_table(con, s, "concept")
  cs <- qualified_table(con, s, "care_site")
  setting <- visit_setting_case(con, config$source_aliases)
  sql <- paste0(
    "SELECT CASE WHEN v.visit_occurrence_id IS NULL THEN 'unlinked_or_unknown' ",
    "ELSE ", setting, " END AS logical_setting, count(*)::bigint AS records, ",
    "count(DISTINCT d.person_id)::bigint AS persons, min(d.", qid(con, date_column), ") AS start_date, ",
    "max(d.", qid(con, date_column), ") AS end_date, ",
    "count(*) FILTER (WHERE d.", qid(con, concept_column), " <> 0)::bigint AS mapped_records, ",
    "count(*) FILTER (WHERE dc.standard_concept='S' AND dc.domain_id=",
    DBI::dbQuoteString(con, expected_domain), ")::bigint AS valid_standard_records, ",
    "count(*) FILTER (WHERE d.", qid(con, source_concept_column), " <> 0)::bigint AS source_coded_records, ",
    "count(*) FILTER (WHERE d.", qid(con, source_concept_column), "<>0 AND sc.concept_id IS NOT NULL)::bigint AS source_resolved_records, ",
    "count(*) FILTER (WHERE d.", qid(con, source_value_column),
    " IS NOT NULL AND btrim(d.", qid(con, source_value_column),
    ")<>'')::bigint AS source_value_records, 'raw_domain_before_event_filters'::text AS data_scope ",
    "FROM ", d, " d LEFT JOIN ", v,
    " v ON v.visit_occurrence_id=d.visit_occurrence_id AND v.person_id=d.person_id ",
    "LEFT JOIN ", cpt, " vc ON vc.concept_id=v.visit_concept_id ",
    "LEFT JOIN ", cs, " cs ON cs.care_site_id=v.care_site_id ",
    "LEFT JOIN ", cpt, " pc ON pc.concept_id=cs.place_of_service_concept_id ",
    "LEFT JOIN ", cpt, " dc ON dc.concept_id=d.", qid(con, concept_column),
    " LEFT JOIN ", cpt, " sc ON sc.concept_id=d.", qid(con, source_concept_column),
    " GROUP BY 1 ORDER BY records DESC"
  )
  DBI::dbGetQuery(con, sql)
}

probe_death <- function(con, config) {
  s <- config$cdm_schema
  if (!table_exists(con, s, "death")) return(data.frame())
  cols <- table_columns(con, s, "death")
  d <- qualified_table(con, s, "death")
  p <- qualified_table(con, s, "person")
  cause_sql <- if ("cause_concept_id" %in% cols) {
    "count(*) FILTER (WHERE cause_concept_id<>0)::bigint"
  } else "NULL::bigint"
  source_sql <- if ("cause_source_value" %in% cols) {
    "count(*) FILTER (WHERE cause_source_value IS NOT NULL AND btrim(cause_source_value)<>'')::bigint"
  } else "NULL::bigint"
  DBI::dbGetQuery(con, paste0(
    "SELECT count(*)::bigint AS deaths, count(DISTINCT d.person_id)::bigint AS persons, ",
    "min(d.death_date) AS start_date, max(d.death_date) AS end_date, ",
    cause_sql, " AS mapped_causes, ", source_sql, " AS source_causes, ",
    "count(*) FILTER (WHERE d.death_date < DATE '1900-01-01')::bigint AS pre1900_deaths, ",
    "count(*) FILTER (WHERE d.death_date > CURRENT_DATE)::bigint AS future_deaths, ",
    "count(*) FILTER (WHERE d.death_date < make_date(p.year_of_birth,1,1))::bigint AS before_birth_deaths ",
    "FROM ", d, " d LEFT JOIN ", p, " p ON p.person_id=d.person_id"
  ))
}

probe_death_setting <- function(con, config) {
  s <- config$cdm_schema
  if (!table_exists(con, s, "death") ||
      !table_exists(con, s, "visit_occurrence")) return(data.frame())
  d <- qualified_table(con, s, "death")
  v <- qualified_table(con, s, "visit_occurrence")
  cpt <- qualified_table(con, s, "concept")
  cs <- qualified_table(con, s, "care_site")
  setting <- visit_setting_case(con, config$source_aliases)
  DBI::dbGetQuery(con, paste0(
    "WITH classified_visits AS (SELECT v.person_id, v.visit_start_date, ",
    "v.visit_end_date, ", setting, " AS logical_setting FROM ", v, " v ",
    "LEFT JOIN ", cpt, " vc ON vc.concept_id=v.visit_concept_id ",
    "LEFT JOIN ", cs, " cs ON cs.care_site_id=v.care_site_id ",
    "LEFT JOIN ", cpt, " pc ON pc.concept_id=cs.place_of_service_concept_id), ",
    "per_death AS (SELECT d.person_id, ",
    "COALESCE(bool_or(cv.logical_setting='inpatient'),false) AS inpatient_overlap, ",
    "COALESCE(bool_or(cv.logical_setting='palliative'),false) AS hospice_overlap, ",
    "COALESCE(bool_or(cv.logical_setting='primary_care'),false) AS primary_overlap, ",
    "count(cv.person_id)>0 AS any_visit_overlap FROM ", d, " d LEFT JOIN classified_visits cv ",
    "ON cv.person_id=d.person_id AND d.death_date BETWEEN cv.visit_start_date AND cv.visit_end_date ",
    "GROUP BY d.person_id) SELECT count(*)::bigint AS deaths, ",
    "count(*) FILTER (WHERE inpatient_overlap)::bigint AS inpatient_date_overlap, ",
    "count(*) FILTER (WHERE hospice_overlap)::bigint AS hospice_date_overlap, ",
    "count(*) FILTER (WHERE primary_overlap)::bigint AS primary_care_date_overlap, ",
    "count(*) FILTER (WHERE NOT inpatient_overlap AND NOT hospice_overlap)::bigint AS no_inpatient_or_hospice_overlap, ",
    "count(*) FILTER (WHERE NOT any_visit_overlap)::bigint AS no_visit_date_overlap ",
    "FROM per_death"
  ))
}

probe_note_schema <- function(con, config) {
  s <- config$cdm_schema
  if (!table_exists(con, s, "note")) return(data.frame())
  expected <- c(
    "note_id", "person_id", "note_date", "note_datetime",
    "note_type_concept_id", "note_class_concept_id", "note_title",
    "note_text", "encoding_concept_id", "language_concept_id", "provider_id",
    "visit_occurrence_id", "visit_detail_id", "note_source_value",
    "note_event_id", "note_event_field_concept_id"
  )
  actual <- table_columns(con, s, "note")
  data.frame(
    actual_columns = length(actual),
    expected_columns = length(expected),
    unexpected_columns = sum(!actual %in% expected),
    missing_columns = sum(!expected %in% actual)
  )
}
