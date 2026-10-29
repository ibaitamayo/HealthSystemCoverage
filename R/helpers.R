assert_connection <- function(con) {
  if (!DBI::dbIsValid(con)) stop("con is not a valid open DBI connection")
}

qid <- function(con, x) as.character(DBI::dbQuoteIdentifier(con, x))

qualified_table <- function(con, schema, table) {
  paste(qid(con, schema), qid(con, table), sep = ".")
}

table_exists <- function(con, schema, table) {
  sql <- paste0(
    "SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = ",
    DBI::dbQuoteString(con, schema), " AND table_name = ",
    DBI::dbQuoteString(con, table), ") AS present"
  )
  isTRUE(DBI::dbGetQuery(con, sql)$present[[1]])
}

table_columns <- function(con, schema, table) {
  sql <- paste0(
    "SELECT column_name FROM information_schema.columns WHERE table_schema = ",
    DBI::dbQuoteString(con, schema), " AND table_name = ",
    DBI::dbQuoteString(con, table), " ORDER BY ordinal_position"
  )
  DBI::dbGetQuery(con, sql)$column_name
}

visit_setting_case <- function(con, aliases, alias = "v", care_site_alias = "cs",
                               visit_concept_alias = "vc",
                               place_concept_alias = "pc") {
  text_sql <- paste0(
    "lower(concat_ws(' ', ", alias, ".visit_source_value, ",
    care_site_alias, ".place_of_service_source_value))"
  )
  priority <- c(
    primary_care_ehr = "primary_care",
    emergency_care = "emergency",
    home_hospitalisation = "home_hospitalisation",
    palliative_care = "palliative",
    mental_health = "mental_health",
    maternal_prenatal = "maternal_prenatal",
    inpatient_hospital = "inpatient",
    specialist_outpatient = "specialist_outpatient"
  )
  clauses <- structured_visit_clauses(con,aliases,alias,care_site_alias)
  for (source_type in names(priority)) {
    patterns <- aliases$pattern[aliases$source_type == source_type]
    if (!length(patterns)) next
    pattern <- paste0("(?:", paste(patterns, collapse = ")|(?:"), ")")
    clauses <- c(
      clauses,
      paste0("WHEN ", text_sql, " ~ ", DBI::dbQuoteString(con, pattern),
             " THEN ", DBI::dbQuoteString(con, unname(priority[[source_type]])))
    )
  }
  paste("CASE", paste(clauses, collapse = " "),
        "ELSE 'other_or_unknown' END")
}

safe_probe <- function(name, fun) {
  tryCatch(
    list(name = name, status = "ok", data = fun(), error = NULL),
    error = function(e) list(name = name, status = "error", data = NULL,
                             error = conditionMessage(e))
  )
}

empty_component_result <- function(spec) {
  transform(
    spec,
    status = "not_assessed",
    reliability = "unknown",
    provenance_reliability = "unknown",
    linkage_reliability = "unknown",
    semantic_reliability = "unknown",
    fitness_for_research = "not_evaluated",
    persons = NA_real_,
    records = NA_real_,
    start_date = as.Date(NA),
    end_date = as.Date(NA),
    structured_fraction = NA_real_,
    mapped_fraction = NA_real_,
    source_coded_fraction = NA_real_,
    source_resolved_fraction = NA_real_,
    auditable_fraction = NA_real_,
    mapping_records = NA_real_,
    valid_standard_records = NA_real_,
    source_coded_records = NA_real_,
    source_resolved_records = NA_real_,
    source_crosswalked_records = NA_real_,
    assigned_target_crosswalked_records = NA_real_,
    crosswalk_ambiguous_records = NA_real_,
    crosswalk_origin_unspecified_records = NA_real_,
    source_crosswalked_fraction = NA_real_,
    assigned_target_crosswalked_fraction = NA_real_,
    auditable_records = NA_real_,
    mapping_audit_policy = NA_character_,
    evidence = NA_character_,
    issues = NA_character_
  )
}

set_component <- function(x, component_id, ...) {
  i <- match(component_id, x$component_id)
  if (is.na(i)) return(x)
  values <- list(...)
  for (nm in names(values)) {
    value <- values[[nm]]
    if (inherits(value, "integer64") && is.numeric(x[[nm]])) {
      value <- as.double(value)
    }
    x[[nm]][i] <- value
  }
  x
}

suppress_small_cells <- function(x, threshold) {
  original<-x
  numeric_count_columns <- names(x)[grepl(
    "(^persons$|^records$|_persons$|_records$|^unmapped_|^deaths$|^periods$|^invalid_periods$|_deaths$|_overlap$)", names(x))]
  for (nm in numeric_count_columns) {
    idx <- !is.na(x[[nm]]) & x[[nm]] > 0 & x[[nm]] < threshold
    x[[nm]][idx] <- NA_real_
  }
  fractions<-c(mapped_fraction="valid_standard_records",source_coded_fraction="source_coded_records",
    source_resolved_fraction="source_resolved_records",source_crosswalked_fraction="source_crosswalked_records",
    assigned_target_crosswalked_fraction="assigned_target_crosswalked_records",auditable_fraction="auditable_records")
  for(f in intersect(names(fractions),names(x))) {
    n<-fractions[[f]]
    if(!all(c(n,"mapping_records")%in%names(x)))next
    hidden<-(!is.na(original[[n]])&original[[n]]>0&original[[n]]<threshold)|
      (!is.na(original$mapping_records)&original$mapping_records>0&original$mapping_records<threshold)
    x[[f]][hidden]<-NA_real_
  }
  x
}

apply_adjudications <- function(x, adjudications) {
  if (is.null(adjudications) || !nrow(adjudications)) return(x)
  fields <- c("status", "reliability", "provenance_reliability",
              "linkage_reliability", "semantic_reliability",
              "fitness_for_research", "persons", "records", "start_date",
              "end_date", "structured_fraction", "mapped_fraction",
              "evidence", "issues")
  for (i in seq_len(nrow(adjudications))) {
    values <- as.list(adjudications[i, intersect(fields, names(adjudications)),
                                   drop = FALSE])
    values <- values[!vapply(values, function(z) length(z) == 1L && is.na(z),
                             logical(1))]
    x <- do.call(set_component,
                 c(list(x = x,
                        component_id = adjudications$component_id[[i]]),
                   values))
  }
  x
}

html_escape <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}
