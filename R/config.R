#' Create a health-system profile configuration
#'
#' @param cdm_schema OMOP CDM schema.
#' @param source_aliases Optional data frame with columns `source_type` and
#'   `pattern`. Patterns are case-insensitive PostgreSQL regular expressions.
#' @param privacy_min_cell Minimum cell count shown in exports.
#' @param database_cutoff Optional known database cut-off date.
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
                               visit_concept_map = NULL) {
  stopifnot(is.character(cdm_schema), length(cdm_schema) == 1L, nzchar(cdm_schema))
  stopifnot(is.numeric(privacy_min_cell), length(privacy_min_cell) == 1L,
            privacy_min_cell >= 0)
  if (is.null(source_aliases)) source_aliases <- default_source_aliases()
  required <- c("source_type", "pattern")
  if (!all(required %in% names(source_aliases))) {
    stop("source_aliases must contain: ", paste(required, collapse = ", "))
  }
  if (!is.null(visit_concept_map)) {
    stopifnot(all(c("concept_id", "setting") %in% names(visit_concept_map)),
              !anyNA(visit_concept_map$concept_id),
              !anyDuplicated(visit_concept_map$concept_id))
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
