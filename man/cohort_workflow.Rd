\name{cohort_from_table}
\alias{cohort_from_table}
\alias{cohort_from_data}
\alias{review_cohort}
\alias{apply_cohort_filters}
\alias{profile_cohort}
\alias{collect_care_events}
\title{Read and review OHDSI cohort membership and profile its observed care}
\usage{
cohort_from_table(schema, table, cohort_id, name = table)
cohort_from_data(data, cohort_id = NULL, name = "Imported OHDSI cohort")
review_cohort(con, config, cohort, cutoff = Sys.Date())
apply_cohort_filters(con, review, rules)
profile_cohort(con, config, cohort, audit_event_dates = FALSE)
collect_care_events(con, config, cohort, components = character(),
  setting_labels = NULL)
}
\arguments{
\item{schema, table}{Schema and table holding instantiated OHDSI membership.}
\item{cohort_id}{One cohort definition identifier. Required for tables and multi-cohort data.}
\item{name}{Display label, without sensitive information.}
\item{data}{Local data frame with cohort_definition_id, subject_id, cohort_start_date, cohort_end_date.}
\item{con}{Open PostgreSQL DBI connection, preferably a verified read-only session.}
\item{config}{Configuration from new_profile_config.}
\item{cohort}{Cohort source object; repeated membership intervals remain separate.}
\item{audit_event_dates}{Also scan clinical domains for calendar-bound exclusions among selected people. Optional because the full domain audit can be expensive. Requires event_date_min and database_cutoff.}
\item{cutoff}{Declared maximum plausible date.}
\item{review}{Result of review_cohort.}
\item{rules}{Explicit character vector of accepted exclusion rules. No rules are automatically approved.}
\item{components}{Additional configured clinical component identifiers to extract as activities. Default extracts visit contacts only.}
\item{setting_labels}{Optional named vector of display labels for logical settings.}
}
\description{Reads membership, proposes quality flags and applies approved filters to SELECT queries only. No table is created or changed. Cohort profiles count distinct selected people and restrict clinical event starts to membership intervals. An ATLAS JSON must be instantiated separately using ATLAS or CohortGenerator; it is not patient membership.}
\details{Cohort and event objects can contain patient-level identifiers and must remain in the approved analysis environment. Standard report bundles export aggregates only. Global reviewed component findings are not reused as cohort-specific results. Multiple overlapping intervals can attribute one contact to several cases, while coverage population counts deduplicate people. Filtering for an observation period at index does not prove continuous enrolment.}
\value{A cohort source, quality review, filtered cohort with sequential attrition, health_system_profile, or local event data frame, depending on the function.}
