\name{review_event_dates}
\alias{review_event_dates}
\title{Count records outside configured event calendar bounds}
\usage{review_event_dates(con, config, cohort = NULL,
tables = setdiff(names(event_date_fields()), "note"))}
\arguments{
\item{con}{Open PostgreSQL DBI connection; use a verified read-only session.}
\item{config}{Configuration with event_date_min and a resolved database_cutoff (provided date or recorded Sys.Date() fallback).}
\item{cohort}{Optional OHDSI cohort. Limits the audit to distinct selected people, not to event membership dates. NULL reviews all records.}
\item{tables}{Explicit standard domains to audit. Defaults to the nine structured domains; note is excluded from the default scan for performance and can be requested explicitly.}
}
\description{Runs aggregate SELECT queries and returns domain counts, affected distinct people and timings. It never changes clinical records or cohort membership.}
\details{Bounds are inclusive. Available standard event date fields are inspected, including end dates. Drug end dates (including verbatim_end_date) are exempt from the upper bound only. Birth dates and observation periods are not event dates. Missing dates, reversed intervals and date/datetime disagreements are outside this calendar-only rule. Counts cover all records of selected people, including records outside membership windows; they are not counts removed from each coverage component. Domain and reason counts can overlap. No cross-domain unique-person total is inferred by summing these counts. Returned counts are unmasked internal aggregates; write_profile_bundle masks small counts in exports.}
\value{A data frame of aggregate counts per available domain.}
