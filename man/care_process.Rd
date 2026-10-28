\name{care_eventlog}
\alias{care_eventlog}
\alias{care_day_blocks}
\alias{review_care_events}
\alias{filter_care_events}
\alias{plot_care_process}
\alias{care_process_summary}
\alias{plot_care_overview}
\title{Observed care contacts, quality review and unordered daily process blocks}
\usage{
review_care_events(events, cutoff = Sys.Date())
filter_care_events(events, rules, cutoff = Sys.Date())
care_day_blocks(events, first_only = FALSE)
care_eventlog(events, first_only = FALSE)
plot_care_process(log)
care_process_summary(events, first_only = FALSE, privacy_min_cell = 5L)
plot_care_overview(events, min_case_fraction = 0.01, privacy_min_cell = 5L)
}
\arguments{
\item{events}{Local data frame with case_id, activity and event_date. Optional end_date and year_of_birth permit additional date checks.}
\item{cutoff}{Declared date cutoff.}
\item{rules}{Explicit accepted event-level quality rules. Whole patients are not removed for a single bad event.}
\item{first_only}{Use only the first observed occurrence of each activity in each case.}
\item{log}{A bupaR eventlog, for example from care_eventlog.}
\item{privacy_min_cell}{Minimum case count released by the aggregate summary. Small positive counts become missing.}
\item{min_case_fraction}{Display-only minimum fraction of drawable cases for a first-contact transition. No events or cases are excluded; hidden edges are not reconnected.}
}
\description{Separates event exclusions from cohort exclusions. Forms one unordered activity set per case and observed date before creating a bupaR log, so equal dates do not acquire an invented clinical order.}
\details{Event logs remain local. Date values are not imputed to observed clock times. Lifecycle complete denotes the registration of a point observation, not hospital discharge or completion of care. Inter-contact arrows describe successive observed start-date blocks, not causal or disease-specific pathways. Interval overlaps remain in the source event data; process durations are not estimated. bupaR and processmapR are optional. plot_care_process returns the native frequency map; unlike care_process_summary it does not suppress small aggregate frequencies. Review disclosure risk before saving or sharing that map.}
\value{A flag-count table, filtered events and attrition, daily blocks, bupaR eventlog, processmapR widget, or aggregate nodes and flows.}
