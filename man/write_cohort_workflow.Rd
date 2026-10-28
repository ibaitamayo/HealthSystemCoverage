\name{write_cohort_workflow}
\alias{write_cohort_workflow}
\title{Save cohort selection, system representation and observed care process maps}
\usage{write_cohort_workflow(profile, selection, events, path)}
\arguments{
\item{profile}{A cohort-scoped health_system_profile.}
\item{selection}{The result of apply_cohort_filters or its aggregate attrition table.}
\item{events}{Reviewed local care events, not exported at patient level.}
\item{path}{Output directory for the aggregate bundle.}
}
\description{Creates a workflow page, independent selection diagram, architectural cohort profile, bupaR/processmapR first-contact and all-contact maps, and aggregate tables. Requires optional bupaR, processmapR, htmlwidgets and a working Pandoc installation for self-contained widgets.}
\details{Native process map frequency labels are not suppressed and the bundle is marked for internal research. Summary CSVs mask small positive counts. Individual identifiers and dates are not intentionally included in maps; animated case-token widgets are rejected. Counts of drawable cases remain separate from cohort coverage denominators.}
\value{Invisibly, the output directory.}
