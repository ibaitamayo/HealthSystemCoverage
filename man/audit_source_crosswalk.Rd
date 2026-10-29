\name{audit_source_crosswalk}
\alias{audit_source_crosswalk}
\title{Audit source-to-concept mapping metadata independently of clinical counts}
\usage{audit_source_crosswalk(con, config)}
\arguments{
\item{con}{Open PostgreSQL DBI connection, preferably verified read-only.}
\item{config}{Profile configuration. Optional crosswalk_domains declares expected target domains.}
}
\description{Returns structural metadata: mapping rows, target-ID resolution, vocabulary distributions, distinct candidates per vocabulary/code and optional domain-contract findings. No patient rows are accessed or modified.}
\details{Raw structural statistics include all map rows, with invalid rows counted separately. Event provenance uses active entries with nonempty declared vocabularies. A documented assigned target does not prove clinical validity. Candidate counts are distinct target IDs, not mapping-row counts; resolution-conditioned and unconditional summaries are labelled separately. Domain-contract violations apply to the crosswalk only, not automatically to event components. The default does not infer expected domains from vocabulary names. Relationship reachability and external ETL documentation are not examined. Crosswalk CSV exports are instance metadata, not cohort counts.}
\value{A list of summary, vocabulary, domain, candidate, determination and findings tables, with explicit sources examined/not examined.}
