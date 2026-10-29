\name{write_profile_bundle}
\alias{write_profile_bundle}
\title{Write an aggregate profile bundle}
\usage{write_profile_bundle(profile, path, level = c("summary", "exhaustive"))}
\arguments{
\item{profile}{Result from \code{profile_health_system()}.}
\item{path}{Existing or new output directory.}
\item{level}{Compact representation by default; \code{exhaustive} preserves legacy exports. Use a fresh directory when changing levels.}
}
\description{Writes a representation-focused report, one hierarchical table and attached validation by default. Percentages describe people with evidence, not completeness or research suitability. Exhaustive mode retains the legacy technical CSVs. This changes the default export schema in version 0.2.7; the assessment engine and queries are unchanged.}
