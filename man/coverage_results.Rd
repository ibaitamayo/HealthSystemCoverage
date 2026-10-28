\name{coverage_results}
\alias{coverage_results}
\title{Hierarchical health-system coverage results}
\usage{coverage_results(profile)}
\arguments{\item{profile}{A health_system_profile object.}}
\description{Returns area, component and subcomponent rows. Percentages describe people matching assessed definitions divided by all CDM persons, not completeness of the healthcare system. Group counts are deduplicated in SQL. Unknown measurements remain missing.}
\value{A data frame containing counts, percentages, status, scope and evidence.}
