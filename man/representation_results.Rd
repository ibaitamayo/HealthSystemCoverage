\name{representation_results}
\alias{representation_results}
\title{Compact hierarchical health-system representation}
\usage{representation_results(profile)}
\arguments{\item{profile}{A health_system_profile object.}}
\value{A 12-column data frame with areas, activity groups and subcomponents, representation, unique people, population percentage, observed dates, a main limitation and measurement status.}
\description{Summarises existing assessment results without changing counts or clinical definitions. Four presentation states do not certify research fitness. Group counts are not sums of children. Unassessed children remain explicit, and suppressed counts are not reconstructed. Full technical results remain available through coverage_results and exhaustive export.}
