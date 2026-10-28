\name{profile_health_system}
\alias{profile_health_system}
\title{Profile logical health-system coverage in an OMOP database}
\usage{profile_health_system(con, config,
modules = c("inventory", "person", "observation_period", "visits",
"conditions", "drugs", "death", "note_schema", "coverage"))}
\arguments{
\item{con}{Open DBI connection.}
\item{config}{Configuration returned by \code{new_profile_config()}.}
\item{modules}{Probe modules to run.}
}
\description{Runs aggregate, read-only probes against an OMOP PostgreSQL database.}
\value{An object of class \code{health_system_profile}.}
