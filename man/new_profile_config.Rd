\name{new_profile_config}
\alias{new_profile_config}
\title{Create a health-system profile configuration}
\usage{new_profile_config(cdm_schema, source_aliases = NULL, adjudications = NULL,
privacy_min_cell = 5L, database_cutoff = NULL, dbms = "postgresql",
component_rules = NULL, source_name = NULL, visit_concept_map = NULL)}
\arguments{
\item{cdm_schema}{OMOP CDM schema.}
\item{source_aliases}{Generic source-type patterns used to classify encounters.}
\item{adjudications}{Optional reviewed component-level conclusions.}
\item{privacy_min_cell}{Minimum exported cell count.}
\item{database_cutoff}{Optional known database cut-off date.}
\item{dbms}{Database engine; currently PostgreSQL.}
\item{component_rules}{Optional explicit component_id, table, concept_id definitions.}
\item{source_name}{Display name of the data source.}
\item{visit_concept_map}{Optional exact concept_id and logical setting mappings.}
}
\description{Creates and validates configuration for the profiler.}
