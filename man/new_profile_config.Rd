\name{new_profile_config}
\alias{new_profile_config}
\title{Create a health-system profile configuration}
\usage{new_profile_config(cdm_schema, source_aliases = NULL, adjudications = NULL,
privacy_min_cell = 5L, database_cutoff = NULL, dbms = "postgresql",
component_rules = NULL, source_name = NULL, visit_concept_map = NULL,
event_date_min = NULL, birth_date_rule = c("year", "date", "off"),
mapping_audit_policy = c("require_local", "report"), source_vocabularies = NULL,
crosswalk_domains = NULL)}
\arguments{
\item{cdm_schema}{OMOP CDM schema.}
\item{source_aliases}{Generic source-type patterns used to classify encounters.}
\item{adjudications}{Optional reviewed component-level conclusions.}
\item{privacy_min_cell}{Minimum exported cell count.}
\item{database_cutoff}{Optional verified ETL, extraction or database-generation date. When event filtering is enabled and no date is supplied, resolves to Sys.Date() when this configuration is created. The resolved date and its origin are recorded. Recreate configuration for each new execution.}
\item{dbms}{Database engine; currently PostgreSQL.}
\item{component_rules}{Optional explicit component_id, table, concept_id definitions.}
\item{source_name}{Display name of the data source.}
\item{visit_concept_map}{Optional exact concept_id and logical setting mappings.}
\item{event_date_min}{Optional inclusive lower calendar bound for clinical events, e.g. \code{"1900-01-01"}. Enables event filtering. NULL preserves previous behaviour. Drug end dates are exempt only from the upper bound.}
\item{birth_date_rule}{With calendar filtering active, compare event years to year_of_birth (default), compare full dates where birth_datetime is available with year fallback, or disable the birth rule. No birthdays are imputed.}
\item{mapping_audit_policy}{Explicit provenance policy: require_local marks wholly undocumented provenance unusable; report limits it to low. Neither asserts clinical semantic invalidity or changes research fitness.}
\item{source_vocabularies}{Optional named character vector mapping OMOP table names to declared source vocabulary IDs, restricting the crosswalk search. Without it, declared crosswalk vocabularies remain candidates, not an inferred event origin.}
\item{crosswalk_domains}{Optional data frame with source_vocabulary_id and expected_domain; multiple domains per vocabulary allowed. Defines provider-reviewed crosswalk contracts. No domain expectation is inferred from vocabulary names.}
}
\description{Creates and validates configuration for the profiler.}
