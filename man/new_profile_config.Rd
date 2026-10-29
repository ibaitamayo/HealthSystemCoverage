\name{new_profile_config}
\alias{new_profile_config}
\title{Create a health-system profile configuration}
\usage{new_profile_config(cdm_schema, source_aliases = NULL, adjudications = NULL,
privacy_min_cell = 5L, database_cutoff = NULL, dbms = "postgresql",
component_rules = NULL, source_name = NULL, visit_concept_map = NULL,
event_date_min = NULL, birth_date_rule = c("year", "date", "off"),
mapping_audit_policy = c("require_local", "report"), source_vocabularies = NULL,
crosswalk_domains = NULL, unknown_setting_threshold = 0.20,
cutoff_lag_days = 183L, community_dispensing_type_ids = c(38000175, 38000176),
discharge_outcome_map = NULL, provider_specialty_map = NULL)}
\arguments{
\item{cdm_schema}{OMOP CDM schema.}
\item{source_aliases}{Generic source-type patterns used to classify encounters.}
\item{adjudications}{Optional reviewed component-level conclusions.}
\item{privacy_min_cell}{Minimum exported cell count.}
\item{database_cutoff}{Optional verified ETL, extraction or database-generation date. User values retain precedence. With event filtering enabled, NULL initially resolves to Sys.Date(); profiling can adopt a unique plausible CDM_SOURCE extraction date only when observed event starts through today do not contradict it. Rejected metadata dates are reported, not used for exclusions. Recreate configuration for each execution.}
\item{dbms}{Database engine; currently PostgreSQL.}
\item{component_rules}{Optional explicit component_id, table, concept_id definitions.}
\item{source_name}{Display name of the data source.}
\item{visit_concept_map}{Optional exact concept_id and logical setting mappings. An optional place_of_service_concept_id column defines pair-specific rules, with NA for concept-only rules. Pair overrides precede universal coded rules; source text aliases are last.}
\item{event_date_min}{Optional inclusive lower calendar bound for clinical events, e.g. \code{"1900-01-01"}. Enables event filtering. NULL preserves previous behaviour. Drug end dates are exempt only from the upper bound.}
\item{birth_date_rule}{With calendar filtering active, compare event years to year_of_birth (default), compare full dates where birth_datetime is available with year fallback, or disable the birth rule. No birthdays are imputed.}
\item{mapping_audit_policy}{Explicit provenance policy: require_local marks wholly undocumented provenance unusable; report limits it to low. Neither asserts clinical semantic invalidity or changes research fitness.}
\item{source_vocabularies}{Optional named character vector mapping OMOP table names to declared source vocabulary IDs, restricting the crosswalk search. Without it, declared crosswalk vocabularies remain candidates, not an inferred event origin.}
\item{crosswalk_domains}{Optional data frame with source_vocabulary_id and expected_domain; multiple domains per vocabulary allowed. Defines provider-reviewed crosswalk contracts. No domain expectation is inferred from vocabulary names.}
\item{unknown_setting_threshold}{Fraction of unclassified or unlinked event settings above which setting-dependent component reliability is capped at low. Default 0.20; declared in reports.}
\item{cutoff_lag_days}{Informational warning threshold for the distance between a Sys.Date() cutoff and the latest observed event start not beyond it. Default 183 days.}
\item{community_dispensing_type_ids}{Provider-reviewable drug type concept IDs indicating pharmacy/mail-order dispensing. Defaults 38000175 and 38000176, validated against the instance vocabulary before use. Generic EHR or generic dispensing types do not prove community provenance. Use numeric(0) to disable this definition.}
\item{discharge_outcome_map}{Optional provider-reviewed death-marker source mapping: visit_concept_id, source_value and grain (event, person or unknown). No local source literal is interpreted by default. These markers are separately audited, not automatically confirmed hospital deaths.}
\item{provider_specialty_map}{Coded specialty_concept_id and component_id pairs for mental_health and maternal_prenatal_care. Defaults to 38004469 and 38004461 respectively; IDs must resolve to active Provider-domain concepts at query time. Counts are provider-linked encounters, not inferred diagnoses, pregnancy or prenatal care. An empty data frame with these columns disables this route.}
}
\description{Creates and validates configuration for the profiler.}
