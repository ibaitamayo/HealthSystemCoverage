# HealthSystemCoverage 0.2.7

* Default exports now focus on health-system representation: an architectural
  view, one 12-column hierarchical table, and attached evaluation evidence under
  audit/. Clinical queries, cohort eligibility and measured counts are unchanged.
* Show people with evidence, population percentages, observed dates and one
  concise limitation. Groups preserve SQL unique-person unions; counts at
  different levels are not additive. Percentages never mean system completeness.
* Use four presentation states, with absence and inability to evaluate listed
  separately. Unknown/unassessed activities are not converted to absent services;
  no non-applicability is invented. Research fitness is not inferred from presence.
* Move mapping fractions, detailed flags, definitions and existing validation out
  of the main table into a linked technical annex. Preserve suppressed cells.
* BREAKING default export schema: use write_profile_bundle(...,
  level = "exhaustive") for legacy technical CSVs. Choose a fresh directory when
  changing levels so old duplicate files cannot silently survive.
* Cohort workflow passes aggregate selection counts to the report. Map and care
  process modules remain available separately; their evaluation logic is unchanged.

# HealthSystemCoverage 0.2.6

* Combine coded visit modality and establishment type before source-text aliases:
  primary-care urgency, hospital emergency/combined ER-inpatient, unspecified
  urgency, hospital contacts of unspecified modality and contradictory pairs.
  Add pair-specific provider mappings; preserve ERIP as a combined record,
  without fabricating or automatically merging episodes. Parent SQL deduplicates
  shared source keys across emergency and inpatient definitions.
* Read CDM_SOURCE dates and adopt an unambiguous plausible source_release_date
  only when observed starts do not contradict it and no user cutoff was supplied.
  Export candidate validation and provenance; rejected metadata never excludes
  newer records. Sys.Date() remains the dynamic fallback.
* Distinguish absent, empty and nonempty domain tables by actual existence probes,
  rather than planner estimates. Empty domains give a scoped not_detected even
  when setting classification is incomplete.
* Add structured type distributions, provider-linked service encounters,
  unstructured note representation, costs through domain-correct event links,
  economic-field availability and location coverage. Costs do not imply copayment;
  the old costs_and_copayment component ID is retained for compatibility only.
* Add discharge-destination and preceding-visit integrity audits. Death markers
  require a coded outcome or explicit provider mapping; report person/event/unknown
  grain, distinct people, source rows and date agreement without equating them.
  Person/unknown-grain findings have no summable records. No arbitrary increase
  of confirmed hospital mortality is made from source markers alone.
* Do not expose unexpected NOTE column names/content; flag malformed structures.
  Unstructured representation no longer becomes misclassification in aggregates.

* Audit visit classification with and without source aliases, export winning
  patterns and reclassified record counts, and warn when aliases empty a setting.
* Resolve unambiguous Visit-domain descendants of inpatient, outpatient and
  emergency roots using the instance vocabulary. Multi-root concepts remain
  unknown; home visits do not imply hospital-at-home or primary care.
* Publish filtered setting distributions, including unlinked events. A declared
  configurable unknown fraction threshold (default 0.20) caps setting-dependent
  reliability at low, without changing counts, eligibility or reviewed stricter
  conclusions. Geographic caps use the selected population's overall audit,
  not separate per-area estimates.
* Show database cutoff alongside latest observed clinical event starts, with
  raw maxima separate. Exclude prospective drug ends from that comparison and
  warn on a Sys.Date() lag over a configurable 183 days. No ETL date is invented.
* Examine drug_type_concept_id distributions and add pharmacy/mail-order
  dispensing definitions validated against the loaded Type Concept vocabulary.
  Generic EHR and generic dispensing do not prove community provenance. Results
  remain limited to the declared definition, not completeness or actual location.
* Export context_audit CSV evidence and visible report warnings consistently
  across global, cohort and geographic outputs. No OMOP tables are modified.

# HealthSystemCoverage 0.2.5

* Convert PostgreSQL integer64 mapping counts before copying them to numeric
  component columns. Component CSV fractions now survive small-cell masking
  when their counts are publishable; genuinely small cells remain suppressed.
* Report zero matches from visit-setting-dependent definitions as unknown,
  not not_detected, consistently for database, cohort and geographic profiles.
  Retain zero counts and explain classifier/alias uncertainty. Unassessed
  definitions remain not_assessed. Explicit concept-rule and direct-table zeros
  remain not_detected under their stated definitions, not proof of absent care.
* Add CSV round-trip and integer64 regression tests for all six mapping fractions.

# HealthSystemCoverage 0.2.4

* Add visible source-coded, resolved-source and crosswalk-provenance fractions,
  distinguishing code presence from documentation of the assigned target.
* Audit resolved vocabulary sources and active declared-vocabulary crosswalks;
  ambiguous crosswalk candidates limit provenance reliability without declaring
  clinical mappings invalid. Missing documentation is unusable only under the
  explicit require_local audit policy; research fitness is not upgraded.
* Add a standalone source_to_concept_map audit: target resolution, distinct
  candidates per vocabulary/code, and optional declared target-domain contracts.
  Crosswalk findings are instance metadata, never clinical event verdicts.
* Add year-of-birth event exclusions when calendar filtering is enabled, with
  optional strict date comparison or off mode. No new person exclusions.
* Export scoped quality findings with ordered incremental exclusions and prior-
  rule overlaps, consistently across global, cohort and geographic profiles.
* R3 vocabulary-reachability verdicts, alias audits, cutoff-lag alerts, visit
  hierarchy inference and dispensing-type discovery remain deferred.

# HealthSystemCoverage 0.2.3

* Add provider-defined GeoJSON/shapefile/sf area boundaries and explicit
  LOCATION-to-area crosswalks, without language-dependent recognition.
* Add area-grouped read-only cohort/database coverage and aggregate-data input.
  Unknown and unmapped populations remain in the global denominator.
* Add one offline comparative map and one filterable hierarchical results table,
  with descriptive percentage-point differences and CSV export.
* Preserve event calendar bounds, explicit cohort membership and small-cell
  masking, including small complements. No new person exclusions are implied.

# HealthSystemCoverage 0.2.2

* Add opt-in event calendar bounds with a supplied ETL/database-generation date
  or a recorded Sys.Date() fallback resolved anew for each configuration.
* Keep future drug end dates; do not exempt drug start dates or pre-minimum ends.
* Apply bounds in quantitative coverage and visit/process extraction, without
  changing cohort membership or its denominator.
* Add optional domain-level aggregate exclusion audits and report disclosure.

# HealthSystemCoverage 0.2.1

* Prepare a standalone GitHub-installable R package.
* Add fictional, database-free demonstration events and report generation.
* Add documentation, contribution/privacy guidance and repository checks.
* Remove site-specific results and multilingual source-expression examples from
  the public distribution. Core source recognition remains opt-in and neutral.

# HealthSystemCoverage 0.2.0

* Add standard OHDSI cohort-table and data-frame inputs.
* Add explicit cohort/event quality review and accepted-rule attrition.
* Add cohort-scoped coverage, daily unordered activity sets, first/all contact
  maps and an explicitly thresholded display-only overview.

# HealthSystemCoverage 0.1.0

* Add architectural SVG representation and hierarchical aggregate results.
