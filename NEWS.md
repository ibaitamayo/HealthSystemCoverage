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
