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
