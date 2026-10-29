# HealthSystemCoverage

Understand **which parts of a health system are represented in an OMOP data
source**, rather than merely which CDM tables contain rows.

The package provides an architectural SVG overview, a linked hierarchical
results table, explicit cohort-quality review and observed care-contact maps.
It complements, rather than replaces, existing OHDSI diagnostics.

## Install from GitHub

```r
install.packages("remotes")
remotes::install_github("ibaitamayo/HealthSystemCoverage", upgrade="never")
```

The core requires R >= 4.1, DBI and jsonlite. Database analyses currently support
PostgreSQL, using a DBI driver such as RPostgres. Process maps additionally need:

```r
install.packages(c("RPostgres", "bupaR", "processmapR", "htmlwidgets", "DiagrammeR"))
```

Saving self-contained process widgets also requires Pandoc. RStudio usually
provides it; command-line environments must configure it separately. No Shiny,
Leaflet or Plotly service is required. Reports work without a web server.

## Try it without a database

```r
library(HealthSystemCoverage)
run_synthetic_demo("synthetic-report")
browseURL(normalizePath("synthetic-report/profile.html"))
```

Every demonstration event is fictional, generated in code and clearly labelled
**not patient data**. No credentials, real site results or cohort membership
are distributed. After installing the optional process dependencies and Pandoc:

```r
run_synthetic_demo("synthetic-workflow", process_maps=TRUE)
browseURL(normalizePath("synthetic-workflow/workflow.html"))
```

## Profile an OMOP data source

```r
library(DBI)
library(RPostgres)
library(HealthSystemCoverage)

con <- dbConnect(
  Postgres(), dbname=Sys.getenv("OMOP_DB"), host=Sys.getenv("OMOP_HOST"),
  user=Sys.getenv("OMOP_USER"), password=Sys.getenv("OMOP_PASSWORD"),
  port=as.integer(Sys.getenv("OMOP_PORT", "5432"))
)
dbExecute(con, "SET default_transaction_read_only=on")

config <- new_profile_config(cdm_schema="cdm", source_name="My data source")
profile <- profile_health_system(con, config)
write_profile_bundle(profile, "system-report")
dbDisconnect(con)
```

Use a SELECT-only account. Do not copy patient credentials into source files,
issues or notebooks. Schema/source recognition and activity definitions must
be configured and reviewed locally; no Spanish, English or other label patterns
are used by default. Outpatient does not automatically mean specialist care.

## Use an ATLAS/OHDSI cohort

Import already generated membership from a table:

```r
cohort <- cohort_from_table("results", "study_cohort", cohort_id=42)
```

Or supply a local data frame:

```r
cohort <- cohort_from_data(membership, cohort_id=42)
```

Both use `cohort_definition_id`, `subject_id`, `cohort_start_date` and
`cohort_end_date`. Subject IDs must refer to PERSON in the same CDM. Repeated
membership intervals remain separate. An ATLAS JSON is a definition, not
membership: instantiate it separately through ATLAS/CohortGenerator. This
package does not download WebAPI definitions or generate cohort tables.

Review quality, then explicitly approve the relevant filters:

```r
review <- review_cohort(con, config, cohort)
print(review)                       # no exclusions applied

# Illustrative rules, not universal defaults: review their impact first.
selected <- apply_cohort_filters(con, review,
  rules=c("no_observation_at_index", "zero_post_index_days"))
profile <- profile_cohort(con, config, selected$cohort)
events <- collect_care_events(con, config, selected$cohort)

review_care_events(events)
checked <- filter_care_events(events,
  rules=c("missing_date", "future_date", "end_before_start", "before_birth_year"))
write_cohort_workflow(profile, selected, checked$events, "cohort-report")
```

The last example assumes an open connection and configuration, as above. Keep
them open until the workflow ends, then call `dbDisconnect(con)`.

## Interpretation

* The 42 subcomponents are grouped into 26 functions and 6 logical areas.
* Bars show matched people / the declared population, **not care completeness**.
* Parent counts deduplicate people and source records; they do not sum children.
* Unknown, unassessed and evaluated zero are distinct states.
* Cohort exclusions and event exclusions are separate; a faulty event does not
  automatically discard the patient. Observation bounds do not prove enrolment.
* Selection flow, system representation and contact processes are separate.
* Same-date activities form unordered sets: no clinical order or hours invented.
* Arrows describe successive observed start-date blocks, not causation,
  completed treatment, verified hospital discharge or disease-specific pathways.
* First-contact, all-contact and display-threshold overview modes remain
  available. A display threshold does not remove patients or reconnect edges.

No clinical or cohort database tables are created or changed by these analyses.
Patient-level membership and event objects stay in the approved environment.
Native process-map frequencies are **not small-cell suppressed**; reports are
marked for internal research and need disclosure review before sharing. Count
table masking is not an anonymisation guarantee.

## Documentation and development

### Scoped quality and mapping provenance (0.2.4)

Profiles now distinguish nonzero source IDs, resolved source concepts, codes
present in a declared-vocabulary crosswalk and documentation of the assigned
target. Crosswalk-only provenance is not treated as a missing-source failure.
`audit_source_crosswalk()` reports target resolution, distinct candidates and
optional declared domain contracts independently of clinical events.

With event calendar filtering enabled, before-birth-year events are excluded
by default (`birth_date_rule="year"`); strict date comparison or off mode are
explicit options. `quality_findings.csv` separates retained defects,
incremental exclusions and prior-rule overlaps. The same scoped logic applies
to global, cohort and geographic results. See
[quality and provenance](docs/QUALITY_AND_PROVENANCE.md).

### Geographic comparison (0.2.3)

Provide local GeoJSON/shapefile boundaries with unique IDs using
`read_health_areas()` (optional `sf` dependency). Configure the population's
location column and, if needed, an explicit code crosswalk in
`profile_health_areas()`. Or use `area_coverage_from_data()` for already
aggregated data. `write_area_report()` creates an offline comparison map and
one filterable table, including hierarchical results and CSV exports.

The map shows descriptive differences, not significance or true care access.
Unmapped/unknown people stay in the denominator. No additional person
exclusions are applied. See [geographic comparison](docs/GEOGRAPHIC_COMPARISON.md)
for inputs, cohort scope, geometry attribution and privacy limitations.

### Optional event calendar exclusions

After selecting eligible people, enable a calendar filter with
`new_profile_config(..., event_date_min="1900-01-01", database_cutoff=etl_date)`.
`etl_date` may be a verified ETL, extraction or database-generation date. If none
is available, omit `database_cutoff`: it resolves to `Sys.Date()` when the config
is created. Recreate the config for each run. The resolved date and its origin
are recorded in the report; it is never inferred from the latest event.
This option filters
quantitative coverage events and extracted process contacts, not people. It
preserves the selected population denominator. Drug end dates may exceed the
cutoff; drug starts may not. Pre-minimum drug ends are not exempt.

Use `review_event_dates(con, config, cohort)` for optional domain audit counts,
or `profile_cohort(con, config, cohort, audit_event_dates=TRUE)` to attach them
to report exports. A whole-database profile can request the `event_dates`
module alongside `coverage`. Audits review all records of selected people,
including those outside cohort windows; counts are not component attrition.
The extra full-domain scan is optional; `note` is not scanned by default and can
be requested explicitly via the audit's `tables` argument. Existing diagnostic probes remain raw
source diagnostics, clearly distinguished from filtered quantitative coverage.
This rule does not remove missing dates or resolve other temporal conflicts.

See [cohort workflow](docs/COHORT_WORKFLOW.md), [configuration](docs/CONFIGURATION.md),
[conceptual model](docs/DESIGN.md), [privacy guidance](SECURITY.md) and
[contributing](CONTRIBUTING.md). Source functions are inspired by the BARDENA
data-resource model, but source names and local mappings remain generalisable.

Automated tests use fictional data only and need no OMOP connection. Run
R CMD build and R CMD check locally. This initial publication does not configure
GitHub Actions; continuous integration can be added later.
The repository is independent of any study project or site's renv lockfile.
