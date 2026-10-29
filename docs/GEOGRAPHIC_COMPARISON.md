# Geographic comparison

Version 0.2.3 adds one comparison map and one filterable table. It does not
re-run or duplicate the whole report once per area. The area grouping is
separate from the health-system hierarchy (area/function/component).

## Provider inputs

1. Local polygon geometry (GeoJSON, shapefile with companion files, zipped
   shapefile, or `sf` object), declared CRS, a unique ID and a display name.
2. Explicit population-to-area assignment. The built-in adapter uses
   `person.location_id -> location.<configured column>`; an optional crosswalk
   translates provider codes into polygon IDs.
3. Instantiated cohort membership and its accepted eligibility rules, if used.
   The area profiler adds no universal birth/death or observation-period filter.
4. Local component definitions, same calendar bounds and cohort windows in
   every area, and geometry attribution/licence.

Do not assume `county` contains health areas. Never match labels by language,
substring or geographic proximity. Multiple source values may map to one
area, but each source value maps to at most one area. If a crosswalk is
provided, unmatched assignments become `__UNKNOWN__`; without one, unmatched
raw groups remain distinct. Both remain visible and in the denominator.

## Example: read-only OMOP analysis

```r
library(HealthSystemCoverage)
areas <- read_health_areas(
  "provider-areas.geojson", id_column="area_code", name_column="area_name"
)
crosswalk <- read.csv("provider-area-crosswalk.csv", colClasses="character")
# Required columns: location_value, area_id. IDs must match the geometry.
config <- new_profile_config("cdm", event_date_min="1900-01-01")
# Without a known ETL/build date, config resolves Sys.Date() on this run.
cohort <- cohort_from_table("results", "cohort", cohort_id=1)
# This cohort must already satisfy the study's accepted person exclusions.
result <- profile_health_areas(
  con, config, areas, cohort,
  location_column="county", crosswalk=crosswalk,
  components=c("primary_care_encounters", "inpatient_admissions"),
  attribution="Provider and geometry licence",
  population_note="Accepted cohort and documented last-known area assignment"
)
write_area_report(result, "area-report")
```

Use a verified read-only connection and appropriate query timeouts. NULL
`components` covers all available definitions and can be expensive for large
domains. For each assessed functional node, a single SQL grouping computes
every area and the global total. Parent nodes union/deduplicate their assessed
children; no area or component percentages are summed. Missing child definitions
do not establish absence of data. The map/table contain assessed nodes only.

## Bring your own aggregate results

`area_coverage_from_data()` accepts `area_id`, `node_id`, `label`, `persons`,
`population`, and optional `records`, `area_name`, `level`, `parent_id`.
Include explicit `__ALL__` global rows and the same mutually exclusive area
partition in every indicator. All people, including those without a mapped
polygon, must be represented. Known counts must reconcile. This adapter also
supports cohorts/assignments created outside the built-in LOCATION adapter.

## Meaning and privacy

The map compares percentages of selected people with matching records, not
population disease risk, clinical service completeness, or quality/access to
care. Last-known location is not residence at every historical event. Do not
use this view as a map of hospital activity or catchment without an explicit
different assignment and denominator.

Clicking an area filters the single table; the comparative map retains all
areas and the global baseline. Unavailable/suppressed/no-denominator areas
remain distinct from zero. Component, group and function results can all be
selected when assessed. CSV export preserves their grouping fields.

Small positive counts, complements and small denominators are masked in
exports. Internal R objects are unmasked. Aggregate differences and overlapping
totals can still disclose small cells; masking is not a release authorisation.
No person IDs, source crosswalk or patient-level records are exported.

The HTML is standalone: source boundaries are projected to SVG paths in R,
not fetched from a map service. The optional `sf` package is required for
geometry preparation; reading existing aggregate geometry objects and writing
reports needs no map JavaScript library, CDN or network connection.
