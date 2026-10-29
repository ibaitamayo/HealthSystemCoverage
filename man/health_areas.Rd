\name{read_health_areas}
\alias{read_health_areas}
\alias{area_coverage_from_data}
\alias{area_coverage_results}
\alias{profile_health_areas}
\alias{write_area_report}
\title{Compare health-system representation across geographic areas}
\usage{
read_health_areas(path, id_column, name_column = id_column, simplify = 0)
area_coverage_from_data(data, areas, privacy_min_cell = 5L,
  source_name = "Health-system area comparison", population_note = "",
  attribution = "")
area_coverage_results(profile)
profile_health_areas(con, config, areas, cohort = NULL,
  location_column = "county", crosswalk = NULL, components = NULL,
  population_note = "", attribution = "")
write_area_report(profile, path)
}
\arguments{
\item{path}{For reading, a local GeoJSON, shapefile, zipped shapefile or sf object. For writing, an output directory.}
\item{id_column, name_column}{Geometry columns containing unique area identifiers and display names. IDs must not equal reserved values __ALL__ or __UNKNOWN__.}
\item{simplify}{Optional nonnegative display simplification tolerance in metres.}
\item{data}{Aggregate data frame with area_id, node_id, label, persons and population. Optional records, area_name, level and parent_id. Include explicit global rows identified by __ALL__, plus the same exclusive area groups for every indicator. Counts are numeric nonnegative integers or NA.}
\item{areas}{Geometry object returned by read_health_areas.}
\item{privacy_min_cell}{Suppress positive small cells, small complements and small area denominators in exports.}
\item{source_name}{Display title; no patient identifiers.}
\item{population_note}{Description of cohort criteria, exclusions and geographic assignment.}
\item{attribution}{Provider-supplied cartographic attribution and licence.}
\item{profile}{Object returned by area_coverage_from_data or profile_health_areas.}
\item{con}{Open PostgreSQL DBI connection; use a verified read-only session.}
\item{config}{Configuration from new_profile_config. Accepted event calendar bounds are respected.}
\item{cohort}{Optional instantiated OHDSI cohort; NULL uses all persons. No additional person exclusions are silently applied.}
\item{location_column}{One of county, city, state, zip, location_source_value or country_source_value in LOCATION. Selection is a provider decision; county does not universally mean health area.}
\item{crosswalk}{Optional data frame with unique location_value and area_id. If supplied, unmatched source values enter __UNKNOWN__. If omitted, values are matched directly to geometry IDs and unmapped groups are retained.}
\item{components}{Optional explicit subset of available leaf definitions. Parent groups are computed as unions of the assessed children, never sums. NULL uses all available definitions.}
}
\description{Produces one geographic comparison and one filterable profile/table, not a separate report per area. Geographic boundaries, population assignments and health-system definitions remain separate configurable inputs.}
\details{The optional sf dependency is needed only to read/prepare boundaries. Missing CRS, duplicate IDs and empty/nonpolygon geometries are rejected. Boundaries are transformed to EPSG:3857 for a standalone SVG display; this display projection is intended for nonpolar regional maps. Source geometries are not bundled with the package.

Database queries group indicators by exclusive last-known person location and globally in the same query. Cohort membership intervals scope clinical events. No spatial join, residential history, hospital catchment or healthcare entitlement is inferred. Unmapped groups stay in the population selector and global denominator. Providers must document the meaning and time validity of assignment. A recorded contact does not establish complete care capture or true access.

The constructor keeps unmasked internal aggregates; area_coverage_results and write_area_report apply export masking. Global rows and area partitions must reconcile whenever the input counts are known. Missing/suppressed counts stay missing; percentages are recomputed from counts. Groups can be unmapped and map features can have no population, without being silently assigned zero coverage.

Output includes offline area_report.html, masked CSV and masked JSON with derived paths. There are no external scripts, map tiles or network requests. A component selector colours areas by descriptive percentage-point difference from the global percentage; clicking an area filters one profile table. Native selectors and keyboard-accessible polygons provide alternatives to hover. No hypothesis test or statistical-significance claim is made. Intensity is scaled within the selected indicator. Suppression does not prevent reconstruction from overlapping aggregates: disclosure review is required before external sharing.}
\value{A hsc_health_areas geometry object, hsc_area_profile aggregate object, masked results data frame, or invisibly the output path, depending on the function.}
