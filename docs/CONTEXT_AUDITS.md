# Classification and time context (0.2.6)

These read-only audits explain limitations of the representation. They neither
repair events nor change cohort membership or the population denominator.
They run with global, cohort and geographic profiling. Their evidence is in
`context_audit/` and their warnings are visible in HTML reports and component
issues. Query failures are not evidence of absence: inspect `probe_errors.csv`.
Geographic profiling fails if its context audit fails, rather than presenting
an apparently successful audit.

## Alias effects (R4)

`visit_reclassification.csv` compares the same visits, person scope, cohort
windows and active event filters, with and without source alias patterns.
Both runs retain coded visit/establishment rules, the provider's exact
`visit_concept_map` and the instance's hierarchy. Text aliases are a final
fallback and never override an informative structured rule. The exported baseline/configured setting and winning alias index,
pattern and source type show which rule took precedence. Counts of records
with `reclassified=true` quantify actual reassignment, not merely regex matches.
Distinct-person counts across rows can overlap and must not be summed.

If a baseline setting has records and the configured setting has zero, an
`alias_emptied_component` alert identifies the original visit component, records
and winning patterns. Its zero remains `unknown`, not absence of that care.
This is a visit-classification audit, not a second full execution of all event
components. Effects on clinical domains are additionally visible in the scoped
setting distributions, without pretending to know their true care provenance.

## Visit hierarchy and unclassified mass (R8)

The instance's `concept_ancestor` is restricted to active concepts in the Visit
domain under roots 9201 (inpatient), 9202 (outpatient) and 9203 (emergency).
`visit_hierarchy.csv` records roots and the resolution for every descendant.
One distinct root produces its broad setting; multiple roots remain
`other_or_unknown` unless a more informative structured visit/establishment
combination resolves the modality. Exact pair-specific provider overrides take
precedence; source aliases are last.
Missing hierarchy is explicitly warned about; exact root fallbacks remain.
No translated concept names or source-language literals are searched by default.

For example, a concept descending from both inpatient and outpatient does not
prove which setting applies. A Home Visit is not automatically hospital-at-home
or primary care. An Outpatient Visit does not establish specialist provenance.
Providers can resolve justified local semantics via `visit_concept_map`, or
add explicit concept-based `component_rules`; the configuration is exported.

`setting_distribution.csv` counts visits, diagnoses, drugs and procedures,
including `other_or_unknown` and `unlinked_or_unknown`. It uses each domain's
own dates and active filters, including cohort windows; missing visit links are
not silently dropped from this audit. The denominator for its fractions is
records in that domain, not people or the component's already-classified subset.
If either unknown/unlinked fraction strictly exceeds
`unknown_setting_threshold` (default 0.20), a profile alert caps reliability of
all setting-dependent components at low. Unknown and stricter unusable
judgements remain unchanged. Counts and statuses are not rewritten by the cap.
Geographic results apply this overall selected-population warning to dependent
rows; this is explicitly not a separate audit or estimate for each area.

## Cutoff transparency (R5)

`parameters.csv` shows the configured `database_cutoff`, its origin,
`max_observed_event_date` and the thresholds. `event_date_maxima.csv` gives raw
maxima and maxima not beyond the cutoff separately for available clinical
event-start date columns, including tables not assigned to functional components.
Birth dates, observation-period bounds and prospective drug end dates are not
used as event starts. Cohort audits use cohort event windows, so their latest
date describes that analysis, not a claim about the full database's freshness.
With no configured cutoff the maximum-date audit uses today's date for the
comparison only; it does not activate an event filter.

If cutoff origin is `Sys.Date() fallback` and its distance from the latest
observed start exceeds `cutoff_lag_days` (default 183), an informational alert
requests the real extraction/ETL date. Raw future outliers cannot hide the lag.
The maximum is not called an ETL date. Before profiling, CDM_SOURCE is read and
an unambiguous, plausible source_release_date can replace the Sys.Date() fallback
only if no observed event start not beyond today contradicts it. User-supplied
cutoffs retain precedence. Missing, multiple, future or contradictory metadata
dates remain audit findings, not exclusion boundaries. No dates from
known-unreliable source metadata are adopted automatically.

## Pharmacy dispensing evidence (R9)

`drug_types.csv` publishes the observed `drug_type_concept_id` distribution
with vocabulary metadata, after the same person/window/event filters.
The default pharmacy/mail-order definition uses 38000175 (Prescription
dispensed in pharmacy) and 38000176 (Prescription dispensed through mail order),
both verified in the development instance. At runtime only requested IDs
resolving to active Type Concept entries are used. The validated metadata is
exported in `dispensing_type_rules.csv`; requested IDs are configuration, not
values inferred from medication names or language-dependent source text.
Providers can change `community_dispensing_type_ids` after reviewing their
source conventions. An empty vector disables the definition. No usable loaded
rule means `not_assessed`, not a failed search interpreted as absence.

This deliberately does **not** treat generic EHR, pharmacy claim, planned
dispensing or generic EHR dispensing as proof of community pharmacy provenance.
The pharmacy/mail-order component means recorded evidence under its declared
type definition. A nonzero result does not verify physical community location,
ingestion, adherence or complete capture. Zero means `not_detected` under this
definition and these filters; generic EHR records still leave their actual
capture mechanism unknown. The distinction between prescribing, dispensing
and administration follows the [OMOP type-concept conventions](https://ohdsi.github.io/CommonDataModel/dataModelConventions.html).

## Configuration

```r
config <- new_profile_config(
  cdm_schema = "cdm",
  event_date_min = "1900-01-01",
  # Supply database_cutoff only when its actual extraction/ETL date is known.
  unknown_setting_threshold = 0.20,
  cutoff_lag_days = 183,
  community_dispensing_type_ids = c(38000175, 38000176)
)
profile <- profile_health_system(con, config)
write_profile_bundle(profile, "coverage-report")
```

Thresholds are configurable judgements, not universal clinical standards.
Small-cell masking remains active, including alert text and exposed fractions.
Overlapping aggregates and dates still require local disclosure review.
