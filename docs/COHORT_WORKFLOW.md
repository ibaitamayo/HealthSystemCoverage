# Cohort workflow — proposed next version

Status: implemented in version 0.2.0. Version 0.1.0 and its outputs are preserved.
The original whole-database API remains available; profile_cohort scopes queries
to OHDSI membership intervals, while review/apply and process functions keep
selection, system representation and observed care trajectories separate.

## Three separate outputs

1. **Selection/attrition diagram:** selected cohort, proposed quality rules,
   accepted exclusions, remaining people and episodes. This is not a process map.
2. **Health-system representation:** the architectural view and hierarchical
   table, scoped to selected people and their specified analysis intervals.
3. **Observed care trajectories:** contacts/activities ordered inside those
   intervals. This is not automatically a disease-specific care pathway,
   causation, or a statement about unrecorded care.

## Input

Accept an already instantiated OHDSI cohort table or local data frame with
`cohort_definition_id`, `subject_id`, `cohort_start_date`, `cohort_end_date`.
Keep multiple membership intervals per person; do not merge them silently.
The caller supplies the table/schema and cohort ID. Different cohorts can have
overlapping membership; denominators count distinct persons, with episodes
reported separately.

An ATLAS/Circe JSON is a definition, not patient membership. Instantiation using
CohortGenerator or ATLAS is a separate, explicitly authorized operation in a
write/results schema. The profiler must not execute arbitrary SQL from a JSON
or create/overwrite clinical/cohort tables by default.

## Quality review before filtering

Propose rules with counts and rationale. Require an explicit list of accepted
rules, retain the unfiltered denominators, and calculate sequential attrition
with distinct-person/episode counts. Do not add overlapping flag counts.

- Missing/reversed cohort dates or missing PERSON link: structural defects.
- Missing OP, no OP covering index and zero post-index observed days: distinct
  states, not interchangeable. OP absence does not prove absence of care.
- Clearly impossible event dates: missing, future relative to a declared
  cutoff, before an internally supported birth date, or reversed intervals.
- Suspected placeholders and age thresholds: configurable proposals requiring
  justification, not universal hard-coded exclusions.
- Date/datetime disagreements and sex-specific diagnosis inconsistencies:
  warnings requiring review, not automatic correction.

Filter the offending event by default when an accepted rule is event-level;
do not discard the entire patient simply because one record is faulty.
An invalid qualifying event may change the first qualifying date; show that
effect. Do not call the first observed diagnosis disease onset or incident
disease without a suitable prior-observation/washout definition.

## Process analysis

- Default: contacts at a broad health-system-component level. Permit additional
  explicitly defined activities, including future MorbFindR/ActivityFindR inputs.
- Case: a cohort membership interval; patient and disease episode are distinct.
- Default date precision: observed date, with no invented hours. Collapse all
  same-day activities into an unordered activity set for sequential maps.
- Retain original events and intervals for audit; contact-start sequence does
  not establish discharge-to-next-contact sequence. Activities may occur during
  a hospital stay; overlaps must not be forced into a non-overlapping route.
- First activity per type is optional; all observed activities is another mode.
- bupaR `eventlog()` and processmapR `process_map()` are optional adapters.
  Patient/event-level logs stay local to the analysis environment. Export only
  reviewed aggregate maps/tables, with configurable small-cell suppression.
- Keep people with no drawable events in the attrition report. Report graph
  populations separately from the architectural-view population.

## Reproducible public example

`run_synthetic_demo()` demonstrates the architectural view without a database.
With `process_maps=TRUE` it also generates selection and process maps. All
demonstration events are fictional, generated in code, not transformations or
samples of patient records. Site-specific configuration and reports are not
distributed.

## Primary references

- [OHDSI cohort definition and membership intervals](https://ohdsi.github.io/TheBookOfOhdsi/Cohorts.html)
- [CohortGenerator](https://ohdsi.github.io/CohortGenerator/)
- [bupaR eventlog and timestamp tie ordering](https://bupaverse.github.io/bupaR/reference/eventlog.html)
- [processmapR process_map](https://bupaverse.github.io/processmapR/reference/process_map.html)
