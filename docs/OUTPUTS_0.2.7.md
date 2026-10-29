# Representation-focused output in 0.2.7

This release changes presentation and default export schemas, not clinical queries,
eligibility rules, concept recognition or the calculation of counts. The broader
experimental distributed-definition validator is not promoted into the evaluator
by this release. Attached evidence identifies its validation scope explicitly.

## Default bundle

- `profile.html`: architectural overview with four presentation states, one scope
  declaration, separate no-match/not-evaluable lists and expandable activity groups.
- `coverage_results.csv`: one hierarchical table, with 12 columns. The node and
  parent IDs and level support grouping; user-facing fields are environment,
  activity, representation, people, percentage, observed dates and one limitation.
- `health_system_coverage.svg`: archival architectural view.
- `audit/index.html` and `audit/validation.json`: definitions, applied policy,
  existing validation, detailed component results, findings and failures.
- `README.md`: interpretation and compatibility notes.

Green means represented, not suitable for every study. Amber means represented
with limitations, including unusable evidence under an audit policy or identified
semantic defects; the main limitation must explain these. Red means no matches
under an assessed definition, not an absent healthcare service. Grey covers
unassessed or unresolved evidence, never absence. No non-applicability is inferred.

Percentages describe people with matching evidence / selected population. They
are not system completeness, sensitivity, service utilisation needs or access.
Group counts remain the current SQL union of unique persons, not sums. Some child
activities may be unassessed even when a group has observed evidence. Suppressed
values remain unavailable; no count is reconstructed by summing children.

The normal table omits record volumes, research-fitness placeholders, repeated
technical notices and mapping fractions. These remain accessible in the annex or
the exhaustive mode, rather than being silently discarded from the result.

## Compatibility

`write_profile_bundle(profile, path, level = "exhaustive")` retains legacy exports.
The in-memory `coverage_results(profile)` also retains technical fields.
Use a fresh directory when changing levels; summary mode refuses common legacy
files rather than deleting user files or leaving duplicate old exports visible.

`write_area_report()` has the same level argument. Summary geography keeps the
comparison map and one filterable table without mapping-fraction columns; the
technical validation is linked separately. Process and cohort workflow outputs
remain separate requested modules, not additions to every profile export.

## Verification

Nine test files, including new summary tests: exact union counts, no-match versus
unknown distinction, preservation of stricter reliability findings, privacy masks,
compact exports, legacy mode, source-name escaping and stale-directory protection.

The real-data preview re-renders the previously computed cohort of 6,598 women.
It executes no new clinical queries, changes no OMOP records and retains the 28
people without drawable contacts in the coverage denominator. Process maps remain
internal research outputs with native small frequencies not suppressed. This
release does not make a new privacy guarantee for those maps.
