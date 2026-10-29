# Quality and provenance (0.2.4)

## Separate representation, provenance and research fitness

`detected_structured` describes structured records, not correct clinical meaning.
`source_concept_id = 0` may be legitimate if the source vocabulary is absent.
The profiler examines both resolved vocabulary concepts and
`source_to_concept_map`; it never declares misclassification merely because a
source concept or a `Maps to` relationship is absent. Vocabulary reachability,
clinical interpretation and external ETL documentation are not evaluated here.

For mapped-event definitions, outputs expose:

* `mapped_fraction`: valid standard targets in the expected domain / mapping
  records. This is a syntactic target check, not validated clinical accuracy.
* `source_coded_fraction`: nonzero source concept IDs / mapping records.
* `source_resolved_fraction`: nonzero source concept IDs found in `concept` /
  mapping records. Resolution is not a `Maps to` proof.
* `source_crosswalked_fraction`: codes present in active crosswalk entries with
  a nonempty declared vocabulary / mapping records.
* `assigned_target_crosswalked_fraction`: the assigned source-code/target pair
  actually occurs in that crosswalk / mapping records.
* `auditable_fraction`: resolved source or documented assigned target / mapping
  records (union, not the sum of the preceding fractions).

Crosswalk code presence alone is not proof that the assigned target is
documented. Target-array joins use one aggregate row per code, so a crosswalk
with many candidates cannot multiply clinical counts. A provider can restrict
the search with `source_vocabularies`, a named vector such as
`c(condition_occurrence="provider_source_vocabulary")`. Without an explicit
event source vocabulary, matching declared crosswalk vocabularies are candidates,
not an inferred origin. Multiple candidate targets/vocabularies or unresolved
origin limit crosswalk-only provenance to low. One-to-many mappings are not
automatically errors; determinism cannot be established from the event alone.

The explicit default `mapping_audit_policy="require_local"` marks wholly
undocumented provenance `unusable` for local auditability; partial documentation
is low. `"report"` limits undocumented provenance to low instead. Neither policy
asserts clinical invalidity, OMOP nonconformance or universal research
unsuitability; `fitness_for_research` is not upgraded. Reviewed semantic
findings and stricter reliability judgements are preserved.

## Crosswalk is an independent audit object

`audit_source_crosswalk(con, config)` reviews the instance metadata, not cohort
events. Reports export `crosswalk_audit/` tables and a visible summary:

* mapping rows, resolving rows and distinct target IDs (total versus resolving);
* distributions by declared vocabulary and target domain;
* distinct target candidates per `(source_vocabulary_id, source_code)`;
* medians over all pairs and over resolvable pairs, labelled separately;
* invalid map rows, and optional provider-declared domain contracts.

Do not confuse row counts with distinct candidates, or combine code strings
across vocabularies without labelling the changed unit. Structural summaries
include invalid entries; event provenance uses entries whose invalid_reason is
NULL. An active mapping is documentation, not proof of historical validity for
every event date or correct clinical meaning.

Domain compatibility is evaluated only when `crosswalk_domains` supplies
`source_vocabulary_id` and `expected_domain` (multiple permitted domains per
vocabulary are allowed). It is not inferred from vocabulary names. A violation
is attached to the crosswalk metadata, not to all events using that vocabulary.
The clinical target actually assigned to an event is checked separately.

## Date filters and scope

Setting `event_date_min` enables lower/upper calendar bounds and, by default,
`birth_date_rule="year"`: any inspected event date whose year is before the
person's recorded birth year is excluded. Dates in the birth year remain valid
under this rule. `"date"` uses birth_datetime when available and year comparison
otherwise; `"off"` disables the birth comparison. No birthday is imputed.
Drug end dates remain exempt only from the upper calendar bound, not from
lower-bound or before-birth checks. These are event rules, not person exclusions.

`quality_findings.csv` and report tables carry component, geographic group,
rule, scope, action, `in_scope`, `excluded_by_filter`, `rule_order`,
`incremental_exclusion`, records and distinct people. Ordering is lower bound,
upper bound, birth. `excluded_by_filter` action counts incremental exclusions;
`excluded_by_prior_rule` documents overlaps and must not be added again.
`retained` means the defect is still inside the displayed component count.
Summing affected-person counts across reasons/components is invalid.

Component findings use the same configured definition, cohort windows and
person population as quantitative coverage. The optional full-domain date
audit deliberately covers all records of selected people, including outside
those windows; its broader scope remains labelled. Raw diagnostic probe files
are not the filtered component counts. Crosswalk metadata findings carry
`instance_crosswalk_not_clinical_counts`, with no invented person count.

Global, cohort and geographic quantitative profiles use the same event and
mapping rules. Counts retain eligible people with no matching events in their
denominator. Small-cell export masking also masks provenance fractions whose
numerators/denominators are suppressed; overlapping outputs still require
disclosure review. Internal R objects retain unmasked aggregate counts.

## Deferred

No any-vocabulary `Maps to` misclassification test, alias reclassification audit,
cutoff-lag threshold, visit-hierarchy expansion or dispensing-type discovery is
introduced by this release. Person-exclusion definitions in ATLAS remain a
separate workflow.
