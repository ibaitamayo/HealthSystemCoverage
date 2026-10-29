# Health-system coverage model

## Purpose

The primary question is not “which OMOP tables contain rows?” but “which parts
of the originating health system can be studied reliably from this database?”

The package therefore separates three concepts:

1. **Expected source function** — the logical system that normally generates
   the information, such as a population register or community dispensing
   system.
2. **Observed representation** — where and how evidence appears in OMOP,
   including structured, unstructured and misclassified records.
3. **Research fitness** — whether provenance, linkage, semantic mapping,
   completeness and time coverage are sufficiently reliable for analysis.

## Generic source functions

- Population and entitlement register
- Primary-care EHR
- Specialist outpatient EHR
- Emergency-care EHR
- Hospital EHR and discharge abstract
- Hospital-at-home and palliative-care systems
- Outpatient prescribing system
- Community dispensing system
- Hospital pharmacy system
- Laboratory and microbiology systems
- Anatomical pathology system
- Radiology information system
- Immunisation registry
- Organised screening programmes
- Mortality register
- Social-care system
- Blood-management system
- Condition-specific registries

Local application names may be supplied as aliases, but they never become the
canonical output labels.

## Status vocabulary

- `not_assessed`: no probe has evaluated the component.
- `not_detected`: the probe found no evidence.
- `detected_unstructured`: evidence exists only as text or another
  non-analytical representation.
- `detected_misclassified`: evidence exists in an inappropriate OMOP domain or
  has demonstrably invalid semantics.
- `detected_partial`: only part of the expected information is represented.
- `detected_structured`: structured records are present.
- `unknown`: the evidence cannot distinguish presence from absence.

Since 0.2.5, zero matches for a visit-setting-dependent definition are
`unknown`, with zero counts retained. The current classifier and alias
precedence can leave settings unrecognised or reassign their records; a zero
does not establish absence. This applies to database, cohort and geographic
profiles, including geographic aggregates containing these definitions.
Components without an evaluated definition remain `not_assessed`. Direct-table
and explicit concept-rule zeros remain `not_detected` under those definitions
and active windows/filters, never a claim that the service does not exist.
This safeguard does not replace the deferred alias-reclassification audit.

These statuses must be interpreted together with `reliability` (`high`,
`moderate`, `low`, `unusable`).

Since 0.2.4 an explicit local-audit policy can mark undocumented mapping
provenance unusable without asserting clinical invalidity. Source concepts and
crosswalk documentation are examined separately; a crosswalk with ambiguous
candidates is documented but not necessarily determined. Crosswalk structural
findings do not automatically change clinical component status. See
[quality and provenance](QUALITY_AND_PROVENANCE.md) for denominators and scope.

Representation and usability are separate output axes. The field
`fitness_for_research` uses `not_evaluated`, `suitable`, `limited` or
`unsuitable`. Where evidence permits, provenance, linkage and semantic
reliability are also recorded independently. Automated detection never upgrades
a component to `suitable`; that conclusion requires reviewed evidence.

## Automatic evidence and reviewed adjudication

The database probes establish reproducible aggregate facts. They cannot infer
the business meaning of a local extract merely from its OMOP destination.
Reviewed adjudications therefore form an explicit second layer and may amend
the interpretation of a component without changing any source data or raw
evidence output.

`unknown`, `not_assessed` and `not_detected` have different meanings. In
particular, `not_detected` means that a defined probe found no evidence in the
database; it does not assert that the real health system lacks the function.

## Initial modules

1. Population denominator and demographics.
2. Observation periods and attrition signals.
3. Encounters by logical care setting.
4. Diagnoses by originating setting.
5. Medicines by prescribing/dispensing/hospital setting.
6. Mortality, cause of death and place-of-death evidence.
7. Schema conformance and cross-domain misclassification.
8. Diagnostics: laboratory, microbiology, pathology and imaging.
9. Vaccination and cancer screening.
10. Nursing, social care, blood management and specific registries.

## Privacy

Standard profile bundles are aggregate and suppress small positive counts in
their count tables. Cohort membership and event-log objects can contain
patient-level data and must remain inside the approved environment. Native
process maps contain unsuppressed aggregate frequencies and are marked for
internal research, not automatically cleared for external disclosure. Free text
is not exported. Site-specific patterns are opt-in and core defaults contain
no source-label expressions in any language. Suppression alone is not a
guarantee against reconstruction from overlapping outputs.

## BARDENA reference model

The conceptual starting point is the published BARDENA data-resource profile,
which describes population, primary-care, hospital, pharmacy, laboratory,
vaccination, blood-management and registry sources. The package deliberately
tests whether those functions survived transformation into OMOP instead of
assuming that their presence in the source warehouse implies their presence in
the CDM.
