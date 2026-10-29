# Structured sources and counting units

## Universal visit / establishment rules

Classification combines visit_concept_id and care_site.place_of_service_concept_id.
No source-language literals are needed for the following rules:

| Visit | Establishment | Result |
|---|---|---|
| ER 9203 or unambiguous descendants | Primary care 38004247 | primary_care_emergency |
| ER | Hospital 38004515 | emergency |
| ER | Other/unidentified | emergency_unspecified |
| ERIP 262 | Hospital/unidentified | emergency_inpatient |
| ERIP or inpatient | Primary care | setting_conflict |
| Other non-inpatient modality | Primary care | primary_care |
| Inpatient | Hospital | inpatient |
| Outpatient | Hospital | specialist_outpatient (specialty not established) |
| Other modality | Hospital | hospital_unspecified |

ERIP has the coded meaning of combined hospital emergency and inpatient care.
It remains one recorded contact, not two synthetic episodes. It contributes to
both emergency and inpatient evidence; do not sum those component counts as
admissions. Parent SQL uses distinct source keys. Different IDs connected by
preceding_visit_occurrence_id are not automatically merged: that link can have
large time gaps, missing endpoints or identity errors. These are audited.

The generic primary-care-urgency component is not named SUR. A local service
name is not a universal concept. Missing establishment identification on ERIP
does not erase its hospital semantics; an ER-only record without an informative
centre cannot establish primary-care versus hospital provenance. Conflicting
codes are surfaced, not silently assigned by priority.

An optional place_of_service_concept_id column in visit_concept_map enables
exact pair overrides; NA means concept-only. Pair overrides are explicit and
exported. Structured defaults precede fallback text aliases. Provider specialty
does not overwrite a conflicting establishment/modality.

## Additional source layers

* Costs: the legacy ID costs_and_copayment now represents resource costs only.
  COST is linked through its declared cost_domain_id and the appropriate clinical
  event key, never an equal numeric ID in an unrelated table. Cost availability
  and patient-payment field availability are distinct. Unlinked/unsupported
  domains remain visible in the instance-level cost-domain audit, not attributed
  to invented people or dates.
* Notes: clinical_notes counts rows with non-null note_text in scope, without
  extracting or interpreting text. Its status is detected_unstructured, not
  misclassified. Unexpected column names may be malformed imported data: only
  their count is reported, never their names/content.
* Services: provider_specialty_map selects provider-linked visits with active
  Provider-domain concepts. Defaults identify psychiatry and obstetrics/
  gynaecology via 38004469 and 38004461. Providers can supply other coded roots
  explicitly as exact IDs; the configured set is exported. This is partial service
  evidence, not inferred psychiatric diagnoses, pregnancy or prenatal care.
* Provenance: event-scoped type distributions are published by source table.
  Generic EHR does not provide the distinction needed for dispensing analyses.
  No search of unexpected fields or clinical notes is performed to rescue it.
* Geography: LOCATION field coverage is inspected; county can feed the existing
  area profiler directly when its values match provider-supplied map IDs.
  Geometry is still required and no historical residence is inferred.
* Inventory: actual existence probes distinguish absent/empty/nonempty tables,
  including derived eras. Planner row estimates are labelled estimates, not
  evidence of emptiness. Empty procedure domains produce not_detected under
  those definitions, not classifier-induced unknown.

## Source dates

Precedence when calendar filters are enabled: user cutoff, then a validated
CDM_SOURCE source_release_date, then Sys.Date(). Candidate dates must be unique,
plausible and not contradicted by observed clinical starts through today.
A metadata contradiction is reported and never resolved by discarding newer
data. Observed maxima are not substituted as extraction dates. Candidate checks
are instance-wide, even for a cohort, so metadata is not validated merely because
the selected cohort happens to be old.

## Discharge outcomes and grain

discharge_destinations publishes the coded destination distribution. The active
Visit-domain UB04 Expired concept 32218 can supply a coded death outcome; it is
not inferred from the English label. Missing coding remains a limitation.

No local death-marker literal is interpreted by default. A provider can declare
discharge_outcome_map with visit_concept_id, source_value and grain:
event, person or unknown. This explicit local mapping is exported, not hidden
inside general rules. The audit records source rows, distinct visit IDs,
distinct people, repeated contacts per person and death-date agreement.
Agreement supports interpretation but does not prove the ETL mechanism or the
physical place of death.

quality_findings.csv includes grain and count_unit. Person/unknown-grain findings
have records=NA: they cannot be counted as deaths/events by summing source rows.
Distinct people across rows can overlap and also must not be summed. The internal
record-sum helper refuses non-event grains. Scoped date-rule overlaps and
crosswalk metadata remain separate, with their own counting units.

Instance metadata tables (provider inventory, costs, location and schema shape)
are explicitly labelled as such; they are not cohort numerators. Event/type and
discharge evidence respects selected cohort windows and active event filters.
Nothing here repairs OMOP, changes cohort membership or completes the optional
population-selection / geographical / process-report workflow automatically.
