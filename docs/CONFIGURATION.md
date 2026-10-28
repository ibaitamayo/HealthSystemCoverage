# Local configuration

Core defaults contain no source-label regular expressions. Standard numeric
visit concepts provide conservative fallbacks. They do not identify a specialty
or distinguish prescribing from administration/dispensing by themselves.

Configure reviewed local provenance using:

* `source_aliases`: `source_type` and a PostgreSQL `pattern`, supplied locally;
* `visit_concept_map`: exact numeric `concept_id` and logical `setting`;
* `component_rules`: `component_id`, OMOP `table`, exact standard `concept_id`;
* `adjudications`: reviewed interpretation for the whole-data-source profile.

Do not distribute real aliases, site findings or credentials with the package.
The CSV in `inst/extdata/source_aliases.csv` is intentionally an empty template.
English display labels are not evidence searches. A narrow activity concept set
is not proof of complete capture of a service.

Supported explicit-rule domains: measurement, observation, specimen,
drug_exposure and procedure_occurrence. Visit-linked domains are classified
using reviewed visit provenance, with cohort-specific linkage requiring matching
person and visit. Population descriptors are not process activities.

Global site adjudications are not reused as evidence in `profile_cohort()`.
Cohort denominators use distinct selected persons; process cases use membership
intervals. Clinical event starts are restricted to their individual windows.
Observation periods can overlap those windows and are not treated as proof of
continuous eligibility. Contacts already underway before index are not added
as new starts; review whether your question needs an additional interval-aware
activity definition.

Read the function help with `?new_profile_config`, `?review_cohort`,
`?collect_care_events` and `?care_eventlog` before analysis. No filter is universal:
declare the cutoff and accepted rules and retain their attrition counts.
