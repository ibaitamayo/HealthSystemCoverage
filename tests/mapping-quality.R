library(HealthSystemCoverage)
cfg<-new_profile_config("cdm",privacy_min_cell=0)
counts<-HealthSystemCoverage:::mapping_counts()
z<-as.data.frame(as.list(setNames(rep(0,length(counts)),counts)))
z$records<-40;z$persons<-30;z$node_id<-"primary_care_diagnoses"
z$mapping_records<-40;z$valid_standard_records<-40
z$source_crosswalked_records<-40;z$assigned_target_crosswalked_records<-40
z$auditable_records<-40;z$crosswalk_ambiguous_records<-40
q<-HealthSystemCoverage:::mapping_quality(z,cfg)
stopifnot(q$source_coded_fraction==0,q$source_resolved_fraction==0,
 q$source_crosswalked_fraction==1,q$auditable_fraction==1,q$provenance_reliability=="low")
comp<-HealthSystemCoverage:::empty_component_result(health_system_spec())
updated<-HealthSystemCoverage:::apply_coverage_quality(comp,z,cfg)
one<-updated[updated$component_id==z$node_id,]
stopifnot(one$status=="detected_structured",one$reliability=="low",one$fitness_for_research=="not_evaluated")
none<-z;none$source_crosswalked_records<-none$assigned_target_crosswalked_records<-none$auditable_records<-none$crosswalk_ambiguous_records<-0
stopifnot(HealthSystemCoverage:::mapping_quality(none,cfg)$provenance_reliability=="unusable")
report<-new_profile_config("cdm",mapping_audit_policy="report")
stopifnot(HealthSystemCoverage:::mapping_quality(none,report)$provenance_reliability=="low")
partial<-none;partial$source_coded_records<-partial$source_resolved_records<-partial$auditable_records<-20
stopifnot(HealthSystemCoverage:::mapping_quality(partial,cfg)$source_resolved_fraction==.5)
# A bare nonzero source ID is not a resolved vocabulary source.
unresolved<-none;unresolved$source_coded_records<-40
stopifnot(HealthSystemCoverage:::mapping_quality(unresolved,cfg)$source_resolved_fraction==0)
# Per-reason and incremental exclusions are different quantities.
stats<-data.frame(node_id="mortality_all_cause",before_min_excluded_records=10,before_min_excluded_persons=10,
 before_birth_excluded_records=3,before_birth_excluded_persons=3,before_birth_prior_records=2,before_birth_prior_persons=2)
f<-HealthSystemCoverage:::coverage_findings(stats,cfg)
stopifnot(sum(f$records[f$incremental_exclusion])==13,
 sum(f$records[f$action=="excluded_by_prior_rule"])==2,
 !any(f$in_scope),all(f$excluded_by_filter))
# Provenance fractions cannot disclose suppressed source numerators.
small<-HealthSystemCoverage:::enrich_coverage_stats(partial,cfg)
small$source_coded_records<-2;small$source_coded_fraction<-2/40
masked<-HealthSystemCoverage:::suppress_small_cells(small,5)
stopifnot(is.na(masked$source_coded_fraction))
# Inspection policy never upgrades a reviewed unusable conclusion.
stopifnot(HealthSystemCoverage:::cap_reliability("unusable","low")=="unusable")
