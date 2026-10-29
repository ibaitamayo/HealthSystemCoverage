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

# PostgreSQL bigint mapping counters must not be copied as raw floating-point
# bits into component columns (which would trigger false small-cell masking).
round_trip <- function(input) {
  input$start_date<-as.Date("2020-01-01")
  input$end_date<-as.Date("2020-12-31")
  config<-new_profile_config("cdm",privacy_min_cell=5)
  measured<-HealthSystemCoverage:::enrich_coverage_stats(input,config)
  components<-HealthSystemCoverage:::apply_coverage_quality(comp,measured,config)
  one<-components[components$component_id==input$node_id,,drop=FALSE]
  stopifnot(one$mapping_records==40,one$auditable_records==40)
  profile<-structure(list(config=config,generated_at=Sys.time(),components=components,
    evidence=list(person=list(status="ok",data=data.frame(persons=30)),
      coverage=list(status="ok",data=measured)),findings=NULL,probe_errors=NULL),
    class="health_system_profile")
  dest<-tempfile("hsc-mapping-roundtrip-")
  write_profile_bundle(profile,dest)
  exported<-read.csv(file.path(dest,"health_system_components.csv"))
  hierarchical<-read.csv(file.path(dest,"coverage_results.csv"))
  leaf<-exported[exported$component_id==input$node_id,,drop=FALSE]
  other<-hierarchical[hierarchical$node_id==input$node_id,,drop=FALSE]
  fractions<-c("mapped_fraction","source_coded_fraction","source_resolved_fraction",
    "source_crosswalked_fraction","assigned_target_crosswalked_fraction","auditable_fraction")
  expected<-c(1,0,0,1,1,1)
  stopifnot(identical(as.numeric(leaf[1,fractions]),expected),
    identical(as.numeric(other[1,fractions]),expected),
    leaf$provenance_reliability=="low",leaf$reliability=="low")
}
round_trip(z)
if(requireNamespace("bit64",quietly=TRUE)) {
  bigint<-z
  for(nm in counts)bigint[[nm]]<-bit64::as.integer64(bigint[[nm]])
  round_trip(bigint)
}

# An evaluated setting-dependent zero is unknown, not absent. A definition
# with no probe remains not_assessed; direct-table zeros keep scoped absence.
zeros<-z[rep(1,3),,drop=FALSE]
zeros$node_id<-c("emergency_care","primary_care_procedures","mortality_all_cause")
for(nm in c("records","persons",counts))zeros[[nm]]<-0
zero_components<-HealthSystemCoverage:::apply_coverage_quality(comp,zeros,cfg)
stopifnot(all(zero_components$status[match(zeros$node_id,zero_components$component_id)]==
 c("unknown","unknown","not_detected")),
 zero_components$status[zero_components$component_id=="community_dispensings"]=="not_assessed",
 grepl("alias reassignment",zero_components$issues[zero_components$component_id=="emergency_care"],fixed=TRUE))
for(id in HealthSystemCoverage:::visit_dependent_components())
 stopifnot(HealthSystemCoverage:::zero_coverage_status(id,cfg)=="unknown")
stopifnot(HealthSystemCoverage:::zero_coverage_status("group_emergency",cfg,"emergency_care")=="unknown",
 HealthSystemCoverage:::zero_coverage_status("group_mortality",cfg,"mortality_all_cause")=="not_detected")
explicit<-cfg
explicit$component_rules<-data.frame(component_id="emergency_care",table="observation",concept_id=1)
stopifnot(HealthSystemCoverage:::zero_coverage_status("emergency_care",explicit)=="not_detected")
# Privacy masking remains active for genuinely small mapping denominators.
tiny<-HealthSystemCoverage:::enrich_coverage_stats(z,cfg)
tiny$mapping_records<-2
tiny<-HealthSystemCoverage:::suppress_small_cells(tiny,5)
stopifnot(all(is.na(tiny[,c("mapped_fraction","source_coded_fraction","source_resolved_fraction",
 "source_crosswalked_fraction","assigned_target_crosswalked_fraction","auditable_fraction")])))
