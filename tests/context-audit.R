library(HealthSystemCoverage)
cfg<-new_profile_config("cdm",event_date_min="1900-01-01",privacy_min_cell=5)
stopifnot(cfg$unknown_setting_threshold==.20,cfg$cutoff_lag_days==183)
metadata<-data.frame(concept_id=c(10,20,30),concept_name=c("a","b","c"),
 domain_id=c("Type Concept","Visit","Type Concept"),invalid_reason=c(NA,NA,"D"))
checked<-HealthSystemCoverage:::validate_dispensing_metadata(c(10,20,30,40),metadata)
stopifnot(identical(checked$validation_status,c("usable","wrong_domain","invalid","unresolved")),
 identical(checked$concept_id,c(10,20,30,40)))
for(args in list(list(unknown_setting_threshold=1.1),list(cutoff_lag_days=-1),
 list(community_dispensing_type_ids=NA_real_)))
 stopifnot(inherits(try(do.call(new_profile_config,c(list(cdm_schema="cdm"),args)),silent=TRUE),"try-error"))

edges<-data.frame(ancestor_concept_id=c(9201,9202,9202,9203),
 descendant_concept_id=c(111,111,222,333))
h<-HealthSystemCoverage:::resolve_visit_hierarchy(edges)
stopifnot(h$setting[h$concept_id==111]=="other_or_unknown",h$ambiguous[h$concept_id==111],
 h$setting[h$concept_id==222]=="outpatient_unspecified",h$setting[h$concept_id==333]=="emergency")
aliases<-data.frame(source_type=c("primary_care_ehr","emergency_care"),pattern=c("local_a","local_b"))
attr(aliases,"visit_hierarchy_map")<-h
sql<-HealthSystemCoverage:::visit_setting_case(DBI::ANSI(),aliases)
stopifnot(grepl("visit_concept_id=222 THEN 'outpatient_unspecified'",sql,fixed=TRUE),
 !grepl("visit_concept_id=111 THEN 'inpatient'",sql,fixed=TRUE))

audit<-list(scope="fictional selected profile",
 visit_reclassification=data.frame(baseline_setting="emergency",configured_setting="primary_care",
  alias_index=1,records=40,persons=30,reclassified=TRUE,alias_pattern="local_a",alias_source_type="primary_care_ehr"),
 setting_distribution=data.frame(source_table="condition_occurrence",logical_setting=c("other_or_unknown","inpatient"),records=c(30,70)),
 event_date_maxima=data.frame(source_table="condition_occurrence",raw_max_event_date=as.Date("2999-01-01"),
  max_observed_event_date=Sys.Date()-400),drug_types=data.frame(drug_type_concept_id=32817,records=100))
cfg$visit_hierarchy_available<-TRUE
alerts<-HealthSystemCoverage:::context_alerts(audit,cfg)
stopifnot(all(c("alias_emptied_component","unclassified_setting_fraction","cutoff_lag","dispensing_scope")%in%alerts$rule_id),
 any(grepl("local_a",alerts$message,fixed=TRUE)))
comp<-HealthSystemCoverage:::empty_component_result(health_system_spec())
comp$reliability[comp$component_id=="inpatient_diagnoses"]<-"high"
comp$reliability[comp$component_id=="hospital_medications"]<-"unusable"
comp$reliability[comp$component_id=="laboratory"]<-"high"
out<-HealthSystemCoverage:::apply_context_quality(comp,audit,cfg)
stopifnot(out$reliability[out$component_id=="inpatient_diagnoses"]=="low",
 out$reliability[out$component_id=="hospital_medications"]=="unusable",
 out$reliability[out$component_id=="laboratory"]=="high",
 out$reliability[out$component_id=="emergency_care"]=="unknown",
 identical(out$persons,comp$persons),identical(out$records,comp$records))
provided<-cfg;provided$database_cutoff_source<-"provided"
stopifnot(!"cutoff_lag"%in%HealthSystemCoverage:::context_alerts(audit,provided)$rule_id)
at_threshold<-audit;at_threshold$setting_distribution$records<-c(20,80)
stopifnot(!"unclassified_setting_fraction"%in%HealthSystemCoverage:::context_alerts(at_threshold,cfg)$rule_id)
tiny<-audit;tiny$visit_reclassification$records<-2
tiny$setting_distribution$records<-c(2,3)
messages<-HealthSystemCoverage:::context_alerts(tiny,cfg)
stopifnot(is.na(messages$fraction[messages$rule_id=="unclassified_setting_fraction"]),
 any(grepl("all [suppressed]",messages$message,fixed=TRUE)))
dest<-tempfile("hsc-context-");dir.create(dest)
HealthSystemCoverage:::write_context_audit(audit,cfg,dest)
p<-read.csv(file.path(dest,"context_audit","parameters.csv"))
stopifnot(p$max_observed_event_date==as.character(Sys.Date()-400),p$unknown_setting_threshold==.20,
 file.exists(file.path(dest,"context_audit","visit_reclassification.csv")))
html<-HealthSystemCoverage:::context_audit_html(audit,cfg)
stopifnot(grepl("Classification and time context",html,fixed=TRUE),grepl("local_a",html,fixed=TRUE))
