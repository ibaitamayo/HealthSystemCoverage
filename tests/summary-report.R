library(HealthSystemCoverage)
p<-structure(list(config=new_profile_config("cdm",privacy_min_cell=5),
  generated_at=Sys.time(),components=HealthSystemCoverage:::empty_component_result(health_system_spec()),
  evidence=list(person=list(data=data.frame(persons=100))),probe_errors=NULL),class="health_system_profile")
set<-function(id,status,reliability="unknown") {
  p$components$status[p$components$component_id==id]<<-status
  p$components$reliability[p$components$component_id==id]<<-reliability
}
set("primary_care_encounters","detected_structured","moderate")
set("primary_care_diagnoses","detected_structured","unusable")
set("clinical_notes","detected_unstructured","moderate")
set("community_dispensings","not_detected")
p$evidence$coverage<-list(data=data.frame(node_id=c("primary_care_encounters","primary_care_diagnoses","group_primary_care","clinical_notes","community_dispensings","vaccinations"),persons=c(50,40,70,20,0,2),records=c(100,80,170,50,0,2),start_date=as.Date("2020-01-01"),end_date=as.Date("2024-01-01")))
r<-representation_results(p)
state<-function(id)r$representation[r$node_id==id]
stopifnot(ncol(r)==12L,!anyDuplicated(r$node_id),state("primary_care_encounters")=="represented",state("primary_care_diagnoses")=="represented_with_limits",state("clinical_notes")=="represented_with_limits",state("community_dispensings")=="not_detected",state("laboratory")=="not_evaluable",state("group_primary_care")=="represented_with_limits")
stopifnot(r$persons[r$node_id=="group_primary_care"]==70,is.na(r$persons[r$node_id=="vaccinations"]),!any(c("fitness_for_research","records","mapped_fraction")%in%names(r)),max(nchar(r$limitation))<=240)
dest<-tempfile("hsc-summary-");write_profile_bundle(p,dest)
stopifnot(length(list.files(dest,pattern="[.]csv$"))==1,file.exists(file.path(dest,"audit/validation.json")),!file.exists(file.path(dest,"health_system_components.csv")))
attached<-jsonlite::read_json(file.path(dest,"audit/validation.json"),simplifyVector=TRUE)
stopifnot(attached$components$reliability[attached$components$component_id=="primary_care_diagnoses"]=="unusable")
html<-paste(readLines(file.path(dest,"profile.html")),collapse="\n")
stopifnot(grepl("not completeness",html,fixed=TRUE),grepl("Definitions and validation",html,fixed=TRUE),!grepl("Research fitness",html,fixed=TRUE))
full<-tempfile("hsc-full-");write_profile_bundle(p,full,level="exhaustive")
stopifnot(file.exists(file.path(full,"health_system_components.csv")),file.exists(file.path(full,"quality_findings.csv")),inherits(try(write_profile_bundle(p,full),silent=TRUE),"try-error"))
# Escaping remains mandatory for source names, labels and attached definitions.
p$config$source_name<-"<script>unsafe()</script>"
escape<-tempfile("hsc-escape-");write_profile_bundle(p,escape)
html<-paste(readLines(file.path(escape,"profile.html")),collapse="\n")
stopifnot(!grepl("<script>unsafe()",html,fixed=TRUE),grepl("&lt;script&gt;unsafe()",html,fixed=TRUE))
