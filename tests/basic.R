library(HealthSystemCoverage)

s <- health_system_spec()
stopifnot(is.data.frame(s))
stopifnot(nrow(s) >= 35L)
stopifnot(!anyDuplicated(s$component_id))
stopifnot(all(c("primary_care_diagnoses", "community_dispensings",
                "mortality_cause", "histopathology",
                "cancer_screening_colorectal") %in% s$component_id))

cfg <- new_profile_config("omop2")
stopifnot(inherits(cfg, "health_system_profile_config"))
stopifnot(cfg$cdm_schema == "omop2")
stopifnot(nrow(cfg$source_aliases)==0L)
components <- HealthSystemCoverage:::empty_component_result(s)
measures <- data.frame(node_id=c("primary_care_encounters","primary_care_diagnoses",
  "group_primary_care","area_community","vaccinations"),
  persons=c(50,40,70,70,2),records=c(100,50,140,140,2),
  start_date=as.Date("2020-01-01"),end_date=as.Date("2024-01-01"))
p <- structure(list(config=cfg,components=components,generated_at=Sys.time(),
  evidence=list(person=list(data=data.frame(persons=100)),coverage=list(data=measures)),
  probe_errors=NULL),class="health_system_profile")
result <- coverage_results(p)
stopifnot(nrow(result)==74L,!anyDuplicated(result$node_id))
stopifnot(result$persons[result$node_id=="group_primary_care"]==70)
stopifnot(is.na(result$percentage[result$node_id=="vaccinations"]))
stopifnot(result$measurement_status[result$node_id=="radiology_imaging"]=="not_assessed")
out <- tempfile("hsc-test-")
write_profile_bundle(p,out)
stopifnot(file.exists(file.path(out,"profile.html")),file.exists(file.path(out,"coverage_results.csv")))
html <- paste(readLines(file.path(out,"profile.html"),warn=FALSE),collapse="\n")
stopifnot(!grepl("__DATA__",html,fixed=TRUE),grepl('section-primary_care',html,fixed=TRUE))
