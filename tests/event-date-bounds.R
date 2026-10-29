library(HealthSystemCoverage)
cfg<-new_profile_config("omop2",database_cutoff="2025-05-21",event_date_min="1900-01-01")
automatic<-new_profile_config("omop2",event_date_min="1900-01-01")
stopifnot(identical(automatic$database_cutoff,Sys.Date()),
 identical(automatic$database_cutoff_source,"Sys.Date() fallback"),
 identical(cfg$database_cutoff,as.Date("2025-05-21")),
 identical(cfg$database_cutoff_source,"provided"))
stopifnot(inherits(try(new_profile_config("omop2",event_date_min="2000-01-01",database_cutoff="1999-01-01"),silent=TRUE),"try-error"))
f<-HealthSystemCoverage:::event_date_exclusions
a<-DBI::ANSI()
drug<-f(a,cfg,"drug_exposure")
stopifnot(grepl("drug_exposure_start_date",drug$after,fixed=TRUE),
 !grepl("drug_exposure_end_date",drug$after,fixed=TRUE),
 !grepl("verbatim_end_date",drug$after,fixed=TRUE),
 grepl("drug_exposure_end_date",drug$before,fixed=TRUE))
visit<-f(a,cfg,"visit_occurrence",alias="v")
stopifnot(grepl("v.visit_end_date",visit$after,fixed=TRUE),grepl("2025-05-21",visit$after,fixed=TRUE))
stopifnot(f(a,cfg,"person")$any=="FALSE")
stopifnot(f(a,new_profile_config("omop2"),"measurement")$any=="FALSE")
# Synthetic boundary cases evaluated by the same strict comparisons as SQL.
start<-as.Date(c("1899-12-31","1900-01-01","2025-05-21","2025-05-22",NA))
stopifnot(identical(which(!is.na(start)&(start<cfg$event_date_min|start>cfg$database_cutoff)),c(1L,4L)))
profile<-structure(list(config=cfg,evidence=list()),class="health_system_profile")
html<-HealthSystemCoverage:::event_filter_summary_html(profile)
stopifnot(grepl("People remain in the denominator",html,fixed=TRUE),grepl("not run",html,fixed=TRUE))
