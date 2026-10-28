library(HealthSystemCoverage)
d<-data.frame(cohort_definition_id=c(1,1,1),subject_id=c("10","10","11"),
  cohort_start_date=as.Date(c("2020-01-01","2021-01-01","2020-01-01")),
  cohort_end_date=as.Date(c("2020-12-31","2021-12-31","2020-12-31")))
c<-cohort_from_data(rbind(d,d[1,]))
stopifnot(nrow(c$data)==3,c$duplicate_rows==1)
stopifnot(inherits(cohort_from_table("results","cohort",1),"hsc_cohort"))
e<-data.frame(case_id=c("1","1","1","1","2","2"),
  activity=c("Hospital","Primary","Hospital","Pharmacy","Primary","Hospital"),
  event_date=as.Date(c("2020-01-01","2020-01-01","2020-01-02","2020-01-03","2020-01-01","2200-01-01")))
review<-review_care_events(e,as.Date("2024-01-01"))
stopifnot(review$records[review$rule=="future_date"]==1)
selected<-filter_care_events(e,"future_date",as.Date("2024-01-01"))
stopifnot(nrow(selected$events)==5,nrow(e)==6)
b<-care_day_blocks(selected$events)
stopifnot(nrow(b)==4,b$activity[b$case_id=="1" & b$event_date==as.Date("2020-01-01")]=="Hospital + Primary")
first<-care_day_blocks(selected$events,TRUE)
stopifnot(nrow(first)==3)
summ<-care_process_summary(selected$events,privacy_min_cell=0L)
stopifnot(summ$drawn_cases==2,summ$simultaneous_blocks==1)
suppressed<-care_process_summary(selected$events,privacy_min_cell=5L)
stopifnot(all(is.na(suppressed$flows$cases)))
stopifnot(care_process_summary(e[FALSE,])$drawn_cases==0L)
if(requireNamespace("bupaR",quietly=TRUE)&&requireNamespace("processmapR",quietly=TRUE)) {
  log<-care_eventlog(selected$events)
  stopifnot(inherits(log,"eventlog"))
  map<-plot_care_process(log)
  stopifnot(inherits(map,"htmlwidget"))
  if(requireNamespace("htmlwidgets",quietly=TRUE)&&nzchar(Sys.which("pandoc"))) {
    cfg<-new_profile_config("example",privacy_min_cell=0)
    cfg$analysis_population<-list(name="Synthetic test cohort",persons=2,episodes=2)
    profile<-structure(list(config=cfg,generated_at=Sys.time(),
      components=HealthSystemCoverage:::empty_component_result(health_system_spec()),
      evidence=list(person=list(data=data.frame(persons=2))),probe_errors=NULL),class="health_system_profile")
    a<-data.frame(step=c(0,1),rule=c("Selected cohort","zero_post_index_days"),episodes=c(3,2),persons=c(3,2))
    dest<-tempfile("hsc-workflow-test-")
    write_cohort_workflow(profile,a,selected$events,dest)
    stopifnot(file.exists(file.path(dest,"workflow.html")),file.exists(file.path(dest,"selection-flow.svg")))
  }
}
