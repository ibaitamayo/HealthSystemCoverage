profile_cohort <- function(con,config,cohort) {
  assert_connection(con)
  stopifnot(inherits(config,"health_system_profile_config"),inherits(cohort,"hsc_cohort"))
  if(config$dbms!="postgresql")stop("Currently supports PostgreSQL only")
  q<-cohort_sql(con,cohort)
  bad<-DBI::dbGetQuery(con,paste0("SELECT count(*) AS n FROM (",q,") c LEFT JOIN ",
    qualified_table(con,config$cdm_schema,"person")," p ON p.person_id=c.subject_id WHERE p.person_id IS NULL OR c.cohort_start_date IS NULL OR c.cohort_end_date IS NULL OR c.cohort_end_date<c.cohort_start_date"))
  if(as.double(bad$n)>0)stop("Unusable cohort intervals: review and explicitly filter before profiling")
  size<-cohort_size(con,q)
  if(size$persons==0)stop("Selected cohort is empty")
  probes<-list(person=list(name="person",status="ok",data=data.frame(persons=size$persons)),
    coverage=safe_probe("coverage",function()probe_coverage(con,config,cohort)))
  components<-empty_component_result(health_system_spec())
  if(!is.null(probes$coverage$data))for(i in seq_len(nrow(components))) {
    z<-probes$coverage$data[probes$coverage$data$node_id==components$component_id[i],,drop=FALSE]
    if(!nrow(z))next
    components$persons[i]<-z$persons[1];components$records[i]<-z$records[1]
    components$start_date[i]<-as.Date(z$start_date[1]);components$end_date[i]<-as.Date(z$end_date[1])
    components$status[i]<-if(z$records[1]>0)"detected_partial" else "not_detected"
    components$evidence[i]<-"Current cohort query under an explicit definition; clinical event start dates restricted to cohort intervals."
    components$issues[i]<-"Detection does not establish semantic validity, clinical completeness or a disease-specific pathway. Zero means no match under this definition."
  }
  # Global site adjudications must not be promoted to findings in this cohort.
  config$analysis_population<-list(name=cohort$name,persons=size$persons,episodes=size$episodes)
  config$adjudications<-NULL
  errors<-if(identical(probes$coverage$status,"error"))data.frame(probe="coverage",error=probes$coverage$error) else NULL
  structure(list(generated_at=Sys.time(),config=config,components=components,
    evidence=probes,probe_errors=errors),class="health_system_profile")
}
