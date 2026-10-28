# Entirely fictional; not sampled, de-identified or transformed patient records.
example_care_events <- function() {
  case<-c(1,1,1,2,2,2,3,3,3,4,4,5,5,5,6,6)
  activity<-c("Primary care","Imaging","Hospital","Emergency","Hospital","Community pharmacy",
    "Primary care","Hospital","Community pharmacy","Primary care","Imaging",
    "Emergency","Imaging","Hospital","Primary care","Community pharmacy")
  day<-c(0,0,10,1,1,14,0,7,21,2,8,0,0,4,3,9)
  data.frame(case_id=as.character(case),person_id=as.character(case),
    source_record_id=as.character(seq_along(case)),source_table="fictional_events",
    activity=activity,event_date=as.Date("2024-01-01")+day,
    end_date=as.Date("2024-01-01")+day,stringsAsFactors=FALSE)
}

run_synthetic_demo <- function(path,process_maps=FALSE) {
  if(dir.exists(path))stop("Choose a new directory for the synthetic demonstration")
  events<-example_care_events()
  people<-unique(events$person_id)
  activity_map<-c("Primary care"="primary_care_encounters","Imaging"="radiology_imaging",
    "Hospital"="inpatient_admissions","Emergency"="emergency_care",
    "Community pharmacy"="community_dispensings")
  cohort<-data.frame(person_id=people,source_record_id=people,
    source_table="fictional_cohort",event_date=as.Date("2024-01-01"),stringsAsFactors=FALSE)
  definitions<-lapply(names(activity_map),function(activity) {
    e<-events[events$activity==activity,c("person_id","source_record_id","source_table","event_date")]
    e
  })
  names(definitions)<-unname(activity_map)
  definitions$population_registry<-cohort
  definitions$longitudinal_follow_up<-cohort
  # Distinct source keys/persons are unioned exactly, never summed across children.
  member<-coverage_membership()
  nodes<-c(member$component_id,unique(member$group_id),unique(member$area_id))
  measured<-lapply(nodes,function(node) {
    ids<-if(node %in% member$component_id)node else member$component_id[member$group_id==node|member$area_id==node]
    ids<-intersect(ids,names(definitions))
    if(!length(ids))return(NULL)
    d<-unique(do.call(rbind,definitions[ids]))
    data.frame(node_id=node,persons=length(unique(d$person_id)),records=nrow(d),
      start_date=min(d$event_date),end_date=max(d$event_date),assessed_children=paste(ids,collapse=";"))
  })
  measured<-do.call(rbind,measured)
  components<-empty_component_result(health_system_spec())
  for(i in seq_len(nrow(components))) {
    m<-measured[measured$node_id==components$component_id[i],,drop=FALSE]
    if(!nrow(m))next
    components$status[i]<-"detected_partial"
    components$persons[i]<-m$persons[1];components$records[i]<-m$records[1]
    components$start_date[i]<-m$start_date[1];components$end_date[i]<-m$end_date[1]
    components$evidence[i]<-"Fictional demonstration events; not patient data or validation evidence."
    components$issues[i]<-"Illustration only. Do not use these values as empirical findings."
  }
  config<-new_profile_config("synthetic",source_name="SYNTHETIC DEMO - NOT PATIENT DATA",privacy_min_cell=0L)
  config$analysis_population<-list(name="Fictional cohort - not patient data",persons=length(people),episodes=length(people))
  profile<-structure(list(config=config,generated_at=Sys.time(),components=components,
    evidence=list(person=list(name="person",status="ok",data=data.frame(persons=length(people))),
      coverage=list(name="coverage",status="ok",data=measured)),probe_errors=NULL),class="health_system_profile")
  if(process_maps) {
    selection<-data.frame(step=0L,rule="Fictional reference population",episodes=length(people),persons=length(people))
    write_cohort_workflow(profile,selection,events,path)
  } else write_profile_bundle(profile,path)
  writeLines(c("SYNTHETIC DEMONSTRATION ONLY", "Every event is fictional and generated in R code.",
    "No database, credentials, patient records or real site findings were used.",
    "Zero small-cell masking is intentional for this fictional toy example only."),
    file.path(path,"SYNTHETIC_DATA_NOTICE.txt"))
  invisible(normalizePath(path))
}
