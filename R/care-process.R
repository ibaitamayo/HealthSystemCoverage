collect_care_events <- function(con,config,cohort,components=character(),setting_labels=NULL) {
  assert_connection(con)
  q<-cohort_sql(con,cohort)
  s<-config$cdm_schema
  # Cases are cohort intervals, not inferred disease episodes. IDs remain local.
  windows<-paste0("SELECT row_number() OVER (ORDER BY subject_id,cohort_start_date,cohort_end_date)::text AS case_id,c.* FROM (",q,") c")
  setting<-visit_setting_case(con,config$source_aliases)
  visits<-paste0("WITH cw AS (",windows,") SELECT cw.case_id,v.person_id,",
    "v.visit_occurrence_id::text AS source_record_id,'visit_occurrence'::text AS source_table,",
    setting," AS activity,v.visit_start_date AS event_date,v.visit_end_date AS end_date,p.year_of_birth ",
    "FROM ",qualified_table(con,s,"visit_occurrence")," v JOIN cw ON cw.subject_id=v.person_id ",
    "JOIN ",qualified_table(con,s,"person")," p ON p.person_id=v.person_id ",
    "LEFT JOIN ",qualified_table(con,s,"care_site")," cs ON cs.care_site_id=v.care_site_id ",
    "WHERE v.visit_start_date BETWEEN cw.cohort_start_date AND cw.cohort_end_date")
  out<-DBI::dbGetQuery(con,visits)
  if(length(components)) {
    queries<-coverage_event_queries(con,config,cohort)
    if(any(!components %in% names(queries)))stop("An additional component has no configured event definition")
    for(cid in components) {
      # Population/OP descriptors are not clinical activities.
      if(cid %in% c("population_registry","demographics","geographic_assignment","longitudinal_follow_up"))
        stop("Population descriptors cannot be process activities")
      activity<-DBI::dbQuoteString(con,cid)
      sql<-paste0("WITH cw AS (",windows,") SELECT cw.case_id,e.person_id,e.record_id AS source_record_id,",
        "e.source_table,",activity,"::text AS activity,e.event_date,e.event_date AS end_date,p.year_of_birth ",
        "FROM (",queries[[cid]],") e JOIN cw ON cw.subject_id=e.person_id ",
        "AND e.event_date BETWEEN cw.cohort_start_date AND cw.cohort_end_date ",
        "JOIN ",qualified_table(con,s,"person")," p ON p.person_id=e.person_id")
      out<-rbind(out,DBI::dbGetQuery(con,sql))
    }
  }
  out$case_id<-as.character(out$case_id)
  out$person_id<-as.character(out$person_id)
  if(!is.null(setting_labels)) {
    mapped<-unname(setting_labels[out$activity]);hit<-!is.na(mapped)
    out$activity[hit]<-mapped[hit]
  }
  out
}

care_event_flags <- function(events,cutoff) {
  required<-c("case_id","activity","event_date")
  stopifnot(all(required %in% names(events)))
  date<-as.Date(events$event_date)
  end<-if("end_date" %in% names(events))as.Date(events$end_date) else date
  before<-if("year_of_birth" %in% names(events))
    as.integer(format(date,"%Y"))<events$year_of_birth else rep(FALSE,nrow(events))
  data.frame(missing_date=is.na(date),future_date=(!is.na(date)&date>as.Date(cutoff))|
    (!is.na(end)&end>as.Date(cutoff)),end_before_start=!is.na(end)&!is.na(date)&end<date,
    before_birth_year=!is.na(before)&before,stringsAsFactors=FALSE)
}

review_care_events <- function(events,cutoff=Sys.Date()) {
  flags<-care_event_flags(events,cutoff)
  do.call(rbind,lapply(names(flags),function(rule) {
    hit<-flags[[rule]]
    data.frame(rule=rule,records=sum(hit),cases=length(unique(events$case_id[hit])))
  }))
}

filter_care_events <- function(events,rules,cutoff=Sys.Date()) {
  stopifnot(is.character(rules),!anyDuplicated(rules))
  flags<-care_event_flags(events,cutoff)
  if(any(!rules %in% names(flags)))stop("Unknown event exclusion rule")
  keep<-rep(TRUE,nrow(events))
  audit<-list(data.frame(step=0L,rule="Selected events",records=nrow(events),cases=length(unique(events$case_id))))
  for(i in seq_along(rules)) {
    keep<-keep & !flags[[rules[i]]]
    audit[[i+1L]]<-data.frame(step=i,rule=rules[i],records=sum(keep),cases=length(unique(events$case_id[keep])))
  }
  list(events=events[keep,,drop=FALSE],attrition=do.call(rbind,audit),review=review_care_events(events,cutoff))
}

care_day_blocks <- function(events,first_only=FALSE) {
  stopifnot(all(c("case_id","activity","event_date") %in% names(events)))
  if(!nrow(events))stop("No drawable events")
  x<-data.frame(case_id=as.character(events$case_id),activity=as.character(events$activity),
    event_date=as.Date(events$event_date),stringsAsFactors=FALSE)
  if(anyNA(x)||any(!nzchar(x$case_id))||any(!nzchar(x$activity)))
    stop("Missing event identifiers, activity or date: review explicitly before making a log")
  x<-unique(x)
  x<-x[order(x$case_id,x$event_date,x$activity),,drop=FALSE]
  if(first_only)x<-x[!duplicated(x[,c("case_id","activity")]),,drop=FALSE]
  key<-paste(x$case_id,as.character(x$event_date),sep="\r")
  groups<-split(x$activity,key)
  first<-!duplicated(key)
  labels<-vapply(groups,paste,character(1),collapse=" + ")
  counts<-lengths(groups)
  out<-data.frame(case_id=x$case_id[first],activity=unname(labels[key[first]]),
    event_date=x$event_date[first],simultaneous_activities=unname(counts[key[first]]),stringsAsFactors=FALSE)
  rownames(out)<-NULL
  out[order(out$case_id,out$event_date),,drop=FALSE]
}

care_eventlog <- function(events,first_only=FALSE) {
  if(!requireNamespace("bupaR",quietly=TRUE))stop("Install the optional bupaR package in the analysis library")
  x<-care_day_blocks(events,first_only)
  x$instance_id<-as.character(seq_len(nrow(x)))
  # Point observations, not completion of hospitalisation or a care pathway.
  x$lifecycle<-"complete"
  x$resource<-"Unspecified resource"
  bupaR::eventlog(x,case_id="case_id",activity_id="activity",activity_instance_id="instance_id",
    lifecycle_id="lifecycle",timestamp="event_date",resource_id="resource",validate=TRUE)
}

plot_care_process <- function(log) {
  if(!requireNamespace("processmapR",quietly=TRUE))stop("Install the optional processmapR package in the analysis library")
  processmapR::process_map(log,type=processmapR::frequency("absolute"))
}

care_process_summary <- function(events,first_only=FALSE,privacy_min_cell=5L) {
  if(!nrow(events))return(list(nodes=data.frame(activity=character(),cases=numeric()),
    flows=data.frame(from=character(),to=character(),cases=numeric()),
    drawn_cases=0L,day_blocks=0L,simultaneous_blocks=0L))
  x<-care_day_blocks(events,first_only)
  sequences<-split(seq_len(nrow(x)),x$case_id)
  edges<-lapply(sequences,function(ix) {
    acts<-c("[Observed window start]",x$activity[ix],"[Observed window end]")
    unique(data.frame(from=utils::head(acts,-1),to=utils::tail(acts,-1),case_id=x$case_id[ix[1]]))
  })
  edges<-do.call(rbind,edges)
  flows<-stats::aggregate(rep(1,nrow(edges)),edges[,c("from","to")],sum)
  names(flows)[3]<-"cases"
  flows$cases[flows$cases>0&flows$cases<privacy_min_cell]<-NA_real_
  activities<-unique(x[,c("case_id","activity")])
  nodes<-stats::aggregate(rep(1,nrow(activities)),activities["activity"],sum)
  names(nodes)[2]<-"cases"
  nodes$cases[nodes$cases>0&nodes$cases<privacy_min_cell]<-NA_real_
  list(nodes=nodes,flows=flows,drawn_cases=length(sequences),day_blocks=nrow(x),
    simultaneous_blocks=sum(x$simultaneous_activities>1L))
}

plot_care_overview <- function(events,min_case_fraction=0.01,privacy_min_cell=5L) {
  if(!requireNamespace("DiagrammeR",quietly=TRUE))stop("DiagrammeR is required for the summary view")
  stopifnot(length(min_case_fraction)==1L,is.finite(min_case_fraction),min_case_fraction>=0,min_case_fraction<=1)
  s<-care_process_summary(events,first_only=TRUE,privacy_min_cell=privacy_min_cell)
  threshold<-max(privacy_min_cell,ceiling(min_case_fraction*s$drawn_cases))
  e<-s$flows[!is.na(s$flows$cases)&s$flows$cases>=threshold,,drop=FALSE]
  names<-unique(c(e$from,e$to))
  quote<-function(x)as.character(jsonlite::toJSON(x,auto_unbox=TRUE))
  nodes<-vapply(seq_along(names),function(i) {
    name<-names[i]
    n<-if(name %in% c("[Observed window start]","[Observed window end]"))s$drawn_cases else s$nodes$cases[match(name,s$nodes$activity)]
    label<-gsub(" + "," +\n",name,fixed=TRUE)
    paste0('n',i,' [label=',quote(paste0(label,"\n",format(n,big.mark=",",trim=TRUE)," cases")),'];')
  },"")
  edges<-vapply(seq_len(nrow(e)),function(i)paste0('n',match(e$from[i],names),' -> n',match(e$to[i],names),
    ' [label=',quote(format(e$cases[i],big.mark=",",trim=TRUE)),',penwidth=',1+3*sqrt(e$cases[i]/max(e$cases)),'];'),"")
  dot<-paste0('digraph { graph [rankdir=LR,bgcolor="white",pad=0.3,nodesep=0.55,ranksep=0.9]; ',
    'node [shape=box,style="rounded,filled",fillcolor="#edf4f8",color="#91a6b3",fontname="Arial",fontsize=13,margin="0.16,0.12"]; ',
    'edge [color="#6c8fa4",fontname="Arial",fontsize=12,arrowsize=0.7]; ',paste(nodes,collapse="\n"),paste(edges,collapse="\n"),
    if(!nrow(e))'label="No transitions meet the display threshold";' else '', '}')
  widget<-htmlwidgets::onRender(DiagrammeR::grViz(dot),
    "function(el){function fit(){const s=el.querySelector('svg');if(s){s.setAttribute('width','100%');s.setAttribute('height','100%');s.style.width='100%';s.style.height='100%';s.setAttribute('preserveAspectRatio','xMidYMid meet');}}fit();new MutationObserver(fit).observe(el,{childList:true,subtree:true});}")
  attr(widget,"display_summary")<-list(threshold=threshold,displayed_transitions=nrow(e),
    total_transitions=nrow(s$flows),cases=s$drawn_cases)
  widget
}
