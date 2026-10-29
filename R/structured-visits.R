# The modality and the establishment are separate coded dimensions. Local
# pair overrides are explicit; aliases cannot overwrite an informative code.
structured_visit_clauses <- function(con,aliases,v="v",cs="cs") {
  quote<-function(x)as.character(DBI::dbQuoteString(con,x))
  cid<-paste0(v,".visit_concept_id");place<-paste0("COALESCE(",cs,".place_of_service_concept_id,0)")
  clauses<-character();put<-function(p,s)clauses<<-c(clauses,paste0("WHEN ",p," THEN ",quote(s)))
  local<-attr(aliases,"visit_concept_map")
  if(!is.null(local)&&"place_of_service_concept_id"%in%names(local))for(i in which(!is.na(local$place_of_service_concept_id)))
    put(paste0(cid,"=",local$concept_id[i]," AND ",place,"=",local$place_of_service_concept_id[i]),local$setting[i])
  h<-attr(aliases,"visit_hierarchy_map")
  ids<-function(setting,root)unique(c(root,if(!is.null(h))h$concept_id[h$setting==setting] else numeric()))
  in_ids<-function(x,ids)paste0(x," IN (",paste(format(ids,scientific=FALSE,trim=TRUE),collapse=","),")")
  er<-in_ids(cid,ids("emergency",9203));ip<-in_ids(cid,ids("inpatient",9201))
  op<-in_ids(cid,ids("outpatient_unspecified",9202))
  primary<-paste0(place,"=38004247");hospital<-paste0(place,"=38004515")
  put(paste0(primary," AND (",ip," OR ",cid,"=262)"),"setting_conflict")
  put(paste0(primary," AND ",er),"primary_care_emergency")
  put(paste0(hospital," AND ",er),"emergency")
  put(paste0(cid,"=262"),"emergency_inpatient")
  put(er,"emergency_unspecified")
  put(primary,"primary_care")
  put(paste0(hospital," AND ",ip),"inpatient")
  put(paste0(hospital," AND ",op),"specialist_outpatient")
  put(hospital,"hospital_unspecified")
  if(!is.null(local))for(i in seq_len(nrow(local))) {
    if("place_of_service_concept_id"%in%names(local)&&!is.na(local$place_of_service_concept_id[i]))next
    put(paste0(cid,"=",local$concept_id[i]),local$setting[i])
  }
  if(!is.null(h)&&nrow(h))for(i in which(h$setting!="other_or_unknown"))
    put(paste0(cid,"=",h$concept_id[i]),h$setting[i])
  put(paste0(cid,"=9201"),"inpatient")
  # Outpatient alone does not establish specialist provenance.
  put(paste0(cid,"=9202"),"outpatient_unspecified")
  clauses
}

primary_settings <- function() c("primary_care","primary_care_emergency")
inpatient_settings <- function() c("inpatient","emergency_inpatient")
hospital_emergency_settings <- function() c("emergency","emergency_inpatient")
hospital_settings <- function() c("inpatient","emergency","emergency_inpatient","specialist_outpatient","hospital_unspecified","palliative")

setting_predicate <- function(con,setting,column="cv.logical_setting") {
  values<-switch(setting,primary_care=primary_settings(),inpatient=inpatient_settings(),emergency=hospital_emergency_settings(),setting)
  paste0(column," IN (",paste(DBI::dbQuoteString(con,values),collapse=","),")")
}
