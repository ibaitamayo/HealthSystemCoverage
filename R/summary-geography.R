write_summary_area_report <- function(profile,path,d) {
  original<-d
  status<-if("status"%in%names(d))d$status else rep("unknown",nrow(d))
  reliability<-if("reliability"%in%names(d))d$reliability else rep("unknown",nrow(d))
  d$representation<-vapply(seq_len(nrow(d)),function(i)component_representation(status[i],reliability[i]),"")
  d$limitation<-ifelse(d$representation=="not_evaluable","Definition or classification not evaluable; absence is not established.",
    ifelse(d$representation=="not_detected","No matches under the assessed area/population definition; not proof the service does not exist.",
      "People with evidence under the selected definitions; not completeness, research fitness or risk-adjusted area differences."))
  keep<-c("area_id","area_name","node_id","parent_id","level","label","persons","population","percentage","representation","limitation","measurement_status","mapped")
  d<-d[,intersect(keep,names(d)),drop=FALSE]
  utils::write.csv(d,file.path(path,"area_coverage_results.csv"),row.names=FALSE,na="")
  audit<-file.path(path,"audit");dir.create(audit,showWarnings=FALSE)
  validation<-list(config=if(is.null(profile$config))NULL else profile$config[c("cdm_schema","event_date_min","database_cutoff","database_cutoff_source","birth_date_rule","privacy_min_cell","source_aliases","component_rules","provider_specialty_map")],
    dispensing_type_rules=profile$context_audit$dispensing_type_rules,
    source_date_validation=profile$context_audit$source_date_validation,
    results=original,
    findings=suppress_small_cells(if(is.null(profile$findings))empty_findings() else profile$findings,profile$privacy_min_cell))
  jsonlite::write_json(validation,file.path(audit,"validation.json"),auto_unbox=TRUE,pretty=TRUE,na="null",null="null",dataframe="rows")
  payload<-list(results=d,map=unclass(profile$areas),source=profile$source_name,
    population_note=profile$population_note,attribution=profile$attribution,mode="summary",
    generated=format(profile$generated_at,tz="UTC",usetz=TRUE))
  encoded<-jsonlite::toJSON(payload,dataframe="rows",auto_unbox=TRUE,na="null",null="null",digits=10)
  encoded<-gsub("<","\\u003c",encoded,fixed=TRUE)
  template<-paste(readLines(system.file("geography","report.html",package="HealthSystemCoverage"),warn=FALSE),collapse="\n")
  html<-sub("__AREA_PAYLOAD__",encoded,template,fixed=TRUE)
  html<-sub("</body>",'<p><a href="audit/validation.json">Attached definitions and evaluation evidence</a>. Percentages describe people with evidence, not completeness.</p></body>',html,fixed=TRUE)
  writeLines(html,file.path(path,"area_report.html"),useBytes=TRUE)
  invisible(normalizePath(path))
}
