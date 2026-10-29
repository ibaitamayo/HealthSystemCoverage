coverage_status_colour <- function(status) {
  colours <- c(detected_structured="#dcece2",detected_partial="#f5e5c7",
    detected_misclassified="#f3cba8",detected_unstructured="#f3cba8",
    not_detected="#efced1",unknown="#e4e7ea",not_assessed="#edf0f2",
    represented="#dcece2",represented_with_limits="#f5e5c7",not_evaluable="#e4e7ea")
  z <- unname(colours[status]);z[is.na(z)] <- "#edf0f2";z
}

svg_metric <- function(row, box, small=FALSE) {
  x<-box[1];y<-box[2];w<-box[3];h<-box[4]
  strip<-if(small) 23 else 29
  by<-y+h-strip
  pct<-row$percentage
  label<-if(row$measurement_status=="suppressed") "Suppressed" else "Not assessed"
  if (row$measurement_status=="measured" && is.na(pct)) label<-"No denominator"
  text<-function(xx,yy,value,size=12,anchor="start") paste0(
    '<text x="',xx,'" y="',yy,'" font-size="',size,'" text-anchor="',anchor,
    '" fill="#263542">',html_escape(value),'</text>')
  result<-paste0('<rect x="',x+1,'" y="',by,'" width="',w-2,
                 '" height="',strip,'" fill="white"/>')
  if (is.na(pct)) return(paste0(result,text(x+w/2,by+17,label,if(small)10 else 12,"middle")))
  barw<-max(10,w-65)
  result<-paste0(result,'<rect x="',x+10,'" y="',by+4,'" width="',barw,
    '" height="9" rx="3" fill="#e0e5e9"/>',
    '<rect x="',x+10,'" y="',by+4,'" width="',barw*min(100,pct)/100,
    '" height="9" rx="3" fill="#608eab"/>',
    text(x+w-8,by+13,if(pct>0 && pct<0.01) '<0.01%' else paste0(format(round(pct,2),trim=TRUE),'%'),if(small)10 else 12,"end"))
  if(!small)result<-paste0(result,text(x+w-8,by+25,
    paste0(format(row$persons,big.mark=",",scientific=FALSE,trim=TRUE)," / ",
      format(row$population,big.mark=",",scientific=FALSE,trim=TRUE)),8,"end"))
  result
}

render_architectural_svg <- function(profile,results) {
  asset<-system.file("architecture","template.svg",package="HealthSystemCoverage")
  svg<-paste(readLines(asset,warn=FALSE),collapse="\n")
  regions<-jsonlite::fromJSON(system.file("architecture","regions.json",package="HealthSystemCoverage"))
  overlay<-character()
  for(i in seq_len(nrow(regions))) {
    region<-regions[i,,drop=FALSE];id<-region$id
    row<-results[results$node_id==paste0("group_",id),,drop=FALSE]
    if(!nrow(row))next
    box<-as.double(region$box[[1]])
    colour<-coverage_status_colour(row$status)
    overlay<-c(overlay,paste0('<g class="region" data-node="group_',id,
      '" id="section-',id,'" tabindex="0" role="button" aria-label="',html_escape(row$label),'">',
      '<title>',html_escape(row$label),'</title>',
      '<rect class="status-fill" x="',box[1],'" y="',box[2],'" width="',box[3],
      '" height="',box[4],'" fill="',colour,'" style="mix-blend-mode:multiply"/>',
      svg_metric(row,box,small=id %in% c("screening","immunisation","occupational_health")),
      '<rect class="hotspot" x="',box[1],'" y="',box[2],'" width="',box[3],
      '" height="',box[4],'" fill="transparent"/></g>'))
  }
  name<-profile$config$source_name
  if(is.null(name))name<-paste0("CDM schema: ",profile$config$cdm_schema)
  denom<-results$population[1]
  overlay<-c(overlay,paste0('<text x="59" y="123" font-size="17">',html_escape(name),
    ' \u00b7 ',html_escape(format(denom,big.mark=",",scientific=FALSE,trim=TRUE)),
    ' persons \u00b7 ',format(profile$generated_at,"%Y-%m-%d"),'</text>'),
    '<style>.hotspot{stroke:transparent;stroke-width:2}.region{cursor:pointer}.region:hover .hotspot,.region:focus .hotspot,.region.selected .hotspot{stroke:#396a82;fill:#396a8210}text{font-family:Arial,Helvetica,sans-serif}</style>',
    '<text x="59" y="758" font-size="14">Room fill: representation status \u00b7 Bars: people matching assessed definitions / all CDM persons</text>')
  sub("</svg>",paste0(paste(overlay,collapse="\n"),"</svg>"),svg,fixed=TRUE)
}

write_architectural_report <- function(profile,path) {
  results<-coverage_results(profile)
  utils::write.csv(results,file.path(path,"coverage_results.csv"),row.names=FALSE,na="")
  svg<-render_architectural_svg(profile,results)
  if(!is.null(profile$config$analysis_population))
    svg<-gsub("all CDM persons","selected cohort persons",svg,fixed=TRUE)
  writeLines(svg,file.path(path,"health_system_coverage.svg"),useBytes=TRUE)
  template<-paste(readLines(system.file("architecture","report.html",package="HealthSystemCoverage"),warn=FALSE),collapse="\n")
  if(!is.null(profile$config$analysis_population))
    template<-gsub("all CDM persons","selected cohort persons",template,fixed=TRUE)
  encoded<-jsonlite::toJSON(results, dataframe="rows",auto_unbox=TRUE,na="null",digits=10)
  # Avoid interpreting an untrusted source description as executable HTML.
  encoded<-gsub("<","\\u003c",encoded,fixed=TRUE)
  out<-sub("__SVG__",svg,template,fixed=TRUE)
  out<-sub("__DATA__",encoded,out,fixed=TRUE)
  meta<-jsonlite::toJSON(list(version=as.character(utils::packageVersion("HealthSystemCoverage")),
    source=if(is.null(profile$config$source_name))profile$config$cdm_schema else profile$config$source_name,
    generated=format(profile$generated_at,tz="UTC"), errors=if(is.null(profile$probe_errors))0L else nrow(profile$probe_errors)),auto_unbox=TRUE)
  meta<-gsub("<","\\u003c",meta,fixed=TRUE)
  out<-sub("__META__",meta,out,fixed=TRUE)
  out<-sub("</body>",paste0(event_filter_summary_html(profile),mapping_provenance_html(profile$components,profile$config),findings_html(profile$findings,profile$config$privacy_min_cell),
    crosswalk_audit_html(profile$evidence$source_crosswalk$data),context_audit_html(profile$evidence$context_audit$data,profile$config),"</body>"),out,fixed=TRUE)
  writeLines(out,file.path(path,"profile.html"),useBytes=TRUE)
  invisible(path)
}
