selection_flow_svg <- function(attrition,drawn_cases=NULL,drawn_persons=NULL) {
  a<-attrition
  if(!is.null(drawn_cases))a<-rbind(a,data.frame(step=max(a$step)+1L,
    rule="Cases with drawable contacts (not a cohort exclusion)",episodes=drawn_cases,
    persons=if(is.null(drawn_persons))NA_real_ else drawn_persons))
  w<-260*nrow(a);labels<-c(no_observation_at_index="Observation at index",zero_post_index_days="Post-index observed days")
  parts<-vapply(seq_len(nrow(a)),function(i) {
    x<-(i-1)*260+12
    label<-a$rule[i];if(label %in% names(labels))label<-labels[[label]]
    last<-i==nrow(a)&&!is.null(drawn_cases)
    if(last)label<-if(is.na(a$persons[i]))"Drawable contact cases" else "People with drawable contacts"
    loss<-if(i>1)a$persons[i-1]-a$persons[i] else 0
    value<-if(is.na(a$persons[i]))a$episodes[i] else a$persons[i]
    caption<-if(i==1)"Reference population" else if(last)
      paste(drawn_cases,"cases;",if(is.na(loss))"people not linked" else paste(loss,"people without contacts")) else paste(loss,"not retained at this step")
    paste0('<rect x="',x,'" y="16" width="230" height="107" rx="7" fill="#f3f6f8" stroke="#ccd6de"/>',
      '<text x="',x+12,'" y="43" font-size="12">',html_escape(label),'</text>',
      '<text x="',x+12,'" y="79" font-size="28">',format(value,big.mark=",",trim=TRUE),'</text>',
      '<text x="',x+12,'" y="106" font-size="11">',html_escape(caption),'</text>',
      if(i<nrow(a))paste0('<path d="M',x+232,' 69 h24 m-6 -5 l6 5 -6 5" stroke="#627987" fill="none"/>') else "")
  },"")
  paste0('<svg xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Cohort selection and drawable trajectories" viewBox="0 0 ',w,' 140" style="width:100%;font-family:Arial">',paste(parts,collapse=""),'</svg>')
}

write_cohort_workflow <- function(profile,selection,events,path) {
  if(!requireNamespace("htmlwidgets",quietly=TRUE))stop("htmlwidgets is required for saving process maps")
  dir.create(path,recursive=TRUE,showWarnings=FALSE)
  write_profile_bundle(profile,path)
  a<-if(is.data.frame(selection))selection else selection$attrition
  utils::write.csv(a,file.path(path,"cohort_attrition.csv"),row.names=FALSE)
  summaries<-list()
  for(mode in c("first","all")) {
    first<-mode=="first"
    if(nrow(events)) {
      log<-care_eventlog(events,first_only=first)
      map<-plot_care_process(log)
      # Native frequency map contains an aggregate graph, not case tokens or dates.
      if(!is.null(map$x$tokens)&&NROW(map$x$tokens)>0)stop("Refusing to export case tokens")
      htmlwidgets::saveWidget(map,file.path(path,paste0("process-",mode,".html")),selfcontained=TRUE)
    } else writeLines('<!doctype html><html lang="en"><meta charset="utf-8"><title>No drawable contacts</title><body><p>No drawable contacts. Cohort population remains in the system coverage denominator.</p></body></html>',file.path(path,paste0("process-",mode,".html")))
    sum<-care_process_summary(events,first_only=first,privacy_min_cell=profile$config$privacy_min_cell)
    summaries[[mode]]<-sum
    utils::write.csv(sum$nodes,file.path(path,paste0("process_",mode,"_nodes.csv")),row.names=FALSE,na="")
    utils::write.csv(sum$flows,file.path(path,paste0("process_",mode,"_flows.csv")),row.names=FALSE,na="")
    utils::write.csv(data.frame(mode=mode,drawn_cases=sum$drawn_cases,day_blocks=sum$day_blocks,
      simultaneous_blocks=sum$simultaneous_blocks),file.path(path,paste0("process_",mode,"_summary.csv")),row.names=FALSE)
  }
  drawn_persons<-if("person_id" %in% names(events))length(unique(events$person_id)) else NULL
  flow<-selection_flow_svg(a,summaries$first$drawn_cases,drawn_persons)
  overview<-plot_care_overview(events,privacy_min_cell=profile$config$privacy_min_cell)
  info<-attr(overview,"display_summary")
  htmlwidgets::saveWidget(overview,file.path(path,"process-overview.html"),selfcontained=TRUE)
  overview_flows<-summaries$first$flows
  overview_flows<-overview_flows[!is.na(overview_flows$cases)&overview_flows$cases>=info$threshold,,drop=FALSE]
  utils::write.csv(overview_flows,file.path(path,"process_overview_flows.csv"),row.names=FALSE,na="")
  utils::write.csv(as.data.frame(info),file.path(path,"overview_display.csv"),row.names=FALSE)
  overview_caption<-paste("Showing",info$displayed_transitions,"of",info$total_transitions,
    "distinct transitions, each seen in at least",info$threshold,"cases (1% display threshold or cell minimum). No cases or events are excluded; hidden edges are not reconnected. Same-day sets remain unordered.")
  writeLines(flow,file.path(path,"selection-flow.svg"))
  title<-if(is.null(profile$config$analysis_population))"Selected cohort" else profile$config$analysis_population$name
  html<-paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Cohort care trajectories</title><style>',
    'body{margin:0;background:#f4f6f8;color:#263b48;font:15px/1.5 Arial}header,main{max-width:1500px;margin:auto;padding:20px}h1{font-size:24px;font-weight:500;margin:0}h2{font-size:19px;font-weight:500}p{max-width:1100px}.panel{background:white;border:1px solid #dce3e8;border-radius:9px;padding:18px;margin-bottom:18px}nav{display:flex;gap:8px;flex-wrap:wrap}button,a{font:inherit;color:#2b526b}button,.link{padding:8px 13px;border:1px solid #cfdae1;border-radius:5px;background:white;text-decoration:none;cursor:pointer}button[aria-pressed=true]{background:#e5eef3}.note{font-size:13px;color:#586b79}.warn{background:#fff5e3;padding:12px;border-radius:6px}iframe{width:100%;height:780px;border:0;background:white}#selection{overflow-x:auto}#selection svg{min-width:760px}#map-title{margin-bottom:4px}@media(max-width:700px){header,main{padding:12px}iframe{height:600px}}</style></head><body>',
    '<header><h1>HealthSystemCoverage 0.2.0</h1><p>',html_escape(title),'</p><nav><a class="link" href="profile.html">Architectural system view &amp; results</a><a class="link" href="coverage_results.csv">Coverage CSV</a><a class="link" href="cohort_attrition.csv">Selection CSV</a><a class="link" href="selection-flow.svg">Selection SVG</a></nav></header><main>',
    '<section class="panel"><h2>Population selection</h2><div id="selection">',flow,'</div><p class="note">Reference population, approved cohort exclusions, then cases with drawable contacts. Missing contacts do not remove people from the cohort coverage denominator. Observation periods describe recorded event bounds, not verified enrolment.</p></section>',
    '<section class="panel"><h2 id="map-title">Overview: frequent first-contact transitions</h2><nav><button id="overview" aria-pressed="true">Overview</button><button id="first" aria-pressed="false">First contacts: complete</button><button id="all" aria-pressed="false">All contacts</button><a class="link" id="standalone" href="process-overview.html">Open full-size map</a><a class="link" id="flows" href="process_overview_flows.csv">Flow table CSV</a></nav>',
    '<p class="note" id="mode-note">',html_escape(overview_caption),'</p>',
    '<iframe id="map" title="Observed care contact process map" src="process-overview.html"></iframe>',
    '<p class="note">Maps generated with bupaR / processmapR. Arrows join successive observed start-date blocks, not completed treatments or discharges. Start/end nodes mean observed-window boundaries, not entry to or exit from the health system. Hospital-coded and hospice-coded contacts do not establish a verified specialty or admission type.</p>',
    '<p class="warn note">Internal research output. Native process-map frequencies are not small-cell suppressed. Aggregate CSVs mask positive counts below the configured threshold; overlapping outputs are not a disclosure-control guarantee. Review before sharing.</p></section>',
    '<section class="panel"><h2>Interpretation</h2><p>This is a trajectory of contacts for the selected population, not an inferred oncology pathway. Comorbidities and unrelated care may contribute. The current prototype maps visit contacts; laboratory, pathology, medicines and other activities can be added through explicit definitions without assigning them to the disease by assumption.</p></section></main>',
    '<script>const overviewCaption=',jsonlite::toJSON(overview_caption,auto_unbox=TRUE),';const frame=document.getElementById("map");for(const mode of ["overview","first","all"]){document.getElementById(mode).onclick=()=>{frame.src="process-"+mode+".html";document.getElementById("standalone").href=frame.src;document.getElementById("flows").href="process_"+mode+"_flows.csv";for(const m of ["overview","first","all"])document.getElementById(m).setAttribute("aria-pressed",String(mode===m));document.getElementById("map-title").textContent=mode==="overview"?"Overview: frequent first-contact transitions":mode==="first"?"First observed contact of each type":"All observed contact-day blocks";document.getElementById("mode-note").textContent=mode==="overview"?overviewCaption:mode==="first"?"Only the first occurrence of each contact type in each cohort interval. Same-day activities form an unordered set (+), not a known sequence.":"All contact types observed on each date, collapsed to one unordered daily set. Node and arrow frequencies count observed blocks/transitions, not unique patients."}}</script></body></html>')
  writeLines(html,file.path(path,"workflow.html"),useBytes=TRUE)
  invisible(normalizePath(path))
}
