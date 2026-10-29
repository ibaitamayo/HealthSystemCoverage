# Geometry and population membership are separate, explicit inputs.
read_health_areas <- function(path,id_column,name_column=id_column,simplify=0) {
  if(!requireNamespace("sf",quietly=TRUE))stop("Install optional package sf to read geographic boundaries")
  if(!is.numeric(simplify)||length(simplify)!=1L||!is.finite(simplify)||simplify<0)stop("Invalid simplify tolerance")
  if(inherits(path,"sf")) x<-path else {
    if(length(path)!=1L||!file.exists(path))stop("Local geometry file does not exist")
    dsn<-if(grepl("[.]zip$",path,ignore.case=TRUE))paste0("/vsizip/",normalizePath(path)) else path
    x<-sf::st_read(dsn,quiet=TRUE)
  }
  if(!all(c(id_column,name_column)%in%names(x)))stop("Missing geographic identifier/name column")
  ids<-as.character(x[[id_column]]);labels<-as.character(x[[name_column]])
  if(!length(ids)||anyNA(ids)||any(!nzchar(ids))||anyDuplicated(ids)||any(ids%in%c("__ALL__","__UNKNOWN__")))
    stop("Geographic IDs must be unique, nonempty and not reserved")
  if(anyNA(labels)||any(!nzchar(labels)))stop("Missing geographic names")
  if(is.na(sf::st_crs(x)))stop("Geometry CRS must be declared; no CRS is guessed")
  if(any(sf::st_is_empty(x)))stop("Empty geographic feature")
  if(any(!as.character(sf::st_geometry_type(x))%in%c("POLYGON","MULTIPOLYGON")))stop("Only polygon/multipolygon boundaries are supported")
  # Display projection only; areas and health indicators are never estimated here.
  x<-sf::st_transform(sf::st_make_valid(x),3857)
  if(simplify>0)x<-sf::st_simplify(x,dTolerance=simplify,preserveTopology=TRUE)
  box<-sf::st_bbox(x);w<-as.double(box["xmax"]-box["xmin"]);h<-as.double(box["ymax"]-box["ymin"])
  if(!all(is.finite(c(w,h)))||w<=0||h<=0)stop("Invalid geographic extent")
  scale<-min(600/w,480/h)
  xy<-function(m)cbind(30+(m[,1]-box["xmin"])*scale,25+(box["ymax"]-m[,2])*scale)
  ring_path<-function(ring) {
    p<-xy(ring)
    paste0("M",paste(apply(p,1,function(z)paste(format(round(z,2),scientific=FALSE,trim=TRUE),collapse=",")),collapse="L"),"Z")
  }
  shapes<-lapply(seq_len(nrow(x)),function(i){
    g<-sf::st_geometry(x)[[i]]
    polys<-if(inherits(g,"POLYGON"))list(g) else g
    path<-paste(vapply(polys,function(poly)paste(vapply(poly,ring_path,""),collapse=" "),""),collapse=" ")
    anchor<-xy(sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(sf::st_geometry(x)[i]))))
    list(area_id=ids[i],area_name=labels[i],path=path,x=unname(anchor[1,1]),y=unname(anchor[1,2]))
  })
  structure(list(features=shapes,width=60+w*scale,height=50+h*scale,
    projection="EPSG:3857 display projection",simplify_metres=simplify),class="hsc_health_areas")
}

area_coverage_from_data <- function(data,areas,privacy_min_cell=5L,
                                    source_name="Health-system area comparison",
                                    population_note="",attribution="") {
  stopifnot(inherits(areas,"hsc_health_areas"),is.data.frame(data))
  required<-c("area_id","node_id","label","persons","population")
  if(!all(required%in%names(data)))stop("Area results require: ",paste(required,collapse=", "))
  if(!is.numeric(privacy_min_cell)||length(privacy_min_cell)!=1L||!is.finite(privacy_min_cell)||privacy_min_cell<0)stop("Invalid privacy_min_cell")
  d<-data[,intersect(c(required,"records","area_name","level","parent_id",quality_fields(),"reliability","status","issues"),names(data)),drop=FALSE]
  for(nm in c("area_id","node_id","label")) {
    d[[nm]]<-as.character(d[[nm]])
    if(anyNA(d[[nm]])||any(!nzchar(d[[nm]])))stop("Missing identifier or label")
  }
  if(anyDuplicated(d[,c("area_id","node_id")]))stop("Duplicated area/node result")
  for(nm in c("persons","population")) {
    if(!is.numeric(d[[nm]])||any(!is.na(d[[nm]])&(!is.finite(d[[nm]])|d[[nm]]<0|d[[nm]]!=floor(d[[nm]]))))stop("Invalid counts: ",nm)
  }
  if(any(d$persons>d$population,na.rm=TRUE))stop("Numerator exceeds area denominator")
  if(!"records"%in%names(d))d$records<-NA_real_
  if(!is.numeric(d$records)||any(!is.na(d$records)&(!is.finite(d$records)|d$records<0|d$records!=floor(d$records))))stop("Invalid records")
  if(!"level"%in%names(d))d$level<-"Component"
  if(!"parent_id"%in%names(d))d$parent_id<-""
  # Every indicator uses the same exclusive population partition, including
  # unmapped/outside/unknown groups. Global numerators are explicit, never sums
  # of overlapping service populations or child indicators.
  population<-split(d$population,d$area_id)
  if(any(vapply(population,function(v)length(unique(v))!=1L,logical(1))))stop("Area denominator changes between indicators")
  area_sets<-split(d$area_id,d$node_id)
  if(any(vapply(area_sets,function(v)!setequal(v,names(population)),logical(1))))stop("Every indicator must include the same area population groups")
  if(!"__ALL__"%in%d$area_id)stop("Supply explicit global rows with area_id='__ALL__'")
  for(id in unique(d$node_id)) {
    r<-d[d$node_id==id,,drop=FALSE];global<-r[r$area_id=="__ALL__",];parts<-r[r$area_id!="__ALL__",]
    if(length(unique(r$label))!=1L)stop("Indicator labels must be consistent")
    if(!anyNA(r$population)&&sum(parts$population)!=global$population)stop("Area denominators do not reconcile with global population")
    if(!anyNA(r$persons)&&sum(parts$persons)!=global$persons)stop("Exclusive area numerators do not reconcile with explicit global count")
  }
  features<-areas$features;ids<-vapply(features,`[[`,"","area_id");labels<-vapply(features,`[[`,"","area_name")
  if(!"area_name"%in%names(d))d$area_name<-d$area_id
  d$area_name<-as.character(d$area_name)
  hit<-match(d$area_id,ids);d$area_name[!is.na(hit)]<-labels[hit[!is.na(hit)]]
  d$area_name[d$area_id=="__ALL__"]<-"All selected people"
  d$area_name[d$area_id=="__UNKNOWN__"]<-"Unknown assignment"
  d$mapped<-d$area_id%in%ids
  structure(list(results=d,areas=areas,privacy_min_cell=privacy_min_cell,
    source_name=source_name,population_note=population_note,attribution=attribution,
    generated_at=Sys.time()),class="hsc_area_profile")
}

area_coverage_results <- function(profile) {
  stopifnot(inherits(profile,"hsc_area_profile"))
  d<-profile$results;k<-profile$privacy_min_cell
  small_population<-!is.na(d$population)&d$population>0&d$population<k
  complement<-d$population-d$persons
  hide<-small_population|(!is.na(d$persons)&d$persons>0&d$persons<k)|
    (!is.na(complement)&complement>0&complement<k)|is.na(d$persons)|is.na(d$population)
  d$measurement_status<-ifelse(hide,"suppressed",ifelse(d$population==0,"no_denominator","measured"))
  d$persons[hide]<-NA_real_
  d$records[hide|(!is.na(d$records)&d$records>0&d$records<k)]<-NA_real_
  d$population[small_population]<-NA_real_
  d$percentage<-ifelse(!hide&!is.na(d$population)&d$population>0,100*d$persons/d$population,NA_real_)
  suppress_small_cells(d,k)
}

profile_health_areas <- function(con,config,areas,cohort=NULL,location_column="county",
                                 crosswalk=NULL,components=NULL,population_note="",attribution="") {
  assert_connection(con)
  stopifnot(inherits(config,"health_system_profile_config"),inherits(areas,"hsc_health_areas"))
  if(config$dbms!="postgresql")stop("Currently supports PostgreSQL only")
  config<-prepare_context_config(con,config)
  if(length(location_column)!=1L||!location_column%in%c("city","state","county","zip","location_source_value","country_source_value"))stop("Unsupported location grouping column")
  s<-config$cdm_schema
  quote<-function(x)as.character(DBI::dbQuoteString(con,x))
  p<-qualified_table(con,s,"person");loc<-qualified_table(con,s,"location")
  raw<-paste0("NULLIF(btrim(l.",as.character(DBI::dbQuoteIdentifier(con,location_column)),"),'')")
  assignment<-paste0("COALESCE(",raw,",'__UNKNOWN__')")
  if(!is.null(crosswalk)) {
    stopifnot(is.data.frame(crosswalk),all(c("location_value","area_id")%in%names(crosswalk)))
    if(anyNA(crosswalk[,c("location_value","area_id")])||anyDuplicated(crosswalk$location_value)||any(!nzchar(crosswalk$area_id))||any(crosswalk$area_id=="__ALL__"))stop("Invalid geographic crosswalk")
    clauses<-vapply(seq_len(nrow(crosswalk)),function(i)paste0("WHEN ",raw,"=",quote(as.character(crosswalk$location_value[i]))," THEN ",quote(as.character(crosswalk$area_id[i]))),"")
    assignment<-if(length(clauses))paste0("CASE ",paste(clauses,collapse=" ")," ELSE '__UNKNOWN__' END") else "'__UNKNOWN__'"
  }
  scope<-if(is.null(cohort))"TRUE" else {
    q<-cohort_sql(con,cohort)
    # Match cohort profiling's interval integrity checks, without altering membership.
    bad<-DBI::dbGetQuery(con,paste0("SELECT count(*) AS n FROM (",q,") c LEFT JOIN ",p," p ON p.person_id=c.subject_id WHERE p.person_id IS NULL OR c.cohort_start_date IS NULL OR c.cohort_end_date IS NULL OR c.cohort_end_date<c.cohort_start_date"))
    if(as.double(bad$n)>0)stop("Review/filter unusable cohort intervals before geographic profiling")
    paste0("EXISTS (SELECT 1 FROM (",q,") c WHERE c.subject_id=p.person_id)")
  }
  person_areas<-paste0("SELECT p.person_id,",assignment," AS area_id FROM ",p," p LEFT JOIN ",loc," l ON l.location_id=p.location_id WHERE ",scope)
  den<-DBI::dbGetQuery(con,paste0("WITH pa AS (",person_areas,") SELECT area_id,count(*)::bigint AS population FROM pa GROUP BY area_id"))
  if(!nrow(den))stop("Empty selected population")
  if(any(den$area_id=="__ALL__"))stop("Reserved area_id __ALL__ in source")
  den$population<-as.double(den$population)
  den<-rbind(data.frame(area_id="__ALL__",population=sum(den$population)),den)
  queries<-coverage_event_queries(con,config,cohort,apply_event_filter=FALSE)
  if(!is.null(components)) {
    if(!length(components)||any(!components%in%names(queries)))stop("Unknown or unavailable component definition")
    queries<-queries[unique(components)]
  }
  member<-coverage_membership()
  nodes<-c(member$component_id,unique(member$group_id),unique(member$area_id))
  catalog_profile<-structure(list(config=config,components=empty_component_result(health_system_spec()),evidence=list()),class="health_system_profile")
  catalog<-coverage_results(catalog_profile)
  out<-list()
  for(node in nodes) {
    ids<-if(node%in%member$component_id)node else member$component_id[member$group_id==node|member$area_id==node]
    ids<-intersect(ids,names(queries));if(!length(ids))next
    events<-paste(vapply(queries[ids],function(q)paste0("(",q,")"),""),collapse=" UNION ")
    message("Area coverage: ",node)
    sql<-paste0("WITH pa AS (",person_areas,"),events AS (",events,") ",
      "SELECT CASE WHEN GROUPING(pa.area_id)=1 THEN '__ALL__' ELSE pa.area_id END AS area_id,",
      coverage_aggregate_sql()," ",
      "FROM events e JOIN pa ON pa.person_id=e.person_id GROUP BY GROUPING SETS ((pa.area_id),())")
    counts<-DBI::dbGetQuery(con,sql)
    z<-merge(den,counts,by="area_id",all.x=TRUE,sort=FALSE)
    count_fields<-names(z)[grepl("(^persons$|^records$|_records$|_persons$)",names(z))]
    for(nm in count_fields){z[[nm]]<-as.double(z[[nm]]);z[[nm]][is.na(z[[nm]])]<-0}
    c<-catalog[catalog$node_id==node,]
    z$node_id<-node;z$label<-c$label;z$level<-c$level;z$parent_id<-c$parent_id
    z<-enrich_coverage_stats(z,config)
    z$status<-ifelse(z$records==0,zero_coverage_status(node,config,ids),ifelse(z$mapping_records>0 & z$valid_standard_records==z$mapping_records,"detected_structured","detected_partial"))
    z$issues<-ifelse(z$records==0 & z$status=="unknown",zero_setting_issue(),NA_character_)
    z$reliability<-vapply(seq_len(nrow(z)),function(i){
      temporal<-intersect(c("before_min_retained_records","after_cutoff_retained_records","before_birth_retained_records","end_before_start_retained_records"),names(z))
      bad_dates<-sum(as.double(unlist(z[i,temporal,drop=FALSE],use.names=FALSE)),na.rm=TRUE)>0
      base<-if(z$records[i]==0)"unknown" else if(bad_dates || (z$mapping_records[i]>0 && z$valid_standard_records[i]<z$mapping_records[i]))"low" else "moderate"
      cap_reliability(base,z$reliability_cap[i])},"")
    out[[node]]<-z
  }
  if(!length(out))stop("No assessed geographic indicators")
  if(!nzchar(population_note))population_note<-paste0("Same selected people and event windows in all areas. Geography: person.location_id -> location.",location_column," (last known assignment, not historical residence). No additional person exclusions are applied.")
  if(!is.null(config$event_date_min))population_note<-paste0(population_note,
    " Event calendar bounds: ",config$event_date_min," through ",config$database_cutoff,
    " (",config$database_cutoff_source,"); drug end dates exempt only from the upper bound.")
  measured<-do.call(rbind,out)
  measured$status[measured$node_id%in%c("clinical_notes","group_documentation")&measured$records>0]<-"detected_unstructured"
  measured$status[measured$node_id%in%c("mental_health","maternal_prenatal_care")&measured$records>0]<-"detected_partial"
  if(!is.null(cohort))config$analysis_population<-list(name=cohort$name)
  context<-probe_context_audit(con,config,cohort)
  alerts<-context_alerts(context,config)
  if(any(alerts$rule_id=="unclassified_setting_fraction"))for(i in seq_len(nrow(measured))) {
    ids<-if(measured$node_id[i]%in%member$component_id)measured$node_id[i] else
      member$component_id[member$group_id==measured$node_id[i]|member$area_id==measured$node_id[i]]
    if(any(ids%in%visit_dependent_components())) {
      if(measured$reliability[i]%in%c("high","moderate","low","unusable"))
        measured$reliability[i]<-cap_reliability(measured$reliability[i],"low")
      measured$issues[i]<-paste(c(measured$issues[i][!is.na(measured$issues[i])],
        "Selected-population setting audit exceeded the unknown fraction threshold; this cap is profile-wide, not an area-specific estimate."),collapse=" ")
    }
  }
  result<-area_coverage_from_data(measured,areas,config$privacy_min_cell,
    if(is.null(config$source_name))s else config$source_name,population_note,attribution)
  result$crosswalk_audit<-audit_source_crosswalk(con,config)
  result$context_audit<-context
  result$config<-config
  result$findings<-rbind(coverage_findings(measured,config),
    context_person_findings(context,config),
    if(!is.null(result$crosswalk_audit$findings))result$crosswalk_audit$findings else empty_findings())
  result
}

write_area_report <- function(profile,path,level=c("summary","exhaustive")) {
  level<-match.arg(level)
  stopifnot(inherits(profile,"hsc_area_profile"))
  dir.create(path,recursive=TRUE,showWarnings=FALSE)
  d<-area_coverage_results(profile)
  if(level=="summary")return(write_summary_area_report(profile,path,d))
  utils::write.csv(d,file.path(path,"area_coverage_results.csv"),row.names=FALSE,na="")
  findings<-if(is.null(profile$findings))empty_findings() else profile$findings
  utils::write.csv(suppress_small_cells(findings,profile$privacy_min_cell),file.path(path,"quality_findings.csv"),row.names=FALSE,na="")
  write_crosswalk_audit(profile$crosswalk_audit,path)
  write_context_audit(profile$context_audit,profile$config,path)
  if(!is.null(profile$config$discharge_outcome_map))utils::write.csv(profile$config$discharge_outcome_map,
    file.path(path,"configured_discharge_outcomes.csv"),row.names=FALSE)
  if(!is.null(profile$config$provider_specialty_map))utils::write.csv(profile$config$provider_specialty_map,
    file.path(path,"configured_provider_specialties.csv"),row.names=FALSE)
  payload<-list(results=d,map=unclass(profile$areas),source=profile$source_name,
    population_note=profile$population_note,attribution=profile$attribution,
    generated=format(profile$generated_at,tz="UTC",usetz=TRUE))
  encoded<-jsonlite::toJSON(payload,dataframe="rows",auto_unbox=TRUE,na="null",null="null",digits=10)
  encoded<-gsub("<","\\u003c",encoded,fixed=TRUE)
  template<-paste(readLines(system.file("geography","report.html",package="HealthSystemCoverage"),warn=FALSE),collapse="\n")
  html<-sub("__AREA_PAYLOAD__",encoded,template,fixed=TRUE)
  html<-sub("</body>",paste0(findings_html(findings,profile$privacy_min_cell),crosswalk_audit_html(profile$crosswalk_audit),
    context_audit_html(profile$context_audit,profile$config),"</body>"),html,fixed=TRUE)
  writeLines(html,file.path(path,"area_report.html"),useBytes=TRUE)
  # Only masked aggregates and geometry are exported, never crosswalk/person IDs.
  writeLines(encoded,file.path(path,"area_coverage_results.json"),useBytes=TRUE)
  invisible(normalizePath(path))
}
