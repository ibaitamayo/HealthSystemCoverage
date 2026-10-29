# Presentation only: assessment definitions, counts and quality caps are unchanged.
representation_label <- function(x) unname(c(represented="Represented",
  represented_with_limits="Represented with limitations",not_detected="Not detected under assessed definition",
  not_evaluable="Not evaluable")[x])

component_representation <- function(status,reliability) {
  if(is.na(status)||status%in%c("not_assessed","unknown"))return("not_evaluable")
  if(status=="not_detected")return("not_detected")
  if(status=="detected_structured"&&!is.na(reliability)&&reliability%in%c("moderate","high"))return("represented")
  "represented_with_limits"
}

component_limitation <- function(row,config,setting_limited=FALSE) {
  if(row$status=="not_assessed")return("Assessment not implemented or configured; absence has not been tested.")
  if(row$status=="unknown")return("Available classification cannot distinguish absence from unrecognised or unlinked evidence.")
  if(row$status=="not_detected") {
    if(domain_is_empty(row$component_id,config))return("Source domain table is empty; this does not establish absence of the service.")
    return("No records matched the assessed definition within this population and time scope; source completeness is not established.")
  }
  if(row$status=="detected_misclassified")return("Semantic misclassification detected; presence does not establish usable clinical evidence.")
  if(!is.na(row$reliability)&&row$reliability=="unusable")return("Evidence fails the applied reliability policy; see the attached validation before use.")
  if(!is.na(row$provenance_reliability)&&row$provenance_reliability%in%c("low","unusable"))return("Source mapping provenance is insufficient under the applied audit policy.")
  if(setting_limited&&row$component_id%in%visit_dependent_components())return("Unidentified care settings exceed the declared threshold; attribution of this activity to a care environment is limited.")
  if(row$status=="detected_unstructured")return("Narrative records are present; structured clinical content and completeness are not established.")
  if(is.na(row$reliability)||!row$reliability%in%c("high","moderate"))return("Classification, linkage or data-quality limitations prevent a stronger conclusion; see validation.")
  if(row$status=="detected_partial")return("Evidence covers part of the activity, not necessarily its diagnoses, treatments and results.")
  "Presence under the assessed definition; completeness and research suitability are not established."
}

#' Compact hierarchical representation table
#' @param profile A health_system_profile object.
#' @return A data frame of areas, groups and activities. Counts are not additive.
#' @export
representation_results <- function(profile) {
  stopifnot(inherits(profile,"health_system_profile"))
  full<-coverage_results(profile);components<-profile$components
  context<-profile$evidence$context_audit$data
  alerts<-if(is.null(context))NULL else context_alerts(context,profile$config)
  setting_limited<-!is.null(alerts)&&any(alerts$rule_id=="unclassified_setting_fraction")
  components$representation<-vapply(seq_len(nrow(components)),function(i)
    component_representation(components$status[i],components$reliability[i]),"")
  member<-coverage_membership()
  out<-full[,c("node_id","parent_id","level","area","label","persons","percentage","start_date","end_date","measurement_status"),drop=FALSE]
  out$representation<-"not_evaluable";out$limitation<-""
  for(i in seq_len(nrow(out))) {
    id<-out$node_id[i]
    ids<-if(id%in%components$component_id)id else member$component_id[member$group_id==id|member$area_id==id]
    child<-components[components$component_id%in%ids,,drop=FALSE]
    states<-child$representation
    if(!nrow(child))next
    if(nrow(child)==1L) {
      out$representation[i]<-states
      out$limitation[i]<-component_limitation(child,profile$config,setting_limited)
    } else {
      present<-states%in%c("represented","represented_with_limits")
      out$representation[i]<-if(all(states=="represented"))"represented" else if(any(present))"represented_with_limits" else if(all(states=="not_detected"))"not_detected" else "not_evaluable"
      out$limitation[i]<-if(any(states=="not_evaluable"))"Some activities cannot be evaluated; group counts cover assessed definitions only." else if(any(present))"Unique people across assessed activities; overlapping activities must not be added. See child limitations." else "No matches under the assessed child definitions; not proof that the service does not exist."
    }
  }
  out[,c("node_id","parent_id","level","area","label","representation","persons","percentage","start_date","end_date","limitation","measurement_status")]
}

summary_validation <- function(profile) {
  context<-profile$evidence$context_audit$data
  cfg<-profile$config
  fields<-c("cdm_schema","source_name","analysis_population","cohort_attrition","database_cutoff",
    "database_cutoff_source","event_date_min","birth_date_rule","privacy_min_cell","unknown_setting_threshold",
    "mapping_audit_policy","source_aliases","component_rules","source_vocabularies","discharge_outcome_map","provider_specialty_map")
  list(package_version=as.character(utils::packageVersion("HealthSystemCoverage")),
    generated_at=format(profile$generated_at,tz="UTC",usetz=TRUE),
    scope_and_definitions=cfg[intersect(fields,names(cfg))],
    visit_concept_map=attr(cfg$source_aliases,"visit_concept_map"),
    visit_hierarchy=attr(cfg$source_aliases,"visit_hierarchy_map"),
    definition_validation=list(dispensing=if(is.null(context))NULL else context$dispensing_type_rules,
      source_dates=if(is.null(context))NULL else context$source_date_validation,
      scope="Attached existing evaluator evidence; not a new validation of every distributed concept set."),
    classification_alerts=if(is.null(context))NULL else context_alerts(context,cfg),
    setting_distribution=if(is.null(context))NULL else suppress_small_cells(context$setting_distribution,cfg$privacy_min_cell),
    components=suppress_small_cells(profile$components,cfg$privacy_min_cell),
    findings=suppress_small_cells(if(is.null(profile$findings))empty_findings() else profile$findings,cfg$privacy_min_cell),
    probe_errors=profile$probe_errors)
}

summary_rows_html <- function(rows) paste(vapply(seq_len(nrow(rows)),function(i) {
  z<-rows[i,];n<-if(is.na(z$persons))if(z$measurement_status=="suppressed")"Suppressed" else "Not measured" else format(z$persons,big.mark=",",scientific=FALSE,trim=TRUE)
  pct<-if(is.na(z$percentage))"\u2014" else paste0(format(round(z$percentage,2),trim=TRUE),"%")
  period<-if(is.na(z$start_date)||is.na(z$end_date))"\u2014" else paste(z$start_date,z$end_date,sep=" \u2013 ")
  paste0('<tr class="',z$representation,'"><td>',html_escape(z$label),'</td><td>',representation_label(z$representation),'</td><td>',n,'</td><td>',pct,'</td><td>',html_escape(period),'</td><td>',html_escape(z$limitation),'</td></tr>')
},""),collapse="")

write_summary_profile_bundle <- function(profile,path) {
  dir.create(path,recursive=TRUE,showWarnings=FALSE)
  # Refuse an old bundle rather than leave stale duplicate CSVs at the top level.
  if(any(file.exists(file.path(path,c("health_system_components.csv","quality_findings.csv","component_overview.html")))))
    stop("Use a new output directory for a summary bundle; old exhaustive files are not removed automatically")
  results<-representation_results(profile)
  utils::write.csv(results,file.path(path,"coverage_results.csv"),row.names=FALSE,na="")
  audit<-file.path(path,"audit");dir.create(audit,showWarnings=FALSE)
  validation<-summary_validation(profile)
  jsonlite::write_json(validation,file.path(audit,"validation.json"),auto_unbox=TRUE,pretty=TRUE,na="null",null="null",dataframe="rows")
  writeLines(paste0('<!doctype html><html lang="en"><meta charset="utf-8"><title>Evaluation validation</title><body><h1>Attached evaluation evidence</h1><p>Definitions, existing validation, scoped quality findings and probe failures. No general fitness-for-research certification is made.</p><p><a href="../profile.html">Return to system representation</a> \u00b7 <a href="validation.json">Download validation JSON</a></p><details><summary>Read technical evidence</summary><pre style="white-space:pre-wrap;overflow-wrap:anywhere">',html_escape(paste(readLines(file.path(audit,"validation.json")),collapse="\n")),'</pre></details></body></html>'),file.path(audit,"index.html"))
  full<-coverage_results(profile);full$status<-results$representation[match(full$node_id,results$node_id)]
  svg<-render_architectural_svg(profile,full)
  svg<-gsub("all CDM persons","selected population",svg,fixed=TRUE)
  writeLines(svg,file.path(path,"health_system_coverage.svg"),useBytes=TRUE)
  population<-if(is.null(profile$evidence$person$data))NA_real_ else as.double(profile$evidence$person$data$persons[1])
  title<-if(is.null(profile$config$source_name))profile$config$cdm_schema else profile$config$source_name
  groups<-coverage_groups();member<-coverage_membership()
  head<-'<tr><th>Activity</th><th>Representation</th><th>People</th><th>% population</th><th>Observed period</th><th>Main limitation</th></tr>'
  groups_html<-paste(vapply(seq_len(nrow(groups)),function(i) {
    gid<-paste0("group_",groups$group_id[i]);parent<-results[results$node_id==gid,]
    children<-results[results$parent_id==gid,]
    paste0('<details id="',gid,'"><summary>',html_escape(groups$title[i]),' \u2014 ',representation_label(parent$representation),'</summary><div class="table"><table>',head,summary_rows_html(parent),summary_rows_html(children),'</table></div></details>')
  },""),collapse="")
  leaf<-results[results$level=="Subcomponent",]
  list_html<-function(state,title) {
    z<-leaf[leaf$representation==state,]
    if(!nrow(z))return("")
    paste0('<details><summary>',title,' \u00b7 ',nrow(z),'</summary><ul>',paste0('<li>',html_escape(z$label),' \u2014 ',html_escape(z$limitation),'</li>',collapse=""),'</ul></details>')
  }
  cutoff<-profile$config$database_cutoff
  scope<-paste0('Population: ',html_escape(format(population,big.mark=",",scientific=FALSE,trim=TRUE)),
    if(is.null(profile$config$analysis_population))' CDM persons' else ' selected cohort persons',
    '. Event cutoff: ',html_escape(as.character(cutoff)),' (',html_escape(profile$config$database_cutoff_source),
    '). Lower bound: ',html_escape(as.character(profile$config$event_date_min)),'. Birth rule: ',html_escape(profile$config$birth_date_rule),'.')
  selection<-profile$config$cohort_attrition
  if(!is.null(selection)&&nrow(selection))scope<-paste0(scope,' Selection: ',paste(vapply(seq_len(nrow(selection)),function(i)paste(html_escape(selection$rule[i]),html_escape(selection$persons[i]),sep=": "),""),collapse=" \u2192 "),'.')
  errors<-profile$probe_errors
  warning<-if(!is.null(errors)&&nrow(errors))paste0('<p role="alert">',nrow(errors),' probe(s) failed. Affected evidence is incomplete; do not interpret failure as absence. See validation.</p>') else ""
  html<-paste0('<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Health-system representation</title><style>body{font:16px/1.5 system-ui;color:#263b48;background:#f5f7f8;max-width:1450px;margin:auto;padding:20px}h1{font-size:25px}h2{font-size:20px}section{background:white;padding:16px;margin:16px 0;border:1px solid #d5dfe4;border-radius:7px}.architecture svg{width:100%;height:auto}details{margin:10px 0}summary{cursor:pointer;padding:7px 0}.table{overflow:auto}table{border-collapse:collapse;width:100%}td,th{padding:9px;text-align:left;vertical-align:top;border-bottom:1px solid #d5dfe4}a{color:#245b78}.represented td:first-child{border-left:5px solid #60866a}.represented_with_limits td:first-child{border-left:5px solid #bd913c}.not_detected td:first-child{border-left:5px solid #b66b72}.not_evaluable td:first-child{border-left:5px solid #9ba5ac}nav{display:flex;gap:18px;flex-wrap:wrap}</style><h1>',html_escape(title),'</h1><p>',scope,'</p><nav><a href="coverage_results.csv">Representation table CSV</a><a href="health_system_coverage.svg">Architectural SVG</a><a href="audit/index.html">Definitions and validation</a></nav>',warning,
    '<section><p>Green: represented \u00b7 Amber: represented with limitations \u00b7 Red: not detected under the assessed definition \u00b7 Grey: not evaluable.</p><div class="architecture">',svg,'</div><p>Bars show people with matching evidence / selected population, not completeness or research suitability. Group counts are unique-person unions of assessed definitions, not sums of child counts.</p></section><section><h2>What cannot be observed or evaluated?</h2>',list_html("not_detected","Not detected under the assessed scope"),list_html("not_evaluable","Not evaluable"),'<p>No services have been declared not applicable by this report; absence of records does not establish non-applicability.</p></section><section><h2>Explore activities</h2>',groups_html,'</section><p>HealthSystemCoverage ',as.character(utils::packageVersion("HealthSystemCoverage")),' \u00b7 <a href="audit/index.html">Attached evaluation evidence</a></p><script>for(const region of document.querySelectorAll(".region[data-node]")){const open=()=>{const target=document.getElementById(region.dataset.node);if(target){target.open=true;target.scrollIntoView({behavior:"smooth",block:"start"});}};region.addEventListener("click",open);region.addEventListener("keydown",e=>{if(e.key==="Enter"||e.key===" "){e.preventDefault();open();}});}</script></html>')
  writeLines(html,file.path(path,"profile.html"),useBytes=TRUE)
  writeLines(c("# Health-system representation summary","","Default since 0.2.7: one 12-column hierarchical CSV, architectural SVG and report; attached evidence in audit/.","Percentages measure people with evidence, not completeness. Area/group/child counts overlap and are not additive.","For the legacy full set of CSVs and reports use write_profile_bundle(profile, a_new_path, level = 'exhaustive')."),file.path(path,"README.md"))
  invisible(normalizePath(path))
}

#' Write an aggregate profile bundle
#' @param profile A health_system_profile object.
#' @param path Output directory; use a fresh directory when changing levels.
#' @param level Summary by default, or exhaustive for legacy exports.
#' @export
write_profile_bundle <- function(profile,path,level=c("summary","exhaustive")) {
  stopifnot(inherits(profile,"health_system_profile"))
  level<-match.arg(level)
  if(level=="exhaustive")write_profile_bundle_exhaustive(profile,path) else write_summary_profile_bundle(profile,path)
}
