audit_source_crosswalk <- function(con,config) {
  table<-qualified_table(con,config$cdm_schema,"source_to_concept_map")
  concept<-qualified_table(con,config$cdm_schema,"concept")
  if(!table_exists(con,config$cdm_schema,"source_to_concept_map"))return(list(status="not_available",summary=data.frame()))
  base<-paste0(" FROM ",table," m LEFT JOIN ",concept," c ON c.concept_id=m.target_concept_id ")
  summary<-DBI::dbGetQuery(con,paste0("SELECT count(*)::bigint AS mapping_records,",
    "count(*) FILTER(WHERE c.concept_id IS NOT NULL)::bigint AS resolving_records,",
    "count(*) FILTER(WHERE c.concept_id IS NULL)::bigint AS unresolved_records,",
    "count(DISTINCT m.target_concept_id)::bigint AS distinct_target_ids,",
    "count(DISTINCT c.concept_id)::bigint AS distinct_resolving_target_ids,",
    "count(*) FILTER(WHERE m.invalid_reason IS NOT NULL)::bigint AS invalid_mapping_records",base))
  summary$target_resolved_fraction<-if(summary$mapping_records>0)as.double(summary$resolving_records)/as.double(summary$mapping_records) else NA_real_
  by_vocab<-DBI::dbGetQuery(con,paste0("SELECT m.source_vocabulary_id,count(*)::bigint AS mapping_records,",
    "count(DISTINCT m.source_code)::bigint AS distinct_codes,",
    "count(*) FILTER(WHERE c.concept_id IS NULL)::bigint AS unresolved_records,",
    "count(DISTINCT c.concept_id)::bigint AS distinct_resolving_target_ids",base," GROUP BY m.source_vocabulary_id"))
  domains<-DBI::dbGetQuery(con,paste0("SELECT m.source_vocabulary_id,c.domain_id,count(*)::bigint AS mapping_records,",
    "count(DISTINCT m.source_code)::bigint AS distinct_codes",base," WHERE c.concept_id IS NOT NULL GROUP BY 1,2"))
  candidates<-DBI::dbGetQuery(con,paste0("SELECT m.source_vocabulary_id,m.source_code,count(*)::bigint AS mapping_records,",
    "count(DISTINCT m.target_concept_id)::bigint AS distinct_target_ids,",
    "count(DISTINCT c.concept_id)::bigint AS distinct_resolving_target_ids",base," GROUP BY 1,2"))
  resolved<-as.double(candidates$distinct_resolving_target_ids)
  determination<-data.frame(unit="source_vocabulary_id + source_code",code_pairs=nrow(candidates),
    pairs_with_resolving_targets=sum(resolved>0),
    median_resolving_targets_all_pairs=if(length(resolved))stats::median(resolved) else NA_real_,
    median_resolving_targets_resolvable_pairs=if(any(resolved>0))stats::median(resolved[resolved>0]) else NA_real_,
    max_resolving_targets=if(length(resolved))max(resolved) else NA_real_)
  domains$domain_check<-"not_evaluated"
  expected<-config$crosswalk_domains
  if(!is.null(expected)&&nrow(expected))for(i in seq_len(nrow(domains))) {
    allowed<-as.character(expected$expected_domain[expected$source_vocabulary_id==domains$source_vocabulary_id[i]])
    if(length(allowed))domains$domain_check[i]<-if(domains$domain_id[i]%in%allowed)"compatible_with_declared_contract" else "detected_misclassified"
  }
  findings<-empty_findings()
  if(summary$unresolved_records>0)findings<-rbind(findings,data.frame(component_id="__SOURCE_CROSSWALK__",area_id="__ALL__",
    rule_id="unresolved_crosswalk_target",scope="instance_crosswalk_not_clinical_counts",action="metadata_present",
    in_scope=FALSE,excluded_by_filter=FALSE,rule_order=NA_integer_,incremental_exclusion=FALSE,records=as.double(summary$unresolved_records),persons=NA_real_,grain="event",count_unit="mapping_row"))
  incompatible<-sum(as.double(domains$mapping_records[domains$domain_check=="detected_misclassified"]))
  if(incompatible>0)findings<-rbind(findings,data.frame(component_id="__SOURCE_CROSSWALK__",area_id="__ALL__",
    rule_id="crosswalk_domain_contract_violation",scope="instance_crosswalk_not_clinical_counts",action="metadata_present",
    in_scope=FALSE,excluded_by_filter=FALSE,rule_order=NA_integer_,incremental_exclusion=FALSE,records=incompatible,persons=NA_real_,grain="event",count_unit="mapping_row"))
  list(status="assessed",summary=summary,by_vocabulary=by_vocab,domains=domains,
    candidates=candidates,determination=determination,expected_domains=expected,findings=findings,
    sources_examined="concept; source_to_concept_map (all rows for structural audit; active declared-vocabulary entries for event provenance)",
    sources_not_examined="concept_relationship reachability and external ETL documentation; event source vocabulary is not inferred from code shape")
}

write_crosswalk_audit <- function(audit,path) {
  if(is.null(audit))return(invisible(NULL))
  dir.create(file.path(path,"crosswalk_audit"),showWarnings=FALSE)
  for(nm in names(audit))if(is.data.frame(audit[[nm]]))utils::write.csv(audit[[nm]],file.path(path,"crosswalk_audit",paste0(nm,".csv")),row.names=FALSE,na="")
  writeLines(c(paste("Status:",audit$status),paste("Examined:",audit$sources_examined),paste("Not examined:",audit$sources_not_examined),
    "Structural crosswalk counts are instance metadata, not cohort/event counts. Domain incompatibility applies to the declared crosswalk contract, never automatically to clinical events."),file.path(path,"crosswalk_audit","SCOPE.txt"))
}

crosswalk_audit_html <- function(audit) {
  if(is.null(audit)||!identical(audit$status,"assessed"))return("")
  tables<-c("summary","by_vocabulary","determination","domains")
  content<-vapply(tables,function(nm){d<-audit[[nm]];if(is.null(d)||!nrow(d))return("")
    rows<-vapply(seq_len(nrow(d)),function(i)paste0("<tr>",paste(vapply(d[i,,drop=FALSE],function(v)paste0("<td>",html_escape(v),"</td>"),""),collapse=""),"</tr>"),"")
    paste0("<h3>",html_escape(nm),"</h3><table><tr>",paste0("<th>",html_escape(names(d)),"</th>",collapse=""),"</tr>",paste(rows,collapse=""),"</table>")},"")
  paste0("<section style='margin:24px;overflow:auto'><h2>Source crosswalk audit</h2><p>Instance metadata, not clinical cohort counts. Candidate unit: vocabulary + code. A documented mapping is not proof of correct clinical meaning. Domain checks use only provider-declared contracts.</p>",paste(content,collapse=""),"<p>Examined: ",html_escape(audit$sources_examined),". Not examined: ",html_escape(audit$sources_not_examined),".</p></section>")
}
