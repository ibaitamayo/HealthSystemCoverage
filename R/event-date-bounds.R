# Calendar bounds only: never repair data, change membership or infer ETL dates.
event_date_fields <- function() list(
  visit_occurrence=c("visit_start_date","visit_end_date"),
  condition_occurrence=c("condition_start_date","condition_end_date"),
  drug_exposure=c("drug_exposure_start_date","drug_exposure_end_date","verbatim_end_date"),
  measurement="measurement_date",observation="observation_date",
  specimen="specimen_date",death="death_date",note="note_date",
  procedure_occurrence=c("procedure_date","procedure_end_date"),
  device_exposure=c("device_exposure_start_date","device_exposure_end_date"))

event_date_exclusions <- function(con,config,table,alias="d",columns=NULL) {
  if(is.null(config$event_date_min))return(list(before="FALSE",after="FALSE",any="FALSE"))
  fields<-event_date_fields()[[table]]
  if(is.null(fields))return(list(before="FALSE",after="FALSE",any="FALSE"))
  if(!is.null(columns))fields<-intersect(fields,columns)
  if(!length(fields))stop("No recognised event date column in ",table)
  quote<-function(x)paste0(DBI::dbQuoteString(con,as.character(x)),"::date")
  upper<-if(table=="drug_exposure")setdiff(fields,c("drug_exposure_end_date","verbatim_end_date")) else fields
  compare<-function(cols,op,bound) {
    if(!length(cols))return("FALSE")
    paste0("COALESCE(",alias,".",cols,op,quote(bound),",false)",collapse=" OR ")
  }
  before<-compare(fields,"<",config$event_date_min)
  after<-compare(upper,">",config$database_cutoff)
  list(before=before,after=after,any=paste0("(",before,") OR (",after,")"))
}

event_date_predicate <- function(con,config,table,alias="d",columns=NULL) {
  paste0("NOT (",event_date_exclusions(con,config,table,alias,columns)$any,")")
}

# Scope audit by selected people, not by already-valid event windows: otherwise
# impossible dates would disappear before they could be counted. No IDs exported.
review_event_dates <- function(con,config,cohort=NULL,
                               tables=setdiff(names(event_date_fields()),"note")) {
  assert_connection(con)
  if(is.null(config$event_date_min))stop("Enable event_date_min and set the verified database_cutoff first")
  q<-if(is.null(cohort))NULL else cohort_sql(con,cohort)
  if(any(!tables %in% names(event_date_fields())) || anyDuplicated(tables))stop("Unknown or duplicated event audit table")
  out<-list()
  for(t in tables) {
    if(!table_exists(con,config$cdm_schema,t))next
    flags<-event_date_exclusions(con,config,t,columns=table_columns(con,config$cdm_schema,t))
    scope<-if(is.null(q))"TRUE" else paste0("EXISTS (SELECT 1 FROM (",q,") cw WHERE cw.subject_id=d.person_id)")
    sql<-paste0("SELECT count(*)::bigint AS records,",
      "count(*) FILTER(WHERE ",flags$before,")::bigint AS before_min_records,",
      "count(*) FILTER(WHERE ",flags$after,")::bigint AS after_cutoff_records,",
      "count(*) FILTER(WHERE ",flags$any,")::bigint AS excluded_records,",
      "count(DISTINCT d.person_id) FILTER(WHERE ",flags$any,")::bigint AS affected_persons FROM ",
      qualified_table(con,config$cdm_schema,t)," d WHERE ",scope)
    started<-proc.time()[[3]]
    z<-DBI::dbGetQuery(con,sql)
    z$table<-t;z$elapsed_seconds<-proc.time()[[3]]-started
    z$scope<-if(is.null(q))"All records" else "All records of selected persons, including outside cohort windows"
    out[[t]]<-z
  }
  if(!length(out))return(data.frame())
  do.call(rbind,out)
}

event_filter_summary_html <- function(profile) {
  cfg<-profile$config
  if(is.null(cfg$event_date_min))return("")
  audit<-profile$evidence$event_dates$data
  text<-paste0("Event calendar filter: ",cfg$event_date_min," to ",cfg$database_cutoff,
    " (cutoff source: ",if(is.null(cfg$database_cutoff_source))"provided" else cfg$database_cutoff_source,")",
    ". Inclusive bounds. Drug end dates are exempt from the upper bound only. ",
    "People remain in the denominator. Missing dates are not excluded by this rule. ",
    "Diagnostic evidence other than event_dates may describe unfiltered source records.")
  rows<-""
  if(!is.null(audit)&&nrow(audit)) {
    audit<-suppress_small_cells(audit,cfg$privacy_min_cell)
    rows<-paste(vapply(seq_len(nrow(audit)),function(i)paste0("<tr><td>",html_escape(audit$table[i]),
      "</td><td>",audit$records[i],"</td><td>",audit$excluded_records[i],"</td><td>",audit$affected_persons[i],"</td></tr>"),""),collapse="")
    rows<-paste0("<p>Audit covers all records of selected people, including outside cohort windows; counts are not the number removed from each component. People may overlap across domains. NA denotes suppression.</p>",
      "<table><thead><tr><th>Domain</th><th>Records reviewed</th><th>Outside bounds</th><th>People affected</th></tr></thead><tbody>",rows,"</tbody></table>")
  } else rows<-"<p>Domain audit was not run. Filtering is active in quantitative coverage queries.</p>"
  paste0("<section style='margin:24px;padding:20px;background:#f4f7f9'><h2>Event date exclusions</h2><p>",html_escape(text),"</p>",rows,"</section>")
}
