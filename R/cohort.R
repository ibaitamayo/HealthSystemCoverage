cohort_fields <- function() c("cohort_definition_id","subject_id","cohort_start_date","cohort_end_date")

# Cohort SQL is generated from validated table identifiers or local membership,
# never from an ATLAS JSON or an arbitrary user SQL string.
cohort_from_table <- function(schema, table, cohort_id, name=table) {
  stopifnot(is.character(schema),length(schema)==1L,nzchar(schema),
    is.character(table),length(table)==1L,nzchar(table),length(cohort_id)==1L,
    grepl("^[0-9]+$",as.character(cohort_id)))
  structure(list(kind="table",schema=schema,table=table,cohort_id=as.character(cohort_id),
    name=name),class="hsc_cohort")
}

cohort_from_data <- function(data, cohort_id=NULL, name="Imported OHDSI cohort") {
  stopifnot(is.data.frame(data),all(cohort_fields() %in% names(data)))
  data <- data[,cohort_fields(),drop=FALSE]
  for(nm in c("cohort_definition_id","subject_id")) {
    if(is.numeric(data[[nm]]) && any(abs(data[[nm]])>=2^53,na.rm=TRUE))
      stop("Use character IDs to preserve integers >= 2^53")
    data[[nm]] <- as.character(data[[nm]])
    if(anyNA(data[[nm]]) || any(!grepl("^[0-9]+$",data[[nm]])))stop("Invalid numeric identifier: ",nm)
  }
  if(is.null(cohort_id)) {
    ids <- unique(data$cohort_definition_id)
    if(length(ids)>1L)stop("Select a single cohort_id explicitly")
  } else data <- data[data$cohort_definition_id==as.character(cohort_id),,drop=FALSE]
  for(nm in c("cohort_start_date","cohort_end_date")) {
    original <- data[[nm]]
    data[[nm]] <- as.Date(original)
    if(any(is.na(data[[nm]]) & !is.na(original)))stop("Unparseable date in ",nm)
  }
  duplicates <- nrow(data)-nrow(unique(data))
  structure(list(kind="data",data=unique(data),name=name,duplicate_rows=duplicates),class="hsc_cohort")
}

print.hsc_cohort <- function(x,...) {
  cat("OHDSI cohort:",x$name,"\nSource:",x$kind,"\n")
  if(x$kind=="data")cat("Membership intervals:",nrow(x$data),"; exact duplicates removed:",x$duplicate_rows,"\n")
  invisible(x)
}

cohort_sql <- function(con,cohort) {
  stopifnot(inherits(cohort,"hsc_cohort"))
  if(cohort$kind=="filtered")return(cohort$sql)
  if(cohort$kind=="table")return(paste0("SELECT DISTINCT cohort_definition_id, subject_id, ",
    "cohort_start_date::date, cohort_end_date::date FROM ",qualified_table(con,cohort$schema,cohort$table),
    " WHERE cohort_definition_id=",DBI::dbQuoteString(con,cohort$cohort_id),"::bigint"))
  d <- cohort$data
  if(!nrow(d))return("SELECT NULL::bigint AS cohort_definition_id,NULL::bigint AS subject_id,NULL::date AS cohort_start_date,NULL::date AS cohort_end_date WHERE false")
  date_sql <- function(v) {
    out<-rep("NULL::date",length(v));known<-!is.na(v)
    out[known]<-paste0(DBI::dbQuoteString(con,as.character(v[known])),"::date");out
  }
  rows <- paste0("(",DBI::dbQuoteString(con,d$cohort_definition_id),"::bigint,",
    DBI::dbQuoteString(con,d$subject_id),"::bigint,",date_sql(d$cohort_start_date),",",
    date_sql(d$cohort_end_date),")")
  paste0("SELECT * FROM (VALUES ",paste(rows,collapse=","),") c(",paste(cohort_fields(),collapse=","),")")
}

cohort_size <- function(con,query) {
  x <- DBI::dbGetQuery(con,paste0("SELECT count(*)::bigint AS episodes,count(DISTINCT subject_id)::bigint AS persons FROM (",query,") cohort_membership"))
  x$persons<-as.double(x$persons);x$episodes<-as.double(x$episodes);x
}

review_cohort <- function(con, config, cohort, cutoff=Sys.Date()) {
  assert_connection(con)
  q <- cohort_sql(con,cohort)
  p <- qualified_table(con,config$cdm_schema,"person")
  op_available <- table_exists(con,config$cdm_schema,"observation_period")
  if(op_available) {
    op <- qualified_table(con,config$cdm_schema,"observation_period")
    from <- paste0(" LEFT JOIN ",op," op ON op.person_id=c.subject_id ")
    end <- "max(LEAST(op.observation_period_end_date,c.cohort_end_date)) FILTER (WHERE op.observation_period_start_date<=c.cohort_start_date AND op.observation_period_end_date>=c.cohort_start_date)"
  } else {from<-"";end<-"NULL::date"}
  base <- paste0("SELECT c.*,p.person_id AS linked_person_id,p.year_of_birth,",end," AS observed_end FROM (",q,") c LEFT JOIN ",p," p ON p.person_id=c.subject_id",from,
    " GROUP BY c.cohort_definition_id,c.subject_id,c.cohort_start_date,c.cohort_end_date,p.person_id,p.year_of_birth")
  date <- paste0(DBI::dbQuoteString(con,as.character(as.Date(cutoff))),"::date")
  flags <- paste0("SELECT b.*, linked_person_id IS NULL AS missing_person,",
    "(cohort_start_date IS NULL OR cohort_end_date IS NULL OR cohort_end_date<cohort_start_date) AS invalid_interval,",
    "(cohort_start_date>",date," OR cohort_end_date>",date,") AS future_interval,",
    "extract(year FROM cohort_start_date)<year_of_birth AS before_birth_year,",
    if(op_available)"observed_end IS NULL" else "NULL::boolean"," AS no_observation_at_index,",
    "(observed_end IS NOT NULL AND observed_end<=cohort_start_date) AS zero_post_index_days FROM (",base,") b")
  rules <- c("missing_person","invalid_interval","future_interval","before_birth_year","no_observation_at_index","zero_post_index_days")
  counts <- lapply(rules,function(rule) {
    x<-DBI::dbGetQuery(con,paste0("SELECT count(*) FILTER (WHERE ",rule,")::bigint AS episodes,count(DISTINCT subject_id) FILTER (WHERE ",rule,")::bigint AS persons FROM (",flags,") f"))
    data.frame(rule=rule,persons=as.double(x$persons),episodes=as.double(x$episodes),
      assessable=!(rule=="no_observation_at_index" && !op_available))
  })
  structure(list(cohort=cohort,query=flags,counts=do.call(rbind,counts),
    baseline=cohort_size(con,q),cutoff=as.Date(cutoff)),class="hsc_cohort_review")
}

print.hsc_cohort_review <- function(x,...) {
  cat("Quality review:",x$cohort$name,"\nNo filters applied. Flag counts may overlap.\n")
  print(x$counts,row.names=FALSE);invisible(x)
}

apply_cohort_filters <- function(con,review,rules) {
  stopifnot(inherits(review,"hsc_cohort_review"),is.character(rules),!anyDuplicated(rules))
  if(any(!rules %in% review$counts$rule))stop("Unknown filter rule")
  if(any(!review$counts$assessable[match(rules,review$counts$rule)]))stop("Selected rule cannot be assessed")
  original<-cohort_sql(con,review$cohort)
  sizes<-list(data.frame(step=0L,rule="Selected cohort",review$baseline))
  query<-original
  for(i in seq_along(rules)) {
    where<-paste0("NOT COALESCE(",rules[seq_len(i)],",false)",collapse=" AND ")
    query<-paste0("SELECT ",paste(cohort_fields(),collapse=",")," FROM (",review$query,") f WHERE ",where)
    sizes[[i+1L]]<-data.frame(step=i,rule=rules[i],cohort_size(con,query))
  }
  cohort<-structure(list(kind="filtered",sql=query,name=review$cohort$name,accepted_rules=rules),class="hsc_cohort")
  list(cohort=cohort,attrition=do.call(rbind,sizes),proposed_rules=review$counts,cutoff=review$cutoff)
}
