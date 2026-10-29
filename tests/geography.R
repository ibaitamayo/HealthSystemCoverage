library(HealthSystemCoverage)
# Entirely fictional geometry and populations, not a real geographic boundary.
areas<-structure(list(features=list(
 list(area_id="A",area_name="Area A",path="M0,0L100,0L100,100L0,100Z",x=50,y=50),
 list(area_id="B",area_name="Area B",path="M100,0L200,0L200,100L100,100Z",x=150,y=50)),
 width=200,height=100),class="hsc_health_areas")
d<-data.frame(area_id=rep(c("__ALL__","A","B","Outside"),2),
 node_id=rep(c("primary_care_encounters","emergency_care"),each=4),
 label=rep(c("Primary care","Emergency"),each=4),
 persons=c(35,10,20,5,27,2,20,5),population=rep(c(110,50,50,10),2))
p<-area_coverage_from_data(d,areas,source_name="<unsafe>fictional</unsafe>",attribution="Fictional test only")
with_ids<-d;with_ids$person_id<-"MUST_NOT_BE_EXPORTED"
stopifnot(!"person_id"%in%names(area_coverage_results(area_coverage_from_data(with_ids,areas))))
r<-area_coverage_results(p)
stopifnot(r$persons[r$area_id=="A"&r$node_id=="primary_care_encounters"]==10,
 is.na(r$persons[r$area_id=="A"&r$node_id=="emergency_care"]),
 is.na(r$percentage[r$area_id=="A"&r$node_id=="emergency_care"]),
 all(r$mapped[r$area_id%in%c("A","B")]),!any(r$mapped[r$area_id=="Outside"]))
bad<-d;bad$population[2]<-51
stopifnot(inherits(try(area_coverage_from_data(bad,areas),silent=TRUE),"try-error"))
bad<-d;bad$persons[2]<-51
stopifnot(inherits(try(area_coverage_from_data(bad,areas),silent=TRUE),"try-error"))
stopifnot(inherits(try(area_coverage_from_data(rbind(d,d[1,]),areas),silent=TRUE),"try-error"))
stopifnot(inherits(try(area_coverage_from_data(d[d$area_id!="__ALL__",],areas),silent=TRUE),"try-error"))
# Counts supplied as NA remain unavailable; no percentages invented.
unknown<-d;unknown$persons[2]<-NA_real_
stopifnot(is.na(area_coverage_results(area_coverage_from_data(unknown,areas))$percentage[2]))
out<-tempfile("hsc-areas-");write_area_report(p,out)
html<-paste(readLines(file.path(out,"area_report.html"),warn=FALSE),collapse="\n")
stopifnot(file.exists(file.path(out,"area_coverage_results.csv")),
 !grepl("__AREA_PAYLOAD__",html,fixed=TRUE),!grepl("<unsafe>",html,fixed=TRUE),
 !grepl("<script src",html,fixed=TRUE),!grepl("fetch(",html,fixed=TRUE),
 grepl("Filter the single profile",html,fixed=TRUE))
if(requireNamespace("sf",quietly=TRUE)) {
 x<-sf::st_as_sf(data.frame(code=c("A","B"),label=c("Area A","Area B"),
  wkt=c("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))","POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))")),wkt="wkt",crs=4326)
 m<-read_health_areas(x,"code","label")
 stopifnot(length(m$features)==2L,nzchar(m$features[[1]]$path),m$width>0,m$height>0)
 x$code<-c("A","A")
 stopifnot(inherits(try(read_health_areas(x,"code"),silent=TRUE),"try-error"))
}
