#' @export
print.health_system_profile <- function(x, ...) {
  cat("Health-system coverage profile\n")
  cat("Schema:", x$config$cdm_schema, "\n")
  cat("Generated:", format(x$generated_at), "\n\n")
  print(table(x$components$status, useNA = "ifany"))
  invisible(x)
}

map_group_status <- function(status) {
  represented <- status %in% c("detected_structured", "detected_partial")
  problematic <- status %in% c("detected_misclassified",
                                "detected_unstructured")
  if (any(problematic)) return("detected_misclassified")
  if (all(status == "detected_structured")) return("detected_structured")
  if (any(represented)) return("detected_partial")
  if (all(status == "not_detected")) return("not_detected")
  "unknown"
}

map_status_label <- function(status) {
  labels <- c(
    detected_structured = "Structured",
    detected_partial = "Partial",
    detected_unstructured = "Unstructured",
    detected_misclassified = "Misclassified",
    not_detected = "Not detected",
    not_assessed = "Not assessed",
    unknown = "Unknown"
  )
  unname(labels[status])
}

system_icon <- function(name) {
  paths <- switch(
    name,
    clinic = "<path d='M4 20V8l8-4 8 4v12M9 20v-5h6v5M12 7v5M9.5 9.5h5'/>",
    pharmacy = "<path d='M5 20V7h14v13M9 7V4h6v3M12 10v6M9 13h6'/>",
    consultation = "<circle cx='8' cy='8' r='3'/><path d='M3 20v-2a5 5 0 0 1 10 0v2M15 8h6M18 5v6'/>",
    prevention = "<path d='M12 3l7 3v5c0 5-3 8-7 10-4-2-7-5-7-10V6l7-3zM12 7v8M8 11h8'/>",
    emergency = "<path d='M3 15V8h11v7M14 11h4l3 3v1h-7M7 8V5h4v3M9 5v3M6 18a2 2 0 1 0 0-4 2 2 0 0 0 0 4zm11 0a2 2 0 1 0 0-4 2 2 0 0 0 0 4z'/>",
    bed = "<path d='M3 18V8M3 14h18v4M7 14V9h5a4 4 0 0 1 4 4v1M3 18v2M21 18v2'/>",
    hospital_drug = "<path d='M5 20V5h14v15M9 5V3h6v2M12 8v5M9.5 10.5h5'/><path d='M8 17h8'/>",
    pathway = "<circle cx='5' cy='6' r='2'/><circle cx='19' cy='18' r='2'/><path d='M7 6h5a4 4 0 0 1 4 4v2a4 4 0 0 0 3 4M12 6l-2-2M12 6l-2 2'/>",
    population = "<circle cx='9' cy='8' r='3'/><circle cx='17' cy='9' r='2'/><path d='M3 20v-2a6 6 0 0 1 12 0v2M15 14a5 5 0 0 1 6 5v1'/>",
    diagnostics = "<path d='M9 3h6M10 3v6l-5 9a2 2 0 0 0 2 3h10a2 2 0 0 0 2-3l-5-9V3M8 15h8'/>",
    mortality = "<path d='M5 4h14v16H5zM8 8h8M8 12h8M8 16h5'/>",
    support = "<path d='M4 20V10h6v10M14 20V4h6v16M2 20h20M6 13h2M16 8h2M16 12h2'/>",
    "<circle cx='12' cy='12' r='8'/>"
  )
  paste0("<svg viewBox='0 0 24 24' aria-hidden='true' fill='none' ",
         "stroke='currentColor' stroke-width='1.7' stroke-linecap='round' ",
         "stroke-linejoin='round'>", paths, "</svg>")
}

render_status_segments <- function(status) {
  paste(vapply(status, function(z) {
    paste0("<span class='coverage-segment status-", gsub("_", "-", z),
           "' aria-label='", html_escape(map_status_label(z)), "'></span>")
  }, character(1)), collapse = "")
}

render_system_group <- function(group, components) {
  child <- components[match(group$components[[1]], components$component_id),
                      , drop = FALSE]
  status <- map_group_status(child$status)
  represented <- sum(child$status %in% c("detected_structured",
                                         "detected_partial"))
  unavailable <- sum(child$status %in% c("not_detected",
                                         "detected_misclassified",
                                         "detected_unstructured"))
  uncertain <- nrow(child) - represented - unavailable
  paste0(
    "<article class='system-node group-", html_escape(status), "'>",
    "<div class='node-icon'>", system_icon(group$icon[[1]]), "</div>",
    "<div class='node-copy'><h3>", html_escape(group$title[[1]]), "</h3>",
    "<p>", represented, " represented | ", unavailable, " unavailable | ",
    uncertain, " uncertain</p></div>",
    "<div class='coverage-strip' aria-label='Component status distribution'>",
    render_status_segments(child$status), "</div>",
    "</article>"
  )
}

render_system_area <- function(area, groups, components) {
  titles <- c(
    community = "Outpatient and community",
    hospital = "Hospital and inpatient",
    shared = "Shared system functions"
  )
  subtitles <- c(
    community = "Care delivered outside an admission",
    hospital = "Emergency, admission and hospital treatment",
    shared = "Population, diagnostics, outcomes and support"
  )
  selected <- groups[groups$area == area, , drop = FALSE]
  nodes <- vapply(seq_len(nrow(selected)), function(i) {
    render_system_group(selected[i, , drop = FALSE], components)
  }, character(1))
  paste0(
    "<section class='system-area area-", area, "'>",
    "<header><h2>", titles[[area]], "</h2><p>", subtitles[[area]], "</p></header>",
    "<div class='system-node-grid'>", paste(nodes, collapse = ""), "</div>",
    "</section>"
  )
}

#' Write an aggregate profile bundle
#'
#' @param profile Result from [profile_health_system()].
#' @param path Existing or new output directory.
#' @return Invisibly, the normalized output directory.
#' @export
write_profile_bundle <- function(profile, path) {
  if (!inherits(profile, "health_system_profile")) stop("profile has the wrong class")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  findings<-if(is.null(profile$findings))empty_findings() else profile$findings
  utils::write.csv(suppress_small_cells(findings,profile$config$privacy_min_cell),
    file.path(path,"quality_findings.csv"),row.names=FALSE,na="")
  write_crosswalk_audit(profile$evidence$source_crosswalk$data,path)
  components <- suppress_small_cells(profile$components,
                                     profile$config$privacy_min_cell)
  utils::write.csv(components, file.path(path, "health_system_components.csv"),
                   row.names = FALSE, na = "")
  errors <- profile$probe_errors
  if (is.null(errors) || !nrow(errors)) {
    errors <- data.frame(probe = character(), error = character())
  }
  utils::write.csv(errors, file.path(path, "probe_errors.csv"),
                   row.names = FALSE, na = "")
  evidence_dir <- file.path(path, "evidence")
  dir.create(evidence_dir, showWarnings = FALSE)
  for (nm in setdiff(names(profile$evidence), "config")) {
    item <- profile$evidence[[nm]]
    if (is.list(item) && identical(item$status, "ok") &&
        is.data.frame(item$data)) {
      utils::write.csv(suppress_small_cells(item$data, profile$config$privacy_min_cell), file.path(evidence_dir, paste0(nm, ".csv")),
                       row.names = FALSE, na = "")
    }
  }
  con <- file(file.path(path, "README.md"), open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(c(
    "# Health-system coverage profile",
    "",
    paste("Generated:", format(profile$generated_at, tz = "UTC")),
    paste("CDM schema:", profile$config$cdm_schema),
    "",
    "The component table describes which logical parts of the health system",
    "were detected. It is an aggregate profile and contains no patient-level data.",
    "",
    "Statuses distinguish detection from semantic reliability. A detected component",
    "can still be unsuitable for research when provenance, linkage or mapping is defective."
  ), con)
  status_class <- paste0("status-", gsub("_", "-", components$status))
  rows <- vapply(seq_len(nrow(components)), function(i) {
    paste0(
      "<tr><td><strong>", html_escape(components$description[[i]]),
      "</strong><br><small>", html_escape(components$expected_source_type[[i]]),
      "</small></td><td><span class='badge ", status_class[[i]], "'>",
      html_escape(components$status[[i]]), "</span></td><td>",
      html_escape(components$fitness_for_research[[i]]), "</td><td>",
      html_escape(components$reliability[[i]]), "</td><td>",
      html_escape(components$records[[i]]), "</td><td>",
      html_escape(components$start_date[[i]]), "</td><td>",
      html_escape(components$end_date[[i]]), "</td><td>",
      html_escape(components$evidence[[i]]), "</td><td>",
      html_escape(components$issues[[i]]), "</td></tr>"
    )
  }, character(1))
  error_notice <- if (nrow(errors)) {
    paste0("<p class='warning'><strong>Warning:</strong> ", nrow(errors),
           " probe(s) failed. See probe_errors.csv; affected components must not be interpreted as absent.</p>")
  } else {
    "<p><strong>All requested probes completed.</strong></p>"
  }
  groups <- system_map_groups()
  community_map <- render_system_area("community", groups, components)
  hospital_map <- render_system_area("hospital", groups, components)
  shared_map <- render_system_area("shared", groups, components)
  html <- c(
    "<!doctype html><html><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width,initial-scale=1'>",
    "<title>Health-system coverage profile</title>",
    "<style>:root{--community:#236a8d;--community-soft:#eaf5fa;--hospital:#6b4c9a;--hospital-soft:#f2eef8;--shared:#39796a;--shared-soft:#edf7f3;--structured:#50a773;--partial:#e0ad35;--problem:#db6f4b;--absent:#c84f55;--unknown:#9aa4ae}*{box-sizing:border-box}body{font-family:system-ui,sans-serif;margin:0;color:#17202a;background:#f6f8fb}main{max-width:1320px;margin:auto;padding:28px}h1{margin:0 0 4px}p{max-width:75rem}.warning{background:#fff0b3;border:1px solid #e1b900;padding:.7rem;border-radius:8px}.system-map{display:grid;grid-template-columns:1fr 1fr;gap:18px;margin:24px 0}.system-area{background:#fff;border-radius:18px;padding:18px;border:2px solid}.system-area>header{display:flex;align-items:baseline;justify-content:space-between;gap:12px;margin-bottom:14px}.system-area h2{font-size:20px;margin:0}.system-area header p{font-size:13px;margin:0;color:#5d6d7e}.area-community{border-color:var(--community);background:linear-gradient(180deg,var(--community-soft),#fff 28%)}.area-community h2,.area-community .node-icon{color:var(--community)}.area-hospital{border-color:var(--hospital);background:linear-gradient(180deg,var(--hospital-soft),#fff 28%)}.area-hospital h2,.area-hospital .node-icon{color:var(--hospital)}.area-shared{grid-column:1/-1;border-color:var(--shared);background:linear-gradient(180deg,var(--shared-soft),#fff 34%)}.area-shared h2,.area-shared .node-icon{color:var(--shared)}.system-node-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:10px}.area-shared .system-node-grid{grid-template-columns:repeat(4,minmax(0,1fr))}.system-node{display:grid;grid-template-columns:46px 1fr;grid-template-rows:auto 7px;gap:8px 12px;align-items:center;border:1px solid #dfe6ee;border-radius:12px;padding:12px;min-width:0}.group-detected-structured{background:#eff9f2}.group-detected-partial{background:#fff8e6}.group-detected-misclassified,.group-detected-unstructured{background:#fff0ea}.group-not-detected{background:#fff0f1}.group-unknown{background:#f1f3f5}.node-icon{width:42px;height:42px;grid-row:1}.node-icon svg{width:100%;height:100%}.node-copy{min-width:0}.node-copy h3{font-size:15px;margin:0 0 3px}.node-copy p{font-size:12px;color:#5d6d7e;margin:0}.coverage-strip{grid-column:1/-1;display:flex;gap:3px;height:7px}.coverage-segment{flex:1;border-radius:5px}.status-detected-structured{background:var(--structured)}.status-detected-partial{background:var(--partial)}.status-detected-unstructured,.status-detected-misclassified{background:var(--problem)}.status-not-detected{background:var(--absent)}.status-not-assessed,.status-unknown{background:var(--unknown)}.legend{display:flex;gap:14px;flex-wrap:wrap;font-size:12px;color:#5d6d7e;margin:12px 0 20px}.legend span{display:flex;align-items:center;gap:5px}.legend i{width:12px;height:12px;border-radius:3px}.panel{background:white;border:1px solid #dfe6ee;border-radius:12px;padding:1rem;overflow:auto}details{margin-top:20px}summary{font-weight:600}.table-wrap{overflow:auto;margin-top:12px}table{border-collapse:collapse;width:100%;font-size:13px}th,td{padding:.65rem;border-bottom:1px solid #e8edf2;text-align:left;vertical-align:top}th{position:sticky;top:0;background:#eef3f8}.badge{display:inline-block;padding:.2rem .45rem;border-radius:999px;background:#e5e7eb;white-space:nowrap}small{color:#5d6d7e}@media(max-width:850px){.system-map{grid-template-columns:1fr}.area-shared{grid-column:auto}.area-shared .system-node-grid{grid-template-columns:repeat(2,minmax(0,1fr))}}@media(max-width:520px){main{padding:16px}.system-node-grid,.area-shared .system-node-grid{grid-template-columns:1fr}.system-area>header{display:block}.system-area header p{margin-top:4px}}</style></head><body><main>",
    "<h1>Health-system coverage profile</h1>",
    paste0("<p>Schema: <strong>", html_escape(profile$config$cdm_schema),
           "</strong> | Generated: ", html_escape(format(profile$generated_at)),
           "</p>"),
    "<p>This map shows which logical parts of the health system survive in the data. Area outlines distinguish care setting; the small bars show the status of the underlying components.</p>",
    error_notice,
    "<div class='legend'><span><i class='status-detected-structured'></i>Structured</span><span><i class='status-detected-partial'></i>Partial</span><span><i class='status-detected-misclassified'></i>Misclassified</span><span><i class='status-not-detected'></i>Not detected</span><span><i class='status-unknown'></i>Unknown / not assessed</span></div>",
    "<div class='system-map'>", community_map, hospital_map, shared_map, "</div>",
    "<details><summary>Detailed component evidence</summary><div class='panel table-wrap'><table><thead><tr><th>Health-system component</th><th>Representation</th><th>Research fitness</th><th>Reliability</th><th>Records</th><th>From</th><th>To</th><th>Evidence</th><th>Known issues</th></tr></thead><tbody>",
    rows,
    "</tbody></table></div></details></main></body></html>"
  )
  writeLines(html, file.path(path, "profile.html"), useBytes = TRUE)
  # Preserve the original overview as a secondary view during this migration.
  file.copy(file.path(path,"profile.html"),file.path(path,"component_overview.html"),overwrite=TRUE)
  write_architectural_report(profile,path)
  if(!is.null(profile$config$component_rules))utils::write.csv(profile$config$component_rules,
    file.path(path,"configured_concept_rules.csv"),row.names=FALSE)
  utils::write.csv(profile$config$source_aliases,file.path(path,"configured_source_aliases.csv"),row.names=FALSE)
  vm<-attr(profile$config$source_aliases,"visit_concept_map")
  if(!is.null(vm))utils::write.csv(vm,file.path(path,"configured_visit_concepts.csv"),row.names=FALSE)
  invisible(normalizePath(path, mustWork = TRUE))
}
