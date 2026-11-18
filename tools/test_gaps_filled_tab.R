#!/usr/bin/env Rscript
# ============================================================================
# tools/test_gaps_filled_tab.R - server-side test for the "Gaps filled" tab
# ============================================================================
# Runs every output in the tab, for every pair x resolution x taxonomic filter x
# map layer, against the REAL closure tables, and then checks the numbers rather
# than only the absence of errors. Expected values are re-read from the tables,
# so the test stays valid after script 14 is re-run.
#
# The rendering libraries (leaflet, plotly, DT, sf) are stubbed.
# What needs testing here is data plumbing - column names, joins, factor levels,
# summary lookups - not whether plotly draws. Every stub FORCES its arguments
# and evaluates leaflet/plotly formulas against the data, so a wrong column
# inside a plot call fails here exactly as it would in the browser.
#
#   Rscript tools/test_gaps_filled_tab.R
#   APP=... CLOSURE_DIR=... CLOSURE_R=... CONFIG=... Rscript tools/test_gaps_filled_tab.R
#
# Requires scripts/14_gap_closure.R to have run at least once.
# ============================================================================
# The tab is behind a release switch in app.R (hidden by default); switch it on here.
Sys.setenv(GAP_FINDER_SHOW_GAPS_FILLED = "true")

suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(tidyr); library(tibble)
  library(data.table); library(cli); library(scales); library(stringr)
  library(glue); library(lubridate)
})

APP       <- Sys.getenv("APP", "")
CLOSURE   <- Sys.getenv("CLOSURE_DIR", "")
CLOSURE_R <- Sys.getenv("CLOSURE_R", "")
CONFIG    <- Sys.getenv("CONFIG", "")
TP_DIR    <- Sys.getenv("TIMEPOINTS_DIR", "")
if (!nzchar(APP) || !nzchar(CLOSURE) || !nzchar(CLOSURE_R) || !nzchar(CONFIG) || !nzchar(TP_DIR)) {
  source(here::here("scripts", "00_setup.R"))
  if (!nzchar(TP_DIR))    TP_DIR    <- here::here(p_data_proc, "timepoints")
  if (!nzchar(APP))       APP       <- here::here("shiny_app", "gap_finder", "app.R")
  if (!nzchar(CLOSURE))   CLOSURE   <- here::here(p_data_proc, "closure")
  if (!nzchar(CLOSURE_R)) CLOSURE_R <- here::here("R", "closure.R")
  if (!nzchar(CONFIG))    CONFIG    <- here::here("configs", paste0("config_", COUNTRY_CODE, ".yml"))
}
if (!dir.exists(CLOSURE) || !length(list.files(CLOSURE, "^closure_summary_"))) {
  stop("No closure tables in ", CLOSURE,
       " - run scripts/14_gap_closure.R first.", call. = FALSE)
}

pass <- 0L; fail <- 0L
ok <- function(cond, what) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("  ok    ", what, "\n") }
  else { fail <<- fail + 1L; cat("  FAIL  ", what, "\n") }
}
try_out <- function(expr, what) {
  r <- tryCatch({ force(expr); TRUE },
                error = function(e) { cat("         -> ", conditionMessage(e), "\n"); FALSE })
  ok(r, what)
  invisible(r)
}

# ---- stubs: force arguments, return something plausible --------------------
force_all <- function(...) invisible(lapply(list(...), function(x) tryCatch(force(x), error = function(e) stop(e))))
mk <- function(ret = invisible(NULL)) function(...) { force_all(...); ret }

leaflet <- function(data = NULL, ...) { force(data); structure(list(data = data), class = "lstub") }
add_basemap <- function(map, ...) map
# Formulas in leaflet are evaluated against map$data - do it, so a wrong
# column name fails here exactly as it would in the browser.
eval_formulas <- function(args, data) {
  for (a in args) if (inherits(a, "formula")) eval(a[[2]], data, environment(a))
}
addPolygons <- function(map, ...) { eval_formulas(list(...), map$data); map }
addLegend   <- function(map, ...) { eval_formulas(list(...), map$data); map }
setView <- function(map, ...) map
colorFactor <- function(palette, domain, na.color = NULL, ...) {
  force(palette); force(domain); function(x) rep(palette[1], length(x))
}
colorBin <- colorNumeric <- colorFactor
labelOptions <- mk(list())
# Run the expression (so the data pipeline really runs and can really fail),
# then return an inert string so nothing hits shiny's HTML serialiser. The
# expression is handed over already quoted: exprToFunction() would otherwise
# quote the quoting call and return the code without running it.
render_fun <- function(expr, env, quoted) {
  shiny::exprToFunction(expr, env, quoted = TRUE)
}
render_stub <- function(expr, env = parent.frame(), quoted = FALSE, ...) {
  f <- render_fun(if (quoted) expr else substitute(expr), env)
  shiny::renderText({ invisible(f()); "ok" })
}
# The same, returning the plot's or table's data as JSON, so a test reads the
# output itself.
render_data <- function(expr, env = parent.frame(), quoted = FALSE, ...) {
  f <- render_fun(if (quoted) expr else substitute(expr), env)
  shiny::renderText({
    x <- f()
    as.character(jsonlite::toJSON(x$data, dataframe = "rows", na = "null", factor = "string",
                                  digits = NA))
  })
}
as_data <- function(json) tibble::as_tibble(jsonlite::fromJSON(json))
leafletOutput <- mk(NULL); renderLeaflet <- render_stub
leafletProxy <- mk(structure(list(data = NULL), class = "lstub"))
clearGroup <- function(map, ...) map

plot_ly <- function(data = NULL, ...) {
  force(data)
  eval_formulas(list(...), data)
  structure(list(data = data), class = "pstub")
}
add_bars <- function(p, ...) {
  args <- list(...)
  eval_formulas(args, if (!is.null(args$data)) args$data else p$data)
  p
}
add_trace <- add_markers <- add_lines <- add_bars
layout <- function(p, ...) { force_all(...); p }
plotlyOutput <- mk(NULL); renderPlotly <- render_data
plotly_layout <- function(p, ..., dl_title = NULL) { force_all(...); p }

datatable <- function(data, ...) {
  force(data); force_all(...)
  structure(list(data = data), class = "dtstub")
}
DTOutput <- mk(NULL); renderDT <- render_data
formatStyle <- formatRound <- formatPercentage <- function(x, ...) { force_all(...); x }

st_drop_geometry <- function(x, ...) x
st_read <- st_simplify <- st_transform <- st_centroid <- st_join <- st_crs <- mk(NULL)
comma <- scales::comma

# ---- a bundle: real closure tables + the minimum the app needs -------------
source(CLOSURE_R)
cfg <- yaml::read_yaml(CONFIG)
types_file <- file.path(dirname(dirname(CONFIG)),
                        cfg$parameters$closure$source_types_file %||% "configs/dataset_types_SE.csv")
NOTES <- cfg$parameters$closure$timepoint_notes
cb <- closure_bundle(CLOSURE, keep_top = 1000L,
                     datasets_meta = closure_read_dataset_meta(types_file))
stopifnot(length(cb$pairs) > 0)

# The checklist releases of the compared dates, and a later one for the rest of
# the bundle, so the footnote has to say both.
MAIN_RELEASE <- "2026-10-31T00:00:00.000+00:00"
RELEASES <- closure_reference_releases(
  c(cb$tables$pair_index$from_date, cb$tables$pair_index$to_date), TP_DIR,
  live_release = MAIN_RELEASE)

cells10 <- unique(cb$tables$cells$eeacellcode[cb$tables$cells$resolution == "10km"])
cells50 <- unique(cb$tables$cells$eeacellcode[cb$tables$cells$resolution == "50km"])

bundle <- list(
  grid_10km = tibble(eeacellcode = cells10),
  grid_50km = tibble(eeacellcode = cells50),
  metadata = c(list(
    snapshot_date = as.Date("2026-07-29"), recent_label = "2025",
    country_name = "Sweden", country_adjective = "Swedish",
    threatened_codes = c("CR", "EN", "VU", "NT"),
    has_all_scope = TRUE, has_threatened_scope = FALSE,
    has_invasive_scope = FALSE, has_sensitive_scope = FALSE,
    data_sources = list(checklists = list(taxonomy = list(
      published = MAIN_RELEASE)))),
    closure_bundle_meta(cb, keep_top = 1000L, notes = NOTES, reference_releases = RELEASES))
)
for (nm in names(cb$tables)) bundle[[paste0("closure_", nm)]] <- as_tibble(cb$tables[[nm]])

# ---- pull the app's closure code out of app.R ------------------------------
src <- readLines(APP, encoding = "UTF-8")
d0 <- grep("^# GAP CLOSURE \\(from 14 via 11\\)", src)
d1 <- grep("^has_kingdom_recency   <- ", src)
stopifnot(length(d0) == 1, length(d1) == 1)
DATA_BLOCK <- parse(text = paste(src[(d0 + 1):(d1 - 2)], collapse = "\n"))

s0 <- grep("^  # GAPS FILLED$", src)
s1 <- grep("^shinyApp\\(ui, server\\)$", src)
stopifnot(length(s0) == 1, length(s1) == 1)
SERVER_BLOCK <- parse(text = paste(src[(s0 + 1):(s1 - 3)], collapse = "\n"))
cat("extracted", d1 - d0, "lines of data setup and", s1 - s0, "lines of server logic\n\n")

# helpers the app defines elsewhere and the blocks rely on
app_data <- bundle
safe_get <- function(name) if (name %in% names(app_data)) app_data[[name]] else NULL
metadata <- bundle$metadata
grid_10km <- bundle$grid_10km
`%||%` <- function(a, b) if (is.null(a)) b else a
pal <- list(sage = "#2A7F62", slate = "#4477AA", sand = "#CCBB44",
            coral = "#EE6677", plum = "#AA3377", text = "#2d2d2d", muted = "#6b6b6b")
PAL_NODATA <- "#dddddd"
country_name <- "Sweden"; country_adjective <- "Swedish"
PAL_DIV    <- c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6")
taxon_ref <- function(x) rep("", length(x))
dl_csv <- function(data_fun, prefix) {
  force(data_fun)
  shiny::renderText({ d <- data_fun(); sprintf("%d rows", NROW(d)) })
}
map_dl_btn <- mk(NULL)
eval(DATA_BLOCK)

cat("has_closure:", has_closure, "| pairs:", length(closure_pair_choices), "\n")
ok(isTRUE(has_closure), "has_closure is TRUE with real tables present")
ok(length(closure_pair_choices) == length(cb$pairs), "one selector entry per pair")

PI <- closure_pair_index
latest <- PI[PI$regime_boundary == 0, ]
latest <- latest[order(latest$to_date, latest$from_date, decreasing = TRUE), ][1, ]
ok(identical(closure_default_pair, latest$pair_id),
   sprintf("the tab opens on the latest like-built step (%s)", closure_default_pair))

cat("\n=== the class / order choices ===\n")
ok(identical(unname(closure_class_choices[1]), CLOSURE_ALL), "classes start with All classes")
ok(!any(closure_class_choices == ""), "no class choice has an empty value (selectize reads it as none)")
oc_all <- closure_order_choices(CLOSURE_ALL)
ok(length(oc_all) == nrow(closure_groups) + 1L, "under All classes every class x order is an order choice")
ins <- closure_order_choices("Insecta")
ok(all(startsWith(unname(ins[-1]), "Insecta|")), "the orders of Insecta are Insecta's only")
ok("Diptera" %in% names(ins), "Diptera is among them, labelled without its class")
if (CLOSURE_NONE %in% closure_class_choices) {
  nc <- closure_order_choices(CLOSURE_NONE)
  ok(all(startsWith(unname(nc[-1]), paste0(CLOSURE_NONE, "|"))),
     "orders with no class in Dyntaxa have their own bucket")
  ok(any(names(nc) == "Squamata"), "Squamata sits in that bucket by name")
}
ok(!any(grepl("\\|$", unname(oc_all))), "a blank order is encoded, never left empty")

# ---- run every output ------------------------------------------------------
app <- shinyApp(ui = fluidPage(), server = function(input, output, session) {
  eval(SERVER_BLOCK)
})

OUTPUTS <- c("closure_banner", "cl_res_note", "cl_filter_note",
             "cl_tile_gained", "cl_tile_lost", "cl_tile_species", "cl_tile_cells",
             "cl_bar_caption", "cl_tile_bar", "cl_tile_rose", "cl_tile_fell", "cl_tile_under",
             "cl_tile_footnote", "cl_map_note", "cl_map", "cl_map_dl",
             "cl_periods_note", "cl_periods", "cl_group_note", "cl_group_table",
             "cl_threat_note", "cl_threat_table", "cl_species_note", "cl_species_table",
             "cl_dataset_note", "cl_dataset_table", "cl_dataset_lost_note", "cl_dataset_lost_table")

ORDER_DIPTERA <- "Insecta|Diptera"
FILTERS <- list(none = c(CLOSURE_ALL, CLOSURE_ALL), class = c("Insecta", CLOSURE_ALL),
                order = c("Insecta", ORDER_DIPTERA))
if (CLOSURE_NONE %in% closure_class_choices) {
  FILTERS$no_class <- c(CLOSURE_NONE, paste0(CLOSURE_NONE, "|Squamata"))
}

for (pid in cb$pairs) {
  for (res in c("10km", "50km")) {
    for (fn in names(FILTERS)) {
      for (layer in c("species", "bar")) {
        cat(sprintf("\n-- %s | %s | %s | %s\n", pid, res, fn, layer))
        testServer(app, {
          session$setInputs(cl_pair = pid, cl_res = res, cl_class = FILTERS[[fn]][1],
                            cl_order = FILTERS[[fn]][2], cl_map_layer = layer)
          outs <- if (layer == "bar") c("cl_map_note", "cl_map") else OUTPUTS
          for (o in outs) try_out(output[[o]], o)
        })
      }
    }
  }
}

# ---- values, not just absence of errors ------------------------------------
# renderUI in testServer returns list(html=, deps=); take the html.
htxt <- function(x) {
  if (is.list(x) && !is.null(x$html)) x <- x$html
  gsub("\\s+", " ", gsub("<[^>]*>", "", paste(as.character(x), collapse = " ")))
}
num <- function(pid, res, m) {
  s <- cb$tables$summary
  s$value[s$pair_id == pid & s$resolution == res & s$key_space == "dyntaxa" &
            s$source_group == "total" & s$metric == m][1]
}
# Runs `code` with the tab's outputs, where the caller's variables are visible.
run <- function(pid, res, cls = CLOSURE_ALL, ord = CLOSURE_ALL, code, env = parent.frame()) {
  testServer(app, {
    session$setInputs(cl_pair = pid, cl_res = res, cl_class = cls, cl_order = ord,
                      cl_map_layer = "species")
    e <- new.env(parent = env)
    e$output <- output; e$session <- session
    eval(code, e)
  })
}

cat("\n=== the tiles show the summary's numbers ===\n")
for (pid in cb$pairs) for (res in c("10km", "50km")) {
  want <- c(cl_tile_gained = num(pid, res, "pairs_gained"), cl_tile_lost = num(pid, res, "pairs_lost"),
            cl_tile_species = num(pid, res, "species_newly_recorded"),
            cl_tile_cells = num(pid, res, "cells_filled"))
  run(pid, res, code = quote({
    for (nm in names(want)) {
      ok(grepl(comma(want[[nm]]), htxt(output[[nm]]), fixed = TRUE),
         sprintf("%s | %s | %-15s shows %s", substr(pid, 1, 21), res, nm, comma(want[[nm]])))
    }
  }))
}

cat("\n=== the low-data bar tiles count the cells ===\n")
for (pid in cb$pairs) for (res in c("10km", "50km")) {
  cl <- cb$tables$cells[pair_id == pid & resolution == res & key_space == "dyntaxa"]
  want <- c(cl_tile_rose = sum(cl$bar_status == "rose_above"),
            cl_tile_fell = sum(cl$bar_status == "fell_below"),
            cl_tile_under = sum(cl$bar_status == "stayed_under"))
  run(pid, res, code = quote({
    for (nm in names(want)) {
      ok(startsWith(trimws(htxt(output[[nm]])), comma(want[[nm]])),
         sprintf("%s | %s | %-13s = %s", substr(pid, 1, 21), res, nm, comma(want[[nm]])))
    }
    ok(grepl(sprintf("another %s cells have no records at either date",
                     comma(sum(cl$bar_status == "still_empty"))),
             htxt(output$cl_tile_under), fixed = TRUE), "below the bar names the cells with no records")
    bar <- format(floor(cl$q_frozen[1]), big.mark = ",")
    ok(startsWith(trimws(htxt(output$cl_tile_bar)), bar),
       sprintf("the bar tile shows the bar, rounded down (%s records)", bar))
    ok(grepl(sprintf("fixed bar of %s records", bar), htxt(output$cl_bar_caption), fixed = TRUE),
       "the caption of the second row names the bar")
  }))
}

cat("\n=== under a filter the tiles count the group ===\n")
P <- closure_default_pair
check_group <- function(cls, ord, lab, keep) {
  g  <- cb$tables$group[pair_id == P & resolution == "10km" & key_space == "dyntaxa"][keep(class, order)]
  st <- cb$tables$species_totals[pair_id == P & resolution == "10km" & key_space == "dyntaxa"][
    keep(class, order) & status == "new"]
  cg <- cb$tables$cells_group[pair_id == P & resolution == "10km"][keep(as.character(class), as.character(order))]
  cc <- cg[, .(f = sum(n_species_from), t = sum(n_species_to)), by = eeacellcode]
  want <- c(cl_tile_gained = sum(g$pairs_gained), cl_tile_lost = sum(g$pairs_lost),
            cl_tile_species = sum(st$n_species), cl_tile_cells = sum(cc$f == 0 & cc$t > 0))
  run(P, "10km", cls, ord, quote({
    for (nm in names(want)) {
      ok(grepl(comma(want[[nm]]), htxt(output[[nm]]), fixed = TRUE),
         sprintf("%-10s %-15s shows %s", lab, nm, comma(want[[nm]])))
    }
    ok(grepl(lab, htxt(output$cl_filter_note), fixed = TRUE), sprintf("the filter note names %s", lab))
    ok(grepl("count all species", htxt(output$cl_filter_note), fixed = TRUE),
       "the filter note says what does not follow it")
  }))
}
check_group("Insecta", CLOSURE_ALL, "Insecta", function(c, o) c == "Insecta")
check_group("Insecta", ORDER_DIPTERA, "Insecta / Diptera", function(c, o) c == "Insecta" & o == "Diptera")
if (CLOSURE_NONE %in% closure_class_choices) {
  check_group(CLOSURE_NONE, CLOSURE_ALL, "No class in Dyntaxa", function(c, o) c == "")
}
run(P, "10km", code = quote({
  ok(is.null(output$cl_filter_note) || !nzchar(htxt(output$cl_filter_note)),
     "no filter note without a filter")
}))

cat("\n=== the banner follows the pair ===\n")
for (pid in cb$pairs) {
  row <- PI[PI$pair_id == pid, ]
  run(pid, "10km", code = quote({
    b <- htxt(output$closure_banner)
    if (row$key_backbone_differs == 1) {
      ok(grepl("Two naming systems", b, fixed = TRUE), sprintf("%s: two naming systems", pid))
      rate <- 100 * num(pid, "10km", "pairs_lost") / num(pid, "10km", "pairs_from")
      ok(grepl(sprintf("%.1f %% are gone", rate), b, fixed = TRUE),
         sprintf("%s: names the share gone (%.1f %%)", pid, rate))
      routed <- num(pid, "10km", "col_route_keys")
      if (!is.na(routed) && routed > 0) {
        ok(grepl(sprintf("%s names", comma(routed)), b, fixed = TRUE),
           sprintf("%s: names how many names were translated (%s)", pid, comma(routed)))
        ok(grepl(sprintf("(%s)", comma(num(pid, "10km", "hybrids_left_out"))), b, fixed = TRUE),
           sprintf("%s: names how many hybrids were left out", pid))
      }
    } else {
      ok(grepl("Same method at both dates", b, fixed = TRUE), sprintf("%s: same method", pid))
    }
    has_note <- any(vapply(NOTES, function(n) n$timepoint %in% c(row$from_date, row$to_date), TRUE))
    ok(grepl("lacks", b, fixed = TRUE) == has_note,
       sprintf("%s: the delivery line %s", pid, if (has_note) "is there" else "is not there"))
    rel <- unique(stats::na.omit(unname(RELEASES[c(row$from_date, row$to_date)])))
    fn <- htxt(output$cl_tile_footnote)
    if (length(rel) == 1) {
      ok(grepl(sprintf("as published on %s, at both dates", closure_date(rel)), fn, fixed = TRUE),
         sprintf("%s: the footnote names the Dyntaxa release of both dates (%s)", pid, closure_date(rel)))
      ok(grepl("the other tabs use the version of 31 October 2026", fn, fixed = TRUE) ==
           (rel != "2026-10-31"),
         sprintf("%s: the footnote says when the other tabs use another release", pid))
    } else {
      ok(!grepl("as published on", fn, fixed = TRUE),
         sprintf("%s: no release named when the dates' release is unknown", pid))
    }
  }))
}

cat("\n=== the losses by dataset carry the delivery note ===\n")
for (pid in cb$pairs) {
  row <- PI[PI$pair_id == pid, ]
  nts <- Filter(function(n) n$timepoint %in% c(row$from_date, row$to_date), NOTES)
  dl <- cb$tables$datasets_lost[pair_id == pid & resolution == "10km" & key_space == "dyntaxa"]
  run(pid, "10km", code = quote({
    note <- htxt(output$cl_dataset_lost_note)
    tb <- as_data(output$cl_dataset_lost_table)
    if (length(nts)) {
      keys <- unique(unlist(lapply(nts, `[[`, "datasets")))
      alone <- sum(dl$vanish_if_dropped[dl$datasetkey %in% keys])
      ok(grepl(comma(alone), note, fixed = TRUE),
         sprintf("%s: counts the %s lost pairs that rested on the noted datasets alone", pid, comma(alone)))
      ok(grepl(substr(nts[[1]]$text, 1, 40), note, fixed = TRUE), sprintf("%s: shows the note's text", pid))
      marked <- sum(nzchar(tb$Note))
      ok(marked == sum(dl$datasetkey %in% keys),
         sprintf("%s: the table marks the %d noted datasets it lists", pid, marked))
    } else {
      ok(!grepl("Data of ", note, fixed = TRUE), sprintf("%s: no delivery note", pid))
      ok(!"Note" %in% names(tb), sprintf("%s: no Note column", pid))
    }
    ok(nrow(tb) == nrow(dl), sprintf("%s: one row per dataset the losses rested on", pid))
    ok(all(grepl("^<a href=\"https://www.gbif.org/dataset/", tb$Dataset)),
       sprintf("%s: every dataset links to its GBIF page", pid))
    if ("38b4c89f-584c-41bb-bd8f-cd1def33e92f" %in% dl$datasetkey) {
      ok(any(grepl(">Artportalen</a>", tb$Dataset, fixed = TRUE)),
         sprintf("%s: datasets are shown by title (Artportalen)", pid))
    }
  }))
}

cat("\n=== periods: shares per type add up, labels carry years ===\n")
run(P, "10km", code = quote({
  d <- as_data(output$cl_periods)
  sh <- tapply(d$share, d$bar, sum)
  ok(all(abs(sh - 100) < 1e-6), "each type's periods add up to 100 %")
  yr <- num(P, "10km", "mechanism_boundary_year")
  ok(paste0("Collected ", yr, " or later") %in% d$period_label,
     sprintf("the newest period reads 'Collected %s or later'", yr))
  ok(paste0("Collected ", yr - 10, "–", yr - 1) %in% d$period_label,
     "the decade before is labelled by its years")
  tot <- sum(d$n_pairs[d$source_group == "total"])
  ok(tot == num(P, "10km", "pairs_gained"), "all types together hold every gained pair")
  ok(any(grepl(comma(tot), d$bar, fixed = TRUE)), "the bar label carries the pair count")
}))
run(P, "10km", "Insecta", ORDER_DIPTERA, quote({
  d <- as_data(output$cl_periods)
  g <- cb$tables$group[pair_id == P & resolution == "10km" & key_space == "dyntaxa" &
                         class == "Insecta" & order == "Diptera"]
  ok(sum(d$n_pairs[d$source_group == "total"]) == g$pairs_gained,
     "under the Diptera filter the periods hold Diptera's gained pairs")
}))

cat("\n=== threatened species ===\n")
run(P, "10km", code = quote({
  st <- cb$tables$species_totals[pair_id == P & resolution == "10km" & key_space == "dyntaxa" &
                                   threat %in% c("CR", "EN", "VU", "NT")]
  t <- htxt(output$cl_threat_note)
  for (s in c("new", "expanded", "shifted")) {
    n <- sum(st[status == s, n_species])
    ok(grepl(comma(n), t, fixed = TRUE), sprintf("the note counts %s threatened species %s", comma(n), s))
  }
  tb <- as_data(output$cl_threat_table)
  ok(all(tb$`Red list` %in% c("CR", "EN", "VU", "NT")), "the table lists CR, EN, VU and NT only")
  ok(all(tb$`New cells` > 0), "every listed species has new cells")
  ok(identical(as.character(tb$`What changed`[1]), "New to the data"), "species new to the data come first")
  ok(sum(tb$`What changed` == "New to the data") == sum(st[status == "new", n_species]),
     "every red-listed species new to the data is listed")
}))

cat("\n=== the class x order table ===\n")
run(P, "10km", code = quote({
  tb <- as_data(output$cl_group_table)
  g <- cb$tables$group[pair_id == P & resolution == "10km" & key_space == "dyntaxa"]
  ok(nrow(tb) == nrow(g), "one row per class x order")
  ok(sum(tb$New) == num(P, "10km", "pairs_gained"), "the rows add up to the new combinations")
  ok(!any(tb$Class == "" | tb$Order == ""), "blank ranks are labelled")
  ok(!"churn_flag" %in% names(tb) && !any(grepl("volatil", names(tb), ignore.case = TRUE)),
     "no volatility flag")
}))

cat("\n=== a pair across regimes gets the method banner and caveats ===\n")
X <- PI$pair_id[1]
closure_pair_index$regime_boundary[closure_pair_index$pair_id == X] <- 1
run(X, "10km", code = quote({
  b <- htxt(output$closure_banner)
  ok(grepl("Two different kinds of GBIF data", b, fixed = TRUE), "a cross-regime pair gets the method banner")
  rate <- 100 * num(X, "10km", "pairs_lost") / num(X, "10km", "pairs_from")
  ok(grepl(sprintf("%.1f %% of those on %s", rate, closure_date(PI$from_date[1])), b, fixed = TRUE),
     "the banner quotes the share gone, with the date")
  ok(grepl("50 km is the safer choice", htxt(output$cl_res_note), fixed = TRUE),
     "10 km carries the precision note")
  for (nm in c("cl_tile_gained", "cl_tile_lost", "cl_tile_cells")) {
    h <- output[[nm]]; if (is.list(h) && !is.null(h$html)) h <- h$html
    ok(grepl("Caveat:", paste(as.character(h), collapse = " "), fixed = TRUE),
       sprintf("%s carries a caveat", nm))
  }
}))
closure_pair_index$regime_boundary[closure_pair_index$pair_id == X] <- 0
run(X, "10km", code = quote({
  h <- output$cl_tile_gained; if (is.list(h) && !is.null(h$html)) h <- h$html
  ok(!grepl("Caveat:", paste(as.character(h), collapse = " "), fixed = TRUE),
     "a like-built pair carries no caveat on pairs gained")
}))

cat(sprintf("\n%d/%d checks passed\n", pass, pass + fail))
if (fail) quit(status = 1)
