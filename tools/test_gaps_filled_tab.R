#!/usr/bin/env Rscript
# ============================================================================
# tools/test_gaps_filled_tab.R - server-side test for the "Gaps filled" tab
# ============================================================================
# Runs every output in the tab, for every pair x resolution x source group,
# against the REAL closure tables, and then checks the numbers rather than only
# the absence of errors.
#
# The rendering libraries (leaflet, plotly, DT, sf, shinyWidgets) are stubbed.
# What needs testing here is data plumbing - column names, joins, factor levels,
# summary lookups - not whether plotly draws. Every stub FORCES its arguments
# and evaluates leaflet/plotly formulas against the data, so a wrong column
# inside a plot call fails here exactly as it would in the browser.
#
#   Rscript tools/test_gaps_filled_tab.R
#   APP=... CLOSURE_DIR=... CLOSURE_R=... Rscript tools/test_gaps_filled_tab.R
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
if (!nzchar(APP) || !nzchar(CLOSURE) || !nzchar(CLOSURE_R)) {
  source(here::here("scripts", "00_setup.R"))
  if (!nzchar(APP))       APP       <- here::here("shiny_app", "gap_finder", "app.R")
  if (!nzchar(CLOSURE))   CLOSURE   <- here::here(p_data_proc, "closure")
  if (!nzchar(CLOSURE_R)) CLOSURE_R <- here::here("R", "closure.R")
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
addPolygons <- function(map, ...) {
  # Formulas in leaflet are evaluated against map$data - do it, so a wrong
  # column name fails here exactly as it would in the browser.
  args <- list(...)
  for (a in args) if (inherits(a, "formula")) eval(a[[2]], map$data, environment(a))
  map
}
addLegend <- function(map, ...) {
  args <- list(...)
  for (a in args) if (inherits(a, "formula")) eval(a[[2]], map$data, environment(a))
  map
}
setView <- function(map, ...) map
colorFactor <- function(palette, domain, na.color = NULL, ...) {
  force(palette); force(domain); function(x) rep(palette[1], length(x))
}
colorBin <- colorNumeric <- colorFactor
labelOptions <- mk(list())
# Force the expression (so the data pipeline really runs and can really fail),
# then return an inert string so nothing hits shiny's HTML serialiser.
render_stub <- function(expr, env = parent.frame(), quoted = FALSE, ...) {
  f <- shiny::exprToFunction(substitute(expr), env, quoted)
  shiny::renderText({ invisible(f()); "ok" })
}
leafletOutput <- mk(NULL); renderLeaflet <- render_stub
leafletProxy <- mk(structure(list(data = NULL), class = "lstub"))
clearGroup <- function(map, ...) map

plot_ly <- function(data = NULL, ...) {
  force(data)
  args <- list(...)
  for (a in args) if (inherits(a, "formula")) eval(a[[2]], data, environment(a))
  structure(list(data = data), class = "pstub")
}
add_bars <- function(p, ...) {
  args <- list(...)
  for (a in args) if (inherits(a, "formula")) eval(a[[2]], p$data, environment(a))
  p
}
add_trace <- add_markers <- add_lines <- add_bars
layout <- function(p, ...) { force_all(...); p }
plotlyOutput <- mk(NULL); renderPlotly <- render_stub
plotly_layout <- function(p, ..., dl_title = NULL) { force_all(...); p }

datatable <- function(data, ...) { force(data); force_all(...); structure(list(data = data), class = "dtstub") }
DTOutput <- mk(NULL); renderDT <- render_stub
formatStyle <- function(x, ...) x

radioGroupButtons <- function(inputId, label = NULL, choices = NULL, ...) {
  force(choices); shiny::radioButtons(inputId, label, choices)
}
pickerInput <- function(inputId, label = NULL, choices = NULL, ...) {
  force(choices); shiny::selectInput(inputId, label, choices)
}
st_drop_geometry <- function(x, ...) x
st_read <- st_simplify <- st_transform <- st_centroid <- st_join <- st_crs <- mk(NULL)
comma <- scales::comma

# ---- a bundle: real closure tables + the minimum the app needs -------------
source(CLOSURE_R)
cb <- closure_bundle(CLOSURE, keep_top = 1000L)
stopifnot(length(cb$pairs) > 0)

cells10 <- unique(cb$tables$cells$eeacellcode[cb$tables$cells$resolution == "10km"])
cells50 <- unique(cb$tables$cells$eeacellcode[cb$tables$cells$resolution == "50km"])

bundle <- list(
  grid_10km = tibble(eeacellcode = cells10),
  grid_50km = tibble(eeacellcode = cells50),
  metadata = list(
    has_closure = TRUE, closure_pairs = cb$pairs, closure_species_top_n = 1000L,
    snapshot_date = as.Date("2026-07-29"), recent_label = "2025",
    country_name = "Sweden", country_adjective = "Swedish",
    has_all_scope = TRUE, has_threatened_scope = FALSE,
    has_invasive_scope = FALSE, has_sensitive_scope = FALSE)
)
for (nm in names(cb$tables)) bundle[[paste0("closure_", nm)]] <- as_tibble(cb$tables[[nm]])

# ---- pull the app's closure code out of app.R ------------------------------
src <- readLines(APP)
d0 <- grep("^# GAP CLOSURE \\(from 14 via 11\\)", src)
d1 <- grep("^has_kingdom_recency   <- ", src)
stopifnot(length(d0) == 1, length(d1) == 1)
DATA_BLOCK <- parse(text = paste(src[(d0 + 1):(d1 - 2)], collapse = "\n"))

s0 <- grep("^  # GAPS FILLED$", src)
s1 <- grep("^shinyApp\\(ui, server\\)$", src)
stopifnot(length(s0) == 1, length(s1) == 1)
SERVER_BLOCK <- parse(text = paste(src[(s0 + 1):(s1 - 3)], collapse = "\n"))
cat("extracted", d1 - d0, "lines of data setup and", s1 - s0, "lines of server logic\n\n")

# helpers the app defines earlier and the block relies on
app_data <- bundle
safe_get <- function(name) if (name %in% names(app_data)) app_data[[name]] else NULL
metadata <- bundle$metadata
grid_10km <- bundle$grid_10km
`%||%` <- function(a, b) if (is.null(a)) b else a
pal <- list(sage = "#2A7F62", slate = "#4477AA", sand = "#CCBB44",
            coral = "#EE6677", plum = "#AA3377", text = "#2d2d2d", muted = "#6b6b6b")
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
ok(any(grepl("⚠", names(closure_pair_choices))),
   "the cross-regime pair is marked in the selector itself")

# ---- run every output, for every pair x resolution x source ----------------
app <- shinyApp(ui = fluidPage(), server = function(input, output, session) {
  eval(SERVER_BLOCK)
})

OUTPUTS <- c("closure_banner", "cl_res_note", "cl_tile_gained", "cl_tile_lost",
             "cl_tile_species", "cl_tile_cells", "cl_tile_fieldwork",
             "cl_tile_footnote", "cl_map", "cl_mech_note", "cl_mechanism",
             "cl_group", "cl_still_open", "cl_species_note", "cl_species_table",
             "cl_dataset_table", "cl_map_dl")

for (pid in cb$pairs) {
  for (res in c("10km", "50km")) {
    for (src_grp in c("total", "observation_platforms", "collections")) {
      cat(sprintf("\n-- %s | %s | %s\n", pid, res, src_grp))
      testServer(app, {
        session$setInputs(cl_pair = pid, cl_res = res, cl_source = src_grp)
        for (o in OUTPUTS) {
          try_out(output[[o]], sprintf("%s", o))
        }
      })
    }
  }
}

# ---- values, not just absence of errors ------------------------------------
cat("\n=== do the tiles show the right numbers? ===\n")
# renderUI in testServer returns list(html=, deps=); take the html.
htxt <- function(x) {
  if (is.list(x) && !is.null(x$html)) x <- x$html
  gsub("\\s+", " ", gsub("<[^>]*>", "", paste(as.character(x), collapse = " ")))
}

check_vals <- function(pid, res, expect) {
  testServer(app, {
    session$setInputs(cl_pair = pid, cl_res = res, cl_source = "total")
    for (nm in names(expect)) {
      got <- htxt(output[[nm]])
      ok(grepl(expect[[nm]], got, fixed = TRUE),
         sprintf("%s | %s | %-17s contains '%s'", substr(pid, 12, 21), res, nm, expect[[nm]]))
    }
  })
}

W <- "2021-01-01__2024-01-01"; X <- "2024-01-01__2026-07-29"
if (W %in% cb$pairs) {
  # These are the published 2021->2024 figures; they change only if the
  # underlying snapshots change, in which case this test SHOULD fail.
  check_vals(W, "10km", list(cl_tile_gained = "856,612", cl_tile_lost = "56,224",
                             cl_tile_species = "4,345", cl_tile_cells = "332"))
  check_vals(W, "50km", list(cl_tile_gained = "164,350", cl_tile_lost = "9,407"))
}
if (X %in% cb$pairs) {
  check_vals(X, "10km", list(cl_tile_gained = "1,492,214", cl_tile_lost = "318,788"))
  check_vals(X, "50km", list(cl_tile_gained = "372,740", cl_tile_lost = "86,011"))
}

cat("\n=== does the banner change with the pair, and carry real numbers? ===\n")
testServer(app, {
  session$setInputs(cl_pair = W, cl_res = "10km", cl_source = "total")
  b <- htxt(output$closure_banner)
  ok(grepl("Like for like", b, fixed = TRUE), "within-regime pair gets the like-for-like banner")
  ok(!grepl("pipeline", b, fixed = TRUE), "within-regime banner carries no method warning")
})
testServer(app, {
  session$setInputs(cl_pair = X, cl_res = "10km", cl_source = "total")
  b <- htxt(output$closure_banner)
  ok(grepl("built differently", b, fixed = TRUE), "cross-regime pair gets the method-change banner")
  ok(grepl("318,788 pairs, 5.4%", b, fixed = TRUE),
     "banner quotes the measured loss rate, not a generic caution")
  ok(grepl("birds", b, fixed = TRUE), "banner names the diagnostic (birds)")
  r <- htxt(output$cl_res_note)
  ok(grepl("50 km is the safer read", r, fixed = TRUE), "10 km carries the precision note")
})
testServer(app, {
  session$setInputs(cl_pair = X, cl_res = "50km", cl_source = "total")
  ok(grepl("not in question", htxt(output$cl_res_note), fixed = TRUE),
     "50 km says cell assignment is not in question")
})

cat("\n=== fieldwork share differs by stream, which is the whole point ===\n")
shares <- c()
for (sg in c("total", "observation_platforms", "collections")) {
  testServer(app, {
    session$setInputs(cl_pair = W, cl_res = "10km", cl_source = sg)
    shares[[sg]] <<- htxt(output$cl_tile_fieldwork)
  })
}
cat("   ", paste(names(shares), shares, sep = ": ", collapse = "  |  "), "\n")
ok(length(unique(shares)) == 3, "each stream reports its own fieldwork share")
ok(grepl("^64%|^63%", trimws(shares[["total"]])), "both streams ~64% (matches the finding)")
ok(grepl("^73%|^72%", trimws(shares[["observation_platforms"]])), "platforms ~73%")
ok(grepl("^10%", trimws(shares[["collections"]])), "collections ~10%")

cat("\n=== blank ranks are labelled, not dropped ===\n")
g <- bundle$closure_group |> filter(pair_id == W, resolution == "10km", key_space == "dyntaxa")
blank <- g |> filter(is.na(class) | trimws(class) == "")
plotted <- g |>
  mutate(across(c(class, order), ~ ifelse(is.na(.x) | trimws(.x) == "", "Unclassified", .x))) |>
  filter(pairs_from >= 5000) |> arrange(desc(pairs_gained)) |> head(25)
cat(sprintf("    groups with a blank class in the table: %d; rows plotted: %d\n",
            nrow(blank), nrow(plotted)))
ok(!any(is.na(plotted$class) | trimws(plotted$class) == ""),
   "no plotted row has an empty class label")
ok(nrow(plotted) == 25, "the panel fills its 25 slots")
ok(all(plotted$pairs_from >= 5000), "every plotted group clears the stated size floor")
ok(!any(duplicated(plotted$label)), "no duplicate labels after bucketing")

cat("\n=== warning markers land on the tiles that need them, and only those ===\n")
warned <- function(pid, res, sg) {
  out <- c()
  testServer(app, {
    session$setInputs(cl_pair = pid, cl_res = res, cl_source = sg)
    for (nm in c("cl_tile_gained", "cl_tile_lost", "cl_tile_species",
                 "cl_tile_cells", "cl_tile_fieldwork")) {
      # Detect the warning by its aria-label, not by the glyph: the glyph is
      # non-ASCII and its encoding survives testServer unreliably, and the
      # aria-label is the part that actually has to be there for a screen reader.
      h <- output[[nm]]; if (is.list(h) && !is.null(h$html)) h <- h$html
      out[[nm]] <<- grepl("Caveat:", paste(as.character(h), collapse = " "), fixed = TRUE)
    }
  })
  out
}
w_within <- warned(W, "10km", "observation_platforms")
cat("    within-regime, single stream:", paste(names(w_within), w_within, collapse = " "), "\n")
ok(!w_within[["cl_tile_gained"]], "within regime: pairs gained carries NO warning")
ok(!w_within[["cl_tile_lost"]],   "within regime: pairs lost carries NO warning")
ok(w_within[["cl_tile_species"]], "species tile always warns (checklist scope), in both regimes")
ok(!w_within[["cl_tile_fieldwork"]], "single stream: no averaging warning")

w_avg <- warned(W, "10km", "total")
ok(w_avg[["cl_tile_fieldwork"]], "both streams: fieldwork tile warns it is an average of two")

w_cross <- warned(X, "10km", "total")
cat("    cross-regime:", paste(names(w_cross), w_cross, collapse = " "), "\n")
ok(all(unlist(w_cross)), "cross regime: every tile carries a warning")

w_cross50 <- warned(X, "50km", "total")
ok(w_cross50[["cl_tile_cells"]],
   "50 km cross-regime: cells-filled warns (the grid is saturated there)")

cat(sprintf("\n%d/%d checks passed\n", pass, pass + fail))
if (fail) quit(status = 1)
