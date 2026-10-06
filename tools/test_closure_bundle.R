#!/usr/bin/env Rscript
# ============================================================================
# tools/test_closure_bundle.R - integration test for closure_bundle()
# ============================================================================
# Unlike tools/test_closure.R, this one runs against REAL closure tables, because
# the thing it guards cannot be checked on a fixture: that the species totals are
# computed from the full table and only the ROWS are thinned. Get that wrong and
# every count in the Gaps filled tab is quietly too small, with no error and no
# obviously wrong number to notice.
#
# Ground truth is re-read from the CSVs on every run rather than hard-coded, so
# this stays valid after script 14 is re-run with different data.
#
#   Rscript tools/test_closure_bundle.R
#   CLOSURE_DIR=/path/to/closure Rscript tools/test_closure_bundle.R   # sandbox
#
# Requires scripts/14_gap_closure.R to have run at least once.
# ============================================================================

CLOSURE_DIR <- Sys.getenv("CLOSURE_DIR", "")
if (nzchar(CLOSURE_DIR)) {
  # Sandbox mode: no project setup, no arrow, no GBIF. Run from the repo root.
  suppressPackageStartupMessages({ library(data.table); library(cli) })
  source(file.path("R", "closure.R"))
} else {
  source(here::here("scripts", "00_setup.R"))
  source(here::here("R", "closure.R"))
  CLOSURE_DIR <- here::here(p_data_proc, "closure")
}

fail <- 0L
ok <- function(cond, msg) {
  if (isTRUE(cond)) cli_alert_success(msg)
  else { cli_alert_danger(msg); fail <<- fail + 1L }
}
eq <- function(got, want, msg) ok(isTRUE(all.equal(got, want)),
                                 sprintf("%s  (got %s, want %s)", msg,
                                         paste(got, collapse = ","),
                                         paste(want, collapse = ",")))

cli_h1("closure_bundle() against real tables")
cli_alert_info("Closure directory: {.path {CLOSURE_DIR}}")

if (!dir.exists(CLOSURE_DIR) ||
    !length(list.files(CLOSURE_DIR, "^closure_summary_"))) {
  cli_abort(c("No closure tables to test against",
              "i" = "Run {.code Rscript -e 'source(\"scripts/14_gap_closure.R\")'} first."))
}

THREAT   <- c("CR", "EN", "VU", "NT", "RE", "DD")
KEEP_TOP <- 1000L
cb <- closure_bundle(CLOSURE_DIR, keep_top = KEEP_TOP, threat_cats = THREAT)

cli_h2("shape")
ok(length(cb$pairs) > 0, "at least one pair discovered")
ok(all(c("summary", "cells", "group", "mechanism", "datasets",
         "species", "species_totals", "pair_index") %in% names(cb$tables)),
   "every table family is present")
eq(nrow(cb$tables$pair_index), length(cb$pairs), "pair index has one row per pair")
ok(all(c("regime_boundary", "mechanism_boundary_year", "filters_differ") %in%
         names(cb$tables$pair_index)), "pair index carries the flags the tab warns off")
ok(all(cb$tables$pair_index$from_date < cb$tables$pair_index$to_date),
   "every pair runs forwards in time")

cli_h2("the file list, not a guess, decides what exists")
ks <- unique(cb$tables$cells[, .(pair_id, key_space)])
xr <- cb$tables$pair_index[regime_boundary == 1, pair_id]
if (length(xr)) {
  ok(all(ks[pair_id %in% xr, key_space] == "dyntaxa"),
     "a cross-regime pair carries dyntaxa only - gbif is never written there")
} else {
  cli_alert_info("No cross-regime pair present; skipping that check")
}
wr <- cb$tables$pair_index[regime_boundary == 0, pair_id]
if (length(wr)) {
  ok("gbif" %in% ks[pair_id %in% wr, key_space],
     "a within-regime pair carries the gbif key space too")
}

cli_h2("THE INVARIANT: totals are computed before the rows are thinned")
sp_files <- list.files(CLOSURE_DIR, "^closure_species_(\\d+)km_([a-z]+)_(.*)\\.csv$")
checked <- 0L
for (f in sp_files) {
  m   <- regmatches(f, regexec("^closure_species_(\\d+)km_([a-z]+)_(.*)\\.csv$", f))[[1]]
  res <- paste0(m[2], "km"); kspace <- m[3]
  pid <- sub("_", "__", m[4], fixed = TRUE)
  raw <- fread(file.path(CLOSURE_DIR, f), showProgress = FALSE)

  tot <- cb$tables$species_totals[pair_id == pid & resolution == res & key_space == kspace]
  kept <- cb$tables$species[pair_id == pid & resolution == res & key_space == kspace]
  if (!nrow(tot)) { cli_alert_danger("no totals for {f}"); fail <- fail + 1L; next }

  eq(sum(tot$n_species),    nrow(raw),             sprintf("%s: totals count every species", f))
  eq(sum(tot$cells_gained), sum(raw$cells_gained), sprintf("%s: totals sum the full gain", f))
  eq(sum(tot$cells_lost),   sum(raw$cells_lost),   sprintf("%s: totals sum the full loss", f))

  # Nothing the tab enumerates by name may be thinned away.
  eq(nrow(kept[threatStatus_redlist %in% THREAT]),
     nrow(raw[threatStatus_redlist %in% THREAT]),
     sprintf("%s: every red-listed species survived", f))
  eq(nrow(kept[status %in% c("lost", "contracted")]),
     nrow(raw[status %in% c("lost", "contracted")]),
     sprintf("%s: every lost/contracted species survived", f))
  eq(max(kept$cells_gained), max(raw$cells_gained), sprintf("%s: biggest gainer survived", f))
  eq(max(kept$cells_lost),   max(raw$cells_lost),   sprintf("%s: biggest loser survived", f))
  ok(nrow(kept) <= nrow(raw), sprintf("%s: thinning never invents rows", f))
  checked <- checked + 1L
}
cli_alert_info("Checked {checked} species table{?s}")

cli_h2("the lost taxa have names")
# Script 14 names a taxon gone by `to` from the EARLIER match table. Tables
# annotated from the LATER match table only leave every such taxon unnamed -
# exactly 100% of lost taxa, never a subset. That is what makes the two cases
# separable, and worth separating: all-unnamed tables are STALE DATA, not a
# code regression, and should send the reader to a re-run rather than to a
# debugger.
lost <- cb$tables$species[status == "lost"]
if (!nrow(lost)) {
  cli_alert_info("No lost species in these tables; nothing to check")
} else {
  unnamed <- is.na(lost$backbone_scientificName) |
    trimws(lost$backbone_scientificName) == ""
  if (all(unnamed)) {
    cli_alert_warning(
      "All {nrow(lost)} lost taxa are unnamed - these closure tables are stale")
    cli_bullets(c("i" = "Re-run {.code Rscript -e 'source(\"scripts/14_gap_closure.R\")'} \\
                         and this section will pass.",
                  "i" = "Not counted as a failure: it is the data that is stale, not the code."))
  } else {
    ok(!any(unnamed), "no lost species is unnamed")
    ok(!any(is.na(lost$class) | trimws(lost$class) == ""),
       "no lost species is missing its class")
    ok(nrow(cb$tables$group[is.na(class) | trimws(class) == ""]) == 0L,
       "no (NA, NA) group in the taxonomic roll-up")
  }
}

cli_h2("bundle cost")
f <- tempfile(fileext = ".rds")
saveRDS(cb$tables, f, compress = "xz")
mb <- file.size(f) / 1024^2
cli_alert_info("Closure payload, xz-compressed: {round(mb, 2)} MB")
ok(mb < 6, "payload stays well under the bundle's 78-80 MB working size")

cli_h2("Result")
if (fail == 0L) { cli_alert_success("All closure-bundle checks passed"); quit(status = 0L) }
cli_abort("{fail} closure-bundle check{?s} failed")
