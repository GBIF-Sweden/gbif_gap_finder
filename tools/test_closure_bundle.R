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
ok(all(c("regime_boundary", "mechanism_boundary_year", "filters_differ",
         "key_backbone_differs") %in% names(cb$tables$pair_index)),
   "pair index carries the flags the tab warns off")
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
# GBIF keys of two backbones (legacy nub, COL) never match, so only a pair on
# one backbone, within one regime, has a gbif key space.
kb <- cb$tables$pair_index[key_backbone_differs == 1, pair_id]
if (length(kb)) {
  ok(all(ks[pair_id %in% kb, key_space] == "dyntaxa"),
     "a pair across key backbones carries dyntaxa only")
}
same <- cb$tables$pair_index[regime_boundary == 0 & key_backbone_differs == 0, pair_id]
if (length(same)) {
  ok(all(vapply(same, function(p) "gbif" %in% ks[pair_id == p, key_space], logical(1))),
     "a within-regime pair on one key backbone carries the gbif key space too")
}

cli_h2("losses by dataset")
dl <- cb$tables$datasets_lost
if (is.null(dl)) {
  cli_alert_warning(
    "No closure_datasets_lost tables - these tables predate them. Re-run \\
     {.code Rscript -e 'source(\"scripts/14_gap_closure.R\")'}.")
} else {
  sl <- cb$tables$summary[metric == "pairs_lost" & source_group == "total",
                          .(pair_id, resolution, key_space, pairs_lost = value)]
  dd <- dl[, .(sole = sum(vanish_if_dropped), max_touched = max(touched)),
           by = .(pair_id, resolution, key_space)]
  chk <- merge(sl, dd, by = c("pair_id", "resolution", "key_space"), all.x = TRUE)
  ok(!anyNA(chk[pairs_lost > 0, sole]),
     "every pair x resolution x key space with losses has its losses-by-dataset table")
  ok(all(chk$sole <= chk$pairs_lost, na.rm = TRUE),
     "pairs resting on one dataset never exceed the pairs lost")
  ok(all(chk$max_touched <= chk$pairs_lost, na.rm = TRUE),
     "no dataset touches more pairs than were lost")
  ok(all(chk[pairs_lost > 0, max_touched] >= 1),
     "the losses are read from the baseline's records, not from the later grain")
}

cli_h2("periods and source types")
mech <- cb$tables$mechanism
ok(all(mech$period %in% CLOSURE_PERIODS), "every period is one of CLOSURE_PERIODS")
ok(all(mech$source_group %in% c("total", CLOSURE_SOURCE_TYPES)),
   "every source group is a source type or the total")
gn <- cb$tables$summary[metric == "pairs_gained" & source_group == "total",
                        .(pair_id, resolution, key_space, gained = value)]
pt <- mech[source_group == "total", .(periods = sum(n_pairs)), by = .(pair_id, resolution, key_space)]
chk <- merge(gn, pt, by = c("pair_id", "resolution", "key_space"), all.x = TRUE)
ok(isTRUE(all.equal(chk$periods, chk$gained)), "the periods of the total add up to the pairs gained")

cli_h2("per class x order (the tab's taxonomic filter)")
cg <- cb$tables$cells_group; mg <- cb$tables$mechanism_group
if (is.null(cg) || is.null(mg)) {
  cli_alert_warning(
    "No per-group tables - these tables predate them. Re-run \\
     {.code Rscript -e 'source(\"scripts/14_gap_closure.R\")'}.")
} else {
  sm <- data.table::dcast(
    cb$tables$summary[key_space == "dyntaxa" & source_group == "total" &
                        metric %in% c("pairs_from", "pairs_to", "pairs_gained", "pairs_lost")],
    pair_id + resolution ~ metric, value.var = "value")
  cs <- cg[, .(pairs_from = sum(n_species_from), pairs_to = sum(n_species_to),
               pairs_gained = sum(pairs_gained), pairs_lost = sum(pairs_lost)),
           by = .(pair_id, resolution)]
  both <- merge(sm, cs, by = c("pair_id", "resolution"), suffixes = c("", "_cells"))
  eq(nrow(both), nrow(sm), "every pair x resolution has its cells per class x order")
  ok(all(both$pairs_from == both$pairs_from_cells & both$pairs_to == both$pairs_to_cells &
         both$pairs_gained == both$pairs_gained_cells & both$pairs_lost == both$pairs_lost_cells),
     "the cells per class x order add up to the pair totals")
  ok(all(cg$n_species_to - cg$n_species_from == cg$pairs_gained - cg$pairs_lost),
     "in every cell x group row the change equals gains minus losses")
  mt <- mg[source_group == "total", .(grouped = sum(n_pairs)), by = .(pair_id, resolution)]
  m2 <- merge(sm[, .(pair_id, resolution, gained = pairs_gained)], mt, by = c("pair_id", "resolution"))
  ok(nrow(m2) == nrow(sm) && all(m2$grouped == m2$gained),
     "the periods per class x order add up to the pairs gained")
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
# The names are Dyntaxa's, so this runs in the Dyntaxa key space, the tab's. In
# the gbif key space a key that never matched Dyntaxa has no Dyntaxa name at
# either end, lost or not.
lost <- cb$tables$species[status == "lost" & key_space == "dyntaxa"]
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
    # Dyntaxa ranks some orders under no class (Squamata, the tunicate orders)
    # and some classes without orders, so one empty rank is the checklist's.
    # Both empty is what unnamed lost taxa look like.
    no_rank <- function(x) is.na(x) | trimws(x) == ""
    ok(!any(no_rank(lost$class) & no_rank(lost$order)),
       "no lost species is missing both class and order")
    grp <- cb$tables$group[key_space == "dyntaxa"]
    ok(nrow(grp[no_rank(class) & no_rank(order)]) == 0L,
       "no (NA, NA) group in the taxonomic roll-up")
    oc <- sort(unique(grp[no_rank(class) & !no_rank(order), order]))
    if (length(oc)) {
      cli_alert_info(
        "{length(oc)} order{?s} without a class in Dyntaxa, e.g. {.val {utils::head(oc, 4)}}")
    }
  }
}

cli_h2("bundle cost")
f <- tempfile(fileext = ".rds")
saveRDS(cb$tables, f, compress = "xz")
mb <- file.size(f) / 1024^2
cli_alert_info("Closure payload, xz-compressed: {round(mb, 2)} MB")
ok(mb < 6, "payload stays well under the bundle's 80-85 MB working size")

cli_h2("Result")
if (fail == 0L) { cli_alert_success("All closure-bundle checks passed"); quit(status = 0L) }
cli_abort("{fail} closure-bundle check{?s} failed")
