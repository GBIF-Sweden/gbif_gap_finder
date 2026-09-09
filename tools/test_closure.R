#!/usr/bin/env Rscript
# tools/test_closure.R
# ============================================================================
# Unit tests for R/closure.R
# ============================================================================
# The closure arithmetic decides every number in the "Gaps filled" tab, and most
# of its failure modes are silent: an off-by-one in a set difference, a
# tie-break creeping into a concentration figure, a mechanism computed over the
# wrong subset. None of those error - they just publish a different number.
#
# So the fixture below is tiny and its right answers are worked out by hand in
# the comments. Six species, five cells, three datasets. If a change to
# R/closure.R breaks one of these, the message says which invariant died.
#
#   Rscript tools/test_closure.R
#
# Exits non-zero on any failure, so it can go in CI. Needs data.table and cli
# only - no arrow, no cubes, no pipeline run.
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(cli); library(here) })
source(here::here("R", "closure.R"))

fail <- 0L
ok <- function(cond, msg) {
  if (isTRUE(cond)) cli_alert_success(msg)
  else { cli_alert_danger(msg); fail <<- fail + 1L }
}
eq <- function(got, want, msg) ok(isTRUE(all.equal(got, want)),
                                 sprintf("%s  (got %s, want %s)", msg,
                                         paste(got, collapse = ","),
                                         paste(want, collapse = ",")))

ALL_CELLS <- c("C1", "C2", "C3", "C4", "C5")
PLATFORMS <- "D1"          # D2, D3 are collections
BASELINE_YEAR <- 2021L

# NOTE: `key` is a reserved ARGUMENT of data.table(), so data.table(key = "A")
# sets the table's key rather than creating a column called `key`. Build these
# through as.data.table(list(...)), which has no such collision. R/closure.R only
# ever reads `key` as an existing column, so it is unaffected - but anyone
# extending this will meet the same trap.
grain <- function(rows) {
  g <- data.table::rbindlist(lapply(rows, function(r) data.table::as.data.table(list(
    key = r[[1]], eeacellcode = r[[2]], datasetkey = r[[3]],
    min_year = r[[4]], occ = r[[5]], n_rec = r[[6]]))))
  g[, source_group := closure_source_group(datasetkey, PLATFORMS)]
  g[]
}

# --- FROM: 4 pairs, cells C1(13) C2(5) C3(7) occupied, C4 C5 empty ----------
gf <- grain(list(
  list("A", "C1", "D1", 2010, 10, 1),
  list("A", "C2", "D1", 2011,  5, 1),
  list("B", "C1", "D2", 2012,  3, 1),
  list("C", "C3", "D1", 2015,  7, 1)
))

# --- TO --------------------------------------------------------------------
# A-C1 deepened 13->23 · A-C2 unchanged · C-C3 dropped (regressed, C lost)
# GAINED: A-C4 (D1, 2022)      -> fieldwork, platform
#         D-C2 (D3, 1995)      -> digitisation, collections, D is new
#         E-C5 (D2, no year)   -> unattributable, collections, E is new
#         F-C4 (D1 2022 + D2 2005) -> spans BOTH groups: overall min 2005 so the
#                                 total says digitisation, but the platform
#                                 stream's own contribution is 2022 = fieldwork
gt <- grain(list(
  list("A", "C1", "D1", 2010, 20, 2),
  list("A", "C2", "D1", 2011,  5, 1),
  list("A", "C4", "D1", 2022,  4, 1),
  list("B", "C1", "D2", 2012,  3, 1),
  list("D", "C2", "D3", 1995,  2, 1),
  list("E", "C5", "D2",   NA,  1, 1),
  list("F", "C4", "D1", 2022,  2, 1),
  list("F", "C4", "D2", 2005,  3, 1)
))

pf <- closure_pairs(gf); pt <- closure_pairs(gt)
gained <- data.table::fsetdiff(pt, pf)
lost   <- data.table::fsetdiff(pf, pt)

cli_h2("pairs")
eq(nrow(pf), 4L, "4 pairs at the baseline")
eq(nrow(pt), 7L, "7 pairs at the comparison point")
eq(nrow(gained), 4L, "4 pairs gained")
eq(nrow(lost), 1L, "1 pair lost")

cli_h2("cells")
cells <- closure_cells(closure_cell_view(gf), closure_cell_view(gt), ALL_CELLS)
eq(nrow(cells), 5L, "every grid cell is present, including the ones with no data")
eq(cells[eeacellcode == "C1", status], "deepened", "C1 13 -> 23 is deepened")
eq(cells[eeacellcode == "C2", status], "deepened", "C2 5 -> 7 is deepened")
eq(cells[eeacellcode == "C3", status], "regressed", "C3 7 -> 0 is regressed")
eq(cells[eeacellcode == "C4", status], "filled", "C4 0 -> 9 is filled")
eq(cells[eeacellcode == "C5", status], "filled", "C5 0 -> 1 is filled")
eq(cells[eeacellcode == "C4", occ_to], 9, "C4 sums both datasets of F plus A")
eq(cells[eeacellcode == "C4", n_species_to], 2L, "C4 holds two species at the end")
eq(sum(cells$status == "filled"), 2L, "2 cells filled")
eq(sum(cells$occ_from), 25, "baseline occurrences preserved")
eq(sum(cells$occ_to), 40, "comparison occurrences preserved (23+7+9+1)")

cli_h2("species")
sp <- closure_species(pf, pt)
eq(sp[key == "A", status], "expanded", "A gained a cell and lost none")
eq(sp[key == "B", status], "unchanged", "B held still")
eq(sp[key == "C", status], "lost", "C disappeared")
eq(sort(sp[status == "new", key]), c("D", "E", "F"), "three species newly recorded")
eq(sp[key == "A", cells_gained], 1L, "A gained exactly one cell")

# A species that swaps one cell for another has delta 0 but is NOT unchanged.
sp_shift <- closure_species(
  data.table::as.data.table(list(key = "Z", eeacellcode = "C1")),
  data.table::as.data.table(list(key = "Z", eeacellcode = "C2")))
eq(sp_shift[key == "Z", status], "shifted", "a cell-for-cell swap reads as shifted, not unchanged")

cli_h2("mechanism")
mech <- closure_mechanism(gained, gt, BASELINE_YEAR)
tot <- mech[source_group == "total"]
eq(tot[mechanism == "fieldwork", n_pairs], 1L, "total: 1 fieldwork pair (A-C4)")
eq(tot[mechanism == "digitisation", n_pairs], 2L, "total: 2 digitisation pairs (D-C2, F-C4)")
eq(tot[mechanism == "unattributable", n_pairs], 1L, "total: 1 unattributable pair (E-C5)")
eq(sum(tot$n_pairs), 4L, "total mechanism counts sum to the gained pairs")

plat <- mech[source_group == "observation_platforms"]
coll <- mech[source_group == "collections"]
eq(plat[mechanism == "fieldwork", n_pairs], 2L,
   "platforms: 2 fieldwork - F-C4 counts here at ITS OWN min year, not the pair's")
eq(nrow(plat[mechanism == "digitisation"]), 0L, "platforms contributed no digitisation")
eq(coll[mechanism == "digitisation", n_pairs], 2L, "collections: 2 digitisation")
eq(coll[mechanism == "unattributable", n_pairs], 1L, "collections: 1 unattributable")
ok(sum(plat$n_pairs) + sum(coll$n_pairs) > sum(tot$n_pairs),
   "per-group counts exceed the total, because F-C4 spans both groups")

cli_h2("dataset concentration (no tie-break)")
dsc <- closure_dataset_concentration(gained, gt)
eq(dsc[datasetkey == "D1", touched], 2L, "D1 touches A-C4 and F-C4")
eq(dsc[datasetkey == "D1", vanish_if_dropped], 1L,
   "dropping D1 removes only A-C4: F-C4 survives on D2")
eq(dsc[datasetkey == "D2", touched], 2L, "D2 touches E-C5 and F-C4")
eq(dsc[datasetkey == "D2", vanish_if_dropped], 1L, "dropping D2 removes only E-C5")
eq(dsc[datasetkey == "D3", vanish_if_dropped], 1L, "D3 solely supports D-C2")
ok(sum(dsc$vanish_if_dropped) <= nrow(gained),
   "pairs that vanish never exceed pairs gained")

cli_h2("frozen thresholds")
fz <- closure_frozen_low(cells, q = 0.10)
eq(unique(fz$q_frozen), as.numeric(stats::quantile(c(13, 5, 7), 0.10, names = FALSE)),
   "threshold comes from the BASELINE's non-zero cells only")
eq(fz[eeacellcode == "C2", low_status], "improved", "C2 was below the frozen threshold and rose above it")
eq(fz[eeacellcode == "C5", low_status], "worsened", "C5 is newly occupied but below the frozen threshold")
eq(fz[eeacellcode == "C1", low_status], "not_low", "C1 was never low")

cli_h2("summary")
sm <- closure_summary(pf, pt, cells, sp, mech, key_space = "gbif",
                      baseline_year = BASELINE_YEAR)
val <- function(m) sm[metric == m & source_group == "total", value]
eq(val("pairs_gained"), 4, "summary: pairs gained")
eq(val("pairs_lost"), 1, "summary: pairs lost")
eq(val("pairs_net"), 3, "summary: net change")
eq(val("cells_filled"), 2, "summary: cells filled")
eq(val("cells_regressed"), 1, "summary: cells regressed")
eq(val("cells_universe"), 5, "summary: the universe is the grid, not the data")
eq(val("cells_never_filled"), 0, "summary: no cell was empty at BOTH ends")
eq(val("cells_empty_now"), 1, "summary: one cell is empty now - C3, which regressed")
eq(val("species_newly_recorded"), 3, "summary: species newly recorded")
eq(val("species_no_longer_recorded"), 1, "summary: species no longer recorded")
eq(val("mechanism_boundary_year"), 2021, "summary: the boundary year is recorded, not implied")
ok(all(sm$key_space == "gbif"), "every summary row is tagged with its key space")

cli_h2("empty-input guards")
empty_pairs <- data.table::as.data.table(list(key = character(), eeacellcode = character()))
ok(nrow(closure_mechanism(empty_pairs, gt, BASELINE_YEAR)) == 0L,
   "no gained pairs -> an empty mechanism table, not an error")
ok(nrow(closure_dataset_concentration(empty_pairs, gt)) == 0L,
   "no gained pairs -> an empty concentration table, not an error")
ok(inherits(try(closure_cells(closure_cell_view(gf), closure_cell_view(gt),
                              character()), silent = TRUE), "try-error"),
   "an empty cell universe is refused, not silently taken from the data")

cli_h2("Result")
if (fail == 0L) { cli_alert_success("All closure checks passed"); quit(status = 0L) }
cli_abort("{fail} closure check{?s} failed")
