#!/usr/bin/env Rscript
# tools/test_bridge.R
# ============================================================================
# Unit tests for R/bridge.R and the pair helpers in R/closure.R
# ============================================================================
# Tiny fixtures whose right answers are worked out in the comments.
#
#   Rscript tools/test_bridge.R
#
# Exits non-zero on any failure. Needs data.table, cli and here only.
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(cli); library(here) })
source(here::here("R", "closure.R"))
source(here::here("R", "bridge.R"))

fail <- 0L
ok <- function(cond, msg) {
  if (isTRUE(cond)) cli_alert_success(msg)
  else { cli_alert_danger(msg); fail <<- fail + 1L }
}
eq <- function(got, want, msg) ok(isTRUE(all.equal(got, want)),
                                 sprintf("%s  (got %s, want %s)", msg,
                                         paste(got, collapse = ","),
                                         paste(want, collapse = ",")))
pairs <- function(...) {
  x <- list(...)
  as.data.table(list(key = vapply(x, `[[`, "", 1), eeacellcode = vapply(x, `[[`, "", 2)))
}

cli_h2("bridge_parent_cell")
eq(bridge_parent_cell(c("10kmE474N381", "10kmE470N380", "10kmE472N384")),
   rep("50kmE470N380", 3), "10 km cells nest in their 50 km cell")
eq(bridge_parent_cell("10kmE475N385"), "50kmE475N385", "a cell on the 5-boundary is its own corner")
ok(is.na(bridge_parent_cell("50kmE470N380")), "a non-10 km code gives NA")

cli_h2("bridge_compare_pairs")
# snapshot: A-c1 A-c2 B-c1 ; cube: A-c1 B-c3 C-c1
ps <- pairs(c("A", "10kmE470N380"), c("A", "10kmE471N380"), c("B", "10kmE470N380"))
pc <- pairs(c("A", "10kmE470N380"), c("B", "10kmE480N380"), c("C", "10kmE470N380"))
cmp <- bridge_compare_pairs(ps, pc)
eq(nrow(cmp$both), 1L, "one pair in both (A-E470)")
eq(nrow(cmp$snapshot_only), 2L, "two snapshot-only pairs (A-E471, B-E470)")
eq(nrow(cmp$cube_only), 2L, "two cube-only pairs (B-E480, C-E470)")

cli_h2("bridge_is_local")
# cube 50 km pairs: A in 50kmE470N380 (from A-E470), B in 50kmE480N380, C in 50kmE470N380
pc50 <- pairs(c("A", "50kmE470N380"), c("B", "50kmE480N380"), c("C", "50kmE470N380"))
# snapshot-only A-E471 -> parent E470N380, cube has A there -> local
# snapshot-only B-E470 -> parent E470N380, cube has B only in E480 -> regional
loc <- bridge_is_local(cmp$snapshot_only, pc50)
got <- setNames(loc, paste(cmp$snapshot_only$key, cmp$snapshot_only$eeacellcode))
eq(unname(got["A 10kmE471N380"]), TRUE,  "A-E471 is local (A still in the 50 km cell)")
eq(unname(got["B 10kmE470N380"]), FALSE, "B-E470 is regional (B gone from that 50 km cell)")
eq(length(bridge_is_local(cmp$snapshot_only[0], pc50)), 0L, "empty input gives empty output")
# order must follow the input, not the merge
rev_in <- cmp$snapshot_only[.N:1]
eq(bridge_is_local(rev_in, pc50), rev(loc), "output order follows the input rows")

cli_h2("bridge_dataset_support")
# A-E471 carried by D1 only; B-E470 carried by D1 and D2
g <- as.data.table(list(
  key = c("A", "A", "B", "B"),
  eeacellcode = c("10kmE470N380", "10kmE471N380", "10kmE470N380", "10kmE470N380"),
  datasetkey = c("D1", "D1", "D1", "D2"),
  occ = c(5, 1, 2, 3)))
sup <- bridge_dataset_support(cmp$snapshot_only, g)
eq(sup[datasetkey == "D1", pairs_any], 2L, "D1 supports both snapshot-only pairs")
eq(sup[datasetkey == "D1", pairs_sole], 1L, "D1 is the sole source of one (A-E471)")
eq(sup[datasetkey == "D2", pairs_sole], 0L, "D2 shares B-E470, so no sole pairs")
eq(sup$datasetkey[1], "D1", "ordered by sole support")
eq(nrow(bridge_dataset_support(cmp$snapshot_only[0], g)), 0L, "empty input gives an empty table")

cli_h2("bridge_group_factors")
lk <- as.data.table(list(key = c("A", "B", "C"), class = c("Aves", "Aves", "Insecta"),
                         order = c("Passeriformes", "Passeriformes", "Diptera")))
gf <- bridge_group_factors(ps, pc, cmp, lk)
pas <- gf[order == "Passeriformes"]
eq(pas$pairs_snapshot, 3L, "Passeriformes: 3 snapshot pairs")
eq(pas$pairs_cube, 2L, "Passeriformes: 2 cube pairs (A-E470, B-E480)")
eq(pas$snapshot_only, 2L, "Passeriformes: 2 snapshot-only")
eq(pas$cube_only, 1L, "Passeriformes: 1 cube-only")
eq(pas$bridge_factor, round(2 / 3, 4), "bridge factor = cube / snapshot")
eq(pas$n_taxa, 2L, "Passeriformes: 2 taxa")
dip <- gf[order == "Diptera"]
eq(dip$pairs_snapshot, 0L, "Diptera: absent from the snapshot counts as 0, not NA")
ok(is.na(dip$bridge_factor), "Diptera: no factor without snapshot pairs")
eq(sum(gf$pairs_cube), nrow(pc), "group cube pairs sum to the total")

cli_h2("bridge_dataset_occ")
gs <- as.data.table(list(datasetkey = c("D1", "D2"), occ = c(10, 5)))
gc_ <- as.data.table(list(datasetkey = c("D1", "D3"), occ = c(8, 7)))
do <- bridge_dataset_occ(gs, gc_)
eq(do[datasetkey == "D2", occ_cube], 0, "a dataset missing from the cube counts 0")
eq(do[datasetkey == "D3", occ_snapshot], 0, "a dataset missing from the snapshot counts 0")
ok(is.na(do[datasetkey == "D3", occ_ratio]), "no ratio without snapshot records")
eq(do[datasetkey == "D1", occ_ratio], 0.8, "ratio = cube / snapshot")
eq(do$datasetkey[1], "D3", "ordered by absolute difference")

cli_h2("bridge_summary")
sm <- bridge_summary(cmp, ps, pc, loc, c(TRUE, FALSE), g, g)
val <- function(m) sm[metric == m, value]
eq(val("pairs_both"), 1, "summary: pairs_both")
eq(val("snapshot_only_pct"), round(100 * 2 / 3, 3), "summary: snapshot-only share of snapshot pairs")
eq(val("snapshot_only_local"), 1, "summary: one local snapshot-only pair")
eq(val("taxa_both"), 2, "summary: A and B in both")
eq(val("cells_both"), 1, "summary: one cell in both")
ok(!"snapshot_only_local" %in% bridge_summary(cmp, ps, pc, NULL, NULL, g, g)$metric,
   "summary: no locality rows when locality is undefined (50 km)")

cli_h2("closure_pair_kind")
m <- function(tp, rg, kb) list(tp = tp, regime = rg, key_backbone = kb)
k1 <- closure_pair_kind(m("2021-01-01", "snapshot", "legacy"), m("2024-01-01", "snapshot", "legacy"))
ok(!k1$regime_boundary && !k1$key_backbone_differs && !k1$is_bridge,
   "2021 -> 2024: within regime, same keys")
eq(k1$key_spaces, c("dyntaxa", "gbif"), "2021 -> 2024: both key spaces")
k2 <- closure_pair_kind(m("2024-01-01", "snapshot", "legacy"), m("2026-10-01", "snapshot", "col"))
ok(!k2$regime_boundary && k2$key_backbone_differs, "2024 -> 2026-10: same regime, keys differ")
eq(k2$key_spaces, "dyntaxa", "2024 -> 2026-10: Dyntaxa space only")
k3 <- closure_pair_kind(m("2026-10-01", "snapshot", "col"), m("2026-10-02", "cube", "col"))
ok(k3$is_bridge, "2026-10-01 snapshot vs 2026-10-02 cube is the bridge")
eq(k3$key_spaces, c("dyntaxa", "gbif"), "bridge: same COL keys, both key spaces")
k4 <- closure_pair_kind(m("2024-01-01", "snapshot", "legacy"), m("2026-07-29", "cube", "col"))
ok(k4$regime_boundary && !k4$is_bridge, "2024 -> July cube: cross-regime, not a bridge")
k5 <- closure_pair_kind(m("2026-07-29", "cube", "col"), m("2026-10-02", "cube", "col"))
ok(!k5$regime_boundary && !k5$is_bridge, "July -> October cube: within regime")
ok(inherits(try(closure_pair_kind(m("x", "live", "col"), m("2024-01-01", "snapshot", "legacy")),
                silent = TRUE), "try-error"), "an unknown regime is refused")
ok(inherits(try(closure_pair_kind(m("2021-01-01", "snapshot", NULL), m("2024-01-01", "snapshot", "legacy")),
                silent = TRUE), "try-error"), "a missing key_backbone is refused")

cli_h2("closure_default_pairs")
meta <- as.data.table(list(
  tp = c("2021-01-01", "2024-01-01", "2026-07-29", "2026-10-01", "2026-10-02"),
  regime = c("snapshot", "snapshot", "cube", "snapshot", "cube")))
dp <- vapply(closure_default_pairs(meta), paste, "", collapse = ">")
eq(dp, c("2021-01-01>2024-01-01", "2024-01-01>2026-10-01", "2021-01-01>2026-10-01",
         "2026-07-29>2026-10-02"),
   "consecutive within each regime, plus first -> last of the snapshot series")
ok(!any(grepl("2026-10-01>2026-10-02|2024-01-01>2026-07-29", dp)),
   "no cross-regime pair by default")
eq(length(closure_default_pairs(meta[regime == "cube"][1])), 0L, "one point gives no pairs")

cli_h2("Result")
if (fail == 0L) { cli_alert_success("All bridge checks passed"); quit(status = 0L) }
cli_abort("{fail} bridge check{?s} failed")
