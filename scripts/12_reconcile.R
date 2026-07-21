# scripts/12_reconcile.R
# ============================================================================
# Reconciliation Guardrail
# ============================================================================
# Fails the build if headline numbers disagree across pipeline layers. Run
# after scripts 10 and 11:   source("scripts/12_reconcile.R")
#
# Encodes the project's source-of-truth decisions (2026-07-21):
#   * "all" scope = all of GBIF; Taxonomic / Concern = backbone match
#   * threatened = CR/EN/VU/NT; the reference is a SPECIES count, not categories
#   * spatial coverage is measured against the FULL grid (zero-filled)
#
# Each check is defensive: a missing input is skipped (not a failure), so this
# can run at any pipeline stage. Real disagreements call stop().
# ============================================================================

source(here::here("scripts", "00_setup.R"))

cli_h1("Reconciliation checks (Script 12)")

problems <- character(0)
note     <- function(ok, msg) if (!isTRUE(ok)) problems <<- c(problems, msg)

first_existing <- function(...) {
  for (p in c(...)) if (!is.null(p) && !is.na(p) && file.exists(p)) return(p)
  NA_character_
}
rd <- function(p) if (!is.na(p)) data.table::fread(p) else NULL

THREAT <- c("CR", "EN", "VU", "NT")

# ---- Load dashboard (long form: metric,value) ------------------------------
dash <- rd(first_existing(here(p_tables, "dashboard_summary_long.csv")))
dval <- function(m) if (is.null(dash)) NA_real_ else
  suppressWarnings(as.numeric(dash[metric == m]$value[1]))

# ---- 1. threatened_in_reference is a SPECIES count, not a category count ---
if (!is.null(dash)) {
  tir <- dval("threatened_in_reference"); tig <- dval("threatened_in_gbif")
  note(is.na(tir) || is.na(tig) || tir >= tig, sprintf(
    "threatened_in_reference (%s) < threatened_in_gbif (%s): counting categories, not species (B1).",
    tir, tig))

  bt <- rd(first_existing(here(p_tables, "overview_taxonomic_by_threat.csv"),
                          here(p_gaps,   "taxonomic_coverage_by_threat.csv")))
  if (!is.null(bt) && "n_ref_total" %in% names(bt)) {
    exp_tir <- sum(bt[threatStatus %in% THREAT]$n_ref_total, na.rm = TRUE)
    note(is.na(tir) || abs(tir - exp_tir) < 1, sprintf(
      "threatened_in_reference (%s) != sum of by-threat n_ref_total for CR/EN/VU/NT (%s).",
      tir, exp_tir))
  }
}

# ---- 2. spatial_gaps 'all' basis must be zero-filled to the FULL grid ------
for (res in c("10km", "50km")) {
  sg <- rd(first_existing(here(p_derived, sprintf("spatial_gaps_all_%s.csv", res)),
                          here(p_gaps,    sprintf("spatial_gaps_%s.csv", res))))
  gl <- rd(first_existing(here(p_derived, sprintf("grid_lookup_%s.csv", res))))
  if (!is.null(sg) && !is.null(gl) && "basisofrecord" %in% names(sg)) {
    n_all  <- nrow(sg[basisofrecord == "all"])
    n_grid <- data.table::uniqueN(gl$eeacellcode)
    note(n_all == n_grid, sprintf(
      "%s: spatial_gaps 'all' rows (%s) != full grid cells (%s): coverage not measured against the full grid.",
      res, n_all, n_grid))
  }
}

# ---- 3. dashboard threatened_missing == nrow(missing_threatened) -----------
mt <- rd(first_existing(here(p_gaps, "taxonomic_missing_threatened.csv")))
if (!is.null(dash) && !is.null(mt)) {
  tm <- dval("threatened_missing")
  note(is.na(tm) || tm == nrow(mt), sprintf(
    "dashboard threatened_missing (%s) != nrow(taxonomic_missing_threatened) (%s): Overview vs Concern drift.",
    tm, nrow(mt)))
}

# ---- 4. taxonomic coverage is internally consistent ------------------------
if (!is.null(dash)) {
  tref <- dval("taxa_in_reference"); tgb <- dval("taxa_in_gbif"); tmiss <- dval("taxa_missing")
  note(any(is.na(c(tref, tgb, tmiss))) || abs((tgb + tmiss) - tref) < 1, sprintf(
    "taxa_in_gbif (%s) + taxa_missing (%s) != taxa_in_reference (%s).", tgb, tmiss, tref))
}

# ---- Verdict ---------------------------------------------------------------
if (length(problems)) {
  cli_alert_danger("Reconciliation FAILED — {length(problems)} issue(s):")
  for (p in problems) cli_alert_warning(p)
  stop("Reconciliation checks failed — see above.", call. = FALSE)
} else {
  cli_alert_success("Reconciliation OK — headline numbers agree across layers.")
}
