# scripts/10_make_phase4_overview.R
# Phase 4: Create clean, analysis/plot-ready overview tables from Phase 3 gap outputs
#
# Inputs (Phase 3):
#   data_proc/gaps/
#     - spatial_gaps_10km.csv
#     - spatial_gaps_50km.csv
#     - spatial_thresholds_by_basis.csv
#     - temporal_overview_month_*.csv
#     - temporal_overview_year_*.csv
#     - cell_recency_10km.csv
#     - cell_recency_50km.csv
#     - taxonomic_gap_summary.csv
#     - missing_threatened_taxa.csv
#
# Outputs (Phase 4):
#   output/tables/
#     - overview_spatial_gap_rates.csv
#     - overview_temporal_recency_rates.csv
#     - overview_temporal_year.csv
#     - overview_temporal_month.csv
#     - overview_taxonomic_summary.csv
#     - overview_missing_threatened_taxa.csv

source("scripts/00_setup.R")

# ---- Dependencies -------------------------------------------------------------
pkgs <- c("data.table", "dplyr", "readr", "stringr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) stop("Missing package: ", p)

# ---- Paths -------------------------------------------------------------------
data_proc_rel <- cfg_get("paths.data_proc", "data_proc")
out_rel <- cfg_get("paths.output", "output")

p_data_proc <- here::here(data_proc_rel)
p_gaps <- file.path(p_data_proc, "gaps")

p_out <- here::here(out_rel)
p_tables <- file.path(p_out, "tables")
dir.create(p_tables, showWarnings = FALSE, recursive = TRUE)

# ---- Helpers -----------------------------------------------------------------
safe_mean <- function(x) mean(as.numeric(x), na.rm = TRUE)
safe_sum  <- function(x) sum(as.numeric(x), na.rm = TRUE)

read_gap <- function(fname) {
  path <- file.path(p_gaps, fname)
  if (!file.exists(path)) stop("Missing gap output: ", path)
  data.table::fread(path)
}

write_table <- function(dt, fname) {
  out <- file.path(p_tables, fname)
  data.table::fwrite(dt, out)
  log_msg("Wrote: ", out)
}

# ---- 1a) Spatial gap rates -----------------------------------------------------
sp10 <- read_gap("spatial_gaps_10km.csv")
sp50 <- read_gap("spatial_gaps_50km.csv")

sp <- data.table::rbindlist(list(sp10, sp50), use.names = TRUE, fill = TRUE)

# Expect columns: grid, basisofrecord, eeacellcode, occurrences, gap_zero, gap_low_q05, gap_low_q10
need <- c("grid", "basisofrecord", "eeacellcode", "occurrences", "gap_zero", "gap_low_q05", "gap_low_q10")
miss <- setdiff(need, names(sp))
if (length(miss)) stop("Missing columns in spatial gaps: ", paste(miss, collapse = ", "))

sp_overview <- sp[, .(
  n_cells = .N,
  n_zero = sum(gap_zero, na.rm = TRUE),
  pct_zero = round(100 * mean(gap_zero, na.rm = TRUE), 2),
  n_low_q05 = sum(gap_low_q05, na.rm = TRUE),
  pct_low_q05 = round(100 * mean(gap_low_q05, na.rm = TRUE), 2),
  n_low_q10 = sum(gap_low_q10, na.rm = TRUE),
  pct_low_q10 = round(100 * mean(gap_low_q10, na.rm = TRUE), 2),
  total_occurrences = safe_sum(occurrences)
), by = .(grid, basisofrecord)]

data.table::setorder(sp_overview, grid, basisofrecord)
write_table(sp_overview, "overview_spatial_gap_rates.csv")

# ---- 1b) Spatial gap rates by basisOfRecord (explicit output) -----------------

sp_overview_by_basis <- sp[, .(
  n_cells = .N,
  n_zero = sum(gap_zero, na.rm = TRUE),
  pct_zero = round(100 * mean(gap_zero, na.rm = TRUE), 2),
  n_low_q05 = sum(gap_low_q05, na.rm = TRUE),
  pct_low_q05 = round(100 * mean(gap_low_q05, na.rm = TRUE), 2),
  n_low_q10 = sum(gap_low_q10, na.rm = TRUE),
  pct_low_q10 = round(100 * mean(gap_low_q10, na.rm = TRUE), 2),
  total_occurrences = safe_sum(occurrences)
), by = .(grid, basisofrecord)]

data.table::setorder(sp_overview_by_basis, grid, basisofrecord)
write_table(sp_overview_by_basis, "overview_spatial_gap_rates_by_basis.csv")


# ---- 2) Temporal overview tables ---------------------------------------------
# Year
t_y10 <- read_gap("temporal_overview_year_10km.csv")
t_y50 <- read_gap("temporal_overview_year_50km.csv")
t_year <- data.table::rbindlist(list(t_y10, t_y50), use.names = TRUE, fill = TRUE)
write_table(t_year, "overview_temporal_year.csv")

# Month
t_m10 <- read_gap("temporal_overview_month_10km.csv")
t_m50 <- read_gap("temporal_overview_month_50km.csv")
t_month <- data.table::rbindlist(list(t_m10, t_m50), use.names = TRUE, fill = TRUE)
write_table(t_month, "overview_temporal_month.csv")

# ---- 3) Temporal recency rates (per cell) ------------------------------------
r10 <- read_gap("cell_recency_10km.csv")
r50 <- read_gap("cell_recency_50km.csv")

rec <- data.table::rbindlist(list(r10, r50), use.names = TRUE, fill = TRUE)

need_r <- c("grid", "basisofrecord", "eeacellcode", "staleness_months", "gap_stale_12m", "gap_stale_5y")
miss_r <- setdiff(need_r, names(rec))
if (length(miss_r)) stop("Missing columns in recency outputs: ", paste(miss_r, collapse = ", "))

# Force consistent types
rec[, staleness_months := as.numeric(staleness_months)]
rec[, gap_stale_12m := as.logical(gap_stale_12m)]
rec[, gap_stale_5y  := as.logical(gap_stale_5y)]

rec_overview <- rec[, .(
  n_cells = .N,
  pct_stale_12m = as.numeric(round(100 * mean(gap_stale_12m, na.rm = TRUE), 2)),
  pct_stale_5y  = as.numeric(round(100 * mean(gap_stale_5y,  na.rm = TRUE), 2)),
  median_staleness_months = as.numeric(stats::median(staleness_months, na.rm = TRUE)),
  p90_staleness_months = as.numeric(stats::quantile(staleness_months, probs = 0.90, na.rm = TRUE, names = FALSE))
), by = .(grid, basisofrecord)]

data.table::setorder(rec_overview, grid, basisofrecord)
write_table(rec_overview, "overview_temporal_recency_rates.csv")

# ---- 4) Taxonomic overview ----------------------------------------------------
tax_sum <- read_gap("taxonomic_gap_summary.csv")
write_table(tax_sum, "overview_taxonomic_summary.csv")

miss_thr_path <- file.path(p_gaps, "missing_threatened_taxa.csv")
if (file.exists(miss_thr_path)) {
  miss_thr <- data.table::fread(miss_thr_path)
  write_table(miss_thr, "overview_missing_threatened_taxa.csv")
} else {
  log_msg("No missing_threatened_taxa.csv found (skipping).")
}

log_msg("Phase 4 overview tables created successfully.")
