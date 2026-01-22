# scripts/07_define_spatial_gaps.R
# Phase 3: Spatial gap metrics (per EEA cell)
#
# Inputs (Phase 2):
#   - data_proc/derived/cell_summary_10km.csv
#   - data_proc/derived/cell_summary_50km.csv
#   - optional: grid_lookup_10km.csv / grid_lookup_50km.csv (not required here)
#
# Outputs (Phase 3):
#   - data_proc/gaps/spatial_gaps_10km.csv
#   - data_proc/gaps/spatial_gaps_50km.csv
#   - data_proc/gaps/spatial_thresholds_by_basis.csv

source("scripts/00_setup.R")

# ---- Dependencies -------------------------------------------------------------
pkgs <- c("data.table", "readr", "dplyr", "stringr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) stop("Missing package: ", p)

# ---- Paths -------------------------------------------------------------------
data_proc_rel <- cfg_get("paths.data_proc", "data_proc")
p_data_proc <- here::here(data_proc_rel)

p_derived <- file.path(p_data_proc, cfg_get("derived.dir", "derived"))
p_gaps <- file.path(p_data_proc, "gaps")
dir.create(p_gaps, showWarnings = FALSE, recursive = TRUE)

# ---- Parameters (gap definitions) --------------------------------------------
# Quantiles computed among cells with occurrences > 0 (per grid + basisofrecord)
Q_LOW <- c(0.05, 0.10)  # you can change later

# ---- Helper ------------------------------------------------------------------
read_cell_summary <- function(fname) {
  path <- file.path(p_derived, fname)
  if (!file.exists(path)) stop("Missing derived file: ", path)
  dt <- data.table::fread(path)
  # expected: grid, basisofrecord, eeacellcode, occurrences
  need <- c("grid", "basisofrecord", "eeacellcode", "occurrences")
  miss <- setdiff(need, names(dt))
  if (length(miss)) stop("Missing columns in ", fname, ": ", paste(miss, collapse = ", "))
  dt
}

compute_spatial_gaps <- function(dt, q_low = Q_LOW) {
  data.table::setDT(dt)
  dt[, occurrences := as.numeric(occurrences)]
  dt[is.na(occurrences), occurrences := 0]
  
  # Base gap: zero coverage
  dt[, gap_zero := (occurrences == 0)]
  
  # Quantile thresholds among non-zero cells, per grid + basisofrecord
  thresh <- dt[occurrences > 0,
               .(q05 = stats::quantile(occurrences, probs = 0.05, na.rm = TRUE, names = FALSE, type = 7),
                 q10 = stats::quantile(occurrences, probs = 0.10, na.rm = TRUE, names = FALSE, type = 7),
                 n_nonzero = .N),
               by = .(grid, basisofrecord)]
  
  # Join thresholds back
  out <- merge(dt, thresh, by = c("grid", "basisofrecord"), all.x = TRUE)
  
  # Low-coverage gaps (relative)
  out[, gap_low_q05 := (occurrences > 0 & !is.na(q05) & occurrences <= q05)]
  out[, gap_low_q10 := (occurrences > 0 & !is.na(q10) & occurrences <= q10)]
  
  # Mapping helper
  out[, log_occ := log10(occurrences + 1)]
  
  list(out = out, thresholds = thresh)
}

# ---- Run ---------------------------------------------------------------------
cell10_name <- cfg_get("derived.outputs.cell_summary_10km", "cell_summary_10km.csv")
cell50_name <- cfg_get("derived.outputs.cell_summary_50km", "cell_summary_50km.csv")

cell10 <- read_cell_summary(cell10_name)
cell50 <- read_cell_summary(cell50_name)

res10 <- compute_spatial_gaps(cell10)
res50 <- compute_spatial_gaps(cell50)

out10 <- file.path(p_gaps, "spatial_gaps_10km.csv")
out50 <- file.path(p_gaps, "spatial_gaps_50km.csv")
thr_out <- file.path(p_gaps, "spatial_thresholds_by_basis.csv")

data.table::fwrite(res10$out, out10)
data.table::fwrite(res50$out, out50)

thr_all <- data.table::rbindlist(list(res10$thresholds, res50$thresholds), use.names = TRUE, fill = TRUE)
data.table::fwrite(thr_all, thr_out)

log_msg("Wrote: ", out10)
log_msg("Wrote: ", out50)
log_msg("Wrote: ", thr_out)
log_msg("Phase 3 (spatial) complete.")
