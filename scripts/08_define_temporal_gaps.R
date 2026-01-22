# scripts/08_define_temporal_gaps.R
# Phase 3: Temporal gap metrics
#
# Inputs:
#   Phase 2:
#     - data_proc/derived/time_summary_10km.csv
#     - data_proc/derived/time_summary_50km.csv
#   Processed cubes:
#     - data_proc/cubes/*.fst  (to compute last_yearmonth per cell robustly)
#
# Outputs (written to data_proc/gaps/):
#   Overview (national-level, basisofrecord == "all"):
#     - temporal_overview_month_10km.csv
#     - temporal_overview_year_10km.csv
#     - temporal_overview_month_50km.csv
#     - temporal_overview_year_50km.csv
#
#   Recency / staleness per cell (all basisOfRecord + "all"):
#     - cell_recency_10km.csv
#     - cell_recency_50km.csv

source("scripts/00_setup.R")

# ---- Dependencies -------------------------------------------------------------
pkgs <- c("data.table", "readr", "dplyr", "stringr", "lubridate")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) stop("Missing package: ", p)
if (!requireNamespace("fst", quietly = TRUE)) stop("Missing package: fst (needed for processed cubes).")

# ---- Paths -------------------------------------------------------------------
data_proc_rel <- cfg_get("paths.data_proc", "data_proc")
p_data_proc <- here::here(data_proc_rel)

p_derived <- file.path(p_data_proc, cfg_get("derived.dir", "derived"))
p_cubes <- file.path(p_data_proc, "cubes")
p_gaps <- file.path(p_data_proc, "gaps")
dir.create(p_gaps, showWarnings = FALSE, recursive = TRUE)

# ---- Parameters --------------------------------------------------------------
STALE_MONTHS_12 <- 12
STALE_MONTHS_60 <- 60  # 5 years

# ---- Helpers -----------------------------------------------------------------
parse_yearmonth <- function(x_chr) {
  # expects YYYY-MM
  x_chr <- stringr::str_trim(as.character(x_chr))
  ok <- stringr::str_detect(x_chr, "^[0-9]{4}-[0-9]{2}$")
  out <- rep(as.Date(NA), length(x_chr))
  out[ok] <- as.Date(paste0(x_chr[ok], "-01"))
  out
}

safe_sum <- function(x) sum(as.numeric(x), na.rm = TRUE)

read_time_summary <- function(fname) {
  path <- file.path(p_derived, fname)
  if (!file.exists(path)) stop("Missing derived file: ", path)
  dt <- data.table::fread(path)
  need <- c("grid", "basisofrecord", "yearmonth", "occurrences")
  miss <- setdiff(need, names(dt))
  if (length(miss)) stop("Missing columns in ", fname, ": ", paste(miss, collapse = ", "))
  dt
}

read_fst_cols <- function(path, cols) {
  meta <- fst::metadata_fst(path)
  available <- meta$columnNames
  cols2 <- intersect(cols, available)
  if (!("occurrences" %in% available)) stop("Missing 'occurrences' in: ", path)
  df <- fst::read_fst(path, columns = cols2)
  data.table::setDT(df)
  df
}

make_overviews <- function(time_dt) {
  data.table::setDT(time_dt)
  time_dt[, ym := parse_yearmonth(yearmonth)]
  time_dt <- time_dt[!is.na(ym)]
  
  time_dt[, year := as.integer(format(ym, "%Y"))]
  time_dt[, month := as.integer(format(ym, "%m"))]
  
  # Use the "all" rollup
  all_dt <- time_dt[basisofrecord == "all"]
  
  by_month <- all_dt[, .(occurrences = safe_sum(occurrences)), by = .(grid, year, month)]
  by_year  <- all_dt[, .(occurrences = safe_sum(occurrences)), by = .(grid, year)]
  
  list(by_month = by_month, by_year = by_year)
}

compute_cell_recency <- function(grid_label, p_cubes_dir) {
  if (!dir.exists(p_cubes_dir)) stop("Cubes folder not found: ", p_cubes_dir)
  cube_files <- list.files(p_cubes_dir, pattern = "\\.fst$", full.names = TRUE)
  if (!length(cube_files)) stop("No .fst cube files found in: ", p_cubes_dir)
  
  cols <- c("grid", "basisofrecord", "eeacellcode", "yearmonth", "occurrences")
  parts <- list()
  
  for (f in cube_files) {
    dt <- read_fst_cols(f, cols)
    
    if (!("grid" %in% names(dt))) { rm(dt); next }
    dt <- dt[grid == grid_label]
    if (!nrow(dt)) { rm(dt); next }
    
    if (!all(c("eeacellcode", "yearmonth", "occurrences", "basisofrecord") %in% names(dt))) {
      rm(dt); next
    }
    
    dt <- dt[as.numeric(occurrences) > 0]
    if (!nrow(dt)) { rm(dt); next }
    
    dt[, ym := parse_yearmonth(yearmonth)]
    dt <- dt[!is.na(ym)]
    if (!nrow(dt)) { rm(dt); next }
    
    # basis-specific last month
    tmp <- dt[, .(
      last_ym = max(ym, na.rm = TRUE),
      total_occurrences = safe_sum(occurrences)
    ), by = .(grid, basisofrecord, eeacellcode)]
    
    # "all" rollup across basisOfRecord
    tmp_all <- dt[, .(
      last_ym = max(ym, na.rm = TRUE),
      total_occurrences = safe_sum(occurrences)
    ), by = .(grid, eeacellcode)]
    tmp_all[, basisofrecord := "all"]
    
    parts[[length(parts) + 1]] <- data.table::rbindlist(list(tmp, tmp_all), use.names = TRUE, fill = TRUE)
    
    rm(dt)
    invisible(gc())
  }
  
  if (!length(parts)) stop("No recency data produced for grid=", grid_label)
  
  out <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
  out <- out[, .(
    last_ym = max(last_ym, na.rm = TRUE),
    total_occurrences = safe_sum(total_occurrences)
  ), by = .(grid, basisofrecord, eeacellcode)]
  
  today <- Sys.Date()
  out[, staleness_months := (as.integer(format(today, "%Y")) - as.integer(format(last_ym, "%Y"))) * 12 +
        (as.integer(format(today, "%m")) - as.integer(format(last_ym, "%m")))]
  out[, gap_stale_12m := (staleness_months > STALE_MONTHS_12)]
  out[, gap_stale_5y  := (staleness_months > STALE_MONTHS_60)]
  
  out
}

# ---- Run ---------------------------------------------------------------------
time10_name <- cfg_get("derived.outputs.time_summary_10km", "time_summary_10km.csv")
time50_name <- cfg_get("derived.outputs.time_summary_50km", "time_summary_50km.csv")

time10 <- read_time_summary(time10_name)
time50 <- read_time_summary(time50_name)

ov10 <- make_overviews(time10)
ov50 <- make_overviews(time50)

# Write separate outputs (cleaner)
out_m10 <- file.path(p_gaps, "temporal_overview_month_10km.csv")
out_y10 <- file.path(p_gaps, "temporal_overview_year_10km.csv")
out_m50 <- file.path(p_gaps, "temporal_overview_month_50km.csv")
out_y50 <- file.path(p_gaps, "temporal_overview_year_50km.csv")

data.table::fwrite(ov10$by_month, out_m10)
data.table::fwrite(ov10$by_year,  out_y10)
data.table::fwrite(ov50$by_month, out_m50)
data.table::fwrite(ov50$by_year,  out_y50)

log_msg("Wrote: ", out_m10)
log_msg("Wrote: ", out_y10)
log_msg("Wrote: ", out_m50)
log_msg("Wrote: ", out_y50)

# Recency per cell
rec10 <- compute_cell_recency("grid10km", p_cubes)
rec50 <- compute_cell_recency("grid50km", p_cubes)

out_r10 <- file.path(p_gaps, "cell_recency_10km.csv")
out_r50 <- file.path(p_gaps, "cell_recency_50km.csv")
data.table::fwrite(rec10, out_r10)
data.table::fwrite(rec50, out_r50)

log_msg("Wrote: ", out_r10)
log_msg("Wrote: ", out_r50)
log_msg("Phase 3 (temporal) complete.")
