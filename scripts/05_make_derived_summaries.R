# scripts/05_make_derived_summaries.R
# Phase 2: Create analysis-ready derived datasets from processed cube files (.fst)
#
# Inputs:
#   - data_proc/cubes/*.fst  (created by scripts/03_ingest_gbif_cubes.R)
#
# Outputs (written to data_proc/derived/):
#     - cell_summary_10km.csv, cell_summary_50km.csv
#     - time_summary_10km.csv, time_summary_50km.csv
#     - species_summary_10km.csv, species_summary_50km.csv
#     - cube_key_summary.csv
#     - family_time_summary_10km.csv, family_time_summary_50km.csv
#     - order_time_summary_10km.csv,  order_time_summary_50km.csv
#
# OPTIONAL (smart species × time):
#     - species_time_summary_topN_10km.csv
#     - species_time_summary_topN_50km.csv
#
# Notes:
# - Reads only required columns from .fst (fast and memory-safe).
# - Aggregates per file first (small), then merges aggregates across files.
# - Species × time can get large; default is "top N species" (configurable).

source("scripts/00_setup.R")

# ---- Parameters --------------------------------------------------------------
# Species × time can get big. Default: compute only top N species (by total occurrences).
MAKE_SPECIES_TIME <- TRUE
SPECIES_TIME_TOP_N <- 2000   # increase if you want more; set to Inf for "all" (not recommended initially)

# ---- Paths (from your YAML) ---------------------------------------------------
data_proc_rel <- cfg_get("paths.data_proc", "data_proc")
p_data_proc   <- here::here(data_proc_rel)

p_cubes   <- file.path(p_data_proc, "cubes")
p_derived <- file.path(p_data_proc, "derived")
dir.create(p_derived, showWarnings = FALSE, recursive = TRUE)

if (!dir.exists(p_cubes)) stop("Cubes folder not found: ", p_cubes)

# ---- Dependencies -------------------------------------------------------------
if (!requireNamespace("fst", quietly = TRUE)) {
  stop("Package 'fst' is required (processed cubes are .fst). Install with install.packages('fst') and renv::snapshot().")
}

# ---- Helpers -----------------------------------------------------------------
safe_sum <- function(x) sum(as.numeric(x), na.rm = TRUE)

read_fst_cols <- function(path, cols) {
  meta <- fst::metadata_fst(path)
  available <- meta$columnNames
  cols2 <- intersect(cols, available)
  if (!("occurrences" %in% available)) stop("Missing 'occurrences' in: ", path)
  df <- fst::read_fst(path, columns = cols2)
  data.table::setDT(df)
  df
}

add_overall_basis <- function(dt, group_cols) {
  overall <- dt[, .(occurrences = safe_sum(occurrences)), by = c("grid", group_cols)]
  overall[, basisofrecord := "all"]
  data.table::rbindlist(list(dt, overall), use.names = TRUE, fill = TRUE)
}

write_split_by_grid <- function(dt, grid_value, filename) {
  out <- dt[grid == grid_value]
  out_path <- file.path(p_derived, filename)
  data.table::fwrite(out, out_path)
  log_msg("Wrote: ", out_path, " (rows=", nrow(out), ")")
}


# ---- Locate cube files --------------------------------------------------------
cube_files <- list.files(p_cubes, pattern = "\\.fst$", full.names = TRUE)
if (length(cube_files) == 0) stop("No .fst cube files found in: ", p_cubes)

log_msg("Found ", length(cube_files), " processed cube files (.fst).")


# ---- Pass 1: Build core summaries + family/order time -------------------------
cell_aggs     <- list()
time_aggs     <- list()
species_aggs  <- list()
family_time_aggs <- list()
order_time_aggs  <- list()
key_stats     <- list()

# Columns we *may* read. Missing columns are handled gracefully.
cols_core <- c(
  "grid", "basisofrecord", "source_file",
  "eeacellcode", "yearmonth",
  "specieskey", "species",
  "family", "familykey",
  "order", "orderkey",
  "occurrences"
)

for (f in cube_files) {
  log_msg("Summarizing (pass 1): ", basename(f))
  dt <- read_fst_cols(f, cols_core)
  
  # Provenance columns should exist; if not, create placeholders
  if (!("grid" %in% names(dt))) dt[, grid := NA_character_]
  if (!("basisofrecord" %in% names(dt))) dt[, basisofrecord := NA_character_]
  if (!("source_file" %in% names(dt))) dt[, source_file := basename(f)]
  
  have_cell   <- "eeacellcode" %in% names(dt)
  have_time   <- "yearmonth"   %in% names(dt)
  have_spkey  <- "specieskey"  %in% names(dt)
  have_sp     <- "species"     %in% names(dt)
  
  have_family <- "family" %in% names(dt) || "familykey" %in% names(dt)
  have_order  <- "order"  %in% names(dt) || "orderkey"  %in% names(dt)
  
  # --- File-level key coverage stats ------------------------------------------
  key_stats[[length(key_stats) + 1]] <- data.table::data.table(
    file = basename(f),
    grid = if (!all(is.na(dt$grid))) unique(dt$grid)[1] else NA_character_,
    basisOfRecord = if (!all(is.na(dt$basisofrecord))) unique(dt$basisofrecord)[1] else NA_character_,
    rows = nrow(dt),
    total_occurrences = safe_sum(dt$occurrences),
    n_cells = if (have_cell) data.table::uniqueN(dt$eeacellcode) else NA_integer_,
    n_months = if (have_time) data.table::uniqueN(dt$yearmonth) else NA_integer_,
    n_specieskeys = if (have_spkey) data.table::uniqueN(dt$specieskey) else NA_integer_
  )
  
  # --- Cell summary ------------------------------------------------------------
  if (have_cell) {
    cell_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                  by = .(grid, basisofrecord, eeacellcode)]
    cell_dt <- add_overall_basis(cell_dt, group_cols = "eeacellcode")
    cell_aggs[[length(cell_aggs) + 1]] <- cell_dt
  }
  
  # --- Time summary ------------------------------------------------------------
  if (have_time) {
    time_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                  by = .(grid, basisofrecord, yearmonth)]
    time_dt <- add_overall_basis(time_dt, group_cols = "yearmonth")
    time_aggs[[length(time_aggs) + 1]] <- time_dt
  }
  
  # --- Species summary (overall across time) ----------------------------------
  if (have_spkey) {
    if (have_sp) {
      sp_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                  by = .(grid, basisofrecord, specieskey, species)]
      sp_all <- dt[, .(occurrences = safe_sum(occurrences)),
                   by = .(grid, specieskey, species)]
      sp_all[, basisofrecord := "all"]
      sp_dt <- data.table::rbindlist(list(sp_dt, sp_all), use.names = TRUE, fill = TRUE)
    } else {
      sp_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                  by = .(grid, basisofrecord, specieskey)]
      sp_dt <- add_overall_basis(sp_dt, group_cols = "specieskey")
    }
    species_aggs[[length(species_aggs) + 1]] <- sp_dt
  }
  
  # --- Family × time summary ---------------------------------------------------
  # Prefer keys if available; also keep label if available.
  if (have_time && have_family) {
    if ("familykey" %in% names(dt) && "family" %in% names(dt)) {
      fam_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                   by = .(grid, basisofrecord, yearmonth, familykey, family)]
      # "all" rollup across basisOfRecord
      fam_all <- dt[, .(occurrences = safe_sum(occurrences)),
                    by = .(grid, yearmonth, familykey, family)]
      fam_all[, basisofrecord := "all"]
      fam_dt <- data.table::rbindlist(list(fam_dt, fam_all), use.names = TRUE, fill = TRUE)
    } else if ("family" %in% names(dt)) {
      fam_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                   by = .(grid, basisofrecord, yearmonth, family)]
      fam_dt <- add_overall_basis(fam_dt, group_cols = c("yearmonth", "family"))
    } else {
      fam_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                   by = .(grid, basisofrecord, yearmonth, familykey)]
      fam_dt <- add_overall_basis(fam_dt, group_cols = c("yearmonth", "familykey"))
    }
    family_time_aggs[[length(family_time_aggs) + 1]] <- fam_dt
  }
  
  # --- Order × time summary ----------------------------------------------------
  if (have_time && have_order) {
    if ("orderkey" %in% names(dt) && "order" %in% names(dt)) {
      ord_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                   by = .(grid, basisofrecord, yearmonth, orderkey, order)]
      ord_all <- dt[, .(occurrences = safe_sum(occurrences)),
                    by = .(grid, yearmonth, orderkey, order)]
      ord_all[, basisofrecord := "all"]
      ord_dt <- data.table::rbindlist(list(ord_dt, ord_all), use.names = TRUE, fill = TRUE)
    } else if ("order" %in% names(dt)) {
      ord_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                   by = .(grid, basisofrecord, yearmonth, order)]
      ord_dt <- add_overall_basis(ord_dt, group_cols = c("yearmonth", "order"))
    } else {
      ord_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                   by = .(grid, basisofrecord, yearmonth, orderkey)]
      ord_dt <- add_overall_basis(ord_dt, group_cols = c("yearmonth", "orderkey"))
    }
    order_time_aggs[[length(order_time_aggs) + 1]] <- ord_dt
  }
  
  rm(dt)
  invisible(gc())
}

log_msg("Combining per-file aggregates (pass 1)...")

# Combine core summaries
cell_all <- if (length(cell_aggs)) data.table::rbindlist(cell_aggs, use.names = TRUE, fill = TRUE) else NULL
if (!is.null(cell_all)) cell_all <- cell_all[, .(occurrences = safe_sum(occurrences)), by = .(grid, basisofrecord, eeacellcode)]

time_all <- if (length(time_aggs)) data.table::rbindlist(time_aggs, use.names = TRUE, fill = TRUE) else NULL
if (!is.null(time_all)) time_all <- time_all[, .(occurrences = safe_sum(occurrences)), by = .(grid, basisofrecord, yearmonth)]

species_all <- if (length(species_aggs)) data.table::rbindlist(species_aggs, use.names = TRUE, fill = TRUE) else NULL
if (!is.null(species_all)) {
  if ("species" %in% names(species_all)) {
    species_all <- species_all[, .(occurrences = safe_sum(occurrences)), by = .(grid, basisofrecord, specieskey, species)]
  } else {
    species_all <- species_all[, .(occurrences = safe_sum(occurrences)), by = .(grid, basisofrecord, specieskey)]
  }
}

# Combine rank × time summaries
family_time_all <- if (length(family_time_aggs)) data.table::rbindlist(family_time_aggs, use.names = TRUE, fill = TRUE) else NULL
if (!is.null(family_time_all)) {
  keys <- intersect(c("familykey", "family"), names(family_time_all))
  by_cols <- c("grid", "basisofrecord", "yearmonth", keys)
  family_time_all <- family_time_all[, .(occurrences = safe_sum(occurrences)), by = by_cols]
}

order_time_all <- if (length(order_time_aggs)) data.table::rbindlist(order_time_aggs, use.names = TRUE, fill = TRUE) else NULL
if (!is.null(order_time_all)) {
  keys <- intersect(c("orderkey", "order"), names(order_time_all))
  by_cols <- c("grid", "basisofrecord", "yearmonth", keys)
  order_time_all <- order_time_all[, .(occurrences = safe_sum(occurrences)), by = by_cols]
}

key_df <- data.table::rbindlist(key_stats, use.names = TRUE, fill = TRUE)
grid_levels <- unique(key_df$grid)
log_msg("Detected grids: ", paste(grid_levels, collapse = ", "))

# ---- Write pass 1 outputs -----------------------------------------------------
log_msg("Writing derived outputs to: ", p_derived)

# Core
if (!is.null(cell_all)) {
  if ("grid10km" %in% grid_levels) write_split_by_grid(cell_all, "grid10km", "cell_summary_10km.csv")
  if ("grid50km" %in% grid_levels) write_split_by_grid(cell_all, "grid50km", "cell_summary_50km.csv")
}
if (!is.null(time_all)) {
  if ("grid10km" %in% grid_levels) write_split_by_grid(time_all, "grid10km", "time_summary_10km.csv")
  if ("grid50km" %in% grid_levels) write_split_by_grid(time_all, "grid50km", "time_summary_50km.csv")
}
if (!is.null(species_all)) {
  if ("grid10km" %in% grid_levels) write_split_by_grid(species_all, "grid10km", "species_summary_10km.csv")
  if ("grid50km" %in% grid_levels) write_split_by_grid(species_all, "grid50km", "species_summary_50km.csv")
}

# Rank × time
if (!is.null(family_time_all)) {
  if ("grid10km" %in% grid_levels) write_split_by_grid(family_time_all, "grid10km", "family_time_summary_10km.csv")
  if ("grid50km" %in% grid_levels) write_split_by_grid(family_time_all, "grid50km", "family_time_summary_50km.csv")
} else {
  log_msg("No family/familykey columns found in cubes -> family_time_summary not written.")
}

if (!is.null(order_time_all)) {
  if ("grid10km" %in% grid_levels) write_split_by_grid(order_time_all, "grid10km", "order_time_summary_10km.csv")
  if ("grid50km" %in% grid_levels) write_split_by_grid(order_time_all, "grid50km", "order_time_summary_50km.csv")
} else {
  log_msg("No order/orderkey columns found in cubes -> order_time_summary not written.")
}

# Key stats
key_out <- file.path(p_derived, "cube_key_summary.csv")
data.table::fwrite(key_df, key_out)
log_msg("Wrote: ", key_out, " (rows=", nrow(key_df), ")")

# ---- Pass 2 (optional): Species × time (top N) --------------------------------
if (MAKE_SPECIES_TIME) {
  log_msg("Preparing species × time summary (top N = ", SPECIES_TIME_TOP_N, ") ...")
  
  if (is.null(species_all)) {
    log_msg("species_all not available -> skipping species × time.")
  } else {
    # Build whitelist of top species per grid using basisofrecord == "all"
    sp_all_only <- species_all[basisofrecord == "all"]
    if ("species" %in% names(sp_all_only)) {
      sp_all_only <- sp_all_only[, .(occurrences = safe_sum(occurrences)), by = .(grid, specieskey, species)]
    } else {
      sp_all_only <- sp_all_only[, .(occurrences = safe_sum(occurrences)), by = .(grid, specieskey)]
    }
    
    pick_top <- function(d, n) {
      d <- d[order(-occurrences)]
      if (is.infinite(n)) return(d$specieskey)
      head(d$specieskey, n)
    }
    
    top_keys_10 <- if ("grid10km" %in% unique(sp_all_only$grid)) pick_top(sp_all_only[grid == "grid10km"], SPECIES_TIME_TOP_N) else integer(0)
    top_keys_50 <- if ("grid50km" %in% unique(sp_all_only$grid)) pick_top(sp_all_only[grid == "grid50km"], SPECIES_TIME_TOP_N) else integer(0)
    
    top_keys <- list(grid10km = top_keys_10, grid50km = top_keys_50)
    
    species_time_aggs <- list()
    
    cols_species_time <- c("grid", "basisofrecord", "yearmonth", "specieskey", "species", "occurrences")
    
    for (f in cube_files) {
      dt <- read_fst_cols(f, cols_species_time)
      
      if (!("grid" %in% names(dt))) dt[, grid := NA_character_]
      if (!("basisofrecord" %in% names(dt))) dt[, basisofrecord := NA_character_]
      
      g <- if (!all(is.na(dt$grid))) unique(dt$grid)[1] else NA_character_
      if (is.na(g) || !(g %in% names(top_keys))) {
        rm(dt); invisible(gc()); next
      }
      
      keep <- top_keys[[g]]
      if (length(keep) == 0L) {
        rm(dt); invisible(gc()); next
      }
      
      # Filter early to keep things small
      if (!("specieskey" %in% names(dt)) || !("yearmonth" %in% names(dt))) {
        rm(dt); invisible(gc()); next
      }
      
      dt <- dt[specieskey %in% keep]
      
      if (nrow(dt) == 0L) {
        rm(dt); invisible(gc()); next
      }
      
      # Aggregate per file then combine later
      if ("species" %in% names(dt)) {
        spt <- dt[, .(occurrences = safe_sum(occurrences)),
                  by = .(grid, basisofrecord, yearmonth, specieskey, species)]
        spt_all <- dt[, .(occurrences = safe_sum(occurrences)),
                      by = .(grid, yearmonth, specieskey, species)]
        spt_all[, basisofrecord := "all"]
        spt <- data.table::rbindlist(list(spt, spt_all), use.names = TRUE, fill = TRUE)
      } else {
        spt <- dt[, .(occurrences = safe_sum(occurrences)),
                  by = .(grid, basisofrecord, yearmonth, specieskey)]
        spt <- add_overall_basis(spt, group_cols = c("yearmonth", "specieskey"))
      }
      
      species_time_aggs[[length(species_time_aggs) + 1]] <- spt
      
      rm(dt)
      invisible(gc())
    }
    
    if (length(species_time_aggs)) {
      sp_time <- data.table::rbindlist(species_time_aggs, use.names = TRUE, fill = TRUE)
      
      if ("species" %in% names(sp_time)) {
        sp_time <- sp_time[, .(occurrences = safe_sum(occurrences)),
                           by = .(grid, basisofrecord, yearmonth, specieskey, species)]
      } else {
        sp_time <- sp_time[, .(occurrences = safe_sum(occurrences)),
                           by = .(grid, basisofrecord, yearmonth, specieskey)]
      }
      
      if ("grid10km" %in% grid_levels) write_split_by_grid(sp_time, "grid10km", "species_time_summary_topN_10km.csv")
      if ("grid50km" %in% grid_levels) write_split_by_grid(sp_time, "grid50km", "species_time_summary_topN_50km.csv")
      
      log_msg("Species × time (top N) written.")
    } else {
      log_msg("Species × time (top N) produced no rows. (Check that cubes contain specieskey+yearmonth.)")
    }
  }
}

log_msg("Phase 2 derived datasets created successfully.")