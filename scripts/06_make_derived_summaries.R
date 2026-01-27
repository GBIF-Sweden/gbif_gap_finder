# scripts/06_make_derived_summaries.R
# ==============================================================================
# Create Analysis-Ready Derived Summaries
# ==============================================================================
# This script:
# - Reads processed cube files from data_proc/cubes/
# - Creates aggregated summaries by cell, time, and species
# - Generates taxonomic rank × time summaries (family, order)
# - Optionally creates species × time for top N species
# - Writes all outputs to data_proc/derived/
#
# Memory strategy: Reads only required columns, aggregates per-file,
# then combines aggregates (not raw data)

library(here)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(data.table)
library(glue)
library(cli)

source(here("scripts", "00_setup.R"))

# Configuration -----------------------------------------------------------

# Species × time can get very large
# Strategy: Compute only for top N species by occurrence count
MAKE_SPECIES_TIME <- TRUE
SPECIES_TIME_TOP_N <- 2000  # Set to Inf for all (not recommended initially)

# Paths -------------------------------------------------------------------
p_cubes <- here(p_data_proc, "cubes")
p_derived <- here(p_data_proc, "derived")

# Create output directory
dir.create(p_derived, showWarnings = FALSE, recursive = TRUE)

# Validate inputs ---------------------------------------------------------
if (!dir.exists(p_cubes)) {
  cli_abort("Cubes directory not found: {.path {p_cubes}}")
}

# Check for fst package
if (!requireNamespace("fst", quietly = TRUE)) {
  cli_abort(c(
    "Package {.pkg fst} is required for reading cube files",
    "i" = "Install with: {.code install.packages('fst')}",
    "i" = "Then run: {.code renv::snapshot()}"
  ))
}

# Helper functions --------------------------------------------------------

#' Safely sum numeric values
#' @param x Numeric vector
#' @return Sum with NA handling
safe_sum <- function(x) {
  sum(as.numeric(x), na.rm = TRUE)
}

#' Read selected columns from fst file
#' @param path Path to .fst file
#' @param cols Character vector of columns to read
#' @return data.table with selected columns
read_fst_cols <- function(path, cols) {
  meta <- fst::metadata_fst(path)
  available_cols <- meta$columnNames
  
  # Only read columns that exist
  cols_to_read <- intersect(cols, available_cols)
  
  # Occurrences column is required
  if (!("occurrences" %in% available_cols)) {
    cli_abort("Column 'occurrences' missing in: {.path {basename(path)}}")
  }
  
  df <- fst::read_fst(path, columns = cols_to_read)
  setDT(df)
  df
}

#' Add "all" basis of record rollup
#' @param dt data.table with basisofrecord column
#' @param group_cols Character vector of grouping columns
#' @return data.table with added "all" rows
add_overall_basis <- function(dt, group_cols) {
  overall <- dt[, .(occurrences = safe_sum(occurrences)), 
                by = c("grid", group_cols)]
  overall[, basisofrecord := "all"]
  
  rbindlist(list(dt, overall), use.names = TRUE, fill = TRUE)
}

#' Write grid-specific subset to CSV
#' @param dt data.table with grid column
#' @param grid_value Grid identifier to filter
#' @param filename Output filename
write_grid_subset <- function(dt, grid_value, filename) {
  subset <- dt[grid == grid_value]
  out_path <- here(p_derived, filename)
  
  fwrite(subset, out_path)
  
  cli_alert_success(
    "{filename}: {scales::comma(nrow(subset))} rows"
  )
}

# Locate cube files -------------------------------------------------------
cli_h2("Locating Cube Files")

cube_files <- list.files(p_cubes, pattern = "\\.fst$", full.names = TRUE)

if (length(cube_files) == 0) {
  cli_abort("No .fst cube files found in: {.path {p_cubes}}")
}

cli_alert_info("Found {length(cube_files)} cube file{?s}")

# Define columns to read --------------------------------------------------
cols_core <- c(
  # Provenance
  "grid", "basisofrecord", "source_file",
  
  # Spatial
  "eeacellcode",
  
  # Temporal  
  "yearmonth",
  
  # Taxonomic - species
  "specieskey", "species",
  
  # Taxonomic - family
  "family", "familykey",
  
  # Taxonomic - order
  "order", "orderkey",
  
  # Metrics
  "occurrences"
)

# Initialize aggregation lists --------------------------------------------
cell_aggs <- list()
time_aggs <- list()
species_aggs <- list()
family_time_aggs <- list()
order_time_aggs <- list()
key_stats <- list()

# Pass 1: Build core summaries --------------------------------------------
cli_h2("Pass 1: Creating Core Summaries")

cli_progress_bar(
  "Processing cube files",
  total = length(cube_files),
  clear = FALSE
)

for (i in seq_along(cube_files)) {
  f <- cube_files[i]
  cli_progress_update()
  
  # Read data
  dt <- read_fst_cols(f, cols_core)
  
  # Ensure provenance columns exist
  if (!("grid" %in% names(dt))) dt[, grid := NA_character_]
  if (!("basisofrecord" %in% names(dt))) dt[, basisofrecord := NA_character_]
  if (!("source_file" %in% names(dt))) dt[, source_file := basename(f)]
  
  # Check which columns are available
  have_cell <- "eeacellcode" %in% names(dt)
  have_time <- "yearmonth" %in% names(dt)
  have_spkey <- "specieskey" %in% names(dt)
  have_sp <- "species" %in% names(dt)
  have_family <- any(c("family", "familykey") %in% names(dt))
  have_order <- any(c("order", "orderkey") %in% names(dt))
  
  # File-level statistics -----------------------------------------------
  key_stats[[length(key_stats) + 1]] <- data.table(
    file = basename(f),
    grid = if (!all(is.na(dt$grid))) unique(dt$grid)[1] else NA_character_,
    basisOfRecord = if (!all(is.na(dt$basisofrecord))) unique(dt$basisofrecord)[1] else NA_character_,
    rows = nrow(dt),
    total_occurrences = safe_sum(dt$occurrences),
    n_cells = if (have_cell) uniqueN(dt$eeacellcode) else NA_integer_,
    n_months = if (have_time) uniqueN(dt$yearmonth) else NA_integer_,
    n_specieskeys = if (have_spkey) uniqueN(dt$specieskey) else NA_integer_
  )
  
  # Cell summary --------------------------------------------------------
  if (have_cell) {
    cell_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                  by = .(grid, basisofrecord, eeacellcode)]
    cell_dt <- add_overall_basis(cell_dt, "eeacellcode")
    cell_aggs[[length(cell_aggs) + 1]] <- cell_dt
  }
  
  # Time summary --------------------------------------------------------
  if (have_time) {
    time_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                  by = .(grid, basisofrecord, yearmonth)]
    time_dt <- add_overall_basis(time_dt, "yearmonth")
    time_aggs[[length(time_aggs) + 1]] <- time_dt
  }
  
  # Species summary -----------------------------------------------------
  if (have_spkey) {
    if (have_sp) {
      # With species names
      sp_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                  by = .(grid, basisofrecord, specieskey, species)]
      sp_all <- dt[, .(occurrences = safe_sum(occurrences)),
                   by = .(grid, specieskey, species)]
      sp_all[, basisofrecord := "all"]
      sp_dt <- rbindlist(list(sp_dt, sp_all), use.names = TRUE, fill = TRUE)
    } else {
      # Keys only
      sp_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                  by = .(grid, basisofrecord, specieskey)]
      sp_dt <- add_overall_basis(sp_dt, "specieskey")
    }
    species_aggs[[length(species_aggs) + 1]] <- sp_dt
  }
  
  # Family × time summary -----------------------------------------------
  if (have_time && have_family) {
    if (all(c("familykey", "family") %in% names(dt))) {
      # With both key and name
      fam_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                   by = .(grid, basisofrecord, yearmonth, familykey, family)]
      fam_all <- dt[, .(occurrences = safe_sum(occurrences)),
                    by = .(grid, yearmonth, familykey, family)]
      fam_all[, basisofrecord := "all"]
      fam_dt <- rbindlist(list(fam_dt, fam_all), use.names = TRUE, fill = TRUE)
    } else if ("family" %in% names(dt)) {
      # Name only
      fam_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                   by = .(grid, basisofrecord, yearmonth, family)]
      fam_dt <- add_overall_basis(fam_dt, c("yearmonth", "family"))
    } else {
      # Key only
      fam_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                   by = .(grid, basisofrecord, yearmonth, familykey)]
      fam_dt <- add_overall_basis(fam_dt, c("yearmonth", "familykey"))
    }
    family_time_aggs[[length(family_time_aggs) + 1]] <- fam_dt
  }
  
  # Order × time summary ------------------------------------------------
  if (have_time && have_order) {
    if (all(c("orderkey", "order") %in% names(dt))) {
      # With both key and name
      ord_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                   by = .(grid, basisofrecord, yearmonth, orderkey, order)]
      ord_all <- dt[, .(occurrences = safe_sum(occurrences)),
                    by = .(grid, yearmonth, orderkey, order)]
      ord_all[, basisofrecord := "all"]
      ord_dt <- rbindlist(list(ord_dt, ord_all), use.names = TRUE, fill = TRUE)
    } else if ("order" %in% names(dt)) {
      # Name only
      ord_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                   by = .(grid, basisofrecord, yearmonth, order)]
      ord_dt <- add_overall_basis(ord_dt, c("yearmonth", "order"))
    } else {
      # Key only
      ord_dt <- dt[, .(occurrences = safe_sum(occurrences)),
                   by = .(grid, basisofrecord, yearmonth, orderkey)]
      ord_dt <- add_overall_basis(ord_dt, c("yearmonth", "orderkey"))
    }
    order_time_aggs[[length(order_time_aggs) + 1]] <- ord_dt
  }
  
  # Clean up
  rm(dt)
  invisible(gc())
}

cli_progress_done()

# Combine per-file aggregates ---------------------------------------------
cli_h2("Combining Aggregates")

cli_alert_info("Merging cell summaries...")
cell_all <- if (length(cell_aggs) > 0) {
  rbindlist(cell_aggs, use.names = TRUE, fill = TRUE) |>
    _[, .(occurrences = safe_sum(occurrences)), by = .(grid, basisofrecord, eeacellcode)]
} else NULL

cli_alert_info("Merging time summaries...")
time_all <- if (length(time_aggs) > 0) {
  rbindlist(time_aggs, use.names = TRUE, fill = TRUE) |>
    _[, .(occurrences = safe_sum(occurrences)), by = .(grid, basisofrecord, yearmonth)]
} else NULL

cli_alert_info("Merging species summaries...")
species_all <- if (length(species_aggs) > 0) {
  combined <- rbindlist(species_aggs, use.names = TRUE, fill = TRUE)
  if ("species" %in% names(combined)) {
    combined[, .(occurrences = safe_sum(occurrences)), 
             by = .(grid, basisofrecord, specieskey, species)]
  } else {
    combined[, .(occurrences = safe_sum(occurrences)), 
             by = .(grid, basisofrecord, specieskey)]
  }
} else NULL

cli_alert_info("Merging family × time...")
family_time_all <- if (length(family_time_aggs) > 0) {
  combined <- rbindlist(family_time_aggs, use.names = TRUE, fill = TRUE)
  keys <- intersect(c("familykey", "family"), names(combined))
  by_cols <- c("grid", "basisofrecord", "yearmonth", keys)
  combined[, .(occurrences = safe_sum(occurrences)), by = by_cols]
} else NULL

cli_alert_info("Merging order × time...")
order_time_all <- if (length(order_time_aggs) > 0) {
  combined <- rbindlist(order_time_aggs, use.names = TRUE, fill = TRUE)
  keys <- intersect(c("orderkey", "order"), names(combined))
  by_cols <- c("grid", "basisofrecord", "yearmonth", keys)
  combined[, .(occurrences = safe_sum(occurrences)), by = by_cols]
} else NULL

# Combine key statistics
key_df <- rbindlist(key_stats, use.names = TRUE, fill = TRUE)
grid_levels <- unique(key_df$grid) |> na.omit()

cli_alert_info("Detected grid levels: {paste(grid_levels, collapse = ', ')}")

# Write outputs -----------------------------------------------------------
cli_h2("Writing Derived Summaries")

# Core summaries
if (!is.null(cell_all)) {
  if ("grid10km" %in% grid_levels) {
    write_grid_subset(cell_all, "grid10km", "cell_summary_10km.csv")
  }
  if ("grid50km" %in% grid_levels) {
    write_grid_subset(cell_all, "grid50km", "cell_summary_50km.csv")
  }
}

if (!is.null(time_all)) {
  if ("grid10km" %in% grid_levels) {
    write_grid_subset(time_all, "grid10km", "time_summary_10km.csv")
  }
  if ("grid50km" %in% grid_levels) {
    write_grid_subset(time_all, "grid50km", "time_summary_50km.csv")
  }
}

if (!is.null(species_all)) {
  if ("grid10km" %in% grid_levels) {
    write_grid_subset(species_all, "grid10km", "species_summary_10km.csv")
  }
  if ("grid50km" %in% grid_levels) {
    write_grid_subset(species_all, "grid50km", "species_summary_50km.csv")
  }
}

# Taxonomic rank × time summaries
if (!is.null(family_time_all)) {
  if ("grid10km" %in% grid_levels) {
    write_grid_subset(family_time_all, "grid10km", "family_time_summary_10km.csv")
  }
  if ("grid50km" %in% grid_levels) {
    write_grid_subset(family_time_all, "grid50km", "family_time_summary_50km.csv")
  }
} else {
  cli_alert_info("No family data found - skipping family × time summaries")
}

if (!is.null(order_time_all)) {
  if ("grid10km" %in% grid_levels) {
    write_grid_subset(order_time_all, "grid10km", "order_time_summary_10km.csv")
  }
  if ("grid50km" %in% grid_levels) {
    write_grid_subset(order_time_all, "grid50km", "order_time_summary_50km.csv")
  }
} else {
  cli_alert_info("No order data found - skipping order × time summaries")
}

# Cube key summary
key_out <- here(p_derived, "cube_key_summary.csv")
fwrite(key_df, key_out)
cli_alert_success("cube_key_summary.csv: {scales::comma(nrow(key_df))} rows")

# Pass 2: Species × time (optional) ---------------------------------------
if (MAKE_SPECIES_TIME) {
  cli_h2("Pass 2: Species × Time (Top N)")
  
  if (is.null(species_all)) {
    cli_alert_warning("Species data not available - skipping species × time")
  } else {
    cli_alert_info("Computing for top {SPECIES_TIME_TOP_N} species per grid")
    
    # Identify top N species per grid
    sp_all_only <- species_all[basisofrecord == "all"]
    
    if ("species" %in% names(sp_all_only)) {
      sp_all_only <- sp_all_only[, .(occurrences = safe_sum(occurrences)), 
                                  by = .(grid, specieskey, species)]
    } else {
      sp_all_only <- sp_all_only[, .(occurrences = safe_sum(occurrences)), 
                                  by = .(grid, specieskey)]
    }
    
    # Function to pick top N species
    pick_top_n <- function(dt, n) {
      dt <- dt[order(-occurrences)]
      if (is.infinite(n)) return(dt$specieskey)
      head(dt$specieskey, n)
    }
    
    # Get top species for each grid
    top_keys <- list(
      grid10km = if ("grid10km" %in% grid_levels) {
        pick_top_n(sp_all_only[grid == "grid10km"], SPECIES_TIME_TOP_N)
      } else integer(0),
      grid50km = if ("grid50km" %in% grid_levels) {
        pick_top_n(sp_all_only[grid == "grid50km"], SPECIES_TIME_TOP_N)
      } else integer(0)
    )
    
    cli_alert_info(
      "Top species counts: 10km={length(top_keys$grid10km)}, 50km={length(top_keys$grid50km)}"
    )
    
    # Aggregate species × time for top species
    species_time_aggs <- list()
    cols_species_time <- c("grid", "basisofrecord", "yearmonth", 
                           "specieskey", "species", "occurrences")
    
    cli_progress_bar(
      "Building species × time",
      total = length(cube_files),
      clear = FALSE
    )
    
    for (f in cube_files) {
      cli_progress_update()
      
      dt <- read_fst_cols(f, cols_species_time)
      
      # Ensure provenance columns
      if (!("grid" %in% names(dt))) dt[, grid := NA_character_]
      if (!("basisofrecord" %in% names(dt))) dt[, basisofrecord := NA_character_]
      
      # Get grid level
      grid_level <- if (!all(is.na(dt$grid))) unique(dt$grid)[1] else NA_character_
      
      # Skip if grid unknown or no top species for this grid
      if (is.na(grid_level) || !(grid_level %in% names(top_keys))) {
        rm(dt)
        invisible(gc())
        next
      }
      
      keep_species <- top_keys[[grid_level]]
      if (length(keep_species) == 0) {
        rm(dt)
        invisible(gc())
        next
      }
      
      # Check required columns
      if (!all(c("specieskey", "yearmonth") %in% names(dt))) {
        rm(dt)
        invisible(gc())
        next
      }
      
      # Filter to top species only
      dt <- dt[specieskey %in% keep_species]
      
      if (nrow(dt) == 0) {
        rm(dt)
        invisible(gc())
        next
      }
      
      # Aggregate
      if ("species" %in% names(dt)) {
        spt <- dt[, .(occurrences = safe_sum(occurrences)),
                  by = .(grid, basisofrecord, yearmonth, specieskey, species)]
        spt_all <- dt[, .(occurrences = safe_sum(occurrences)),
                      by = .(grid, yearmonth, specieskey, species)]
        spt_all[, basisofrecord := "all"]
        spt <- rbindlist(list(spt, spt_all), use.names = TRUE, fill = TRUE)
      } else {
        spt <- dt[, .(occurrences = safe_sum(occurrences)),
                  by = .(grid, basisofrecord, yearmonth, specieskey)]
        spt <- add_overall_basis(spt, c("yearmonth", "specieskey"))
      }
      
      species_time_aggs[[length(species_time_aggs) + 1]] <- spt
      
      rm(dt)
      invisible(gc())
    }
    
    cli_progress_done()
    
    # Combine and write
    if (length(species_time_aggs) > 0) {
      sp_time <- rbindlist(species_time_aggs, use.names = TRUE, fill = TRUE)
      
      if ("species" %in% names(sp_time)) {
        sp_time <- sp_time[, .(occurrences = safe_sum(occurrences)),
                           by = .(grid, basisofrecord, yearmonth, specieskey, species)]
      } else {
        sp_time <- sp_time[, .(occurrences = safe_sum(occurrences)),
                           by = .(grid, basisofrecord, yearmonth, specieskey)]
      }
      
      if ("grid10km" %in% grid_levels) {
        write_grid_subset(sp_time, "grid10km", "species_time_summary_topN_10km.csv")
      }
      if ("grid50km" %in% grid_levels) {
        write_grid_subset(sp_time, "grid50km", "species_time_summary_topN_50km.csv")
      }
    } else {
      cli_alert_warning("No species × time data generated")
    }
  }
}

# Summary -----------------------------------------------------------------
cli_h2("Summary")

summary_info <- tribble(
  ~output, ~status,
  "Cell summaries", if (!is.null(cell_all)) "✅ Created" else "❌ No data",
  "Time summaries", if (!is.null(time_all)) "✅ Created" else "❌ No data",
  "Species summaries", if (!is.null(species_all)) "✅ Created" else "❌ No data",
  "Family × time", if (!is.null(family_time_all)) "✅ Created" else "⚠️ Skipped",
  "Order × time", if (!is.null(order_time_all)) "✅ Created" else "⚠️ Skipped",
  "Species × time (top N)", if (MAKE_SPECIES_TIME && !is.null(species_all)) "✅ Created" else "⚠️ Skipped",
  "Cube key summary", "✅ Created"
)

print(summary_info)

cli_alert_success("Derived summaries complete!")
cli_alert_info("Output location: {.path {p_derived}}")
