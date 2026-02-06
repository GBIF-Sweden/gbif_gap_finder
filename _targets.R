# _targets.R
# ==============================================================================
# GBIF Sweden Gap Analysis - Targets Pipeline
# ==============================================================================
# This file defines the complete analysis pipeline using the {targets} package.
#
# Usage:
#   tar_make()        # Run the full pipeline
#   tar_make(names)   # Run specific targets
#   tar_visnetwork()  # Visualize the pipeline
#   tar_read(name)    # Read a target's output
#   tar_load(name)    # Load a target into the environment
#
# To run from scratch:
#   tar_destroy()     # Clear all cached targets
#   tar_make()        # Rebuild everything
#
# Pipeline Phases:
#   1. Ingestion     - Download and process raw data (scripts 01-04)
#   2. Validation    - Check data integrity (script 05)
#   3. Summaries     - Create analysis-ready tables (scripts 06a, 06b)
#   4. Gap Analysis  - Identify gaps (scripts 07-09)
#   5. Integration   - Combined overview (script 10)
#   6. Reports       - Generate HTML reports (Rmd files)

library(targets)
library(tarchetypes)

# Source project setup (loads globals, config, etc.)
source("scripts/00_setup.R")

# Set target options
tar_option_set(
  packages = c(
    "here", "dplyr", "tidyr", "readr", "stringr", "purrr", "glue",
    "sf", "data.table", "fst", "cli", "lubridate", "tibble", "yaml"
  ),
  format = "rds",
  error = "continue"  # Continue pipeline even if a target errors
)

# Define the pipeline -----------------------------------------------------

list(
  
  # =========================================================================
  # PHASE 1: DATA INGESTION
  # =========================================================================
  
  # 1.1 Ingest EEA grids ----------------------------------------------------
  tar_target(
    grid_10km,
    {
      dir_grid <- here(cfg_get("paths.grid_10km_dir", "data_raw/eea_grid_10km"))
      file_grid <- cfg_get("files.grids.grid10km", "se_10km.shp")
      path <- file.path(dir_grid, file_grid)
      
      grid <- sf::st_read(path, quiet = TRUE) |>
        sf::st_transform(CRS_PROJECT)  # EPSG:3035
      
      # Handle geometry types
      sf::sf_use_s2(FALSE)
      geom_types <- unique(sf::st_geometry_type(grid))
      
      if (any(geom_types == "MULTISURFACE")) {
        grid <- grid |>
          sf::st_cast("GEOMETRYCOLLECTION", warn = FALSE) |>
          sf::st_collection_extract("POLYGON", warn = FALSE) |>
          sf::st_cast("MULTIPOLYGON", warn = FALSE)
      } else {
        grid <- sf::st_cast(grid, "MULTIPOLYGON", warn = FALSE)
      }
      
      # Repair invalid geometries
      if (sum(!sf::st_is_valid(grid)) > 0) {
        grid <- sf::st_make_valid(grid)
      }
      
      # Save to disk
      out_path <- here(cfg_get("paths.data_proc", "data_proc"), "grids_10km.gpkg")
      sf::st_write(grid, out_path, delete_dsn = TRUE, quiet = TRUE)
      
      grid
    }
  ),
  
  tar_target(
    grid_50km,
    {
      dir_grid <- here(cfg_get("paths.grid_50km_dir", "data_raw/eea_grid_50km"))
      file_grid <- cfg_get("files.grids.grid50km", "EEA_50km_grid_v2024.gpkg")
      path <- file.path(dir_grid, file_grid)
      
      grid <- sf::st_read(path, quiet = TRUE) |>
        sf::st_transform(CRS_PROJECT)  # EPSG:3035
      
      # Handle geometry types
      sf::sf_use_s2(FALSE)
      geom_types <- unique(sf::st_geometry_type(grid))
      
      if (any(geom_types == "MULTISURFACE")) {
        grid <- grid |>
          sf::st_cast("GEOMETRYCOLLECTION", warn = FALSE) |>
          sf::st_collection_extract("POLYGON", warn = FALSE) |>
          sf::st_cast("MULTIPOLYGON", warn = FALSE)
      } else {
        grid <- sf::st_cast(grid, "MULTIPOLYGON", warn = FALSE)
      }
      
      # Repair invalid geometries
      if (sum(!sf::st_is_valid(grid)) > 0) {
        grid <- sf::st_make_valid(grid)
      }
      
      # Save to disk
      out_path <- here(cfg_get("paths.data_proc", "data_proc"), "grids_50km.gpkg")
      sf::st_write(grid, out_path, delete_dsn = TRUE, quiet = TRUE)
      
      grid
    }
  ),
  
  # 1.2 Ingest taxonomy (Dyntaxa + Red List) --------------------------------
  tar_target(
    taxa_reference,
    {
      # Source the taxonomy ingestion script
      source(here("scripts", "03_ingest_taxonomy.R"), local = TRUE)
      
      # Return the saved file path
      here("data_proc", "dyntaxa_current.rds")
    },
    format = "file"
  ),
  
  # 1.3 Ingest GBIF occurrence cubes ---------------------------------------
  tar_target(
    cube_manifest,
    {
      cube_dir <- here(cfg_get("paths.gbif_cube_dir", "data_raw/gbif_occurrence_cubes"))
      data_proc_dir <- here(cfg_get("paths.data_proc", "data_proc"))
      out_cube_dir <- file.path(data_proc_dir, "cubes")
      dir.create(out_cube_dir, showWarnings = FALSE, recursive = TRUE)
      
      cube_map <- cfg_get("files.cube_files")
      
      manifest_list <- list()
      
      for (grid_name in names(cube_map)) {
        basis_list <- cube_map[[grid_name]]
        
        for (basis_name in names(basis_list)) {
          filename <- basis_list[[basis_name]]
          filepath <- file.path(cube_dir, filename)
          
          if (!file.exists(filepath)) {
            warning(paste("Cube file not found:", filepath))
            next
          }
          
          # Read and process
          dt <- data.table::fread(filepath, showProgress = FALSE, encoding = "UTF-8")
          
          # Standardize column names
          new_names <- names(dt) |>
            stringr::str_replace_all("\\s+", "_") |>
            stringr::str_replace_all("[^A-Za-z0-9_]", "") |>
            tolower()
          data.table::setnames(dt, new_names)
          
          # Add provenance
          dt[, `:=`(grid = grid_name, basisofrecord = basis_name, source_file = filename)]
          
          # Write processed cube
          out_path <- file.path(out_cube_dir, paste0("cube_", grid_name, "_", basis_name, ".fst"))
          fst::write_fst(as.data.frame(dt), out_path, compress = 50)
          
          # Store manifest entry
          manifest_list[[length(manifest_list) + 1]] <- tibble::tibble(
            grid = grid_name,
            basisOfRecord = basis_name,
            source_file = filename,
            processed_file = out_path,
            rows = nrow(dt),
            cols = ncol(dt)
          )
          
          rm(dt)
          invisible(gc())
        }
      }
      
      manifest_df <- dplyr::bind_rows(manifest_list)
      
      # Save manifest
      manifest_path <- file.path(data_proc_dir, "cube_manifest.csv")
      readr::write_csv(manifest_df, manifest_path)
      
      manifest_df
    }
  ),
  
  # =========================================================================
  # PHASE 2: VALIDATION
  # =========================================================================
  
  tar_target(
    validation_complete,
    {
      # Simple validation checks
      checks <- list(
        grid_10km_exists = file.exists(here("data_proc", "grids_10km.gpkg")),
        grid_50km_exists = file.exists(here("data_proc", "grids_50km.gpkg")),
        taxa_ref_exists = file.exists(here("data_proc", "dyntaxa_current.rds")),
        cube_manifest_exists = file.exists(here("data_proc", "cube_manifest.csv"))
      )
      
      all_passed <- all(unlist(checks))
      
      list(
        checks = checks,
        all_passed = all_passed,
        timestamp = Sys.time()
      )
    },
    # Depends on Phase 1 targets
    cue = tar_cue(depend = TRUE)
  ),
  
  # =========================================================================
  # PHASE 3: DERIVED SUMMARIES (Split into 06a and 06b)
  # =========================================================================
  
  # 3.1 Core summaries (fast, ~10-30 min) -----------------------------------
  tar_target(
    core_summaries,
    {
      # Source the core summaries script
      source(here("scripts", "06a_make_core_summaries.R"), local = TRUE)
      
      # Return list of created files
      derived_dir <- here("data_proc", "derived")
      core_files <- list.files(derived_dir, pattern = "^(cell|time|order|family|cube).*\\.csv$", 
                               full.names = TRUE)
      
      list(
        files = core_files,
        n_files = length(core_files),
        completed_at = Sys.time()
      )
    },
    cue = tar_cue(depend = TRUE)
  ),
  
  # 3.2 Species-level summaries (slower, ~30-60 min with fast mode) ---------
  tar_target(
    species_summaries,
    {
      # Source the species summaries script
      source(here("scripts", "06b_make_species_summaries_highmem.R"), local = TRUE)
      
      # Return list of created directories and files
      derived_dir <- here("data_proc", "derived")
      by_order_dir <- file.path(derived_dir, "by_order")
      by_family_dir <- file.path(derived_dir, "by_family")
      
      order_files <- if (dir.exists(by_order_dir)) {
        list.files(by_order_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
      } else character(0)
      
      family_files <- if (dir.exists(by_family_dir)) {
        list.files(by_family_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
      } else character(0)
      
      list(
        by_order_files = order_files,
        by_family_files = family_files,
        n_order_files = length(order_files),
        n_family_files = length(family_files),
        completed_at = Sys.time()
      )
    },
    cue = tar_cue(depend = TRUE)
  ),
  
  # Combined summaries target for downstream dependencies
  tar_target(
    derived_summaries,
    {
      list(
        core = core_summaries,
        species = species_summaries,
        all_complete = TRUE
      )
    }
  ),
  
  # =========================================================================
  # PHASE 4: GAP ANALYSIS
  # =========================================================================
  
  tar_target(
    spatial_gaps,
    {
      source(here("scripts", "07_spatial_gaps.R"), local = TRUE)
      
      gaps_dir <- here("data_proc", "gaps")
      list.files(gaps_dir, pattern = "^spatial_.*\\.csv$", full.names = TRUE)
    },
    cue = tar_cue(depend = TRUE)
  ),
  
  tar_target(
    temporal_gaps,
    {
      source(here("scripts", "08_temporal_gaps.R"), local = TRUE)
      
      gaps_dir <- here("data_proc", "gaps")
      list.files(gaps_dir, pattern = "^temporal_.*\\.csv$|^cell_recency.*\\.csv$", full.names = TRUE)
    },
    cue = tar_cue(depend = TRUE)
  ),
  
  tar_target(
    taxonomic_gaps,
    {
      source(here("scripts", "09_taxonomic_gaps.R"), local = TRUE)
      
      gaps_dir <- here("data_proc", "gaps")
      list.files(gaps_dir, pattern = "^taxonomic_.*\\.csv$", full.names = TRUE)
    },
    cue = tar_cue(depend = TRUE)
  ),
  
  # =========================================================================
  # PHASE 5: INTEGRATED OVERVIEW
  # =========================================================================
  
  tar_target(
    gap_overview,
    {
      source(here("scripts", "10_integrated_overview.R"), local = TRUE)
      
      tables_dir <- here("output", "tables")
      list(
        standard_tables = list.files(tables_dir, pattern = "\\.csv$", full.names = TRUE, recursive = FALSE),
        integrated_tables = list.files(file.path(tables_dir, "integrated"), pattern = "\\.csv$", full.names = TRUE)
      )
    },
    cue = tar_cue(depend = TRUE)
  ),
  
  # =========================================================================
  # PHASE 6: REPORTS (Optional - run manually with tar_make(names = ...))
  # =========================================================================
  
  # 01 - Data validation / sanity checks
  tar_render(
    report_sanity_checks,
    path = here("analysis", "01_sanity_checks.Rmd"),
    output_dir = here("analysis"),
    cue = tar_cue(mode = "never")
  ),
  
  # 02 - Spatial gap analysis
  tar_render(
    report_spatial_gaps,
    path = here("analysis", "02_spatial_gaps.Rmd"),
    output_dir = here("analysis"),
    cue = tar_cue(mode = "never")
  ),
  
  # 03 - Temporal gap analysis
  tar_render(
    report_temporal_gaps,
    path = here("analysis", "03_temporal_gaps.Rmd"),
    output_dir = here("analysis"),
    cue = tar_cue(mode = "never")
  ),
  
  # 04 - Taxonomic gap analysis
  tar_render(
    report_taxonomic_gaps,
    path = here("analysis", "04_taxonomic_gaps.Rmd"),
    output_dir = here("analysis"),
    cue = tar_cue(mode = "never")
  ),
  
  # 05 - Integrated final report
  tar_render(
    report_integrated,
    path = here("analysis", "05_integrated_report.Rmd"),
    output_dir = here("analysis"),
    cue = tar_cue(mode = "never")
  ),
  
  # =========================================================================
  # PIPELINE SUMMARY
  # =========================================================================
  
  tar_target(
    pipeline_summary,
    {
      list(
        completed_at = Sys.time(),
        validation = validation_complete,
        core_summaries = core_summaries,
        species_summaries = species_summaries,
        n_spatial_gap_files = length(spatial_gaps),
        n_temporal_gap_files = length(temporal_gaps),
        n_taxonomic_gap_files = length(taxonomic_gaps),
        n_overview_tables = length(unlist(gap_overview))
      )
    }
  )
)
