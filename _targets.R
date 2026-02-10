# _targets.R
# ============================================================================
# gbifgaps — Targets Pipeline Definition
# ============================================================================
# Every target delegates to the numbered scripts via source().
# The scripts are the single source of truth for all logic.
# This file only defines the dependency graph and tracks outputs.
#
# Usage:
#   tar_make()              Run the full pipeline
#   tar_make(spatial_gaps)  Run up to a specific target
#   tar_visnetwork()        Visualise the DAG
#   tar_read(name)          Read a cached result
#
# To rebuild from scratch:
#   tar_destroy()
#   tar_make()
#
# Pipeline phases:
#   1. Ingestion    — scripts 02, 03, 04
#   2. Validation   — script 05
#   3. Summaries    — scripts 06a, 06b
#   4. Gap Analysis — scripts 07, 08, 09
#   5. Integration  — script 10
#   6. Gap App Prep  — script 11
#   7. Explorer Prep — script 12
#   7. Reports      — Rmd files (manual trigger)
# ============================================================================

library(targets)
library(tarchetypes)

# Source project setup (loads globals, config, packages)
source("scripts/00_setup.R")

# ============================================================================
# Target Options
# ============================================================================

tar_option_set(
  packages = c(
    "here", "dplyr", "tidyr", "readr", "stringr", "purrr", "glue",
    "sf", "data.table", "fst", "cli", "lubridate", "tibble", "yaml"
  ),
  format = "rds",
  error  = "continue"
)

# ============================================================================
# Pipeline Definition
# ============================================================================

list(

  # ==========================================================================
  # Phase 1: Data Ingestion
  # ==========================================================================

  # 1.1 Ingest EEA grids (script 02) ----------------------------------------
  #
  # Delegates entirely to 02_ingest_grids.R which reads raw shapefiles,
  # standardises CRS/geometry, and writes to data_proc/*.gpkg.
  # Returns the output file paths so targets can track them.

  tar_target(
    grids,
    {
      source(here("scripts", "02_ingest_grids.R"), local = TRUE)

      grid_files <- c(
        here("data_proc", "grids_10km.gpkg"),
        here("data_proc", "grids_50km.gpkg")
      )

      # Verify outputs exist
      stopifnot(all(file.exists(grid_files)))

      grid_files
    },
    format = "file"
  ),

  # 1.2 Ingest taxonomy backbone + red list (script 03) ----------------------
  #
  # Delegates to 03_ingest_taxonomy.R which reads DwC-A exports, joins
  # taxonomy + distribution, enriches with red list threat status, and
  # writes taxa_reference_current.rds.

  tar_target(
    taxa_reference,
    {
      source(here("scripts", "03_ingest_taxonomy.R"), local = TRUE)

      output_files <- c(
        here("data_proc", "taxa_reference_current.rds"),
        here("data_proc", "dyntaxa_backbone.csv")
      )

      stopifnot(all(file.exists(output_files)))

      output_files
    },
    format = "file"
  ),

  # 1.3 Ingest GBIF occurrence cubes (script 04) -----------------------------
  #
  # Delegates to 04_ingest_gbif_cubes.R which reads CSV cubes, standardises
  # column names, writes .fst files to data_proc/cubes/, and creates a
  # manifest CSV.

  tar_target(
    cube_manifest,
    {
      source(here("scripts", "04_ingest_gbif_cubes.R"), local = TRUE)

      manifest_path <- here("data_proc", "cube_manifest.csv")
      stopifnot(file.exists(manifest_path))

      manifest_path
    },
    format = "file"
  ),

  # ==========================================================================
  # Phase 2: Validation (script 05)
  # ==========================================================================
  #
  # Runs QA checks on all Phase 1 outputs and writes a Markdown report
  # to logs/. Depends on all ingestion targets.

  tar_target(
    validation_report,
    {
      # Declare dependencies (targets reads these even if not used in code)
      grids
      taxa_reference
      cube_manifest

      source(here("scripts", "05_validate_inputs.R"), local = TRUE)

      # Find the most recent validation report
      reports <- sort(
        list.files(
          here("logs"),
          pattern = "^validation_report_.*\\.md$",
          full.names = TRUE
        ),
        decreasing = TRUE
      )

      if (length(reports) == 0) {
        stop("Validation report not created")
      }

      reports[1]
    },
    format = "file"
  ),

  # ==========================================================================
  # Phase 3: Derived Summaries
  # ==========================================================================

  # 3.1 Core summaries — cell, time, order-level (script 06a) ----------------

  tar_target(
    core_summaries,
    {
      validation_report

      source(here("scripts", "06a_make_core_summaries.R"), local = TRUE)

      derived_dir <- here("data_proc", "derived")
      core_files  <- list.files(
        derived_dir,
        pattern = "^(cell|time|order|family|cube|grid_lookup).*\\.csv$",
        full.names = TRUE
      )

      if (length(core_files) == 0) {
        stop("No core summary files created")
      }

      core_files
    },
    format = "file"
  ),

  # 3.2 Species-level summaries (script 06b) ---------------------------------
  #
  # Uses the highmem variant by default. Set
  # parameters.processing.low_memory_mode: true in config.yml to use lowmem.

  tar_target(
    species_summaries,
    {
      core_summaries

      low_mem <- cfg_get("parameters.processing.low_memory_mode", FALSE)

      if (low_mem) {
        source(
          here("scripts", "06b_make_species_summaries_lowmem.R"),
          local = TRUE
        )
      } else {
        source(
          here("scripts", "06b_make_species_summaries_highmem.R"),
          local = TRUE
        )
      }

      derived_dir <- here("data_proc", "derived")
      species_files <- list.files(
        derived_dir,
        pattern = "\\.csv$",
        recursive = TRUE,
        full.names = TRUE
      )

      # Filter to by_order/ and by_family/ only
      species_files <- species_files[
        grepl("by_order|by_family", species_files)
      ]

      species_files
    },
    format = "file"
  ),

  # ==========================================================================
  # Phase 4: Gap Analysis
  # ==========================================================================

  # 4.1 Spatial gaps (script 07) ---------------------------------------------

  tar_target(
    spatial_gaps,
    {
      core_summaries

      source(here("scripts", "07_spatial_gaps.R"), local = TRUE)

      list.files(
        here("data_proc", "gaps"),
        pattern = "^spatial_.*\\.csv$",
        full.names = TRUE
      )
    },
    format = "file"
  ),

  # 4.2 Temporal gaps (script 08) --------------------------------------------

  tar_target(
    temporal_gaps,
    {
      core_summaries

      source(here("scripts", "08_temporal_gaps.R"), local = TRUE)

      list.files(
        here("data_proc", "gaps"),
        pattern = "^temporal_.*\\.csv$|^cell_recency.*\\.csv$",
        full.names = TRUE
      )
    },
    format = "file"
  ),

  # 4.3 Taxonomic gaps (script 09) -------------------------------------------

  tar_target(
    taxonomic_gaps,
    {
      core_summaries
      species_summaries

      source(here("scripts", "09_taxonomic_gaps.R"), local = TRUE)

      list.files(
        here("data_proc", "gaps"),
        pattern = "^taxonomic_.*\\.csv$",
        full.names = TRUE
      )
    },
    format = "file"
  ),

  # ==========================================================================
  # Phase 5: Integrated Overview (script 10)
  # ==========================================================================

  tar_target(
    gap_overview,
    {
      spatial_gaps
      temporal_gaps
      taxonomic_gaps

      source(here("scripts", "10_make_gap_overview.R"), local = TRUE)

      tables_dir <- here("output", "tables")
      all_tables <- list.files(
        tables_dir,
        pattern = "\\.csv$",
        recursive = TRUE,
        full.names = TRUE
      )

      if (length(all_tables) == 0) {
        stop("No overview tables created")
      }

      all_tables
    },
    format = "file"
  ),

  # ==========================================================================
  # Phase 6: Gap App Data Prep (script 11)
  # ==========================================================================

  tar_target(
    gap_app_data,
    {
      gap_overview

      source(here("scripts", "11_prepare_gap_app_data.R"), local = TRUE)

      shiny_path <- here("shiny_app", "gap_analysis", "data", "shiny_data.rds")
      stopifnot(file.exists(shiny_path))

      shiny_path
    },
    format = "file"
  ),

  # ==========================================================================
  # Phase 7: Explorer App Data Prep (script 12)
  # ==========================================================================

  tar_target(
    explorer_app_data,
    {
      gap_app_data

      source(here("scripts", "12_prepare_explorer_app_data.R"), local = TRUE)

      explorer_path <- here("shiny_app", "explorer", "data", "shiny_data.rds")
      stopifnot(file.exists(explorer_path))

      explorer_path
    },
    format = "file"
  ),

  # ==========================================================================
  # Phase 8: Reports (manual trigger — run with tar_make(report_*))
  # ==========================================================================
  #
  # Reports are set to cue = "never" so they don't run automatically.
  # Render individually: tar_make(names = report_sanity_checks)
  # Render all:          tar_make(names = starts_with("report_"))

  tar_render(
    report_sanity_checks,
    path       = here("analysis", "01_sanity_checks.Rmd"),
    output_dir = here("analysis"),
    cue        = tar_cue(mode = "never")
  ),

  tar_render(
    report_spatial_gaps,
    path       = here("analysis", "02_spatial_gaps.Rmd"),
    output_dir = here("analysis"),
    cue        = tar_cue(mode = "never")
  ),

  tar_render(
    report_temporal_gaps,
    path       = here("analysis", "03_temporal_gaps.Rmd"),
    output_dir = here("analysis"),
    cue        = tar_cue(mode = "never")
  ),

  tar_render(
    report_taxonomic_gaps,
    path       = here("analysis", "04_taxonomic_gaps.Rmd"),
    output_dir = here("analysis"),
    cue        = tar_cue(mode = "never")
  ),

  tar_render(
    report_integrated,
    path       = here("analysis", "05_integrated_report.Rmd"),
    output_dir = here("analysis"),
    cue        = tar_cue(mode = "never")
  ),

  # ==========================================================================
  # Pipeline Summary
  # ==========================================================================
  #
  # Collects metadata from all phases into a single summary object.

  tar_target(
    pipeline_summary,
    {
      list(
        completed_at           = Sys.time(),
        n_grid_files           = length(grids),
        n_taxa_files           = length(taxa_reference),
        n_core_summary_files   = length(core_summaries),
        n_species_files        = length(species_summaries),
        n_spatial_gap_files    = length(spatial_gaps),
        n_temporal_gap_files   = length(temporal_gaps),
        n_taxonomic_gap_files  = length(taxonomic_gaps),
        n_overview_tables      = length(gap_overview),
        shiny_data_ready       = file.exists(gap_app_data)
      )
    }
  )
)
