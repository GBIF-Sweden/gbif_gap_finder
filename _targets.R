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
# Pipeline phases:
#   1. Ingestion    — scripts 01, 02, 03, 04
#   2. Validation   — script 05
#   3. Summaries    — scripts 06a, 06b
#   4. Gap Analysis — scripts 07, 08, 09a, 09b, 09c
#   5. Integration  — script 10
#   6. Gap App Prep  — script 11
#   7. Explorer Prep — script 12
#   8. Reports      — Rmd files (manual trigger)
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
    # Core (matches required_packages in R/packages.R)
    "here", "dplyr", "tidyr", "readr", "stringr", "purrr", "glue",
    "tibble", "lubridate", "scales",
    "sf", "data.table", "cli", "yaml",
    # Script-specific (loaded by individual scripts but needed by targets workers)
    "arrow"
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

  # 1.0 Download raw data — taxonomy, red list, invasives, sensitive, admin (script 01)
  tar_target(
    raw_data,
    {
      source(here("scripts", "01_download_raw_data.R"), local = TRUE)
      metadata_path <- here(p_data_raw, "download_metadata.json")
      stopifnot(file.exists(metadata_path))
      metadata_path
    },
    format = "file"
  ),

  # 1.1 Ingest EEA grids (script 02)
  tar_target(
    grids,
    {
      source(here("scripts", "02_ingest_grids.R"), local = TRUE)
      grid_files <- c(
        here(p_data_proc, "grids_10km.gpkg"),
        here(p_data_proc, "grids_50km.gpkg")
      )
      stopifnot(all(file.exists(grid_files)))
      grid_files
    },
    format = "file"
  ),

  # 1.2 Ingest taxonomy backbone + red list + invasives + sensitive (script 03)
  tar_target(
    taxa_reference,
    {
      raw_data  # depends on download step
      source(here("scripts", "03_ingest_taxonomy.R"), local = TRUE)
      output_files <- c(
        here(p_data_proc, "taxa_reference_current.rds"),
        here(p_data_proc, "taxonomy_backbone.csv")
      )
      stopifnot(all(file.exists(output_files)))
      output_files
    },
    format = "file"
  ),

  # 1.3 Convert GBIF cubes to parquet (script 04)
  tar_target(
    cube_parquet,
    {
      source(here("scripts", "04_convert_cubes_parquet.R"), local = TRUE)
      manifest_path <- here(p_data_proc, "cubes", "cube_manifest.csv")
      stopifnot(file.exists(manifest_path))
      manifest_path
    },
    format = "file"
  ),

  # ==========================================================================
  # Phase 2: Validation (script 05)
  # ==========================================================================

  tar_target(
    validation_report,
    {
      grids; taxa_reference; cube_parquet
      source(here("scripts", "05_validate_inputs.R"), local = TRUE)
      reports <- sort(
        list.files(here("logs"), pattern = "^validation_report_.*\\.md$", full.names = TRUE),
        decreasing = TRUE
      )
      if (length(reports) == 0) stop("Validation report not created")
      reports[1]
    },
    format = "file"
  ),

  # ==========================================================================
  # Phase 3: Derived Summaries
  # ==========================================================================

  # 3.1 Core summaries + publisher summaries (script 06a)
  tar_target(
    core_summaries,
    {
      validation_report
      source(here("scripts", "06a_make_core_summaries.R"), local = TRUE)
      # Match 06a outputs only — exclude per-scope files produced by 09c
      # which live in the same directory (cell_summary_all_10km.csv etc.)
      all_csvs <- list.files(
        here(p_data_proc, "derived"),
        pattern = "^(cell|time|order|family|cube|grid_lookup|publisher|published).*\\.csv$",
        full.names = TRUE
      )
      core_files <- all_csvs[!grepl(
        "_(all|dyntaxa|threatened|invasive|sensitive)_(10|50)km\\.csv$",
        all_csvs
      )]
      if (length(core_files) == 0) stop("No core summary files created")
      core_files
    },
    format = "file"
  ),

  # 3.2 Species-level summaries (script 06b)
  tar_target(
    species_summaries,
    {
      core_summaries
      source(here("scripts", "06b_make_species_summaries.R"), local = TRUE)
      species_files <- list.files(
        here(p_data_proc, "derived"),
        pattern = "\\.csv$", recursive = TRUE, full.names = TRUE
      )
      species_files[grepl("by_order|by_family", species_files)]
    },
    format = "file"
  ),

  # ==========================================================================
  # Phase 4: Gap Analysis
  # ==========================================================================

  tar_target(
    spatial_gaps,
    {
      core_summaries
      source(here("scripts", "07_spatial_gaps.R"), local = TRUE)
      list.files(here(p_data_proc, "gaps"), pattern = "^spatial_.*\\.csv$", full.names = TRUE)
    },
    format = "file"
  ),

  tar_target(
    temporal_gaps,
    {
      core_summaries
      source(here("scripts", "08_temporal_gaps.R"), local = TRUE)
      list.files(here(p_data_proc, "gaps"), pattern = "^temporal_.*\\.csv$|^cell_recency.*\\.csv$", full.names = TRUE)
    },
    format = "file"
  ),

  tar_target(
    reconcile_taxonomy,
    {
      taxa_reference; cube_parquet
      source(here("scripts", "09a_reconcile_taxonomy.R"), local = TRUE)
      match_table <- here(p_data_proc, "gaps", "taxonomic_match_table.csv")
      stopifnot(file.exists(match_table))
      match_table
    },
    format = "file"
  ),

  tar_target(
    taxonomic_gaps,
    {
      reconcile_taxonomy; core_summaries; species_summaries
      source(here("scripts", "09b_taxonomic_gaps.R"), local = TRUE)
      list.files(here(p_data_proc, "gaps"), pattern = "^taxonomic_.*\\.csv$", full.names = TRUE)
    },
    format = "file"
  ),

  # 09c is a sibling of 09b — both depend on reconcile_taxonomy.
  # 09c produces per-scope (all / dyntaxa / threatened / invasive / sensitive)
  # summaries + the recent-period layer (cell_recency, basis_recent,
  # spatial_gaps zero-filled, cell_last_year, tax_cell_recency) +
  # recent_cutoff.rds as a pipeline constant.
  tar_target(
    scope_summaries,
    {
      reconcile_taxonomy; cube_parquet; grids
      source(here("scripts", "09c_scope_summaries.R"), local = TRUE)
      recent_cutoff <- here(p_data_proc, "recent_cutoff.rds")
      stopifnot(file.exists(recent_cutoff))

      # Per-scope files (cell_summary_all_10km.csv etc.)
      scope_files <- list.files(
        here(p_derived),
        pattern = "_(all|dyntaxa|threatened|invasive|sensitive)_(10|50)km\\.csv$",
        full.names = TRUE
      )
      # Non-scope 09c outputs
      tax_cell_recency <- list.files(
        here(p_derived),
        pattern = "^tax_cell_recency_(10|50)km\\.csv$",
        full.names = TRUE
      )
      species_scope <- here(p_derived, "species_scope_summary.csv")

      all_files <- c(recent_cutoff, scope_files, tax_cell_recency,
                     if (file.exists(species_scope)) species_scope)
      if (length(scope_files) == 0) stop("No scope summary files created by 09c")
      all_files
    },
    format = "file"
  ),

  # ==========================================================================
  # Phase 5: Integrated Overview (script 10)
  # ==========================================================================

  tar_target(
    gap_overview,
    {
      spatial_gaps; temporal_gaps; taxonomic_gaps
      source(here("scripts", "10_make_gap_overview.R"), local = TRUE)
      all_tables <- list.files(here(p_output, "tables"), pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
      if (length(all_tables) == 0) stop("No overview tables created")
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
      gap_overview; scope_summaries
      source(here("scripts", "11_prepare_gap_app_data.R"), local = TRUE)
      shiny_path <- here("shiny_app", "gap_app", "data", "shiny_data.rds")
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
      explorer_path <- here("shiny_app", "gbif_explorer", "data", "shiny_data.rds")
      stopifnot(file.exists(explorer_path))
      explorer_path
    },
    format = "file"
  ),

  # ==========================================================================
  # Phase 8: Reports (manual trigger)
  # ==========================================================================

  tar_render(report_overview,
    path = here("analysis", "01_overview.Rmd"), output_dir = here("analysis"),
    cue = tar_cue(mode = "never")),
  tar_render(report_spatial_gaps,
    path = here("analysis", "02_spatial_gaps.Rmd"), output_dir = here("analysis"),
    cue = tar_cue(mode = "never")),
  tar_render(report_temporal_gaps,
    path = here("analysis", "03_temporal_gaps.Rmd"), output_dir = here("analysis"),
    cue = tar_cue(mode = "never")),
  tar_render(report_record_types,
    path = here("analysis", "04_record_types.Rmd"), output_dir = here("analysis"),
    cue = tar_cue(mode = "never")),
  tar_render(report_taxonomic_gaps,
    path = here("analysis", "05_taxonomic_gaps.Rmd"), output_dir = here("analysis"),
    cue = tar_cue(mode = "never")),
  tar_render(report_species_of_concern,
    path = here("analysis", "06_species_of_concern.Rmd"), output_dir = here("analysis"),
    cue = tar_cue(mode = "never")),
  tar_render(report_publishers,
    path = here("analysis", "07_publishers.Rmd"), output_dir = here("analysis"),
    cue = tar_cue(mode = "never")),
  tar_render(report_priorities,
    path = here("analysis", "08_priorities.Rmd"), output_dir = here("analysis"),
    cue = tar_cue(mode = "never")),

  # ==========================================================================
  # Pipeline Summary
  # ==========================================================================

  tar_target(
    pipeline_summary,
    {
      list(
        completed_at           = Sys.time(),
        country                = COUNTRY_CODE,
        n_grid_files           = length(grids),
        n_taxa_files           = length(taxa_reference),
        n_core_summary_files   = length(core_summaries),
        n_species_files        = length(species_summaries),
        n_spatial_gap_files    = length(spatial_gaps),
        n_temporal_gap_files   = length(temporal_gaps),
        n_taxonomic_gap_files  = length(taxonomic_gaps),
        n_scope_summary_files  = length(scope_summaries),
        n_overview_tables      = length(gap_overview),
        gap_app_ready          = file.exists(gap_app_data),
        explorer_app_ready     = file.exists(explorer_app_data)
      )
    }
  )
)
