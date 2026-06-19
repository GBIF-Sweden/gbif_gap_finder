# run.R
# ============================================================================
# Helper Script for Running the GBIF Gap Finder Pipeline
# ============================================================================
# Source this file for convenient pipeline commands.
# Works with both targets-based and script-based workflows.
#
# Usage:
#   source("run.R")
#   status()         # Check pipeline progress
#   run_all()        # Full pipeline
#   launch_gap_finder() # Start the Gap Finder app

library(targets)
library(here)

# ============================================================================
# Banner
# ============================================================================

cat("
+--------------------------------------------------------------------+
|                     GBIF Gap Finder                                |
+--------------------------------------------------------------------+
|                                                                    |
|  Common commands:                                                  |
|                                                                    |
|    tar_make()           Run the full pipeline                      |
|    tar_visnetwork()     Visualise the pipeline graph               |
|    tar_progress()       Check pipeline status                      |
|    tar_read(name)       Read a cached result                       |
|                                                                    |
|  Phase-specific runs:                                              |
|                                                                    |
|    run_download()       Download raw data (taxonomy, invasives...)  |
|    run_phase_1()        Ingestion (grids, taxonomy, cubes)         |
|    run_phase_2()        Validation                                 |
|    run_phase_3()        Derived summaries                          |
|    run_phase_4()        Gap analysis (spatial, temporal, taxonomic) |
|    run_phase_5()        Integrated overview                        |
|    run_gap_finder_prep()  Prepare data for the Gap Finder app      |
|    run_reports()        Render all R Markdown reports               |
|                                                                    |
|  Shiny apps:                                                       |
|                                                                    |
|    launch_gap_finder()    Launch the Gap Finder app                |
|                                                                    |
+--------------------------------------------------------------------+
")

# ============================================================================
# Convenience Functions
# ============================================================================

#' Download raw data (script 01 — taxonomy, red list, invasives, sensitive, admin)
run_download <- function() {
  tar_make(names = raw_data)
}

#' Run Phase 1: Data Ingestion (scripts 01-04)
run_phase_1 <- function() {
  tar_make(names = c(raw_data, grids, taxa_reference, cube_parquet))
}

#' Run Phase 2: Validation (script 05)
run_phase_2 <- function() {
  tar_make(names = validation_report)
}

#' Run Phase 3: Derived Summaries (scripts 06a, 06b)
run_phase_3 <- function() {
  tar_make(names = c(core_summaries, species_summaries))
}

#' Run Phase 4: Gap Analysis (scripts 07, 08, 09a, 09b, 09c)
run_phase_4 <- function() {
  tar_make(names = c(spatial_gaps, temporal_gaps, reconcile_taxonomy,
                     taxonomic_gaps, scope_summaries))
}

#' Run Phase 5: Integrated Overview (script 10)
run_phase_5 <- function() {
  tar_make(names = gap_overview)
}

#' Prepare data for the Gap Finder app (script 11)
run_gap_finder_prep <- function() {
  tar_make(names = gap_finder_data)
}

#' Render all R Markdown reports
run_reports <- function() {
  tar_make(names = c(
    report_overview,
    report_spatial_gaps,
    report_temporal_gaps,
    report_record_types,
    report_taxonomic_gaps,
    report_species_of_concern,
    report_publishers,
    report_priorities
  ))
}

#' Launch the Gap Finder app
launch_gap_finder <- function() {
  gap_data_path <- here("shiny_app", "gap_finder", "data", "shiny_data.rds")
  if (!file.exists(gap_data_path)) {
    cat("\u26a0\ufe0f  Gap app data not found. Preparing first...\n")
    run_gap_finder_prep()
  }
  cat("\U0001f680 Launching Gap Finder app...\n")
  shiny::runApp(here("shiny_app", "gap_finder"))
}

#' Run the full pipeline (core only; reports are separate)
run_all <- function() {
  tar_make()
  cat("\n\u2705 Core pipeline complete!\n")
  cat("To render reports, run: run_reports()\n")
}

#' Check pipeline status
status <- function() {
  source(here("scripts", "00_setup.R"))

  cat("\n\U0001f4ca Pipeline Status:\n\n")
  print(tar_progress())

  cat("\n\U0001f4c1 Output Summary:\n")

  manifest_path <- here(p_data_proc, "cubes", "cube_manifest.csv")
  if (file.exists(manifest_path)) {
    manifest <- readr::read_csv(manifest_path, show_col_types = FALSE)
    cat("  Parquet cubes:", nrow(manifest), "\n")
  }

  gaps_dir <- here(p_data_proc, "gaps")
  if (dir.exists(gaps_dir)) {
    gap_files <- list.files(gaps_dir, pattern = "\\.csv$")
    cat("  Gap analysis files:", length(gap_files), "\n")
  }

  tables_dir <- here(p_output, "tables")
  if (dir.exists(tables_dir)) {
    table_files <- list.files(tables_dir, pattern = "\\.csv$", recursive = TRUE)
    cat("  Overview tables:", length(table_files), "\n")
  }

  # Data source status
  cat("\n\U0001f4e6 Data Sources:\n")
  taxa_path <- here(p_data_proc, "taxa_reference_current.rds")
  cat("  Taxonomy backbone:", ifelse(file.exists(taxa_path), "Yes", "No"), "\n")

  invasive_files <- if (dir.exists(raw_invasives_dir)) list.files(raw_invasives_dir, pattern = "\\.(txt|csv)$") else character(0)
  cat("  Invasive species:", ifelse(length(invasive_files) > 0, paste0("Yes (", length(invasive_files), " files)"), "No"), "\n")

  sensitive_files <- if (dir.exists(raw_sensitive_dir)) list.files(raw_sensitive_dir, pattern = "\\.(txt|csv)$") else character(0)
  cat("  Sensitive species:", ifelse(length(sensitive_files) > 0, paste0("Yes (", length(sensitive_files), " files)"), "No"), "\n")

  gap_finder_path <- here("shiny_app", "gap_finder", "data", "shiny_data.rds")
  cat("  Gap Finder data ready:", ifelse(file.exists(gap_finder_path), "Yes", "No"), "\n")
}

#' Destroy all cached targets and rebuild from scratch
rebuild <- function() {
  cat("\u26a0\ufe0f  This will delete all cached targets and rebuild.\n")
  cat("Continue? (y/n): ")
  if (interactive() && tolower(readline()) == "y") {
    tar_destroy()
    tar_make()
  } else {
    cat("Cancelled.\n")
  }
}

cat("\nRun status() to check progress, or tar_make() to run.\n\n")
