# run.R
# ============================================================================
# Helper Script for Running the GBIF Gap Analysis Pipeline
# ============================================================================
# Source this file for convenient pipeline commands.
# Works with both targets-based and script-based workflows.
#
# Usage:
#   source("run.R")
#   status()         # Check pipeline progress
#   run_all()        # Full pipeline
#   launch_app()     # Start Shiny dashboard

library(targets)
library(here)

# ============================================================================
# Banner
# ============================================================================

cat("
+--------------------------------------------------------------------+
|            gbifgaps - GBIF Biodiversity Gap Analysis               |
+--------------------------------------------------------------------+
|                                                                    |
|  Common commands:                                                  |
|                                                                    |
|    tar_make()           Run the full pipeline                      |
|    tar_visnetwork()     Visualise the pipeline graph               |
|    tar_progress()       Check pipeline status                      |
|    tar_read(name)       Read a target's cached result              |
|                                                                    |
|  Phase-specific runs:                                              |
|                                                                    |
|    run_phase_1()        Ingestion (grids, taxonomy, cubes)         |
|    run_phase_2()        Validation                                 |
|    run_phase_3()        Derived summaries                          |
|    run_phase_4()        Gap analysis (spatial, temporal, taxonomic) |
|    run_phase_5()        Integrated overview                        |
|    run_shiny_prep()     Prepare data for Shiny app                 |
|    run_reports()        Render all R Markdown reports               |
|                                                                    |
|  Shiny app:                                                        |
|                                                                    |
|    launch_app()         Launch the Shiny dashboard                 |
|                                                                    |
+--------------------------------------------------------------------+
")

# ============================================================================
# Convenience Functions
# ============================================================================

#' Run Phase 1: Data Ingestion (scripts 02-04)
run_phase_1 <- function() {
  tar_make(names = c(grids, taxa_reference, cube_manifest))
}

#' Run Phase 2: Validation (script 05)
run_phase_2 <- function() {
  tar_make(names = validation_report)
}

#' Run Phase 3: Derived Summaries (scripts 06a, 06b)
run_phase_3 <- function() {
  tar_make(names = c(core_summaries, species_summaries))
}

#' Run Phase 4: Gap Analysis (scripts 07-09)
run_phase_4 <- function() {
  tar_make(names = c(spatial_gaps, temporal_gaps, taxonomic_gaps))
}

#' Run Phase 5: Integrated Overview (script 10)
run_phase_5 <- function() {
  tar_make(names = gap_overview)
}

#' Prepare data for Shiny app (script 11)
run_shiny_prep <- function() {
  tar_make(names = shiny_data)
}

#' Render all R Markdown reports
run_reports <- function() {
  tar_make(names = c(
    report_sanity_checks,
    report_spatial_gaps,
    report_temporal_gaps,
    report_taxonomic_gaps,
    report_integrated
  ))
}

#' Launch the Shiny dashboard
#'
#' Checks for prepared data and copies it to the app directory
#' before launching.
launch_app <- function() {
  shiny_data_path <- here("data_proc", "shiny_data.rds")

  if (!file.exists(shiny_data_path)) {
    cat(
      "\u26a0\ufe0f  Shiny data not found. Preparing first...\n"
    )
    run_shiny_prep()
  }

  app_data_dir <- here("shiny_app", "data")
  dir.create(app_data_dir, showWarnings = FALSE, recursive = TRUE)
  file.copy(
    shiny_data_path,
    file.path(app_data_dir, "shiny_data.rds"),
    overwrite = TRUE
  )

  cat("\U0001f680 Launching Shiny app...\n")
  shiny::runApp(here("shiny_app"))
}

#' Run the full pipeline (core only; reports are separate)
run_all <- function() {
  tar_make()
  cat("\n\u2705 Core pipeline complete!\n")
  cat("To render reports, run: run_reports()\n")
}

#' Check pipeline status
status <- function() {
  cat("\n\U0001f4ca Pipeline Status:\n\n")
  print(tar_progress())

  cat("\n\U0001f4c1 Output Summary:\n")

  manifest_path <- here("data_proc", "cube_manifest.csv")
  if (file.exists(manifest_path)) {
    manifest <- readr::read_csv(
      manifest_path, show_col_types = FALSE
    )
    cat("  Cube files ingested:", nrow(manifest), "\n")
  }

  gaps_dir <- here("data_proc", "gaps")
  if (dir.exists(gaps_dir)) {
    gap_files <- list.files(gaps_dir, pattern = "\\.csv$")
    cat("  Gap analysis files:", length(gap_files), "\n")
  }

  tables_dir <- here("output", "tables")
  if (dir.exists(tables_dir)) {
    table_files <- list.files(
      tables_dir, pattern = "\\.csv$", recursive = TRUE
    )
    cat("  Overview tables:", length(table_files), "\n")
  }

  shiny_path <- here("data_proc", "shiny_data.rds")
  cat(
    "  Shiny data ready:",
    ifelse(file.exists(shiny_path), "Yes", "No"), "\n"
  )
}

#' Destroy all cached targets and rebuild from scratch
rebuild <- function() {
  cat(
    "\u26a0\ufe0f  This will delete all cached targets and rebuild.\n"
  )
  cat("Continue? (y/n): ")

  if (interactive() && tolower(readline()) == "y") {
    tar_destroy()
    tar_make()
  } else {
    cat("Cancelled.\n")
  }
}

# Show hint on load
cat(
  "\nRun status() to check progress, or tar_make() to run.\n\n"
)
