# run.R
# ==============================================================================
# Helper Script for Running the GBIF Sweden Gap Analysis Pipeline
# ==============================================================================
# This script provides convenient commands for running the targets pipeline.
# Source this file or run individual commands as needed.

library(targets)
library(here)

# =========================================================================
# QUICK REFERENCE
# =========================================================================
#
# Run full pipeline:
#   tar_make()
#
# Run specific phase:
#   tar_make(names = c(grid_10km, grid_50km, taxa_reference, cube_manifest))  # Phase 1
#   tar_make(names = validation_complete)                                      # Phase 2
#   tar_make(names = c(derived_summaries, grid_lookups))                       # Phase 3
#   tar_make(names = c(spatial_gaps, temporal_gaps, taxonomic_gaps))           # Phase 4
#   tar_make(names = gap_overview)                                             # Phase 5
#
# Render reports (must be triggered explicitly):
#   tar_make(names = report_sanity_checks)
#   tar_make(names = report_gap_analysis)
#   tar_make(names = report_final)
#
# Visualize pipeline:
#   tar_visnetwork()
#
# Check status:
#   tar_progress()
#   tar_manifest()
#
# Read/load results:
#   tar_read(pipeline_summary)
#   tar_load(taxa_reference)
#
# Rebuild from scratch:
#   tar_destroy()
#   tar_make()
#
# =========================================================================

cat("
╔══════════════════════════════════════════════════════════════════════════════╗
║           GBIF Sweden Gap Analysis - Targets Pipeline                        ║
╠══════════════════════════════════════════════════════════════════════════════╣
║                                                                              ║
║  Common commands:                                                            ║
║                                                                              ║
║    tar_make()           Run the full pipeline                                ║
║    tar_visnetwork()     Visualize the pipeline graph                         ║
║    tar_progress()       Check pipeline status                                ║
║    tar_read(name)       Read a target's cached result                        ║
║                                                                              ║
║  Phase-specific runs:                                                        ║
║                                                                              ║
║    run_phase_1()        Ingestion (grids, taxonomy, cubes)                   ║
║    run_phase_2()        Validation                                           ║
║    run_phase_3()        Derived summaries                                    ║
║    run_phase_4()        Gap analysis (spatial, temporal, taxonomic)          ║
║    run_phase_5()        Integrated overview                                  ║
║    run_shiny_prep()     Prepare data for Shiny app                           ║
║    run_reports()        Render all RMarkdown reports                         ║
║                                                                              ║
║  Shiny app:                                                                  ║
║                                                                              ║
║    launch_app()         Launch the Shiny dashboard                           ║
║                                                                              ║
╚══════════════════════════════════════════════════════════════════════════════╝
")

# =========================================================================
# CONVENIENCE FUNCTIONS
# =========================================================================

#' Run Phase 1: Data Ingestion
run_phase_1 <- function() {
  tar_make(names = c(grid_10km, grid_50km, taxa_reference, cube_manifest))
}

#' Run Phase 2: Validation
run_phase_2 <- function() {
  tar_make(names = validation_complete)
}

#' Run Phase 3: Derived Summaries
run_phase_3 <- function() {
  tar_make(names = c(derived_summaries, grid_lookups))
}
 
#' Run Phase 4: Gap Analysis
run_phase_4 <- function() {
  tar_make(names = c(spatial_gaps, temporal_gaps, taxonomic_gaps))
}

#' Run Phase 5: Integrated Overview
run_phase_5 <- function() {
  tar_make(names = gap_overview)
}

#' Prepare data for Shiny app
run_shiny_prep <- function() {
  tar_make(names = shiny_data)
}

#' Run all reports
run_reports <- function() {
  tar_make(names = c(
    report_sanity_checks,
    report_spatial_gaps,
    report_temporal_gaps,
    report_taxonomic_gaps,
    report_integrated
  ))
}

#' Launch the Shiny app
launch_app <- function() {
  # Check if shiny data exists
  shiny_data_path <- here("data_proc", "shiny_data.rds")
  
  if (!file.exists(shiny_data_path)) {
    cat("⚠️  Shiny data not found. Preparing data first...\n")
    run_shiny_prep()
  }
  
 # Copy shiny data to app folder
  app_data_dir <- here("shiny_app", "data")
  dir.create(app_data_dir, showWarnings = FALSE, recursive = TRUE)
  file.copy(shiny_data_path, file.path(app_data_dir, "shiny_data.rds"), overwrite = TRUE)
  
  cat("🚀 Launching Shiny app...\n")
  shiny::runApp(here("shiny_app"))
}

#' Run everything (full pipeline + reports)
run_all <- function() {
  tar_make()
  cat("\n✅ Core pipeline complete!\n")
  cat("To render reports, run: run_reports()\n")
}

#' Check pipeline status
status <- function() {
  cat("\n📊 Pipeline Status:\n\n")
  print(tar_progress())
  
  cat("\n📁 Output Summary:\n")
  
  if (file.exists(here("data_proc", "cube_manifest.csv"))) {
    manifest <- readr::read_csv(here("data_proc", "cube_manifest.csv"), show_col_types = FALSE)
    cat("  Cube files ingested:", nrow(manifest), "\n")
  }
  
  if (dir.exists(here("data_proc", "gaps"))) {
    gap_files <- list.files(here("data_proc", "gaps"), pattern = "\\.csv$")
    cat("  Gap analysis files:", length(gap_files), "\n")
  }
  
  if (dir.exists(here("output", "tables"))) {
    table_files <- list.files(here("output", "tables"), pattern = "\\.csv$", recursive = TRUE)
    cat("  Overview tables:", length(table_files), "\n")
  }
}

#' Clean all targets and rebuild
rebuild <- function() {
  cat("⚠️  This will delete all cached targets and rebuild from scratch.\n")
  cat("Continue? (y/n): ")
  
  if (tolower(readline()) == "y") {
    tar_destroy()
    tar_make()
  } else {
    cat("Cancelled.\n")
  }
}

# Show status on load
cat("\nRun status() to check pipeline progress, or tar_make() to run.\n\n")
