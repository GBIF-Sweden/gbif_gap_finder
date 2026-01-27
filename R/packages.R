# R/packages.R
# ==============================================================================
# Package Dependency Management
# ==============================================================================
# This file:
# - Defines all required and optional packages
# - Provides functions to check, install, and load packages
# - Integrates with renv for reproducibility

library(cli)

# Required packages -------------------------------------------------------
# Core packages needed for project functionality

required_packages <- c(
  # Project infrastructure
  "renv", "here", "yaml", "cli",
  
  # Project utilities
  "usethis", "glue",
  
  # Data APIs (for GBIF/external data access)
  "rgbif", "httr2", "jsonlite",
  
  # Data manipulation (tidyverse core)
  "dplyr", "tidyr", "stringr", "lubridate", "readr", "purrr", "tibble",
  
  # High-performance data handling
  "data.table", "fst",
  
  # Spatial analysis
  "sf", "terra", "exactextractr", "units",
  
  # Visualization
  "ggplot2", "viridis", "scales", "patchwork", "leaflet", "gt",
  
  # Workflow orchestration
  "targets", "tarchetypes",
  
  # Diagrams and documentation
  "DiagrammeR",
  
  # Reporting
  "rmarkdown", "knitr"
)

# Optional packages -------------------------------------------------------
# Packages for specialized analyses (not required for core workflow)

optional_packages <- c(
  "vegan",    # Community ecology analysis
  "iNEXT",    # Biodiversity estimation
  "BAT",      # Biodiversity assessment tools
  "finch"     # Darwin Core parsing
)

# Package management functions --------------------------------------------

#' Check which required packages are installed
#' @param pkgs Character vector of package names
#' @return Character vector of missing package names (invisible)
check_packages <- function(pkgs = required_packages) {
  # Use vapply for type safety
  is_installed <- vapply(
    pkgs,
    function(pkg) requireNamespace(pkg, quietly = TRUE),
    logical(1)
  )
  
  missing <- pkgs[!is_installed]
  
  if (length(missing) == 0) {
    cli_alert_success(
      "All {length(pkgs)} required packages are installed"
    )
  } else {
    cli_alert_warning(
      "Missing {length(missing)} package{?s}:"
    )
    cli_ul(missing)
  }
  
  invisible(missing)
}

#' Install missing packages
#' @param pkgs Character vector of package names to check
#' @param repos CRAN repository URL
#' @return Character vector of packages that were missing (invisible)
install_missing_packages <- function(
    pkgs = required_packages,
    repos = getOption("repos")
) {
  missing <- check_packages(pkgs)
  
  if (length(missing) == 0) {
    cli_alert_info("No packages to install")
    return(invisible(character(0)))
  }
  
  cli_alert_info("Installing {length(missing)} package{?s}...")
  
  tryCatch(
    {
      install.packages(missing, repos = repos)
      cli_alert_success("Installation complete")
      cli_alert_info("Update renv lockfile: {.code renv::snapshot()}")
    },
    error = function(e) {
      cli_abort(
        c(
          "Failed to install packages",
          "x" = "Error: {e$message}",
          "i" = "Try installing individually or check internet connection"
        )
      )
    }
  )
  
  invisible(missing)
}

#' Load required packages into R session
#' @param pkgs Character vector of package names
#' @return NULL (invisible)
#' @note This function loads packages into the namespace
load_packages <- function(pkgs = required_packages) {
  missing <- check_packages(pkgs)
  
  if (length(missing) > 0) {
    cli_abort(
      c(
        "Cannot load packages - {length(missing)} missing",
        "i" = "Install with: {.code install_missing_packages()}"
      )
    )
  }
  
  cli_alert_info("Loading {length(pkgs)} packages...")
  
  # Suppress package startup messages for cleaner output
  invisible(
    lapply(pkgs, function(pkg) {
      suppressPackageStartupMessages(
        library(pkg, character.only = TRUE)
      )
    })
  )
  
  cli_alert_success("Packages loaded")
  invisible(NULL)
}

#' Check and report on optional packages
#' @return Character vector of missing optional packages
check_optional_packages <- function() {
  is_installed <- vapply(
    optional_packages,
    function(pkg) requireNamespace(pkg, quietly = TRUE),
    logical(1)
  )
  
  installed <- optional_packages[is_installed]
  missing <- optional_packages[!is_installed]
  
  if (length(installed) > 0) {
    cli_alert_info(
      "Optional packages installed: {length(installed)}/{length(optional_packages)}"
    )
  }
  
  if (length(missing) > 0) {
    cli_alert_info("Optional packages not installed:")
    cli_ul(missing)
    cli_alert_info(
      "Install with: {.code install.packages(c({paste(missing, collapse = ', ')}))}"
    )
  }
  
  invisible(missing)
}
