# R/packages.R
# ============================================================================
# Package Management
# ============================================================================
# Purpose:
#   Define, check, install, and load all required packages for the
#   GBIF gap finder pipeline.
#
# Usage:
#   source("R/packages.R")
#
# Exports:
#   - check_packages()            List missing required packages
#   - install_missing_packages()  Install any that are missing
#   - load_packages()             Attach packages in correct order
#   - set_plot_theme()            Set consistent ggplot2 defaults
#   - package_versions()          Table of loaded package versions
# ============================================================================

# Required packages ----------------------------------------------------------
# These are the packages actually used by the pipeline scripts (00-11).

required_packages <- c(
  # Core data manipulation
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "purrr",
  "tibble",
  "lubridate",
  "glue",

  # High-performance data manipulation (cube processing)
  "data.table",

  # Spatial
  "sf",

  # Formatting (scales::comma is used in every script)
  "scales",

  # Project management
  "here",
  "yaml",
  "cli",
  "fs"
)

# Optional packages (enhance pipeline but not strictly required) -------------

optional_packages <- c(
  "arrow",       # Parquet file support (required for scripts 04+)
  "rgbif",       # GBIF data access (script 01)
  "httr",        # GBIF Registry API (script 06a publisher names)
  "httr2",       # GBIF Species API (script 09a Tier 4)
  "geodata",     # GADM admin boundaries (script 01)
  "mregions2",   # Marine Regions EEZ (script 02, T-D5; only when marine.enabled)
  "furrr",       # Parallel processing
  "progressr"    # Progress bars
)

# App/reporting packages (used by Shiny app and Rmd reports, not scripts) ----

app_packages <- c(
  # Shiny app runtime (loaded by shiny_app/gap_finder/app.R)
  "shiny",
  "shinyWidgets",
  "plotly",
  "leaflet",
  "DT",
  "ggplot2",
  "viridis",
  # Rmd reports (analysis/*.Rmd)
  "patchwork",
  "gridExtra",
  "knitr",
  "kableExtra",
  "gt",
  "rmarkdown",
  "forcats",
  "targets",
  "tarchetypes"
)

# Check and install ----------------------------------------------------------

#' Check which required packages are missing
#'
#' @return Character vector of missing package names
check_packages <- function() {
  required_packages[
    !vapply(
      required_packages,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]
}

#' Install any missing required packages
#'
#' @return Invisible NULL
install_missing_packages <- function() {
  missing <- check_packages()

  if (length(missing) == 0) {
    cli::cli_alert_success("All required packages are installed")
    return(invisible(NULL))
  }

  cli::cli_alert_info(
    "Installing {length(missing)} missing package{?s}: {missing}"
  )
  install.packages(missing, dependencies = TRUE)
}

# Run check on source --------------------------------------------------------

missing <- check_packages()

if (length(missing) > 0) {
  cli::cli_alert_warning(
    "Missing {length(missing)} required package{?s}: {missing}"
  )
  cli::cli_alert_info("Run: install_missing_packages()")
}

# Load packages --------------------------------------------------------------

#' Load all required packages in the correct order
#'
#' dplyr is loaded after data.table so that dplyr verbs take
#' precedence for interactive use, while data.table syntax remains
#' available for high-performance cube processing.
#'
#' @param verbose Logical; print loading messages?
#' @return Invisible NULL
load_packages <- function(verbose = FALSE) {
  suppressPackageStartupMessages({
    # Data manipulation (data.table first, dplyr masks intentionally)
    library(data.table)
    library(dplyr)
    library(tidyr)
    library(readr)
    library(stringr)
    library(purrr)
    library(lubridate)
    library(glue)

    # Spatial
    library(sf)

    # Formatting (scales::comma used across all scripts)
    library(scales)

    # Project utilities
    library(here)
    library(cli)
  })

  if (verbose) {
    cli::cli_alert_success(
      "Loaded {length(required_packages)} packages"
    )
  }
  invisible(NULL)
}

# ggplot2 defaults -----------------------------------------------------------

#' Set consistent ggplot2 theme across all outputs
#'
#' Applies a minimal theme with viridis colour scales.
#' Call once per session (typically in 00_setup.R).
#'
#' @return Invisible NULL
set_plot_theme <- function() {
  theme_set(
    theme_minimal(base_size = 11) +
      theme(
        panel.grid.minor = element_blank(),
        strip.background = element_rect(
          fill = "grey95", color = NA
        ),
        legend.position = "bottom"
      )
  )

  options(
    ggplot2.continuous.colour = "viridis",
    ggplot2.continuous.fill   = "viridis"
  )
  invisible(NULL)
}

# Package versions -----------------------------------------------------------

#' Get version info for all loaded packages
#'
#' @return A tibble with columns `package` and `version`
package_versions <- function() {
  loaded <- loadedNamespaces()
  versions <- vapply(
    loaded,
    function(p) as.character(packageVersion(p)),
    character(1)
  )
  tibble::tibble(
    package = loaded,
    version = versions
  ) |>
    dplyr::arrange(package)
}

# Session info ---------------------------------------------------------------

#' Print session info for reproducibility
#'
#' @return Invisible; prints to console
print_session_info <- function() {
  cli::cli_h2("Session Information")
  print(sessionInfo())
}
