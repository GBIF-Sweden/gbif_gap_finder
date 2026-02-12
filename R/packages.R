# R/packages.R
# ============================================================================
# Package Management
# ============================================================================
# Purpose:
#   Define, check, install, and load all required packages for the
#   GBIF gap analysis pipeline.
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

required_packages <- c(
  # Core tidyverse
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "purrr",
  "tibble",
  "forcats",
  "lubridate",
  "glue",

  # High-performance data manipulation (used in cube processing)
  "data.table",
  "fst",

  # Spatial
  "sf",
  "terra",

  # Visualisation
  "ggplot2",
  "viridis",
  "scales",
  "patchwork",
  "gridExtra",

  # Tables
  "knitr",
  "kableExtra",
  "gt",
  "DT",

  # Reporting
  "rmarkdown",

  # Project management
  "here",
  "yaml",
  "cli",
  "fs",

  # Pipeline (optional but recommended)
  "targets",
  "tarchetypes"
)

# Optional packages (enhance functionality but not required) -----------------

optional_packages <- c(
  "rgbif",       # GBIF data access
  "httr2",       # GBIF Species API (09a taxonomy reconciliation)
  "arrow",       # Parquet file support
  "furrr",       # Parallel processing
  "progressr"    # Progress bars
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

    # Visualisation
    library(ggplot2)
    library(viridis)
    library(scales)

    # Tables and reporting
    library(knitr)

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
