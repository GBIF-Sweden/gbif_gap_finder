# R/packages.R
# ==============================================================================
# Package Management for GBIF Sweden Gap Analysis
# ==============================================================================
# This script:
# - Defines all required packages
# - Checks for missing packages
# - Provides installation helper
# - Loads packages in correct order
#
# Usage: source("R/packages.R")

# Required packages --------------------------------------------------------

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
 
 # Data manipulation
 "data.table",
 "fst",
 
 # Spatial
 "sf",
 "terra",
 
 # Visualization
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

# Optional packages (not required but enhance functionality)
optional_packages <- c(
 "rgbif",       # GBIF data access
 "arrow",       # Parquet file support
 "furrr",       # Parallel processing
 "progressr"    # Progress bars
)

# Check and install --------------------------------------------------------

#' Check which packages are missing
#' @return Character vector of missing package names
check_packages <- function() {
 required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
}

#' Install missing packages
install_missing_packages <- function() {
 missing <- check_packages()
 
 if (length(missing) == 0) {
   cli::cli_alert_success("All required packages are installed")
   return(invisible(NULL))
 }
 
 cli::cli_alert_info("Installing {length(missing)} missing package(s): {missing}")
 install.packages(missing, dependencies = TRUE)
}

# Run check on source
missing <- check_packages()

if (length(missing) > 0) {
 cli::cli_alert_warning(
   "Missing {length(missing)} required package(s): {missing}"
 )
 cli::cli_alert_info(
   "Run: install_missing_packages()"
 )
}

# Load packages ------------------------------------------------------------

#' Load all required packages
#' @param verbose Logical; print loading messages?
load_packages <- function(verbose = FALSE) {
 suppressPackageStartupMessages({
   # Core data manipulation
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
   
   # Visualization
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
   cli::cli_alert_success("Loaded {length(required_packages)} packages")
 }
}

# Set ggplot2 defaults -----------------------------------------------------

set_plot_theme <- function() {
 theme_set(
   theme_minimal(base_size = 11) +
     theme(
       panel.grid.minor = element_blank(),
       strip.background = element_rect(fill = "grey95", color = NA),
       legend.position = "bottom"
     )
 )
 
 # Set default color scales to viridis
 options(
   ggplot2.continuous.colour = "viridis",
   ggplot2.continuous.fill = "viridis"
 )
}

# Package versions ---------------------------------------------------------

#' Get version info for all loaded packages
#' @return data.frame with package names and versions
package_versions <- function() {
 loaded <- loadedNamespaces()
 versions <- sapply(loaded, function(p) as.character(packageVersion(p)))
 data.frame(
   package = loaded,
   version = versions,
   row.names = NULL,
   stringsAsFactors = FALSE
 ) |>
   dplyr::arrange(package)
}

# Session info for reproducibility ----------------------------------------

#' Print session info for reproducibility
print_session_info <- function() {
 cli::cli_h2("Session Information")
 print(sessionInfo())
}
