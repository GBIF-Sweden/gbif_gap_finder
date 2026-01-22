# R/packages.R
# Central list of package dependencies + helpers (renv-friendly: no auto-install on source)

required_packages <- c(
  # Repro / project helpers
  "renv", "usethis", "here", "glue", "yaml",
  
  # Data access / APIs (optional for later)
  "rgbif", "httr2", "jsonlite",
  
  # Data handling
  "data.table", "dplyr", "tidyr", "stringr", "lubridate", "readr",
  
  # Spatial
  "sf", "terra", "exactextractr", "units",
  
  # Visualization
  "ggplot2", "viridis", "scales", "patchwork", "leaflet",
  
  # Workflow / pipeline
  "targets", "tarchetypes", "fst",
  
  # Diagrams
  "DiagrammeR",
  
  # Reporting (only needed if you use RMarkdown)
  "rmarkdown", "knitr"
)


optional_packages <- c(
  # Diagnostics / completeness
  "vegan", "iNEXT", "BAT", "finch"
)

check_packages <- function(pkgs = required_packages) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    message("Missing packages:\n- ", paste(missing, collapse = "\n- "))
  } else {
    message("All required packages are installed.")
  }
  invisible(missing)
}

install_missing_packages <- function(pkgs = required_packages, repos = getOption("repos")) {
  missing <- check_packages(pkgs)
  if (length(missing)) install.packages(missing, repos = repos)
  invisible(missing)
}

load_packages <- function(pkgs = required_packages) {
  missing <- check_packages(pkgs)
  if (length(missing)) {
    stop("Cannot load packages; missing:\n- ", paste(missing, collapse = "\n- "))
  }
  invisible(lapply(pkgs, function(p) library(p, character.only = TRUE)))
}

