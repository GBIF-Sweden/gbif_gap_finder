# scripts/06_make_grid_lookup.R
# Phase 2: Prepare lookup tables to join grid polygons to cube eeacellcode.
#
# Outputs to data_proc/derived/:
#   - grid_lookup_10km.csv
#   - grid_lookup_50km.csv
#
# If the script cannot confidently identify the EEA cell code field,
# it will stop and tell you which columns look plausible.

source("scripts/00_setup.R")

data_proc_rel <- cfg_get("paths.data_proc", "data_proc")
p_data_proc   <- here::here(data_proc_rel)

p_derived <- file.path(p_data_proc, "derived")
dir.create(p_derived, showWarnings = FALSE, recursive = TRUE)

grid10_path <- file.path(p_data_proc, "grids_10km.gpkg")
grid50_path <- file.path(p_data_proc, "grids_50km.gpkg")

if (!file.exists(grid10_path)) stop("Missing processed grid: ", grid10_path)
if (!file.exists(grid50_path)) stop("Missing processed grid: ", grid50_path)

guess_code_field <- function(nms) {
  n <- tolower(nms)
  # Prioritize likely names
  prefer <- nms[grepl("eea", n) & grepl("code", n)]
  if (length(prefer) > 0) return(prefer[1])
  prefer <- nms[grepl("cell", n) & grepl("code", n)]
  if (length(prefer) > 0) return(prefer[1])
  # Fall back to any code/cell/grid-ish column
  cand <- nms[grepl("eea|cell|code|grid", n)]
  if (length(cand) > 0) return(cand[1])
  NA_character_
}

make_lookup <- function(g, grid_label, out_file) {
  nms <- names(g)
  code_field <- guess_code_field(nms)
  
  if (is.na(code_field)) {
    stop(
      "Could not identify a cell code field for ", grid_label, ".\n",
      "Columns are:\n- ", paste(nms, collapse = "\n- "), "\n"
    )
  }
  
  # Check uniqueness and missingness
  code <- g[[code_field]]
  if (any(is.na(code))) {
    stop("Cell code field '", code_field, "' has NA values for ", grid_label)
  }
  
  n_total <- nrow(g)
  n_unique <- length(unique(code))
  if (n_unique != n_total) {
    stop(
      "Cell code field '", code_field, "' is not unique for ", grid_label, " (",
      n_unique, " unique of ", n_total, " rows).\n",
      "Pick another field and/or adjust guess_code_field()."
    )
  }
  
  # Make a stable polygon id for joins (row-based; reproducible as long as grid doesn't change)
  g$poly_id <- sprintf("%s_%06d", grid_label, seq_len(nrow(g)))
  
  # Output lookup: poly_id <-> eeacellcode
  lookup <- data.frame(
    poly_id = g$poly_id,
    eeacellcode = as.character(code),
    stringsAsFactors = FALSE
  )
  
  out_path <- file.path(p_derived, out_file)
  readr::write_csv(lookup, out_path)
  
  log_msg(grid_label, ": cell code field = ", code_field)
  log_msg("Wrote: ", out_path, " (rows=", nrow(lookup), ")")
  
  invisible(list(code_field = code_field, out = out_path))
}

# Read grids
g10 <- sf::st_read(grid10_path, quiet = TRUE)
g50 <- sf::st_read(grid50_path, quiet = TRUE)

make_lookup(g10, "grid10km", "grid_lookup_10km.csv")
make_lookup(g50, "grid50km", "grid_lookup_50km.csv")

log_msg("Grid lookup tables created.")
