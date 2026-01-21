# scripts/04_validate_inputs.R
# Phase 1 Validation: quick QA of processed grids, cube outputs, and taxa reference.
# Writes a markdown QA report to logs/.

source("scripts/00_setup.R")

# ---- Paths (uses your YAML: paths.data_proc + paths.logs) ---------------------
data_proc_rel <- cfg_get("paths.data_proc", "data_proc")
logs_rel      <- cfg_get("paths.logs", "logs")

p_data_proc <- here::here(data_proc_rel)
p_logs      <- here::here(logs_rel)
dir.create(p_logs, showWarnings = FALSE, recursive = TRUE)

ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
report_path <- file.path(p_logs, paste0("validation_report_", ts, ".md"))

# ---- Helpers -----------------------------------------------------------------
md <- character()
add <- function(...) md <<- c(md, paste0(...))
hr  <- function() add("\n---\n")
h1  <- function(x) add("\n# ", x, "\n")
h2  <- function(x) add("\n## ", x, "\n")
ok  <- function(x) add("- ✅ ", x)
warn<- function(x) add("- ⚠️ ", x)
bad <- function(x) add("- ❌ ", x)

exists_file <- function(path) file.exists(path) && !dir.exists(path)
exists_dir  <- function(path) dir.exists(path)

safe_read_rds <- function(path) {
  if (!exists_file(path)) stop("Missing file: ", path)
  readRDS(path)
}

# Identify likely cell code field in grid
guess_cellcode_field <- function(nms) {
  nms_l <- tolower(nms)
  candidates <- nms[grepl("eea|cell|code|grid", nms_l)]
  if (length(candidates) == 0) return(NA_character_)
  # Prefer explicit matches
  pref <- candidates[grepl("eea", tolower(candidates)) & grepl("code", tolower(candidates))]
  if (length(pref) > 0) return(pref[1])
  candidates[1]
}

# Read a small sample from fst or rds cube output
read_cube_sample <- function(path, n = 5000) {
  if (!exists_file(path)) stop("Processed cube missing: ", path)
  ext <- tools::file_ext(path)
  
  if (ext == "fst") {
    if (!requireNamespace("fst", quietly = TRUE)) stop("Package fst not available.")
    # fst supports reading slices; first n rows
    return(fst::read_fst(path, from = 1, to = n))
  }
  
  if (ext == "rds") {
    dt <- readRDS(path)
    if (inherits(dt, "data.table") || inherits(dt, "data.frame")) {
      return(as.data.frame(utils::head(dt, n)))
    }
    stop("Unknown object in RDS: ", path)
  }
  
  stop("Unknown processed cube file extension: ", ext)
}

# ---- Start report -------------------------------------------------------------
h1("Phase 1 Validation Report")
add("\nRun time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
add("\nProject root: `", here::here(), "`\n")
add("\nProcessed data folder: `", p_data_proc, "`\n")
add("\nConfig file: `", here::here("config.yml"), "`\n")

# ==============================================================================
# 1) Grids
# ==============================================================================
h2("1) Grid layers (10km & 50km)")

grid10_proc <- if (exists("out_grid_10km_gpkg")) out_grid_10km_gpkg else here::here(p_data_proc, "grids_10km.gpkg")
grid50_proc <- if (exists("out_grid_50km_gpkg")) out_grid_50km_gpkg else here::here(p_data_proc, "grids_50km.gpkg")

if (!exists_file(grid10_proc)) bad(paste0("10km processed grid missing: `", grid10_proc, "`")) else ok(paste0("10km processed grid found: `", grid10_proc, "`"))
if (!exists_file(grid50_proc)) bad(paste0("50km processed grid missing: `", grid50_proc, "`")) else ok(paste0("50km processed grid found: `", grid50_proc, "`"))

# Only proceed if present
if (exists_file(grid10_proc) && exists_file(grid50_proc)) {
  g10 <- sf::st_read(grid10_proc, quiet = TRUE)
  g50 <- sf::st_read(grid50_proc, quiet = TRUE)
  
  # CRS check
  epsg10 <- sf::st_crs(g10)$epsg
  epsg50 <- sf::st_crs(g50)$epsg
  add("\n- 10km CRS EPSG: `", epsg10, "`\n")
  add("- 50km CRS EPSG: `", epsg50, "`\n")
  if (!is.na(epsg10) && epsg10 == CRS_SWEREF99TM) ok("10km CRS is SWEREF99 TM (EPSG:3006)") else warn("10km CRS is not EPSG:3006 (check transformation)")
  if (!is.na(epsg50) && epsg50 == CRS_SWEREF99TM) ok("50km CRS is SWEREF99 TM (EPSG:3006)") else warn("50km CRS is not EPSG:3006 (check transformation)")
  
  # Geometry types + validity
  gt10 <- unique(sf::st_geometry_type(g10))
  gt50 <- unique(sf::st_geometry_type(g50))
  add("\n- 10km geometry types: `", paste(gt10, collapse = ", "), "`\n")
  add("- 50km geometry types: `", paste(gt50, collapse = ", "), "`\n")
  
  # Validity quick check (sample to avoid heavy)
  inv10 <- sum(!sf::st_is_valid(g10))
  inv50 <- sum(!sf::st_is_valid(g50))
  add("\n- 10km invalid geometries: `", inv10, "`\n")
  add("- 50km invalid geometries: `", inv50, "`\n")
  if (inv10 == 0) ok("10km geometries valid") else warn("10km has invalid geometries (may still work, but consider fixing)")
  if (inv50 == 0) ok("50km geometries valid") else warn("50km has invalid geometries (may still work, but consider fixing)")
  
  # Row counts
  add("\n- 10km cells (rows): `", nrow(g10), "`\n")
  add("- 50km cells (rows): `", nrow(g50), "`\n")
  
  # Try to find cell code field
  f10 <- guess_cellcode_field(names(g10))
  f50 <- guess_cellcode_field(names(g50))
  add("\n- 10km likely cell code field: `", f10, "`\n")
  add("- 50km likely cell code field: `", f50, "`\n")
  
  if (!is.na(f10)) {
    u10 <- length(unique(g10[[f10]]))
    if (u10 == nrow(g10)) ok("10km cell code field appears unique") else warn("10km cell code field not unique (check identifier)")
  } else {
    warn("Could not guess 10km cell code field (we'll need to identify it for joining to cube eeacellcode)")
  }
  
  if (!is.na(f50)) {
    u50 <- length(unique(g50[[f50]]))
    if (u50 == nrow(g50)) ok("50km cell code field appears unique") else warn("50km cell code field not unique (check identifier)")
  } else {
    warn("Could not guess 50km cell code field (we'll need to identify it for joining to cube eeacellcode)")
  }
}

hr()

# ==============================================================================
# 2) Cube ingestion outputs
# ==============================================================================
h2("2) Occurrence Cube outputs")

manifest_path <- file.path(p_data_proc, "cube_manifest.csv")
totals_path   <- file.path(p_data_proc, "cube_totals_by_basisOfRecord.csv")

if (!exists_file(manifest_path)) bad(paste0("Cube manifest missing: `", manifest_path, "`")) else ok(paste0("Cube manifest found: `", manifest_path, "`"))
if (!exists_file(totals_path))   bad(paste0("Cube totals missing: `", totals_path, "`"))   else ok(paste0("Cube totals found: `", totals_path, "`"))

if (exists_file(manifest_path)) {
  man <- readr::read_csv(manifest_path, show_col_types = FALSE)
  add("\n- Manifest rows: `", nrow(man), "`\n")
  add("- Full ingests: `", sum(man$full_ingest %in% TRUE), "`\n")
  add("- Skipped full ingests: `", sum(man$full_ingest %in% FALSE), "`\n")
  
  # Check processed files exist where expected
  if ("processed_file" %in% names(man)) {
    expected <- man$processed_file[man$full_ingest %in% TRUE]
    missing  <- expected[!is.na(expected) & !file.exists(expected)]
    if (length(missing) == 0) ok("All processed cube files exist for full_ingest==TRUE") else bad(paste0("Missing processed cube outputs: ", paste(missing, collapse = ", ")))
  } else {
    warn("Manifest has no 'processed_file' column to verify outputs")
  }
  
  # Sample a few processed cubes to check schema
  sample_rows <- man[man$full_ingest %in% TRUE & !is.na(man$processed_file), , drop = FALSE]
  if (nrow(sample_rows) > 0) {
    k <- min(3, nrow(sample_rows))
    sample_rows <- sample_rows[seq_len(k), ]
    
    add("\n### 2.1 Column checks (sample of processed cubes)\n")
    for (i in seq_len(nrow(sample_rows))) {
      pf <- sample_rows$processed_file[i]
      g  <- sample_rows$grid[i]
      bor<- sample_rows$basisOfRecord[i]
      
      add("\n- Sample: `", basename(pf), "` (grid=", g, ", basisOfRecord=", bor, ")\n")
      
      samp <- tryCatch(read_cube_sample(pf, n = 2000), error = function(e) e)
      if (inherits(samp, "error")) {
        warn(paste0("Could not read sample: ", samp$message))
        next
      }
      
      cn <- tolower(names(samp))
      required <- c("specieskey", "eeacellcode", "yearmonth", "occurrences")
      missing_cols <- required[!required %in% cn]
      if (length(missing_cols) == 0) ok("Required cube columns present: specieskey, eeacellcode, yearmonth, occurrences")
      else warn(paste0("Missing some expected cube columns: ", paste(missing_cols, collapse = ", ")))
      
      # basic sanity of occurrences
      if ("occurrences" %in% cn) {
        occ <- as.numeric(samp[[names(samp)[cn == "occurrences"][1]]])
        if (all(is.finite(occ), na.rm = TRUE)) ok("occurrences column parses as numeric") else warn("occurrences has non-numeric values (check parsing)")
        if (min(occ, na.rm = TRUE) >= 0) ok("occurrences min >= 0") else warn("occurrences has negative values (unexpected)")
      }
      
      rm(samp)
      invisible(gc())
    }
  } else {
    warn("No fully ingested cubes available to sample (all were skipped as large?)")
  }
}

if (exists_file(totals_path)) {
  tots <- readr::read_csv(totals_path, show_col_types = FALSE)
  add("\n### 2.2 Totals quick view\n")
  add("\n- Totals rows: `", nrow(tots), "`\n")
  if (all(c("grid", "basisOfRecord", "total_occurrences") %in% names(tots))) {
    # show top 5 by total occurrences
    top5 <- tots |>
      dplyr::arrange(dplyr::desc(total_occurrences)) |>
      dplyr::slice_head(n = 5)
    add("\nTop 5 (grid, basisOfRecord, total_occurrences):\n\n")
    add("```text\n")
    add(paste(capture.output(print(top5[, c("grid","basisOfRecord","total_occurrences")])), collapse = "\n"))
    add("\n```\n")
  }
}

hr()

# ==============================================================================
# 3) Taxa reference outputs
# ==============================================================================
h2("3) Taxa reference (Red List / taxonomy)")

taxa_ref_path <- file.path(p_data_proc, "taxa_reference_current.rds")
dist_path     <- file.path(p_data_proc, "redlist_se_distribution_current.rds")
taxon_path    <- file.path(p_data_proc, "redlist_se_taxon_current.rds")

if (!exists_file(taxa_ref_path)) bad(paste0("taxa_reference_current.rds missing: `", taxa_ref_path, "`")) else ok(paste0("Taxa reference found: `", taxa_ref_path, "`"))
if (!exists_file(dist_path))     warn(paste0("Distribution RDS not found (optional): `", dist_path, "`")) else ok("Distribution RDS present")
if (!exists_file(taxon_path))    warn(paste0("Taxon RDS not found (optional): `", taxon_path, "`")) else ok("Taxon RDS present")

if (exists_file(taxa_ref_path)) {
  tr <- safe_read_rds(taxa_ref_path)
  
  add("\n- Taxa reference rows: `", nrow(tr), "`\n")
  add("- Taxa reference cols: `", ncol(tr), "`\n")
  
  # Column presence (DwC-ish)
  # Note: your script may have standardized names to lowercase; check both variants.
  cols <- names(tr)
  cols_l <- tolower(cols)
  
  need_any <- function(options) any(tolower(options) %in% cols_l)
  
  if (need_any(c("taxonID", "taxonid"))) ok("taxonID present") else warn("taxonID not found (check ingestion mapping)")
  if (need_any(c("scientificName", "scientificname"))) ok("scientificName present") else warn("scientificName not found")
  if (need_any(c("taxonRank", "taxonrank"))) ok("taxonRank present") else warn("taxonRank not found")
  if (need_any(c("acceptedNameUsageID", "acceptednameusageid"))) ok("acceptedNameUsageID present") else warn("acceptedNameUsageID not found")
  if (need_any(c("threatStatus", "threatstatus"))) ok("threatStatus present") else warn("threatStatus not found")
  
  # Duplicate key checks
  key_candidates <- c("taxonid", "taxonID", "id")
  key <- key_candidates[tolower(key_candidates) %in% cols_l][1]
  if (!is.na(key)) {
    kname <- cols[match(tolower(key), cols_l)]
    dup_n <- sum(duplicated(tr[[kname]]))
    add("\n- Duplicate count for key `", kname, "`: `", dup_n, "`\n")
    if (dup_n == 0) ok(paste0("No duplicates in key: ", kname)) else warn(paste0("Duplicates present in key: ", kname, " (may be fine depending on structure)"))
  } else {
    warn("No obvious key column found to check duplicates (id/taxonID)")
  }
  
  # Missingness of threatStatus (indicator of successful join between taxon and distribution)
  th <- NULL
  if ("threatStatus" %in% cols) th <- tr$threatStatus
  if ("threatstatus" %in% cols) th <- tr$threatstatus
  if (!is.null(th)) {
    na_rate <- mean(is.na(th))
    add("\n- threatStatus NA rate: `", round(na_rate, 3), "`\n")
    if (na_rate < 0.95) ok("threatStatus attached for a meaningful share of rows") else warn("threatStatus mostly NA (join key may not match between taxon and distribution)")
  }
}

# ---- Write report -------------------------------------------------------------
writeLines(md, report_path)
message("Validation report written to: ", report_path)

