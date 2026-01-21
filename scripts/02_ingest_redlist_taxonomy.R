# scripts/02_ingest_redlist_taxonomy.R
# Read Swedish red list distribution + taxon backbone, write proc .rds, build taxa_reference_current.rds.

source("scripts/00_setup.R")

# --- Inputs (fixed filenames you provided) ------------------------------------
f_dist  <- file.path(raw_redlist_se_dir, "distribution.txt")
f_taxon <- file.path(raw_redlist_se_dir, "taxon.txt")

# --- Output files --------------------------------------------------------------
out_dist_rds  <- here::here("data_proc", "redlist_se_distribution_current.rds")
out_taxon_rds <- here::here("data_proc", "redlist_se_taxon_current.rds")
out_taxa_ref  <- here::here("data_proc", "taxa_reference_current.rds")

# --- Helpers ------------------------------------------------------------------
detect_delim <- function(path) {
  # crude but effective: compare tabs vs commas in first line
  first <- readLines(path, n = 1, warn = FALSE)
  if (length(first) == 0) return("\t")
  if (stringr::str_count(first, "\t") >= stringr::str_count(first, ",")) "\t" else ","
}

read_table_safe <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  delim <- detect_delim(path)
  readr::read_delim(
    path,
    delim = delim,
    show_col_types = FALSE,
    progress = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  )
}

# --- Helpers: DwC-aware column renaming ---------------------------------------

# Map "lowercased input names" -> "desired DwC name"
dwc_name_map <- c(
  # shared / IDs
  "id" = "id",  # keep if you need it as join key
  "taxonid" = "taxonID",
  "acceptednameusageid" = "acceptedNameUsageID",
  
  # Taxon core
  "scientificname" = "scientificName",
  "scientificnameauthorship" = "scientificNameAuthorship",
  "taxonrank" = "taxonRank",
  "taxonomicstatus" = "taxonomicStatus",
  "nomenclaturalstatus" = "nomenclaturalStatus",
  "taxonremarks" = "taxonRemarks",
  "kingdom" = "kingdom",
  "phylum" = "phylum",
  "class" = "class",
  "order" = "order",
  "family" = "family",
  "genus" = "genus",
  
  # Occurrence-ish fields present in your distribution file
  "countrycode" = "countryCode",
  "occurrencestatus" = "occurrenceStatus",
  "establishmentmeans" = "establishmentMeans",
  
  # Not a DwC core term, but keep camelCase (and document it as a project term)
  "threatstatus" = "threatStatus"
)

# Rename columns by case-insensitive match, preserving DwC camelCase for known terms.
rename_to_dwc <- function(df, map = dwc_name_map) {
  old <- names(df)
  key <- tolower(old)
  
  new <- old
  hit <- key %in% names(map)
  new[hit] <- unname(map[key[hit]])
  
  names(df) <- new
  df
}


# --- Read ---------------------------------------------------------------------
log_msg("Reading red list distribution (A): ", f_dist)
dist  <- read_table_safe(f_dist)  |> rename_to_dwc()


log_msg("Reading red list taxonomy (B): ", f_taxon)
taxon <- read_table_safe(f_taxon) |> rename_to_dwc()

# --- Save processed inputs -----------------------------------------------------
saveRDS(dist,  out_dist_rds,  compress = "xz")
saveRDS(taxon, out_taxon_rds, compress = "xz")
log_msg("Wrote: ", out_dist_rds)
log_msg("Wrote: ", out_taxon_rds)

# --- Build taxa reference (join A + B) ----------------------------------------
# Prefer join key "id" (your earlier column lists included id in both files).
join_key <- NULL

if ("id" %in% names(dist) && "id" %in% names(taxon)) {
  join_key <- "id"
} else if ("taxonid" %in% names(dist) && "taxonid" %in% names(taxon)) {
  join_key <- "taxonid"
}

if (is.null(join_key)) {
  log_msg("WARNING: Could not find a common join key (id or taxonid).",
          " Writing taxa_reference_current as taxonomy table only.")
  taxa_ref <- taxon
} else {
  log_msg("Joining distribution + taxonomy by: ", join_key)
  
  taxa_ref <- taxon |>
    dplyr::left_join(
      dist,
      by = join_key,
      suffix = c("_taxon", "_dist")
    )
}

# Keep a clean core set if present (does not fail if missing)
core_cols <- c(
  "id", "taxonID", "acceptedNameUsageID", "scientificName",
  "kingdom", "phylum", "class", "order", "family", "genus",
  "taxonRank", "scientificNameAuthorship", "taxonomicStatus",
  "nomenclaturalStatus", "taxonRemarks",
  "countryCode", "occurrenceStatus", "establishmentMeans", "threatStatus"
  
)

keep_cols <- intersect(core_cols, names(taxa_ref))
taxa_ref_clean <- taxa_ref |>
  dplyr::select(dplyr::all_of(keep_cols), dplyr::everything())

saveRDS(taxa_ref_clean, out_taxa_ref, compress = "xz")
log_msg("Wrote: ", out_taxa_ref)

# --- Tiny QA ------------------------------------------------------------------
log_msg("Rows (dist):  ", nrow(dist))
log_msg("Rows (taxon): ", nrow(taxon))
log_msg("Rows (ref):   ", nrow(taxa_ref_clean))

log_msg("Done: red list + taxonomy ingested.")
