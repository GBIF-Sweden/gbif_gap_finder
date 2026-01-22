# scripts/09_define_taxonomic_gaps.R
# Phase 3: Taxonomic gap metrics
#
# Inputs:
#   Phase 2:
#     - data_proc/derived/species_summary_10km.csv
#     - data_proc/derived/species_summary_50km.csv
#   Reference taxonomy:
#     - data_proc/taxa_reference_current.rds  (from scripts/02_ingest_redlist_taxonomy.R)
#
# Outputs:
#   - data_proc/gaps/taxonomic_match_table.csv
#   - data_proc/gaps/missing_taxa.csv
#   - data_proc/gaps/missing_threatened_taxa.csv
#   - data_proc/gaps/taxonomic_gap_summary.csv

source("scripts/00_setup.R")

# ---- Dependencies -------------------------------------------------------------
pkgs <- c("data.table", "readr", "dplyr", "stringr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) stop("Missing package: ", p)

# ---- Paths -------------------------------------------------------------------
data_proc_rel <- cfg_get("paths.data_proc", "data_proc")
p_data_proc <- here::here(data_proc_rel)

p_derived <- file.path(p_data_proc, cfg_get("derived.dir", "derived"))
p_gaps <- file.path(p_data_proc, "gaps")
dir.create(p_gaps, showWarnings = FALSE, recursive = TRUE)

# ---- Inputs ------------------------------------------------------------------
sp10_name <- cfg_get("derived.outputs.species_summary_10km", "species_summary_10km.csv")
sp50_name <- cfg_get("derived.outputs.species_summary_50km", "species_summary_50km.csv")

sp10_path <- file.path(p_derived, sp10_name)
sp50_path <- file.path(p_derived, sp50_name)

if (!file.exists(sp10_path)) stop("Missing: ", sp10_path)
if (!file.exists(sp50_path)) stop("Missing: ", sp50_path)

tax_ref_path <- file.path(p_data_proc, "taxa_reference_current.rds")
if (!file.exists(tax_ref_path)) stop("Missing: ", tax_ref_path)

# ---- Helpers -----------------------------------------------------------------
standardize_name <- function(x) {
  x <- tolower(as.character(x))
  x <- stringr::str_trim(x)
  x <- stringr::str_squish(x)
  # drop authorship-like trailing parts very conservatively (optional):
  # keep only first two tokens (Genus species) if it looks like a binomial
  parts <- stringr::str_split(x, "\\s+", simplify = TRUE)
  is_binom <- ncol(parts) >= 2 & stringr::str_detect(parts[, 1], "^[a-z]+$") & stringr::str_detect(parts[, 2], "^[a-z-]+$")
  out <- x
  out[is_binom] <- paste(parts[is_binom, 1], parts[is_binom, 2])
  out
}

read_species_set <- function(path) {
  dt <- data.table::fread(path)
  # expected columns: grid, basisofrecord, specieskey, (species), occurrences
  # keep basisofrecord == "all" to define "present in GBIF cube"
  dt <- dt[basisofrecord == "all"]
  if (!("species" %in% names(dt))) stop("Expected 'species' column in: ", path)
  dt[, species_std := standardize_name(species)]
  # unique species strings (and keys)
  dt <- unique(dt[, .(specieskey, species, species_std)])
  dt
}

# ---- Load data ----------------------------------------------------------------
cube_species_10 <- read_species_set(sp10_path)
cube_species_50 <- read_species_set(sp50_path)

cube_species <- data.table::rbindlist(
  list(
    cube_species_10[, .(grid = "grid10km", specieskey, species, species_std)],
    cube_species_50[, .(grid = "grid50km", specieskey, species, species_std)]
  ),
  use.names = TRUE, fill = TRUE
)
cube_species <- unique(cube_species)

tax_ref <- readRDS(tax_ref_path)
tax_ref <- data.table::as.data.table(tax_ref)

need_ref <- c("taxonID", "scientificName", "taxonRank", "threatStatus")
miss_ref <- setdiff(need_ref, names(tax_ref))
if (length(miss_ref)) stop("Missing columns in taxa_reference_current.rds: ", paste(miss_ref, collapse = ", "))

tax_ref[, scientific_std := standardize_name(scientificName)]

# ---- Matching: reference scientific name -> cube species label ----------------
# We define presence if the standardized names match.
# This is conservative and inspectable.
match_tbl <- merge(
  tax_ref[, .(taxonID, scientificName, scientific_std, taxonRank, threatStatus)],
  cube_species[, .(grid, specieskey, species, species_std)],
  by.x = "scientific_std",
  by.y = "species_std",
  all.x = TRUE,
  allow.cartesian = TRUE
)

match_tbl[, matched := !is.na(specieskey)]

# Summarize per taxonID whether it matched in any grid
match_summary <- match_tbl[, .(
  matched_any = any(matched),
  matched_grid10 = any(matched & grid == "grid10km"),
  matched_grid50 = any(matched & grid == "grid50km")
), by = .(taxonID, scientificName, taxonRank, threatStatus, scientific_std)]

# Missing taxa = not matched in any grid
missing_taxa <- match_summary[matched_any == FALSE]

# Missing threatened taxa (CR/EN/VU) – adjust if your threatStatus coding differs
threatened_codes <- c("CR", "EN", "VU")
missing_threatened <- missing_taxa[threatStatus %in% threatened_codes]

# Summary table by rank and threat status
gap_summary <- match_summary[, .(
  n_ref = .N,
  n_matched_any = sum(matched_any),
  n_missing = sum(!matched_any),
  pct_missing = round(100 * sum(!matched_any) / .N, 2)
), by = .(taxonRank, threatStatus)][order(taxonRank, threatStatus)]

# ---- Write outputs ------------------------------------------------------------
out_match <- file.path(p_gaps, "taxonomic_match_table.csv")
out_missing <- file.path(p_gaps, "missing_taxa.csv")
out_miss_thr <- file.path(p_gaps, "missing_threatened_taxa.csv")
out_summary <- file.path(p_gaps, "taxonomic_gap_summary.csv")

data.table::fwrite(match_tbl, out_match)
data.table::fwrite(missing_taxa, out_missing)
data.table::fwrite(missing_threatened, out_miss_thr)
data.table::fwrite(gap_summary, out_summary)

log_msg("Wrote: ", out_match)
log_msg("Wrote: ", out_missing)
log_msg("Wrote: ", out_miss_thr)
log_msg("Wrote: ", out_summary)
log_msg("Phase 3 (taxonomic) complete.")
