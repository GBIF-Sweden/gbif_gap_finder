# GBIF Sweden – Data Gap Analysis (Spatial, Temporal, Taxonomic)

This repository contains a reproducible workflow to assess **data gaps in GBIF-mediated biodiversity occurrence data in Sweden**, focusing on:

- **Spatial gaps** (coverage across standardized grid cells)
- **Temporal gaps** (coverage over time)
- **Taxonomic gaps** (coverage across taxa, using reference lists)

The analysis is based on **GBIF Occurrence Cube data** and reference datasets including **EEA grid cells (10 km and 50 km)**, the **Sweden Species Red List**, and **Dyntaxa** as a taxonomic reference.

---

## Project status

✅ Project setup + reproducible structure  
✅ Data ingestion / standardization  
✅ Phase 1 validation  
⏳ Spatial / temporal / taxonomic gap analyses  
⏳ Reporting + figures  

---

## Repository structure

```
gbif_sweden_data_gaps/
├─ analysis/                 # RMarkdown analysis notebooks
├─ scripts/                  # Step-by-step pipeline scripts (run in order)
├─ R/                        # Reusable helper functions + constants
├─ data_raw/                 # External raw downloads (never manually edited)
├─ data_proc/                # Clean/standardized derived datasets
├─ output/                   # Figures, tables, maps (generated)
├─ docs/                     # Extra documentation / notes
├─ literature/               # Literature, papers, etc
├─ logs/                     # Run logs + session info
├─ renv/                     # renv infrastructure
├─ renv.lock                 # Locked package versions for reproducibility
├─ config.yml                # Dataset paths + version-controlled references
└─ README.md                 # This file
```

---

## Data sources (raw inputs)

Raw datasets are stored in `data_raw/` and must **not be edited manually**.

**Main inputs:**
- **GBIF Occurrence Cube** (downloaded last week; GBIF-mediated data for Sweden)
- **EEA grid cells** (10 km and 50 km resolution)
- **Sweden Species Red List** (current downloaded version; expected to be updated)
- **Dyntaxa taxonomy reference** (to be downloaded)

All source metadata (download links, DOIs, filenames, notes) are documented here:

- `data_raw/data_sources.md`

> Note: Raw data may not be committed to Git if files are large and/or redistribution is restricted.

---

## Reproducibility

This project is designed to be reproducible using:

- **Git + GitHub** for version control
- **renv** for locked R package versions
- **config.yml** for version-controlled dataset paths + filenames

### Restore the R environment

After cloning the repository, restore package versions with:

```r
renv::restore()
```

This installs the package versions specified in `renv.lock`.

---

## Configuration (updatable reference datasets)

Dataset file names and locations are managed via:

config.yml

This makes it easy to replace reference datasets (especially updated versions of the Swedish Red List export) without changing scripts.

When a dataset updates:

- Download and place it in the relevant data_raw/... folder

- Update the filename(s) in config.yml

- Re-run the relevant ingestion script(s)

---

## Workflow (run scripts in order)

All scripts should be run from the project root directory.

0) Setup checks (packages, folders, configuration)
```r
source("scripts/00_setup.R")
```
1) Ingest + standardize EEA grids (10 km / 50 km)
```r
source("scripts/01_ingest_grids.R")
```
Outputs written to data_proc/:

- grids_10km.gpkg

- grids_50km.gpkg

- Note: the EEA 50km grid source contains MULTISURFACE geometry and is converted to polygon geometry during ingestion.

2) Ingest + standardize Swedish Red List / taxonomy export
```r
source("scripts/02_ingest_redlist_taxonomy.R")
```
Outputs written to data_proc/:

- redlist_se_distribution_current.rds

- redlist_se_taxon_current.rds

- taxa_reference_current.rds

3) Ingest GBIF Occurrence Cube files
```r
source("scripts/03_ingest_gbif_cubes.R")
```
Outputs written to data_proc/:

- cubes/ (one processed file per cube input)

- cube_manifest.csv

- cube_totals_by_basisOfRecord.csv

Large cube files can be ingested in a lightweight mode (totals computed without full ingestion), depending on script settings.

4) Phase 1 validation (QA checks)
```r
source("scripts/04_validate_inputs.R")
```
Outputs written to logs/:

- validation_report_*.md


---

## Derived data (generated)

All standardized and derived datasets are written to data_proc/.

Current expected outputs include:

- processed grid layers (*.gpkg)

- taxonomic reference (taxa_reference_current.rds)

- processed cube files in data_proc/cubes/ (.fst or .rds, depending on configuration)

- cube manifest and totals tables (*.csv)

- validation reports (logs/validation_report_*.md)


---

## Analyses

Analyses and reporting will be written in `analysis/` as RMarkdown documents.

Entry point:
- `analysis/00_project_overview.Rmd`

---

## Notes / conventions

- Do **not** manually edit anything in `data_raw/`.
- Store generated figures/tables/maps in `output/`.
- Use a consistent CRS across spatial layers (defined in project helpers, e.g. `R/globals.R`).
- Keep all processing steps in scripts so the full workflow can be re-run end-to-end.

---

## Author

Lena Thöle

<!-- badges: start -->
<!-- badges: end -->
