# GBIF Sweden – Data Gap Analysis (Spatial, Temporal, Taxonomic)

This repository contains a reproducible workflow to assess **data gaps in GBIF-mediated biodiversity occurrence data in Sweden**, focusing on:

- **Spatial gaps** (coverage across standardized grid cells)
- **Temporal gaps** (coverage over time)
- **Taxonomic gaps** (coverage across taxa, using reference lists)

The analysis is based on **GBIF Occurrence Cube data** and reference datasets including **EEA grid cells (10 km and 50 km)**, the **Sweden Species Red List**, and **Dyntaxa** as a taxonomic reference.

---

## Project status

✅ Project setup + reproducible structure  
⏳ Data ingestion / standardization  
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

## Data sources (raw data)

Raw datasets are stored in `data-raw/` and should **not be edited manually**.

**Main inputs:**
- **GBIF Occurrence Cube** (downloaded last week; GBIF-mediated data for Sweden)
- **EEA grid cells** (10 km and 50 km resolution)
- **Sweden Species Red List** (current downloaded version; expected to be updated)
- **Dyntaxa taxonomy reference** (to be downloaded)

All raw datasets and download metadata (source, download date, license, filenames, notes) are documented in:

- `data_raw/data_sources.Rmd`

> Note: Raw data may not be committed to Git if files are large and/or redistribution is restricted.

---

## Reproducibility

This project is designed to be as reproducible as possible using:

- **Git + GitHub** for version control
- **renv** for locked R package versions
- **usethis** for a clean, standard project structure

### Restore the R environment

After cloning the repository, restore package versions using:

```r
renv::restore()
```

This installs the package versions specified in `renv.lock`.

---

## Configuration (updatable reference datasets)

Dataset file names and locations are managed via:

- `config.yml`

This makes it easy to replace reference datasets (especially the **Red List** and **Dyntaxa**) without changing analysis code.

When a new Red List version becomes available:
1. Download and place it in `data-raw/red_list/`
2. Update the Red List filename (or path) entry in `config.yml`
3. Re-run the ingestion script (see below)

The same approach will be used for updated Dyntaxa exports.

---

## Workflow (run scripts in order)

All scripts should be run from the project root directory.

### 1) Setup checks (packages, folders, configuration)

```r
source("scripts/00_setup.R")
```

### 2) Ingest + standardize EEA grids (10 km / 50 km)

```r
source("scripts/01_ingest_grids.R")
```

### 3) Ingest + standardize Sweden Red List

```r
source("scripts/02_ingest_redlist.R")
```

### 4) Ingest + validate GBIF Occurrence Cube

```r
source("scripts/03_ingest_gbif_cube.R")
```

### 5) (Once available) Ingest + standardize Dyntaxa

```r
source("scripts/04_ingest_dyntaxa.R")
```

---

## Derived data (generated)

Scripts write standardized, analysis-ready datasets to `data/` (file formats may vary depending on input formats):

- `data/grids_10km.gpkg`
- `data/grids_50km.gpkg`
- `data/grids_50km.gpkg`
- `data/red_list_current.rds`
- `data/dyntaxa_current.rds` *(once available)*
- Occurrence Cube derived summaries (defined during ingestion)

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
