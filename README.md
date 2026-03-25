# GBIF Biodiversity Data Gap Analysis

Systematic analysis of spatial, temporal, and taxonomic gaps in national biodiversity occurrence data from GBIF. Designed as a reusable pipeline for any GBIF node — currently configured for **Sweden**.

> **Package name:** This project is being developed into the `gbifgaps` R package. See [ROADMAP.Rmd](ROADMAP.Rmd) for the full development plan.

## Overview

This project analyses GBIF occurrence data for a given country to identify:

- **Spatial gaps** — areas with missing or insufficient sampling coverage
- **Temporal gaps** — time periods with reduced or absent data collection
- **Taxonomic gaps** — species groups under-represented in the data
- **Sampling bias** — Troudet-style analysis of taxonomic representation vs. proportional sampling
- **Establishment means** — native, introduced, and invasive species scope filtering and monitoring
- **Publisher analysis** — which organisations contribute data, single-publisher dependency
- **Recent activity** — rolling 12-month window: observations dated vs published to GBIF

The analysis uses EEA reference grids (10 km and 50 km) with EPSG:3035 (ETRS89-LAEA) projection, shared across all European countries. Administrative boundaries from GADM are overlaid on maps for regional context.

## Project Structure

```
gbifgaps/
├── configs/
│   ├── config_SE.yml           # Sweden configuration
│   ├── config_NO.yml           # Norway configuration
│   └── config_template.yml     # Template for new countries
├── R/
│   ├── globals.R               # Country-aware paths, constants, config helpers
│   └── packages.R              # Package management
├── scripts/
│   ├── 00_setup.R                         # Environment setup
│   ├── 01_download_raw_data.R             # Download from GBIF/EEA/GADM
│   ├── 02_ingest_grids.R                  # Process + clip EEA grids
│   ├── 03_ingest_taxonomy.R               # National taxonomy + red list
│   ├── 04_convert_cubes_parquet.R         # CSV → parquet conversion
│   ├── 05_validate_inputs.R               # QA checks → Markdown report
│   ├── 06a_make_core_summaries.R          # Cell/time/order/publisher summaries
│   ├── 06b_make_species_summaries.R       # Species-level + bias correction
│   ├── 07_spatial_gaps.R                  # Spatial gap analysis
│   ├── 08_temporal_gaps.R                 # Temporal gap analysis
│   ├── 09a_reconcile_taxonomy.R           # GBIF ↔ backbone matching (4-tier)
│   ├── 09b_taxonomic_gaps.R              # Taxonomic gap analysis
│   ├── 10_make_gap_overview.R             # Integrated summary tables
│   ├── 11_prepare_gap_app_data.R          # Bundle data for Gap Analysis app
│   └── 12_prepare_explorer_app_data.R     # Bundle data for GBIF Explorer app
├── analysis/                    # R Markdown reports (01–08)
├── data/
│   ├── shared/
│   │   └── grids/               # EEA grids (Europe-wide, shared)
│   ├── SE/
│   │   ├── raw/                 # Raw downloads (cubes, taxonomy, redlist, admin)
│   │   ├── proc/                # Processed data (parquet, derived, gaps)
│   │   ├── output/              # Summary tables
│   │   └── data_sources.Rmd     # Data provenance documentation
│   └── NO/                      # Norway (placeholder)
├── shiny_app/
│   ├── gap_app/                 # Gap Analysis dashboard
│   └── gbif_explorer/           # Biodiversity Explorer
├── _targets.R                   # Pipeline DAG definition
├── run.R                        # Convenience functions
└── ROADMAP.Rmd                  # Development plan
```

## Quick Start

### 1. Configure

Copy `configs/config_template.yml` to `configs/config_SE.yml` (or your country code) and fill in the taxonomy, red list, and admin boundary settings.

### 2. Setup

```r
source("scripts/00_setup.R")
```

### 3. Download Data

```r
# Download taxonomy, red list, admin boundaries
source("scripts/01_download_raw_data.R")

# GBIF cubes: download via SQL API (instructions printed by script 01)
# Place cube CSVs in data/{CC}/raw/cubes/
```

### 4. Run Pipeline

```r
source("scripts/02_ingest_grids.R")            # Clip grids to country
source("scripts/03_ingest_taxonomy.R")          # Process taxonomy + red list
source("scripts/04_convert_cubes_parquet.R")    # CSV → parquet
source("scripts/05_validate_inputs.R")          # QA checks
source("scripts/06a_make_core_summaries.R")     # Core + publisher summaries
source("scripts/06b_make_species_summaries.R")  # Species-level summaries
source("scripts/07_spatial_gaps.R")             # Spatial gaps
source("scripts/08_temporal_gaps.R")            # Temporal gaps
source("scripts/09a_reconcile_taxonomy.R")      # GBIF ↔ backbone matching
source("scripts/09b_taxonomic_gaps.R")          # Taxonomic gaps
source("scripts/10_make_gap_overview.R")        # Overview tables
source("scripts/11_prepare_gap_app_data.R")     # Gap app data bundle
source("scripts/12_prepare_explorer_app_data.R") # Explorer app data bundle
```

Or use `targets`:

```r
source("run.R")
tar_make()
```

## GBIF Occurrence Cubes

Cubes are downloaded via the **GBIF SQL API** as a single file per grid resolution, containing all basis of record types, publisher information, and publication dates:

```sql
SELECT specieskey, species, kingdom, phylum, class, "order", family,
  basisofrecord, publishingorgkey, datasetkey,
  GBIF_EEARGCode(10000, decimallatitude, decimallongitude, 0) AS eeacellcode,
  "year", "month",
  YEAR(CAST(lastinterpreted AS TIMESTAMP)) AS year_published,
  MONTH(CAST(lastinterpreted AS TIMESTAMP)) AS month_published,
  COUNT(*) AS occurrences
FROM occurrence
WHERE countrycode = 'SE' AND hascoordinate = TRUE
  AND hasgeospatialissues = FALSE AND occurrencestatus = 'PRESENT'
  AND specieskey IS NOT NULL
GROUP BY ...
```

Submit at https://www.gbif.org/occurrence/download/sql. Script 01 prints the full query for your country.

## Adapting for Another Country

1. Copy `configs/config_template.yml` to `configs/config_{CC}.yml`
2. Fill in taxonomy, red list, and admin boundary settings
3. Set your country: `Sys.setenv(GBIFGAPS_COUNTRY = "CC")`
4. Download cubes via GBIF SQL API (change `countrycode` in the query)
5. Place EEA grids in `data/shared/grids/` (shared, one-time download)
6. Run the pipeline from script 01

## Pipeline Phases

| Phase | Scripts | Description | Runtime |
|-------|---------|-------------|---------|
| Ingestion | 01–04 | Download, process, convert to parquet | ~30 min |
| Validation | 05 | QA checks | ~2 min |
| Summaries | 06a, 06b | Core + species summaries | ~45 min |
| Gap Analysis | 07–09b | Spatial, temporal, taxonomic gaps | ~30 min |
| Integration | 10 | Overview tables | ~5 min |
| App Prep | 11, 12 | Shiny data bundles | ~15 min |

## Data Sources (Sweden)

| Source | Description | DOI |
|--------|-------------|-----|
| GBIF SQL Cubes | 10km: 49M rows, 50km: 26M rows | [10km](https://doi.org/10.15468/dl.g45z9f), [50km](https://doi.org/10.15468/dl.2vhxkh) |
| Dyntaxa | Swedish taxonomic backbone (~110k taxa) | [10.15468/j43wfc](https://doi.org/10.15468/j43wfc) |
| Swedish Red List | Threat status (v1.9) | [10.15468/jhwkpq](https://doi.org/10.15468/jhwkpq) |
| EEA Reference Grids | 10km + 50km, Europe-wide, EPSG:3035 | [EEA](https://sdi.eea.europa.eu/) |
| GADM | Administrative boundaries | [gadm.org](https://gadm.org/) |

## Requirements

- R >= 4.1.0
- ~16 GB RAM recommended
- ~20 GB disk space for full pipeline
- Key packages: `sf`, `data.table`, `arrow`, `dplyr`, `plotly`, `leaflet`, `shiny`, `rgbif`
- Full dependency list managed via `renv`

## License

Analysis code: MIT License. Data sources have their own licenses.
