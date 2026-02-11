# GBIF Biodiversity Data Gap Analysis

Systematic analysis of spatial, temporal, and taxonomic gaps in national biodiversity occurrence data from GBIF. Designed as a reusable pipeline for any GBIF node — currently configured for **Sweden**.

> **Package name:** This project is being developed into the `gbifgaps` R package. See [ROADMAP.md](ROADMAP.md) for the full development plan.

## Overview

This project analyses GBIF occurrence data for a given country to identify:

- **Spatial gaps** — areas with missing or insufficient sampling coverage
- **Temporal gaps** — time periods with reduced or absent data collection
- **Taxonomic gaps** — species groups under-represented in the data

The analysis uses EEA reference grids (10 km and 50 km) with EPSG:3035 (ETRS89-LAEA) projection. Grid systems are configurable for non-European countries.

## Project Structure

```
gbif_gap_analysis/
├── config.yml              # Central configuration (country, paths, params)
├── .Renviron               # API credentials (not in git)
├── R/
│   ├── globals.R           # Project paths, constants, config helpers
│   └── packages.R          # Package management
├── scripts/
│   ├── 00_setup.R                           # Environment setup
│   ├── 01_download_raw_data.R               # Download from GBIF/EEA
│   ├── 02_ingest_grids.R                    # Process EEA grids
│   ├── 03_ingest_taxonomy.R                 # National taxonomy + red list
│   ├── 04_ingest_gbif_cubes.R              # Convert cubes to FST
│   ├── 05_validate_inputs.R                 # QA checks → Markdown report
│   ├── 06a_make_core_summaries.R            # Cell/time/order summaries
│   ├── 06b_make_species_summaries_*.R       # Species-level + bias correction
│   ├── 07_spatial_gaps.R                    # Spatial gap analysis
│   ├── 08_temporal_gaps.R                   # Temporal gap analysis
│   ├── 09_taxonomic_gaps.R                  # Taxonomic gap analysis
│   ├── 10_make_gap_overview.R               # Integrated summary tables
│   ├── 11_prepare_gap_app_data.R            # Bundle data for Gap Analysis app
│   └── 12_prepare_explorer_app_data.R       # Bundle data for GBIF Explorer app
├── analysis/                # R Markdown reports
│   ├── 01_sanity_checks.Rmd            # Data validation & QA
│   ├── 02_spatial_gaps.Rmd             # Spatial gap analysis
│   ├── 03_temporal_gaps.Rmd            # Temporal gap analysis
│   ├── 04_taxonomic_gaps.Rmd           # Taxonomic gap analysis
│   ├── 05_basis_of_record.Rmd          # Basis of record analysis
│   └── 06_integrated_report.Rmd        # Combined assessment
├── data_raw/                # Raw input data (not in git)
├── data_proc/               # Processed data (derived/, gaps/)
├── output/                  # Summary tables and figures
├── docs/                    # Metrics definitions and documentation
└── shiny_app/               # Interactive dashboards (gap_app + gbif_explorer)
```

## Quick Start

### 1. Configure

Edit `config.yml` to set your country, taxonomy DOIs, and file paths. The default configuration is for Sweden (Dyntaxa + Swedish Red List).

### 2. Setup

```r
source("scripts/00_setup.R")
```

### 3. Data Preparation

```r
# Download raw data (requires GBIF credentials in .Renviron)
source("scripts/01_download_raw_data.R")

# Process inputs
source("scripts/02_ingest_grids.R")
source("scripts/03_ingest_taxonomy.R")
source("scripts/04_ingest_gbif_cubes.R")

# Validate
source("scripts/05_validate_inputs.R")
```

### 4. Create Summaries

```r
# Core summaries (~10-30 min)
source("scripts/06a_make_core_summaries.R")

# Species-level summaries (~30-60 min)
source("scripts/06b_make_species_summaries_highmem.R")
```

### 5. Gap Analysis

```r
source("scripts/07_spatial_gaps.R")
source("scripts/08_temporal_gaps.R")
source("scripts/09_taxonomic_gaps.R")
source("scripts/10_make_gap_overview.R")
```

### 6. App Data

```r
source("scripts/11_prepare_gap_app_data.R")      # Gap Analysis app
source("scripts/12_prepare_explorer_app_data.R")  # GBIF Explorer app
```

## Pipeline Phases

| Phase | Scripts | Description | Approx. Runtime |
|-------|---------|-------------|-----------------|
| 1. Ingestion | 01–04 | Download and process raw data | ~1–2 hours |
| 2. Validation | 05 | Check data integrity | ~5 min |
| 3. Summaries | 06a, 06b | Create analysis-ready tables | ~1–2 hours |
| 4. Gap Analysis | 07–09 | Identify spatial/temporal/taxonomic gaps | ~30 min |
| 5. Integration | 10 | Combined overview tables | ~10 min |
| 6. Gap App Prep | 11 | Bundle data for Gap Analysis app | ~5 min |
| 7. Explorer Prep | 12 | Bundle data for GBIF Explorer app | ~10 min |

## Adapting for Another Country

1. Copy `config.yml` and update the `country`, `taxonomy`, and `redlist` sections
2. Point `paths` to your raw data directories
3. Update `files` with your actual file names
4. Adjust `parameters.crs` if not using EEA grids (EPSG:3035)
5. Run the pipeline from script 01

See `ROADMAP.md` → Phase 2 for the full abstraction plan.

## Configuration

All settings live in `config.yml`. Key parameters:

```yaml
country:
  code: "SE"
  name: "Sweden"

parameters:
  crs: 3035                          # ETRS89-LAEA (EEA standard)
  processing:
    large_order_threshold: 500000    # Split large orders by family
    low_memory_mode: false           # Set true for <16 GB RAM
```

## Requirements

- R >= 4.1.0
- ~16 GB RAM recommended (8 GB minimum with `low_memory_mode: true`)
- ~50 GB disk space for full pipeline

## Data Sources (Sweden defaults)

| Source | Description | DOI |
|--------|-------------|-----|
| GBIF Occurrence Cubes | Species data on EEA grids | [10km](https://doi.org/10.15468/dl.wzv3uc), [50km](https://doi.org/10.15468/dl.qyp3uw) |
| Dyntaxa | Swedish taxonomic backbone | [10.15468/j43wfc](https://doi.org/10.15468/j43wfc) |
| Swedish Red List | Threat status | [10.15468/jhwkpq](https://doi.org/10.15468/jhwkpq) |
| EEA Reference Grids | 10 km and 50 km in EPSG:3035 | [EEA GeoNetwork](https://sdi.eea.europa.eu/) |

## License

Analysis code: MIT License. Data sources have their own licenses — see `docs/`.

## Contact

For questions, open an issue or see [ROADMAP.md](ROADMAP.md) for contribution plans.
