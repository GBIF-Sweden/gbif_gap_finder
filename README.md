# GBIF Sweden – Data Gap Analysis (Spatial, Temporal, Taxonomic)

This repository contains a reproducible workflow to assess **data gaps in GBIF-mediated biodiversity occurrence data in Sweden**, focusing on:

- **Spatial gaps** (coverage across standardized grid cells)
- **Temporal gaps** (coverage over time, recency, and completeness)
- **Taxonomic gaps** (coverage across taxa using reference lists)

The analysis is based on **GBIF Occurrence Cube data** and reference datasets including **EEA grid cells (10 km and 50 km)** and the **Swedish Species Red List**.

---

## Project Status

✅ Project setup + reproducible structure  
✅ Data ingestion / standardization (scripts 01-03)  
✅ Validation (script 04)  
✅ Derived summaries (scripts 05-06)  
✅ **Spatial / temporal / taxonomic gap analyses (scripts 07-09)**  
✅ **Integrated overview & priority lists (script 10)**  
✅ **Analysis notebooks & reporting (RMarkdown)**  

---

## Repository Structure

```
gbif_sweden_data_gaps/
├── analysis/                 # RMarkdown analysis notebooks
│   ├── 01_quick_sanity_checks.Rmd
│   ├── 02_gap_analysis.Rmd          # Exploratory gap analysis
│   └── 03_final_report.Rmd          # Publication-quality report
├── scripts/                  # Step-by-step pipeline scripts (run in order)
│   ├── 00_setup.R                   # Setup & configuration
│   ├── 01_ingest_grids.R            # Ingest EEA grids
│   ├── 02_ingest_redlist_taxonomy.R # Ingest Swedish Red List
│   ├── 03_ingest_gbif_cubes.R       # Ingest GBIF occurrence cubes
│   ├── 04_validate_inputs.R         # Validation checks
│   ├── 05_make_derived_summaries.R  # Create analysis-ready summaries
│   ├── 06_make_grid_lookup.R        # Create grid lookup tables
│   ├── 07_define_spatial_gaps.R     # Spatial gap analysis
│   ├── 08_define_temporal_gaps.R    # Temporal gap analysis
│   ├── 09_define_taxonomic_gaps.R   # Taxonomic gap analysis
│   └── 10_make_gap_overview.R       # Integrated overview & priorities
├── R/                        # Reusable helper functions + constants
├── data_raw/                 # External raw downloads (never manually edited)
│   ├── gbif_occurrence_cubes/
│   ├── eea_grid_10km/
│   ├── eea_grid_50km/
│   └── red_list_se/
├── data_proc/                # Clean/standardized derived datasets
│   ├── grids_10km.gpkg
│   ├── grids_50km.gpkg
│   ├── taxa_reference_current.rds
│   ├── cubes/                # Processed cube files (.fst)
│   ├── derived/              # Analysis-ready summaries
│   └── gaps/                 # Gap analysis outputs (42+ files)
├── output/                   # Generated figures, tables, maps
│   ├── tables/               # Overview tables
│   │   └── integrated/       # Multi-dimensional cross-tabs
│   └── figures/              # Generated plots and maps
├── docs/                     # Documentation
│   ├── data_sources.Rmd      # Data source documentation
│   └── metrics.md            # Gap metrics definitions
├── logs/                     # Run logs + session info
├── renv/                     # renv infrastructure
├── renv.lock                 # Locked package versions
├── config.yml                # Dataset paths + parameters
└── README.md                 # This file
```

---

## Data Sources (Raw Inputs)

Raw datasets are stored in `data_raw/` and must **not be edited manually**.

**Main inputs:**

- **GBIF Occurrence Cube** (downloaded 2026-01-08; GBIF-mediated data for Sweden)
- **EEA grid cells** (10 km and 50 km resolution)
- **Swedish Species Red List** (SLU Artdatabanken 2024, version 1.8)

All source metadata (download links, DOIs, filenames, notes) are documented in:

- `docs/data_sources.Rmd`

> **Note:** Raw data files are not committed to Git due to size and redistribution restrictions.

---

## Reproducibility

This project is designed to be fully reproducible using:

- **Git + GitHub** for version control
- **renv** for locked R package versions
- **config.yml** for version-controlled dataset paths + parameters

### Restore the R Environment

After cloning the repository, restore package versions with:

```r
renv::restore()
```

This installs the exact package versions specified in `renv.lock`.

---

## Configuration (Updatable Reference Datasets)

Dataset file names, locations, and analysis parameters are managed via `config.yml`.

This makes it easy to:
- Replace reference datasets (e.g., updated Swedish Red List)
- Adjust gap metric thresholds
- Update file paths

**When a dataset updates:**

1. Download and place it in the relevant `data_raw/...` folder
2. Update the filename(s) in `config.yml`
3. Re-run the relevant ingestion script(s)

---

## Workflow (Run Scripts in Order)

All scripts should be run from the project root directory.

### Phase 0: Setup

```r
source("scripts/00_setup.R")
```

Checks packages, folders, and configuration.

### Phase 1: Data Ingestion

**1) Ingest + standardize EEA grids (10 km / 50 km)**

```r
source("scripts/01_ingest_grids.R")
```

**Outputs** (written to `data_proc/`):
- `grids_10km.gpkg` (7,693 cells covering Sweden)
- `grids_50km.gpkg` (332 cells, Sweden-domain filtered)

*Note: EEA 50km grid contains MULTISURFACE geometry, converted to POLYGON during ingestion.*

**2) Ingest + standardize Swedish Red List taxonomy**

```r
source("scripts/02_ingest_redlist_taxonomy.R")
```

**Outputs** (written to `data_proc/`):
- `redlist_se_distribution_current.rds`
- `redlist_se_taxon_current.rds`
- `taxa_reference_current.rds` (11,240 taxa with threat status)

**3) Ingest GBIF Occurrence Cube files**

```r
source("scripts/03_ingest_gbif_cubes.R")
```

**Outputs** (written to `data_proc/`):
- `cubes/` (one processed `.fst` file per cube input, 18 files total)
- `cube_manifest.csv`
- `cube_totals_by_basisOfRecord.csv`

**4) Phase 1 validation (QA checks)**

```r
source("scripts/04_validate_inputs.R")
```

**Outputs** (written to `logs/`):
- `validation_report_*.md`

---

### Phase 2: Derived Summaries

**5) Create analysis-ready derived datasets**

```r
source("scripts/05_make_derived_summaries.R")
```

**Outputs** (written to `data_proc/derived/`):

**Spatial summaries:**
- `cell_summary_10km.csv` (occurrences per cell by basis of record)
- `cell_summary_50km.csv`

**Temporal summaries:**
- `time_summary_10km.csv` (occurrences per month by basis of record)
- `time_summary_50km.csv`

**Taxonomic summaries:**
- `species_summary_10km.csv` (occurrences per species by basis of record)
- `species_summary_50km.csv`
- `family_time_summary_10km.csv` (family × time)
- `family_time_summary_50km.csv`
- `order_time_summary_10km.csv` (order × time)
- `order_time_summary_50km.csv`

**Cube overview:**
- `cube_key_summary.csv`

**6) Create grid lookup tables**

```r
source("scripts/06_make_grid_lookup.R")
```

**Outputs** (written to `data_proc/derived/`):
- `grid_lookup_10km.csv` (links polygon IDs to eeacellcode)
- `grid_lookup_50km.csv`

---

### Phase 3: Gap Analysis

**7) Spatial gap analysis**

```r
source("scripts/07_define_spatial_gaps.R")
```

**Outputs** (written to `data_proc/gaps/`, 7 files):

- `spatial_gaps_10km.csv` (cell-level detail with gap flags)
- `spatial_gaps_50km.csv`
- `spatial_thresholds_by_basis.csv` (quantile thresholds: q05, q10, q25)
- `spatial_summary_by_basis.csv` (aggregated by grid × basis)
- `spatial_summary_by_grid.csv` (overall grid-level summary)
- `spatial_zero_coverage_cells.csv` (priority cells with no data)
- `spatial_low_coverage_cells_q10.csv` (undersampled cells)

**8) Temporal gap analysis**

```r
source("scripts/08_define_temporal_gaps.R")
```

**Outputs** (written to `data_proc/gaps/`, 21 files):

**National trends:**
- `temporal_overview_year_10km.csv` (annual totals)
- `temporal_overview_month_10km.csv` (monthly patterns)
- `temporal_year_by_basis_10km.csv` (year × basis)
- `temporal_month_by_basis_10km.csv` (month × basis)
- `temporal_year_month_10km.csv` (year × month heatmap data)
- `temporal_decade_summary_10km.csv`
- *(+ 50km equivalents for all above)*

**Completeness:**
- `temporal_year_completeness_10km.csv` (which years have all 12 months?)
- `temporal_month_completeness_10km.csv` (how many years per month?)
- *(+ 50km equivalents)*

**Gaps:**
- `temporal_gap_years_detail.csv` (years with zero data by basis)
- `temporal_gap_years_summary.csv`

**Recency:**
- `cell_recency_10km.csv` (last observation per cell per basis)
- `cell_recency_50km.csv`
- `temporal_sampling_frequency_summary.csv`

**Taxonomic × temporal:**
- `temporal_year_by_family_10km.csv` (if family data available)
- `temporal_year_by_family_50km.csv`

**9) Taxonomic gap analysis**

```r
source("scripts/09_define_taxonomic_gaps.R")
```

**Outputs** (written to `data_proc/gaps/`, 14 files):

**Core matching:**
- `taxonomic_match_table.csv` (full matching details)
- `taxonomic_match_summary.csv` (per-taxon summary)
- `taxonomic_missing_taxa.csv` (taxa not in GBIF)
- `taxonomic_missing_threatened.csv` (threatened taxa missing)

**Coverage analysis:**
- `taxonomic_gap_summary.csv` (rank × threat cross-tab)
- `taxonomic_coverage_by_rank.csv`
- `taxonomic_coverage_by_threat.csv`
- `taxonomic_coverage_by_basis.csv` (which basis covers which taxa?)

**Spatial coverage:**
- `taxonomic_spatial_coverage.csv` (cells per taxon)
- `taxonomic_threatened_spatial_coverage.csv` (threatened species locations)

**Higher taxonomy:**
- `taxonomic_gaps_by_family.csv`
- `taxonomic_gaps_by_order.csv`

**Priorities:**
- `taxonomic_priority_taxa.csv` (4,758 taxa requiring attention)

---

### Phase 4: Integrated Overview

**10) Create integrated overview tables**

```r
source("scripts/10_make_gap_overview.R")
```

**Outputs** (written to `output/tables/` and `output/tables/integrated/`):

**Standard overview tables** (`output/tables/`, 10 files):
- `dashboard_summary.csv` (single-row executive summary)
- `comparison_grid_resolutions.csv`
- `comparison_basis_types.csv`
- `comparison_taxon_ranks.csv`
- `overview_spatial_gap_rates.csv`
- `overview_temporal_year.csv`
- `overview_temporal_month.csv`
- `overview_temporal_recency_rates.csv`
- `overview_taxonomic_summary.csv`
- `overview_missing_taxa.csv`

**Integrated multi-dimensional tables** (`output/tables/integrated/`, 8 files):
- `space_time_basis_10km.csv` (spatial coverage over time by basis)
- `space_time_basis_50km.csv`
- `space_taxonomy_simple_10km.csv` (species richness by basis)
- `space_taxonomy_basis_10km.csv` (by taxonomic rank)
- `time_taxonomy_basis_10km.csv` (temporal trends by rank)
- `priority_zero_coverage_cells.csv` (cells with NO data)
- `priority_undersampled_taxa.csv` (threatened species needing surveys)
- `priority_stale_cells.csv` (cells not sampled in 5+ years)

---

## Analysis & Reporting

### Exploratory Analysis

Quick sanity checks (tables + plots):

```r
rmarkdown::render("analysis/01_quick_sanity_checks.Rmd")
```

Comprehensive gap analysis diagnostics:

```r
rmarkdown::render("analysis/02_gap_analysis.Rmd")
```

**Features:**
- Dashboard summary
- Spatial coverage maps
- Temporal trends (annual, seasonal)
- Data recency analysis
- Taxonomic coverage by rank
- Priority areas and taxa

### Final Report (Publication Quality)

```r
rmarkdown::render("analysis/03_final_report.Rmd")
```

**Features:**
- Executive summary with status indicators
- Professional tables (`{gt}` formatting)
- High-resolution maps
- Threatened species analysis
- Priority recommendations
- Complete data citations

**Output:** `analysis/03_final_report.html`

---

## Gap Metrics Summary

All gap metrics are defined in detail in `docs/metrics.md`.

**Key metrics:**

**Spatial:**
- Zero coverage (cells with 0 occurrences)
- Low coverage (bottom 5%, 10%, 25% quantiles)
- Coverage by basis of record

**Temporal:**
- Year and month completeness
- Data staleness (cells not sampled in 1 year / 5 years)
- Gap years (years with zero observations)
- Sampling frequency

**Taxonomic:**
- Coverage by taxonomic rank
- Coverage by IUCN threat status
- Missing taxa (not in GBIF)
- Spatial coverage per taxon
- Priority threatened species

---

## Key Results

**Based on current analysis:**

- **Spatial:** 100% coverage (10km), 100% coverage (50km)
- **Temporal:** 380 year range (1646–2025)
- **Taxonomic:** 52.8% coverage (5,936 / 11,240 taxa)
- **Priority Taxa:** 4,758 threatened/undersampled taxa requiring attention
- **Data Recency:** 23.5% of cells not sampled in 5+ years

**Total Gap Analysis Outputs:** 60+ files across 4 phases

---

## Notes / Conventions

- **Never** manually edit anything in `data_raw/`
- Store generated figures/tables in `output/`
- All processing steps in scripts → full workflow can be re-run end-to-end
- Consistent CRS: SWEREF99 TM (EPSG:3006)
- All gap metrics parameterized in `config.yml`

---

## Citation

When using this analysis, please cite:

**Data sources:**
- GBIF Occurrence Cubes: https://doi.org/10.15468/dl.wzv3uc (10km), https://doi.org/10.15468/dl.qyp3uw (50km)
- SLU Artdatabanken (2024). The Swedish Red List 2020. Version 1.8. https://doi.org/10.15468/jhwkpq
- EEA Reference Grids: https://sdi.eea.europa.eu/geonetwork/

**Software:**
- R 4.x with packages: sf, dplyr, ggplot2, data.table, fst, gt, viridis

---

## Author

Lena Thöle

**Analysis Date:** 2026-01-26
