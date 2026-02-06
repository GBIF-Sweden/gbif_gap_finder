# GBIF Sweden Data Gap Analysis

Systematic analysis of spatial, temporal, and taxonomic gaps in Swedish biodiversity occurrence data from GBIF.

## Overview

This project analyzes GBIF occurrence data for Sweden to identify:

- **Spatial gaps**: Areas with missing or insufficient sampling coverage
- **Temporal gaps**: Time periods with reduced or missing data collection  
- **Taxonomic gaps**: Species groups that are under-represented in the data

The analysis uses EEA reference grids (10km and 50km) with EPSG:3035 (ETRS89-LAEA) projection.

## Project Structure

```
gbif_sweden_data_gaps/
├── config.yml              # Central configuration
├── .Renviron               # API credentials (not in git)
├── R/
│   ├── globals.R           # Project paths and constants
│   └── packages.R          # Package management
├── scripts/
│   ├── 00_setup.R                      # Environment setup
│   ├── 01_download_raw_data.R          # Download from GBIF/sources
│   ├── 02_ingest_grids.R               # Process EEA grids
│   ├── 03_ingest_taxonomy.R            # Process Dyntaxa + Red List
│   ├── 04_ingest_gbif_cubes.R          # Convert cubes to FST
│   ├── 05_validate_inputs.R            # Validate all inputs
│   ├── 06a_make_core_summaries.R       # Cell/time/order summaries
│   ├── 06b_make_species_summaries.R    # Species-level with bias correction
│   ├── 07_spatial_gaps.R               # Spatial gap analysis
│   ├── 08_temporal_gaps.R              # Temporal gap analysis
│   ├── 09_taxonomic_gaps.R             # Taxonomic gap analysis
│   └── 10_integrated_overview.R        # Combined summary tables
├── analysis/
│   ├── 01_sanity_checks.Rmd            # Data quality checks
│   ├── 02_spatial_gaps.Rmd             # Spatial gap analysis report
│   ├── 03_temporal_gaps.Rmd            # Temporal gap analysis report
│   ├── 04_taxonomic_gaps.Rmd           # Taxonomic gap analysis report
│   └── 05_integrated_report.Rmd        # Combined findings
├── data_raw/                           # Raw input data (not in git)
│   ├── gbif_occurrence_cubes/
│   ├── eea_grid_10km/
│   ├── eea_grid_50km/
│   ├── dyntaxa/
│   └── red_list_se/
├── data_proc/                          # Processed data
│   ├── derived/                        # Script 06a/06b outputs
│   │   ├── cell_summary_*.csv
│   │   ├── time_summary_*.csv
│   │   ├── by_order/                   # Species summaries by order
│   │   └── by_family/                  # Species summaries by family (large orders)
│   └── gaps/                           # Script 07-09 outputs
├── output/
│   ├── tables/                         # Summary tables
│   │   └── integrated/                 # Multi-dimensional summaries
│   └── figures/                        # Generated figures
└── docs/                               # Documentation
```

## Quick Start

### 1. Setup

```r
# Install required packages
source("R/packages.R")
install_missing(check_missing(required_packages))

# Load environment
source("scripts/00_setup.R")
```

### 2. Data Preparation

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

### 3. Create Summaries

```r
# Core summaries (fast: ~10-30 min)
source("scripts/06a_make_core_summaries.R")

# Species-level summaries (slower: ~30-60 min with fast mode)
source("scripts/06b_make_species_summaries_highmem.R")
```

### 4. Gap Analysis

```r
source("scripts/07_spatial_gaps.R")
source("scripts/08_temporal_gaps.R")
source("scripts/09_taxonomic_gaps.R")
source("scripts/10_integrated_overview.R")
```

### 5. Reports

```r
# Knit analysis reports
rmarkdown::render("analysis/01_sanity_checks.Rmd")
rmarkdown::render("analysis/02_spatial_gaps.Rmd")
rmarkdown::render("analysis/03_temporal_gaps.Rmd")
rmarkdown::render("analysis/04_taxonomic_gaps.Rmd")
rmarkdown::render("analysis/05_integrated_report.Rmd")
```

## Pipeline Phases

| Phase | Scripts | Description | Runtime |
|-------|---------|-------------|---------|
| 1. Ingestion | 01-04 | Download and process raw data | ~1-2 hours |
| 2. Validation | 05 | Check data integrity | ~5 min |
| 3. Summaries | 06a, 06b | Create analysis-ready tables | ~1-2 hours |
| 4. Gap Analysis | 07-09 | Identify gaps | ~30 min |
| 5. Integration | 10 | Combined overview | ~10 min |
| 6. Reports | Rmd files | Generate HTML reports | ~15 min |

## Key Outputs

### Derived Summaries (data_proc/derived/)

| File | Description |
|------|-------------|
| `cell_summary_*.csv` | Occurrences/species per grid cell |
| `time_summary_*.csv` | Occurrences/species per year-month |
| `cell_time_summary_*.csv` | Cell × time matrix |
| `order_*_summary_*.csv` | Order-level aggregations |
| `by_order/species_*.csv` | Species-level with bias correction |
| `by_family/species_*.csv` | Large orders split by family |

### Gap Analysis (data_proc/gaps/)

| File | Description |
|------|-------------|
| `spatial_gaps_*.csv` | Cells with coverage gaps |
| `temporal_gap_years_*.csv` | Years/periods with data gaps |
| `taxonomic_missing_*.csv` | Species missing from GBIF |

## Bias Correction

Species-level summaries include bias correction columns:

- `familycount`, `ordercount`, `classcount`: Total occurrences for taxonomic group
- `relative_family`, `relative_order`, `relative_class`: Normalized occurrence rates

Use `relative_*` columns to account for uneven sampling effort across taxonomy.

## Configuration

Edit `config.yml` to customize:

- File paths
- Grid resolutions (10km, 50km)
- Gap thresholds
- Processing parameters

Key parameters:

```yaml
parameters:
  crs: 3035                          # ETRS89-LAEA (EEA standard)
  processing:
    large_order_threshold: 500000    # Split large orders by family
    low_memory_mode: false           # Set true for <16GB RAM
```

## Requirements

- R >= 4.1.0
- ~16GB RAM recommended (8GB minimum with low_memory_mode)
- ~50GB disk space for full pipeline

## Data Sources

- **GBIF Occurrence Cubes**: Species occurrence data aggregated to EEA grid cells
- **Dyntaxa**: Swedish taxonomic backbone with species checklist
- **Swedish Red List**: Threat status for Swedish species
- **EEA Reference Grids**: 10km and 50km grids in EPSG:3035

## License

This analysis code is provided under MIT License.  
Data sources have their own licenses - see `docs/data_sources.Rmd`.

## Contact

For questions about this analysis, please open an issue.
