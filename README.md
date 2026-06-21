# GBIF Gap Finder

Systematic analysis of spatial, temporal, and taxonomic gaps in national biodiversity occurrence data from GBIF. Designed as a reusable pipeline for any GBIF node — currently configured for **Sweden**.

> See [ROADMAP.Rmd](ROADMAP.Rmd) for the full development plan.

## Overview

This project analyses GBIF occurrence data for a given country to identify:

- **Spatial gaps** — areas with missing or insufficient sampling coverage, filterable by taxonomic group
- **Temporal gaps** — time periods with reduced or absent data collection, with log/linear heatmap views
- **Taxonomic gaps** — species groups under-represented in the data, measured against a national taxonomy backbone
- **Sampling bias** — Troudet-style analysis of taxonomic representation vs. proportional sampling
- **Invasive species** — integration of national invasive species registries with occurrence data
- **Sensitive species** — restricted access species flagged with generalization categories (5/25/50 km)
- **Establishment means** — native, introduced, and invasive species scope filtering and monitoring
- **National taxonomic backbone/All GBIF scope** — toggle between gap analysis (against national backbone) and full GBIF overview
- **Publisher analysis** — which organisations contribute data, single-publisher dependency
- **Recent activity** — rolling 12-month window of observations by event date

The analysis uses EEA reference grids (10 km and 50 km) with EPSG:3035 (ETRS89-LAEA) projection, shared across all European countries. Administrative boundaries from GADM are overlaid on maps for regional context.

## Project Structure

```
gbif_gap_finder/
├── configs/
│   ├── config_SE.yml           # Sweden configuration
│   ├── config_NO.yml           # Norway configuration
│   └── config_template.yml     # Template for new countries
├── R/
│   ├── globals.R               # Config, paths, constants, shared utilities
│   └── packages.R              # Package management (required / optional / app)
├── scripts/
│   ├── 00_setup.R                         # Environment setup
│   ├── 01a_download_raw_data.R            # Download raw data from GBIF/EEA/GADM
│   ├── 01b_resolve_data_sources.R         # Resolve dataset + cube DOIs from GBIF keys
│   ├── 02_ingest_grids.R                  # Process + clip EEA grids
│   ├── 03_ingest_taxonomy.R               # National taxonomy + red list + invasives
│   ├── 04_convert_cubes_parquet.R         # CSV → parquet conversion
│   ├── 05_validate_inputs.R               # QA checks → Markdown report
│   ├── 06a_make_core_summaries.R          # Cell/time/order/publisher summaries
│   ├── 06b_make_species_summaries.R       # Species-level + bias correction
│   ├── 07_spatial_gaps.R                  # Spatial gap analysis
│   ├── 08_temporal_gaps.R                 # Temporal gap analysis
│   ├── 09a_reconcile_taxonomy.R           # GBIF ↔ backbone matching (4-tier)
│   ├── 09b_taxonomic_gaps.R              # Taxonomic gap analysis
│   ├── 09c_scope_summaries.R              # Per-scope summaries + recent-period layer
│   ├── 10_make_gap_overview.R             # Integrated summary tables
│   └── 11_prepare_gap_finder_data.R       # Bundle data for the Gap Finder app
├── analysis/
│   ├── 01_overview.Rmd                    # Dashboard overview report
│   ├── 02_priorities.Rmd                  # Priority actions report
│   ├── 03_spatial_gaps.Rmd                # Spatial coverage analysis
│   ├── 04_temporal_gaps.Rmd               # Temporal trends analysis
│   ├── 05_taxonomic_gaps.Rmd              # Taxonomic coverage analysis
│   ├── 06_species_of_concern.Rmd          # Threatened/invasive/sensitive
│   ├── 07_publishers.Rmd                  # Publisher dependency analysis
│   └── 08_record_types.Rmd                # Basis of record analysis
├── data/
│   ├── shared/
│   │   └── grids/               # EEA grids (Europe-wide, shared)
│   ├── SE/
│   │   ├── raw/                 # Raw downloads (cubes, taxonomy, redlist, invasives, admin)
│   │   ├── proc/                # Processed data (parquet, derived, gaps)
│   │   └── output/              # Summary tables
│   └── NO/                      # Norway (placeholder)
├── docs/
│   ├── data_sources_SE.Rmd      # Sweden data provenance documentation
│   └── data_sources_NO.Rmd      # Norway data provenance documentation
├── shiny_app/
│   ├── gap_finder/              # Gap Finder dashboard
├── _targets.R                   # Pipeline definition
├── run.R                        # Convenience functions
└── ROADMAP.Rmd                  # Development plan
```

## Quick Start

### 1. Configure

Copy `configs/config_template.yml` to `configs/config_SE.yml` (or your country code) and fill in the taxonomy, red list, invasive species, and admin boundary settings.

### 2. Setup

```r
source("scripts/00_setup.R")
```

### 3. Download Data

```r
# Download taxonomy, red list, invasive species registry, admin boundaries
source("scripts/01a_download_raw_data.R")

# GBIF cubes: download via SQL API (instructions printed by script 01a)
# Place cube CSVs in data/{CC}/raw/cubes/

# Resolve dataset + cube DOIs from their GBIF keys (for citations)
source("scripts/01b_resolve_data_sources.R")
```

### 4. Run Pipeline

```r
source("scripts/02_ingest_grids.R")            # Clip grids to country
source("scripts/03_ingest_taxonomy.R")          # Process taxonomy + red list + invasives
source("scripts/04_convert_cubes_parquet.R")    # CSV → parquet
source("scripts/05_validate_inputs.R")          # QA checks
source("scripts/06a_make_core_summaries.R")     # Core + publisher summaries
source("scripts/06b_make_species_summaries.R")  # Species-level summaries
source("scripts/07_spatial_gaps.R")             # Spatial gaps
source("scripts/08_temporal_gaps.R")            # Temporal gaps
source("scripts/09a_reconcile_taxonomy.R")      # GBIF ↔ backbone matching
source("scripts/09b_taxonomic_gaps.R")          # Taxonomic gaps
source("scripts/09c_scope_summaries.R")         # Per-scope summaries + recent period
source("scripts/10_make_gap_overview.R")        # Overview tables
source("scripts/11_prepare_gap_finder_data.R") # Gap Finder data bundle
```

Or use `targets`:

```r
source("run.R")
tar_make()
```

## Data Sources

The pipeline integrates five national data sources (all configured in `configs/config_{CC}.yml`):

| Source | Purpose | Sweden Example |
|--------|---------|----------------|
| National taxonomy backbone | Reference species pool for gap analysis | [Dyntaxa](https://doi.org/10.15468/j43wfc) |
| National red list | Threat status (CR, EN, VU, NT, DD) | [Swedish Red List 2025](https://doi.org/10.15468/zbbyqv) |
| Invasive species register | `is_invasive` flag (species level) | [GRIIS Sweden](https://doi.org/10.15468/i57bff) |
| Sensitive species list | `is_sensitive` flag + generalization category | [Restricted Access Species](https://doi.org/10.15468/jwbtsb) |
| GBIF occurrence cubes | Aggregated occurrence data per grid cell | [SQL API](https://www.gbif.org/occurrence/download/sql) |

Additional shared data:

| Source | Description |
|--------|-------------|
| EEA Reference Grids | 10km + 50km, Europe-wide, EPSG:3035 |
| GADM | Administrative boundaries |

## GBIF Occurrence Cubes

Cubes are downloaded via the **GBIF SQL API** as a single file per grid resolution, containing all basis of record types and publisher/dataset attribution:

```sql
SELECT specieskey, species, kingdom, phylum, class, "order", family,
  basisofrecord, publishingorgkey, datasetkey,
  GBIF_EEARGCode(10000, decimallatitude, decimallongitude, 0) AS eeacellcode,
  "year", "month",
  COUNT(*) AS occurrences
FROM occurrence
WHERE countrycode = 'SE' AND hascoordinate = TRUE
  AND hasgeospatialissues = FALSE AND occurrencestatus = 'PRESENT'
  AND specieskey IS NOT NULL
GROUP BY ...
```

Submit at https://www.gbif.org/occurrence/download/sql. Script 01a prints the full query for your country.

The cube has **14 columns**: `specieskey`, `species`, `kingdom`, `phylum`, `class`, `order`, `family`, `basisofrecord`, `publishingorgkey`, `datasetkey`, `eeacellcode`, `year`, `month`, `occurrences`. See `docs/data_sources_SE.Rmd` for a per-column description.

## Taxonomy Architecture

The pipeline uses the national taxonomy backbone (e.g., Dyntaxa for Sweden) as the primary reference for gap analysis. Every GBIF species is matched to the backbone through a 4-tier reconciliation process:

- **Tier 1** — Direct accepted name match
- **Tier 2** — Synonym resolution via backbone
- **Tier 3** — Infraspecific collapse (subspecies → species)
- **Tier 4** — GBIF Species API lookup

Each species receives three key flags:

- `in_dyntaxa` — whether the species is in the national backbone (gap metrics only apply to these)
- `is_invasive` — whether the species appears on the national invasive species registry
- `is_sensitive` — whether the species is on the restricted access list (coordinates generalized in GBIF)

Script **09c** uses these flags to produce five scope-filtered variants of every cube-based summary (cell, time, order, family, published, recency, spatial gaps, cell-last-year):

- `_all` — all GBIF species (overview)
- `_dyntaxa` — species matched to the national backbone (for gap analysis)
- `_threatened` — Red List species (CR/EN/VU/NT)
- `_invasive` — invasive species registry
- `_sensitive` — restricted access list

The Gap Finder app reads these per-scope files directly, so scope switching in the UI is a lookup, not a computation. The recent-period cutoff is also derived once by 09c (from the data's max yearmonth) and saved as a pipeline constant.

## Adapting for Another Country

1. Copy `configs/config_template.yml` to `configs/config_{CC}.yml`
2. Fill in taxonomy, red list, invasive species, and sensitive species settings (all optional except taxonomy)
3. Set your country: `Sys.setenv(GBIF_GAP_COUNTRY = "CC")`
4. Download cubes via GBIF SQL API (change `countrycode` in the query)
5. Place EEA grids in `data/shared/grids/` (shared, one-time download)
6. Run the pipeline from script 01

## Pipeline Phases

| Phase | Scripts | Description | Runtime |
|-------|---------|-------------|---------|
| Download | 01 | Taxonomy, red list, invasives, admin boundaries | ~5 min |
| Ingestion | 02–04 | Grids, taxonomy processing, CSV → parquet | ~30 min |
| Validation | 05 | QA checks | ~2 min |
| Summaries | 06a, 06b | Core + species summaries (taxonomy-agnostic) | ~45 min |
| Gap Analysis | 07, 08, 09a, 09b | Spatial, temporal, taxonomic gaps | ~30 min |
| Scope + Recent | 09c | Per-scope summaries + recent-period layer | ~20 min |
| Integration | 10 | Overview tables | ~5 min |
| App Prep | 11 | Shiny data bundle | ~10 min |

## Requirements

- R >= 4.1.0
- ~16 GB RAM recommended
- ~20 GB disk space for full pipeline
- Pipeline packages: `sf`, `data.table`, `arrow`, `dplyr`, `scales`, `stringr`, `cli` (see `R/packages.R`)
- Shiny app packages: `shiny`, `plotly`, `leaflet`, `DT`, `ggplot2` (see `app_packages` in `R/packages.R`)
- Full dependency list managed via `renv`

## License

Analysis code: MIT License. Data sources have their own licenses.
