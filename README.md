# gbifgaps — GBIF Biodiversity Data Gap Analysis

Identify spatial, temporal, and taxonomic gaps in national GBIF occurrence data. Designed as a reusable pipeline for any GBIF node — currently configured for Sweden, with a template and Norway example for adaptation.

## What It Does

Takes a country's GBIF occurrence cubes + national taxonomy and produces:

- **Spatial gaps** — grid cells with missing or low sampling coverage (10 km and 50 km EEA grids)
- **Temporal gaps** — time periods with reduced data collection, staleness maps, seasonal patterns
- **Taxonomic gaps** — species groups under-represented relative to the national checklist, with threatened species prioritisation
- **Integrated overview** — combined priority tables identifying where, when, and what to sample next
- **Interactive dashboards** — Shiny app for exploring results

## Quick Start

```r
# 1. Edit config.yml (see "Adapting for Another Country" below)

# 2. Setup
source("scripts/00_setup.R")

# 3. Download + process raw data
source("scripts/01_download_raw_data.R")
source("scripts/02_ingest_grids.R")
source("scripts/03_ingest_taxonomy.R")
source("scripts/04_ingest_gbif_cubes.R")

# 4. Validate
source("scripts/05_validate_inputs.R")

# 5. Create summaries (~1–2 hours)
source("scripts/06a_make_core_summaries.R")
source("scripts/06b_make_species_summaries_highmem.R")

# 6. Gap analysis (~30 min)
source("scripts/07_spatial_gaps.R")
source("scripts/08_temporal_gaps.R")
source("scripts/09_taxonomic_gaps.R")
source("scripts/10_make_gap_overview.R")

# 7. Prepare Shiny dashboard
source("scripts/11_prepare_shiny_data.R")
```

Or use `{targets}` for dependency-tracked execution:

```r
library(targets)
tar_make()        # runs everything
tar_visnetwork()  # visualise the pipeline DAG
```

## Adapting for Another Country

The pipeline is country-agnostic. All country-specific settings live in `config.yml`.

**To set up a new country:**

1. **Copy the template:** `cp config_template.yml config.yml`
2. **Set your country** — code, name, timezone
3. **Find your national taxonomy** on GBIF
   - Go to [gbif.org/dataset/search?type=CHECKLIST](https://www.gbif.org/dataset/search?type=CHECKLIST)
   - Filter by your country's publishing organisation
   - Copy the dataset key (UUID) and DwC-A download URL
4. **Find your national red list** on GBIF (or set `redlist.enabled: false`)
5. **Request GBIF occurrence cubes** filtered to your country code
6. **Download EEA reference grids** clipped to your country from [EEA](https://sdi.eea.europa.eu/)
7. **Update file names** in `config.yml` to match your downloads
8. **Run the pipeline** from script 01

See `config_template.yml` for detailed instructions on each field, and `config_norway.yml` for a worked example using Norway's Nortaxa taxonomy.

**What might need code changes:** If your national taxonomy DwC-A has a non-standard structure (unusual column names, no Distribution.csv, different ID format), you may need to adjust `scripts/03_ingest_taxonomy.R`. All other scripts work on the standardised intermediate files and require no changes.

## Project Structure

```
gbifgaps/
├── config.yml                  ← Central configuration (edit this)
├── config_template.yml         ← Annotated template for new countries
├── config_norway.yml           ← Worked example for Norway
├── _targets.R                  ← Pipeline definition (delegates to scripts)
├── run.R                       ← Convenience helpers (status, run phases)
├── R/
│   ├── globals.R               ← Paths, constants, cfg_get() helper
│   └── packages.R              ← Package loading + helpers
├── scripts/
│   ├── 00_setup.R              ← Environment setup
│   ├── 01_download_raw_data.R  ← Download from GBIF / EEA
│   ├── 02_ingest_grids.R       ← Process EEA grids → .gpkg
│   ├── 03_ingest_taxonomy.R    ← National taxonomy + red list → .rds
│   ├── 04_ingest_gbif_cubes.R  ← CSV cubes → .fst
│   ├── 05_validate_inputs.R    ← QA checks → Markdown report
│   ├── 06a_make_core_summaries.R       ← Cell / time / order aggregates
│   ├── 06b_make_species_summaries_*.R  ← Species-level + bias correction
│   ├── 07_spatial_gaps.R       ← Zero- and low-coverage cells
│   ├── 08_temporal_gaps.R      ← Trends, recency, seasonality
│   ├── 09_taxonomic_gaps.R     ← Backbone coverage, threatened species
│   ├── 10_make_gap_overview.R  ← Integrated priority tables
│   └── 11_prepare_shiny_data.R ← Bundle for Shiny app
├── analysis/                   ← R Markdown reports (HTML output)
├── shiny_app/                  ← Interactive dashboards
├── data_raw/                   ← Raw downloads (not tracked in git)
├── data_proc/                  ← Processed intermediates
└── output/                     ← Final summary tables
```

## Pipeline Phases

| Phase | Scripts | What it does | Runtime |
|-------|---------|-------------|---------|
| Ingestion | 01–04 | Download and process raw data | ~1–2 h |
| Validation | 05 | Check data integrity | ~5 min |
| Summaries | 06a, 06b | Aggregate cubes into analysis tables | ~1–2 h |
| Gap Analysis | 07–09 | Spatial, temporal, taxonomic gaps | ~30 min |
| Integration | 10 | Combined overview + priority lists | ~10 min |
| Shiny Prep | 11 | Bundle data for dashboards | ~5 min |
| Reports | analysis/*.Rmd | HTML reports (optional, manual) | ~10 min |

## Requirements

- **R ≥ 4.1.0**
- **RAM:** 16 GB recommended. Set `parameters.processing.low_memory_mode: true` in config for 8 GB.
- **Disk:** ~50 GB for full pipeline (mostly cube CSVs and .fst files)
- **GBIF account** for requesting occurrence downloads ([gbif.org](https://www.gbif.org))

Key R packages: `data.table`, `sf`, `fst`, `dplyr`, `cli`, `yaml`, `here`, `targets`. Full list in `R/packages.R`.

## Data Sources (Sweden defaults)

| Source | Description | DOI |
|--------|-------------|-----|
| GBIF Occurrence Cubes | Species data on EEA grids | [10km](https://doi.org/10.15468/dl.wzv3uc), [50km](https://doi.org/10.15468/dl.qyp3uw) |
| Dyntaxa | Swedish taxonomic backbone | [10.15468/j43wfc](https://doi.org/10.15468/j43wfc) |
| Swedish Red List | Threat status | [10.15468/jhwkpq](https://doi.org/10.15468/jhwkpq) |
| EEA Reference Grids | 10 km and 50 km, EPSG:3035 | [EEA](https://sdi.eea.europa.eu/) |

## License

Analysis code: MIT License. Data sources have their own licenses — see individual DOIs.

## Contributing

Open an issue for bugs or feature requests. Pull requests welcome, especially for new country configurations.
