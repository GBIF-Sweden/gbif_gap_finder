# GBIF Gap Finder

Systematic analysis of spatial, temporal, and taxonomic gaps in national biodiversity occurrence data from GBIF. Designed as a reusable pipeline for any GBIF node — currently configured for **Sweden**.

> See [ROADMAP.Rmd](ROADMAP.Rmd) for the full development plan.
>
> **Using the dashboard?** See the [User Manual](docs/user_manual.md) for how to read each tab and interpret the gaps.

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
├── sql/
│   └── gbif_occurrence_cube.sql # Canonical GBIF SQL cube spec (b3verse; the query IS the cube)
├── _targets.R                   # Pipeline definition
├── run.R                        # Convenience functions
└── ROADMAP.Rmd                  # Development plan
```

## Quick Start

> **Clone with Git LFS.** The prebuilt Shiny data bundle (`shiny_app/gap_finder/data/shiny_data.rds`, ~90 MB) is tracked with [Git LFS](https://git-lfs.com). Install it *before* cloning, or the bundle arrives as a small pointer file instead of the real data:
>
> ```
> git lfs install
> git clone git@github.com:GBIF-Sweden/gbif_gap_finder.git
> # already cloned without LFS? cd into the repo and run: git lfs pull
> ```

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

# GBIF cubes: script 01a renders the canonical SQL (sql/gbif_occurrence_cube.sql) and
# submits it automatically via rgbif::occ_download_sql() when GBIF credentials are set;
# with no credentials it prints the identical query to run by hand at the SQL API.
# Cube CSVs land in data/{CC}/raw/cubes/

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

The cube definition lives in one canonical, version-controlled SQL spec —
`sql/gbif_occurrence_cube.sql` — with `${COUNTRY_CODE}` / `${RESOLUTION}` / `${COL_CHECKLIST_KEY}` placeholders. The
`GROUP BY` query *is* the cube spec, so a cube is fully reproducible from its SQL. Script **01a**
renders it per resolution and **submits the download automatically** via
`rgbif::occ_download_sql()` (→ `occ_download_wait` → `occ_download_import`); with no GBIF
credentials it prints the identical query to run by hand at the
[SQL API](https://www.gbif.org/occurrence/download/sql). Resolved download keys are cached to
`data/{CC}/raw/cubes/cube_download_keys.yml`.

The schema is a **b-cubed–compatible superset** (b3verse, 2026-07): the original GBIF dimensions
plus three aggregate measures, so the cube can also feed `b3gbi::process_cube()`:

```sql
SELECT occurrence.classificationdetails['${COL_CHECKLIST_KEY}']['specieskey'] AS specieskey,
  species, kingdom, phylum, class, "order", family,
  basisofrecord, publishingorgkey, datasetkey,
  GBIF_EEARGCode(${RESOLUTION}, decimallatitude, decimallongitude, 0) AS eeacellcode,
  "year", "month",
  COUNT(*) AS occurrences,
  MIN(COALESCE(coordinateuncertaintyinmeters, 1000)) AS mincoordinateuncertaintyinmeters,
  MIN(GBIF_TemporalUncertainty(eventdate, NULL))      AS mintemporaluncertainty,
  COUNT(DISTINCT recordedby)                          AS distinctobservers
FROM occurrence
WHERE countrycode = '${COUNTRY_CODE}' AND hascoordinate = TRUE
  AND hasgeospatialissues = FALSE AND occurrencestatus = 'PRESENT'
  AND specieskey IS NOT NULL
GROUP BY ...
```

`specieskey` is pinned to GBIF's **Catalogue of Life Extended Release** backbone via the
`classificationdetails['${COL_CHECKLIST_KEY}']` selector, so the cube is COL regardless of GBIF's
mutable default (GBIF completed the COL migration in 2025). COL taxonIDs are usually alphanumeric
(e.g. `6VFN8`) but some are purely numeric (e.g. `67343` = *Anemone nemorosa*) — a numeric key is
**not** a legacy Backbone nub key. The download is automated, so provenance is too: `01a` records the
real download key of each pull to the version-controlled `provenance/cube_downloads_{CC}.yml`, and
`01b` resolves DOI + citation from it (add `cubes.<grid>.download_key` to the config only to pin a
specific historical download). Because
`04` is existence-gated, it re-converts a cube to parquet only when the raw CSV is newer, and `05`
fails the run if a parquet is older than its CSV — so a re-download always propagates downstream.

The cube has **17 columns**: the 14 core fields (`specieskey`, `species`, `kingdom`, `phylum`,
`class`, `order`, `family`, `basisofrecord`, `publishingorgkey`, `datasetkey`, `eeacellcode`,
`year`, `month`, `occurrences`) plus `mincoordinateuncertaintyinmeters`, `mintemporaluncertainty`,
and `distinctobservers`. The grid radius stays 0, so cell assignment is unchanged; the three
measures are additive, so older 14-column cubes still convert (04/05 report them as absent). See
`docs/data_sources_SE.Rmd` for a per-column description.

## Marine coverage (EEZ) — optional

By default the grid universe is terrestrial (the country's land cells plus any cell that carries
data). Set `marine.enabled: true` in `configs/config_{CC}.yml` to bring the country's **Exclusive
Economic Zone** into the grid, so marine coverage and zero-coverage sea gaps are measured too:

```yaml
marine:
  enabled: true            # off by default — land-only grid, unchanged
  zone: "eez"              # "eez" (full EEZ) | "territorial" (12 nm)
  mrgid: 5694              # Marine Regions EEZ gazetteer id (Sweden = 5694)
  force_download: false
```

When enabled, script **02** fetches the EEZ from [Marine Regions](https://marineregions.org) via
the optional `mregions2` package (cached under `data/{CC}/raw/marine/`), widens the country clip to
`centroid ∈ (land ∪ EEZ)`, and tags sea cells with a `marine` flag. Everything downstream measures
against whatever grid `02` writes, so no other script changes. Leave `enabled: false` for land-only
nodes — the grid is then byte-for-byte the old behaviour. For Sweden this surfaces ~138
zero-coverage 10 km sea cells (Baltic / Skagerrak / Kattegat), dropping 10 km coverage from ~100 %
to 97.8 %.

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
- [Git LFS](https://git-lfs.com) — the app data bundle (`shiny_data.rds`) is LFS-tracked
- ~16 GB RAM recommended
- ~20 GB disk space for full pipeline
- Pipeline packages: `sf`, `data.table`, `arrow`, `dplyr`, `scales`, `stringr`, `cli` (see `R/packages.R`)
- Shiny app packages: `shiny`, `plotly`, `leaflet`, `DT`, `ggplot2` (see `app_packages` in `R/packages.R`)
- Optional: `mregions2` — only when `marine.enabled` (fetches the EEZ; see *Marine coverage*)
- Full dependency list managed via `renv`

## License

Analysis code: MIT License. Data sources have their own licenses.
