# GBIF Gap Finder

Systematic analysis of spatial, temporal, and taxonomic gaps in national biodiversity occurrence data from GBIF. Designed as a reusable pipeline for any GBIF node — currently configured for **Sweden**.

> **Live dashboard (Sweden):** <https://gbif.se/gap-finder/>
>
> See [ROADMAP.Rmd](ROADMAP.Rmd) for the full development plan.
>
> **Using the dashboard?** See the [User Manual](docs/user_manual.md) for how to read each tab and interpret the gaps.
>
> **Summary report:** the main figures of every tab on one page, to print or share —
> <https://gbif.se/gap-finder/gap_finder_report.html> (see [Summary report](#summary-report)).

## Overview

This project analyses GBIF occurrence data for a given country to identify:

- **Spatial gaps** — areas with missing or insufficient sampling coverage, filterable by taxonomic group
- **Temporal gaps** — time periods with reduced or absent data collection, with log/linear heatmap views
- **Taxonomic gaps** — species groups under-represented in the data, measured against a national taxonomy backbone
- **Sampling bias** — Troudet-style analysis of taxonomic representation vs. proportional sampling
- **Invasive species** — integration of national invasive species registries with occurrence data
- **Sensitive species** — restricted access species flagged with generalization categories (5/25/50 km)
- **Establishment means** — native, introduced, and invasive species scope filtering and monitoring
- **Backbone-relative and all-GBIF views** — the Taxonomic and Species of Concern tabs measure against the national backbone; the occurrence-based tabs report all of GBIF
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
│   ├── packages.R              # Package management (required / optional / app)
│   ├── report_helpers.R        # Bundle path + header stamp for the summary report
│   ├── eea_grid.R              # EEA grid cell codes from coordinates (time points)
│   ├── historic_io.R           # Reading historical snapshot deliveries (time points)
│   └── closure.R               # Gap-closure arithmetic (Gaps filled tab)
├── scripts/
│   ├── 00_preflight.R                     # Check every external dependency (gates tar_make)
│   ├── 00_setup.R                         # Environment setup
│   ├── 01a_download_raw_data.R            # Download raw data from GBIF/EEA/GADM
│   ├── 01b_resolve_data_sources.R         # Resolve dataset + cube DOIs from GBIF keys
│   ├── 02_ingest_grids.R                  # Process + clip EEA grids
│   ├── 03_ingest_taxonomy.R               # National taxonomy + red list + invasives
│   ├── 04_convert_cubes_parquet.R         # CSV → parquet conversion
│   ├── 04b_build_historical_cubes.R       # Historical snapshots → time-point cubes
│   ├── 05_validate_inputs.R               # QA checks → Markdown report
│   ├── 06a_make_core_summaries.R          # Cell/time/order/publisher summaries
│   ├── 06b_make_species_summaries.R       # Species-level + bias correction
│   ├── 07_spatial_gaps.R                  # Spatial gap analysis
│   ├── 08_temporal_gaps.R                 # Temporal gap analysis
│   ├── 09a_reconcile_taxonomy.R           # GBIF ↔ backbone matching (5-tier)
│   ├── 09a1_build_col_crosswalk.R         # Dyntaxa ↔ CoL key crosswalk (feeds 09a Tier 5)
│   ├── 09b_taxonomic_gaps.R               # Taxonomic gap analysis
│   ├── 09c_scope_summaries.R              # Per-scope summaries + recent-period layer
│   ├── 10_make_gap_overview.R             # Integrated summary tables
│   ├── 11_prepare_gap_finder_data.R       # Bundle data for the Gap Finder app
│   ├── 12_reconcile.R                     # Cross-layer reconciliation guardrail
│   ├── 13_metrics_snapshot.R              # Refresh the figures in docs/metrics.md
│   └── 14_gap_closure.R                   # Gap closure between time points
├── analysis/
│   └── gap_finder_report.Rmd              # Summary report, one section per app tab
├── data/
│   ├── shared/
│   │   └── grids/               # EEA grids (Europe-wide, shared)
│   ├── SE/
│   │   ├── raw/                 # Raw downloads (cubes, taxonomy, redlist, invasives, admin)
│   │   ├── proc/                # Processed data (parquet, derived, gaps, time points)
│   │   └── output/              # Summary tables
│   └── NO/                      # Norway (placeholder)
├── docs/
│   ├── user_manual.md           # How to read each tab
│   ├── metrics.md               # Gap metric definitions + current figures
│   ├── CHANGELOG.md             # Release history
│   ├── data_sources_SE.Rmd      # Sweden data provenance documentation
│   └── data_sources_NO.Rmd      # Norway data provenance documentation
├── provenance/                  # Cube download keys + upstream release versions (auto-written)
├── shiny_app/
│   └── gap_finder/              # Gap Finder dashboard (app.R, Dockerfile, per-country data/,
│                                #   www/ incl. the rendered summary report)
├── sql/
│   └── gbif_occurrence_cube.sql # Canonical GBIF SQL cube spec (b3verse; the query IS the cube)
├── tools/                       # Version stamping + tests
├── _targets.R                   # Pipeline definition
├── run.R                        # Convenience functions
├── run_timepoint.R              # Run scripts 05–10 for one historical time point
└── ROADMAP.Rmd                  # Development plan
```

## Quick Start

> **Clone with Git LFS.** The prebuilt Shiny data bundle (`shiny_app/gap_finder/data/{CC}/shiny_data.rds`, ~80 MB) is tracked with [Git LFS](https://git-lfs.com). Install it *before* cloning, or the bundle arrives as a small pointer file instead of the real data:
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
source("scripts/09a1_build_col_crosswalk.R")    # Backbone ↔ CoL crosswalk (Tier 5)
source("scripts/09a_reconcile_taxonomy.R")      # GBIF ↔ backbone matching
source("scripts/09b_taxonomic_gaps.R")          # Taxonomic gaps
source("scripts/09c_scope_summaries.R")         # Per-scope summaries + recent period
source("scripts/10_make_gap_overview.R")        # Overview tables
source("scripts/11_prepare_gap_finder_data.R") # Gap Finder data bundle
source("scripts/12_reconcile.R")                # Cross-tab consistency checks
source("scripts/13_metrics_snapshot.R")         # Refresh docs/metrics.md
```

`scripts/00_preflight.R` checks every external dependency first (it gates `tar_make()`;
`PREFLIGHT_OFFLINE=1` skips the network checks), and `scripts/01b_resolve_data_sources.R`
resolves dataset DOIs and writes `provenance/`.

Or use `targets`:

```r
source("run.R")
tar_make()
```

### 5. Check the results

A run has not regressed when these hold:

- `git diff docs/metrics.md` — script 13 rewrites the current figures on every run; only the
  dates should change unless the data did.
- `git diff provenance/` — shows whether anything upstream (backbone, red list, CoL release)
  moved.
- `data/{CC}/proc/gaps/col_crosswalk_validation.md` — script 09a1 compares its own matching
  figures against `parameters.taxonomic.crosswalk_baseline` in the country config and prints
  `REGRESSED` when one drops. Update that baseline deliberately after a verified rebuild.
  These are crosswalk health checks, not the published figures: e.g. "threatened not reached by
  the crosswalk" (152) is higher than the official "missing threatened" (122) because it counts
  all ranks and ignores the name-matching tiers.
- `scripts/12_reconcile.R` (the `reconciliation` target) — Overview, Taxonomic and Concern agree.

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
[SQL API](https://www.gbif.org/occurrence/download/sql). Resolved download keys are recorded to the
version-controlled `provenance/cube_downloads_{CC}.yml` (see below).

The schema is a **b-cubed–compatible superset** (b3verse): the original GBIF dimensions
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
mutable default. COL taxonIDs are usually alphanumeric
(e.g. `6VFN8`) but some are purely numeric (e.g. `67343` = *Anemone nemorosa*) — a numeric key is
**not** a legacy Backbone nub key. The download is automated, so provenance is too: `01a` records the
real download key of each pull to the version-controlled `provenance/cube_downloads_{CC}.yml`, and
`01b` resolves DOI + citation from it (add `cubes.<grid>.download_key` to the config only to pin a
specific historical download). `04` re-converts a cube to parquet whenever the raw CSV is newer,
and `05` fails the run if a parquet is older than its CSV — so a re-download always propagates
downstream.

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

In the app, the Spatial tab's **Coverage area** toggle switches between *Land + sea*, *Land only*
and *Sea only*. It governs the Spatial map and statistics, the Overview coverage figures and the
Priorities zero/stale cells. Script 11 sorts every 10 km cell into three groups: *land* (centroid
on Swedish land), *sea* (off land and in the Swedish EEZ, or in a coastal gap between the land and
EEZ outlines) and *outside* (foreign land along the Norwegian/Finnish border and foreign waters
beyond the EEZ, kept in the grid because they carry data). Land only shows land, Sea only shows sea,
and outside cells appear only in Land + sea. For Sweden: 4,490 land + 1,564 sea + 254 outside =
6,308 cells.

## Taxonomy Architecture

The pipeline uses the national taxonomy backbone (e.g., Dyntaxa for Sweden) as the primary reference for gap analysis. Every GBIF species is matched to the backbone through a 5-tier reconciliation process:

- **Tier 1** — Direct accepted name match
- **Tier 2** — Synonym resolution via backbone
- **Tier 3** — Infraspecific collapse (subspecies → species)
- **Tier 4** — GBIF Species API lookup
- **Tier 5** — Dyntaxa ↔ Catalogue of Life key crosswalk (built by `09a1`, matched on the cube's CoL `specieskey`)

Each species receives three key flags:

- `in_dyntaxa` — whether the species is in the national backbone (gap metrics only apply to these)
- `is_invasive` — whether the species appears on the national invasive species registry
- `is_sensitive` — whether the species is on the restricted access list (coordinates generalized in GBIF)

Script **09c** uses these flags to produce four scope-filtered variants of every cube-based summary (cell, time, cell × time, order × cell, order × time, family × time, recency, record-type recent period, spatial gaps, cell-last-year), per grid:

- `_all` — all GBIF species
- `_threatened` — Red List species in `threatened_categories` (config; CR/EN/VU/NT for Sweden)
- `_invasive` — invasive species registry
- `_sensitive` — restricted access list

There is no backbone scope. The occurrence-based tabs (Spatial, Temporal, Record Types, Publisher) use all GBIF species; the Taxonomic and Concern tabs measure against the backbone through the 09a/09b match, not through a scope-filtered summary. `in_dyntaxa` is still computed but produces no scope files.

The Gap Finder app reads these per-scope files directly, so scope switching in the UI is a lookup, not a computation. The recent-period cutoff is also derived once by 09c (from the data's max yearmonth) and saved as a pipeline constant.

## Adapting for Another Country

1. Copy `configs/config_template.yml` to `configs/config_{CC}.yml`
2. Fill in taxonomy, red list, invasive species, and sensitive species settings (all optional except taxonomy)
3. Set your country: `Sys.setenv(GBIF_GAP_COUNTRY = "CC")`
4. Download cubes via GBIF SQL API (change `countrycode` in the query)
5. Place EEA grids in `data/shared/grids/` (shared, one-time download)
6. Run the pipeline from script 01
7. After the first verified run, set `parameters.taxonomic.crosswalk_baseline` from
   `col_crosswalk_validation.md` so later runs are checked against it

## App colours and filters

- **Palette rule** (defined once, near the top of `shiny_app/gap_finder/app.R`):
  categorical charts use the Paul Tol colours in `pal`; maps of counts and recency use
  sequential viridis (pale = few or old records, dark = many or recent, grey = no data);
  diverging RdYlBu is kept for values above/below an expected level.
- **Taxonomic filters** go kingdom → phylum → class → order → family on the Temporal,
  Taxonomic and Species of Concern tabs. Spatial filters kingdom → class → order (script 09c
  writes `order_cell_recency_<grid>.csv`, ~620k rows / ~2 MB in the bundle) and applies to the
  Occurrences and Data recency maps. Publishers filter kingdom → class → order, plus a publisher-category filter that also drives the dependency map. Family on Spatial or
  Publishers would need a family × cell layer, which was measured and left out (~110 MB at 10 km).
- **Establishment means** are grouped by `estab_group()` in `app.R` (reintroduced natives count
  as native); a value Dyntaxa adds later shows up as "Other" instead of disappearing.
- **Threatened** means the config's `threatened_categories` (CR / EN / VU / NT), passed to
  the app in the bundle metadata. Data Deficient (DD) is listed next to them in the Concern
  tables but never counted as threatened.

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
| Report | `analysis/gap_finder_report.Rmd` | Summary report into the app's `www/` (manual, see below) | ~1 min |

## Summary report

`analysis/gap_finder_report.Rmd` renders one HTML page that follows the app's tabs — Overview,
Priorities, Spatial, Temporal, Taxonomic, Species of concern, Publishers, Record types, Data &
sources — with each tab's main figure at the app's default settings. It reads the same bundle
as the app and computes every number the same way, so the two agree. The header shows the data
date (from the bundle), the render date and the Gap Finder version.

The page is written to `shiny_app/gap_finder/www/gap_finder_report.html` and committed, so the
image ships it and the app links to it from the Overview citation card
(<https://gbif.se/gap-finder/gap_finder_report.html>). Re-render it whenever the bundle
changes, before tagging a release:

```r
Sys.setenv(GAP_FINDER_VERSION = "1.0.0")   # the release it ships with; omit for "dev"
rmarkdown::render("analysis/gap_finder_report.Rmd",
                  output_dir = "shiny_app/gap_finder/www")
# or: tar_invalidate(report); tar_make(names = report)
```

## Deployment

The Shiny app ships as a container image. A `v*` tag push builds and publishes it;
nothing is deployed automatically from this repository.

**What a tag push does** (`.github/workflows/publish-shiny-images.yml`):

1. `validate-release` checks the app, Dockerfile and data bundle are present, and that
   `shiny_data.rds` is the real ~90 MB file rather than an un-fetched Git LFS pointer.
2. `publish` builds `shiny_app/gap_finder/Dockerfile.gap_finder` for `linux/amd64` and
   pushes to GHCR as `ghcr.io/gbif-sweden/gap-finder`, tagged with the git tag,
   `latest`, and `sha-<commit>`.
3. `.github/workflows/citation-version.yml` writes the version and date into
   `CITATION.cff` on the default branch as a follow-up commit — so `git pull` after a
   release, or the next push is rejected as non-fast-forward.

**Build arguments** (CI supplies both; a local build gets the defaults):

| Arg | Default | Effect |
|-----|---------|--------|
| `GBIF_GAP_COUNTRY` | `SE` | Which config and data bundle are baked in |
| `GAP_FINDER_VERSION` | `dev` | What the app reports as its version; CI passes the git tag |
| `APT_SNAPSHOT` | `20261006T000000Z` | Ubuntu archive snapshot the system libraries (GDAL/GEOS/PROJ, curl, openssl…) are installed from. Pinned so rebuilds are identical; security fixes arrive only when it moves. Bump the default in the Dockerfile at each release |

**Runtime environment:**

| Variable | Default | Effect |
|----------|---------|--------|
| `GBIF_GAP_COUNTRY` | baked at build | Selects the bundle at `data/{CC}/shiny_data.rds` |
| `GAP_FINDER_VERSION` | baked at build | Shown in the About panel; `dev` when unset |
| `GAP_FINDER_BASEMAP` | `Esri.WorldGrayCanvas` | Any `leaflet.providers` name. An unknown name warns and falls back rather than rendering a blank map. Do not use a `CartoDB.*` provider: CARTO raster basemaps now require an API key and render an "API KEY REQUIRED" watermark |
| `GAP_FINDER_SHOW_GAPS_FILLED` | `false` | `true` shows the **Gaps filled** tab. Off by default while the tab is under review; the closure data stays in the bundle either way |

Run it locally:

```bash
docker run --rm -p 3838:3838 ghcr.io/gbif-sweden/gap-finder:latest
# then open http://localhost:3838
```

**The last hop is manual and lives outside this repo.** The public instance at
<https://gbif.se/gap-finder/> is updated by a GBIF Sweden colleague pulling the
published image onto the NRM server. A green build therefore does NOT mean the change
is live — confirm the deploy separately, and say which tag should be pulled.

> **To be added once the server maintainer sends it:** the run command or service
> definition used on the NRM server (image tag pulled, port, environment variables, number
> of instances). Until then, ask GBIF Sweden how the public instance is run.

## Requirements

- R >= 4.1.0
- [Git LFS](https://git-lfs.com) — the app data bundle (`shiny_data.rds`) is LFS-tracked
- ~16 GB RAM recommended
- ~20 GB disk space for full pipeline
- Pipeline packages: `sf`, `data.table`, `arrow`, `dplyr`, `scales`, `stringr`, `cli` (see `R/packages.R`)
- Shiny app packages: `shiny`, `plotly`, `leaflet`, `DT`, `ggplot2` (see `app_packages` in `R/packages.R`)
- Optional: `mregions2` — only when `marine.enabled` (fetches the EEZ; see *Marine coverage*)
  - `mregions2` pulls in `redland`, which needs the Redland C libraries at the OS level.
    `renv::restore()` installs it regardless of `marine.enabled`, so a clean restore needs them:
    macOS `brew install redland`; Ubuntu/Debian `librdf0-dev` (build) and `librdf0t64` (runtime,
    `librdf0` before 24.04). The Docker image installs both.
- Full dependency list managed via `renv`

## How to cite

Please cite the Gap Finder as:

> Thöle, L. M., Holston, K. C., Shah, M., & Johansson, V. GBIF Gap Finder: a reproducible
> pipeline for biodiversity data gap analysis. GBIF Sweden, Swedish Museum of Natural History
> (NRM). <https://gbif.se/gap-finder/>

Authors:

- Lena M. Thöle — <https://orcid.org/0000-0002-5405-3613>
- Kevin C. Holston — <https://orcid.org/0000-0002-0786-4069>
- Manash Shah — <https://orcid.org/0000-0002-9607-9512>
- Veronika Johansson — <https://orcid.org/0000-0002-3028-9947>

Version and release date are in [CITATION.cff](CITATION.cff); GitHub's **Cite this repository**
button turns it into APA or BibTeX. Please also cite the GBIF downloads and checklists behind
the figures — their DOIs are listed on the app's **Data & sources** tab.

## License

Analysis code: MIT License. Data sources have their own licenses.
