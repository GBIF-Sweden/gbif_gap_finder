# gbifgaps — Changelog

## 2026-04-17: Pipeline Housekeeping — Redundancy Cleanup & Bug Fixes

### Bug Fixes

- **CRITICAL: `is_accepted` silently dropped taxa with NA `acceptedNameUsageID`**
  — In 09a and 09b, `is_accepted := (taxonID == acceptedNameUsageID)` evaluates
  to `NA` when `acceptedNameUsageID` is empty/NA (common in many DwC-A checklists
  for accepted taxa). These taxa vanished from *both* the accepted and synonym
  pools, silently reducing the matching universe. Fixed with new
  `classify_accepted()` in globals.R that uses `taxonomicStatus` as the primary
  signal and falls back to the ID comparison, treating NA/empty
  `acceptedNameUsageID` as accepted. (Scripts 09a, 09b)

- **SIGNIFICANT: Threat status resolution lost data via column-level selection**
  — In 09b, `tax_accepted[, threatStatus := get(primary_threat)]` picked the
  first non-null *column name*, not the first non-NA *value per row*. A taxon
  with `threatStatus_redlist = NA` but `threatStatus_backbone = "VU"` would
  lose the "VU". Fixed with new `resolve_threat_status()` using
  `data.table::fcoalesce()` to pick the first non-NA value across columns
  per row. (Scripts 09b, 09c)

- **MINOR: Yearmonth construction had no NA guard in 06a** — `year * 100L +
  as.integer(month)` produces garbage when year or month is NA. The consolidated
  `read_cube()` in globals.R now uses `fifelse(!is.na(year) & !is.na(month), ...)`
  consistently across all cube-reading scripts.

- **MINOR: Script 11 `add_yearmonth_cols()` wastefully overwrote year/month**
  — 09c already adds year and month columns to time summaries. Fixed to skip
  if both columns already exist.

### Redundancy Cleanup

- **Consolidated cube reader** — Four variants of "read parquet cube into
  data.table" (`read_cube` in 06a, `read_cube_cols` in 08, `load_cube_scoped`
  in 09c, `read_cube_sample` in 05) unified into a single `read_cube()` in
  globals.R with parameters for column selection, grid labeling, and taxonomy
  recoding. Script 09c's `load_cube_scoped` is now a thin wrapper.

- **Consolidated file readers** — New `read_derived_summary()` and `safe_read()`
  in globals.R replace duplicated `read_cell_summary()` (07),
  `read_time_summary()` (08), `safe_read_gap()` / `safe_read_derived()` (10),
  and `safe_read()` (11). Scripts 07 and 08 use thin wrappers.

- **Single `%||%` definition** — Removed six redundant definitions
  (01, 04, 06a, 09a, 09c, 11); one definition in globals.R.

- **Single `clean_for_filename()`** — Moved from 06b to globals.R.

- **Package loading centralised** — `00_setup.R` now calls `load_packages()`
  from packages.R. Individual scripts stripped of redundant `library()` calls
  (~70 removed), keeping only script-specific extras (arrow, httr, httr2,
  rgbif, geodata).

- **`packages.R` restructured** — Split into `required_packages` (pipeline),
  `optional_packages` (enhancing), `app_packages` (Shiny/Rmd). Added `scales`
  to required (used in every script via `scales::comma()`). Moved ggplot2,
  viridis, knitr, gt, DT, shinyWidgets, rmarkdown, forcats to app_packages.
  Moved `arrow` to optional. `load_packages()` is no longer dead code.

- **Directory creation centralised** — `ensure_dirs()` now called from
  `00_setup.R` (added `p_logs`, `p_cubes` to the directory list). Removed
  ~15 scattered `dir.create()` calls from individual scripts. Only dynamic
  per-taxon directories (06b) and the shiny output directory (11) retain
  their own `dir.create()`.

- **Stale `exists()` guards removed** — `raw_invasives_dir` and
  `raw_sensitive_dir` fallback guards in 01, 03, and run.R removed since
  globals.R always defines them.

### Files Modified
- `R/globals.R` — added `read_cube()`, `read_derived_summary()`, `safe_read()`,
  `clean_for_filename()`, `classify_accepted()`, `resolve_threat_status()`,
  `%||%`; added `p_logs` and `p_cubes` to `ensure_dirs()`
- `R/packages.R` — restructured package lists, `load_packages()` now includes
  `scales`
- `scripts/00_setup.R` — calls `load_packages()` and `ensure_dirs()`
- `scripts/01–11` — stripped redundant library calls, removed duplicate helper
  functions, applied bug fixes
- `_targets.R` — synced `tar_option_set(packages)` with restructured packages.R
- `run.R` — removed stale `exists()` guards in `status()`

### Net Effect
- globals.R grew +197 lines (new shared utilities)
- Individual scripts collectively shrank -267 lines
- Total: -70 lines, significantly reduced maintenance surface
- No changes to pipeline outputs or data schemas

---

## 2026-04-16: Scope Summaries & Recent-Period Layer (09c refactor)

### Architectural Change
Consolidated all scope-filtered and recent-period computations into a new
script **09c_scope_summaries.R**. This is a cleanup of the previous dual-scope
approach in 06a and the section 5d cube work in 11, both of which duplicated
taxonomy plumbing and spread the recent-period cutoff logic across multiple
files. After this refactor:

- **06a/06b** are taxonomy-agnostic — pure cube aggregations, no more
  `taxa_reference_current.rds` loading, no `tag_cube_taxonomy()`,
  no `write_dual_scope()`.
- **09a** still produces the 4-tier reconciliation with `in_dyntaxa`,
  `is_invasive`, `is_sensitive` flags. Tier 4 API now runs in batches
  (outer `while` loop) rather than a single-shot cap — cache persists at
  the end of every batch. Default `api_max_batches = Inf` (runs to
  completion); set to a finite value for partial runs.
- **09c (NEW)** — sibling of 09b, depends on 09a. For each of five scopes
  (`all`, `dyntaxa`, `threatened`, `invasive`, `sensitive`) and each grid
  resolution, produces: cell/time/cell-time/order-time/family-time/
  published-time/order-published-time/family-published-time summaries,
  plus the recent-period layer (cell_recency, basis_recent,
  spatial_gaps zero-filled, cell_last_year). Also writes
  `tax_cell_recency` (kingdom × class × cell, not scope-filtered),
  `recent_cutoff.rds` (pipeline constant), and `species_scope_summary.csv`
  (per-specieskey flag lookup for app runtime filtering).
- **11** — pure loader, down from 1700 → ~940 lines. No cube loading,
  no heavy computation. Loads pre-computed scope summaries under
  `<scope>_<summary_type>` names and provides backward-compat aliases
  (e.g. `time_summary_10km` → `all_time_summary`). Troudet bias now uses
  the order-level and class-level published splits from 09c
  (previously `pub_last_year`/`pub_prior` were computed per-row inline
  from the cube).

### New Features
- **Five-scope summaries** replacing the old dual-scope (full + dyntaxa)
  system. `threatened`, `invasive`, `sensitive` scopes are now first-class
  and pre-computed, enabling the new Species of Concern tab without runtime
  filtering.
- **Recent-period cutoff as pipeline constant** — derived once by 09c from
  the cube's max yearmonth, saved to `data/{CC}/proc/recent_cutoff.rds`.
  Script 11 and any downstream consumer reads this file rather than
  recomputing.
- **Order-level and family-level published-time summaries** — enable
  Troudet bias "published" toggle at class/order granularity. Previously
  `pub_last_year` / `pub_prior` were NA in the class/order bias tables.
- **Batched Tier 4 API queries in 09a** — `api_max_batches` config knob;
  cache persists between batches.

### Outputs Added (per scope × per grid, in `data/{CC}/proc/derived/`)
- `cell_summary_<scope>_<grid>.csv`
- `time_summary_<scope>_<grid>.csv`
- `cell_time_summary_<scope>_<grid>.csv`
- `order_cell_summary_<scope>_<grid>.csv`
- `order_time_summary_<scope>_<grid>.csv`
- `family_time_summary_<scope>_<grid>.csv`
- `published_time_summary_<scope>_<grid>.csv`
- `order_published_time_summary_<scope>_<grid>.csv`
- `family_published_time_summary_<scope>_<grid>.csv`
- `cell_recency_<scope>_<grid>.csv`
- `basis_recent_<scope>_<grid>.csv`
- `spatial_gaps_<scope>_<grid>.csv` (zero-filled against grid)
- `cell_last_year_<scope>_<grid>.csv`

Plus (not scope-filtered):
- `tax_cell_recency_<grid>.csv` (kingdom × class × cell)
- `species_scope_summary.csv`
- `recent_cutoff.rds`

### Outputs Removed
- `cell_summary_dyntaxa_10km.csv`, `time_summary_dyntaxa_10km.csv` etc. —
  replaced by the scope-suffix variants from 09c (the `_dyntaxa` scope).

### Files Modified
- `scripts/06a_make_core_summaries.R` — removed taxonomy lookup,
  `tag_cube_taxonomy()`, `write_dual_scope()`. Back to pure aggregation.
- `scripts/06b_make_species_summaries.R` — same treatment.
- `scripts/09a_reconcile_taxonomy.R` — batched Tier 4 loop (+41 lines).
- `scripts/09c_scope_summaries.R` — NEW (705 lines).
- `scripts/11_prepare_gap_app_data.R` — rewritten as loader (~940 lines).
- `_targets.R` — `scope_summaries` target added (sibling of
  `taxonomic_gaps`); `gap_app_data` now depends on both.
- `run.R` — `run_phase_4()` includes `scope_summaries`.
- `README.md` — Quick Start, Pipeline Phases, Taxonomy Architecture.
- `ROADMAP.Rmd`, `derived_summaries_overview.Rmd`, `metrics.md` — updated
  to reflect new architecture.

### Pipeline Re-run Order
Full re-run from 09a is required (09a output unchanged in schema, but
06a/06b outputs changed due to removed dual-scope, and 09c is brand new).
Suggested: rerun 06a → 06b → 09a → (09b, 09c) → 10 → 11.

### Known Limitation
`tax_by_order` / `tax_by_family` in the app are still derived from
match_summary (not scope-filtered per run). The scope toggles in the UI
map to the pre-computed per-scope summaries for cube-based metrics
(occurrences, cells, time) but not for taxonomic coverage ratios — the
latter only change when establishmentMeans is the scope. This matches
the pre-refactor behavior and is acceptable because taxonomic coverage
ratios are defined relative to the full backbone.

---

## 2026-04-15: Sensitive Species, Dual-Scope Summaries, New Cubes

### New Features
- **Sensitive species integration** — SLU Artdatabanken restricted access list (203 species) downloaded, ingested, and propagated as `is_sensitive` flag + `sensitivity_category` (5km/25km/50km) through the full pipeline (scripts 01 → 03 → 06a → 06b → 09a → 11 → app)
- **Dual-scope summaries** — script 06a now produces both full-GBIF and Dyntaxa-scoped variants of all core summaries (`cell_summary_10km.csv` + `cell_summary_dyntaxa_10km.csv`, etc.) via `write_dual_scope()`. Taxonomy lookup joined at cube level.
- **Primates exclusion** — `parameters.taxonomic.exclude_orders: ["Primates"]` in config. Applied in 06a/06b taxa lookup and in the app at load time.
- **Filter breadcrumb** — Taxonomic tab shows active filter path ("Animalia → Chordata → Aves") with a clear button.
- **Taxonomy flags on species summaries** — `in_dyntaxa`, `is_invasive`, `is_sensitive`, `establishmentMeans` carried through all species-level CSVs from 06b.

### Fixes
- **year_published MJD conversion** — GBIF SQL API returns Modified Julian Date, not integer year. Script 04 now converts via `as.Date(x, origin = "1858-11-17")`, controlled by `parameters.taxonomic.year_published_format: "mjd"` in config.
- **New GBIF cubes** downloaded with corrected `year_published` field (10km: `0020270-260409193756587`, 50km: `0020272-260409193756587`).

### Files Modified
- `R/globals.R` — added `raw_sensitive_dir`
- `scripts/00_setup.R` — added sensitive + invasives to path display
- `scripts/01_download_raw_data.R` — section 3c for sensitive species download
- `scripts/03_ingest_taxonomy.R` — section 3.3d for sensitive species ingestion; `taxonRemarks` fallback for sensitivity category
- `scripts/04_convert_cubes_parquet.R` — MJD→year conversion for `year_published`/`month_published`
- `scripts/06a_make_core_summaries.R` — taxonomy lookup loading, `tag_cube_taxonomy()`, `write_dual_scope()`, dual-scope output for all phases
- `scripts/06b_make_species_summaries.R` — taxonomy lookup, flag columns in `group_base`
- `scripts/09a_reconcile_taxonomy.R` — `is_sensitive` in `taxa_hier_cols` + safety net
- `scripts/11_prepare_gap_app_data.R` — `is_sensitive` + `establishmentMeans` in `species_scope_lookup`
- `shiny_app/gap_app/app.R` — Primates exclusion at load, filter breadcrumb UI + server
- `configs/config_SE.yml` — sensitive species config, cube DOIs, `exclude_orders`, `year_published_format`
- `configs/config_NO.yml` — sensitive species placeholder, `year_published_format`
- `configs/config_template.yml` — sensitive species template section
- `_targets.R`, `run.R`, `README.md`, `ROADMAP.Rmd`, `CHANGELOG.md`, `data_sources.Rmd`, `data_sources_template.Rmd`

### Pipeline Re-run Order
Full re-run from script 01 (or from 03 if only taxonomy changed).

---

# gbifgaps — Taxonomy Restructuring & Feature Updates
## Changelog (Previous)

### Overview

This update implements four architectural changes and numerous feature additions:
1. **Invasive species integration** — Swedish Invasive Species Registry joined to the taxonomy backbone
2. **Dyntaxa/All GBIF toggle** — Global scope switch with `in_dyntaxa` flag propagated through all data
3. **Spatial tab taxonomic filtering** — Kingdom-level recency data for filtering out dominant groups (e.g., Aves)
4. **UI improvements** — About descriptions, heatmap scale toggle, chart fixes

---

### Files Modified

#### `R/globals.R`
- Added `raw_invasives_dir` path variable (defaults to `data/{CC}/raw/invasives`)
- Added `raw_invasives_dir` to `ensure_dirs()` function

#### `scripts/01_download_raw_data.R`
- **New section 3b**: Downloads the Swedish Invasive Species Registry DwC-A
  - Config keys: `invasives.enabled`, `invasives.dataset_key`, `invasives.export_url`, `invasives.doi`
  - Uses same `download_gbif_dataset()` helper as taxonomy and red list
- Updated summary metadata and validation checks to include invasives

#### `scripts/03_ingest_taxonomy.R`
- **New section 3.3b** (after red list enrichment): Reads invasive species checklist
  - Creates `is_invasive` boolean lookup by `scientificName`
  - Left-joins to `taxa_reference`, defaulting to `FALSE` when no match
- **New section 3.3c**: Adds `in_dyntaxa = TRUE` to all taxa in the backbone reference
  - All taxa in `taxa_reference_current.rds` get `in_dyntaxa = TRUE`
  - GBIF species not matched in script 09a get `in_dyntaxa = FALSE`
- Updated `core_columns` to include `is_invasive` and `in_dyntaxa`
- Both columns are preserved in final `taxa_reference_clean` output

#### `scripts/09a_reconcile_taxonomy.R`
- Updated `taxa_hier_cols` to include `is_invasive` and `in_dyntaxa` from accepted taxa
- After enrichment merge:
  - Sets `in_dyntaxa = FALSE` for unmatched species (`match_tier == "unmatched"`)
  - Sets `is_invasive = FALSE` for species without the flag
- Reports diagnostic counts for both flags at end of enrichment

#### `scripts/11_prepare_gap_app_data.R`
- **New**: Invasive species coverage summary (`shiny_data$tax_by_invasive`)
- **New**: Dyntaxa scope summary (`shiny_data$dyntaxa_scope`)
- **New section 5b**: Loads reconciliation table, creates `shiny_data$species_scope_lookup`
  - Lightweight lookup: specieskey → in_dyntaxa, is_invasive, kingdom, match_tier
  - Used by app's global Dyntaxa/All GBIF toggle
- **New section 5c**: Computes `shiny_data$kingdom_cell_recency` from parquet cube
  - Per-kingdom, per-cell: total_occ, max_year, max_yearmonth, staleness_months
  - Enables the spatial tab's "filter by kingdom" on the recency map
- **Fixed**: Published time summary data (`published_time_summary`)
  - Casts `year_published` / `month_published` to integer
  - Filters out NA, out-of-range (year < 1900 or > current+1, month < 1 or > 12)
  - Reports count of dropped rows
- Updated metadata flags: `has_invasive`, `has_dyntaxa_scope`, `has_kingdom_cell_recency`

#### `shiny_app/gap_app/app.R`

**Data extraction (top of file):**
- Added loading of `species_scope_lookup`, `tax_by_invasive`, `kingdom_cell_recency`
- Added derived variables: `has_dyntaxa_scope`, `has_kingdom_recency`, `spatial_kingdom_choices`

**Header UI:**
- Added global **Dyntaxa/All GBIF scope toggle** (`selectInput("dyntaxa_scope")`)
  - "Dyntaxa species (gap analysis)" — default, enables all gap metrics
  - "All GBIF Sweden (overview)" — shows all occurrences, disables gap metrics

**Server — new reactives:**
- `dyntaxa_mode()` — reactive boolean, TRUE when Dyntaxa scope is active
- `output$scope_info_banner` — warning banner shown on Taxonomic tab when in All GBIF mode

**Spatial tab:**
- Added expandable **About description** explaining coverage map, kingdom filter, recency interpretation
- Added **kingdom filter** (`selectInput("spatial_kingdom_filter")`) in display panel
- Updated `observe()` for map rendering:
  - When kingdom filter is active and `map_var == "stale"`, uses `kingdom_cell_recency` filtered to selected kingdom
  - Legend title shows selected kingdom name
  - Falls back to full `cell_recency` when no kingdom filter

**Temporal tab:**
- Added expandable **About description** explaining trend, heatmap, seasonal pattern
- Added **heatmap scale toggle** (`radioButtons("heatmap_scale")`) with three modes:
  - "Log scale" — `log10(occ + 1)`, default (existing behaviour)
  - "Linear" — raw occurrence counts
  - "Count labels" — linear scale with numeric annotations on each cell

**Basis of Record tab:**
- Added expandable **About description** explaining record types and their significance

**Taxonomic tab:**
- Added expandable **About description** explaining Dyntaxa reference, Troudet bias, scope filter
- Added `scope_info_banner` display for All GBIF mode

**Threatened tab:**
- Added expandable **About description** explaining Red List categories and missing species

**Publisher tab:**
- Added expandable **About description** explaining publisher dependency and mobilisation timeline
- **Fixed Published Over Time chart**:
  - Explicit `as.integer(year_published)` cast
  - Filters year range to `[2000, current_year+1]`
  - Handles empty data with placeholder message
  - Added `dtick = 1` for clean x-axis labels

**Priorities tab:**
- Added expandable **About description** explaining priority types and targeting

---

### Configuration Required

Add to `configs/config_SE.yml` (see `config_SE_additions.yml`):
```yaml
invasives:
  enabled: true
  dataset_key: "c1a61294-6195-4055-9b22-a2e48e5e16d7"
  export_url: "https://api.gbif.org/v1/dataset/c1a61294-6195-4055-9b22-a2e48e5e16d7/export"
  doi: "https://doi.org/10.15468/yxfse8"
  name: "Swedish Invasive Species Registry"
```

### Pipeline Re-run Order

After applying these changes, re-run scripts in this order:
1. `01_download_raw_data.R` — downloads invasive species DwC-A
2. `03_ingest_taxonomy.R` — rebuilds taxa_reference with is_invasive + in_dyntaxa
3. `09a_reconcile_taxonomy.R` — propagates flags to reconciliation table
4. `09b_taxonomic_gaps.R` — no changes needed, uses updated reconciliation
5. `11_prepare_gap_app_data.R` — builds shiny_data bundle with new datasets
6. Restart Shiny app

Scripts 02, 04–08, 10 do not need re-running unless underlying data has changed.

---

### Known Remaining Items (not implemented in this batch)

1. **Dyntaxa filter bug (Primates)**: Needs investigation of `establishmentMeans` / `occurrenceStatus` values for Primates in the Dyntaxa distribution table. May require an explicit exclusion list.

2. **Restricted species obfuscation**: Requires a data source for ArtDatabanken sensitivity classifications. Once available, add coordinate stripping/coarsening in the pipeline.

3. **Climate correlation (SMHI data)**: Deferred to v2. Requires new pipeline script to download and spatially join SMHI gridded temperature/precipitation data.

4. **Publisher type classification**: The GBIF Organization API returns institution metadata. A classification step (museums, universities, citizen science platforms, agencies) could be added to `06a` when resolving publisher names.

5. **BasisOfRecord "last 12 months" stats**: The data infrastructure exists (`overview_last_year`), but per-basis breakdowns need computation in script 11.

6. **Default kingdom view on Taxonomic tab**: The initial view should match GBIF's country page kingdom breakdown. Currently requires the user to select a kingdom manually.
