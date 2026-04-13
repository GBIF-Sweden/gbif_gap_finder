# gbifgaps — Taxonomy Restructuring & Feature Updates
## Changelog

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
