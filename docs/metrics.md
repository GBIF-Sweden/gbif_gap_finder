# Gap metrics definition (Phase 3)

This document defines the **gap metrics** used in this project to quantify data coverage and identify gaps in **GBIF-mediated biodiversity occurrence data for Sweden**, based on GBIF Occurrence Cube summaries and reference datasets.

All metrics are designed to be:
- **reproducible** (derived from tracked inputs + scripts)
- **comparable** between 10 km and 50 km grids
- **interpretable** for reporting and decision-making

---

## 1) Data sources used for metrics

### 1.1 GBIF Occurrence Cube (Sweden)
The main input is GBIF Occurrence Cube data, split by `basisOfRecord` and aggregated into EEA grid cells.

Core cube variables used:
- `eeacellcode` (grid cell identifier)
- `yearmonth` (monthly time step, format: `YYYY-MM`)
- `specieskey` and `species` (GBIF backbone key + label)
- `occurrences` (count of occurrence records in that group)

Coverage is analysed separately for two spatial reference grids:
- **10 km grid**
- **50 km grid**

### 1.2 EEA grids (10km / 50km)
EEA grids are used as the spatial reference for mapping and cell-based gap detection.

### 1.3 Swedish Red List taxonomy export
The Swedish reference taxonomy and associated metadata is used as the local reference for taxonomic gap assessments.

Core variables used:
- `taxonID`
- `scientificName`
- `taxonRank`
- `acceptedNameUsageID`
- `threatStatus`

---

## 2) General definitions

### 2.1 Coverage
In this project, “coverage” means **the presence of GBIF-mediated occurrence records** in a spatial/temporal/taxonomic unit.

- Coverage does **not** imply ecological completeness.
- Coverage reflects **data availability and sampling**, and includes biases.

### 2.2 basisOfRecord
Cubes are split into separate files representing different `basisOfRecord` categories (e.g., `humanObservation`, `preservedSpecimen`, etc.).

Analyses are performed:
- for each `basisOfRecord` separately
- and for an `"all"` rollup (sum across basisOfRecord categories)

This allows detection of **sampling bias by record type**.

---

## 3) Spatial gap metrics

Spatial gaps are assessed at the level of EEA grid cells (`eeacellcode`).

All spatial metrics can be computed for:
- 10 km grid
- 50 km grid

And for:
- each `basisOfRecord`
- `"all"` combined

### 3.1 Spatial “zero coverage” gaps (true gaps)
**Definition:**  
A grid cell is a *spatial gap* if:

- `occurrences == 0`

This represents complete absence of GBIF-mediated occurrence records in that cell for the selected data subset.

**Output field example:**
- `gap_zero = TRUE/FALSE`

---

### 3.2 Spatial “low coverage” gaps (relative gaps)
Zero-only gaps can miss areas that are technically covered but **poorly sampled**.  
Therefore, low-coverage gaps are also defined using thresholds.

#### 3.2.1 Quantile-based low-coverage gaps (recommended)
**Definition:**  
Among all grid cells with `occurrences > 0`, define a quantile threshold *q* (e.g., 0.05 or 0.10).  
A cell is classified as low-coverage if:

- `occurrences > 0` AND `occurrences <= quantile(occurrences, q)`

Typical values:
- q = 0.05 (lowest 5%)
- q = 0.10 (lowest 10%)

**Output fields example:**
- `gap_low_q05 = TRUE/FALSE`
- `gap_low_q10 = TRUE/FALSE`

#### 3.2.2 Fixed-threshold low-coverage gaps
A fixed threshold may be used for interoperability, for example:

- `occurrences < 10`
- `occurrences < 100`

This definition is less comparable across datasets and therefore optional.

---

### 3.3 Spatial intensity (non-gap metric)
For mapping and visualization, an intensity layer may also be computed:

- `log_occ = log10(occurrences + 1)`

This is not a gap definition, but a useful complement.

---

## 4) Temporal gap metrics

Temporal gaps are assessed at monthly resolution (`yearmonth`, format `YYYY-MM`).

All temporal metrics can be computed:
- for each grid size (10km / 50km)
- for each `basisOfRecord` and for `"all"`

### 4.1 No-data months
**Definition:**  
A month is a temporal gap if:

- `occurrences == 0`

This can be summarized at national level (all Sweden), or stratified by grid cells and taxa.

---

### 4.2 Recency / staleness
Temporal gaps are often best captured through “how recently” data was recorded.

#### 4.2.1 Last observation month per unit
**Definition:**  
For a given unit (cell, species, etc.) the last observation month is:

- `last_yearmonth = max(yearmonth where occurrences > 0)`

#### 4.2.2 Staleness time window
**Definition:**  
A unit is considered “stale” if the last observation month is older than a threshold window.

Example thresholds:
- no records in the last **12 months**
- no records in the last **5 years**

This produces a highly interpretable map of:
- recently sampled areas
- not-recently sampled areas

---

### 4.3 Seasonal bias (context metric)
Seasonal bias is not necessarily defined as a “gap”, but it is a key interpretive metric.

**Definition:**
- Split `yearmonth` into:
  - `year`
  - `month`
- Compare total occurrences across months (1–12)

Outputs:
- occurrences per month (seasonality profile)
- year × month heatmap (sampling intensity across years and seasons)

---

## 5) Taxonomic gap metrics

Taxonomic gaps compare GBIF-mediated taxonomic coverage to a Swedish reference taxonomy.

### 5.1 Reference taxa
Reference taxa are defined as taxa listed in the Swedish taxonomy export, with associated metadata (including threat status where available).

### 5.2 Taxa detected in GBIF cube
Taxa detected in the cube are defined as:
- unique `specieskey` (GBIF backbone identifier)
- with corresponding label `species` (name string)

### 5.3 Matching strategy (taxon mapping)
The Swedish taxonomy export uses `taxonID` identifiers which are different from GBIF `specieskey`.  
Therefore, taxonomic gap calculations depend on a matching strategy.

#### 5.3.1 Match strategy A: scientific name string matching (initial / fast)
**Definition:**
- Standardize both name strings:
  - lowercase
  - trim whitespace
  - optionally remove authorship text
- Match `scientificName` (reference) to `species` (cube)

This provides an initial estimate of taxonomic gaps.

**Limitations:**
- sensitive to synonyms and spelling differences
- may under/overestimate gaps if names do not align exactly

#### 5.3.2 Match strategy B: GBIF backbone matching (recommended upgrade)
A more robust mapping can be produced by resolving Swedish names to GBIF backbone keys using GBIF name matching.

Potential tools:
- `rgbif::name_backbone()`
- bulk name matching via GBIF API

This improves taxonomic gap results but may introduce ambiguous matches requiring inspection.

---

### 5.4 Taxonomic “missing taxa” (gap definition)
**Definition:**  
A reference taxon is considered missing from GBIF-mediated data if:

- it exists in the Swedish reference taxonomy  
AND
- no match is found in the cube taxa set (using the chosen matching strategy)

Outputs include:
- lists of missing taxa (`taxonID`, `scientificName`, `taxonRank`, `threatStatus`)
- summary counts by taxonomic rank (family/order/class, etc.)

---

### 5.5 Threat-status weighted taxonomic gaps (recommended reporting)
To contextualize missing taxa, taxonomic gaps should be summarized by threat categories.

Example groupings:
- Threatened: `CR`, `EN`, `VU`
- Near threatened: `NT`
- Data deficient: `DD`
- Least concern: `LC`

Primary outputs:
- number and proportion of missing taxa by threat category
- list of missing threatened taxa

---

## 6) Parameters to be set (recommended)

The following parameters should be set centrally (not hardcoded across scripts), so results can be easily reproduced with alternative definitions:

### Spatial low-coverage quantile threshold
- Default: `q = 0.05` (lowest 5% of non-zero cells)
- Optional alternative: `q = 0.10`

### Temporal recency window
- Default: “no records in last 5 years”
- Optional: “no records in last 12 months”

### Taxonomic matching strategy
- Default: scientific name string matching (initial)
- Upgrade: GBIF backbone resolution

---

## 7) Outputs expected from Phase 3 (planned)

Phase 3 will write gap metrics and intermediate results to:

- `data_proc/gaps/`

Planned outputs include:
- `spatial_gaps_10km.csv`
- `spatial_gaps_50km.csv`
- `temporal_gaps_summary.csv`
- `taxonomic_gap_summary.csv`
- `missing_taxa.csv`
- `missing_threatened_taxa.csv`

All outputs will be generated from Phase 2 derived datasets.
