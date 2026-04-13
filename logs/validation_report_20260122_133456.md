
# Phase 1 Validation Report


Run time: 2026-01-22 13:34:56


Project root: `/Users/lenathol2503/Desktop/gbif_sweden_data_gaps`


Processed data folder: `/Users/lenathol2503/Desktop/gbif_sweden_data_gaps/data_proc`


Config file: `/Users/lenathol2503/Desktop/gbif_sweden_data_gaps/config.yml`


## 1) Grid layers (10km & 50km)

- ✅ 10km processed grid found: `/Users/lenathol2503/Desktop/gbif_sweden_data_gaps/data_proc/grids_10km.gpkg`
- ✅ 50km processed grid found: `/Users/lenathol2503/Desktop/gbif_sweden_data_gaps/data_proc/grids_50km.gpkg`

- 10km CRS EPSG: `3006`

- 50km CRS EPSG: `3006`

- ✅ 10km CRS is SWEREF99 TM (EPSG:3006)
- ✅ 50km CRS is SWEREF99 TM (EPSG:3006)

- 10km geometry types: `POLYGON`

- 50km geometry types: `POLYGON`


- 10km invalid geometries: `0`

- 50km invalid geometries: `0`

- ✅ 10km geometries valid
- ✅ 50km geometries valid

- 10km cells (rows): `6988`

- 50km cells (rows): `23048`


- 10km likely cell code field: `CELLCODE`

- 50km likely cell code field: `cellcode`

- ✅ 10km cell code field appears unique
- ✅ 50km cell code field appears unique

---


## 2) Occurrence Cube outputs

- ✅ Cube manifest found: `/Users/lenathol2503/Desktop/gbif_sweden_data_gaps/data_proc/cube_manifest.csv`
- ✅ Cube totals found: `/Users/lenathol2503/Desktop/gbif_sweden_data_gaps/data_proc/cube_totals_by_basisOfRecord.csv`

- Manifest rows: `18`

- Full ingests: `18`

- Skipped full ingests: `0`

- ✅ All processed cube files exist for full_ingest==TRUE

### 2.1 Column checks (sample of processed cubes)


- Sample: `cube_grid10km_occurrence.fst` (grid=grid10km, basisOfRecord=occurrence)

- ✅ Required cube columns present: specieskey, eeacellcode, yearmonth, occurrences
- ✅ occurrences column parses as numeric
- ✅ occurrences min >= 0

- Sample: `cube_grid10km_observation.fst` (grid=grid10km, basisOfRecord=observation)

- ✅ Required cube columns present: specieskey, eeacellcode, yearmonth, occurrences
- ✅ occurrences column parses as numeric
- ✅ occurrences min >= 0

- Sample: `cube_grid10km_humanObservation.fst` (grid=grid10km, basisOfRecord=humanObservation)

- ✅ Required cube columns present: specieskey, eeacellcode, yearmonth, occurrences
- ✅ occurrences column parses as numeric
- ✅ occurrences min >= 0

### 2.2 Totals quick view


- Totals rows: `18`


Top 5 (grid, basisOfRecord, total_occurrences):


```text

# A tibble: 5 × 3
  grid     basisOfRecord     total_occurrences
  <chr>    <chr>                         <dbl>
1 grid10km humanObservation          127827874
2 grid50km humanObservation          127827874
3 grid10km preservedSpecimen           3784850
4 grid50km preservedSpecimen           3784850
5 grid10km materialSample              2543153

```


---


## 3) Taxa reference (Red List / taxonomy)

- ✅ Taxa reference found: `/Users/lenathol2503/Desktop/gbif_sweden_data_gaps/data_proc/taxa_reference_current.rds`
- ✅ Distribution RDS present
- ✅ Taxon RDS present

- Taxa reference rows: `11240`

- Taxa reference cols: `19`

- ✅ taxonID present
- ✅ scientificName present
- ✅ taxonRank present
- ✅ acceptedNameUsageID present
- ✅ threatStatus present

- Duplicate count for key `taxonID`: `0`

- ✅ No duplicates in key: taxonID

- threatStatus NA rate: `0.495`

- ✅ threatStatus attached for a meaningful share of rows
