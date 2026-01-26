
# Phase 1 Validation Report


**Run time:** ##------ Mon Jan 26 15:02:00 2026 ------##

**Project root:** `/Users/lenathol2503/Desktop/gbif_sweden_data_gaps`

**Processed data:** `/Users/lenathol2503/Desktop/gbif_sweden_data_gaps/data_proc`

**Config file:** `/Users/lenathol2503/Desktop/gbif_sweden_data_gaps/config.yml`


---


## 1) Grid Layers (10km & 50km)

- ✅ 10km grid file found

- ✅ 50km grid file found


#### 10km grid details

- EPSG: `3006`

- ✅ CRS is SWEREF99 TM (EPSG:3006)

- Geometry types: `MULTIPOLYGON`

- Invalid geometries: `0`

- ✅ All geometries valid

- Total cells: `6,988`

- Cell code field: `CELLCODE`

- ✅ Cell codes are unique


#### 50km grid details

- EPSG: `3006`

- ✅ CRS is SWEREF99 TM (EPSG:3006)

- Geometry types: `MULTIPOLYGON`

- Invalid geometries: `0`

- ✅ All geometries valid

- Total cells: `23,048`

- Cell code field: `cellcode`

- ✅ Cell codes are unique


---


## 2) GBIF Occurrence Cube Outputs

- ✅ Cube manifest found

- Manifest entries: `18`

- Full ingests: `18`

- Skipped ingests: `0`

- ✅ All expected cube files exist


### 2.1 Column Checks (Sample Cubes)


**Sample:** `cube_grid10km_occurrence.fst`

- Grid: grid10km

- Basis: occurrence

- ✅ All required columns present

- ✅ Occurrences column is numeric

- ✅ Occurrences ≥ 0


**Sample:** `cube_grid10km_observation.fst`

- Grid: grid10km

- Basis: observation

- ✅ All required columns present

- ✅ Occurrences column is numeric

- ✅ Occurrences ≥ 0


**Sample:** `cube_grid10km_humanObservation.fst`

- Grid: grid10km

- Basis: humanObservation

- ✅ All required columns present

- ✅ Occurrences column is numeric

- ✅ Occurrences ≥ 0


### 2.2 Totals Summary

- ✅ Cube totals file found

- Total entries: `18`


**Top 5 by occurrence count:**


```text
# A tibble: 5 × 3
```

```text
  grid     basisOfRecord     total_occurrences
```

```text
  <chr>    <chr>                         <dbl>
```

```text
1 grid10km humanObservation          127827874
```

```text
2 grid50km humanObservation          127827874
```

```text
3 grid10km preservedSpecimen           3784850
```

```text
4 grid50km preservedSpecimen           3784850
```

```text
5 grid10km materialSample              2543153
```


---


## 3) Taxa Reference (Red List / Taxonomy)

- ✅ Taxa reference found

- Rows: `11,240`

- Columns: `19`


#### Column Checks

- ✅ taxonID present

- ✅ scientificName present

- ✅ taxonRank present

- ✅ acceptedNameUsageID present

- ✅ threatStatus present


- Key field: `taxonID`

- Duplicates: `0`

- ✅ No duplicate keys

- threatStatus NA rate: `0.495`

- ✅ threatStatus has meaningful coverage

