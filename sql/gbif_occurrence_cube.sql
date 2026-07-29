-- sql/gbif_occurrence_cube.sql
-- ===========================================================================
-- Canonical GBIF SQL Occurrence Cube specification for the Gap Finder pipeline.
-- ===========================================================================
-- The query IS the cube definition: this file is the single source of truth for
-- what a cube contains, so a cube is fully reproducible from its SQL. It is
-- rendered by render_cube_sql() (R/globals.R) and submitted by 01a via
-- rgbif::occ_download_sql(); the same text is printed as the manual-download
-- fallback. Keep it valid GBIF SQL (see techdocs.gbif.org "API SQL Downloads").
--
-- Placeholders (substituted by render_cube_sql):
--   COUNTRY_CODE  = ISO-3166-1 alpha-2 occurrence country, e.g. SE (the WHERE filter)
--   RESOLUTION    = EEA reference-grid resolution in metres (10000 or 50000)
--
-- Grain: species x EEA-cell x year x month x basisOfRecord x publisher x
-- dataset. This is a b-cubed-COMPATIBLE SUPERSET: it keeps the extra dimensions
-- (basisofrecord / publishingorgkey / datasetkey / month) the Record Types and
-- Publisher tabs need, on top of the standard species-occurrence-cube grain.
-- Measures are aggregates over each group (never GROUP BY dimensions):
--   occurrences                      = COUNT(*)
--   mincoordinateuncertaintyinmeters = MIN(COALESCE(coordinateUncertaintyInMeters, 1000))
--   mintemporaluncertainty           = MIN(GBIF_TemporalUncertainty(eventDate, NULL))  -- seconds; 2nd arg = time (null when only a date)
--   distinctobservers                = COUNT(DISTINCT recordedBy)
-- The eeaCellCode randomisation radius stays 0, so cell assignment is unchanged
-- from the pre-b3verse cube; uncertainty is reported as a measure, not used to
-- perturb the grid.
--
-- Taxonomic backbone: kingdom / phylum / class / order / family are interpreted
-- by GBIF against its CURRENT DEFAULT backbone, now the Catalogue of Life
-- Extended Release (COL XR) — a permanent 2025 switch. `specieskey` is pinned
-- EXPLICITLY to COL via the classificationdetails map, keyed by the COL checklist
-- ${COL_CHECKLIST_KEY} (parameters.taxonomic.col_checklist_key, substituted by
-- render_cube_sql). This makes the backbone a deliberate, version-controlled
-- choice rather than a dependency on GBIF's mutable default, and returns the COL
-- taxonID directly (alphanumeric, e.g. "6VFN8"; NOTE some valid COL ids are purely
-- numeric, e.g. Anemone nemorosa = 67343 — a numeric key is NOT a legacy nub key).
-- Per GBIF (2026-07) a pre-migration download could mix COL and legacy-Backbone
-- keys; pinning classificationdetails returns all-COL. The same key is resolved
-- for citation/provenance by scripts/01b_resolve_data_sources.R
-- (data_sources_meta$checklists$col_backbone). classificationdetails syntax per
-- GBIF SQL guidance (2026-07) — confirm on the next live download.
SELECT
  occurrence.classificationdetails['${COL_CHECKLIST_KEY}']['specieskey'] AS specieskey,
  species, kingdom, phylum, class, "order", family,
  basisofrecord, publishingorgkey, datasetkey,
  GBIF_EEARGCode(${RESOLUTION}, decimallatitude, decimallongitude, 0) AS eeacellcode,
  "year", "month",
  COUNT(*) AS occurrences,
  MIN(COALESCE(coordinateuncertaintyinmeters, 1000)) AS mincoordinateuncertaintyinmeters,
  MIN(GBIF_TemporalUncertainty(eventdate, NULL)) AS mintemporaluncertainty,
  COUNT(DISTINCT recordedby) AS distinctobservers
FROM occurrence
WHERE countrycode = '${COUNTRY_CODE}'
  AND hascoordinate = TRUE
  AND hasgeospatialissues = FALSE
  AND occurrencestatus = 'PRESENT'
  AND specieskey IS NOT NULL
GROUP BY
  occurrence.classificationdetails['${COL_CHECKLIST_KEY}']['specieskey'],
  species, kingdom, phylum, class, "order", family,
  basisofrecord, publishingorgkey, datasetkey,
  GBIF_EEARGCode(${RESOLUTION}, decimallatitude, decimallongitude, 0),
  "year", "month"
