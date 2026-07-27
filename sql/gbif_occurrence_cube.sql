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
--   mintemporaluncertainty           = MIN(GBIF_TemporalUncertainty(eventDate))  -- seconds
--   distinctobservers                = COUNT(DISTINCT recordedBy)
-- The eeaCellCode randomisation radius stays 0, so cell assignment is unchanged
-- from the pre-b3verse cube; uncertainty is reported as a measure, not used to
-- perturb the grid.
SELECT
  specieskey, species, kingdom, phylum, class, "order", family,
  basisofrecord, publishingorgkey, datasetkey,
  GBIF_EEARGCode(${RESOLUTION}, decimallatitude, decimallongitude, 0) AS eeacellcode,
  "year", "month",
  COUNT(*) AS occurrences,
  MIN(COALESCE(coordinateuncertaintyinmeters, 1000)) AS mincoordinateuncertaintyinmeters,
  MIN(GBIF_TemporalUncertainty(eventdate)) AS mintemporaluncertainty,
  COUNT(DISTINCT recordedby) AS distinctobservers
FROM occurrence
WHERE countrycode = '${COUNTRY_CODE}'
  AND hascoordinate = TRUE
  AND hasgeospatialissues = FALSE
  AND occurrencestatus = 'PRESENT'
  AND specieskey IS NOT NULL
GROUP BY
  specieskey, species, kingdom, phylum, class, "order", family,
  basisofrecord, publishingorgkey, datasetkey,
  GBIF_EEARGCode(${RESOLUTION}, decimallatitude, decimallongitude, 0),
  "year", "month"
