source("run.R")
launch_app()
tar_read(pipeline_summary)
tar_visnetwork()

# ----------------

# SETUP
source("scripts/00_setup.R")

# DOWNLOAD IF NEEDED - CAN BE DONE MANUALLY
#source("scripts/01_download_raw_data.R")

# INGESTION
source("scripts/02_ingest_grids.R")
source("scripts/03_ingest_taxonomy.R")
source("scripts/04_ingest_gbif_cubes.R)


source("scripts/07_spatial_gaps.R")
source("scripts/07_spatial_gaps.R")
source("scripts/07_spatial_gaps.R")
source("scripts/07_spatial_gaps.R")
