source("./R/data_io.R")
source("./R/preprocess.R")
source("./R/plot.R")

catchment_file <- "C:/Users/Sebastian/Documents/Projekte/Promotion/02_GIS/land_cover_wv/ezgs/pegel_ezgs.gpkg"
raster_file <- "C:/Users/Sebastian/Documents/Projekte/Promotion/02_GIS/land_cover_wv/u2000_clc1990_v2020_20u1_wv.tif"

# Read catchment polgons and land cover raster
catchments <- load_catchments_as_sf(catchment_file)
land_cover <- load_land_cover_rasters(raster_file)

# Calculate the land cover class frequencies for each catchment
res <- calculate_land_cover_frequency(catchments, land_cover, TRUE)

# Check if the relative class frequency for each catchment sums up to 1 should
# be TRUE.
res %>%
  group_by(id) %>%
  summarise(sum = sum(freq)) %>% 
  filter(sum != 1) %>% nrow() == 0