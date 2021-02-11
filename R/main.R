source("./R/data_io.R")
source("./R/preprocess.R")
source("./R/plot.R")

# Set path to required datasets
basin_file <- "./data/wv_basin.geojson"
subbasin_file <- "./data/wv_subbasins.geojson"
raster_file <- "./data/land_cover.tif"

# Read catchment polygons and land cover raster
basin <- load_catchments_as_sf(basin_file)
subbasins <- load_catchments_as_sf(subbasin_file)
land_cover <- load_land_cover_rasters(raster_file)

# Calculate the land cover class frequencies for each catchment
res <- calculate_land_cover_frequency(subbasins, land_cover, TRUE)

# Check if the relative class frequency for each catchment sums up to 1 should
# be TRUE.
res %>%
  group_by(id) %>%
  summarise(sum = sum(freq)) %>% 
  filter(sum != 1) %>% nrow() == 0

# Select a catchment and plot its landcover class frequencies
subbasin <- subbasins %>% filter(id == c(100060))
landcover_classes <- res %>% filter(id == c(100060))

plot_landcover_frequency(basin, subbasin, landcover_classes, "bar")

