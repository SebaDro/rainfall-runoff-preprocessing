source("./R/setup.R")

subbasin_file <- "./data/wv_subbasins.geojson"
subbasins <- load_catchments_as_sf(subbasin_file) %>% st_transform(st_crs(4326))

base_path <- "/path/to/regnie/" #Must be absolute
paths <- paste0(base_path, "ra", 2010:2020, "m")

for (path in paths) {
  cat(sprintf("\nRead files from folder %s\n", path))
  files <- create_regnie_file_list(path)
  res <- calculate_precipitation_means_chunkwise(files, subbasins, 100)
  res <- res %>% rename(precipitation = mean)
  
  # Note: precipitation has unit 1/10 mm
  write.csv(res, file = paste0("./output/precipitation", basename(path), ".csv"), row.names = FALSE)
  cat(sprintf("\nReading files from folder %s done!\n", path))
}