# This script provides batch preprocessing of daily DWD REGNIE datasets for
# multiple years. Adjust the parameters for your needs and run the remaining
# code lines. 
# Note, finetuning the performance of the script is still pending. Thus, you may
# run into memory issues if processing is done for too many years at once. It may
# help to split up the processing into smaller chunks by reducing the time frame.

source("./R/setup.R")

##### Path parameters #####
output_path <- "./output/" # output path to save results
subbasin_file <- "./data/wv_subbasins.geojson" 
base_path <- "/path/to/regnie/" # must be absolute

##### Execution parameters #####
start_year <- 1990
end_year <- 2020

subbasins <- load_catchments_as_sf(subbasin_file) %>% st_transform(st_crs(4326))
paths <- paste0(base_path, "ra", start_year:end_year, "m")

for (path in paths) {
  cat(sprintf("/nRead files from folder %s/n", path))
  files <- create_regnie_file_list(path)
  res <- calculate_precipitation_means_chunkwise(files, subbasins, 100)
  res <- res %>% rename(precipitation = mean)
  
  # Note: precipitation has unit 1/10 mm
  write.csv(res, file = paste0(output_path,"precipitation_", basename(path), ".csv"), row.names = FALSE)
  cat(sprintf("/nReading files from folder %s done!/n", path))
}

# After saving the precipitation means for each single year, we have to merge the
# single files all together and save as a single file
paths <- paste0(output_path, "precipitation_ra", start_year:end_year, "m.csv")
res <- data.frame()
for(path in paths) {
  data <- read_csv(path, col_types = cols(catchment_id = col_character()))
  res <- res %>%  bind_rows(data)
}

# Since precipitation values have unit 1/10 mm we convert to mm before storing and
# round to two decimal places. In addition, we rearrange the columns to have a
# separate column for each basin in order to have a more compact storage format
res <- res %>%
  mutate(precipitation = precipitation / 10) %>%
  mutate(precipitation = round(precipitation, 2)) %>% 
  pivot_wider(names_from = catchment_id, values_from = precipitation)

write_csv(res, paste0(output_path, "precipitation_timeseries.csv"))