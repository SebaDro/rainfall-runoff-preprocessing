source("./R/setup.R")

##### Path parameters #####
output_path <- "./output/" # output path to save results
subbasin_file <- "./data/wv_subbasins.geojson"
class_config_file <- "./config/clc-codelist.csv"
# TODO Download CORINE Land Cover from https://land.copernicus.eu/pan-european/corine-land-cover
# and copy the annual GeoTIFF files to the './data' folder
raster_paths <- c("./data/clc1990.tif",
                  "./data/clc2000.tif",
                  "./data/clc2006.tif",
                  "./data/clc2012.tif",
                  "./data/clc2018.tif")

##### Execution parameters #####
# Suffix for resulting files
res_file_suff <- "wv"
# Name of the ID column
id_col <- "id"
# Should land cover classes that are not present be kept? If so, missing classes
# will be filled with zero percentage
fill_missing_classes <- FALSE

# Load codelist for CORINE Land Cover classes
codelist <-
  read_csv(class_config_file, col_types = cols(code = col_character()))
codes <- codelist %>% pull(code)

subbasins <- load_catchments_as_sf(subbasin_file)

for (path in raster_paths) {
  cat(sprintf("\nStart processing raster file %s \n", path))
  land_cover <- read_stars(path)
  
  # Check if both, the raster data and the features/polygons have the same CRS.
  # If not, transform features/polygons to raster CRS
  if(st_crs(subbasins) != st_crs(land_cover)) {
    subbasins <- st_transform(subbasins, st_crs(land_cover))    
  }
  
  res_tmp <-
    calculate_land_cover_frequency(subbasins, land_cover, id_col, FALSE)
  
  res <- res_tmp %>% pivot_wider(
    id_cols = catchment_id,
    names_from = class,
    values_from = freq,
    values_fill = 0
  )
  
  if (fill_missing_classes) {
    # Fill up entries with missing classes and set frequency of zero
    res[setdiff(codes, names(res))] <- 0
  }
  
  # Order columns ascending and round class freqeuncies
  res <- res %>%
    relocate(catchment_id, intersect(codes, names(res))) %>%
    mutate(across(1:ncol(res), round, 4))
  
  out_file <- paste0(output_path, file_path_sans_ext(basename(path)), "_", res_file_suff, ".csv")
  write_csv(res, out_file)
  cat(sprintf("/nSuccesfully wrote result to %s\n", out_file))
}