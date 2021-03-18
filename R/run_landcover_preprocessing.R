source("./R/setup.R")

# Parameters
output_path <- "./output/" # output path to save results
res_file_suff <- "wv"
subbasin_file <- "./data/wv_subbasins.geojson"

# TODO Download CORINE Land Cover from https://land.copernicus.eu/pan-european/corine-land-cover
# and copy the annual GeoTIFF files to the './data' folder
raster_paths <- c("./data/U2018_CLC2018_V2020_20u1.tif",
                  "./data//U2018_CLC2012_V2020_20u1.tif",
                  "./data//U2012_CLC2006_V2020_20u1.tif",
                  "./data//U2006_CLC2000_V2020_20u1.tif",
                  "./data//U2006_CLC2000_V2020_20u1.tif")
subbasins <- load_catchments_as_sf(subbasin_file, "CAMELS_GB_catchment_boundaries")
id_col <- "ID"

class_config_file <- "./config/clc-codelist.csv"
fill_missing_classes <- TRUE


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
    # Load codelist for CORINE Land Cover classes and fill up result table with
    # missing and frequency of zero
    codelist <-
      read_csv(class_config_file, col_types = cols(code = col_character()))
    codes <- codelist %>% pull(code)
    
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
