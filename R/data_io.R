library(raster)
library(sf)

#' Loads catchment datasets from a spatial data source
#'
#'  @param path Path to the spatial data source.
#'  If data source is a shapefile, give the path to the directory containing
#'  the shapefile.
#'  If data source is a geopackage, give the path to the *.gpkg file.
#'
#'  @param layer Name of the layer containing the catchment dataset
#'
#'  @return catchment datasets as simple feature object
load_catchments <- function(path, layer) {
  rgdal::readOGR(dsn = path, layer = layer)
}

#' Loads catchment datasets from a spatial data source
#'
#'  @param path Path to the spatial data source.
#'  If data source is a shapefile, give the path to the directory containing
#'  the shapefile.
#'  If data source is a geopackage, give the path to the *.gpkg file.
#'
#'  @param layer Name of the layer containing the catchment dataset
#'
#'  @return catchment datasets as simple feature object
load_catchments_as_sf <- function(path, layer) {
  sf::st_read(dsn = path,
              layer = layer,
              stringsAsFactors = FALSE)
}

#' Loads land cover classification data as a Raster object
#'
#' @param path Path to the Raster file
#'
#' @return Raster objects that represents the land cover classes
load_land_cover_rasters <- function(path) {
  raster::raster(path)
}


#' Loads timeseries data for discharge gauges from two CSV files, containing
#' gauge metadata and the timeseries data. Both datasets will be joined and
#' returned as tibble.
#'
#' @param metadata_path path to the metadata CSV file
#' @param timeseries_path path to the timeseries CSV file 
#'
#' @return a tibble containing bot, gauge metadata and discharge timeseries
load_timeseries_data <- function(metadata_path, timeseries_path) {
  metadata <- read_csv(metadata_file, col_types = cols(ID = col_character()))
  read_csv(timeseries_file) %>%
    gather(key = "ID", "discharge", -date) %>% 
    left_join(metadata, by = c("ID"))
}