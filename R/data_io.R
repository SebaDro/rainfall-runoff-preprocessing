library(raster)
library(sf)
library(tidyverse)
library(stars)

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

#' Loads several DWD REGNIE raster files with daily precipitation data into a 
#' stars object.
#' For more information about DWD REGNIE data see: https://www.dwd.de/DE/leistungen/regnie/regnie.html
#'
#' @param files a data frame with dates in the first column and the paths to
#' the REGNIE raster file in the second column.
#'
#' @return a stars object which holds one precipitation attribute and the
#' dimensions x, y and date
load_regnie_as_stars <- function(files){

  x_delta <- 1 / 60
  y_delta <- 1 / 120
  
  x_offset <- (6 - 10 * x_delta) - x_delta / 2
  y_offset <- (55 + 10 * y_delta) + y_delta / 2
  
  crs <- st_crs(4326)
  
  res <- NULL
  
  for (f in files[,2]) {
    values <- read_fwf(
      f,
      col_positions = fwf_widths(rep(4, 611)),
      na = c("-999"),
      col_types = cols(.default = col_integer()),
      n_max = 971
    )
    
    m <- t(as.matrix(values))
    res <- c(res, m)
  }
  
  a <- array(res, dim = c(x = 611, y = 971, date = length(files)))
  data <- st_as_stars("precipitation" = a)
  
  data %>%
    st_set_dimensions("x",
                      offset = x_offset,
                      delta = x_delta,
                      refsys = crs) %>%
    st_set_dimensions("y",
                      offset = y_offset,
                      delta = -y_delta,
                      refsys = crs) %>% 
    st_set_dimensions("date",
                      values = files[,1],
                      names = "date")
}