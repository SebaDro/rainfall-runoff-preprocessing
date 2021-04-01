library(raster)
library(sf)
library(tidyverse)
library(stars)
library(tools)
library(ncdf4)

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
  read_csv(timeseries_file, comment = "#") %>%
    gather(key = "ID", "discharge", -date) %>% 
    left_join(metadata, by = c("ID"))
}

#' Loads several DWD REGNIE raster files with daily precipitation data into a 
#' stars object.
#' For more information about DWD REGNIE data see: https://www.dwd.de/DE/leistungen/regnie/regnie.html
#'
#' @param files a data frame with dates in the first column and the paths to
#' the REGNIE raster files (either compressed with file ending .gz or uncompressed)
#' in the second column.
#'
#' @return a stars object which holds one precipitation attribute and the
#' dimensions x, y and date
load_regnie_as_stars <- function(files){
  n_files <- nrow(files)
  
  # Set coordinates delta and offset as defined in
  # in the DWD REGNIE format description 
  x_delta <- 1 / 60
  y_delta <- 1 / 120
  x_offset <- (6 - 10 * x_delta) - x_delta / 2
  y_offset <- (55 + 10 * y_delta) + y_delta / 2
  
  crs <- st_crs(4326)
  
  res <- NULL
  dates <- integer(0)
  class(dates) <- "Date"
  
  for (i in 1:nrow(files)) {
    f <- files[i,2]
    tryCatch(
      {
        # Read raster values from ASCII files as fixed width files
        values <- read_fwf(
          f,
          col_positions = fwf_widths(rep(4, 611)),
          na = c("-999"),
          col_types = cols(.default = col_integer()),
          n_max = 971
        )

        # Transpose raster matrix to match stars coordinate axis dimensions
        m <- t(as.matrix(values))
        res <- c(res, m)
        
        dates <- c(dates, files[i,1])
        
        cat(sprintf('\r %s %% completed', paste0(round(i / n_files * 100))))
        
      }, error = function(c) {
        message(sprintf("Could not create star object for file %s.\n%s", f, c))
      }
    )
  }
  
  # Create a stars object with attribute 'precipitation' and
  # dimensions 'x', 'y' and 'date'
  a <- array(res, dim = c(x = 611, y = 971, date = length(dates)))
  data <- st_as_stars("precipitation" = a)
  
  # Set dimensions of stars objects using DWD REGNIE coordinates delta and offset
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
                      values = dates,
                      names = "date")
}

#' Creates a list of absolute paths to DWD REGNIE raster files inside folders 
#' each one containing daily raster files for whole year
#'
#' @param path paths to the folders containing the daily raster files
#'
#' @return data frame which contains the dates of the raster files in the first
#' column and the corresponding file paths in the second column
create_regnie_file_list <- function(paths) {
  table <- data.frame()
  
  for (path in paths) {
    year <- path %>% 
      basename() %>% 
      substr(3, 6)
    
    files <- list.files(path, full.names = TRUE, pattern = "\\.gz$")
    
    dates <- files %>%
      basename() %>% 
      file_path_sans_ext() %>% 
      substr(5,8) %>%
      paste0(year) %>% 
      as.Date("%m%d%Y")
    
    table <- table %>%
      bind_rows(data.frame(dates = dates, files = files))
  }
  table
}

#' Creates multiple NetCDF files, each one containing discharge and precipitation
#' timeseries data for a single catchment
#'
#' @param data tibble or data frame containing the timeseries data for several
#' catchments
#' @param out_path path of directory to write the NetCDF files to
#' @param id_col Name of the catchment ID column. IDs will be used as prefix
#' for NetCDF files
#' @param date_col name of the dates column
#' @param date_dim name of the dates dimension to use
#' @param prec_col name of the precipitation column
#' @param prec_dim name of the precipitation dimension to use
#' @param prec_col name of the discharge column
#' @param prec_dim name of the discharge dimension to use
#'
#' @return void
save_timeseries_as_netcdf <- function(data,
                                      out_path,
                                      id_col,
                                      date_col = "date",
                                      date_dim = "date",
                                      prec_col = "precipitation",
                                      prec_dim = "precipitation",
                                      discharge_col = "discharge",
                                      discharge_dim = "discharge") {
  
  # iterate over single catchments in order to create a separate NetCDF file
  # for each catchment
  for (id in pull(distinct(data[id_col]), id_col)) {
    single_basin_data <- data %>% filter(catchment_id == id)
    
    dates <- single_basin_data %>% pull(date)
    prec_values <- single_basin_data %>% pull(prec_col)
    discharge_values <- single_basin_data %>% pull(discharge_col)
    
    # define date as only dimension
    time_units <- paste0("days since ", dates[1])
    date_values <- as.numeric(dates[] - dates[1])
    timedim <- ncdim_def(date_dim, time_units, date_values)
    
    # variable definitions for precipitation and discharge
    precipitation_def <-
      ncvar_def(
        name = prec_dim,
        units = "mm/d",
        dim = list(timedim),
        longname = "mean precipitation [mm] per day",
        prec = "float"
      )
    discharge_def <-
      ncvar_def(
        name = discharge_dim,
        units = "m^3/d",
        dim = list(timedim),
        longname = "daily discharge [m^3]",
        prec = "float"
      )
    
    # create NetCDF file
    filename <- paste0(out_path, id, ".nc")
    ncdf_file <-
      nc_create(filename, list(precipitation_def, discharge_def), force_v4 = TRUE)
    
    # write out timeseries values and close file
    ncvar_put(ncdf_file, precipitation_def, prec_values)
    ncvar_put(ncdf_file, discharge_def, discharge_values)
    
    nc_close(ncdf_file)
  }
}

#' Creates multiple NetCDF files, each one containing timeseries data variables
#' for a single catchment
#'
#' @param data tibble or data frame containing the timeseries data for several
#' catchments. All observation columns will be used as an variable for the
#' resulting NetCDF file, taking into account those columns with col_type of units.
#' @param out_path path of directory to write the NetCDF files to
#' @param id_col Name of the catchment ID column. IDs will be used as prefix
#' for NetCDF files
#' @param date_col name of the dates column
#' @param date_dim name of the dates dim to use
#'
#' @return void
save_tibble_as_netcdf <- function(data,
                                  out_path,
                                  id_col,
                                  date_col = "date",
                                  date_dim = "date") {
  
  # iterate over single catchments in order to create a separate NetCDF file
  # for each catchment
  for (id in pull(distinct(data[id_col]), id_col)) {
    single_basin_data <- data %>% filter(catchment_id == id)
    
    dates <- single_basin_data %>% pull(date)
    # prec_values <- single_basin_data %>% pull(prec_col)
    # discharge_values <- single_basin_data %>% pull(discharge_col)
    
    # define date as only dimension
    time_units <- paste0("days since ", dates[1])
    date_values <- as.numeric(dates[] - dates[1])
    timedim <- ncdim_def(date_dim, time_units, date_values)
    
    col_types <- data %>% 
      head %>% 
      collect %>% 
      lapply(class)
    
    # filter columns with column type units. Only those columns will
    # be used as NetCDF variables
    col_types <- col_types[col_types == "units"]
    
    var_list <- list()
    
    for(i in 1:length(col_types)) {
      name <- names(col_types)[i]
      unit <- data %>%
        pull(name)%>%
        units() %>%
        as.character()
      
      values <- single_basin_data %>% pull(name)
      var_def <- ncvar_def(
        name = name,
        units = unit,
        dim = list(timedim),
        prec = "float"
      )
      
      var_list[[i]] <- list(values = values, def = var_def)
    }
    
    # create NetCDF file
    filename <- paste0(out_path, id, ".nc")
    var_defs <- lapply(var_list,`[[`, c('def'))
    ncdf_file <-
      nc_create(filename, var_defs, force_v4 = TRUE)
    
    for(i in 1:length(var_list)) {
      ncvar_put(ncdf_file, var_list[i][[1]]$def, var_list[i][[1]]$values)
    }
    
    nc_close(ncdf_file)
  }
}