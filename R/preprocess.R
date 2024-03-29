library(dplyr)
library(raster)
library(sf)

#' Calculates the relative frequency of land cover classes for spatial features
#'
#' @param features Features as SpatialPolygons or Features from package sf or sp
#' @param raster A Raster or Stars object with land cover class cells
#' @param id_col Name of the features column that contains the identifiers.
#' Default 'id'.
#' @param join Indicates whether to join the frequency statistic to the
#' catchment polygons or to return the frequency statistics only as data frame
#'Default TRUE
#'
#' @return Either the catchment poylgons including the frequency statistics or
#' a data frame that holds the frequency statistics
calculate_land_cover_frequency <- function(features, raster, id_col="id", join=FALSE) {
    res <- NULL
    n_features <- nrow(features)
    
    if (is(raster, "RasterLayer")) {
      for (i in 1:nrow(features)) {
        cat(sprintf("\r Calculate land cover classes for subbasin %s. %s%% completed", pull(features[i,], id_col), paste0(round(i / n_features * 100))))
        cells <- raster::extract(raster, features[i, ], df = FALSE)
        table <- as_tibble_col(cells[[1]], column_name = "class") %>%
          drop_na() %>%
          count(class, name = "count") %>%
          mutate(freq = count / sum(count)) %>%
          mutate(catchment_id = pull(features[i,], id_col))
        
        res <- bind_rows(res, table)
      }
    }
    else if (is(raster, "stars")) {
      for (i in 1:nrow(features)) {
        ## st_as_stars needs to be called, if raster ist a stars proxy object
        cat(sprintf("\r Calculate land cover classes for subbasin %s. %s%% completed", pull(features[i,], id_col), paste0(round(i / n_features * 100))))
        table <- st_as_stars(raster[features[i, ]]) %>%
          setNames(c("class")) %>%
          as_tibble() %>%
          drop_na() %>%
          count(class, name = "count") %>%
          mutate(freq = count / sum(count)) %>%
          mutate(catchment_id = pull(features[i,], id_col))
          # mutate(catchment_id = features[i, ]$id)
        
        res <- bind_rows(res, table)
      }
    }
    else{
      stop(sprintf("Raster object is none of the supported type: %s, %s", "RasterLayer", "stars"))
    }
    
    if (join) {
      left_join(features, res, setNames("catchment_id", id_col))
    } else {
      res
    }
}

#' Prepares a vendor specific discharge timeseries dataset to be analyze ready
#'
#' @param data dataset that contains discharge timeseries data and gauge metadata
#' in a weird format
#'
#' @return list which contains a gauge metadata data frame and discharge timeseries
#' data frame
prepare_wv_discharge_data <- function(data) {
  # First split datatset into gauge metadata and timeseries data
  timeseries <- data[10:nrow(data), ]
  meta <- t(data[1:9, ])
  
  # Prepare metadata column names
  colnames(meta) <- meta[1,]
  rownames(meta) <- NULL
  meta <- meta[-1,]
  
  # Prepare timeseries column names
  colnames(timeseries)[2:ncol(timeseries)] <- meta[,"Name"]
  colnames(timeseries)[1] <- "date"
  
  # Convert date strings to POSIXct
  timeseries[,"date"] <- as.POSIXct(timeseries[,"date"], format = "%d.%m.%Y %H:%M:%OS")
  
  # Convert string numbers to decimals
  timeseries[, 2:ncol(timeseries)] <- apply(timeseries[, 2:ncol(timeseries)], 2,
                                            function(x)
                                              as.numeric(gsub(",", ".",
                                                              gsub(" ", "", x, fixed = TRUE))))
  
  list(meta, timeseries)
}

#' Wrapper function to calculate precipitation means chunkwise for a large list
#' of REGNIE raster files.
#' 
#' @param files List of REGNIE raster files
#' @param features Features to calculate precipitation means for
#' @param chunks List of raster files will be split into chunks of this size
#'
#' @return DataFrame that contains the time indexed precipitation means for all REGNIE
#' raster files and each feature
calculate_precipitation_means_chunkwise <- function(files, features, chunks){
  n <- nrow(files)
  
  sequences <- seq(0, n, chunks)
  if(!(n %in% sequences)) {
    sequences <- c(sequences, n) 
  }
  
  table = data.frame()
  
  for(i in 1:(length(sequences) - 1)) {
    i_start <-  sequences[i] + 1
    i_end <- sequences[i + 1]
    
    cat(sprintf("\nStart reading REGNIE raster files %s to %s \n", i_start, i_end))
    stars <- load_regnie_as_stars(files[i_start:i_end,])
    means <- calculate_precipitation_means(stars, subbasins)
    
    table <- table %>% bind_rows(means)
  }
  table
}

#' Calculates the mean precipitation for spatial features from a Stars object that
#' holds a raster dataset of precipitation values.
#' For each feature those raster cells that lie within the features boundary
#' will be selected and the mean over all selected raster cell will be calculated.
#'
#' @param stars Stars object with with three dimensions _x_, _y_ and _date_ and
#' _precipitation_ as parameter.
#' @param features Features as SpatialPolygons or Features from package sf or sp
#'
#' @return DataFrame that contains the time indexed precipitation means for each
#' feature
calculate_precipitation_means <- function(stars, features){
  table = data.frame()
  for (i in 1:nrow(features)) {
    feature <- features[i,]
    means <-
      as.data.frame(st_apply(suppressMessages(stars[feature]), c("date"), mean, na.rm = TRUE)) %>%
      add_column(catchment_id = pull(features[i, ], id))
    table <- table %>% bind_rows(means)
  }
  table %>% arrange(date)
}