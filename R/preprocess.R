library(dplyr)
library(raster)
library(sf)

#' Calculates the relative frequency of land cover classes for catchments
#'
#' @param feature Catchments as SpatialPolygons or Features from package sf or sp
#' @param raster A Raster or Stars object with land cover class cells
#' @param join Indicates whether to join the frequency statistic to the
#' catchment polygons or to return the frequency statistics only as data frame
#'
#' @return Either the catchment poylgons including the frequency statistics or
#' a data frame that holds the frequency statistics
calculate_land_cover_frequency <- function(features, raster, join) {
    res <- NULL
    
    if (is(raster, "RasterLayer")) {
      for (i in 1:nrow(features)) {
        cells <- raster::extract(raster, features[i, ], df = FALSE)
        table <- as_tibble_col(cells[[1]], column_name = "class") %>%
          drop_na() %>%
          count(class, name = "count") %>%
          mutate(freq = count / sum(count)) %>%
          mutate(catchment_id = features[i, ]$id)
        
        res <- bind_rows(res, table)
      }
    }
    else if (is(raster, "stars")) {
      for (i in 1:nrow(features)) {
        table <- raster[features[i, ]] %>%
          setNames(c("class")) %>%
          as_tibble() %>%
          drop_na() %>%
          count(class, name = "count") %>%
          mutate(freq = count / sum(count)) %>%
          mutate(catchment_id = features[i, ]$id)
        
        res <- bind_rows(res, table)
      }
    }
    else{
      stop(sprintf("Raster object is none of the supported type: %s, %s", "RasterLayer", "stars"))
    }
    
    if (join) {
      left_join(features, res, by = c("id" = "catchment_id"))
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