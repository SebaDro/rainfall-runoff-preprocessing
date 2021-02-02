library(dplyr)
library(raster)
library(sf)

#' Calculates the relative frequency of land cover classes for catchments
#'
#' @param polygons Catchments as SpatialPolygons from package sf or sp
#' @param raster A Raster object with land cover class cells
#' @param join Indicates whether to join the frequency statistic to the 
#' catchment polygons or to return the frequency statistics only as data frame
#'
#' @return Either the catchment poylgons including the frequency statistics or
#' a data frame that holds the frequency statistics
#' @export
#'
#' @examples
calculate_land_cover_frequency <- function(polygons, raster, join) {
  table = data.frame()
  for (i in 1:nrow(polygons)) {
    # Extract cells from the raster that fall within a catchment polygon
    cells <- raster::extract(raster, polygons[i, ], df = FALSE)
    # Calculate the relative frequency of each land cover class for a 
    # catchment and create a dataframe that contains the calculated statistics
    # as well as the catchment ID
    clc_freq <- as.data.frame(table(class = cells[[1]]), responseName = "count") %>%
      mutate(freq = count / length(cells[[1]]), catchment_id = pull(polygons[i, ], id))
    # Add to result table
    table <- rbind(table, clc_freq)
  }
  if (join) {
    left_join(catchments, table, by = c("id" = "catchment_id"))  
  } else{
    table  
  }
}