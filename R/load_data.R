#' Loads catchment datasets from a spatial data source
#' 
#'  @param path Path to the spatial data source.
#'  If data source is a shapefile, give the path to the directory containing the shapefile.
#'  If data source is a geopackage, give the path to the *.gpkg file.
#'  
#'  @param layer Name of the layer containing the catchment dataset
#'  
#'  @return catchment datasets
load_catchments <- function(path, layer) {
  rgdal::readOGR(dsn = path, layer = layer)
}