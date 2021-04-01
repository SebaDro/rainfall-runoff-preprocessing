# Install required packages
if(!require(gridExtra)) install.packages('gridExtra')
if(!require(raster)) install.packages('raster')
if(!require(sf)) install.packages('sf')
if(!require(stars)) install.packages('stars')
if(!require(tidyverse)) install.packages('tidyverse')
if(!require(tools)) install.packages('tools')
if(!require(ncdf4)) install.packages('ncdf4')
if(!require(ncdf4)) install.packages('units')

# Load user scripts
source("./R/data_io.R")
source("./R/plot.R")
source("./R/preprocess.R")