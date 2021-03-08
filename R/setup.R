# Install required packages
if(!require(gridExtra)) install.packages('gridExtra')
if(!require(raster)) install.packages('raster')
if(!require(sf)) install.packages('sf')
if(!require(stars)) install.packages('stars')
if(!require(tidyverse)) install.packages('tidyverse')
if(!require(tools)) install.packages('tools')

# Load user scripts
source("./R/data_io.R")
source("./R/plot.R")
source("./R/preprocess.R")