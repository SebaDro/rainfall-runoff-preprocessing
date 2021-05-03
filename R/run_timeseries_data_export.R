# For convenience, this script loads discharge and precipitation timeseries from
# different CSV files and exports it to NetCDF. Note, that discharge per area will
# calculates by deviding discharge volume by catchment. The resulting specific
# discahrge will be exported as variable to NetCDF, too.

source("./R/setup.R")
library(units)

# Parameters
prec_path <- "./data/precipitation_timeseries.csv"
disch_path <- "./data/discharge_timeseries.csv"
subbasin_file <- "./data/wv_subbasins.geojson"

# Load discharge and precipitation timeseries data and join both datasets
prec_data <- read_csv(prec_path, comment = "#") %>% 
  pivot_longer(!date, names_to = "catchment_id", values_to = "precipitation")

disch_data <- read_csv(disch_path, comment = "#", col_types = cols(date = col_date(), .default=col_double())) %>% 
  pivot_longer(!date, names_to = "catchment_id", values_to = "discharge")

timeseries_data <- disch_data %>% inner_join(prec_data, by = c("date", "catchment_id"))

# Load basins, calculate the area of each one and join with timeseries data
subbasins <- load_catchments_as_sf(subbasin_file)
subbasins$id <- subbasins$id %>% as.character()

basin_areas <- subbasins %>%
  mutate("area" = st_area(.)) %>%
  as_tibble() %>%
  select(id, area)

timeseries_data <- timeseries_data %>% inner_join(basin_areas, by = c("catchment_id" = "id"))

# Set units for precipitation and discharge data
timeseries_data$discharge <- timeseries_data$discharge %>% set_units(m^3/d)
timeseries_data$precipitation <- timeseries_data$precipitation %>% set_units(mm/d)

# Calculate specific discharge by dividing discharge volume by catchment area
# Subsequently convert specific discharge to mm per day
timeseries_data <- timeseries_data %>%
  mutate(discharge_spec = discharge / area)
units(timeseries_data$discharge_spec) <-  make_units(mm/d)

# Save the resulting tibble as NetCDF file
save_tibble_as_netcdf(
  select(timeseries_data, -area),
  out_path = "./output/",
  id_col = "catchment_id",
  date_col = "date",
  date_dim = "date"
)