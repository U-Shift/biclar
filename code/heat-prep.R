# prepare tables for HEAT SE estimations
library(tidyverse)
library(sf)


# locations id
HEAT_all_locations_data = read_csv("HEAT/all_locations_data.csv") #data from https://github.com/HEAT-WHO/HEAT_data/blob/main/data/heatr/locations_data/all_locations_data.csv
HEAT_all_locations_data = HEAT_all_locations_data %>% filter(country_location_name == "Portugal")
HEAT_all_locations_data = HEAT_all_locations_data %>% filter(sub1_location_name %in% c("Distrito de Lisboa", "Distrito de Setúbal"))
HEAT_all_locations_data = HEAT_all_locations_data %>% filter(is.na(city_location_id))
# saveRDS(HEAT_all_locations_data, "HEAT/HEAT_locations_data_AML.Rds")
# match location id with the 18 different locations in the spreadsheet, and add mean in missing ones and in AML (region).

# PM25 por municipio, incluindo AML
PM25 = readxl::read_excel("export2/Scenarios_Routing.xlsx", sheet = "PM25")
PM25$PM25 = round(PM25$PM25, 2)
PM25$location_id = as.character(PM25$location_id)
