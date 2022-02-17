# Aim: test out Jittering in study region
remotes::install_github("atumworld/odr")
library(tidyverse)

od_all = readRDS("TRIPSmode_freguesias.Rds")
zones = readRDS("FREGUESIASgeo.Rds")
od_all_sf = od::od_to_sf(od_all, zones)
plot(od_all_sf)

osm_data_region = osmextract::oe_get_network(place = "Portugal", mode = "cycling", )
