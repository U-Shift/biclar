# Aim: test out Jittering in study region
remotes::install_github("atumworld/odr")
# remotes::install_github("dabreegster/odjitter", subdir = "r") ?
library(tidyverse)
library(tmap)
tmap_mode("view")

od_all = readRDS("TRIPSmode_freguesias.Rds")
zones = readRDS("FREGUESIASgeo.Rds")
od_all_sf = od::od_to_sf(od_all, zones)
# plot(od_all_sf)
od_all_sf_top = od_all_sf %>% 
  filter(DICOFREor11 != DICOFREde11) %>% 
  top_n(n = 100, wt = Total)

# osm_data_national = osmextract::oe_get_network(place = "Portugal", mode = "cycling", force_download = TRUE)
# nrow(osm_data_national) # 1 million rows, 1106127 (2022-02)
# osm_data_region = osm_data_national[zones, ]
# nrow(osm_data_region) # 142201
# saveRDS(osm_data_region, "osm_data_region.Rds")
# piggyback::pb_upload("osm_data_region.Rds")
osm_data_region = readRDS("osm_data_region.Rds")
osm_data_region = osm_data_region %>% 
  filter(!str_detect(highway, pattern = "motor|trunk|path|services|ped"))
osm_data_region_top = osm_data_region %>% 
  sample_n(500)

tm_shape(zones) +
  tm_borders() +
  tm_shape(osm_data_region_top) +
  tm_lines("highway")

summary(od_all$Total)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 1.54    27.42   122.16   724.82   469.94 66521.17 
set.seed(42)
od_all = od_all %>% 
  filter(Total > 200)

od_all_jittered = odjitter::jitter(
  od = od_all,
  zones = zones,
  subpoints = osm_data_region,
  disaggregation_key = "Total",
  disaggregation_threshold = 500 
)
nrow(od_all_jittered) # 11604 with threshold 500
# nrow(od_all_jittered) # 57356 - with threshold 100
saveRDS(od_all_jittered, "od_all_jittered_500.Rds")
# saveRDS(od_all_jittered, "od_all_jittered_100.Rds")
piggyback::pb_upload("od_all_jittered_500.Rds")
# piggyback::pb_upload("od_all_jittered_100.Rds")

# case study zones
small_netowrk = sf::st_transform(slopes::lisbon_road_network, 4326)
lisbon_central = zones[small_netowrk, ]
qtm(lisbon_central)

od_jittered_central = od_all_jittered %>%
  filter(DICOFREde11 %in% lisbon_central$Dicofre)
qtm(od_jittered_central)

od_jittered_central = od_all_jittered[lisbon_central, ]
od_jittered_top = od_jittered_central %>% 
  sample_n(size = 1000)
qtm(od_jittered_top)
