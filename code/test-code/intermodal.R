# Multi-modal intefaces for scenario 2


# Read and clean data -------------------------------------------------------------------------

library(tidyverse)
library(sf)
# train_stations = st_read("D:/GIS/TML/dados/Interfaces/estacoes_comboio.shp")
# train_stations = train_stations %>% st_transform(crs = 3857) %>% select(ID, NAME, geometry)
# 
# mapview::mapview(train_stations)
# 
# saveRDS(train_stations, "train_stations.Rds")
# piggyback::pb_upload("train_stations.Rds")

train_stations = readRDS("train_stations.Rds")

# ferry_stations = st_read("D:/GIS/TML/dados/Interfaces/estacoes_fluviais.shp")
# ferry_stations = ferry_stations %>% st_transform(crs = 3857) %>% select(OBJECTID, POI_NAME, geometry)
# 
# mapview::mapview(ferry_stations)
# 
# ferry_stations = ferry_stations %>% filter(!ferry_stations$POI_NAME %in% c("Fluvial Tróia", "Fluvial Setúbal")) #remove the ones that are extra AML
# 
# names(ferry_stations) = names(train_stations)
# 
# saveRDS(ferry_stations, "ferry_stations.Rds")
# piggyback::pb_upload("ferry_stations.Rds")

ferry_stations = readRDS("ferry_stations.Rds")

library(tmap)
tmap_mode("view")

tm_shape(train_stations) +
  tm_sf(col = "blue", size = 0.3) +
  tm_shape(ferry_stations) +
  tm_sf(col = "red", size = 0.3)

          