# This script is used to get the model coefficients of the baseline model, as pct does, 
# by using an existing dataset (IMOB) and estimate their coefficients

# it reproduces: https://itsleeds.github.io/pct/reference/model_pcycle_pct_2020.html
# but with data from Lisbon Metro Region, and trip data from IMOB 2017

# the coefficients are as explained in https://itsleeds.github.io/pct/reference/uptake_pct_godutch.html
# to be latter used as arguments of a given uptake function



# packages used -------------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(stplanr)
# devtools::install_github("cyclestreets/cyclestreets-r") #update for batch routing function
library(cyclestreets)
library(slopes)
library(pct)


# data ----------------------------------------------------------------------------------------

# Get trips by OD pair, volume and cycling % between OD pair

ODtrips = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/TRIPSmode_freguesias.Rds")) #is geometry is already as desire line

ODtrips = st_transform(ODtrips, 4326) # project in WGS84 (gps style)


table(ODtrips$Bike == 0) #6991 don't have at least 1 bike trip. Should we keep those routes in the model?


ODtrips = ODtrips %>%
  mutate(pcycle = Bike/Total,
                             weights = Total,
                             id = seq.int(nrow(ODtrips))) %>% 
  filter(DICOFREor11 != DICOFREde11) %>% 
  select(id, pcycle, weights, geometry)




# Routing -------------------------------------------------------------------------------------

# ODroutes = stplanr::route(
#   l = ODtrips,
#   route_fun = journey,
#   plan = "quietest"
# ) # takes 2.5hours


ODroutes = batch(ODtrips,
                 name = "aml_all_quiet",
                 strategies = "quietest", #or quietest c("fastest", "quietest")
                 maxDistance = 70000, #max(st_length(ODtrips)) is 68.6 km. default is max 5km
                 filename = "odall_aml",
                 includeJsonOutput = 1, #0 - only summary info like time and dist
                 username = "temospena",
                 password = Sys.getenv("CYCLESTREETS_PW"), #pw for CS account
                 pat = Sys.getenv("CYCLESTREETS"), #API key from CS for this project
                 serverId = 1
)
#this took 30 minutes for 7194 od pairs

# https://www.cyclestreets.net/journey/batch/3189/ -> journeys are here!
#download the results there (this R package is not dealing well with that finalization)
# https://www.cyclestreets.net/journey/batch/3189/odall_aml-data.csv.gz


# res = cyclestreets:::batch_read("D:/rosa/Transferencias/odall_aml-data.csv.gz") #not working
ODroutes = read_csv("routes_unjitered_full.csv")
ODroutes = ODroutes %>% filter(!is.na(json))

ODroutesjson = geojsonsf::geojson_sf(ODroutes$json, expand_geometries = FALSE) %>% filter(!is.na(calories))

# ODroutesjson %>% tmap::qtm() #looks good!

ODroutes = ODroutes %>% select(start_id, distance, time_seconds, calories) %>%
  left_join(ODroutesjson, by=c("distance" = "length", "time_seconds" = "time", "calories" = "calories")) %>%
  select(-waypoint)

ODroutes = st_as_sf(ODroutes)

rm(ODroutesjson)

# merge tables --------------------------------------------------------------------------------

ODroutes$length = round(units::drop_units(st_length(ODroutes))/1000,3) #in km
ODroutes$id = as.integer(gsub("_alpha", "", ODroutes$start_id))

ODroutes = ODroutes %>% left_join(ODtrips %>% st_drop_geometry()) %>%
  select(id, pcycle, length, weights, geometry)
names(ODroutes)[3] = "distance"


# gradient ------------------------------------------------------------------------------------

dem25 = raster::raster("r5r/LisboaAML_COPERNICUS_clip_WGS84.tif")
ODroutes$gradient = slopes::slope_raster(ODroutes, dem25)

summary(ODroutes$gradient)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.007551 0.023941 0.031950 0.032338 0.039451 0.082551 

ODroutes$gradient_perc = ODroutes$gradient*100

write.table(ODroutes %>% st_drop_geometry() %>% select(-id), "model_IMOB2017.txt", row.names = F, sep = "\t")

# model pct -----------------------------------------------------------------------------------

model_imob = model_pcycle_pct_2020(
  pcycle = ODroutes$pcycle,
  distance = ODroutes$distance,
  gradient = ODroutes$gradient_perc,
  weights = ODroutes$weights
)

model_imob
model_imob$coefficients

# (Intercept)                 distance           sqrt(distance)            I(distance^2)                 gradient  
# -5.63547                 -0.15092                  0.67306                  0.00150                 -0.58156  
# distance:gradient  sqrt(distance):gradient  
# -0.06099                  0.35138 