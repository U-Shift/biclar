# Compute the routing for jittered od pairs, for fast and quiet routes

# using r5r
# see: https://ipeagit.github.io/r5r/reference/detailed_itineraries.html

library(dplyr)
library(sf)
# library(rJava)
# Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-11.0.11/")
# # get installed version of Java
# .jinit()
# .jcall("java.lang.System","S","getProperty","java.version") #should be 11!
options(java.parameters = '-Xmx16G') #memory max 16GB
options(java.home="C:/Program Files/Java/jdk-11.0.11/")
library(r5r)
library(stplanr)
library(tmap)

#problem with frequencies of ferry
# remotes::install_github("ipeaGIT/gtfstools")
# library(gtfstools)
# ferry_gtfs <- gtfstools::read_gtfs("r5r/transtejoboth2021.zip")
# trip_geom <- get_trip_geometry(ferry_gtfs, file = "shapes")
# plot(trip_geom) #is OK!
# 

# data_path <- "r5r/ferrys.zip"
# output_path <- tempfile("validation_result")
# validator_path <- download_validator(tempdir())
# validate_gtfs(data_path, output_path, validator_path)
# 
# 
# # read GTFS data
# freq_gtfs <- gtfstools::read_gtfs("r5r/gtfs_4.zip")
# # convert from frequencies to time tables
# stop_times_gtfs <- gtfstools::frequencies_to_stop_times(freq_gtfs)
# # save it as a new GTFS.zip file
# gtfstools::write_gtfs(gtfs = stop_times_gtfs,
#                       path = 'r5r/stop_times_gtfs.zip')
# 
# names(freq_gtfs)
# spo_shapes <- read_gtfs("r5r/gtfs_4.zip", files = c("shapes", "trips"))
# trip_geom <- get_trip_geometry(freq_gtfs, file = "shapes")
# plot(trip_geom) #ERROR  in CPL_geos_is_empty(st_geometry(x))
# 
# single_trip <- freq_gtfs$trips$trip_id[1]
# single_trip
# both_geom <- get_trip_geometry(freq_gtfs, trip_id = single_trip)
# plot(both_geom["origin_file"])
# 
# write_gtfs(freq_gtfs, "ferrys.zip")



#DO NOT RUN AGAIN:
r5r_lts_elevation = setup_r5(data_path = "r5r/", elevation = "MINETTI", overwrite = TRUE) #apagar o network.dat anterior que esteja na pasta
# includes .pbf of OSM ALM + .tif of AML raster coopernicus 25m // + fluvial gtfs
# ATTENTION: With elevation MINETTI this process takes about 24h for AML!

# r5r_lts_elevation_network = r5r::transit_network_to_sf(r5r_lts_elevation)
# plot(r5r_lts_elevation_network$routes) #ferrys are ok there!

## with a threshold of 100 jittered trips, resulting 57356 od pairs

# od_jittered_100 = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/od_all_jittered_100.Rds"))
# od_jittered_test = od_jittered_100
# slice_sample(n = 10)
# od_jittered_test = readRDS("od_all_jittered_500.Rds")
# od_jittered_test = readRDS("od_all_jittered_100.Rds")
od_jittered_500 = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/od_all_jittered_500.Rds"))
# od_jittered_500 = readRDS("od_all_jittered_500.Rds")

od_jittered_500$distanceOD = as.numeric(st_length(od_jittered_500))
od_jittered_filter = od_jittered_500 %>%
  # filter(distance < 1200) %>%   #max 9km - 75% #hide for river crossing trips
  filter(Total > 10)
od_jittered_filter$id = 1:nrow(od_jittered_filter)


#with stplanr
od_jittered_filter_points = line2df(od_jittered_filter)
od_jittered_filter_OR = od_jittered_filter_points[,c(1,2,3)]
names(od_jittered_filter_OR) = c("id", "lon", "lat")
od_jittered_filter_DE = od_jittered_filter_points[,c(1,4,5)]
names(od_jittered_filter_DE) = c("id", "lon", "lat")

#r5r with elevation and ferry, for 500 disagregation and LTS2

routes_r5r_lts2_elevation = detailed_itineraries(
  r5r_lts_elevation,
  origins = od_jittered_filter_OR,
  destinations = od_jittered_filter_DE,
  mode = c("BICYCLE", "FERRY"),
  mode_egress = "BICYCLE",
  departure_datetime = Sys.time(),
  time_window = 1L,
  # suboptimal_minutes = 0L,
  fare_structure = NULL,
  max_fare = Inf,
  max_walk_time = Inf,
  max_bike_time = Inf,
  max_trip_duration = 300L, #because of GTFS ferry
  # walk_speed = 3.6,
  bike_speed = 12,
  max_rides = 3,
  max_lts = 2, #1 - quietest, 4 - hardcore
  shortest_path = TRUE, #FALSE?
  all_to_all = FALSE,
  n_threads = Inf,
  verbose = FALSE,
  progress = TRUE,
  drop_geometry = FALSE,
  output_dir = NULL
)

saveRDS(routes_r5r_lts2_elevation, "routes_jittered_500_r5rlts2_elev_ferry.Rds")


#to get the LTS level for each segment
network_r5r_elevation = r5r::street_network_to_sf(r5r_lts_elevation)
routes_r5r_lts2_elevation_lts = st_join(routes_r5r_lts2_elevation, network_r5r_elevation$edges)

#join original info of IMOB
routes_r5r_lts2_elevation_lts = routes_r5r_lts2_elevation_lts %>% mutate(id = as.integer(from_id)) %>%
  select(id, option, total_duration, total_distance, segment, mode, distance, from_vertex, to_vertex, length, bicycle_lts, geometry) %>%
  left_join(od_jittered_filter %>% st_drop_geometry(), by="id")

#estimate enmac potential
routes_r5r_lts2_elevation_lts$Bikeper = routes_r5r_lts2_elevation_lts$Bike / routes_r5r_lts2_elevation_lts$Total
routes_r5r_lts2_elevation_lts$new_cyc4 = ifelse(routes_r5r_lts2_elevation_lts$Bikeper >= ENMAC4, routes_r5r_lts2_elevation_lts$Bike, ENMAC4 * routes_r5r_lts2_elevation_lts$Total - routes_r5r_lts2_elevation_lts$Bike)
routes_r5r_lts2_elevation_lts$new_cyc10 = ifelse(routes_r5r_lts2_elevation_lts$Bikeper >= ENMAC10, routes_r5r_lts2_elevation_lts$Bike, ENMAC10 * routes_r5r_lts2_elevation_lts$Total - routes_r5r_lts2_elevation_lts$Bike)
routes_r5r_lts2_elevation_lts$new_car4 = ifelse(routes_r5r_lts2_elevation_lts$Bikeper >= ENMAC4, routes_r5r_lts2_elevation_lts$Car + routes_r5r_lts2_elevation_lts$CarP, (routes_r5r_lts2_elevation_lts$Car + routes_r5r_lts2_elevation_lts$CarP) - routes_r5r_lts2_elevation_lts$new_cyc4)
routes_r5r_lts2_elevation_lts$new_car10 = ifelse(routes_r5r_lts2_elevation_lts$Bikeper >= ENMAC10, routes_r5r_lts2_elevation_lts$Car + routes_r5r_lts2_elevation_lts$CarP, (routes_r5r_lts2_elevation_lts$Car + routes_r5r_lts2_elevation_lts$CarP) - routes_r5r_lts2_elevation_lts$new_cyc10)
routes_r5r_lts2_elevation_lts$Bike_4_total = routes_r5r_lts2_elevation_lts$Bike + routes_r5r_lts2_elevation_lts$new_cyc4
routes_r5r_lts2_elevation_lts$Bike_10_total = routes_r5r_lts2_elevation_lts$Bike + routes_r5r_lts2_elevation_lts$new_cyc10

saveRDS(routes_r5r_lts2_elevation_lts, "routes_jittered_500_r5rlts2_levels_ENMAC410.Rds") #7.019k rows, info per segment of osm

sum(routes_r5r_lts2_elevation_lts$new_cyc4) / sum(routes_r5r_lts2_elevation_lts$Total) #3.954%

#rnet with LTS
rnet_r5r_lts2_raw = overline( #this takes too much memory - try to reduce info first! 
  routes_r5r_lts2_elevation_lts,
  attrib = c("Bike", "bicycle_lts", "Bike_4_total", "Bike_10_total"),
  fun = c("mean", "sum")
)
names(rnet_r5r_lts2_raw)
rnet_r5r_lts2_raw_full = rnet_r5r_lts2_raw %>% 
  transmute(Quietness = bicycle_lts_mean, Baseline = Bike_sum, ENMAC4 = Bike_4_total, ENMAC10 = Bike_10_total) %>%
  mutate_if(is.numeric, round) 
nrow(rnet_r5r_lts2_raw_full) # 59 k for quiet, 56 k for fast

saveRDS(rnet_r5r_lts2_raw_full, "rnet_r5r_lts2_raw_full.Rds")








# old version ---------------------------------------------------------------------------------


#r5r without elevation and ferry

routes_r5r_lts3 = detailed_itineraries(
  r5r_lts,
  origins = od_jittered_filter_OR,
  destinations = od_jittered_filter_DE,
  mode = "BICYCLE",
  # mode_egress = "WALK",
  # departure_datetime = Sys.time(),
  # time_window = 1L,
  # suboptimal_minutes = 0L,
  fare_structure = NULL,
  max_fare = Inf,
  max_walk_time = Inf,
  max_bike_time = Inf,
  max_trip_duration = 120L, #in minutes
  # walk_speed = 3.6,
  bike_speed = 12,
  # max_rides = 3,
  max_lts = 3, #1 - quietest, 4 - hardcore
  shortest_path = TRUE, #FALSE?
  all_to_all = FALSE,
  n_threads = Inf,
  verbose = FALSE,
  progress = TRUE,
  drop_geometry = FALSE,
  output_dir = NULL
)

routes_r5r_lts3 = routes_r5r_lts3 %>% mutate(id = as.integer(from_id)) %>%
  select(id, total_duration, total_distance, route) %>%
  left_join(od_jittered_filter %>% st_drop_geometry(), by="id")

# filter by duration > 1h?


#cycling potential function
ENMAC4 = 0.04 # 4%
ENMAC10 = 0.10 # 10#
routes_r5r_lts3$Bikeper = routes_r5r_lts3$Bike / routes_r5r_lts3$Total

routes_r5r_lts3$new_cyc4 = ifelse(routes_r5r_lts3$Bikeper >= ENMAC4, routes_r5r_lts3$Bike, ENMAC4 * routes_r5r_lts3$Total - routes_r5r_lts3$Bike)
routes_r5r_lts3$new_cyc10 = ifelse(routes_r5r_lts3$Bikeper >= ENMAC10, routes_r5r_lts3$Bike, ENMAC10 * routes_r5r_lts3$Total - routes_r5r_lts3$Bike)
routes_r5r_lts3$new_car4 = ifelse(routes_r5r_lts3$Bikeper >= ENMAC4, routes_r5r_lts3$Car + routes_r5r_lts3$CarP, (routes_r5r_lts3$Car + routes_r5r_lts3$CarP) - routes_r5r_lts3$new_cyc4)
routes_r5r_lts3$new_car10 = ifelse(routes_r5r_lts3$Bikeper >= ENMAC10, routes_r5r_lts3$Car + routes_r5r_lts3$CarP, (routes_r5r_lts3$Car + routes_r5r_lts3$CarP) - routes_r5r_lts3$new_cyc10)



#export
# saveRDS(routes_jittered_ENMAC410, "routes_jittered_ENMAC410_fastest_500.Rds")
saveRDS(routes_r5r_lts3, "routes_jittered_ENMAC410_LTS3_100.Rds")

qtm(routes_r5r_lts3)
## NOT USEFUL - does not cross the river !!!

# rnet
rnet_enmac_raw = overline2( #not working...
  routes_r5r_lts3,
  attrib = c("Bike", "new_cyc4", "new_cyc10"),
  fun = c("mean", "sum")
)
names(rnet_enmac_raw)
rnet_enmac_full = rnet_enmac_raw %>% 
  transmute(Quietness = quietness_mean, Baseline = Bike_sum, ENMAC4 = new_cyc4_sum, ENMAC10 = new_cyc10_sum) %>%
  mutate_if(is.numeric, round) 
nrow(rnet_enmac_full) # 59 k for quiet, 56 k for fast
sum(routes_jittered_500_ENMAC410$new_cyc4) / sum(routes_jittered_500_ENMAC410$Total)