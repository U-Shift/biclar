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
options(java.parameters = '-Xmx44G') #memory max 8GB in laptop, 44 in desktop
library(r5r)
library(stplanr)
library(tmap)


# GTFSall = tidytransit::read_gtfs("r5r/GTFS_AML_2022_bikeonly_noCarris.zip")
# tidytransit::validate_gtfs(GTFSall)
# GTFSall[["."]] = NULL
# tidytransit::write_gtfs(GTFSall, "r5r/GTFS_AML_2022_noCarris.zip")
GTFSall = gtfstools::read_gtfs("r5r/allmodes/GTFS_AML_2022_bikeonly.zip")
GTFSall = gtfstools::frequencies_to_stop_times(GTFSall) #necessary to ifnore frequencies.txt
gtfstools::write_gtfs(GTFSall, "r5r/GTFS_AML_2022_bikeonly_noCarris.zip") #then delete frequencies.txt inside zip


# r5r_lts_intermodality = setup_r5(data_path = "r5r/", elevation = "MINETTI") #to create new, delete network.dat in the folder. otherwise just load it
# includes .pbf of OSM ALM + .tif of AML raster coopernicus 25m // + gtfs for all modes except bus

# #export nework with osm_id and LTS levels
# r5r_lts_intermodality_shp = street_network_to_sf(r5r_lts_intermodality)
# r5r_lts_intermodality_shp = r5r_lts_intermodality_shp$edges
# saveRDS(r5r_lts_intermodality_shp, "r5r/r5r_lts_intermodality_elev_202210_shp.Rds")


r5r_lts_intermodalityALL = setup_r5(data_path = "r5r/allmodes2/", elevation = "MINETTI") #to create new, delete network.dat in the folder. otherwise just load it
# includes .pbf of OSM ALM + .tif of AML raster coopernicus 25m // + gtfs for all modes

# # #export nework with osm_id and LTS levels
# r5r_lts_intermodalityALL_shp = street_network_to_sf(r5r_lts_intermodalityALL)
# r5r_lts_intermodalityALL_shp = r5r_lts_intermodalityALL_shp$edges
# saveRDS(r5r_lts_intermodalityALL_shp, "r5r/allmodes/r5r_lts_intermodalityALL_elev_202210_shp.Rds")


## with a threshold of 100 jittered trips, resulting 57356 od pairs

od_jittered_100 = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/od_all_jittered_100.Rds"))
# od_jittered_test = od_jittered_100
# slice_sample(n = 10)
# od_jittered_test = readRDS("od_all_jittered_500.Rds")
# od_jittered_test = readRDS("od_all_jittered_100.Rds") #use this on final product


od_jittered_100$distance = as.numeric(st_length(od_jittered_100))
od_jittered_100filter = od_jittered_100 %>%
  # filter(distance < 1200) %>%   #max 9km - 75% #hide for river crossing trips
  filter(Total > 10) #exclude od pairs with less than 10 trips 
od_jittered_100filter$id = 1:nrow(od_jittered_100filter)
nrow(od_jittered_100filter) # 56626

#with stplanr
od_jittered_100filter_points = line2df(od_jittered_100filter)
od_jittered_100filter_OR = od_jittered_100filter_points[,c(1,2,3)]
names(od_jittered_100filter_OR) = c("id", "lon", "lat")
od_jittered_100filter_DE = od_jittered_100filter_points[,c(1,4,5)]
names(od_jittered_100filter_DE) = c("id", "lon", "lat")

#departure time, as WET?
departure_datetime <- as.POSIXct("13-10-2022 09:00:00", format = "%d-%m-%Y %H:%M:%S")
departure_datetime = lubridate::force_tz(departure_datetime, "WET") #make it western european time




# #routing with LTS3 (enthused and confident) - all transit modes (except Bus and subway)
# routes_r5r_100jit_lts3__intermod_NoSub_elev = detailed_itineraries(
#   r5r_lts_intermodalityALL,
#   origins = od_jittered_100filter_OR,
#   destinations = od_jittered_100filter_DE,
#   mode = c("BICYCLE", "RAIL", "TRAM", "FERRY", "BUS"), #remove subway
#   mode_egress = "BICYCLE",
#   departure_datetime = departure_datetime,
#   # time_window = 1L,
#   # suboptimal_minutes = 0L,
#   fare_structure = NULL,
#   max_fare = Inf,
#   max_walk_time = Inf,
#   max_bike_time = 30L, #25min no total - será pouco ou muito?
#   max_trip_duration = 120L, #in minutes
#   # walk_speed = 3.6,
#   bike_speed = 14, #higher to be competitive with PT
#   max_rides = 1, #max public transit rides for the same trip
#   max_lts = 3, #1 - quietest, 4 - hardcore
#   shortest_path = TRUE, #FALSE? fastest or multiple alternatives?
#   all_to_all = FALSE,
#   n_threads = Inf,
#   verbose = FALSE,
#   progress = TRUE,
#   drop_geometry = FALSE,
#   output_dir = NULL
# )
# 
# saveRDS(routes_r5r_100jit_lts3__intermod_NoSub_elev, "routes_r5r_100jit_lts3__intermodALL_NoSubBus_elev_resultsraw.Rds")
# 
# routes_r5r_100jit_lts3__intermod_NoSub_elev = routes_r5r_100jit_lts3__intermod_NoSub_elev %>% mutate(id = as.integer(from_id)) %>%
#   select(id, total_duration, total_distance, segment, mode, segment_duration, distance, route, geometry) %>%
#   left_join(od_jittered_100filter %>% st_drop_geometry(), by="id")
# 
# names(routes_r5r_100jit_lts3__intermod_NoSub_elev)[7] = "distance"
# names(routes_r5r_100jit_lts3__intermod_NoSub_elev)[18] = "eucl_distance"
# 
# saveRDS(routes_r5r_100jit_lts3__intermod_NoSub_elev, "routes_r5r_100jit_lts3__intermodALL_NoSubBus_elev_raw.Rds")
# 
# 
# 
# 
# 
# #routing with lts2 (enthused and confident) - all transit modes (except Bus - no data)
# routes_r5r_100jit_lts2__intermod_NoSub_elev = detailed_itineraries(
#   r5r_lts_intermodalityALL,
#   origins = od_jittered_100filter_OR,
#   destinations = od_jittered_100filter_DE,
#   mode = c("BICYCLE", "RAIL", "TRAM", "FERRY"), #remove subway
#   mode_egress = "BICYCLE",
#   departure_datetime = as.POSIXct("13-10-2022 09:00:00", format = "%d-%m-%Y %H:%M:%S"), #Sys.time(), 
#   # time_window = 1L,
#   # suboptimal_minutes = 0L,
#   fare_structure = NULL,
#   max_fare = Inf,
#   max_walk_time = Inf,
#   max_bike_time = 25L, #25min no total - será pouco ou muito?
#   max_trip_duration = 120L, #in minutes
#   # walk_speed = 3.6,
#   bike_speed = 14, #higher to be competitive with PT
#   max_rides = 1, #max public transit rides for the same trip
#   max_lts = 2, #1 - quietest, 4 - hardcore
#   shortest_path = TRUE, #FALSE? fastest or multiple alternatives?
#   all_to_all = FALSE,
#   n_threads = Inf,
#   verbose = FALSE,
#   progress = TRUE,
#   drop_geometry = FALSE,
#   output_dir = NULL
# )
# 
# saveRDS(routes_r5r_100jit_lts2__intermod_NoSub_elev, "routes_r5r_100jit_lts2__intermod_NoSubBus_elev_resultsraw.Rds")
# 
# routes_r5r_100jit_lts2__intermod_NoSub_elev = routes_r5r_100jit_lts2__intermod_NoSub_elev %>% mutate(id = as.integer(from_id)) %>%
#   select(id, total_duration, total_distance, segment, mode, segment_duration, distance, route, geometry) %>%
#   left_join(od_jittered_100filter %>% st_drop_geometry(), by="id")
# 
# names(routes_r5r_100jit_lts2__intermod_NoSub_elev)[7] = "distance"
# names(routes_r5r_100jit_lts2__intermod_NoSub_elev)[18] = "eucl_distance"
# 
# saveRDS(routes_r5r_100jit_lts2__intermod_NoSub_elev, "routes_r5r_100jit_lts2__intermod_NoSubBus_elev_raw.Rds")



#routing with LTS3 (enthused and confident) - all transit modes
routes_r5r_100jit_lts3__intermodALL_NoSub_elev = detailed_itineraries(
  r5r_lts_intermodalityALL,
  origins = od_jittered_100filter_OR,
  destinations = od_jittered_100filter_DE,
  mode = c("BICYCLE", "BUS", "RAIL", "TRAM", "FERRY"), #remove subway
  mode_egress = "BICYCLE",
  departure_datetime = as.POSIXct("13-10-2022 09:00:00", format = "%d-%m-%Y %H:%M:%S"), #Sys.time(), 
  # time_window = 1L,
  # suboptimal_minutes = 0L,
  fare_structure = NULL,
  max_fare = Inf,
  max_walk_time = Inf,
  max_bike_time = 25L, #25min no total - será pouco ou muito?
  max_trip_duration = 120L, #in minutes
  # walk_speed = 3.6,
  bike_speed = 14, #higher to be competitive with PT
  max_rides = 1, #max public transit rides for the same trip
  max_lts = 3, #1 - quietest, 4 - hardcore
  shortest_path = TRUE, #FALSE? fastest or multiple alternatives?
  all_to_all = FALSE,
  n_threads = Inf,
  verbose = FALSE,
  progress = TRUE,
  drop_geometry = FALSE,
  output_dir = NULL
)

saveRDS(routes_r5r_100jit_lts3__intermodALL_NoSub_elev, "routes_r5r_100jit_lts3__intermodALL_NoSub_elev_resultsraw.Rds")

routes_r5r_100jit_lts3__intermodALL_NoSub_elev = routes_r5r_100jit_lts3__intermodALL_NoSub_elev %>% mutate(id = as.integer(from_id)) %>%
  select(id, total_duration, total_distance, segment, mode, segment_duration, distance, route, geometry) %>%
  left_join(od_jittered_100filter %>% st_drop_geometry(), by="id")

names(routes_r5r_100jit_lts3__intermodALL_NoSub_elev)[7] = "distance"
names(routes_r5r_100jit_lts3__intermodALL_NoSub_elev)[18] = "eucl_distance"

saveRDS(routes_r5r_100jit_lts3__intermodALL_NoSub_elev, "routes_r5r_100jit_lts3__intermodALL_NoSub_elev_raw.Rds")

# 
# 
# #routing with LTS2 (nut concerned) - only ferry
# routes_r5r_100jit_lts2__ferry_elev = detailed_itineraries(
#   r5r_lts_intermodalityALL,
#   origins = od_jittered_100filter_OR,
#   destinations = od_jittered_100filter_DE,
#   mode = c("BICYCLE", "FERRY"), #only ferry
#   mode_egress = "BICYCLE",
#   departure_datetime = departure_datetime,
#   # departure_datetime = as.POSIXct("13-10-2022 09:00:00", format = "%d-%m-%Y %H:%M:%S"), #Sys.time(), 
#   # time_window = 1L,
#   # suboptimal_minutes = 0L,
#   fare_structure = NULL,
#   max_fare = Inf,
#   max_walk_time = Inf,
#   max_bike_time = Inf, #no restrictons 
#   max_trip_duration = 120L, #in minutes
#   # walk_speed = 3.6,
#   bike_speed = 14, #higher to be competitive with PT
#   max_rides = 1, #max public transit rides for the same trip
#   max_lts = 2, #1 - quietest, 4 - hardcore
#   shortest_path = TRUE, #FALSE? fastest or multiple alternatives?
#   all_to_all = FALSE,
#   n_threads = Inf,
#   verbose = FALSE,
#   progress = TRUE,
#   drop_geometry = FALSE,
#   output_dir = NULL
# )
# 
# saveRDS(routes_r5r_100jit_lts2__ferry_elev, "routes_r5r_100jit_lts2__ferry_elev_resultsraw.Rds")
# 
# routes_r5r_100jit_lts2__ferry_elev = routes_r5r_100jit_lts2__ferry_elev %>% mutate(id = as.integer(from_id)) %>%
#   select(id, total_duration, total_distance, segment, mode, segment_duration, distance, route, geometry) %>%
#   left_join(od_jittered_100filter %>% st_drop_geometry(), by="id")
# 
# names(routes_r5r_100jit_lts2__ferry_elev)[7] = "distance"
# names(routes_r5r_100jit_lts2__ferry_elev)[18] = "eucl_distance"
# 
# saveRDS(routes_r5r_100jit_lts2__ferry_elev, "routes_r5r_100jit_lts2__ferry_elev_raw.Rds")
# 
# 
# 
# #routing with LTS2 (but concerned) - all transit modes
# routes_r5r_100jit_lts2__intermodALL_NoSub_elev = detailed_itineraries(
#   r5r_lts_intermodalityALL,
#   origins = od_jittered_100filter_OR,
#   destinations = od_jittered_100filter_DE,
#   mode = c("BICYCLE", "BUS", "RAIL", "TRAM", "FERRY"), #remove subway
#   mode_egress = "BICYCLE",
#   departure_datetime = as.POSIXct("13-10-2022 09:00:00", format = "%d-%m-%Y %H:%M:%S"), #Sys.time(), 
#   # time_window = 1L,
#   # suboptimal_minutes = 0L,
#   fare_structure = NULL,
#   max_fare = Inf,
#   max_walk_time = Inf,
#   max_bike_time = 25L, #25min no total - será pouco ou muito?
#   max_trip_duration = 120L, #in minutes
#   # walk_speed = 3.6,
#   bike_speed = 14, #higher to be competitive with PT
#   max_rides = 1, #max public transit rides for the same trip
#   max_lts = 2, #1 - quietest, 4 - hardcore
#   shortest_path = TRUE, #FALSE? fastest or multiple alternatives?
#   all_to_all = FALSE,
#   n_threads = Inf,
#   verbose = FALSE,
#   progress = TRUE,
#   drop_geometry = FALSE,
#   output_dir = NULL
# )
# 
# saveRDS(routes_r5r_100jit_lts2__intermodALL_NoSub_elev, "routes_r5r_100jit_lts2__intermodALL_NoSub_elev_resultsraw.Rds")
# 
# routes_r5r_100jit_lts2__intermodALL_NoSub_elev = routes_r5r_100jit_lts2__intermodALL_NoSub_elev %>% mutate(id = as.integer(from_id)) %>%
#   select(id, total_duration, total_distance, segment, mode, segment_duration, distance, route, geometry) %>%
#   left_join(od_jittered_100filter %>% st_drop_geometry(), by="id")
# 
# names(routes_r5r_100jit_lts2__intermodALL_NoSub_elev)[7] = "distance"
# names(routes_r5r_100jit_lts2__intermodALL_NoSub_elev)[18] = "eucl_distance"
# 
# saveRDS(routes_r5r_100jit_lts2__intermodALL_NoSub_elev, "routes_r5r_100jit_lts2__intermodALL_NoSub_elev_raw.Rds")
# 


#routing with lts4 (strong and fearless) - all transit modes
routes_r5r_100jit_lts4__intermodALL_NoSub_elev = detailed_itineraries(
  r5r_lts_intermodalityALL,
  origins = od_jittered_100filter_OR,
  destinations = od_jittered_100filter_DE,
  mode = c("BICYCLE", "BUS", "RAIL", "TRAM", "FERRY"), #remove subway
  mode_egress = "BICYCLE",
  departure_datetime = as.POSIXct("13-10-2022 09:00:00", format = "%d-%m-%Y %H:%M:%S"), #Sys.time(), 
  # time_window = 1L,
  # suboptimal_minutes = 0L,
  fare_structure = NULL,
  max_fare = Inf,
  max_walk_time = Inf,
  max_bike_time = 25L, #25min no total - será pouco ou muito?
  max_trip_duration = 120L, #in minutes
  # walk_speed = 3.6,
  bike_speed = 14, #higher to be competitive with PT
  max_rides = 1, #max public transit rides for the same trip
  max_lts = 4, #1 - quietest, 4 - hardcore
  shortest_path = TRUE, #FALSE? fastest or multiple alternatives?
  all_to_all = FALSE,
  n_threads = Inf,
  verbose = FALSE,
  progress = TRUE,
  drop_geometry = FALSE,
  output_dir = NULL
)

saveRDS(routes_r5r_100jit_lts4__intermodALL_NoSub_elev, "routes_r5r_100jit_lts4__intermodALL_NoSub_elev_resultsraw.Rds")

routes_r5r_100jit_lts4__intermodALL_NoSub_elev = routes_r5r_100jit_lts4__intermodALL_NoSub_elev %>% mutate(id = as.integer(from_id)) %>%
  select(id, total_duration, total_distance, segment, mode, segment_duration, distance, route, geometry) %>%
  left_join(od_jittered_100filter %>% st_drop_geometry(), by="id")

names(routes_r5r_100jit_lts4__intermodALL_NoSub_elev)[7] = "distance"
names(routes_r5r_100jit_lts4__intermodALL_NoSub_elev)[18] = "eucl_distance"

saveRDS(routes_r5r_100jit_lts4__intermodALL_NoSub_elev, "routes_r5r_100jit_lts4__intermodALL_NoSub_elev_raw.Rds")





#routing with lts4 (strong and fearless) - only ferry
routes_r5r_100jit_lts4__ferry_elev = detailed_itineraries(
  r5r_lts_intermodalityALL,
  origins = od_jittered_100filter_OR,
  destinations = od_jittered_100filter_DE,
  mode = c("BICYCLE", "FERRY"), #only ferry
  mode_egress = "BICYCLE",
  departure_datetime = departure_datetime, #Sys.time(), 
  # time_window = 1L,
  # suboptimal_minutes = 0L,
  fare_structure = NULL,
  max_fare = Inf,
  max_walk_time = Inf,
  max_bike_time = Inf, #no restrictons 
  max_trip_duration = 120L, #in minutes
  # walk_speed = 3.6,
  bike_speed = 14, #higher to be competitive with PT
  max_rides = 1, #max public transit rides for the same trip
  max_lts = 4, #1 - quietest, 4 - hardcore
  shortest_path = TRUE, #FALSE? fastest or multiple alternatives?
  all_to_all = FALSE,
  n_threads = Inf,
  verbose = FALSE,
  progress = TRUE,
  drop_geometry = FALSE,
  output_dir = NULL
)

saveRDS(routes_r5r_100jit_lts4__ferry_elev, "routes_r5r_100jit_lts4__ferry_elev_resultsraw.Rds")

routes_r5r_100jit_lts4__ferry_elev = routes_r5r_100jit_lts4__ferry_elev %>% mutate(id = as.integer(from_id)) %>%
  select(id, total_duration, total_distance, segment, mode, segment_duration, distance, route, geometry) %>%
  left_join(od_jittered_100filter %>% st_drop_geometry(), by="id")

names(routes_r5r_100jit_lts4__ferry_elev)[7] = "distance"
names(routes_r5r_100jit_lts4__ferry_elev)[18] = "eucl_distance"

saveRDS(routes_r5r_100jit_lts4__ferry_elev, "routes_r5r_100jit_lts4__ferry_elev_raw.Rds")





#routing with lts3 (enthused and confident) - only ferry
routes_r5r_100jit_lts3__ferry_elev = detailed_itineraries(
  r5r_lts_intermodalityALL,
  origins = od_jittered_100filter_OR,
  destinations = od_jittered_100filter_DE,
  mode = c("BICYCLE", "FERRY"), #only ferry
  mode_egress = "BICYCLE",
  departure_datetime = departure_datetime, 
  # time_window = 1L,
  # suboptimal_minutes = 0L,
  fare_structure = NULL,
  max_fare = Inf,
  max_walk_time = Inf,
  max_bike_time = Inf, #no restrictons 
  max_trip_duration = 120L, #in minutes
  # walk_speed = 3.6,
  bike_speed = 14, #higher to be competitive with PT
  max_rides = 1, #max public transit rides for the same trip
  max_lts = 3, #1 - quietest, 4 - hardcore
  shortest_path = TRUE, #FALSE? fastest or multiple alternatives?
  all_to_all = FALSE,
  n_threads = Inf,
  verbose = FALSE,
  progress = TRUE,
  drop_geometry = FALSE,
  output_dir = NULL
)

saveRDS(routes_r5r_100jit_lts3__ferry_elev, "routes_r5r_100jit_lts3__ferry_elev_resultsraw.Rds")

routes_r5r_100jit_lts3__ferry_elev = routes_r5r_100jit_lts3__ferry_elev %>% mutate(id = as.integer(from_id)) %>%
  select(id, total_duration, total_distance, segment, mode, segment_duration, distance, route, geometry) %>%
  left_join(od_jittered_100filter %>% st_drop_geometry(), by="id")

names(routes_r5r_100jit_lts3__ferry_elev)[7] = "distance"
names(routes_r5r_100jit_lts3__ferry_elev)[18] = "eucl_distance"

saveRDS(routes_r5r_100jit_lts3__ferry_elev, "routes_r5r_100jit_lts3__ferry_elev_raw.Rds")


###### STOP HERE #######
# see r5r_filter_overline_routes.R for nex steps





