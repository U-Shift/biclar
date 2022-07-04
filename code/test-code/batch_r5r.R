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
options(java.parameters = '-Xmx32G') #memory max 16GB
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

# saveRDS(routes_r5r_lts2_elevation, "routes_jittered_500_r5rlts2_elev_ferry.Rds")
routes_r5r_lts2_elevation = readRDS("routes_jittered_500_r5rlts2_elev_ferry.Rds")

#join original info of IMOB
routes_r5r_lts2_elevation = routes_r5r_lts2_elevation %>% mutate(id = as.integer(from_id)) %>%
  select(id, option, total_duration, total_distance, segment, mode, distance, geometry) %>%
  left_join(od_jittered_filter %>% st_drop_geometry(), by="id")

#estimate enmac potential
routes_r5r_lts2_elevation$Bikeper = routes_r5r_lts2_elevation$Bike / routes_r5r_lts2_elevation$Total
routes_r5r_lts2_elevation$new_cyc4 = ifelse(routes_r5r_lts2_elevation$Bikeper >= ENMAC4, routes_r5r_lts2_elevation$Bike, ENMAC4 * routes_r5r_lts2_elevation$Total - routes_r5r_lts2_elevation$Bike)
routes_r5r_lts2_elevation$new_cyc10 = ifelse(routes_r5r_lts2_elevation$Bikeper >= ENMAC10, routes_r5r_lts2_elevation$Bike, ENMAC10 * routes_r5r_lts2_elevation$Total - routes_r5r_lts2_elevation$Bike)
routes_r5r_lts2_elevation$new_car4 = ifelse(routes_r5r_lts2_elevation$Bikeper >= ENMAC4, routes_r5r_lts2_elevation$Car + routes_r5r_lts2_elevation$CarP, (routes_r5r_lts2_elevation$Car + routes_r5r_lts2_elevation$CarP) - routes_r5r_lts2_elevation$new_cyc4)
routes_r5r_lts2_elevation$new_car10 = ifelse(routes_r5r_lts2_elevation$Bikeper >= ENMAC10, routes_r5r_lts2_elevation$Car + routes_r5r_lts2_elevation$CarP, (routes_r5r_lts2_elevation$Car + routes_r5r_lts2_elevation$CarP) - routes_r5r_lts2_elevation$new_cyc10)
routes_r5r_lts2_elevation$Bike_4_total = routes_r5r_lts2_elevation$Bike + routes_r5r_lts2_elevation$new_cyc4
routes_r5r_lts2_elevation$Bike_10_total = routes_r5r_lts2_elevation$Bike + routes_r5r_lts2_elevation$new_cyc10

sum(routes_r5r_lts2_elevation$new_cyc4) / sum(routes_r5r_lts2_elevation$Total) #3.58%

routes_r5r_lts2_elevation = sf::st_as_sf(
  as.data.frame(sf::st_drop_geometry(routes_r5r_lts2_elevation)),
  geometry = routes_r5r_lts2_elevation$geometry
)

#rnet with LTS
rnet_r5r_lts2_raw = overline( #this takes too much memory - try to reduce info first! 
  routes_r5r_lts2_elevation,
  attrib = c("Bike", "Bike_4_total", "Bike_10_total"),
  fun = c("mean", "sum"),
  regionalise = 9e99 #it was failing - see issue https://github.com/ropensci/stplanr/issues/466
)


names(rnet_r5r_lts2_raw) 
rnet_r5r_lts2_raw = rnet_r5r_lts2_raw %>% 
  transmute(Baseline = Bike_sum, ENMAC4 = Bike_4_total_sum, ENMAC10 = Bike_10_total_sum) %>% #Quietness = bicycle_lts_mean, 
  mutate_if(is.numeric, round) 
nrow(rnet_r5r_lts2_raw) # 51390

saveRDS(rnet_r5r_lts2_raw, "rnet_r5r_lts2_raw.Rds")


#to get the LTS level for each segment
network_r5r_elevation = r5r::street_network_to_sf(r5r_lts_elevation) #forgot to save r5r_lts_elevation!!!!
routes_r5r_lts2_elevation_lts = st_join(routes_r5r_lts2_elevation, network_r5r_elevation$edges)

rnet_r5r_lts2_raw_lts = st_join(rnet_r5r_lts2_raw, r5r_network$edges) #it also works witthout ferry and elevation?

table(rnet_r5r_lts2_raw_lts$bicycle_lts)
# 1      2      3      4 
# 507842  23420 105428 189958 

saveRDS(rnet_r5r_lts2_raw_lts, "rnet_jittered_500_r5rlts2_levels_ENMAC410.Rds") #826k rows, info per segment of osm


#plot
library(biclar)
mun = MUNICIPIOS[6] #Lisboa
BUFFER = MUNICIPIOSgeo %>% filter(Concelho == mun) %>% st_buffer(500) #maybe reduce to 200? 
REDE = rnet_r5r_lts2_raw_lts %>% st_filter(BUFFER)

REDE = REDE %>% filter(Baseline >= quantile(REDE$Baseline, 0.60)) #0.6 for quiet, 0.7 for fast, change here scenario. (Barreiro should be 0.65)


#make map
tm_rnet_lts = function(
    rnet,
    palette = "-temperature_diverging",
    lwd_multiplier = 4,
    scale = 5,
    lwd = names(rnet)[1],
    col = names(rnet)[2],
    max_features = 10000,
    breaks = c(0, 1, 2, 3, 4),
    labels = c("1", "2", "3", "4"),
    title.col = "Level of Traffic Stress"
) {
  rnet_names = names(rnet)
  rnet_names = rnet_names[-grep(pattern = "geom|id", x = rnet_names)]
  lwd_scaled = rnet[[lwd]] - min(rnet[[lwd]])
  lwd_scaled = lwd_scaled / max(lwd_scaled)
  lwd_scaled = lwd_scaled * (lwd_multiplier - 1)
  lwd_scaled = lwd_scaled + 1
  rnet$lwd = lwd_scaled
  pal = cols4all::c4a(palette = palette, n = length(breaks) - 1)
  names(pal) = labels
  rnet$cols = cut(rnet[[col]], breaks = breaks, labels = labels)
  
  m = tmap::tm_shape(rnet) +
    tmap::tm_lines(id = NULL,
                   lwd = "lwd",
                   scale = scale,
                   popup.vars = rnet_names,
                   col = "cols",
                   palette = pal,
                   title.col = title.col
    )
  # } else {
  #   # Note: not working
  # m = tmap::tm_shape(rnet) +
  #   tmap::tm_lines(lwd = "lwd", lwd.scale = 5,
  #                  col.scale = palette)
  # tmap::tmap_mode("view")
  # }
  tmap::tmap_leaflet(m)
}


m = tm_rnet_lts(
  REDE,
  lwd = "Baseline", #Baseline, ENMAC4, ENMAC10
  col = "bicycle_lts",
  palette = "mako", # "mako", "linear_yl_rd_bk" - linear_yl_rd_bk for fastest, mako for quietest (tried reds, rocket, burg)
  scale = 15,
  lwd_multiplier = 15 #15 used in region and fast, 12 for quiet
)

m

htmlwidgets::saveWidget(m, paste0("pkgdown/assets/lisbon_baseline_lts2.html")) 






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