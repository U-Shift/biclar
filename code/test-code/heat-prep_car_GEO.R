# same as heat-prep_car.R bur with linestrings from routes

library(dplyr)
library(sf)
options(java.parameters = '-Xmx110G') 
library(r5r)
r5r_lts_intermodalityALL = setup_r5(data_path = "r5r/allmodes2/", elevation = "MINETTI") #to create new, delete network.dat in the folder. otherwise just load it


routes_r5r_100jit_car_GEO = detailed_itineraries(
  r5r_lts_intermodalityALL,
  origins = od_jittered_100filter_OR,
  destinations = od_jittered_100filter_DE,
  mode = "CAR", #only car
  mode_egress = "WALK",
  departure_datetime = departure_datetime, 
  max_trip_duration = 120L, #in minutes
  shortest_path = TRUE, #FALSE? fastest or multiple alternatives?
  all_to_all = FALSE,
  n_threads = Inf,
  verbose = FALSE,
  progress = TRUE,
  drop_geometry = FALSE
  # output_dir = "r5r/resultstemp/"
)


routes_r5r_100jit_car_GEO = routes_r5r_100jit_car_GEO %>% mutate(id = as.integer(from_id)) %>%
  select(id, total_duration, total_distance, segment, mode, segment_duration, distance, geometry) %>%
  left_join(od_jittered_100filter %>% st_drop_geometry(), by="id")

names(routes_r5r_100jit_car_GEO)[7] = "distance"
names(routes_r5r_100jit_car_GEO)[18] = "eucl_distance"

saveRDS(routes_r5r_100jit_car_GEO, "routes_r5r_100jit_car_GEO.Rds")
