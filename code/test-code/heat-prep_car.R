#### CAR ONLY TO COMPARE ESTIMATES ####

library(dplyr)
library(sf)
options(java.parameters = '-Xmx40G') 
library(r5r)
r5r_lts_intermodalityALL = setup_r5(data_path = "r5r/allmodes2/", elevation = "MINETTI") #to create new, delete network.dat in the folder. otherwise just load it
departure_datetime = lubridate::force_tz(departure_datetime, "WET") #make it western european time



routes_r5r_100jit_car = detailed_itineraries(
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
  drop_geometry = TRUE,
  output_dir = NULL
)


routes_r5r_100jit_car = routes_r5r_100jit_car %>% mutate(id = as.integer(from_id)) %>%
  select(id, total_duration, total_distance, segment, mode, segment_duration, distance, route) %>%
  left_join(od_jittered_100filter %>% st_drop_geometry(), by="id")

names(routes_r5r_100jit_car)[7] = "distance"
names(routes_r5r_100jit_car)[18] = "eucl_distance"

saveRDS(routes_r5r_100jit_car, "routes_r5r_100jit_car_raw.Rds")

summary(routes_r5r_100jit_car$total_distance)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 15    3316    6748   10734   13805  104572 
#doesnt make sence for the reality

# stop_r5()




# For Car + TP --------------------------------------------------------------------------------

# LTS4

OD_13legs_lts4 = readRDS("routes_allmodesNSub4_preoverline.Rds") 
OD_13legs_lts4 = OD_13legs_lts4 %>% 
  mutate(id_route = id,
         segment_route = segment) %>% 
  mutate(id = 1:nrow(OD_13legs_lts4)) %>% 
  select(-c(2:8)) %>% 
  mutate(origins = lwgeom::st_startpoint(OD_13legs_lts4$geometry),
         destinations = lwgeom::st_endpoint(OD_13legs_lts4$geometry)) %>% 
  st_drop_geometry()
OD_13legs_lts4_or = OD_13legs_lts4 %>%
  select(id, origins) %>%
  st_as_sf() %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>% 
  st_drop_geometry()
OD_13legs_lts4_de = OD_13legs_lts4 %>%
  select(id, destinations) %>%
  st_as_sf() %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>% 
  st_drop_geometry()


routes_r5r_100jit_car_TP_lts4 = detailed_itineraries(
  r5r_lts_intermodalityALL,
  origins = OD_13legs_lts4_or,
  destinations = OD_13legs_lts4_de,
  mode = "CAR", # car
  mode_egress = "CAR",
  departure_datetime = departure_datetime, 
  max_trip_duration = 120L, #in minutes
  shortest_path = TRUE, #FALSE? fastest or multiple alternatives?
  all_to_all = FALSE,
  n_threads = Inf,
  verbose = FALSE,
  progress = TRUE,
  drop_geometry = TRUE, #not needed
  output_dir = NULL
)


routes_r5r_100jit_car_TP_lts4 = routes_r5r_100jit_car_TP_lts4 %>% mutate(id = as.integer(from_id)) %>%
  select(id, mode, segment_duration, distance) %>%
  left_join(OD_13legs_lts4, by="id") %>% 
  select(-c("origins", "destinations")) %>% 
  mutate(id = id_route,
         segment = segment_route) %>% 
  select(-c("id_route", "segment_route"))

saveRDS(routes_r5r_100jit_car_TP_lts4, "routes_r5r_100jit_car_TP_lts4_raw.Rds")

summary(routes_r5r_100jit_car_TP_lts4$distance)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 81    1620    2328    2509    3221   13384 




# LTS3

OD_13legs_lts3 = readRDS("routes_allmodesNSub3_preoverline.Rds") 
OD_13legs_lts3 = OD_13legs_lts3 %>% 
  mutate(id_route = id,
         segment_route = segment) %>% 
  mutate(id = 1:nrow(OD_13legs_lts3)) %>% 
  select(-c(2:8)) %>% 
  mutate(origins = lwgeom::st_startpoint(OD_13legs_lts3$geometry),
         destinations = lwgeom::st_endpoint(OD_13legs_lts3$geometry)) %>% 
  st_drop_geometry()
OD_13legs_lts3_or = OD_13legs_lts3 %>%
  select(id, origins) %>%
  st_as_sf() %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>% 
  st_drop_geometry()
OD_13legs_lts3_de = OD_13legs_lts3 %>%
  select(id, destinations) %>%
  st_as_sf() %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>% 
  st_drop_geometry()


routes_r5r_100jit_car_TP_lts3 = detailed_itineraries(
  r5r_lts_intermodalityALL,
  origins = OD_13legs_lts3_or,
  destinations = OD_13legs_lts3_de,
  mode = "CAR", # car
  mode_egress = "CAR",
  departure_datetime = departure_datetime, 
  max_trip_duration = 120L, #in minutes
  shortest_path = TRUE, #FALSE? fastest or multiple alternatives?
  all_to_all = FALSE,
  n_threads = Inf,
  verbose = FALSE,
  progress = TRUE,
  drop_geometry = TRUE, #not needed
  output_dir = NULL
)


routes_r5r_100jit_car_TP_lts3 = routes_r5r_100jit_car_TP_lts3 %>% mutate(id = as.integer(from_id)) %>%
  select(id, mode, segment_duration, distance) %>%
  left_join(OD_13legs_lts3, by="id") %>% 
  select(-c("origins", "destinations")) %>% 
  mutate(id = id_route,
         segment = segment_route) %>% 
  select(-c("id_route", "segment_route"))

saveRDS(routes_r5r_100jit_car_TP_lts3, "routes_r5r_100jit_car_TP_lts3_raw.Rds")

summary(routes_r5r_100jit_car_TP_lts3$distance)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 81    1443    2102    2304    2973   11798 

# stop_r5()



# only tp segment -----------------------------------------------------------------------------

#LTS4
OD_2legs_lts4 = readRDS("routes_allmodesNSub4_TPonly_preoverline.Rds") 
OD_2legs_lts4 = OD_2legs_lts4 %>% 
  mutate(id_route = id,
         segment_route = segment,
         mode_TP = mode) %>% 
  mutate(id = 1:nrow(OD_2legs_lts4)) %>% 
  select(-c(2:8)) %>% 
  mutate(origins = lwgeom::st_startpoint(OD_2legs_lts4$geometry),
         destinations = lwgeom::st_endpoint(OD_2legs_lts4$geometry)) %>% 
  st_drop_geometry()
OD_2legs_lts4_or = OD_2legs_lts4 %>%
  select(id, origins) %>%
  st_as_sf() %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>% 
  st_drop_geometry()
OD_2legs_lts4_de = OD_2legs_lts4 %>%
  select(id, destinations) %>%
  st_as_sf() %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>% 
  st_drop_geometry()


routes_r5r_100jit_car_TP_lts4_ONLYTPsegment = detailed_itineraries(
  r5r_lts_intermodalityALL,
  origins = OD_2legs_lts4_or,
  destinations = OD_2legs_lts4_de,
  mode = "CAR", # car
  mode_egress = "CAR",
  departure_datetime = departure_datetime, 
  max_trip_duration = 120L, #in minutes
  shortest_path = TRUE, #FALSE? fastest or multiple alternatives?
  all_to_all = FALSE,
  n_threads = Inf,
  verbose = FALSE,
  progress = TRUE,
  drop_geometry = TRUE, #not needed
  output_dir = NULL
)


routes_r5r_100jit_car_TP_lts4_ONLYTPsegment = routes_r5r_100jit_car_TP_lts4_ONLYTPsegment %>%
  mutate(id = as.integer(from_id)) %>%
  select(id, mode, segment_duration, distance) %>%
  left_join(OD_2legs_lts4, by="id") %>% 
  select(-c("origins", "destinations")) %>% 
  mutate(id = id_route,
         segment = segment_route) %>% 
  select(-c("id_route", "segment_route"))

saveRDS(routes_r5r_100jit_car_TP_lts4_ONLYTPsegment, "routes_r5r_100jit_car_TP_lts4_ONLYTPsegment_raw.Rds")

summary(routes_r5r_100jit_car_TP_lts4_ONLYTPsegment$distance)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 29    6169    9497   11623   15370   59072 


#LTS 3
OD_2legs_lts3 = readRDS("routes_allmodesNSub3_TPonly_preoverline.Rds") 
OD_2legs_lts3 = OD_2legs_lts3 %>% 
  mutate(id_route = id,
         segment_route = segment,
         mode_TP = mode) %>% 
  mutate(id = 1:nrow(OD_2legs_lts3)) %>% 
  select(-c(2:8)) %>% 
  mutate(origins = lwgeom::st_startpoint(OD_2legs_lts3$geometry),
         destinations = lwgeom::st_endpoint(OD_2legs_lts3$geometry)) %>% 
  st_drop_geometry()
OD_2legs_lts3_or = OD_2legs_lts3 %>%
  select(id, origins) %>%
  st_as_sf() %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>% 
  st_drop_geometry()
OD_2legs_lts3_de = OD_2legs_lts3 %>%
  select(id, destinations) %>%
  st_as_sf() %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>% 
  st_drop_geometry()


routes_r5r_100jit_car_TP_lts3_ONLYTPsegment = detailed_itineraries(
  r5r_lts_intermodalityALL,
  origins = OD_2legs_lts3_or,
  destinations = OD_2legs_lts3_de,
  mode = "CAR", # car
  mode_egress = "CAR",
  departure_datetime = departure_datetime, 
  max_trip_duration = 120L, #in minutes
  shortest_path = TRUE, #FALSE? fastest or multiple alternatives?
  all_to_all = FALSE,
  n_threads = Inf,
  verbose = FALSE,
  progress = TRUE,
  drop_geometry = TRUE, #not needed
  output_dir = NULL
)


routes_r5r_100jit_car_TP_lts3_ONLYTPsegment = routes_r5r_100jit_car_TP_lts3_ONLYTPsegment %>%
  mutate(id = as.integer(from_id)) %>%
  select(id, mode, segment_duration, distance) %>%
  left_join(OD_2legs_lts3, by="id") %>% 
  select(-c("origins", "destinations")) %>% 
  mutate(id = id_route,
         segment = segment_route) %>% 
  select(-c("id_route", "segment_route"))

saveRDS(routes_r5r_100jit_car_TP_lts3_ONLYTPsegment, "routes_r5r_100jit_car_TP_lts3_ONLYTPsegment_raw.Rds")

summary(routes_r5r_100jit_car_TP_lts3_ONLYTPsegment$distance)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 29    5286    8302   10651   14591   59072 

# stop_r5()



# organizar tabelas ---------------------------------------------------------------------------


# Com automovel de 1 a 3
routes_r5r_100jit_car = read("routes_r5r_100jit_car_raw.Rds")

# # LTS3
routes_allmodesNSub3_TPonly_preoverline = readRDS("routes_allmodesNSub3_TPonly_preoverline.Rds")
routes_r5r_100jit_car_TP_lts3_ONLYTPsegment = readRDS("routes_r5r_100jit_car_TP_lts3_ONLYTPsegment_raw.Rds")


CAR_TP_TRANSF_lts3 = routes_r5r_100jit_car_TP_lts3_ONLYTPsegment %>% mutate(LTS = 3) %>%
  rename(duration_car = segment_duration,
         distance_car = distance) %>%
  mutate(Car = Car + CarP) %>%
  select(-CarP) %>%
  left_join(routes_allmodesNSub3_TPonly_preoverline %>%
              select(id, segment_duration, distance) %>%
              st_drop_geometry()) %>%
  rename(duration_TP = segment_duration,
         distance_TP = distance,
         TP = Other,
         Bike_4 = cyc4,
         Bike_10 = cyc10,
         Bike_new4 = new_cyc4,
         Bike_new10 = new_cyc10) %>%
  mutate(Car_4 = Car - Bike_new4,
         Car_10 = Car - Bike_new10,
         TP_4 = TP + Bike_new4,
         TP_10 = TP + Bike_new10,) %>%
  left_join(routes_r5r_100jit_car %>% select(id, segment_duration, distance)) %>% 
  rename(duration_car_total = segment_duration,
         distance_car_total = distance) %>% 
  select(LTS, id, segment, DICOFREor11, DICOFREde11, Total, Car, Bike, Walk, TP, Bikeper,
         Bike_new4, Bike_new10, Bike_4, Bike_10, Car_4, Car_10, TP_4, TP_10,
         mode, duration_car, distance_car, duration_car_total, distance_car_total, mode_TP, duration_TP, distance_TP)


# LTS4
routes_allmodesNSub4_TPonly_preoverline = readRDS("routes_allmodesNSub4_TPonly_preoverline.Rds")
routes_r5r_100jit_car_TP_lts4_ONLYTPsegment = readRDS("routes_r5r_100jit_car_TP_lts4_ONLYTPsegment_raw.Rds")

CAR_TP_TRANSF_lts4 = routes_r5r_100jit_car_TP_lts4_ONLYTPsegment %>% mutate(LTS = 4) %>%
  rename(duration_car = segment_duration,
         distance_car = distance) %>%
  mutate(Car = Car + CarP) %>%
  select(-CarP) %>%
  left_join(routes_allmodesNSub4_TPonly_preoverline %>%
              select(id, segment_duration, distance) %>%
              st_drop_geometry()) %>%
  rename(duration_TP = segment_duration,
         distance_TP = distance,
         TP = Other,
         Bike_4 = cyc4,
         Bike_10 = cyc10,
         Bike_new4 = new_cyc4,
         Bike_new10 = new_cyc10) %>%
  mutate(Car_4 = Car - Bike_new4,
         Car_10 = Car - Bike_new10,
         TP_4 = TP + Bike_new4,
         TP_10 = TP + Bike_new10,) %>%
  left_join(routes_r5r_100jit_car %>% select(id, segment_duration, distance)) %>% 
  rename(duration_car_total = segment_duration,
         distance_car_total = distance) %>% 
  select(LTS, id, segment, DICOFREor11, DICOFREde11, Total, Car, Bike, Walk, TP, Bikeper,
         Bike_new4, Bike_new10, Bike_4, Bike_10, Car_4, Car_10, TP_4, TP_10,
         mode, duration_car, distance_car, duration_car_total, distance_car_total, mode_TP, duration_TP, distance_TP)

#Bind
CAR_TP_TRANSF = rbind(CAR_TP_TRANSF_lts3, CAR_TP_TRANSF_lts4)





# Com apenas segmento de automovel na viagem da perna 2
# # LTS3
# routes_allmodesNSub3_TPonly_preoverline = readRDS("routes_allmodesNSub3_TPonly_preoverline.Rds")
# 
# CAR_TP_TRANSF_lts3 = routes_r5r_100jit_car_TP_lts3_ONLYTPsegment %>% mutate(LTS = 3) %>%
#   rename(duration_car = segment_duration,
#          distance_car = distance) %>% 
#   mutate(Car = Car + CarP) %>% 
#   select(-CarP) %>% 
#   left_join(routes_allmodesNSub3_TPonly_preoverline %>%
#               select(id, segment_duration, distance) %>%
#               st_drop_geometry()) %>% 
#   rename(duration_TP = segment_duration,
#          distance_TP = distance,
#          TP = Other,
#          Bike_4 = cyc4,
#          Bike_10 = cyc10,
#          Bike_new4 = new_cyc4,
#          Bike_new10 = new_cyc10) %>% 
#   mutate(Car_4 = Car - Bike_new4,
#          Car_10 = Car - Bike_new10,
#          TP_4 = TP + Bike_new4,
#          TP_10 = TP + Bike_new10,) %>% 
#   select(LTS, id, segment, DICOFREor11, DICOFREde11, Total, Car, Bike, Walk, TP, Bikeper,
#          Bike_new4, Bike_new10, Bike_4, Bike_10, Car_4, Car_10, TP_4, TP_10,
#          mode, duration_car, distance_car, mode_TP, duration_TP, distance_TP)
#   
# routes_allmodesNSub3_TPonly_preoverline = readRDS("routes_allmodesNSub3_TPonly_preoverline.Rds")
# 
# 
# # LTS4
# routes_allmodesNSub4_TPonly_preoverline = readRDS("routes_allmodesNSub4_TPonly_preoverline.Rds")
# 
# CAR_TP_TRANSF_lts4 = routes_r5r_100jit_car_TP_lts4_ONLYTPsegment %>% mutate(LTS = 4) %>%
#   rename(duration_car = segment_duration,
#          distance_car = distance) %>% 
#   mutate(Car = Car + CarP) %>% 
#   select(-CarP) %>% 
#   left_join(routes_allmodesNSub4_TPonly_preoverline %>%
#               select(id, segment_duration, distance) %>%
#               st_drop_geometry()) %>% 
#   rename(duration_TP = segment_duration,
#          distance_TP = distance,
#          TP = Other,
#          Bike_4 = cyc4,
#          Bike_10 = cyc10,
#          Bike_new4 = new_cyc4,
#          Bike_new10 = new_cyc10) %>% 
#   mutate(Car_4 = Car - Bike_new4,
#          Car_10 = Car - Bike_new10,
#          TP_4 = TP + Bike_new4,
#          TP_10 = TP + Bike_new10,) %>% 
#   select(LTS, id, segment, DICOFREor11, DICOFREde11, Total, Car, Bike, Walk, TP, Bikeper,
#          Bike_new4, Bike_new10, Bike_4, Bike_10, Car_4, Car_10, TP_4, TP_10,
#          mode, duration_car, distance_car, mode_TP, duration_TP, distance_TP)
# 
# #Bind
# CAR_TP_TRANSF = rbind(CAR_TP_TRANSF_lts3, CAR_TP_TRANSF_lts4)
# 
# 


#Enviar Filipe
library(openxlsx)
write.xlsx(CAR_TP_TRANSF, "HEAT/CAR_TP_TRANSF.xlsx")
