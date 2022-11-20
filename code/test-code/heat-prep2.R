#### CAR ONLY TO COMPARE ESTIMATES ####

library(dplyr)
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



# População movel AML = 80.4%
# Deslocacoes / pessoa / dia = 2.6 AML



# 1.3

HEAT13_bike = routes_r5r_100jit_lts3__ferry_elev %>%
  filter(mode == "BICYCLE") %>% #the difference is here. this network does not consider ferry lines
  st_drop_geometry() %>%
  group_by(id) %>% 
  summarise(distance = sum(distance), 
            Bike = Bike,
            Car = Car + CarP,
            Total = Total,
            Bikeper = Bike / Total) %>% 
  ungroup() %>% 
  filter(distance <=5000) %>%
  mutate(
    cyc4 = ifelse(Bikeper >= ENMAC4, Bike, ENMAC4 * Total),
    new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
    cyc10 = ifelse(Bikeper >= ENMAC10, Bike, ENMAC10 * Total),
    new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)
  )

sum(HEAT13_bike$Bike) #12k
sum(HEAT13_bike$Total) #2317371
sum(HEAT13_bike$new_cyc4) #81671
sum(HEAT13_bike$new_cyc10) #219928
weighted.mean(HEAT13_bike$distance, w= HEAT13_bike$Bike) # 2.703 km
weighted.mean(HEAT13_bike$distance, w= HEAT13_bike$cyc4) # 2.676 km
weighted.mean(HEAT13_bike$distance, w= HEAT13_bike$cyc10) # 2.674 km

sum(HEAT13_bike$Bike)/sum(HEAT13_bike$Total) # 0.52
sum(HEAT13_bike$cyc4)/sum(HEAT13_bike$Total) # 4.04
sum(HEAT13_bike$cyc10)/sum(HEAT13_bike$Total) # 10.01
sum(HEAT13_bike$Car)/sum(HEAT13_bike$Total) # 46.34
(sum(HEAT13_bike$Car)-sum(HEAT13_bike$new_cyc4))/sum(HEAT13_bike$Total) # 42.81
(sum(HEAT13_bike$Car)-sum(HEAT13_bike$new_cyc10))/sum(HEAT13_bike$Total) # 36.85


summary(routes_r5r_100jit_lts3__ferry_elev$eucl_distance) #max = 20143
routes_car_filtered = routes_r5r_100jit_car %>%
  filter(distance <= 20100) %>% #euclidean distance of routes_ferry3_filtered
  mutate(Bikeper = Bike / Total,
         Car = Car + CarP) %>% 
  mutate(
    cyc4 = ifelse(Bikeper >= ENMAC4, Bike, ENMAC4 * Total),
    new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
    cyc10 = ifelse(Bikeper >= ENMAC10, Bike, ENMAC10 * Total),
    new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)) %>% 
  mutate(car4 = Car + CarP - new_cyc4,
        car10 = Car + CarP - new_cyc10)
weighted.mean(routes_car_filtered$distance, w= routes_car_filtered$Car) #7404
weighted.mean(routes_car_filtered$distance, w= routes_car_filtered$car4) #7393
weighted.mean(routes_car_filtered$distance, w= routes_car_filtered$car10) #7474


rnet_joined = readRDS("export2/rnet_ferry3_overline_morethan200_clean_tags.Rds")
summary(rnet_joined$carspeed)
a = rnet_joined %>% filter(carspeed > 50)
round(sum(a$Bike) / sum(rnet_joined$Bike), 2) #2% stress roads



# 1.4

HEAT14_bike = routes_r5r_100jit_lts4__ferry_elev %>%
  filter(mode == "BICYCLE") %>%
  st_drop_geometry() %>%
  group_by(id) %>% 
  summarise(distance = sum(distance), 
            Bike = Bike,
            Car = Car + CarP,
            Total = Total,
            Bikeper = Bike / Total) %>% 
  ungroup() %>% 
  filter(distance <=5000) %>%
  mutate(
    cyc4 = ifelse(Bikeper >= ENMAC4, Bike, ENMAC4 * Total),
    new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
    cyc10 = ifelse(Bikeper >= ENMAC10, Bike, ENMAC10 * Total),
    new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)
  )

# sum(rnet_ferry4_overline_morethan200_clean$Bike)
sum(HEAT14_bike$Bike) #13k
sum(HEAT14_bike$Total) #2521688
sum(HEAT14_bike$new_cyc4) #88824
sum(HEAT14_bike$new_cyc10) #239245
weighted.mean(HEAT14_bike$distance, w= HEAT14_bike$Bike) # 2.709 km
weighted.mean(HEAT14_bike$distance, w= HEAT14_bike$cyc4) # 2.669 km
weighted.mean(HEAT14_bike$distance, w= HEAT14_bike$cyc10) # 2.667 km

sum(HEAT14_bike$Bike)/sum(HEAT14_bike$Total) # 0.52
sum(HEAT14_bike$cyc4)/sum(HEAT14_bike$Total) # 4.04
sum(HEAT14_bike$cyc10)/sum(HEAT14_bike$Total) # 10.01
sum(HEAT14_bike$Car)/sum(HEAT14_bike$Total) # 47.38
(sum(HEAT14_bike$Car)-sum(HEAT14_bike$new_cyc4))/sum(HEAT14_bike$Total) # 43.86
(sum(HEAT14_bike$Car)-sum(HEAT14_bike$new_cyc10))/sum(HEAT14_bike$Total) # 37.89


summary(routes_r5r_100jit_lts4__ferry_elev$eucl_distance) #max = 26246
routes_car_filtered = routes_r5r_100jit_car %>%
  filter(distance <= 26200) %>% #euclidean distance of routes_ferry4_filtered
  mutate(Bikeper = Bike / Total,
         Car = Car + CarP) %>% 
  mutate(
    cyc4 = ifelse(Bikeper >= ENMAC4, Bike, ENMAC4 * Total),
    new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
    cyc10 = ifelse(Bikeper >= ENMAC10, Bike, ENMAC10 * Total),
    new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)) %>% 
  mutate(car4 = Car + CarP - new_cyc4,
         car10 = Car + CarP - new_cyc10)
weighted.mean(routes_car_filtered$distance, w= routes_car_filtered$Car) #8463
weighted.mean(routes_car_filtered$distance, w= routes_car_filtered$car4) #8441
weighted.mean(routes_car_filtered$distance, w= routes_car_filtered$car10) #8536

rnet_joined = readRDS("export2/rnet_ferry4_overline_morethan200_clean_tags.Rds")
summary(rnet_joined$carspeed)
a = rnet_joined %>% filter(carspeed > 50)
round(sum(a$Bike) / sum(rnet_joined$Bike), 2) #38% stress roads





# 2.3

HEAT23_bike = routes_r5r_100jit_lts3__ferry_elev %>%
  filter(mode == "BICYCLE") %>% #the difference is here. this network does not consider ferry lines
  st_drop_geometry() %>%
  group_by(id) %>% 
  summarise(distance = sum(distance), 
            Bike = Bike,
            Car = Car + CarP,
            Total = Total,
            Bikeper = Bike / Total) %>% 
  ungroup() %>% 
  filter(distance <=10000) %>%
  mutate(
    cyc4 = ifelse(Bikeper >= ENMAC4, Bike, ENMAC4 * Total),
    new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
    cyc10 = ifelse(Bikeper >= ENMAC10, Bike, ENMAC10 * Total),
    new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)
  )

sum(HEAT23_bike$Bike) #19643k
sum(HEAT23_bike$Total) #3708354
sum(HEAT23_bike$new_cyc4) #131697
sum(HEAT23_bike$new_cyc10) #352441
weighted.mean(HEAT23_bike$distance, w= HEAT23_bike$Bike) # 4.529
weighted.mean(HEAT23_bike$distance, w= HEAT23_bike$cyc4) #  4.424
weighted.mean(HEAT23_bike$distance, w= HEAT23_bike$cyc10) # 4.394

sum(HEAT23_bike$Bike)/sum(HEAT23_bike$Total) # 0.53
sum(HEAT23_bike$cyc4)/sum(HEAT23_bike$Total) # 4.08
sum(HEAT23_bike$cyc10)/sum(HEAT23_bike$Total) # 10.03
sum(HEAT23_bike$Car)/sum(HEAT23_bike$Total) # 52.21
(sum(HEAT23_bike$Car)-sum(HEAT23_bike$new_cyc4))/sum(HEAT23_bike$Total) # 48.66
(sum(HEAT23_bike$Car)-sum(HEAT23_bike$new_cyc10))/sum(HEAT23_bike$Total) # 42.71


summary(routes_r5r_100jit_lts3__ferry_elev$eucl_distance) #max = 20143
routes_car_filtered = routes_r5r_100jit_car %>%
  filter(distance <= 20100) %>% #euclidean distance of routes_ferry3_filtered
  mutate(Bikeper = Bike / Total,
         Car = Car + CarP) %>% 
  mutate(
    cyc4 = ifelse(Bikeper >= ENMAC4, Bike, ENMAC4 * Total),
    new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
    cyc10 = ifelse(Bikeper >= ENMAC10, Bike, ENMAC10 * Total),
    new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)) %>% 
  mutate(car4 = Car + CarP - new_cyc4,
         car10 = Car + CarP - new_cyc10)
weighted.mean(routes_car_filtered$distance, w= routes_car_filtered$Car) # 7.404
weighted.mean(routes_car_filtered$distance, w= routes_car_filtered$car4) # 7.393
weighted.mean(routes_car_filtered$distance, w= routes_car_filtered$car10) # 7.474


rnet_joined = readRDS("export2/rnet_ferry3_ebike_overline_morethan50_clean_tags.Rds")
summary(rnet_joined$carspeed)
a = rnet_joined %>% filter(carspeed > 30)
round(sum(a$Bike) / sum(rnet_joined$Bike), 2) #2% stress roads



# 2.4

HEAT24_bike = routes_r5r_100jit_lts4__ferry_elev %>%
  filter(mode == "BICYCLE") %>% #the difference is here. this network does not consider ferry lines
  st_drop_geometry() %>%
  group_by(id) %>% 
  summarise(distance = sum(distance), 
            Bike = Bike,
            Car = Car + CarP,
            Total = Total,
            Bikeper = Bike / Total) %>% 
  ungroup() %>% 
  filter(distance <=10000) %>%
  mutate(
    cyc4 = ifelse(Bikeper >= ENMAC4, Bike, ENMAC4 * Total),
    new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
    cyc10 = ifelse(Bikeper >= ENMAC10, Bike, ENMAC10 * Total),
    new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)
  )

sum(HEAT24_bike$Bike) #21630
sum(HEAT24_bike$Total) #3975986
sum(HEAT24_bike$new_cyc4) #141446
sum(HEAT24_bike$new_cyc10) #377903
weighted.mean(HEAT24_bike$distance, w= HEAT24_bike$Bike) # 4586
weighted.mean(HEAT24_bike$distance, w= HEAT24_bike$cyc4) #  4390
weighted.mean(HEAT24_bike$distance, w= HEAT24_bike$cyc10) # 4346

sum(HEAT24_bike$Bike)/sum(HEAT24_bike$Total) # 0.54
sum(HEAT24_bike$cyc4)/sum(HEAT24_bike$Total) # 4.10
sum(HEAT24_bike$cyc10)/sum(HEAT24_bike$Total) # 10.05
sum(HEAT24_bike$Car)/sum(HEAT24_bike$Total) # 53.08
(sum(HEAT24_bike$Car)-sum(HEAT24_bike$new_cyc4))/sum(HEAT24_bike$Total) # 49.53
(sum(HEAT24_bike$Car)-sum(HEAT24_bike$new_cyc10))/sum(HEAT24_bike$Total) # 43.58


summary(routes_r5r_100jit_lts4__ferry_elev$eucl_distance) #max = 26246
routes_car_filtered = routes_r5r_100jit_car %>%
  filter(distance <= 26200) %>% #euclidean distance of routes_ferry3_filtered
  mutate(Bikeper = Bike / Total,
         Car = Car + CarP) %>% 
  mutate(
    cyc4 = ifelse(Bikeper >= ENMAC4, Bike, ENMAC4 * Total),
    new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
    cyc10 = ifelse(Bikeper >= ENMAC10, Bike, ENMAC10 * Total),
    new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)) %>% 
  mutate(car4 = Car + CarP - new_cyc4,
         car10 = Car + CarP - new_cyc10)
weighted.mean(routes_car_filtered$distance, w= routes_car_filtered$Car) # 8.463
weighted.mean(routes_car_filtered$distance, w= routes_car_filtered$car4) # 8.441
weighted.mean(routes_car_filtered$distance, w= routes_car_filtered$car10) # 8.536


rnet_joined = readRDS("export2/rnet_ferry4_ebike_overline_morethan50_clean_tags.Rds")
summary(rnet_joined$carspeed)
a = rnet_joined %>% filter(carspeed > 50)
round(sum(a$Bike) / sum(rnet_joined$Bike), 2) #2% stress roads
