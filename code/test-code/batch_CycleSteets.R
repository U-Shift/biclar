# Compute the routing for jittered od pairs, for fast and quiet routes

# devtools::install_github("cyclestreets/cyclestreets-r") #update for batch routing function
# see: https://rpackage.cyclestreets.net/reference/batch.html

library(dplyr)
library(sf)
library(cyclestreets)
library(stplanr)
library(tmap)

## with a threshold of 100 jittered trips, resulting 57356 od pairs

# od_jittered_100 = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/od_all_jittered_100.Rds"))
# od_jittered_test = od_jittered_100
  # slice_sample(n = 10)
od_jittered_test = readRDS("od_all_jittered_500.Rds")

od_jittered_test$distance = as.numeric(st_length(od_jittered_test))
od_jittered_filter = od_jittered_test %>%
  filter(distance < 9000) %>%   #max 9km - 75%
  filter(Total > 10)

# Check what % of existing bike trips are captured
sum(od_jittered_filter$Bike) / sum(od_jittered_test$Bike)

od_jittered_filter$id = 1:nrow(od_jittered_filter)

plot(od_jittered_filter$geometry, lwd = 0.1)

## mvp - robin
routes_jittered_test = route(
  l = od_jittered_filter %>% sample_n(10),
  route_fun = journey,
  plan = "quietest"
)

plot(routes_jittered_test$geometry)                       

routes_jittered_quietest = route(
  l = od_jittered_filter,
  route_fun = journey,
  plan = "quietest"
)

saveRDS(routes_jittered_quietest, "routes_jittered_quietest_threshold_500_max_9km_total_max_total_10.Rds")

## end robin
  
samplejitter1 = od_jittered_filter %>% slice_head(n= 10000) #maximum allowed - job id 425 quiestest / 430 fastest
samplejitter2 = od_jittered_filter %>% filter(id == c(10001:20000)) # job id 426 quiestest / 431 fastest
samplejitter3 = od_jittered_filter %>% filter(id == c(20001:30000)) # job id 427 quiestest / 432 fastest
samplejitter4 = od_jittered_filter %>% filter(id == c(30001:40000)) # job id 428 quiestest / 433 fastest
samplejitter5 = od_jittered_filter %>% filter(id == c(40001:50000)) # job id 429 quiestest / 434 fastest


#run one by one, and open the result at https://www.cyclestreets.net/journey/batch/423/ #id here
routes_jittered = batch(samplejitter5,
                        name = "biclar_filtered_sample",
                        strategies = "fastest", #or quietest
                        minDistance = 200,
                        maxDistance = 10000, #change here the assumed max euclidean distance (default = 5000)
                        filename = "biclar_filtered",
                        includeJsonOutput = 1, #0 - only summary info like time and dist
                        # emailOnCompletion = "temospena@gmail.com",
                        username = "temospena",
                        password = Sys.getenv("CYCLESTREETS_PW"), #pw for CS account
                        # base_url = "https://api.cyclestreets.net/v2/batchroutes.createjob",
                        # id = 326, #number of job id 326
                        # id = "id",
                        pat = Sys.getenv("CYCLESTREETS"), #API key from CS for this project
                        serverId = 1


# Baseline route network --------------------------------------------------

rnet_baseline_raw = overline(routes_jittered_quietest, attrib = c("Bike", "quietness"), fun = c(mean, sum))
rnet_baseline = rnet_baseline_raw %>% 
  transmute(Quietness = quietness_fn1, Baseline = round(Bike_fn2)) %>% 
  filter(Baseline > 10)
nrow(rnet_baseline) # 14k
write_rds(rnet_baseline, "rnet_quietest_threshold_500_max_9km_total_max_total_10.Rds")

# Create results for one municipality
head(zones)


tm_shape(rnet_baseline) +
  tm_lines(lwd = "Baseline", col = "Quietness", palette = "-Reds", breaks = c(0, 50, 75, 100), scale = 5)


# try in cyclestreets online tool -------------------------------------------------------------

st_write(od_jittered_test, "od_biclar_10.geojson", delete_dsn = TRUE)
st_write(od_jittered_filter, "od_jittered_filter.json")
# routes_jittered_online = read.csv("lisbon_test-data.csv")
routes_jittered_online_sf1 = cyclestreets:::batch_read("biclar_filtered-data1.gz")
# routes_jittered_online = json2sf_cs(routes_jittered_online)

