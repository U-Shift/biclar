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
sum(od_jittered_filter$Bike) / sum(od_jittered_test$Bike) #91.68% with jittered_100

od_jittered_filter$id = 1:nrow(od_jittered_filter)

plot(od_jittered_filter$geometry, lwd = 0.1)

## mvp - robin
routes_jittered_test = route(
  l = od_jittered_filter %>% sample_n(10),
  route_fun = journey,
  plan = "quietest"
)

routes_jittered_quietest = route(
  l = od_jittered_filter,
  route_fun = journey,
  plan = "quietest"
)

routes_jittered_fastest = route(
  l = od_jittered_filter,
  route_fun = journey,
  plan = "fastest"
)

saveRDS(routes_jittered_fastest, "routes_jittered_fastest_threshold_500_max_9km_total_max_total_10.Rds")
saveRDS(routes_jittered_quietest, "routes_jittered_quietest_threshold_500_max_9km_total_max_total_10.Rds")
system("gh release list")
system("gh release upload 0.0.1 routes_jittered_quietest_threshold_500_max_9km_total_max_total_10.Rds")
system("gh release upload 0.0.1 routes_jittered_fastest_threshold_500_max_9km_total_max_total_10.Rds")
gh_release_upload = function(file, tag = "0.0.1") {
  msg = glue::glue("gh release upload {tag} {file}")
  message("Running this command:\n", msg)
  system(msg)
}


## end robin

st_write(od_jittered_filter, "od_jittered_filter.geojson")
  
samplejitter1 = od_jittered_filter %>% slice_head(n= 10000) #maximum allowed - job id 425 quiestest / 430 fastest / 438 fastest AND quietest, both diections
samplejitter2 = od_jittered_filter %>% filter(id == c(10001:20000)) # job id 426 quiestest / 431 fastest / 436 fastest AND quietest, both diections
samplejitter3 = od_jittered_filter %>% filter(id == c(20001:30000)) # job id 427 quiestest / 432 fastest / 437 fastest AND quietest, both diections
samplejitter4 = od_jittered_filter %>% filter(id == c(30001:40000)) # job id 428 quiestest / 433 fastest / 439 fastest AND quietest, both diections
samplejitter5 = od_jittered_filter %>% filter(id == c(40001:50000)) # job id 429 quiestest / 434 fastest / 440 fastest AND quietest, both diections


#run one by one, and open the result at https://www.cyclestreets.net/journey/batch/423/ #id here
routes_jittered = batch(samplejitter5,
                        name = "biclar_filtered_sample",
                        strategies = c("fastest", "quietest"), #or quietest
                        minDistance = 200,
                        maxDistance = 10000, #change here the assumed max euclidean distance (default = 5000)
                        bothDirections = 1,
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
)


# Baseline route network --------------------------------------------------

# # For the baseline
# rnet_baseline_raw = overline(routes_jittered_quietest, attrib = c("Bike", "quietness"), fun = c(mean, sum))
# rnet_baseline = rnet_baseline_raw %>% 
#   transmute(Quietness = quietness_fn1, Baseline = round(Bike_fn2)) %>% 
#   filter(Baseline > 10)
# nrow(rnet_baseline) # 14k
# write_rds(rnet_baseline, "rnet_quietest_threshold_500_max_9km_total_max_total_10.Rds")

# Generate ENMAC scenario rnet - see ad_scenarioENMAC_rosa.R file --------
# file.edit("code/test-code/add_scenarioENMAC_rosa.R")
# routes_jittered_500_ENMAC410 = readRDS("routes_jittered_quietest500_ENMAC410.Rds")
routes_jittered_500_ENMAC410 = readRDS("routes_jittered_ENMAC410_fastest_500.Rds")
rnet_enmac_raw = overline(
  routes_jittered_500_ENMAC410,
  attrib = c("Bike", "quietness", "new_cyc4", "new_cyc10"),
  fun = c("mean", "sum")
  )
names(rnet_enmac_raw)
rnet_enmac_full = rnet_enmac_raw %>% 
  transmute(Quietness = quietness_mean, Baseline = Bike_sum, ENMAC4 = new_cyc4_sum, ENMAC10 = new_cyc10_sum) %>%
  mutate_if(is.numeric, round) 
nrow(rnet_enmac_full) # 59 k
sum(routes_jittered_500_ENMAC410$new_cyc4) / sum(routes_jittered_500_ENMAC410$Total)
write_rds(rnet_enmac_full, "rnet_enmac_fastest_full.Rds")
# write_rds(rnet_enmac_full, "rnet_enmac_full.Rds")
# rnet_enmac_full = readRDS("rnet_enmac_full.Rds")
rnet_enmac_region = rnet_enmac_full %>% 
  slice_max(order_by = ENMAC10, n = 20000)
# write_rds(rnet_enmac_region, "rnet_enmac_region_fastest_top_20000.Rds")

# Visualise the results ------
rnet_enmac_region = readRDS("rnet_enmac_region_top_20000.Rds")
# rnet_enmac_region = readRDS("rnet_enmac_region_fastest_top_20000.Rds")
m = tm_rnet(rnet_enmac_region %>% slice_max(ENMAC4, n = 1000), lwd = "ENMAC10", col = "Quietness", palette = "johnson")
m = tm_rnet(rnet_enmac_region, lwd = "ENMAC10", col = "Quietness", palette = "-mako", scale = 30)
# htmlwidgets::saveWidget(m, "m_rnet_enmac_region_fastest_top_20000.html")
htmlwidgets::saveWidget(m, "pkgdown/assets/m_rnet_enmac_region_fastest.html")
gh_release_upload("m_rnet_enmac_region_fastest_top_20000.html")
gh_release_upload("m_rnet_enmac_region_quietest_top_20000.html")


# Create results for one municipality
head(zones)

# try in cyclestreets online tool -------------------------------------------------------------

st_write(od_jittered_test, "od_biclar_10.geojson", delete_dsn = TRUE)
st_write(od_jittered_filter, "od_jittered_filter.json")
# routes_jittered_online = read.csv("lisbon_test-data.csv")
routes_jittered_online_sf1 = cyclestreets:::batch_read("biclar_filtered-data1.gz")
# routes_jittered_online = json2sf_cs(routes_jittered_online)

