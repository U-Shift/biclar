# Compute the routing for jittered od pairs, for fast and quiet routes

# devtools::install_github("cyclestreets/cyclestreets-r") #update for batch routing function
# see: https://rpackage.cyclestreets.net/reference/batch.html

library(dplyr)
library(sf)
library(cyclestreets)

## with a threshold of 100 jittered trips, resulting 57356 od pairs

od_jittered_100 = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/od_all_jittered_100.Rds"))

od_jittered_test = od_jittered_100 %>% slice_sample(n = 100)
plot(od_jittered_test$geometry, lwd = 0.1)

routes_jittered = batch(od_jittered_test,
                        name = "biclar test",
                        strategies = "fastest", #or quietest
                        minDistance = 200,
                        maxDistance = 10000, #change here the assumed max euclidean distance
                        filename = "biclar_test",
                        includeJsonOutput = 1, #0 - only summary info like time and dist
                        emailOnCompletion = "temospena@gmail.com",
                        username = "temospena",
                        password = Sys.getenv("CYCLESTREETS_PW"), #pw for CS account
                        base_url = "https://api.cyclestreets.net/v2/batchroutes.createjob",
                        id = "start",
                        pat = Sys.getenv("CYCLESTREETS") #API key from CS for this project
)

plot(routes_jittered$geometry, lwd = 0.1)                       
