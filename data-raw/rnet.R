## code to prepare `rnet` dataset goes here
library(tidyverse)
# rnet = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/rnet_quietest_threshold_500_max_9km_total_max_total_10.Rds"))
rnet = readRDS("rnet_enmac_region_top_10000.Rds")
rnet = rnet %>% 
 filter(Baseline > 10)
central_lisbon = zonebuilder::zb_zone(x = "Lisbon", n_circles = 1)
rnet_lisbon = rnet[central_lisbon, , op = sf::st_within]
summary(rnet_lisbon)
plot(rnet_lisbon)
usethis::use_data(rnet_lisbon, overwrite = TRUE)
