# 
# rnet = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/rnet_quietest_threshold_500_max_9km_total_max_total_10.Rds"))
# zones_lisbon = zones %>% 
#   filter(Concelho == "LISBOA")
# rnet = rnet[zones_lisbon, ]
# # rnet = rnet %>% 
# #   filter(Baseline > 50)
# fs::file_size("data-raw/rnet_lisbon.geojson") # 931 kb
# central_lisbon = zonebuilder::zb_zone(x = "Lisbon", n_circles = 1)
# rnet = rnet[central_lisbon, , op = sf::st_within]
# sf::write_sf(rnet, "data-raw/rnet_lisbon.geojson")


tm_rnet = function(rnet, palette, min_width = 1, max_width = 9, lwd = names(rnet)[1], col = names(rnet)[1], max_features = 10000) {
  tmap::tm_shape(rnet) +
    tmap::tm_lines(id = NULL)
}

# tm_rnet(rnet)
