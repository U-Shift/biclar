# Aim: get quietness scores on route networks

library(tidyverse)
library(tmap)
tmap_mode("view")
local_crs = "EPSG:3857"

# Import data -------------------------------------------------------------

# transport_network = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/quietnessAML_alltags_fewer.Rds"))
transport_network = quietnessAML_alltags_fewer
names(transport_network)
summary(sf::st_geometry_type(transport_network))

# rnet = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/rnet_allmodesNSub4_bikeegress_overline_morethan100_clean.Rds"))
rnet = rnet_allmodesNSub2_overline_morethan100_clean #for instance
rnet$id = seq(nrow(rnet))

# Join --------------------------------------------------------------------

transport_network = sf::st_cast(sf::st_cast(transport_network, "MULTILINESTRING"), "LINESTRING")

# Idea: union segments that are touching and have same values

rnet_union = sf::st_as_sf(sf::st_union(rnet))
# takes ages
# rnet_buffer = sf::st_buffer(rnet, dist = 1, max_cells = 10000)
rnet_buffer = stplanr::geo_projected(shp = rnet, fun = sf::st_buffer, crs = local_crs, dist = 1)
rnet_union_buffer = stplanr::geo_projected(shp = rnet_union, fun = sf::st_buffer, crs = local_crs, dist = 1)
qtm(rnet_union_buffer)
# rnet_union_buffer = stplanr::geo_buffer(rnet_union, 1)
# Removes relevant network segments:
transport_network_in_buffer = transport_network[rnet_union_buffer, , op = sf::st_within]
# transport_network_in_buffer = transport_network[rnet_union_buffer, ]
transport_network_in_buffer$length = as.numeric(sf::st_length(transport_network_in_buffer))

# qtm(rnet_buffer) +
#   tm_shape(transport_network_in_buffer) +
#   tm_lines(lwd = 3, col = "red")

# rnet_joined = sf::st_join(rnet, transport_network_in_buffer, join = sf::st_contains)
transport_network_joined = sf::st_join(transport_network_in_buffer, rnet_buffer %>% select(id), join = sf::st_within)
transport_network_summary = transport_network_joined %>% 
  sf::st_drop_geometry() %>% 
  group_by(id) %>% 
  summarise(quietness = weighted.mean(w = length, quietness, na.rm = TRUE), carspeed = weighted.mean(w = length, car_speed, na.rm = TRUE))
## TO-DO: colocar aqui carspeedmax !

rnet_joined = left_join(rnet, transport_network_summary)
# tm_shape(rnet_joined %>% filter(cyc10 > 500)) +
#   tm_lines(col = "quietness", lwd = 3, palette = "Blues")

library(biclar)
tm_rnet(rnet_joined,
        lwd = "Bike", #Baseline, ENMAC4, ENMAC10
        col = "quietness",
        palette = "-mako", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)


saveRDS(rnet_joined, "export/rnet_allmodesNSub2_overline_morethan100_clean.Rds")

# # Tests -------------------------------------------------------------------
# 
# 
# 
# lisbon_central = tmaptools::geocode_OSM(q = "lisbon", as.sf = TRUE)
# lisbon_1km = sf::st_buffer(lisbon_central, dist = 1000)
# 
# transport_network_small = transport_network[lisbon_1km, , op = sf::st_within]
# rnet_small = rnet[lisbon_1km, , op = sf::st_within]
# summary(sf::st_length(rnet_small))
# summary(sf::st_length(transport_network_small))
# # transport_network_linestring = sf::st_cast(transport_network_small, to = "LINESTRING", group_or_split = TRUE)
# transport_network_small = sf::st_cast(sf::st_cast(transport_network_small, "MULTILINESTRING"),"LINESTRING")
# 
# # Idea: union segments that are touching and have same values
# 
# rnet_union = sf::st_as_sf(sf::st_union(rnet_small))
# # takes ages
# # rnet_buffer = sf::st_buffer(rnet_small, dist = 1, max_cells = 10000)
# rnet_buffer = stplanr::geo_projected(shp = rnet_small, fun = sf::st_buffer, crs = local_crs, dist = 1)
# rnet_union_buffer = stplanr::geo_projected(shp = rnet_union, fun = sf::st_buffer, crs = local_crs, dist = 1)
# qtm(rnet_union_buffer)
# # rnet_union_buffer = stplanr::geo_buffer(rnet_union, 1)
# # Removes relevant network segments:
# transport_network_in_buffer = transport_network_small[rnet_union_buffer, , op = sf::st_within]
# # transport_network_in_buffer = transport_network_small[rnet_union_buffer, ]
# transport_network_in_buffer$length = as.numeric(sf::st_length(transport_network_in_buffer))
# 
# qtm(rnet_buffer) +
#   tm_shape(transport_network_in_buffer) +
#   tm_lines(lwd = 3, col = "red")
# 
# # rnet_joined = sf::st_join(rnet_small, transport_network_in_buffer, join = sf::st_contains)
# transport_network_joined = sf::st_join(transport_network_in_buffer, rnet_buffer %>% select(id), join = sf::st_within)
# transport_network_summary = transport_network_joined %>% 
#   sf::st_drop_geometry() %>% 
#   group_by(id) %>% 
#   summarise(quietness = weighted.mean(w = length, quietness, na.rm = TRUE))
# 
# rnet_joined = left_join(rnet_small, transport_network_summary)
# tm_shape(rnet_joined) +
#   tm_lines(col = "quietness", lwd = 3, palette = "Blues")
# 
# transport_network_intersection = sf::st_intersection(transport_network_in_buffer, rnet_union_buffer)
# qtm(transport_network_intersection) +
#   qtm(rnet_union_buffer)
# transport_network_intersection$length = as.numeric(sf::st_length(transport_network_intersection))
# transport_network_keep = transport_network_intersection %>% 
#   filter(length > 7)
# 
# qtm(rnet_buffer) +
#   tm_shape(transport_network_keep) +
#   tm_lines(lwd = 3, col = "red") +
#   tm_shape(transport_network_keep %>% filter(length < 20)) +
#   tm_lines(lwd = 5, col = "blue")
# # +
# #   qtm(transport_network_small)
# 
# 


# # Another idea:
# 
# transport_network_small = transport_network_small %>% 
#   group_by(osm_id, quietness) %>% 
#   summarise(quietness = mean(quietness))
# 
# rnet_segments = sf::st_join(transport_network_small, rnet_small, join = sf::st_intersects)
# rnet_segment_filtered = rnet_segments %>% 
#   filter(!is.na(Bike))
# qtm(rnet_segment_filtered)
# rnet_contained_within = rnet_segments %>% 
#   slice(unlist(sf::st_contains_properly(rnet_small, rnet_segment_filtered)))
# 
# qtm(rnet_contained_within)