
# Tests -------------------------------------------------------------------



lisbon_central = tmaptools::geocode_OSM(q = "lisbon", as.sf = TRUE)
lisbon_1km = sf::st_buffer(lisbon_central, dist = 1000)

transport_network_small = transport_network[lisbon_1km, , op = sf::st_within]
rnet_small = rnet[lisbon_1km, , op = sf::st_within]
summary(sf::st_length(rnet_small))
summary(sf::st_length(transport_network_small))
# transport_network_linestring = sf::st_cast(transport_network_small, to = "LINESTRING", group_or_split = TRUE)
transport_network_small = sf::st_cast(sf::st_cast(transport_network_small, "MULTILINESTRING"),"LINESTRING")

# Idea: union segments that are touching and have same values

rnet_union = sf::st_as_sf(sf::st_union(rnet_small))
# takes ages
# rnet_buffer = sf::st_buffer(rnet_small, dist = 1, max_cells = 10000)
rnet_buffer = stplanr::geo_projected(shp = rnet_small, fun = sf::st_buffer, crs = local_crs, dist = 1)
rnet_union_buffer = stplanr::geo_projected(shp = rnet_union, fun = sf::st_buffer, crs = local_crs, dist = 1)
qtm(rnet_union_buffer)
# rnet_union_buffer = stplanr::geo_buffer(rnet_union, 1)
# Removes relevant network segments:
transport_network_in_buffer = transport_network_small[rnet_union_buffer, , op = sf::st_within]
# transport_network_in_buffer = transport_network_small[rnet_union_buffer, ]
transport_network_in_buffer$length = as.numeric(sf::st_length(transport_network_in_buffer))

qtm(rnet_buffer) +
  tm_shape(transport_network_in_buffer) +
  tm_lines(lwd = 3, col = "red")

# rnet_joined = sf::st_join(rnet_small, transport_network_in_buffer, join = sf::st_contains)
transport_network_joined = sf::st_join(transport_network_in_buffer, rnet_buffer %>% select(id), join = sf::st_within)
transport_network_summary = transport_network_joined %>%
  sf::st_drop_geometry() %>%
  group_by(id) %>%
  summarise(quietness = weighted.mean(w = length, quietness, na.rm = TRUE))

rnet_joined = left_join(rnet_small, transport_network_summary)
tm_shape(rnet_joined) +
  tm_lines(col = "quietness", lwd = 3, palette = "Blues")

transport_network_intersection = sf::st_intersection(transport_network_in_buffer, rnet_union_buffer)
qtm(transport_network_intersection) +
  qtm(rnet_union_buffer)
transport_network_intersection$length = as.numeric(sf::st_length(transport_network_intersection))
transport_network_keep = transport_network_intersection %>%
  filter(length > 7)

qtm(rnet_buffer) +
  tm_shape(transport_network_keep) +
  tm_lines(lwd = 3, col = "red") +
  tm_shape(transport_network_keep %>% filter(length < 20)) +
  tm_lines(lwd = 5, col = "blue")
# +
#   qtm(transport_network_small)




# Another idea:

transport_network_small = transport_network_small %>%
  group_by(osm_id, quietness) %>%
  summarise(quietness = mean(quietness))

rnet_segments = sf::st_join(transport_network_small, rnet_small, join = sf::st_intersects)
rnet_segment_filtered = rnet_segments %>%
  filter(!is.na(Bike))
qtm(rnet_segment_filtered)
rnet_contained_within = rnet_segments %>%
  slice(unlist(sf::st_contains_properly(rnet_small, rnet_segment_filtered)))

qtm(rnet_contained_within)



# use NEW rnet_join() function from stplanr ---------------------------------------------------

library(tidyverse)
library(sf)
library(stplanr)
rnet_x = transport_network #OSM_id is the first column
nrow(rnet_x) # 2184086
rnet_y = rnet_ferry4_overline_morethan100_clean %>% mutate(id = 1:nrow(rnet_ferry4_overline_morethan100_clean))
nrow(rnet_y) # 10590


time_i = Sys.time()
rnet_join = rnet_join(rnet_x, rnet_y)
Sys.time() - time_i
# Time difference of 3.987354 mins
nrow(rnet_join) # 85442


rnetj_summary = rnet_join %>%
  sf::st_drop_geometry() %>%
  group_by(osm_id) %>%
    summarise(
      Bike = weighted.mean(Bike, length_y, na.rm = TRUE),
      Total = weighted.mean(Total, length_y, na.rm = TRUE),
      new_cyc4 = weighted.mean(new_cyc4, length_y, na.rm = TRUE),
      cyc4 = weighted.mean(cyc4, length_y, na.rm = TRUE),
      new_cyc10 = weighted.mean(new_cyc10, length_y, na.rm = TRUE),
      cyc10 = weighted.mean(cyc10, length_y, na.rm = TRUE),
      )
osm_joined_rnet = left_join(rnet_x, rnetj_summary)
nrow(osm_joined_rnet) #2184086

sum(rnet_y$Bike) # 250545
sum(osm_joined_rnet$Bike, na.rm = TRUE) # 2112663

rnet_y$length = as.numeric(st_length(rnet_y))
sum(rnet_y$Bike * rnet_y$length) # 17115920
osm_joined_rnet$length = as.numeric(st_length(osm_joined_rnet))
sum(osm_joined_rnet$Bike * osm_joined_rnet$length, na.rm = TRUE) #47510538
#are those the same? 


# reverse process
rnet_y = transport_network 
nrow(rnet_y) # 2184086
rnet_x = rnet_ferry4_overline_morethan100_clean %>% mutate(id = 1:nrow(rnet_ferry4_overline_morethan100_clean)) #id is the last column
nrow(rnet_x) # 10590

time_i = Sys.time()
osm_subset = rnet_subset(rnet_y, rnet_x) #reduce the large OSM network
Sys.time() - time_i # Time difference of 2.259305 mins
nrow(osm_subset) # 58876

time_i = Sys.time()
rnet_join2 = rnet_join(rnet_x, osm_subset, key_column = 10) # id - is this parameter working??
Sys.time() - time_i# Time difference of 1.233207 mins
nrow(rnet_join2) # 53298

rnetj_summary2 = rnet_join2 %>% # WHERE IS THE ID  #STOP HERE
  sf::st_drop_geometry() %>%
  group_by(id) %>%
  summarise(
    quietness = weighted.mean(quietness, length_y, na.rm = TRUE),
    carspeed = weighted.mean(car_speed, length_y, na.rm = TRUE),
    carspeed_max = max(car_speed, na.rm = TRUE),
  )
osm_joined_rnet2 = left_join(rnet_y, rnetj_summary2)
nrow(osm_joined_rnet2) #
