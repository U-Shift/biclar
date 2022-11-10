# Aim: get quietness scores on route networks

library(tidyverse)
library(tmap)
tmap_mode("view")
local_crs = "EPSG:3857"

# Import data -------------------------------------------------------------

# transport_network = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/quietnessAML_alltags_fewer.Rds"))
transport_network = quietnessAML_alltags_fewer
names(transport_network)
# summary(sf::st_geometry_type(transport_network))
transport_network = sf::st_cast(sf::st_cast(transport_network, "MULTILINESTRING"), "LINESTRING")


rnet_ferry2_overline_morethan100_clean = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/rnet_ferry2_overline_morethan100_clean.Rds"))
#enmac
# rnet = rnet_ferry2_overline_morethan10_clean 
rnet = rnet_ferry2_overline_morethan100_clean
# rnet = rnet_ferry4_overline_morethan10_clean 
# rnet = rnet_ferry4_overline_morethan100_clean
# 
# #ebikes
# rnet = rnet_ferry2_ebike_overline_morethan10_clean 
# rnet = rnet_ferry2_ebike_overline_morethan100_clean 
# rnet = rnet_ferry4_ebike_overline_morethan10_clean 
# rnet = rnet_ferry4_ebike_overline_morethan100_clean
# 
# #intermodal
# rnet = rnet_allmodesNSub2_overline_morethan50_clean 
# rnet = rnet_allmodesNSub2_overline_morethan100_clean 
# rnet = rnet_allmodesNSub4_overline_morethan30_clean 
# rnet = rnet_allmodesNSub4_overline_morethan100_clean

rnet$id = seq(nrow(rnet))

# Join --------------------------------------------------------------------

# Idea: union segments that are touching and have same values

rnet_union = sf::st_as_sf(sf::st_union(rnet))
rnet_buffer = stplanr::geo_projected(shp = rnet, fun = sf::st_buffer, crs = local_crs, dist = 1)
rnet_union_buffer = stplanr::geo_projected(shp = rnet_union, fun = sf::st_buffer, crs = local_crs, dist = 1) #takes some time (20min)
# Removes relevant network segments:
transport_network_in_buffer = transport_network[rnet_union_buffer, , op = sf::st_within] #takes some time (10min)
transport_network_in_buffer$length = as.numeric(sf::st_length(transport_network_in_buffer))


transport_network_joined = sf::st_join(transport_network_in_buffer, rnet_buffer %>% select(id), join = sf::st_within)
transport_network_summary = transport_network_joined %>% 
  sf::st_drop_geometry() %>% 
  group_by(id) %>% 
  summarise(quietness = round(weighted.mean(w = length, quietness, na.rm = TRUE)),
            carspeed = round(weighted.mean(w = length, car_speed, na.rm = TRUE)),
            carspeedmax = round(max(car_speed, na.rm = TRUE)))
# table(transport_network_summary$carspeedmax)
# table(transport_network_summary$quietness)
# hist(transport_network_summary$quietness)


rnet_joined = left_join(rnet, transport_network_summary)

colnames(rnet_joined)
rnet_joined = rnet_joined %>% mutate(Total = round(Total),
                                     Bike = round(Bike),
                                     Bikeper = scales::label_percent(accuracy = 0.01, sufix = " %")(Bikeper/100),
                                     new_cyc4 = round(new_cyc4),
                                     cyc4 = round(cyc4),
                                     new_cyc10 = round(new_cyc10),
                                     cyc10 = round(cyc10),
                                     quietness = as.integer(quietness),
                                     carspeed = as.integer(carspeed),
                                     carspeedmax = as.integer(carspeedmax)) %>% 
                              select(Total, Bikeper, Bike, cyc4, new_cyc4, cyc10, new_cyc10, quietness, carspeed, carspeedmax, geometry) # -Total ?


# slopes weighted here 

library(slopes)
# dem25 = raster::raster("r5r/LisboaAML_COPERNICUS_clip_WGS84.tif")
rnet_joined$slope = slopes::slope_raster(rnet_joined, dem25)

summary(rnet_joined$slope)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00000 0.01532 0.03039 0.04074 0.05507 0.37669      49 

rnet_joined$slope = scales::label_percent(accuracy = 0.1, sufix = " %")(rnet_joined$slope)


####


library(biclar)
tm_rnet(rnet_joined,
        lwd = "Bike", #Baseline, ENMAC4, ENMAC10
        col = "quietness",
        palette = "-mako", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)


# saveRDS(rnet_joined, "export/rnet_ferry2_overline_morethan10_clean_tags.Rds")
# saveRDS(rnet_joined, "export/rnet_ferry2_overline_morethan100_clean_tags.Rds")
# saveRDS(rnet_joined, "export/rnet_ferry4_overline_morethan10_clean_tags.Rds")
# saveRDS(rnet_joined, "export/rnet_ferry4_overline_morethan100_clean_tags.Rds")
# saveRDS(rnet_joined, "export/rnet_ferry2_ebike_overline_morethan10_clean_tags.Rds")
# saveRDS(rnet_joined, "export/rnet_ferry2_ebike_overline_morethan100_clean_tags.Rds")
# saveRDS(rnet_joined, "export/rnet_ferry4_ebike_overline_morethan10_clean_tags.Rds")
# saveRDS(rnet_joined, "export/rnet_ferry4_ebike_overline_morethan100_clean.Rds")
# saveRDS(rnet_joined, "export/rnet_allmodesNSub2_overline_morethan50_clean_tags.Rds")
# saveRDS(rnet_joined, "export/rnet_allmodesNSub2_overline_morethan100_clean_tags.Rds")
# saveRDS(rnet_joined, "export/rnet_allmodesNSub4_overline_morethan30_clean_tags.Rds")
# saveRDS(rnet_joined, "export/rnet_allmodesNSub4_overline_morethan100_clean_tags.Rds")


