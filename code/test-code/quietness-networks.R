# Aim: get quietness scores on route networks

library(tidyverse)
library(biclar)
library(tmap)
tmap_mode("view")
local_crs = "EPSG:3857"

# Import data -------------------------------------------------------------

# # transport_network = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/quietnessAML_alltags_fewer.Rds"))
# transport_network = quietnessAML_alltags_fewer
# names(transport_network)
# # summary(sf::st_geometry_type(transport_network))
# transport_network = sf::st_cast(sf::st_cast(transport_network, "MULTILINESTRING"), "LINESTRING")
# saveRDS(transport_network, "transport_network.Rds")


# rnet_allmodesNSub4_overline_morethan100_clean = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/rnet_allmodesNSub4_overline_morethan100_clean.Rds"))

#enmac
# rnet = rnet_ferry3_overline_morethan20_clean #done
# rnet = rnet_ferry3_overline_morethan100_clean #done
# rnet = rnet_ferry3_overline_morethan200_clean #done
# rnet = rnet_ferry4_overline_morethan20_clean #done
# rnet = rnet_ferry4_overline_morethan100_clean #done
# rnet = rnet_ferry4_overline_morethan200_clean #done
# 
# #ebikes
# rnet = rnet_ferry3_ebike_overline_morethan50_clean #done
# rnet = rnet_ferry3_ebike_overline_morethan100_clean #done
# rnet = rnet_ferry3_ebike_overline_morethan200_clean #done
# rnet = rnet_ferry3_ebike_overline_morethan400_clean #done
# rnet = rnet_ferry4_ebike_overline_morethan50_clean #done
# rnet = rnet_ferry4_ebike_overline_morethan100_clean #done
# rnet = rnet_ferry4_ebike_overline_morethan200_clean #done
# rnet = rnet_ferry4_ebike_overline_morethan400_clean #done

# #intermodal
# rnet = rnet_allmodesNSub3_overline_morethan10_clean #done
# rnet = rnet_allmodesNSub3_overline_morethan30_clean #done
# rnet = rnet_allmodesNSub3_overline_morethan50_clean #done
# rnet = rnet_allmodesNSub4_overline_morethan10_clean #done
# rnet = rnet_allmodesNSub4_overline_morethan30_clean #done
# rnet = rnet_allmodesNSub4_overline_morethan50_clean #done

colnames(rnet)
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
                                     Bikeper = scales::label_percent(accuracy = 0.1, sufix = " %")(Bikeper),
                                     new_cyc4 = round(new_cyc4),
                                     cyc4 = round(cyc4),
                                     new_cyc10 = round(new_cyc10),
                                     cyc10 = round(cyc10),
                                     quietness = as.integer(quietness),
                                     carspeed = as.integer(carspeed),
                                     carspeedmax = as.integer(carspeedmax)) %>% 
                              select(Total, Bikeper, Bike, cyc4, new_cyc4, cyc10, new_cyc10, quietness, carspeed, carspeedmax, geometry) # -Total ?


# slopes weighted here 

# library(slopes)
# dem25 = raster::raster("r5r/LisboaAML_COPERNICUS_clip_WGS84.tif")
rnet_joined$slope = slopes::slope_raster(rnet_joined, dem25)

# summary(rnet_joined$slope)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.000053 0.013610 0.027265 0.036485 0.049604 0.256815       12 

rnet_joined$slope = scales::label_percent(accuracy = 0.1, sufix = " %")(rnet_joined$slope)



# saveRDS(rnet_joined, "export2/rnet_ferry3_overline_morethan20_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_ferry3_overline_morethan100_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_ferry3_overline_morethan200_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_ferry4_overline_morethan20_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_ferry4_overline_morethan100_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_ferry4_overline_morethan200_clean_tags.Rds") #done

# saveRDS(rnet_joined, "export2/rnet_ferry3_ebike_overline_morethan50_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_ferry3_ebike_overline_morethan100_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_ferry3_ebike_overline_morethan200_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_ferry3_ebike_overline_morethan400_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_ferry4_ebike_overline_morethan50_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_ferry4_ebike_overline_morethan100_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_ferry4_ebike_overline_morethan200_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_ferry4_ebike_overline_morethan400_clean_tags.Rds") #done

# saveRDS(rnet_joined, "export2/rnet_allmodesNSub3_overline_morethan10_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_allmodesNSub3_overline_morethan30_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_allmodesNSub3_overline_morethan50_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_allmodesNSub4_overline_morethan10_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_allmodesNSub4_overline_morethan30_clean_tags.Rds") #done
# saveRDS(rnet_joined, "export2/rnet_allmodesNSub4_overline_morethan50_clean_tags.Rds") #done


####

rnet_joined = readRDS("export2/rnet_ferry4_overline_morethan200_clean_tags.Rds")
tm_rnet(rnet_joined, #filtrar barreiro [MUNICIPIOSgeo %>% filter(Concelho == "Barreiro"), , op = sf::st_within]
        lwd = "cyc4", #Baseline, ENMAC4, ENMAC10
        col = "quietness",
        palette = "-reds", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)




### NOT NEEDED NOW
# sf::st_write(rnet_joined, "export2/rnet_ferry3_ebike_overline_morethan200_clean_tags.geojson") 
# sf::st_write(rnet_joined, "export2/rnet_ferry4_ebike_overline_morethan200_clean_tags.geojson") 
# sf::st_write(rnet_joined, "export2/rnet_allmodesNSub3_overline_morethan50_clean_tags.geojson") 
# sf::st_write(rnet_joined, "export2/rnet_allmodesNSub4_overline_morethan50_clean_tags.geojson") 


#test 2 layers
rnet_joined2 = readRDS("export2/rnet_ferry4_overline_morethan100_clean_tags.Rds")
tm_rnet(rnet_joined, #ferry3
        lwd = "Bike", #Baseline, ENMAC4, ENMAC10
        col = "quietness",
        palette = "-mako", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
) +
  tm_rnet(rnet_joined2,
          lwd = "Bike", #Baseline, ENMAC4, ENMAC10
          col = "quietness",
          palette = "-burg", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
          scale = 15,
          lwd_multiplier = 15
  )


