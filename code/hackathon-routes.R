# Aim: create input desire line and route data for hackathon

library(tidyverse)
library(stplanr)

od_cities = readRDS("TRIPSmode_municipal.Rds")
class(od_cities)
names(od_cities)

od_wide = od_cities %>% 
  pivot_wider(names_from = modo, values_from = viagens, values_fill = 0)

piggyback::pb_download("MUNICIPIOSgeo.Rds")
piggyback::pb_download("CENTROIDS_municipios.Rds")
city_boundaries = readRDS("MUNICIPIOSgeo.Rds")
city_centroids = readRDS("CENTROIDS_municipios.Rds")

city_centroids = sf::st_join(city_centroids, city_boundaries) %>% 
  select(Concelho, fid) %>% 
  filter(fid != "3")

mapview::mapview(city_boundaries) +
  mapview::mapview(city_centroids)

# desire_lines_cities = od::od_to_sf(od_wide, city_boundaries) # bug?
# works well:
od_wide = od_wide %>%
  filter(Origem %in% city_centroids$Concelho) %>% 
  filter(Destino %in% city_centroids$Concelho) 
desire_lines_cities = od2line(od_wide, city_centroids)
plot(desire_lines_cities)
mapview::mapview(desire_lines_cities)
nrow(desire_lines_cities)
# 281 lines

# only those that have at least 1 cycle trip
summary(desire_lines_cities$Bike > 0) # 62
desire_lines_cycle = desire_lines_cities %>% 
  filter(Bike > 0)
plot(desire_lines_cycle)
# add euclidean distance
desire_lines_cycle$length_euclidean = as.numeric(sf::st_length(desire_lines_cycle))
summary(desire_lines_cycle)
desire_lines_cycle = desire_lines_cycle %>% 
  filter(length_euclidean > 0)
summary(desire_lines_cycle$length_euclidean)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4706   10880   14586   16714   21250   40226 
totals = desire_lines_cycle %>% select(Bike:Walk) %>% 
  sf::st_drop_geometry() %>% 
  rowSums()
plot(desire_lines_cycle)
plot(
  desire_lines_cycle$length_euclidean,
  desire_lines_cycle$Bike / totals,
  cex = totals / mean(totals) 
  )
sum(totals)
# simplify - round to integers 
desire_lines_integers = desire_lines_cycle %>% 
  mutate_if(is.numeric, .funs = round)
desire_lines_integers
summary(desire_lines_integers)
mapview::mapview(desire_lines_integers)

# calculate cyclestreets routes
routes_integers_cs_fastest = route(l = desire_lines_integers,
                                   route_fun = cyclestreets::journey,
                                   plan = "fastest")
routes_integers_cs_balanced = route(l = desire_lines_integers,
                                   route_fun = cyclestreets::journey,
                                   plan = "balanced")
routes_integers_cs_quietest = route(l = desire_lines_integers,
                                    route_fun = cyclestreets::journey,
                                    plan = "quietest")

sum(routes_integers_cs_fastest$distances) / 
  sum(routes_integers_cs_balanced$distances)
# [1] 0.941153
sum(routes_integers_cs_quietest$distances) / 
  sum(routes_integers_cs_balanced$distances)
# [1] 1.07065

routes_integers_cs_fastest_agg = routes_integers_cs_fastest %>% 
  group_by(Origem, Destino) %>% 
  summarise(
    Origem = first(Origem),
    Destino = first(Destino),
    Bike = mean(Bike),
    All = mean(Bike) + mean(Car) + mean(Motorcycle) + mean(Transit) + mean(Walk) + mean(Other),
    Hilliness_average = mean(gradient_segment),
    Hilliness_90th_percentile = quantile(gradient_segment, probs = 0.9)
  )
nrow(routes_integers_cs_fastest_agg) == nrow(desire_lines_integers) # TRUE
mapview::mapview(routes_integers_cs_fastest_agg, zcol = "Bike")
mapview::mapview(routes_integers_cs_fastest_agg, zcol = "Hilliness_average")
mapview::mapview(routes_integers_cs_fastest_agg, zcol = "Hilliness_90th_percentile")

# output and upload data --------------------------------------------------

write_and_upload_sf = function(object_name, extension = ".geojson", repo = NULL) {
  file_name = paste0(object_name, extension)
  object = get(object_name)
  if(!file.exists(file_name)) {
    sf::write_sf(object, file_name)
  }
  piggyback::pb_upload(file_name, repo = repo)
}


sf::write_sf(desire_lines_integers, "desire_lines_integers.geojson")
piggyback::pb_upload("desire_lines_integers.geojson", repo = "u-shift/cyclingpotential-hack")

write_and_upload_sf("routes_integers_cs_fastest_agg", repo = "u-shift/cyclingpotential-hack")
write_and_upload_sf("routes_integers_cs_fastest", repo = "u-shift/cyclingpotential-hack")
write_and_upload_sf("routes_integers_cs_balanced", repo = "u-shift/cyclingpotential-hack")
write_and_upload_sf("routes_integers_cs_quietest", repo = "u-shift/cyclingpotential-hack")
