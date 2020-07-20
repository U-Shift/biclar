# Aim: create input desire line and route data for hackathon

library(tidyverse)
library(stplanr)

od_cities = readRDS("TRIPSmode_municipal.Rds")
class(od_cities)
names(od_cities)

od_wide = od_cities %>% 
  pivot_wider(names_from = modo, values_from = viagens, values_fill = 0)

piggyback::pb_download("MUNICIPIOSgeo.Rds")
piggyback::pb_download("MUNICIPIOSgeo.Rds")
city_boundaries = readRDS("MUNICIPIOSgeo.Rds")
city_boundaries = readRDS("MUNICIPIOSgeo.Rds")
mapview::mapview(city_boundaries)

# desire_lines_cities = od::od_to_sf(od_wide, city_boundaries) # bug?
# works well:
desire_lines_cities = od2line(od_wide, city_boundaries)
plot(desire_lines_cities)
mapview::mapview(desire_lines_cities)
# 970 lines

# only those that have at least 1 cycle trip
summary(desire_lines_cities$Bike > 0) # 71
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
# [1] 0.986864
sum(routes_integers_cs_quietest$distances) / 
  sum(routes_integers_cs_balanced$distances)
# [1] 1.135998

routes_integers_cs_fastest_agg = routes_integers_cs_fastest %>% 
  group_by(Origem, Destino) %>% 
  summarise(
    Origem = first(Origem),
    Destino = first(Destino),
    Bike = mean(Bike)
  )
nrow(routes_integers_cs_fastest_agg) == nrow(desire_lines_integers) # TRUE
mapview::mapview(routes_integers_cs_fastest_agg, zcol = "Bike")
