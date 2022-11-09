# get osm and cyclestreets tags for r5r

# get osm and cyclestreets tags for r5r

library(tidyverse)
library(sf)
library(cyclestreets)
library(stplanr)
options(java.parameters = '-Xmx44G') #memory max 8GB in laptop, 44 in desktop
library(r5r)

Lisbon_limit = MUNICIPIOSgeo %>% filter(Concelho == "Lisboa") %>% st_boundary()
AML_limit =  MUNICIPIOSgeo %>% st_union() %>% st_boundary()

quietnessLisbon = ways(bb = Lisbon_limit, limit = 50000L) #37k

quietness1 = quietnessAML[1,]
quietnessAML = for (i in MUNICIPIOS){
  Mun_limit = MUNICIPIOSgeo %>% filter(Concelho == i) %>% st_boundary()
  quientess = ways(bb = Mun_limit, limit = 50000L)
  quietness1 = rbind(quietness1, quientess)
  quietnessall = quietness1
  print(i)
  print(nrow(quientess))
}

quietnessAML = quietnessall
rm(quietness, quietness1, quietnessall)

nrow(quietnessAML) #237k
quietnessAML = distinct(quietnessAML)
nrow(quietnessAML) #150k

r5r_lts_intermodalityALL = setup_r5(data_path = "r5r/allmodes2/", elevation = "MINETTI") #sse r5r_intermodality.r
r5r_lts_intermodalityALL_edges = street_network_to_sf(r5r_lts_intermodalityALL)
r5r_lts_intermodalityALL_edges = r5r_lts_intermodalityALL_edges$edges


r5r_lts_osmtags = r5r_lts_intermodalityALL_edges %>% 
  mutate(osm_id = as.character(osm_id)) %>%
  left_join(osm_data_region %>% st_drop_geometry(), by="osm_id")

quietnessAML_alltags = r5r_lts_osmtags %>%
  left_join(quietnessAML %>% 
              st_drop_geometry() %>%
              mutate(id = as.character(id)),
            by=c("osm_id" = "id"))
colnames(quietnessAML_alltags)
quietnessAML_alltags_few = quietnessAML_alltags %>% select(osm_id, bicycle_lts, quietness, car_speed) %>% ungroup()

quietnessAML_alltags_fewer = quietnessAML_alltags_few %>% group_by(osm_id, bicycle_lts, quietness, car_speed) %>% summarise() %>% ungroup()
nrow(quietnessAML_alltags_fewer) #196k

saveRDS(quietnessAML_alltags_fewer, "quietnessAML_alltags_fewer.Rds")
