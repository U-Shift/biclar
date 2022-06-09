# scenarios applied to jittered routes

routes_jittered_quietest500 = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/routes_jittered_quietest_threshold_500_max_9km_total_max_total_10.Rds"))
routes_jittered_quietest500$id2 = 1:nrow(routes_jittered_quietest500)

#cycling potential function
od_top_enmac = routes_jittered_quietest500
ENMAC4 = 0.04 # 4%
ENMAC10 = 0.10 # 10#
od_top_enmac$Bikeper = od_top_enmac$Bike / od_top_enmac$Total

od_top_enmac_aside = od_top_enmac %>% filter(Bikeper >= ENMAC10) #don't mess with the ones that already reach target
od_top_enmac = od_top_enmac %>% filter(Bikeper < ENMAC10)

od_top_enmac$new_cyc = ENMAC10 * od_top_enmac$Total - od_top_enmac$Bike
od_top_enmac$new_carT = (od_top_enmac$Car + od_top_enmac$CarP) - od_top_enmac$new_cyc

od_top_enmac_aside$new_cyc = od_top_enmac_aside$Bike
od_top_enmac_aside$new_carT = (od_top_enmac_aside$Car + od_top_enmac_aside$CarP)

od_top_enmac = rbind(od_top_enmac, od_top_enmac_aside)

#4%
routes_jittered_quietest500_ENMAC410 = routes_jittered_quietest500 %>% st_drop_geometry() %>% 
  left_join(od_top_enmac %>% select(id2, Bikeper, new_cyc, new_carT), by="id2") %>%
  mutate(new_cyc4 = new_cyc, new_car4 = new_carT) %>% 
  select(-c("new_cyc", "new_carT"))

#10%
routes_jittered_quietest500_ENMAC410 = routes_jittered_quietest500_ENMAC410 %>%
  left_join(od_top_enmac %>% select(id2, new_cyc, new_carT), by="id2") %>%
  mutate(new_cyc10 = new_cyc, new_car10 = new_carT) %>% 
  select(-c("new_cyc", "new_carT"))

routes_jittered_quietest500_ENMAC410 = routes_jittered_quietest500_ENMAC410 %>% st_as_sf()
saveRDS(routes_jittered_quietest500_ENMAC410, "routes_jittered_quietest500_ENMAC410.Rds")
