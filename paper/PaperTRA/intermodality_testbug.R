## new values fot number of trips that can be replaced from car to Bike + PT (instead of total trops old+new)

TESTEbug1 = CAR_TP_TRANSF %>%
  group_by(LTS, mode_TP) %>% 
  summarise(TP_4 = sum(Bike_new4), TP_10 = sum(Bike_new10)) %>% 
  ungroup() |> 
  pivot_longer(c(3,4), names_to = "ENMAC", values_to = "viagens") %>% 
  pivot_wider(names_from = mode_TP, values_from = viagens) %>% 
  mutate (Total = rowSums(.[3:6])) %>% 
  as.data.frame()

TESTEbug2 = CAR_TP_TRANSF %>%
  group_by(LTS, mode_TP) %>% 
  summarise(TP_4 = sum(TP_4), TP_10 = sum(TP_10)) %>% 
  ungroup() |> 
  pivot_longer(c(3,4), names_to = "ENMAC", values_to = "viagens") %>% 
  pivot_wider(names_from = mode_TP, values_from = viagens) %>% 
  mutate (Total = rowSums(.[3:6])) %>% 
  as.data.frame()

# none of them make sense

# What am I missing here?