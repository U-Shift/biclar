library(ggplot2)
library(plotly)
library(scales)

#use routes jittered, not rnet!!!

routes_quietest_grouped = routes_jittered_quietest500_ENMAC410 %>% 
  select(route_number, Bike, new_cyc4, new_cyc10, Total, length) %>% 
  # mutate(length_km = as.numeric(st_length(routes_jittered_quietest500_ENMAC410))/1000) %>% 
  mutate(length_km = length/1000) %>% 
  sf::st_drop_geometry() %>% 
  group_by(route_number) %>% 
  slice(1) %>%
  mutate(distance_band = cut(length_km, 0:20)) %>% 
  group_by(distance_band) %>% 
  summarise(num_cyc = sum(Bike), cyc_length_km = mean(length_km),
            num_cyc_enmac4 = sum(Bike + new_cyc4), cyc_length_km_enmac4 = mean(length_km),
            num_cyc_enmac10 = sum(Bike + new_cyc10), cyc_length_km_enmac10 = mean(length_km),
            num_all_modes = sum(Total), all_modes_length_km = mean(length_km))
routes_quietest_grouped = routes_quietest_grouped %>% 
  mutate_if(is.numeric, round, digits = 2) 

#something missing in between?


p1 = ggplot(routes_quietest_grouped) + 
  geom_smooth(span = 0.2, aes(cyc_length_km_enmac10,
                              num_cyc_enmac10,
                              color="ENMAC 10", 
                              group =1,
                              text = paste("Route distance:", cyc_length_km_enmac10, "<br>Cycling trips:", num_cyc_enmac10))) +
  geom_smooth(span = 0.2, aes(cyc_length_km_enmac4,
                              num_cyc_enmac4,
                              color="ENMAC 4",
                              group =2,
                              text = paste("Route distance:", cyc_length_km_enmac4, "<br>Cycling trips:", num_cyc_enmac4))) +
  geom_smooth(span = 0.2, aes(cyc_length_km,
                              num_cyc,
                              color="Baseline", 
                              group =3,
                              text = paste("Route distance:", cyc_length_km, "<br>Cycling trips:", num_cyc))) +
  # geom_smooth(span = 0.2, aes(all_modes_length_km, num_all_modes, color="All modes"))  +
  scale_y_continuous("Cycling trips/day", labels = label_number(suffix = " k", scale = 1e-3)) +
  scale_x_continuous("Route distance (km)", breaks = seq(0,30, by=5)) +
  labs(title="", color='Scenario') +
  theme_minimal() +  
  guides(color=guide_legend(override.aes=list(fill=NA))) +# removes the grey background in legend categories 
  scale_color_manual(values = c("blue", "orange", "red")) #add  "black" if using all_modes
ggplotly(p1) 



# prepare for website -------------------------------------------------------------------------

# routes1r = readRDS("routes_ferry4_preoverline.Rds") #não. o que se quer mostrar é viagens também com mais de 5km
routes1r = readRDS("routes_r5r_100jit_lts4__ferry_elev_raw.Rds")
# routes2r = readRDS("routes_ferry4_ebike_preoverline.Rds") #não faz sentido pois é igual ao primeiro
routes3r = readRDS("routes_r5r_100jit_lts4__intermodALL_NoSub_elev_raw.Rds")

# ENMAC4 = 0.04 # 4%
# ENMAC10 = 0.10 # 10#


routes1r = routes1r %>% 
  sf::st_drop_geometry() %>% 
  ungroup() %>% 
  filter(mode == "BICYCLE") %>% 
  group_by(id) %>% 
  summarise(Bike = Bike,
            Car = Car + CarP,
            Total = Total,
            distance = sum(distance),
            Bikeper = Bike / Total) %>% 
  ungroup() %>%
  mutate(
    cyc4 = ifelse(Bikeper >= ENMAC4, Bike, ENMAC4 * Total),
    new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
    cyc10 = ifelse(Bikeper >= ENMAC10, Bike, ENMAC10 * Total),
    new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike))


#filter by municipio

routes1r_plot = routes1r %>% 
  mutate(length_km = distance/1000) %>% 
  mutate(distance_band = cut(length_km, 0:20)) %>% 
  group_by(distance_band) %>% 
  summarise(cyc_dist_km = mean(length_km),
            trips_cyc_baseline_1 = sum(Bike),
            trips_cyc_enmac4_1 = sum(cyc4),
            trips_cyc_enmac10_1 = sum(cyc10),
            trips_all_baseline_1 = sum(Total)) %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate_at(c(3:6), round, digits = 0)




routes3rid = routes3r %>% 
  sf::st_drop_geometry() %>% 
  group_by(id) %>% 
  summarise(count = n()) %>% 
  filter(count == 3)

routes3r = routes3r %>% 
  sf::st_drop_geometry() %>% 
  ungroup() %>% 
  filter(id %in% routes3rid$id) %>% 
  filter(mode == "BICYCLE") %>% 
  group_by(id) %>% 
  summarise(Bike = Bike,
            Car = Car + CarP,
            Total = Total,
            distance = sum(distance),
            Bikeper = Bike / Total) %>% 
  ungroup() %>%
  mutate(
    cyc4 = ifelse(Bikeper >= ENMAC4, Bike, ENMAC4 * Total),
    new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
    cyc10 = ifelse(Bikeper >= ENMAC10, Bike, ENMAC10 * Total),
    new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike))


#filter by municipio

routes3r_plot = routes3r %>% 
  mutate(length_km = distance/1000) %>% 
  mutate(distance_band = cut(length_km, 0:20)) %>% 
  group_by(distance_band) %>% 
  summarise(cyc_dist_km = mean(length_km),
            trips_cyc_baseline_3 = sum(Bike),
            trips_cyc_enmac4_3 = sum(cyc4),
            trips_cyc_enmac10_3 = sum(cyc10),
            trips_all_baseline_3 = sum(Total)) %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate_at(c(3:6), round, digits = 0)



# juntar
 routes_13r_plot =
   routes1r_plot %>% 
   left_join(routes3r_plot %>% select(-cyc_dist_km))

 # saveRDS(routes_13r_plot, "routes_13r_plot.Rds")

 
#something missing in between?


p1 = ggplot(routes1r_plot) + 
  geom_smooth(span = 0.2, aes(cyc_dist_km,
                              trips_cyc_enmac10_1,
                              color="ENMAC 10%", 
                              group =1,
                              text = paste("Distancia:", cyc_dist_km, "<br>Viagens em bicicleta:", trips_cyc_enmac10_1))) +
  geom_smooth(span = 0.2, aes(cyc_dist_km,
                              trips_cyc_enmac4_1,
                              color="ENMAC 4%",
                              group =2,
                              text = paste("Distancia:", cyc_dist_km, "<br>Viagens em bicicleta:", trips_cyc_enmac4_1))) +
  geom_smooth(span = 0.2, aes(cyc_dist_km,
                              trips_cyc_baseline_1,
                              color="IMOB 2018", 
                              group =3,
                              text = paste("Distancia:", cyc_dist_km, "<br>Viagens em bicicleta:", trips_cyc_baseline_1))) +
  # geom_smooth(span = 0.2, aes(cyc_dist_km,
  #                             trips_all_baseline_1,
  #                             color="Todos os modos", 
  #                             group =3,
  #                             text = paste("Distancia:", cyc_dist_km, "<br>Viagens total:", trips_all_baseline_1))) +
  scale_y_continuous("Viagens em bicictela /dia", labels = label_number(suffix = " k", scale = 1e-3)) +
  scale_x_continuous("Distancia de viagem (km)", breaks = seq(0,30, by=5)) +
  labs(title="", color='Cenario') +
  theme_minimal() +  
  guides(color=guide_legend(override.aes=list(fill=NA))) +# removes the grey background in legend categories 
  scale_color_manual(values = c("blue", "orange", "red")) #add  "black" if using all_modes
ggplotly(p1) %>% config(displaylogo = FALSE) %>% 
  layout(hovermode = "x") %>% style(hoverinfo = "y")




p2 = ggplot(routes_13r_plot) + 
  geom_smooth(span = 0.2, aes(cyc_dist_km,
                              trips_cyc_enmac10_1,
                              color="ENMAC 10%", 
                              group =1,
                              text = paste("Distancia:", cyc_dist_km, "<br>Viagens em bicicleta:", trips_cyc_enmac10_1))) +
  geom_smooth(span = 0.2, aes(cyc_dist_km,
                              trips_cyc_enmac4_1,
                              color="ENMAC 4%",
                              group =2,
                              text = paste("Distancia:", cyc_dist_km, "<br>Viagens em bicicleta:", trips_cyc_enmac4_1))) +
  geom_smooth(span = 0.3, aes(cyc_dist_km,
                              trips_cyc_enmac10_3,
                              color="Intermodal ENMAC 10%", 
                              group =1,
                              text = paste("Distancia:", cyc_dist_km, "<br>Viagens em bicicleta:", trips_cyc_enmac10_3))) +
  geom_smooth(span = 0.3, aes(cyc_dist_km,
                              trips_cyc_enmac4_3,
                              color="Intermodal ENMAC 4%",
                              group =2,
                              text = paste("Distancia:", cyc_dist_km, "<br>Viagens em bicicleta:", trips_cyc_enmac4_3))) +
  geom_smooth(span = 0.2, aes(cyc_dist_km,
                              trips_cyc_baseline_1,
                              color="IMOB 2018", 
                              group =3,
                              text = paste("Distancia:", cyc_dist_km, "<br>Viagens em bicicleta:", trips_cyc_baseline_1))) +
  # geom_smooth(span = 0.2, aes(cyc_dist_km,
  #                             trips_all_baseline_1,
  #                             color="Todos os modos", 
  #                             group =3,
  #                             text = paste("Distancia:", cyc_dist_km, "<br>Viagens total:", trips_all_baseline_1))) +
  scale_y_continuous("Viagens em bicictela /dia", labels = label_number(suffix = " k", scale = 1e-3)) +
  scale_x_continuous("Distancia de viagem (km)", breaks = seq(0,30, by=5)) +
  labs(title="", color='Cenario') +
  theme_minimal() +  
  guides(color=guide_legend(override.aes=list(fill=NA))) +# removes the grey background in legend categories 
  scale_color_manual(values = c("blue", "darkgreen", "black", "red", "orange")) #add  "black" if using all_modes
ggplotly(p2) %>% config(displaylogo = FALSE) %>% 
  layout(hovermode = "x") %>% style(hoverinfo = "y")
