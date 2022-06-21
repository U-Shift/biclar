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


