
#Statistics for HEAT

HEATbind_aml_plot = HEATbind_aml_MER %>%
  select(Municipio, ENMAC, dist_max, Bike_new, value_newcyc_eur) %>% 
  mutate(Meta_ENMAC = factor(ENMAC, levels = c(0.04, 0.10), labels = c("4%", "10%")),
        Estrategia = factor(dist_max, levels = c(5000, 10000), labels =c("ate 5 km", "ate 10km e-bike")))

qplot(data = HEATbind_aml_plot, value_newcyc_eur, Bike_new, colour = Estrategia, shape = Meta_ENMAC) +
  labs(x = "Valor médio por novo ciclista", y= "novos ciclistas") +
  theme_minimal()

plotly::ggplotly(qplot(data = HEATbind_aml_plot, value_newcyc_eur, Bike_new, colour = Estrategia, shape = Meta_ENMAC) +
                 labs(x = "Valor médio por novo ciclista", y= "novos ciclistas") +
                 theme_minimal())





HEATbind_amunicipios_plot = HEATbind_municipios_MER %>%
  select(Municipio, ENMAC, dist_max, Bike_new, value_newcyc_eur) %>% 
  mutate(Meta_ENMAC = factor(ENMAC, levels = c(0.04, 0.10), labels = c("4%", "10%")),
         Estrategia = factor(dist_max, levels = c(5000, 10000), labels =c("ate 5 km", "ate 10km e-bike")))

qplot(data = HEATbind_amunicipios_plot, value_newcyc_eur, Bike_new, colour = Estrategia, shape = Meta_ENMAC) +
  labs(x = "Valor médio por novo ciclista", y= "novos ciclistas") +
  theme_minimal()

plotly::ggplotly(qplot(data = HEATbind_aml_plot, value_newcyc_eur, Bike_new, colour = Estrategia, shape = Meta_ENMAC) +
                   labs(x = "Valor médio por novo ciclista", y= "novos ciclistas") +
                   theme_minimal())


