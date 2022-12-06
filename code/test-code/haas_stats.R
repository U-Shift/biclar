
#Statistics for HEAT


## AML
HEATbind_aml_plot = HEATbind_aml_PPP %>%
  select(Municipio, ENMAC, dist_max, Bike_new, value_newcyc_eur) %>% 
  mutate(Meta_ENMAC = factor(ENMAC, levels = c(0.04, 0.10), labels = c("4%", "10%")),
        Estrategia = factor(dist_max, levels = c(5000, 10000), labels =c("ate 5 km", "ate 10km e-bike")))

qplot(data = HEATbind_aml_plot, value_newcyc_eur, Bike_new, colour = Estrategia, shape = Meta_ENMAC) +
  labs(x = "Benefício médio por novo ciclista", y= "novos ciclistas") +
  theme_minimal()

plotly::ggplotly(qplot(data = HEATbind_aml_plot, value_newcyc_eur, Bike_new, colour = Estrategia, shape = Meta_ENMAC) +
                 labs(x = "Benefício médio por novo ciclista", y= "novos ciclistas") +
                 theme_minimal())





#Municipios

HEATbind_amunicipios_plot = HEATbind_municipios_PPP %>%
  bind_rows(HEATbind_municipios_intermodal_PPP) %>% 
  select(CodePath, Municipio, ENMAC, dist_max, Bike_new, value_newcyc_eur) %>% 
  mutate(Estrategia = substr(CodePath, 1,1),
         LTS = substr(CodePath, 2,2)) %>%
  filter(LTS == "4") %>% 
  mutate(Meta_ENMAC = factor(ENMAC, levels = c(0.04, 0.10), labels = c("4%", "10%")),
         Estrategia = factor(Estrategia, levels = c("1", "2", "3"), labels =c("ate 5 km", "ate 10km e-bike","intermodal ate 5km ")),
         )

qplot(data = HEATbind_amunicipios_plot, value_newcyc_eur, Bike_new, colour = Estrategia, shape = Meta_ENMAC) +
  labs(x = "Benefício médio por novo ciclista", y= "novos ciclistas") +
  theme_minimal()



plotly::ggplotly(qplot(data = HEATbind_amunicipios_plot,
                       value_newcyc_eur,
                       Bike_new,
                       colour = Estrategia,
                       shape = Meta_ENMAC,
                       ) +
                   labs(x = "Benefício médio por novo ciclista", y= "novos ciclistas") +
                   theme_minimal()) #naõ consigo por o label



#Comparar AML com total municipios
HEAT_municipios_PPP_redux = bind_rows(HEATbind_municipios_PPP, HEATbind_municipios_intermodal_PPP) %>% 
  select(-c(routes_filepath, rnet_filepath, car_routes_filepath)) %>% 
  mutate(Estrategia = substr(CodePath, 1,1),
         LTS = substr(CodePath, 2,2)) %>%
  mutate(Meta_ENMAC = factor(ENMAC, levels = c(0.04, 0.10), labels = c("4%", "10%")),
         Estrategia = factor(Estrategia, levels = c("1", "2", "3"), labels =c("ate 5 km", "ate 10km e-bike","intermodal ate 5km ")),
  )

HEAT_aml_PPP_redux = bind_rows(HEATbind_aml_PPP, HEATbind_intermodal_PPP) %>% 
  select(-c(routes_filepath, rnet_filepath, car_routes_filepath)) %>% 
  mutate(Estrategia = substr(Code, 1,1),
         LTS = substr(Code, 2,2)) %>%
  mutate(Meta_ENMAC = factor(ENMAC, levels = c(0.04, 0.10), labels = c("4%", "10%")),
         Estrategia = factor(Estrategia, levels = c("1", "2", "3"), labels =c("ate 5 km", "ate 10km e-bike","intermodal ate 5km ")),
  )


HEAT_municipios_PPP_redux_143 = HEAT_municipios_PPP_redux %>%
  filter(Estrategia == "ate 5 km",
         Meta_ENMAC == "4%",
         LTS == "3")

HEAT_aml_PPP_redux_143 = HEAT_aml_PPP_redux %>%
  filter(Estrategia == "ate 5 km",
         Meta_ENMAC == "4%",
         LTS == "3")


sum(as.numeric(gsub(" ","",HEAT_municipios_PPP_redux_143$Mortality))) #11.889
sum(as.numeric(gsub(" ","",HEAT_municipios_PPP_redux_143$CO2eq))) #7668
sum(HEAT_municipios_PPP_redux_143$Economic) #37777970

sum(as.numeric(gsub(" ","",HEAT_aml_PPP_redux_143$Mortality))) #12
sum(as.numeric(gsub(" ","",HEAT_aml_PPP_redux_143$CO2eq))) #7658
sum(HEAT_aml_PPP_redux_143$Economic) #37882000
format(sum(HEAT_aml_PPP_redux_143$Economic10), scientific=F) #336.770.000


HEAT_municipios_PPP_redux_2104 = HEAT_municipios_PPP_redux %>%
  filter(Estrategia == "ate 10km e-bike",
         Meta_ENMAC == "10%",
         LTS == "4")

HEAT_aml_PPP_redux_2104 = HEAT_aml_PPP_redux %>%
  filter(Estrategia == "ate 10km e-bike",
         Meta_ENMAC == "10%",
         LTS == "4")


sum(as.numeric(gsub(" ","",HEAT_municipios_PPP_redux_2104$Mortality))) #89.41
sum(as.numeric(gsub(" ","",HEAT_municipios_PPP_redux_2104$CO2eq))) #65533
sum(HEAT_municipios_PPP_redux_2104$Economic) #284019900

sum(as.numeric(gsub(" ","",HEAT_aml_PPP_redux_2104$Mortality))) #90
sum(as.numeric(gsub(" ","",HEAT_aml_PPP_redux_2104$CO2eq))) #65420
sum(HEAT_aml_PPP_redux_2104$Economic) #284690000
format(sum(HEAT_aml_PPP_redux_2104$Economic10), scientific=F) #2.537.000.000




saveRDS(HEAT_municipios_PPP_redux, "HEAT/HEAT_municipios_123_redux.Rds")
