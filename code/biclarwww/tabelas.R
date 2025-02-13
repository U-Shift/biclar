# result tables by mun (and aml)

tabelas_mun = municipios_index %>% st_drop_geometry() %>%
  select(-url) %>% 
  dplyr::left_join(HEAT_municipios_PPP_redux) %>% 
  dplyr::mutate(Economic10 = Economic10/1000,
                CO2eq = as.numeric(gsub(" ","", CO2eq10))/10) %>%
  dplyr::select(Municipio, Estrategia, LTS, Meta_ENMAC, Pop21, Total, Bike, Bike_new, CO2eq, Economic10) 
saveRDS(tabelas_mun, "HEAT/tabelas_mun.Rds")

tabelas_aml = HEAT_aml_redux %>% 
  dplyr::mutate(Economic10 = Economic10/1000,
                CO2eq = as.numeric(gsub(" ","", CO2eq10))/10) %>%
  dplyr::select(Estrategia, LTS, Meta_ENMAC, Pop21, Total, Bike, Bike_new, CO2eq, Economic10) 
saveRDS(tabelas_aml, "HEAT/tabelas_aml.Rds")






  filter(Municipio == mun)


  # dplyr::mutate_if(is.numeric, round, 1) %>%
  DT::datatable(escape = -1, #para colocar os links, a contar com os rownames
                filter = 'none',
                rownames = FALSE,
                colnames = c("Município", "População 2021", "Viagens /dia", "Viagens Bicicleta", "Novas Viagens Bicicleta 4%",
                             "CO2eq evitado em 10 anos (ton)", "Benefícios estimados em 10 anos (milhares €)"),
                options = list(dom = "t",
                               pageLength = 18,
                               responsive = TRUE,
                               extensions = c('Responsive'))) %>% 