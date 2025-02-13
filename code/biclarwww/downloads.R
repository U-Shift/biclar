library(dplyr)
library(sf)

for (i in municipios){
  
  # i = "barreiro"
  mun = stringr::str_to_title(i)
  # i = "vila franca de xira"
  # mun = "Vila Franca de Xira" #problema com o De
  
  # cenario 1
  mapa11r = sf::st_read("export_rnet/rnet_aml_1404_15.gpkg", quiet = TRUE)
  mapa11r = mapa11r[MUNICIPIOSgeo %>% filter(Concelho == mun) %>% sf::st_buffer(300), , op = sf::st_within] %>% 
    mutate(tCO2eq = tCO2eq10/10,
           quietness = as.integer(quietness)) 
  mapa12r = sf::st_read("export_rnet/rnet_aml_1410_15.gpkg", quiet = TRUE)
  mapa12r = mapa12r[MUNICIPIOSgeo %>% filter(Concelho == mun) %>% sf::st_buffer(300), , op = sf::st_within] %>% 
    mutate(tCO2eq = tCO2eq10/10,
           quietness = as.integer(quietness)) 
  
  mapa1r = mapa11r %>%
    rename(ViagensTotal_base = Total,
           Ciclistas_base = Bike,
           Ciclistas_4 = cyc,
           NovosCiclistas_4 = new_cyc,
           Beneficios10_keuro_4 = Beneficios10_keuro,
           tCO2eqanual_4 = tCO2eq) %>% 
    select(-c(tCO2eq10, Bikeper)) %>% 
    bind_cols(mapa12r %>% sf::st_drop_geometry()) %>% 
    rename(Ciclistas_10 = cyc,
           NovosCiclistas_10 = new_cyc,
           Beneficios10_keuro_10 = Beneficios10_keuro,
           tCO2eqanual_10 = tCO2eq) %>% 
    select(-c(tCO2eq10, Bikeper, Total, Bike)) %>% 
    select(-c(12:14)) %>% 
    rename(tranquilidade = quietness...5,
           carspeed = carspeed...6,
           declive = slope...7) %>%
    select(c(1:4, 8:13, 5:7)) %>% 
    mutate(declive = as.numeric(gsub("%", "", declive)))
  
  mapa11l = sf::st_read("export_rnet/rnet_aml_1304_15.gpkg", quiet = TRUE)
  mapa11l = mapa11l[MUNICIPIOSgeo %>% filter(Concelho == mun) %>% sf::st_buffer(300), , op = sf::st_within] %>% 
    mutate(tCO2eq = tCO2eq10/10,
           quietness = as.integer(quietness)) 
  mapa12l = sf::st_read("export_rnet/rnet_aml_1310_15.gpkg", quiet = TRUE)
  mapa12l = mapa12l[MUNICIPIOSgeo %>% filter(Concelho == mun) %>% sf::st_buffer(300), , op = sf::st_within] %>% 
    mutate(tCO2eq = tCO2eq10/10,
           quietness = as.integer(quietness)) 
  
  
  mapa1l = mapa11l %>%
    rename(ViagensTotal_base = Total,
           Ciclistas_base = Bike,
           Ciclistas_4 = cyc,
           NovosCiclistas_4 = new_cyc,
           Beneficios10_keuro_4 = Beneficios10_keuro,
           tCO2eqanual_4 = tCO2eq) %>% 
    select(-c(tCO2eq10, Bikeper)) %>% 
    bind_cols(mapa12l %>% sf::st_drop_geometry()) %>% 
    rename(Ciclistas_10 = cyc,
           NovosCiclistas_10 = new_cyc,
           Beneficios10_keuro_10 = Beneficios10_keuro,
           tCO2eqanual_10 = tCO2eq) %>% 
    select(-c(tCO2eq10, Bikeper, Total, Bike)) %>% 
    select(-c(12:14)) %>% 
    rename(tranquilidade = quietness...5,
           carspeed = carspeed...6,
           declive = slope...7) %>%
    select(c(1:4, 8:13, 5:7)) %>% 
    mutate(declive = as.numeric(gsub("%", "", declive)))
  
  
  # cenario 2
  mapa21r = sf::st_read("export_rnet/rnet_aml_2404_50.gpkg", quiet = TRUE)
  mapa21r = mapa21r[MUNICIPIOSgeo %>% filter(Concelho == mun) %>% sf::st_buffer(300), , op = sf::st_within] %>% 
    mutate(tCO2eq = tCO2eq10/10,
           quietness = as.integer(quietness)) 
  mapa22r = sf::st_read("export_rnet/rnet_aml_2410_50.gpkg", quiet = TRUE)
  mapa22r = mapa22r[MUNICIPIOSgeo %>% filter(Concelho == mun) %>% sf::st_buffer(300), , op = sf::st_within] %>% 
    mutate(tCO2eq = tCO2eq10/10,
           quietness = as.integer(quietness)) 
  
  mapa2r = mapa21r %>%
    rename(ViagensTotal_base = Total,
           Ciclistas_base = Bike,
           Ciclistas_4 = cyc,
           NovosCiclistas_4 = new_cyc,
           Beneficios10_keuro_4 = Beneficios10_keuro,
           tCO2eqanual_4 = tCO2eq) %>% 
    select(-c(tCO2eq10, Bikeper)) %>% 
    bind_cols(mapa22r %>% sf::st_drop_geometry()) %>% 
    rename(Ciclistas_10 = cyc,
           NovosCiclistas_10 = new_cyc,
           Beneficios10_keuro_10 = Beneficios10_keuro,
           tCO2eqanual_10 = tCO2eq) %>% 
    select(-c(tCO2eq10, Bikeper, Total, Bike)) %>% 
    select(-c(12:14)) %>% 
    rename(tranquilidade = quietness...5,
           carspeed = carspeed...6,
           declive = slope...7) %>%
    select(c(1:4, 8:13, 5:7)) %>% 
    mutate(declive = as.numeric(gsub("%", "", declive)))
  
  mapa21l = sf::st_read("export_rnet/rnet_aml_2304_50.gpkg", quiet = TRUE)
  mapa21l = mapa21l[MUNICIPIOSgeo %>% filter(Concelho == mun) %>% sf::st_buffer(300), , op = sf::st_within] %>% 
    mutate(tCO2eq = tCO2eq10/10,
           quietness = as.integer(quietness)) 
  mapa22l = sf::st_read("export_rnet/rnet_aml_2310_50.gpkg", quiet = TRUE)
  mapa22l = mapa22l[MUNICIPIOSgeo %>% filter(Concelho == mun) %>% sf::st_buffer(300), , op = sf::st_within] %>% 
    mutate(tCO2eq = tCO2eq10/10,
           quietness = as.integer(quietness)) 
  
  
  mapa2l = mapa21l %>%
    rename(ViagensTotal_base = Total,
           Ciclistas_base = Bike,
           Ciclistas_4 = cyc,
           NovosCiclistas_4 = new_cyc,
           Beneficios10_keuro_4 = Beneficios10_keuro,
           tCO2eqanual_4 = tCO2eq) %>% 
    select(-c(tCO2eq10, Bikeper)) %>% 
    bind_cols(mapa22l %>% sf::st_drop_geometry()) %>% 
    rename(Ciclistas_10 = cyc,
           NovosCiclistas_10 = new_cyc,
           Beneficios10_keuro_10 = Beneficios10_keuro,
           tCO2eqanual_10 = tCO2eq) %>% 
    select(-c(tCO2eq10, Bikeper, Total, Bike)) %>% 
    select(-c(12:14)) %>% 
    rename(tranquilidade = quietness...5,
           carspeed = carspeed...6,
           declive = slope...7) %>%
    select(c(1:4, 8:13, 5:7)) %>% 
    mutate(declive = as.numeric(gsub("%", "", declive)))
  
  
  # cenario 3
  mapa31r = sf::st_read("export_rnet/rnet_aml_3404_10.gpkg", quiet = TRUE)
  mapa31r = mapa31r[MUNICIPIOSgeo %>% filter(Concelho == mun) %>% sf::st_buffer(300), , op = sf::st_within] %>% 
    mutate(tCO2eq = tCO2eq10/10,
           quietness = as.integer(quietness)) 
  mapa32r = sf::st_read("export_rnet/rnet_aml_3410_10.gpkg", quiet = TRUE)
  mapa32r = mapa32r[MUNICIPIOSgeo %>% filter(Concelho == mun) %>% sf::st_buffer(300), , op = sf::st_within] %>% 
    mutate(tCO2eq = tCO2eq10/10,
           quietness = as.integer(quietness)) 
  
  mapa3r = mapa31r %>%
    rename(ViagensTotal_base = Total,
           Ciclistas_base = Bike,
           Ciclistas_4 = cyc,
           NovosCiclistas_4 = new_cyc,
           Beneficios10_keuro_4 = Beneficios10_keuro,
           tCO2eqanual_4 = tCO2eq) %>% 
    select(-c(tCO2eq10, Bikeper)) %>% 
    bind_cols(mapa32r %>% sf::st_drop_geometry()) %>% 
    rename(Ciclistas_10 = cyc,
           NovosCiclistas_10 = new_cyc,
           Beneficios10_keuro_10 = Beneficios10_keuro,
           tCO2eqanual_10 = tCO2eq) %>% 
    select(-c(tCO2eq10, Bikeper, Total, Bike)) %>% 
    select(-c(12:14)) %>% 
    rename(tranquilidade = quietness...5,
           carspeed = carspeed...6,
           declive = slope...7) %>%
    select(c(1:4, 8:13, 5:7)) %>% 
    mutate(declive = as.numeric(gsub("%", "", declive)))
  
  mapa31l = sf::st_read("export_rnet/rnet_aml_3304_10.gpkg", quiet = TRUE)
  mapa31l = mapa31l[MUNICIPIOSgeo %>% filter(Concelho == mun) %>% sf::st_buffer(300), , op = sf::st_within] %>% 
    mutate(tCO2eq = tCO2eq10/10,
           quietness = as.integer(quietness)) 
  mapa32l = sf::st_read("export_rnet/rnet_aml_3310_10.gpkg", quiet = TRUE)
  mapa32l = mapa32l[MUNICIPIOSgeo %>% filter(Concelho == mun) %>% sf::st_buffer(300), , op = sf::st_within] %>% 
    mutate(tCO2eq = tCO2eq10/10,
           quietness = as.integer(quietness)) 
  
  
  mapa3l = mapa31l %>%
    rename(ViagensTotal_base = Total,
           Ciclistas_base = Bike,
           Ciclistas_4 = cyc,
           NovosCiclistas_4 = new_cyc,
           Beneficios10_keuro_4 = Beneficios10_keuro,
           tCO2eqanual_4 = tCO2eq) %>% 
    select(-c(tCO2eq10, Bikeper)) %>% 
    bind_cols(mapa32l %>% sf::st_drop_geometry()) %>% 
    rename(Ciclistas_10 = cyc,
           NovosCiclistas_10 = new_cyc,
           Beneficios10_keuro_10 = Beneficios10_keuro,
           tCO2eqanual_10 = tCO2eq) %>% 
    select(-c(tCO2eq10, Bikeper, Total, Bike)) %>% 
    select(-c(12:14)) %>% 
    rename(tranquilidade = quietness...5,
           carspeed = carspeed...6,
           declive = slope...7) %>%
    select(c(1:4, 8:13, 5:7)) %>% 
    mutate(declive = as.numeric(gsub("%", "", declive)))

  
  if (i == "vila franca de xira") {
    i = "vfxira"
  }
  
  
  download_folder = paste0("biclarwww/", i)
  # dir.create(download_folder) 
  
  path_sf_1r = paste0(download_folder, "/", i, "_rededireta_cenario1.gpkg")
  sf::st_write(mapa1r, path_sf_1r, delete_dsn = TRUE)
  
  path_sf_1l = paste0(download_folder, "/", i, "_redesegura_cenario1.gpkg")
  sf::st_write(mapa1l, path_sf_1l, delete_dsn = TRUE)
  
  path_sf_2r = paste0(download_folder, "/", i, "_rededireta_cenario2.gpkg")
  sf::st_write(mapa2r, path_sf_2r, delete_dsn = TRUE)
  
  path_sf_2l = paste0(download_folder, "/", i, "_redesegura_cenario2.gpkg")
  sf::st_write(mapa2l, path_sf_2l, delete_dsn = TRUE)
  
  path_sf_3r = paste0(download_folder, "/", i, "_rededireta_cenario3.gpkg")
  sf::st_write(mapa3r, path_sf_3r, delete_dsn = TRUE)
  
  path_sf_3l = paste0(download_folder, "/", i, "_redesegura_cenario3.gpkg")
  sf::st_write(mapa3l, path_sf_3l, delete_dsn = TRUE)
  
  print(i)
  
  rm(mapa11l, mapa11r, mapa12l, mapa12r, mapa1l, mapa1r,
     mapa21l, mapa21r, mapa22l, mapa22r, mapa2l, mapa2r,
     mapa31l, mapa31r, mapa32l, mapa32r, mapa3l, mapa3r)
}

#demora 5min48seg a correr





# para toda a AML -----------------------------------------------------------------------------


# cenario 1
mapa11r = sf::st_read("export_rnet/rnet_aml_1404_15.gpkg", quiet = TRUE)
mapa11r = mapa11r %>% 
  mutate(tCO2eq = tCO2eq10/10,
         quietness = as.integer(quietness)) 
mapa12r = sf::st_read("export_rnet/rnet_aml_1410_15.gpkg", quiet = TRUE)
mapa12r = mapa12r %>% 
  mutate(tCO2eq = tCO2eq10/10,
         quietness = as.integer(quietness)) 

mapa1r = mapa11r %>%
  rename(ViagensTotal_base = Total,
         Ciclistas_base = Bike,
         Ciclistas_4 = cyc,
         NovosCiclistas_4 = new_cyc,
         Beneficios10_keuro_4 = Beneficios10_keuro,
         tCO2eqanual_4 = tCO2eq) %>% 
  select(-c(tCO2eq10, Bikeper)) %>% 
  bind_cols(mapa12r %>% sf::st_drop_geometry()) %>% 
  rename(Ciclistas_10 = cyc,
         NovosCiclistas_10 = new_cyc,
         Beneficios10_keuro_10 = Beneficios10_keuro,
         tCO2eqanual_10 = tCO2eq) %>% 
  select(-c(tCO2eq10, Bikeper, Total, Bike)) %>% 
  select(-c(12:14)) %>% 
  rename(tranquilidade = quietness...5,
         carspeed = carspeed...6,
         declive = slope...7) %>%
  select(c(1:4, 8:13, 5:7)) %>% 
  mutate(declive = as.numeric(gsub("%", "", declive)))

mapa11l = sf::st_read("export_rnet/rnet_aml_1304_15.gpkg", quiet = TRUE)
mapa11l = mapa11l %>% 
  mutate(tCO2eq = tCO2eq10/10,
         quietness = as.integer(quietness)) 
mapa12l = sf::st_read("export_rnet/rnet_aml_1310_15.gpkg", quiet = TRUE)
mapa12l = mapa12l%>% 
  mutate(tCO2eq = tCO2eq10/10,
         quietness = as.integer(quietness)) 


mapa1l = mapa11l %>%
  rename(ViagensTotal_base = Total,
         Ciclistas_base = Bike,
         Ciclistas_4 = cyc,
         NovosCiclistas_4 = new_cyc,
         Beneficios10_keuro_4 = Beneficios10_keuro,
         tCO2eqanual_4 = tCO2eq) %>% 
  select(-c(tCO2eq10, Bikeper)) %>% 
  bind_cols(mapa12l %>% sf::st_drop_geometry()) %>% 
  rename(Ciclistas_10 = cyc,
         NovosCiclistas_10 = new_cyc,
         Beneficios10_keuro_10 = Beneficios10_keuro,
         tCO2eqanual_10 = tCO2eq) %>% 
  select(-c(tCO2eq10, Bikeper, Total, Bike)) %>% 
  select(-c(12:14)) %>% 
  rename(tranquilidade = quietness...5,
         carspeed = carspeed...6,
         declive = slope...7) %>%
  select(c(1:4, 8:13, 5:7)) %>% 
  mutate(declive = as.numeric(gsub("%", "", declive)))


# cenario 2
mapa21r = sf::st_read("export_rnet/rnet_aml_2404_50.gpkg", quiet = TRUE)
mapa21r = mapa21r%>% 
  mutate(tCO2eq = tCO2eq10/10,
         quietness = as.integer(quietness)) 
mapa22r = sf::st_read("export_rnet/rnet_aml_2410_50.gpkg", quiet = TRUE)
mapa22r = mapa22r%>% 
  mutate(tCO2eq = tCO2eq10/10,
         quietness = as.integer(quietness)) 

mapa2r = mapa21r %>%
  rename(ViagensTotal_base = Total,
         Ciclistas_base = Bike,
         Ciclistas_4 = cyc,
         NovosCiclistas_4 = new_cyc,
         Beneficios10_keuro_4 = Beneficios10_keuro,
         tCO2eqanual_4 = tCO2eq) %>% 
  select(-c(tCO2eq10, Bikeper)) %>% 
  bind_cols(mapa22r %>% sf::st_drop_geometry()) %>% 
  rename(Ciclistas_10 = cyc,
         NovosCiclistas_10 = new_cyc,
         Beneficios10_keuro_10 = Beneficios10_keuro,
         tCO2eqanual_10 = tCO2eq) %>% 
  select(-c(tCO2eq10, Bikeper, Total, Bike)) %>% 
  select(-c(12:14)) %>% 
  rename(tranquilidade = quietness...5,
         carspeed = carspeed...6,
         declive = slope...7) %>%
  select(c(1:4, 8:13, 5:7)) %>% 
  mutate(declive = as.numeric(gsub("%", "", declive)))

mapa21l = sf::st_read("export_rnet/rnet_aml_2304_50.gpkg", quiet = TRUE)
mapa21l = mapa21l%>% 
  mutate(tCO2eq = tCO2eq10/10,
         quietness = as.integer(quietness)) 
mapa22l = sf::st_read("export_rnet/rnet_aml_2310_50.gpkg", quiet = TRUE)
mapa22l = mapa22l%>% 
  mutate(tCO2eq = tCO2eq10/10,
         quietness = as.integer(quietness)) 


mapa2l = mapa21l %>%
  rename(ViagensTotal_base = Total,
         Ciclistas_base = Bike,
         Ciclistas_4 = cyc,
         NovosCiclistas_4 = new_cyc,
         Beneficios10_keuro_4 = Beneficios10_keuro,
         tCO2eqanual_4 = tCO2eq) %>% 
  select(-c(tCO2eq10, Bikeper)) %>% 
  bind_cols(mapa22l %>% sf::st_drop_geometry()) %>% 
  rename(Ciclistas_10 = cyc,
         NovosCiclistas_10 = new_cyc,
         Beneficios10_keuro_10 = Beneficios10_keuro,
         tCO2eqanual_10 = tCO2eq) %>% 
  select(-c(tCO2eq10, Bikeper, Total, Bike)) %>% 
  select(-c(12:14)) %>% 
  rename(tranquilidade = quietness...5,
         carspeed = carspeed...6,
         declive = slope...7) %>%
  select(c(1:4, 8:13, 5:7)) %>% 
  mutate(declive = as.numeric(gsub("%", "", declive)))


# cenario 3
mapa31r = sf::st_read("export_rnet/rnet_aml_3404_10.gpkg", quiet = TRUE)
mapa31r = mapa31r%>% 
  mutate(tCO2eq = tCO2eq10/10,
         quietness = as.integer(quietness)) 
mapa32r = sf::st_read("export_rnet/rnet_aml_3410_10.gpkg", quiet = TRUE)
mapa32r = mapa32r%>% 
  mutate(tCO2eq = tCO2eq10/10,
         quietness = as.integer(quietness)) 

mapa3r = mapa31r %>%
  rename(ViagensTotal_base = Total,
         Ciclistas_base = Bike,
         Ciclistas_4 = cyc,
         NovosCiclistas_4 = new_cyc,
         Beneficios10_keuro_4 = Beneficios10_keuro,
         tCO2eqanual_4 = tCO2eq) %>% 
  select(-c(tCO2eq10, Bikeper)) %>% 
  bind_cols(mapa32r %>% sf::st_drop_geometry()) %>% 
  rename(Ciclistas_10 = cyc,
         NovosCiclistas_10 = new_cyc,
         Beneficios10_keuro_10 = Beneficios10_keuro,
         tCO2eqanual_10 = tCO2eq) %>% 
  select(-c(tCO2eq10, Bikeper, Total, Bike)) %>% 
  select(-c(12:14)) %>% 
  rename(tranquilidade = quietness...5,
         carspeed = carspeed...6,
         declive = slope...7) %>%
  select(c(1:4, 8:13, 5:7)) %>% 
  mutate(declive = as.numeric(gsub("%", "", declive)))

mapa31l = sf::st_read("export_rnet/rnet_aml_3304_10.gpkg", quiet = TRUE)
mapa31l = mapa31l%>% 
  mutate(tCO2eq = tCO2eq10/10,
         quietness = as.integer(quietness)) 
mapa32l = sf::st_read("export_rnet/rnet_aml_3310_10.gpkg", quiet = TRUE)
mapa32l = mapa32l%>% 
  mutate(tCO2eq = tCO2eq10/10,
         quietness = as.integer(quietness)) 


mapa3l = mapa31l %>%
  rename(ViagensTotal_base = Total,
         Ciclistas_base = Bike,
         Ciclistas_4 = cyc,
         NovosCiclistas_4 = new_cyc,
         Beneficios10_keuro_4 = Beneficios10_keuro,
         tCO2eqanual_4 = tCO2eq) %>% 
  select(-c(tCO2eq10, Bikeper)) %>% 
  bind_cols(mapa32l %>% sf::st_drop_geometry()) %>% 
  rename(Ciclistas_10 = cyc,
         NovosCiclistas_10 = new_cyc,
         Beneficios10_keuro_10 = Beneficios10_keuro,
         tCO2eqanual_10 = tCO2eq) %>% 
  select(-c(tCO2eq10, Bikeper, Total, Bike)) %>% 
  select(-c(12:14)) %>% 
  rename(tranquilidade = quietness...5,
         carspeed = carspeed...6,
         declive = slope...7) %>%
  select(c(1:4, 8:13, 5:7)) %>% 
  mutate(declive = as.numeric(gsub("%", "", declive)))


download_folder = "biclarwww/aml/"
# dir.create(download_folder) 

path_sf_1r = paste0(download_folder, "aml_rededireta_cenario1.gpkg")
sf::st_write(mapa1r, path_sf_1r, delete_dsn = TRUE)

path_sf_1l = paste0(download_folder, "aml_redesegura_cenario1.gpkg")
sf::st_write(mapa1l, path_sf_1l, delete_dsn = TRUE)

path_sf_2r = paste0(download_folder, "aml_rededireta_cenario2.gpkg")
sf::st_write(mapa2r, path_sf_2r, delete_dsn = TRUE)

path_sf_2l = paste0(download_folder, "aml_redesegura_cenario2.gpkg")
sf::st_write(mapa2l, path_sf_2l, delete_dsn = TRUE)

path_sf_3r = paste0(download_folder, "aml_rededireta_cenario3.gpkg")
sf::st_write(mapa3r, path_sf_3r, delete_dsn = TRUE)

path_sf_3l = paste0(download_folder, "aml_redesegura_cenario3.gpkg")
sf::st_write(mapa3l, path_sf_3l, delete_dsn = TRUE)


rm(mapa11l, mapa11r, mapa12l, mapa12r, mapa1l, mapa1r,
   mapa21l, mapa21r, mapa22l, mapa22r, mapa2l, mapa2r,
   mapa31l, mapa31r, mapa32l, mapa32r, mapa3l, mapa3r)
