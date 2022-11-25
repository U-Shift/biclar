# heat in lines

# names(HEAT_aml_redux)
# names(rnet_tags)

# read data

HEAT_aml_redux = readRDS("HEAT/HEAT_AML_PPP_123_redux.Rds")
RNETS = readxl::read_excel("export2/Scenarios_Routing.xlsx", sheet = "rnet")


for (i in 1:nrow(RNETS)){

rnet_tags = readRDS(RNETS$path[i])
code = RNETS$Code[i]
HEAT_aml = HEAT_aml_redux %>% filter(Code == code)

# Mortality10_aml = as.numeric(gsub(" ","", HEAT_aml$Mortality10))/HEAT_aml$Bike_new #este não faz sentido por segmento
tCO2eq10_aml = as.numeric(gsub(" ","", HEAT_aml$CO2eq10))/HEAT_aml$Bike_new
Econommic10_aml = HEAT_aml$value_newcyc_eur

if (HEAT_aml$ENMAC == 0.04) {
  rnet_tags = rnet_tags %>%
    select(-c(cyc10, new_cyc10)) %>%
    rename(cyc = cyc4,
           new_cyc = new_cyc4)
  
} else {
  rnet_tags = rnet_tags %>%
    select(-c(cyc4, new_cyc4)) %>%
    rename(cyc = cyc10,
           new_cyc = new_cyc10)
} 
    

rnet_tags = rnet_tags %>%
  select(-carspeedmax) %>% 
  mutate(Beneficios10_keuro = round(Econommic10_aml*new_cyc/1000), #em milhares de euro
         tCO2eq10 = round(tCO2eq10_aml*new_cyc))  

summary(rnet_tags$Beneficios10)
summary(rnet_tags$tCO2eq10)

path = paste0("export_rnet/rnet_aml_", code, "_", RNETS$Rede[i], ".Rds")
path_sf = paste0("export_rnet/rnet_aml_", code, "_", RNETS$Rede[i], ".gpkg")

saveRDS(rnet_tags, path)
sf::st_write(rnet_tags, path_sf, delete_dsn = TRUE)

print(path)

}


# test map

rnet_test = sf::st_read("export_rnet/rnet_aml_3410_10.gpkg")
biclar::tm_rnet(rnet_test[MUNICIPIOSgeo %>% filter(Concelho == "Barreiro"), , op = sf::st_within],
                lwd = "cyc", #Baseline, ENMAC4, ENMAC10
                col = "quietness",
                palette = "-reds", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
                scale = 15,
                lwd_multiplier = 15)
