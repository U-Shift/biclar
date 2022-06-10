## export separeted html map for each 18 municipalities

MUNICIPIOS = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/MUNICIPIOSgeo.Rds"))
MUNICIPIOS = MUNICIPIOS$Concelho

for(i in 1:length(MUNICIPIOS)){
  mun = MUNICIPIOS[i]
  REDE = REDEexistente %>% filter(Concelho == mun)  
  
  #make and export 18 maps
  m = tm_layout(title = mun, panel.show = T) +
    # REPLACE HERE THE TMAP FUNCION FOR ZOOM AT MUNICIPALITIES
    #
    # tm_shape(REDE) +
    # tm_sf(col = "Tipologia",
    #       lwd = 2.5,
    #       palette = greens2,
    #       title.col = "Rede Existente por tipologia",
    #       popup.vars = c(
    #         "Município" = "Municipio",
    #         "Tipologia" = "Tipologia",
    #         "Função" = "Funcao",      
    #         "Sentido" = "Sentido", 
    #         "Comprimento" = "Lenght", 
    #         "ID" = "ID"
    #       ),
    # )  
  
  map = tmap::tmap_leaflet(m)
  tmap::tmap_save(tm = map, filename = paste0("maps/",mun, "_Rede500.html"))
}




