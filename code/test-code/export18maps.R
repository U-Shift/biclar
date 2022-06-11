## export separeted html map for each 18 municipalities

MUNICIPIOSgeo = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/MUNICIPIOSgeo.Rds"))
MUNICIPIOS = MUNICIPIOSgeo$Concelho


REDE = #SET ROUTES FILE HERE

for(i in 1:length(MUNICIPIOS)){
  mun = MUNICIPIOS[i]
  BUFFER = MUNICIPIOSgeo %>% filter(Concelho == mun) %>% sf::st_buffer(500)
  REDE = REDE %>% sf::st_crop(BUFFER) #%>% filter(Concelho == mun) 
  
  #make and export 18 maps
  m = tm_layout(title = mun, panel.show = T) +
    biclar::tm_rnet(REDE)
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




