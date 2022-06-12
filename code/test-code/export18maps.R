## export separeted html map for each 18 municipalities

library(dplyr)
library(sf)
library(biclar)

MUNICIPIOSgeo = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/MUNICIPIOSgeo.Rds"))
MUNICIPIOS = MUNICIPIOSgeo$Concelho

REDEregion = readRDS("rnet_enmac_quietest_full.Rds")
# REDEregion = readRDS("rnet_enmac_fastest_full.Rds")

scenario = "enmac10" #baseline, enmac4, enmac10
route = "quiet" #quiet, fast

#Quiet, Baseline - DONE
#Quiet, ENMAC4 - DONE
#Quiet, ENMAC10 - DONE
#Fast, Baseline
#Fast, ENMAC4
#Fast, ENMAC10

for(i in 1:length(MUNICIPIOS)){
  mun = MUNICIPIOS[i]
  BUFFER = MUNICIPIOSgeo %>% filter(Concelho == mun) %>% st_buffer(500)
  REDE = REDEregion %>% st_filter(BUFFER)
  REDE = REDE %>% filter(ENMAC10 >= quantile(REDE$ENMAC10, 0.60)) #try other slices, change here scenario
  
  #make and export 18 maps
  m = tm_rnet(
    REDE,
    lwd = "ENMAC10", #Baseline, ENMAC4, ENMAC10
    col = "Quietness",
    palette = "-mako", # "mako", "reds" - reds for fastest, mako for quietest
    scale = 12,
    lwd_multiplier = 12 #15 used in region
  )
  
  mun = tolower(mun)
  htmlwidgets::saveWidget(m, paste0("pkgdown/assets/", mun, "_", scenario, "_", route, ".html")) 

}



