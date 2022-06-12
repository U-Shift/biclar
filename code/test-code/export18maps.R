## export separeted html map for each 18 municipalities

library(dplyr)
library(sf)
library(biclar)

MUNICIPIOSgeo = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/MUNICIPIOSgeo.Rds"))
MUNICIPIOS = MUNICIPIOSgeo$Concelho

REDEregion = readRDS("rnet_enmac_quietest_full.Rds")
# REDE = readRDS("rnet_enmac_fastest_full.Rds")

scenario = "baseline" #baseline, enmac4, enmac10
route = "quiet" #quiet, fast

for(i in 1:length(MUNICIPIOS)){
  mun = MUNICIPIOS[i]
  BUFFER = MUNICIPIOSgeo %>% filter(Concelho == mun) %>% st_buffer(500)
  REDE = REDEregion %>% st_filter(BUFFER)
  REDE = REDE %>% filter(Baseline >= quantile(REDE$Baseline, 0.60)) #try other slices
  
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

#Quiet, Baseline - DONE
#Quiet, ENMAC4
#Quiet, ENMAC10
#Fast, Baseline
#Fast, ENMAC4
#Fast, ENMAC10

