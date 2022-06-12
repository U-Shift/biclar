## export separeted html map for each 18 municipalities

library(dplyr)
library(sf)
library(biclar)

MUNICIPIOSgeo = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/MUNICIPIOSgeo.Rds"))
MUNICIPIOS = MUNICIPIOSgeo$Concelho

# REDEregion = readRDS("rnet_enmac_quietest_full.Rds") #quiet
REDEregion = readRDS("rnet_enmac_fastest_full.Rds") #fast

scenario = "enmac10" #baseline, enmac4, enmac10
route = "fast" #quiet, fast

#Quiet, Baseline - DONE
#Quiet, ENMAC4 - DONE
#Quiet, ENMAC10 - DONE
#Fast, Baseline - DONE
#Fast, ENMAC4 - DONE
#Fast, ENMAC10

for(i in 1:(length(MUNICIPIOS)-1)){
  mun = MUNICIPIOS[i] #test with Lisbon 6
  BUFFER = MUNICIPIOSgeo %>% filter(Concelho == mun) %>% st_buffer(500) #maybe reduce to 200? 
  REDE = REDEregion %>% st_filter(BUFFER)
  REDE = REDE %>% filter(ENMAC10 >= quantile(REDE$ENMAC10, 0.70)) #0.6 for quiet, 0.7 for fast, change here scenario. (Barreiro should be 0.65)
  
  #make and export 18 maps
  m = tm_rnet(
    REDE,
    lwd = "ENMAC10", #Baseline, ENMAC4, ENMAC10
    col = "Quietness",
    palette = "-linear_yl_rd_bk", # "mako", "linear_yl_rd_bk" - linear_yl_rd_bk for fastest, mako for quietest (tried reds, rocket, burg)
    scale = 15,
    lwd_multiplier = 15 #15 used in region and fast, 12 for quiet
  )
  
  mun = tolower(mun)
  htmlwidgets::saveWidget(m, paste0("pkgdown/assets/", mun, "_", scenario, "_", route, ".html")) 

}


## Manually save  vila franca xira , as, vfxira ######

# REDEregion = readRDS("rnet_enmac_quietest_full.Rds") #quiet
REDEregion = readRDS("rnet_enmac_fastest_full.Rds") #fast

scenario = "baseline" #baseline, enmac4, enmac10
route = "fast" #quiet, fast

mun = MUNICIPIOS[18] #Vila Franca de Xira
BUFFER = MUNICIPIOSgeo %>% filter(Concelho == mun) %>% st_buffer(500) #maybe reduce to 200? 
REDE = REDEregion %>% st_filter(BUFFER)
REDE = REDE %>% filter(Baseline >= quantile(REDE$Baseline, 0.70)) #0.6 for quiet, 0.7 for fast, change here scenario. (Barreiro should be 0.65)

#make and export 18 maps
m = tm_rnet(
  REDE,
  lwd = "Baseline", #Baseline, ENMAC4, ENMAC10
  col = "Quietness",
  palette = "-linear_yl_rd_bk", # "mako", "linear_yl_rd_bk" - linear_yl_rd_bk for fastest, mako for quietest (tried reds, rocket, burg)
  scale = 15,
  lwd_multiplier = 15 #15 used in region and fast, 12 for quiet
)

htmlwidgets::saveWidget(m, paste0("pkgdown/assets/vfxira_", scenario, "_", route, ".html")) #Só com VFXIRA

