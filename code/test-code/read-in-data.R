# get single file:
piggyback::pb_list()
piggyback::pb_download("TRIPSmode.Rds")
od_lisbon = readRDS("TRIPSmode.Rds")
summary(od_lisbon)
# Origem            Destino              modo              viagens        
# Length:970         Length:970         Length:970         Min.   :     1.9  
# Class :character   Class :character   Class :character   1st Qu.:    47.5  
# Mode  :character   Mode  :character   Mode  :character   Median :   332.0  
# Mean   :  5499.3  
# 3rd Qu.:  1620.9  
# Max.   :342412.4  


# get all files -----------------------------------------------------------

file_list = piggyback::pb_list()
piggyback::pb_download_url(file = file_list$file_name)
# [1] "https://github.com/U-Shift/pct-lisbon2/releases/download/0.0.1/CENTROIDS.Rds"           
# [2] "https://github.com/U-Shift/pct-lisbon2/releases/download/0.0.1/Ciclovias2020CORRECT.Rds"
# [3] "https://github.com/U-Shift/pct-lisbon2/releases/download/0.0.1/CountingPoins.Rds"       
# [4] "https://github.com/U-Shift/pct-lisbon2/releases/download/0.0.1/FREGUESIASgeo.Rds"       
# [5] "https://github.com/U-Shift/pct-lisbon2/releases/download/0.0.1/TRIPSmode_freguesias.Rds"
# [6] "https://github.com/U-Shift/pct-lisbon2/releases/download/0.0.1/TRIPSmode_municipal.Rds" 
# [7] "https://github.com/U-Shift/pct-lisbon2/releases/download/0.0.1/TRIPS_original.Rds"   
piggyback::pb_download(file_list$file_name)
file_list[c("file_name", "size")]
# file_name    size
# 1            CENTROIDS.Rds    3563
# 2 Ciclovias2020CORRECT.Rds  187146
# 3        CountingPoins.Rds  105663
# 4        FREGUESIASgeo.Rds 1738613
# 5 TRIPSmode_freguesias.Rds  165802
# 6  TRIPSmode_municipal.Rds    8122
# 7       TRIPS_original.Rds 1335244

trips_municipal = readRDS("TRIPSmode_municipal.Rds")
nrow(trips_municipal) # 970
trips_f = readRDS("TRIPSmode_freguesias.Rds")
nrow(trips_f) # 7312
names(trips_f)
# trips_all = readRDS("TRIPS_original.Rds")   
centroids = readRDS("CENTROIDS.Rds")
summary(trips_f$DICOFREor11 %in% centroids$Dicofre)
summary(trips_f$DICOFREde11 %in% centroids$Dicofre)
summary(centroids$Dicofre %in% trips_f$DICOFREde11)



# Test routing works ------------------------------------------------------

desire_lines = od::od_to_sf(trips_f, centroids)
# Error in od_coordinates(x, z, silent = silent) : 
#   all(o_code %in% p_code) is not TRUE
desire_lines = stplanr::od2line(flow = trips_f, centroids)
plot(desire_lines)
desire_lines$length_euclidean = as.numeric(sf::st_length(desire_lines))
summary(desire_lines$length_euclidean == 0)
# Mode   FALSE    TRUE 
# logical    7194     118 
library(parallel)
library(cyclestreets)
library(stplanr)
library(tidyverse)
cl <- makeCluster(detectCores())
clusterExport(cl, c("journey"))
desire_lines_sample = desire_lines %>% sample_frac(size = 0.05)
nrow(desire_lines_sample)
system.time({
  routes = route(l = desire_lines_sample, route_fun = cyclestreets::journey, cl = cl) # multi-core
})
# 85 * 20 / 60 = 28 minutes
nrow(routes)
nrow(unique(trips_f[1:2] %>% sf::st_drop_geometry()))
routes_aggregated = routes %>% 
  group_by(DICOFREor11, DICOFREde11) %>% 
  summarise(
    all = mean(Total)
  )
nrow(desire_lines_sample) - nrow(routes_aggregated)
summary(desire_lines_sample$length_euclidean == 0)
# find missing OD pairs
sel_present = paste(desire_lines_sample$DICOFREor11, desire_lines_sample$DICOFREde11) %in%
  paste(routes_aggregated$DICOFREor11, routes_aggregated$DICOFREde11) 
summary(sel_present)
mapview::mapview(routes)
