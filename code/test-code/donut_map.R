# try to replicate donut_maps
# https://github.com/mtennekes/donutmaps

TRIPSmode_municipal = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/TRIPSmode_municipal.Rds"))
CENTROIDS_municipios = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/CENTROIDS_municipios.Rds"))
ZONAMENTO = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/ZONAMENTO_imob.Rds"))
ZONAMENTO$DTMN_DSG[ZONAMENTO$DTMN_DSG == "Setúbal"] = "Setubal"


DonutTrips = TRIPSmode_municipal %>% 
  group_by(Origem, Destino) %>% 
  summarise(viagens = round(sum(viagens))) %>% 
  left_join(ZONAMENTO %>%
              select(DTMN, DTMN_DSG) %>%
              distinct(), 
            by = c("Origem" = "DTMN_DSG")) %>% 
  rename(DTMN_or = DTMN) %>% 
  left_join(ZONAMENTO %>% select(DTMN, DTMN_DSG) %>% distinct(), by = c("Destino" = "DTMN_DSG")) %>% 
  rename(DTMN_de = DTMN) %>% 
  select(DTMN_or, DTMN_de, viagens, Origem, Destino)

CENTROIDS_municipios = CENTROIDS_municipios %>% select(DTMN11, geometry)


DonutTrips = stplanr::od2line(flow = DonutTrips,
                              zones = CENTROIDS_municipios)


library(tmap)
library(donutmaps)

bm = bake_donuts(x = DonutTrips,
            var = "viagens",
            groupname = "Origem",
            pal = CBS_pal,
            donut_size_min = 30000, donut_size_max = 400000,
            flow_th = 500, flow_max = 20000, flow_buffer = 500, flow_scale = 10,
            donut_scale = 1.75)

#não funciona, só com a versão tmap 4
# https://mtennekes.github.io/tmap4/index.html