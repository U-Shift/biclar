# r5r ferry filter
library(tidyverse)
library(sf)
library(stplanr)

# for cenarios 1 ------------------------------------------------------------------------------

## LTS4
routes_r5r_100jit_lts4__ferry_elev = readRDS("routes_r5r_100jit_lts4__ferry_elev_raw.Rds")
# filter for conventional bike
table(routes_r5r_100jit_lts4__ferry_elev$mode)
# BICYCLE   FERRY 
# 54384    2011 
summary(routes_r5r_100jit_lts4__ferry_elev$distance)

routes_ferry4_filtered = routes_r5r_100jit_lts4__ferry_elev %>%
  filter(mode == "BICYCLE") %>%
  st_drop_geometry() %>%
  group_by(id) %>% 
  summarise(distance = sum(distance))
summary(routes_ferry4_filtered$distance) #50% less than 5180m, 75% less than 9600m in total
# routes_ferry4_filtered = routes_ferry4_filtered %>% filter(distance <= 9000)
routes_ferry4_filtered = routes_ferry4_filtered %>% filter(distance <= 5000) #fazer até 5km para os cenarios 4 e 10


routes_ferry4_filtered = routes_r5r_100jit_lts4__ferry_elev %>% filter(id %in% routes_ferry4_filtered$id)
table(routes_ferry4_filtered$mode)
# BICYCLE   FERRY 
#   25491      88
# 2.522k    7626
# 2.506k (2  pernas da mesma viagem, first & last mile) 

# sum(routes_ferry4_filtered$Total[routes_ferry4_filtered$mode == "FERRY"])


# get potential
#cycling potential function
ENMAC4 = 0.04 # 4%
ENMAC10 = 0.10 # 10#
routes_ferry4_filtered$Bikeper = routes_ferry4_filtered$Bike / routes_ferry4_filtered$Total
summary(routes_ferry4_filtered$Bikeper)


routes_ferry4_filtered = routes_ferry4_filtered %>% mutate(
  cyc4 = ifelse(Bikeper >= ENMAC4, Bike, ENMAC4 * Total),
  new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
  cyc10 = ifelse(Bikeper >= ENMAC10, Bike, ENMAC10 * Total),
  new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)
)

sum(routes_ferry4_filtered$Bike) #13k
sum(routes_ferry4_filtered$cyc4) #102k
sum(routes_ferry4_filtered$new_cyc4) #89k
sum(routes_ferry4_filtered$cyc10) #253k
sum(routes_ferry4_filtered$new_cyc10) #240k


rnet_ferry4_overline = routes_ferry4_filtered %>%
  stplanr::overline(c("Bike", "Total", "new_cyc4", "cyc4", "new_cyc10", "cyc10"),
                    fun = "sum") %>% 
  mutate(Bikeper = Bike / Total)
saveRDS(rnet_ferry4_overline, "routes_ferry4_overline.Rds")
saveRDS(routes_ferry4_filtered, "routes_ferry4_preoverline.Rds")

## filter segments with less than X trips?
summary(rnet_ferry4_overline$cyc4)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.774   7.979  19.923  42.091  51.866 692.216 

rnet_ferry4_overline_morethan15 = rnet_ferry4_overline %>% filter(cyc4 >=15) #só sesimbra e mafra
rnet_ferry4_overline_morethan50 = rnet_ferry4_overline %>% filter(cyc4 >=50) 
rnet_ferry4_overline_morethan100 = rnet_ferry4_overline %>% filter(cyc4 >=100) # estruturante

# rnet_ferry4_overline_morethan20 = rnet_ferry4_overline %>% filter(cyc4 >=20) #só sesimbra e mafra
# rnet_ferry4_overline_morethan30 = rnet_ferry4_overline %>% filter(cyc4 >=30)
# rnet_ferry4_overline_morethan60 = rnet_ferry4_overline %>% filter(cyc4 >=60) 
# rnet_ferry4_overline_morethan80 = rnet_ferry4_overline %>% filter(cyc4 >=80) 
# rnet_ferry4_overline_morethan200 = rnet_ferry4_overline %>% filter(cyc4 >=200) #estruturante



#rnet clean to get rid of small pieces - 100
rnet_ferry4_overline_morethan100_clean = rnet_ferry4_overline_morethan100 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry4_overline_morethan100, d = 100 ))
# summary(rnet_ferry4_overline_morethan100_clean$group)

rnet_ferry4_overline_morethan100_group = rnet_ferry4_overline_morethan100_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
# summary(rnet_ferry4_overline_morethan100_group$count)
group = rnet_ferry4_overline_morethan100_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_ferry4_overline_morethan100_clean = rnet_ferry4_overline_morethan100_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry4_overline_morethan100_group)



#rnet clean to get rid of small pieces - 15
rnet_ferry4_overline_morethan15_clean = rnet_ferry4_overline_morethan15 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry4_overline_morethan15, d = 100 ))
# summary(rnet_ferry4_overline_morethan15_clean$group)

rnet_ferry4_overline_morethan15_group = rnet_ferry4_overline_morethan15_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
# summary(rnet_ferry4_overline_morethan15_group$count)
group = rnet_ferry4_overline_morethan15_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_ferry4_overline_morethan15_clean = rnet_ferry4_overline_morethan15_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry4_overline_morethan15_group)





#rnet clean to get rid of small pieces - 50
rnet_ferry4_overline_morethan50_clean = rnet_ferry4_overline_morethan50 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry4_overline_morethan50, d = 100 ))
# summary(rnet_ferry4_overline_morethan50_clean$group)

rnet_ferry4_overline_morethan50_group = rnet_ferry4_overline_morethan50_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
# summary(rnet_ferry4_overline_morethan50_group$count)
group = rnet_ferry4_overline_morethan50_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_ferry4_overline_morethan50_clean = rnet_ferry4_overline_morethan50_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry4_overline_morethan50_group)




library(biclar)
tm_rnet(rnet_ferry4_overline_morethan20_clean,
            lwd = "cyc4", #Baseline, ENMAC4, ENMAC10
            # col = "Quietness",
            palette = "-burg", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
            scale = 15,
            lwd_multiplier = 15
)

saveRDS(rnet_ferry4_overline_morethan100_clean, "export2/rnet_ferry4_overline_morethan100_clean.Rds")
saveRDS(rnet_ferry4_overline_morethan50_clean, "export2/rnet_ferry4_overline_morethan50_clean.Rds")
saveRDS(rnet_ferry4_overline_morethan15_clean, "export2/rnet_ferry4_overline_morethan15_clean.Rds")
# rnet_ferry4_overline_morethan100_clean = readRDS("export2/rnet_ferry4_overline_morethan100_clean.Rds")



mapathres = rnet_ferry4_overline_morethan15_clean %>% mutate(
  volume = cut(
    cyc4,
    breaks = c(15, 20, 30,50, 60, 80, 100, max(cyc4)),
    labels = c("15-20", "20-30","30-50","50-60","60-80","80-100", ">100")
))

mapview::mapview(mapathres, zcol = "volume")








## LTS3

routes_r5r_100jit_lts3__ferry_elev = readRDS("routes_r5r_100jit_lts3__ferry_elev_raw.Rds")
# filter for conventional bike
table(routes_r5r_100jit_lts3__ferry_elev$mode)
# BICYCLE   FERRY 
# 47916    1241 
summary(routes_r5r_100jit_lts3__ferry_elev$distance)

routes_ferry3_filtered = routes_r5r_100jit_lts3__ferry_elev %>%
  filter(mode == "BICYCLE") %>%
  st_drop_geometry() %>%
  group_by(id) %>% 
  summarise(distance = sum(distance))
summary(routes_ferry3_filtered$distance) #50% less than 5003m, 75% less than 8900m in total
# routes_ferry3_filtered = routes_ferry3_filtered %>% filter(distance <= 9000)
routes_ferry3_filtered = routes_ferry3_filtered %>% filter(distance <= 5000) #fazer até 5km para os cenarios 4 e 10


routes_ferry3_filtered = routes_r5r_100jit_lts3__ferry_elev %>% filter(id %in% routes_ferry3_filtered$id)
table(routes_ferry3_filtered$mode)
# BICYCLE   FERRY 
# 23409    81 
# 2.317k    7k
# 2.303k (2  pernas da mesma viagem, first & last mile) 

# sum(routes_ferry3_filtered$Total[routes_ferry3_filtered$mode == "FERRY"])


# get potential
#cycling potential function
ENMAC4 = 0.04 # 4%
ENMAC10 = 0.10 # 10#
routes_ferry3_filtered$Bikeper = routes_ferry3_filtered$Bike / routes_ferry3_filtered$Total

routes_ferry3_filtered = routes_ferry3_filtered %>% mutate(
  cyc4 = ifelse(Bikeper >= ENMAC4, Bike, ENMAC4 * Total),
  new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
  cyc10 = ifelse(Bikeper >= ENMAC10, Bike, ENMAC10 * Total),
  new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)
)

summary(routes_ferry3_filtered$cyc4)
summary(routes_ferry3_filtered$new_cyc4) 
sum(routes_ferry3_filtered$Bike) #12k
sum(routes_ferry3_filtered$cyc4) #94k
sum(routes_ferry3_filtered$new_cyc4) #82k
sum(routes_ferry3_filtered$cyc10) #235k
sum(routes_ferry3_filtered$new_cyc10) #221k


rnet_ferry3_overline = routes_ferry3_filtered %>%
  stplanr::overline(c("Bike", "Total", "new_cyc4", "cyc4", "new_cyc10", "cyc10"),
                    fun = "sum") %>% 
  mutate(Bikeper = Bike / Total)
saveRDS(rnet_ferry3_overline, "routes_ferry3_overline.Rds")
saveRDS(routes_ferry3_filtered, "routes_ferry3_preoverline.Rds")

## filter segments with less than X trips?
summary(rnet_ferry3_overline$cyc4)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.774   7.964  19.771  45.195  54.999 990.643 

rnet_ferry3_overline_morethan15 = rnet_ferry3_overline %>% filter(cyc4 >=15)
rnet_ferry3_overline_morethan50 = rnet_ferry3_overline %>% filter(cyc4 >=50)
rnet_ferry3_overline_morethan100 = rnet_ferry3_overline %>% filter(cyc4 >=100)
# rnet_ferry3_overline_morethan200 = rnet_ferry3_overline %>% filter(cyc4 >=200)


#rnet clean to get rid of small pieces - 100
rnet_ferry3_overline_morethan100_clean = rnet_ferry3_overline_morethan100 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry3_overline_morethan100, d = 100 ))

rnet_ferry3_overline_morethan100_group = rnet_ferry3_overline_morethan100_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_ferry3_overline_morethan100_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_ferry3_overline_morethan100_clean = rnet_ferry3_overline_morethan100_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry3_overline_morethan100_group)


#rnet clean to get rid of small pieces - 15
rnet_ferry3_overline_morethan15_clean = rnet_ferry3_overline_morethan15 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry3_overline_morethan15, d = 100 ))

rnet_ferry3_overline_morethan15_group = rnet_ferry3_overline_morethan15_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_ferry3_overline_morethan15_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_ferry3_overline_morethan15_clean = rnet_ferry3_overline_morethan15_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry3_overline_morethan15_group)



#rnet clean to get rid of small pieces - 50
rnet_ferry3_overline_morethan50_clean = rnet_ferry3_overline_morethan50 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry3_overline_morethan50, d = 100 ))

rnet_ferry3_overline_morethan50_group = rnet_ferry3_overline_morethan50_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_ferry3_overline_morethan50_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_ferry3_overline_morethan50_clean = rnet_ferry3_overline_morethan50_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry3_overline_morethan50_group)


library(biclar)
tm_rnet(rnet_ferry3_overline_morethan50_clean,
        lwd = "cyc4", #Baseline, ENMAC4, ENMAC10
        # col = "Quietness",
        palette = "-linear_yl_rd_bk", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)

saveRDS(rnet_ferry3_overline_morethan100_clean, "export2/rnet_ferry3_overline_morethan100_clean.Rds")
saveRDS(rnet_ferry3_overline_morethan15_clean, "export2/rnet_ferry3_overline_morethan15_clean.Rds")
saveRDS(rnet_ferry3_overline_morethan50_clean, "export2/rnet_ferry3_overline_morethan50_clean.Rds")
# rnet_ferry3_overline_morethan100_clean = readRDS("export2/rnet_ferry3_overline_morethan100_clean.Rds")




# for ebikes ----------------------------------------------------------------------------------

## LTS4
# filter for electric bike
table(routes_r5r_100jit_lts4__ferry_elev$mode)
# BICYCLE   FERRY 
# 54384    2011 
summary(routes_r5r_100jit_lts4__ferry_elev$distance)

routes_ferry4_ebike_filtered = routes_r5r_100jit_lts4__ferry_elev %>%
  filter(mode == "BICYCLE") %>%
  st_drop_geometry() %>%
  group_by(id) %>% 
  summarise(distance = sum(distance))
summary(routes_ferry4_ebike_filtered$distance) #50% less than 5180m, 75% less than 9600m in total
routes_ferry4_ebike_filtered = routes_ferry4_ebike_filtered %>% filter(distance <= 10000) #fazer até 10km para os cenarios ebike


routes_ferry4_ebike_filtered = routes_r5r_100jit_lts4__ferry_elev %>% filter(id %in% routes_ferry4_ebike_filtered$id)
table(routes_ferry4_ebike_filtered$mode)
# BICYCLE   FERRY 
# 40774     704 
# 3.976k    59k

# sum(routes_ferry4_ebike_filtered$Total[routes_ferry4_ebike_filtered$mode == "FERRY"])


# get potential
#cycling potential function
ENMAC4 = 0.04 # 4%
ENMAC10 = 0.10 # 10#
routes_ferry4_ebike_filtered$Bikeper = routes_ferry4_ebike_filtered$Bike / routes_ferry4_ebike_filtered$Total

routes_ferry4_ebike_filtered = routes_ferry4_ebike_filtered %>% mutate(
  cyc4 = ifelse(Bikeper >= ENMAC4, Bike, ENMAC4 * Total),
  new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
  cyc10 = ifelse(Bikeper >= ENMAC10, Bike, ENMAC10 * Total),
  new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)
)

summary(routes_ferry4_ebike_filtered$cyc4)
summary(routes_ferry4_ebike_filtered$new_cyc4) 
sum(routes_ferry4_ebike_filtered$Bike) #22k
sum(routes_ferry4_ebike_filtered$cyc4) #166k
sum(routes_ferry4_ebike_filtered$new_cyc4) #148k
sum(routes_ferry4_ebike_filtered$cyc10) #406k
sum(routes_ferry4_ebike_filtered$new_cyc10) #384k

rnet_ferry4_ebike_overline = routes_ferry4_ebike_filtered %>%
  stplanr::overline(c("Bike", "Total", "new_cyc4", "cyc4", "new_cyc10", "cyc10"), #vale a pena manter ambos 4% 4 10% ?
                    fun = "sum") %>% 
  mutate(Bikeper = Bike / Total)
saveRDS(rnet_ferry4_ebike_overline, "routes_ferry4_ebike_overline.Rds")
saveRDS(routes_ferry4_ebike_filtered, "routes_ferry4_ebike_preoverline.Rds")

## filter segments with less than X trips?
summary(rnet_ferry4_ebike_overline$new_cyc4)

rnet_ferry4_ebike_overline_morethan50 = rnet_ferry4_ebike_overline %>% filter(cyc4 >=50) #still too much detail
rnet_ferry4_ebike_overline_morethan100 = rnet_ferry4_ebike_overline %>% filter(cyc4 >=100)
rnet_ferry4_ebike_overline_morethan200 = rnet_ferry4_ebike_overline %>% filter(cyc4 >=200) #super highways?
rnet_ferry4_ebike_overline_morethan400 = rnet_ferry4_ebike_overline %>% filter(cyc4 >=400) #super hyper highways?

#rnet clean to get rid of small pieces - 100
rnet_ferry4_ebike_overline_morethan100_clean = rnet_ferry4_ebike_overline_morethan100 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry4_ebike_overline_morethan100, d = 100 ))

rnet_ferry4_ebike_overline_morethan100_group = rnet_ferry4_ebike_overline_morethan100_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_ferry4_ebike_overline_morethan100_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_ferry4_ebike_overline_morethan100_clean = rnet_ferry4_ebike_overline_morethan100_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry4_ebike_overline_morethan100_group)


# #rnet clean to get rid of small pieces - 50
rnet_ferry4_ebike_overline_morethan50_clean = rnet_ferry4_ebike_overline_morethan50 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry4_ebike_overline_morethan50, d = 100 ))

rnet_ferry4_ebike_overline_morethan50_group = rnet_ferry4_ebike_overline_morethan50_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_ferry4_ebike_overline_morethan50_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_ferry4_ebike_overline_morethan50_clean = rnet_ferry4_ebike_overline_morethan50_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry4_ebike_overline_morethan50_group)


#rnet clean to get rid of small pieces - 200
rnet_ferry4_ebike_overline_morethan200_clean = rnet_ferry4_ebike_overline_morethan200 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry4_ebike_overline_morethan200, d = 100 ))

rnet_ferry4_ebike_overline_morethan200_group = rnet_ferry4_ebike_overline_morethan200_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_ferry4_ebike_overline_morethan200_group %>% filter(count >=10) #discard networks with less than 5 semnets?
groupp = group$group

rnet_ferry4_ebike_overline_morethan200_clean = rnet_ferry4_ebike_overline_morethan200_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry4_ebike_overline_morethan200_group)



#rnet clean to get rid of small pieces - 400
rnet_ferry4_ebike_overline_morethan400_clean = rnet_ferry4_ebike_overline_morethan400 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry4_ebike_overline_morethan400, d = 100 ))

rnet_ferry4_ebike_overline_morethan400_group = rnet_ferry4_ebike_overline_morethan400_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_ferry4_ebike_overline_morethan400_group %>% filter(count >=10) #discard networks with less than 5 semnets?
groupp = group$group

rnet_ferry4_ebike_overline_morethan400_clean = rnet_ferry4_ebike_overline_morethan400_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry4_ebike_overline_morethan400_group)


library(biclar)
tm_rnet(rnet_ferry3_ebike_overline_morethan100_clean,
        lwd = "cyc4", #Baseline, ENMAC4, ENMAC10
        # col = "Quietness",
        palette = "-linear_yl_rd_bk", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)

saveRDS(rnet_ferry4_ebike_overline_morethan100_clean, "export2/rnet_ferry4_ebike_overline_morethan100_clean.Rds")
saveRDS(rnet_ferry4_ebike_overline_morethan200_clean, "export2/rnet_ferry4_ebike_overline_morethan200_clean.Rds")
saveRDS(rnet_ferry4_ebike_overline_morethan400_clean, "export2/rnet_ferry4_ebike_overline_morethan400_clean.Rds")
saveRDS(rnet_ferry4_ebike_overline_morethan50_clean, "export2/rnet_ferry4_ebike_overline_morethan50_clean.Rds")
# saveRDS(rnet_ferry4_ebike_overline_morethan30_clean, "export2/rnet_ferry4_ebike_overline_morethan30_clean.Rds")
# saveRDS(rnet_ferry4_ebike_overline_morethan10_clean, "export2/rnet_ferry4_ebike_overline_morethan10_clean.Rds")
# rnet_ferry4_ebike_overline_morethan10_clean = readRDS("export2/rnet_ferry4_ebike_overline_morethan10clean.Rds")
# rnet_ferry4_ebike_overline_morethan10_clean = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/rnet_ferry4_ebike_overline_morethan10_clean.Rds"))





## LTS3
# filter for electric bike
table(routes_r5r_100jit_lts3__ferry_elev$mode)
# BICYCLE   FERRY 
# 47916    1241 
summary(routes_r5r_100jit_lts3__ferry_elev$distance)

routes_ferry3_ebike_filtered = routes_r5r_100jit_lts3__ferry_elev %>%
  filter(mode == "BICYCLE") %>%
  st_drop_geometry() %>%
  group_by(id) %>% 
  summarise(distance = sum(distance))
summary(routes_ferry3_ebike_filtered$distance) #50% less than 5003m, 75% less than 8900m in total
routes_ferry3_ebike_filtered = routes_ferry3_ebike_filtered %>% filter(distance <= 10000) #fazer até 10km para os cenarios ebike


routes_ferry3_ebike_filtered = routes_r5r_100jit_lts3__ferry_elev %>% filter(id %in% routes_ferry3_ebike_filtered$id)
table(routes_ferry3_ebike_filtered$mode)
# BICYCLE   FERRY 
# 37919    563 
# 3.708k    47k

# sum(routes_ferry3_ebike_filtered$Total[routes_ferry3_ebike_filtered$mode == "BICYCLE"])


# get potential
#cycling potential function
ENMAC4 = 0.04 # 4%
ENMAC10 = 0.10 # 10#
routes_ferry3_ebike_filtered$Bikeper = routes_ferry3_ebike_filtered$Bike / routes_ferry3_ebike_filtered$Total

routes_ferry3_ebike_filtered = routes_ferry3_ebike_filtered %>% mutate(
  cyc4 = ifelse(Bikeper >= ENMAC4, Bike, ENMAC4 * Total),
  new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
  cyc10 = ifelse(Bikeper >= ENMAC10, Bike, ENMAC10 * Total),
  new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)
)

summary(routes_ferry3_ebike_filtered$cyc4)
summary(routes_ferry3_ebike_filtered$new_cyc4) 
sum(routes_ferry3_ebike_filtered$Bike) #20k
sum(routes_ferry3_ebike_filtered$cyc4) #153k
sum(routes_ferry3_ebike_filtered$new_cyc4) #134k
sum(routes_ferry3_ebike_filtered$cyc10) #377k
sum(routes_ferry3_ebike_filtered$new_cyc10) #357k

rnet_ferry3_ebike_overline = routes_ferry3_ebike_filtered %>%
  stplanr::overline(c("Bike", "Total", "new_cyc4", "cyc4", "new_cyc10", "cyc10"), #vale a pena manter ambos 4% 4 10% ?
                    fun = "sum") %>% 
  mutate(Bikeper = Bike / Total)
saveRDS(rnet_ferry3_ebike_overline, "routes_ferry3_ebike_overline.Rds")
saveRDS(routes_ferry3_ebike_filtered, "routes_ferry3_ebike_preoverline.Rds")

## filter segments with less than X trips?
summary(rnet_ferry3_ebike_overline$new_cyc4)

rnet_ferry3_ebike_overline_morethan50 = rnet_ferry3_ebike_overline %>% filter(cyc4 >=50) 
rnet_ferry3_ebike_overline_morethan100 = rnet_ferry3_ebike_overline %>% filter(cyc4 >=100)
rnet_ferry3_ebike_overline_morethan200 = rnet_ferry3_ebike_overline %>% filter(cyc4 >=200) #super highways
rnet_ferry3_ebike_overline_morethan400 = rnet_ferry3_ebike_overline %>% filter(cyc4 >=400) #super hyper highways


#rnet clean to get rid of small pieces - 100
rnet_ferry3_ebike_overline_morethan100_clean = rnet_ferry3_ebike_overline_morethan100 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry3_ebike_overline_morethan100, d = 100 ))

rnet_ferry3_ebike_overline_morethan100_group = rnet_ferry3_ebike_overline_morethan100_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_ferry3_ebike_overline_morethan100_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_ferry3_ebike_overline_morethan100_clean = rnet_ferry3_ebike_overline_morethan100_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry3_ebike_overline_morethan100_group)


# #rnet clean to get rid of small pieces - 50
rnet_ferry3_ebike_overline_morethan50_clean = rnet_ferry3_ebike_overline_morethan50 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry3_ebike_overline_morethan50, d = 100 ))

rnet_ferry3_ebike_overline_morethan50_group = rnet_ferry3_ebike_overline_morethan50_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_ferry3_ebike_overline_morethan50_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_ferry3_ebike_overline_morethan50_clean = rnet_ferry3_ebike_overline_morethan50_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry3_ebike_overline_morethan50_group)



# #rnet clean to get rid of small pieces - 200
rnet_ferry3_ebike_overline_morethan200_clean = rnet_ferry3_ebike_overline_morethan200 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry3_ebike_overline_morethan200, d = 100 ))

rnet_ferry3_ebike_overline_morethan200_group = rnet_ferry3_ebike_overline_morethan200_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_ferry3_ebike_overline_morethan200_group %>% filter(count >=10) #discard networks with less than 10 semnets?
groupp = group$group

rnet_ferry3_ebike_overline_morethan200_clean = rnet_ferry3_ebike_overline_morethan200_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry3_ebike_overline_morethan200_group)



# #rnet clean to get rid of small pieces - 400
rnet_ferry3_ebike_overline_morethan400_clean = rnet_ferry3_ebike_overline_morethan400 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry3_ebike_overline_morethan400, d = 100 ))

rnet_ferry3_ebike_overline_morethan400_group = rnet_ferry3_ebike_overline_morethan400_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_ferry3_ebike_overline_morethan400_group %>% filter(count >=10) #discard networks with less than 10 semnets?
groupp = group$group

rnet_ferry3_ebike_overline_morethan400_clean = rnet_ferry3_ebike_overline_morethan400_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry3_ebike_overline_morethan400_group)


library(biclar)
tm_rnet(rnet_ferry3_ebike_overline_morethan200_clean,
        lwd = "cyc4", #Baseline, ENMAC4, ENMAC10
        # col = "Quietness",
        palette = "-linear_yl_rd_bk", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)

saveRDS(rnet_ferry3_ebike_overline_morethan100_clean, "export2/rnet_ferry3_ebike_overline_morethan100_clean.Rds")
saveRDS(rnet_ferry3_ebike_overline_morethan200_clean, "export2/rnet_ferry3_ebike_overline_morethan200_clean.Rds")
saveRDS(rnet_ferry3_ebike_overline_morethan50_clean, "export2/rnet_ferry3_ebike_overline_morethan50_clean.Rds")
saveRDS(rnet_ferry3_ebike_overline_morethan400_clean, "export2/rnet_ferry3_ebike_overline_morethan400_clean.Rds")
# rnet_ferry3_ebike_overline_morethan100_clean = readRDS("export2/rnet_ferry3_ebike_overline_morethan100_clean.Rds")






# for intermodality ---------------------------------------------------------------------------

## LTS4
# filter for conventional bike
table(routes_r5r_100jit_lts4__intermodALL_NoSub_elev$mode)
# 50k trips by bike
summary(routes_r5r_100jit_lts4__intermodALL_NoSub_elev$distance)

routes_allmodesNSub4_intermid = routes_r5r_100jit_lts4__intermodALL_NoSub_elev %>% st_drop_geometry() %>%
  group_by(id) %>% summarise(count =n()) %>% filter(count != 1)
table(routes_allmodesNSub4_intermid$count) #13k
routes_allmodesNSub4_filtered = routes_r5r_100jit_lts4__intermodALL_NoSub_elev %>%
  filter(id %in% routes_allmodesNSub4_intermid$id)
table(routes_allmodesNSub4_filtered$mode) #potencial não filtrado mas que tem intermodalidade
# BICYCLE     BUS   FERRY    RAIL    TRAM 
# 25496     592     273   11401     465
# 2.353k    54k     23k   1.054k    46k
# 1.176k (2  pernas da mesma viagem, first & last mile) 

# sum(routes_allmodesNSub4_filtered$Total[routes_allmodesNSub4_filtered$mode == "TRAM"])

routes_allmodesNSub4_dist = routes_allmodesNSub4_filtered %>%
  filter(mode == "BICYCLE") %>%
  st_drop_geometry() %>%
  group_by(id) %>% 
  summarise(distance = sum(distance))
summary(routes_allmodesNSub4_filtered$distance) #75% less than 4600m in total
routes_allmodesNSub4_dist = routes_allmodesNSub4_dist %>% filter(distance <= 5000) # 2.5+2.5km? #this is maybe low, but ok to show the benefits
# 5k trips by bike
routes_allmodesNSub4_filtered = routes_allmodesNSub4_filtered %>% filter(id %in% routes_allmodesNSub4_dist$id)
table(routes_allmodesNSub4_filtered$mode) #potencial FILTRADO
# BICYCLE     BUS   FERRY    RAIL    TRAM 
# 10764     171      90    4880     241
# 1.002k    16k     8k      453k    24k
# 501k (2  pernas da mesma viagem, first & last mile) 

# sum(routes_allmodesNSub4_filtered$Total[routes_allmodesNSub4_filtered$mode == "TRAM"])

routes_allmodesNSub4_filtered = routes_allmodesNSub4_filtered %>% filter(mode == "BICYCLE") #drop routes with other modes

# get potential
#cycling potential function
routes_allmodesNSub4_filtered$Bikeper = routes_allmodesNSub4_filtered$Bike / routes_allmodesNSub4_filtered$Total

routes_allmodesNSub4_filtered = routes_allmodesNSub4_filtered %>% mutate(
  cyc4 = ifelse(Bikeper >= ENMAC4, Bike, ENMAC4 * Total),
  new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
  cyc10 = ifelse(Bikeper >= ENMAC10, Bike, ENMAC10 * Total),
  new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)
)

summary(routes_allmodesNSub4_filtered$Bikeper)
summary(routes_allmodesNSub4_filtered$Bike)
summary(routes_allmodesNSub4_filtered$cyc10)
summary(routes_allmodesNSub4_filtered$new_cyc10) 
sum(routes_allmodesNSub4_filtered$Bike) #4k
sum(routes_allmodesNSub4_filtered$cyc4) #42k
sum(routes_allmodesNSub4_filtered$new_cyc4) #38k
sum(routes_allmodesNSub4_filtered$cyc10) #102k
sum(routes_allmodesNSub4_filtered$new_cyc10) #97k


rnet_allmodesNSub4_overline = routes_allmodesNSub4_filtered %>%
  stplanr::overline(c("Bike", "Total", "new_cyc4", "cyc4", "new_cyc10", "cyc10"),
                    fun = "sum") %>% 
  mutate(Bikeper = Bike / Total)
saveRDS(rnet_allmodesNSub4_overline, "routes_allmodesNSub4_overline.Rds")
saveRDS(routes_allmodesNSub4_filtered, "routes_allmodesNSub4_preoverline.Rds")


## filter segments with less than X trips?
summary(rnet_allmodesNSub4_overline$new_cyc4)

rnet_allmodesNSub4_overline_morethan10 = rnet_allmodesNSub4_overline %>% filter(cyc4 >=10)
rnet_allmodesNSub4_overline_morethan30 = rnet_allmodesNSub4_overline %>% filter(cyc4 >=30)
rnet_allmodesNSub4_overline_morethan50 = rnet_allmodesNSub4_overline %>% filter(cyc4 >=50)

#rnet clean to get rid of small pieces
#more than 30 trips in 1 segment
rnet_allmodesNSub4_overline_morethan30_clean = rnet_allmodesNSub4_overline_morethan30 %>%
  mutate(group = stplanr::rnet_group(rnet_allmodesNSub4_overline_morethan30, d = 100 ))

rnet_allmodesNSub4_overline_morethan30_group = rnet_allmodesNSub4_overline_morethan30_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_allmodesNSub4_overline_morethan30_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_allmodesNSub4_overline_morethan30_clean = rnet_allmodesNSub4_overline_morethan30_clean %>%
  filter(group %in% groupp) #more than 10 segments connected
rm(rnet_allmodesNSub4_overline_morethan30_group, group)

library(biclar)
tm_rnet(rnet_allmodesNSub4_overline_morethan30_clean,
        lwd = "cyc10", #Baseline, ENMAC4, ENMAC10
        # col = "Quietness",
        palette = "-burg", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15 #15? less thin
)

saveRDS(rnet_allmodesNSub4_overline_morethan30_clean, "export2/rnet_allmodesNSub4_overline_morethan30_clean.Rds")

#more than 10 trips in 1 segment
rnet_allmodesNSub4_overline_morethan10_clean = rnet_allmodesNSub4_overline_morethan10 %>%
  mutate(group = stplanr::rnet_group(rnet_allmodesNSub4_overline_morethan10, d = 100 )) #STOP HERE

rnet_allmodesNSub4_overline_morethan10_group = rnet_allmodesNSub4_overline_morethan10_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_allmodesNSub4_overline_morethan10_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_allmodesNSub4_overline_morethan10_clean = rnet_allmodesNSub4_overline_morethan10_clean %>%
  filter(group %in% groupp) #more than 10 segments connected
rm(rnet_allmodesNSub4_overline_morethan10_group, group)

tm_rnet(rnet_allmodesNSub4_overline_morethan10_clean,
        lwd = "Bike", #Baseline, ENMAC4, ENMAC10
        # col = "Quietness",
        palette = "-burg", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)


saveRDS(rnet_allmodesNSub4_overline_morethan10_clean, "export2/rnet_allmodesNSub4_overline_morethan10_clean.Rds")



#more than 50 trips in 1 segment
rnet_allmodesNSub4_overline_morethan50_clean = rnet_allmodesNSub4_overline_morethan50 %>%
  mutate(group = stplanr::rnet_group(rnet_allmodesNSub4_overline_morethan50, d = 100 )) #STOP HERE

rnet_allmodesNSub4_overline_morethan50_group = rnet_allmodesNSub4_overline_morethan50_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_allmodesNSub4_overline_morethan50_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_allmodesNSub4_overline_morethan50_clean = rnet_allmodesNSub4_overline_morethan50_clean %>%
  filter(group %in% groupp) #more than 50 segments connected
rm(rnet_allmodesNSub4_overline_morethan50_group, group)

tm_rnet(rnet_allmodesNSub4_overline_morethan50_clean,
        lwd = "Bike", #Baseline, ENMAC4, ENMAC10
        # col = "Quietness",
        palette = "-burg", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)


saveRDS(rnet_allmodesNSub4_overline_morethan50_clean, "export2/rnet_allmodesNSub4_overline_morethan50_clean.Rds")







## LTS3
# filter for conventional bike
table(routes_r5r_100jit_lts3__intermod_NoSub_elev$mode)
# 45k trips by bike
summary(routes_r5r_100jit_lts3__intermod_NoSub_elev$distance)

routes_allmodesNSub3_intermid = routes_r5r_100jit_lts3__intermod_NoSub_elev %>% st_drop_geometry() %>%
  group_by(id) %>% summarise(count =n()) %>% filter(count != 1)
table(routes_allmodesNSub3_intermid$count) #12k
routes_allmodesNSub3_filtered = routes_r5r_100jit_lts3__intermod_NoSub_elev %>%
  filter(id %in% routes_allmodesNSub3_intermid$id)
table(routes_allmodesNSub3_filtered$mode) #potencial não filtrado mas que tem intermodalidade
# BICYCLE     BUS   FERRY    RAIL    TRAM 
# 24316     474     208   10871     605
# 2.267k    43k     17k   1.012k    60k
# 1.133k (2  pernas da mesma viagem, first & last mile) 

# sum(routes_allmodesNSub3_filtered$Total[routes_allmodesNSub3_filtered$mode == "BICYCLE"])

routes_allmodesNSub3_dist = routes_allmodesNSub3_filtered %>%
  filter(mode == "BICYCLE") %>%
  st_drop_geometry() %>%
  group_by(id) %>% 
  summarise(distance = sum(distance))
summary(routes_allmodesNSub3_filtered$distance) #75% less than 4500m in total
routes_allmodesNSub3_dist = routes_allmodesNSub3_dist %>% filter(distance <= 5000) # 2.5+2.5km? #this is maybe low, but ok to show the benefits
# 5k trips by bike
routes_allmodesNSub3_filtered = routes_allmodesNSub3_filtered %>% filter(id %in% routes_allmodesNSub3_dist$id)
table(routes_allmodesNSub3_filtered$mode) #potencial FILTRADO
# BICYCLE     BUS   FERRY    RAIL    TRAM 
#   11462     160      82    5025     464 
# 1.077k    15k     7k      470k    46k
# 538k (2  pernas da mesma viagem, first & last mile) 

# sum(routes_allmodesNSub3_filtered$Total[routes_allmodesNSub3_filtered$mode == "BICYCLE"])

routes_allmodesNSub3_filtered = routes_allmodesNSub3_filtered %>% filter(mode == "BICYCLE") #drop routes with other modes

# get potential
#cycling potential function
routes_allmodesNSub3_filtered$Bikeper = routes_allmodesNSub3_filtered$Bike / routes_allmodesNSub3_filtered$Total

routes_allmodesNSub3_filtered = routes_allmodesNSub3_filtered %>% mutate(
  cyc4 = ifelse(Bikeper >= ENMAC4, Bike, ENMAC4 * Total),
  new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
  cyc10 = ifelse(Bikeper >= ENMAC10, Bike, ENMAC10 * Total),
  new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)
)

summary(routes_allmodesNSub3_filtered$Bikeper)
summary(routes_allmodesNSub3_filtered$Bike)
summary(routes_allmodesNSub3_filtered$cyc10)
summary(routes_allmodesNSub3_filtered$new_cyc10) 
sum(routes_allmodesNSub3_filtered$Bike) #4k
sum(routes_allmodesNSub3_filtered$cyc4) #45k
sum(routes_allmodesNSub3_filtered$new_cyc4) #40k
sum(routes_allmodesNSub3_filtered$cyc10) #109k
sum(routes_allmodesNSub3_filtered$new_cyc10) #104k


rnet_allmodesNSub3_overline = routes_allmodesNSub3_filtered %>%
  stplanr::overline(c("Bike", "Total", "new_cyc4", "cyc4", "new_cyc10", "cyc10"),
                    fun = "sum") %>% 
  mutate(Bikeper = Bike / Total)
saveRDS(rnet_allmodesNSub3_overline, "routes_allmodesNSub3_overline.Rds")
saveRDS(routes_allmodesNSub3_filtered, "routes_allmodesNSub3_preoverline.Rds")


## filter segments with less than X trips?
summary(rnet_allmodesNSub3_overline$cyc4)

rnet_allmodesNSub3_overline_morethan10 = rnet_allmodesNSub3_overline %>% filter(cyc4 >=10)
rnet_allmodesNSub3_overline_morethan30 = rnet_allmodesNSub3_overline %>% filter(cyc4 >=30)
rnet_allmodesNSub3_overline_morethan50 = rnet_allmodesNSub3_overline %>% filter(cyc4 >=50)


#rnet clean to get rid of small pieces

#more than 30 trips in 1 segment
rnet_allmodesNSub3_overline_morethan30_clean = rnet_allmodesNSub3_overline_morethan30 %>%
  mutate(group = stplanr::rnet_group(rnet_allmodesNSub3_overline_morethan30, d = 100 )) #STOP HERE

rnet_allmodesNSub3_overline_morethan30_group = rnet_allmodesNSub3_overline_morethan30_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_allmodesNSub3_overline_morethan30_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_allmodesNSub3_overline_morethan30_clean = rnet_allmodesNSub3_overline_morethan30_clean %>%
  filter(group %in% groupp) #more than 10 segments connected
rm(rnet_allmodesNSub3_overline_morethan30_group, group)

tm_rnet(rnet_allmodesNSub3_overline_morethan30_clean,
        lwd = "Bike", #Baseline, ENMAC4, ENMAC10
        # col = "Quietness",
        palette = "-burg", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)


saveRDS(rnet_allmodesNSub3_overline_morethan30_clean, "export2/rnet_allmodesNSub3_overline_morethan30_clean.Rds")


#more than 10 trips in 1 segment
rnet_allmodesNSub3_overline_morethan10_clean = rnet_allmodesNSub3_overline_morethan10 %>%
  mutate(group = stplanr::rnet_group(rnet_allmodesNSub3_overline_morethan10, d = 100 )) #STOP HERE

rnet_allmodesNSub3_overline_morethan10_group = rnet_allmodesNSub3_overline_morethan10_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_allmodesNSub3_overline_morethan10_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_allmodesNSub3_overline_morethan10_clean = rnet_allmodesNSub3_overline_morethan10_clean %>%
  filter(group %in% groupp) #more than 10 segments connected
rm(rnet_allmodesNSub3_overline_morethan10_group, group)

tm_rnet(rnet_allmodesNSub3_overline_morethan10_clean,
        lwd = "Bike", #Baseline, ENMAC4, ENMAC10
        # col = "Quietness",
        palette = "-burg", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)


saveRDS(rnet_allmodesNSub3_overline_morethan10_clean, "export2/rnet_allmodesNSub3_overline_morethan10_clean.Rds")


#more than 50 trips in 1 segment
rnet_allmodesNSub3_overline_morethan50_clean = rnet_allmodesNSub3_overline_morethan50 %>%
  mutate(group = stplanr::rnet_group(rnet_allmodesNSub3_overline_morethan50, d = 100 )) #STOP HERE

rnet_allmodesNSub3_overline_morethan50_group = rnet_allmodesNSub3_overline_morethan50_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
group = rnet_allmodesNSub3_overline_morethan50_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_allmodesNSub3_overline_morethan50_clean = rnet_allmodesNSub3_overline_morethan50_clean %>%
  filter(group %in% groupp) #more than 10 segments connected
rm(rnet_allmodesNSub3_overline_morethan50_group, group)

tm_rnet(rnet_allmodesNSub3_overline_morethan50_clean,
        lwd = "Bike", #Baseline, ENMAC4, ENMAC10
        # col = "Quietness",
        palette = "-burg", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)


saveRDS(rnet_allmodesNSub3_overline_morethan50_clean, "export2/rnet_allmodesNSub3_overline_morethan50_clean.Rds")


#### see quietness-networks.R for next steps

#### round values
#### clean "group"
#### match with quietness & speed
