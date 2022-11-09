# r5r ferry filter
library(tidyverse)
library(sf)
library(stplanr)

# for cenarios 1 ------------------------------------------------------------------------------

## LTS4
# filter for conventional bike
table(routes_r5r_100jit_lts4__ferry_elev$mode)
# BICYCLE   FERRY 
# 54384    2011 
summary(routes_r5r_100jit_lts4__ferry_elev$distance)

routes_ferry4_filtered = routes_r5r_100jit_lts4__ferry_elev %>%
  filter(mode == "BICYCLE") %>%
  st_drop_geometry() %>%
  group_by(id) %>% 
  summarise(distance = distance)
summary(routes_ferry4_filtered$distance) #50% less than 5000m, 75% less than 9000m in total
# routes_ferry4_filtered = routes_ferry4_filtered %>% filter(distance <= 9000)
routes_ferry4_filtered = routes_ferry4_filtered %>% filter(distance <= 5000) #fazer até 5km para os cenarios 4 e 10


routes_ferry4_filtered = routes_r5r_100jit_lts4__ferry_elev %>% filter(id %in% routes_ferry4_filtered$id)
table(routes_ferry4_filtered$mode)
# BICYCLE   FERRY 
# 28197    1441 
# 2.410k    117k
# 2.233k (2  pernas da mesma viagem, first & last mile) 

# sum(routes_ferry4_filtered$Total[routes_ferry4_filtered$mode == "FERRY"])


# get potential
#cycling potential function
ENMAC4 = 0.04 # 4%
ENMAC10 = 0.10 # 10#
routes_ferry4_filtered$Bikeper = routes_ferry4_filtered$Bike / routes_ferry4_filtered$Total

routes_ferry4_filtered = routes_ferry4_filtered %>% mutate(
  cyc4 = ifelse(Bikeper >= 4, Bike, ENMAC4 * Total),
  new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
  cyc10 = ifelse(Bikeper >= 10, Bike, ENMAC10 * Total),
  new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)
)

summary(routes_ferry4_filtered$cyc4)
summary(routes_ferry4_filtered$new_cyc4) 
sum(routes_ferry4_filtered$Bike) #15k
sum(routes_ferry4_filtered$cyc4) #114k
sum(routes_ferry4_filtered$new_cyc4) #102k
sum(routes_ferry4_filtered$cyc10) #286k
sum(routes_ferry4_filtered$new_cyc10) #272k

rnet_ferry4_overline = routes_ferry4_filtered %>%
  stplanr::overline(c("Bike", "Total", "new_cyc4", "cyc4", "new_cyc10", "cyc10"),
                    fun = "sum") %>% 
  mutate(Bikeper = Bike / Total)
saveRDS(rnet_ferry4_overline, "routes_ferry4_overline.Rds")
saveRDS(routes_ferry4_filtered, "routes_ferry4_preoverline.Rds")

## filter segments with less than X trips?
summary(rnet_ferry4_overline$new_cyc4)

rnet_ferry4_overline_morethan10 = rnet_ferry4_overline %>% filter(cyc4 >=10)
rnet_ferry4_overline_morethan100 = rnet_ferry4_overline %>% filter(cyc4 >=100)

summary(rnet_ferry4_overline_morethan10$new_cyc4)
summary(rnet_ferry4_overline_morethan10$cyc4)
summary(rnet_ferry4_overline_morethan100$new_cyc4)
summary(rnet_ferry4_overline_morethan100$cyc4)

#rnet clean to get rid of small pieces - 100
rnet_ferry4_overline_morethan100_clean = rnet_ferry4_overline_morethan100 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry4_overline_morethan100, d = 100 ))
summary(rnet_ferry4_overline_morethan100_clean$group)

rnet_ferry4_overline_morethan100_group = rnet_ferry4_overline_morethan100_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
summary(rnet_ferry4_overline_morethan100_group$count)
group = rnet_ferry4_overline_morethan100_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_ferry4_overline_morethan100_clean = rnet_ferry4_overline_morethan100_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry4_overline_morethan100_group)


# #rnet clean to get rid of small pieces - 10
# rnet_ferry4_overline_morethan10_clean = rnet_ferry4_overline_morethan10 %>%
#   mutate(group = stplanr::rnet_group(rnet_ferry4_overline_morethan10, d = 100 ))
# summary(rnet_ferry4_overline_morethan10_clean$group)
# 
# rnet_ferry4_overline_morethan10_group = rnet_ferry4_overline_morethan10_clean %>%
#   st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
# summary(rnet_ferry4_overline_morethan10_group$count)
# group = rnet_ferry4_overline_morethan10_group %>% filter(count >=5) #discard networks with less than 5 semnets?
# groupp = group$group
# 
# rnet_ferry4_overline_morethan10_clean = rnet_ferry4_overline_morethan10_clean %>%
#   filter(group %in% groupp) #more than 5 segments connected
# rm(rnet_ferry4_overline_morethan10_group)


library(biclar)
tm_rnet(rnet_ferry4_overline_morethan100_clean,
            lwd = "cyc4", #Baseline, ENMAC4, ENMAC10
            # col = "Quietness",
            palette = "-linear_yl_rd_bk", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
            scale = 15,
            lwd_multiplier = 15
)

saveRDS(rnet_ferry4_overline_morethan100_clean, "export/rnet_ferry4_overline_morethan100_clean.Rds")
# saveRDS(rnet_ferry4_overline_morethan10_clean, "export/rnet_ferry4_overline_morethan10_clean.Rds")
# rnet_ferry4_overline_morethan100_clean = readRDS("export/rnet_ferry4_overline_morethan100_clean.Rds")

# Run also for LTS3, and mode egress = walk







## LTS2
# filter for conventional bike
table(routes_r5r_100jit_lts2__ferry_elev$mode)
# BICYCLE   FERRY 
# 43731    899 
summary(routes_r5r_100jit_lts2__ferry_elev$distance)


routes_ferry2_filtered = routes_r5r_100jit_lts2__ferry_elev %>%
  filter(mode == "BICYCLE") %>%
  st_drop_geometry() %>%
  group_by(id) %>% 
  summarise(distance = distance)
summary(routes_ferry2_filtered$distance) ##50% less than 4500m, 75% less than 7700m in total
routes_ferry2_filtered = routes_ferry2_filtered %>% filter(distance <= 5000) #fazer até 5km para os cenarios 4 e 10


routes_ferry2_filtered = routes_r5r_100jit_lts2__ferry_elev %>% filter(id %in% routes_ferry2_filtered$id)
table(routes_ferry2_filtered$mode)
# BICYCLE   FERRY 
# 24431    812 
# 2.398k    69k
# 2.329k (2  pernas da mesma viagem, first & last mile) 

# sum(routes_ferry2_filtered$Total[routes_ferry2_filtered$mode == "BICYCLE"])


# get potential
#cycling potential function
ENMAC4 = 0.04 # 4%
ENMAC10 = 0.10 # 10#
routes_ferry2_filtered$Bikeper = routes_ferry2_filtered$Bike / routes_ferry2_filtered$Total

routes_ferry2_filtered = routes_ferry2_filtered %>% mutate(
  cyc4 = ifelse(Bikeper >= 4, Bike, ENMAC4 * Total),
  new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
  cyc10 = ifelse(Bikeper >= 10, Bike, ENMAC10 * Total),
  new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)
)

summary(routes_ferry2_filtered$cyc4)
summary(routes_ferry2_filtered$new_cyc4) 
sum(routes_ferry2_filtered$Bike) #13k
sum(routes_ferry2_filtered$cyc4) #99k
sum(routes_ferry2_filtered$new_cyc4) #88k
sum(routes_ferry2_filtered$cyc10) #247k
sum(routes_ferry2_filtered$new_cyc10) #235k


rnet_ferry2_overline = routes_ferry2_filtered %>%
  stplanr::overline(c("Bike", "Total", "new_cyc4", "cyc4", "new_cyc10", "cyc10"),
                    fun = "sum") %>% 
  mutate(Bikeper = Bike / Total)
saveRDS(rnet_ferry2_overline, "routes_ferry2_overline.Rds")
saveRDS(routes_ferry2_filtered, "routes_ferry2_preoverline.Rds")

## filter segments with less than X trips?
summary(rnet_ferry2_overline$new_cyc4)

rnet_ferry2_overline_morethan10 = rnet_ferry2_overline %>% filter(cyc4 >=10)
rnet_ferry2_overline_morethan100 = rnet_ferry2_overline %>% filter(cyc4 >=100)

summary(rnet_ferry2_overline_morethan10$new_cyc4)
summary(rnet_ferry2_overline_morethan10$cyc4)
summary(rnet_ferry2_overline_morethan100$new_cyc4)
summary(rnet_ferry2_overline_morethan100$cyc4)

#rnet clean to get rid of small pieces - 100
rnet_ferry2_overline_morethan100_clean = rnet_ferry2_overline_morethan100 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry2_overline_morethan100, d = 100 ))
summary(rnet_ferry2_overline_morethan100_clean$group)

rnet_ferry2_overline_morethan100_group = rnet_ferry2_overline_morethan100_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
summary(rnet_ferry2_overline_morethan100_group$count)
group = rnet_ferry2_overline_morethan100_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_ferry2_overline_morethan100_clean = rnet_ferry2_overline_morethan100_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry2_overline_morethan100_group, group)


# # #rnet clean to get rid of small pieces - 10
# rnet_ferry2_overline_morethan10_clean = rnet_ferry2_overline_morethan10 %>%
#   mutate(group = stplanr::rnet_group(rnet_ferry2_overline_morethan10, d = 100 ))
# summary(rnet_ferry2_overline_morethan10_clean$group)
# 
# rnet_ferry2_overline_morethan10_group = rnet_ferry2_overline_morethan10_clean %>%
#   st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
# summary(rnet_ferry2_overline_morethan10_group$count)
# group = rnet_ferry2_overline_morethan10_group %>% filter(count >=3) #discard networks with less than 3 semnets?
# groupp = group$group
# 
# rnet_ferry2_overline_morethan10_clean = rnet_ferry2_overline_morethan10_clean %>%
#   filter(group %in% groupp) #more than 5 segments connected
# rm(rnet_ferry2_overline_morethan10_group, group)


library(biclar)
tm_rnet(rnet_ferry2_overline_morethan100_clean,
        lwd = "Bike", #Baseline, ENMAC4, ENMAC10
        # col = "Quietness",
        palette = "-reds", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)

saveRDS(rnet_ferry2_overline_morethan100_clean, "export/rnet_ferry2_overline_morethan100_clean.Rds")
saveRDS(rnet_ferry2_overline_morethan10_clean, "export/rnet_ferry2_overline_morethan10_clean.Rds")
# rnet_ferry2_overline_morethan100_clean = readRDS("export/rnet_ferry2_overline_morethan100_clean.Rds")


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
  summarise(distance = distance)
summary(routes_ferry4_ebike_filtered$distance) #50% less than 5000m, 75% less than 9000m in total
routes_ferry4_ebike_filtered = routes_ferry4_ebike_filtered %>% filter(distance <= 10000) #fazer até 10km para os cenarios ebike


routes_ferry4_ebike_filtered = routes_r5r_100jit_lts4__ferry_elev %>% filter(id %in% routes_ferry4_ebike_filtered$id)
table(routes_ferry4_ebike_filtered$mode)
# BICYCLE   FERRY 
# 43388    2011 
# 4.182k    162k

# sum(routes_ferry4_ebike_filtered$Total[routes_ferry4_ebike_filtered$mode == "BICYCLE"])


# get potential
#cycling potential function
ENMAC4 = 0.04 # 4%
ENMAC10 = 0.10 # 10#
routes_ferry4_ebike_filtered$Bikeper = routes_ferry4_ebike_filtered$Bike / routes_ferry4_ebike_filtered$Total

routes_ferry4_ebike_filtered = routes_ferry4_ebike_filtered %>% mutate(
  cyc4 = ifelse(Bikeper >= 4, Bike, ENMAC4 * Total),
  new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
  cyc10 = ifelse(Bikeper >= 10, Bike, ENMAC10 * Total),
  new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)
)

summary(routes_ferry4_ebike_filtered$cyc4)
summary(routes_ferry4_ebike_filtered$new_cyc4) 
sum(routes_ferry4_ebike_filtered$Bike) #22k
sum(routes_ferry4_ebike_filtered$cyc4) #174k
sum(routes_ferry4_ebike_filtered$new_cyc4) #156k
sum(routes_ferry4_ebike_filtered$cyc10) #435k
sum(routes_ferry4_ebike_filtered$new_cyc10) #414k

rnet_ferry4_ebike_overline = routes_ferry4_ebike_filtered %>%
  stplanr::overline(c("Bike", "Total", "new_cyc4", "cyc4", "new_cyc10", "cyc10"), #vale a pena manter ambos 4% 4 10% ?
                    fun = "sum") %>% 
  mutate(Bikeper = Bike / Total)
saveRDS(rnet_ferry4_ebike_overline, "routes_ferry4_ebike_overline.Rds")
saveRDS(routes_ferry4_ebike_filtered, "routes_ferry4_ebike_preoverline.Rds")

## filter segments with less than X trips?
summary(rnet_ferry4_ebike_overline$new_cyc4)

rnet_ferry4_ebike_overline_morethan10 = rnet_ferry4_ebike_overline %>% filter(cyc4 >=10)
rnet_ferry4_ebike_overline_morethan100 = rnet_ferry4_ebike_overline %>% filter(cyc4 >=100)

summary(rnet_ferry4_ebike_overline_morethan10$new_cyc4)
summary(rnet_ferry4_ebike_overline_morethan10$cyc4)
summary(rnet_ferry4_ebike_overline_morethan100$new_cyc4)
summary(rnet_ferry4_ebike_overline_morethan100$cyc4)

#rnet clean to get rid of small pieces - 100
rnet_ferry4_ebike_overline_morethan100_clean = rnet_ferry4_ebike_overline_morethan100 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry4_ebike_overline_morethan100, d = 100 ))
summary(rnet_ferry4_ebike_overline_morethan100_clean$group)

rnet_ferry4_ebike_overline_morethan100_group = rnet_ferry4_ebike_overline_morethan100_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
summary(rnet_ferry4_ebike_overline_morethan100_group$count)
group = rnet_ferry4_ebike_overline_morethan100_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_ferry4_ebike_overline_morethan100_clean = rnet_ferry4_ebike_overline_morethan100_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry4_ebike_overline_morethan100_group)


# #rnet clean to get rid of small pieces - 10
# rnet_ferry4_ebike_overline_morethan10_clean = rnet_ferry4_ebike_overline_morethan10 %>%
#   mutate(group = stplanr::rnet_group(rnet_ferry4_ebike_overline_morethan10, d = 100 ))
# summary(rnet_ferry4_ebike_overline_morethan10_clean$group)
# 
# rnet_ferry4_ebike_overline_morethan10_group = rnet_ferry4_ebike_overline_morethan10_clean %>%
#   st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
# summary(rnet_ferry4_ebike_overline_morethan10_group$count)
# group = rnet_ferry4_ebike_overline_morethan10_group %>% filter(count >=5) #discard networks with less than 5 semnets?
# groupp = group$group
# 
# rnet_ferry4_ebike_overline_morethan10_clean = rnet_ferry4_ebike_overline_morethan10_clean %>%
#   filter(group %in% groupp) #more than 5 segments connected
# rm(rnet_ferry4_ebike_overline_morethan10_group)


library(biclar)
tm_rnet(rnet_ferry4_ebike_overline_morethan100_clean,
        lwd = "cyc4", #Baseline, ENMAC4, ENMAC10
        # col = "Quietness",
        palette = "-linear_yl_rd_bk", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)

saveRDS(rnet_ferry4_ebike_overline_morethan100_clean, "export/rnet_ferry4_ebike_overline_morethan100_clean.Rds")
# saveRDS(rnet_ferry4_ebike_overline_morethan10_clean, "export/rnet_ferry4_ebike_overline_morethan10_clean.Rds")
# rnet_ferry4_ebike_overline_morethan100_clean = readRDS("export/rnet_ferry4_ebike_overline_morethan100_clean.Rds")

# Run also for LTS3, and mode egress = walk




## LTS2
# filter for ebike bike
table(routes_r5r_100jit_lts2__ferry_elev$mode)
# BICYCLE   FERRY 
# 43731    899 
summary(routes_r5r_100jit_lts2__ferry_elev$distance)


routes_ferry2_ebike_filtered = routes_r5r_100jit_lts2__ferry_elev %>%
  filter(mode == "BICYCLE") %>%
  st_drop_geometry() %>%
  group_by(id) %>% 
  summarise(distance = distance)
summary(routes_ferry2_ebike_filtered$distance) ##50% less than 4500m, 75% less than 7700m in total
routes_ferry2_ebike_filtered = routes_ferry2_ebike_filtered %>% filter(distance <= 10000) #fazer até 5km para os cenarios 4 e 10


routes_ferry2_ebike_filtered = routes_r5r_100jit_lts2__ferry_elev %>% filter(id %in% routes_ferry2_ebike_filtered$id)
table(routes_ferry2_ebike_filtered$mode)
# BICYCLE   FERRY 
# 37674    899 
# 3.679k    76k

# sum(routes_ferry2_ebike_filtered$Total[routes_ferry2_ebike_filtered$mode == "FERRY"])


# get potential
#cycling potential function
ENMAC4 = 0.04 # 4%
ENMAC10 = 0.10 # 10#
routes_ferry2_ebike_filtered$Bikeper = routes_ferry2_ebike_filtered$Bike / routes_ferry2_ebike_filtered$Total

routes_ferry2_ebike_filtered = routes_ferry2_ebike_filtered %>% mutate(
  cyc4 = ifelse(Bikeper >= 4, Bike, ENMAC4 * Total),
  new_cyc4 = ifelse(Bikeper >= ENMAC4, 0, cyc4 - Bike),
  cyc10 = ifelse(Bikeper >= 10, Bike, ENMAC10 * Total),
  new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)
)

summary(routes_ferry2_ebike_filtered$cyc4)
summary(routes_ferry2_ebike_filtered$new_cyc4) 
sum(routes_ferry2_ebike_filtered$Bike) #19k
sum(routes_ferry2_ebike_filtered$cyc4) #150k
sum(routes_ferry2_ebike_filtered$new_cyc4) #133k
sum(routes_ferry2_ebike_filtered$cyc10) #375k
sum(routes_ferry2_ebike_filtered$new_cyc10) #357k


rnet_ferry2_ebike_overline = routes_ferry2_ebike_filtered %>%
  stplanr::overline(c("Bike", "Total", "new_cyc4", "cyc4", "new_cyc10", "cyc10"),
                    fun = "sum") %>% 
  mutate(Bikeper = Bike / Total)
saveRDS(rnet_ferry2_ebike_overline, "routes_ferry2_ebike_overline.Rds")
saveRDS(routes_ferry2_ebike_filtered, "routes_ferry2_ebike_preoverline.Rds")

## filter segments with less than X trips?
summary(rnet_ferry2_ebike_overline$new_cyc4)

rnet_ferry2_ebike_overline_morethan10 = rnet_ferry2_ebike_overline %>% filter(cyc4 >=10)
rnet_ferry2_ebike_overline_morethan100 = rnet_ferry2_ebike_overline %>% filter(cyc4 >=100)

summary(rnet_ferry2_ebike_overline_morethan10$new_cyc4)
summary(rnet_ferry2_ebike_overline_morethan10$cyc4)
summary(rnet_ferry2_ebike_overline_morethan100$new_cyc4)
summary(rnet_ferry2_ebike_overline_morethan100$cyc4)

#rnet clean to get rid of small pieces - 100
rnet_ferry2_ebike_overline_morethan100_clean = rnet_ferry2_ebike_overline_morethan100 %>%
  mutate(group = stplanr::rnet_group(rnet_ferry2_ebike_overline_morethan100, d = 100 ))
summary(rnet_ferry2_ebike_overline_morethan100_clean$group)

rnet_ferry2_ebike_overline_morethan100_group = rnet_ferry2_ebike_overline_morethan100_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
summary(rnet_ferry2_ebike_overline_morethan100_group$count)
group = rnet_ferry2_ebike_overline_morethan100_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_ferry2_ebike_overline_morethan100_clean = rnet_ferry2_ebike_overline_morethan100_clean %>%
  filter(group %in% groupp) #more than 5 segments connected
rm(rnet_ferry2_ebike_overline_morethan100_group, group)


# # #rnet clean to get rid of small pieces - 10
# rnet_ferry2_ebike_overline_morethan10_clean = rnet_ferry2_ebike_overline_morethan10 %>%
#   mutate(group = stplanr::rnet_group(rnet_ferry2_ebike_overline_morethan10, d = 100 ))
# summary(rnet_ferry2_ebike_overline_morethan10_clean$group)
# 
# rnet_ferry2_ebike_overline_morethan10_group = rnet_ferry2_ebike_overline_morethan10_clean %>%
#   st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
# summary(rnet_ferry2_ebike_overline_morethan10_group$count)
# group = rnet_ferry2_ebike_overline_morethan10_group %>% filter(count >=3) #discard networks with less than 3 semnets?
# groupp = group$group
# 
# rnet_ferry2_ebike_overline_morethan10_clean = rnet_ferry2_ebike_overline_morethan10_clean %>%
#   filter(group %in% groupp) #more than 5 segments connected
# rm(rnet_ferry2_ebike_overline_morethan10_group, group)


library(biclar)
tm_rnet(rnet_ferry2_ebike_overline_morethan100_clean,
        lwd = "Bike", #Baseline, ENMAC4, ENMAC10
        # col = "Quietness",
        palette = "-reds", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)

saveRDS(rnet_ferry2_ebike_overline_morethan100_clean, "export/rnet_ferry2_ebike_overline_morethan100_clean.Rds")
saveRDS(rnet_ferry2_ebike_overline_morethan10_clean, "export/rnet_ferry2_ebike_overline_morethan10_clean.Rds")
# rnet_ferry2_ebike_overline_morethan100_clean = readRDS("export/rnet_ferry2_ebike_overline_morethan100_clean.Rds")




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

colnames(routes_allmodesNSub4_filtered)
routes_allmodesNSub4_filtered = routes_allmodesNSub4_filtered %>% filter(mode == "BICYCLE") #drop routes with other modes

# get potential
#cycling potential function
target10 = 0.10 # 10% of bike+transit, replacing car trips
routes_allmodesNSub4_filtered$Bikeper = routes_allmodesNSub4_filtered$Bike / routes_allmodesNSub4_filtered$Total

routes_allmodesNSub4_filtered = routes_allmodesNSub4_filtered %>% mutate(
  cyc10 = ifelse(Bikeper >= 10, Bike, ENMAC10 * Total),
  new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)
)

summary(routes_allmodesNSub4_filtered$Bike)
summary(routes_allmodesNSub4_filtered$cyc10)
summary(routes_allmodesNSub4_filtered$new_cyc10) 
sum(routes_allmodesNSub4_filtered$Bike) #4k
sum(routes_allmodesNSub4_filtered$cyc10) #100k
sum(routes_allmodesNSub4_filtered$new_cyc10) #97k


routes_allmodesNSub4_filtered[1,]
routes_allmodesNSub4_filtered[3000,]

rnet_allmodesNSub4_overline = routes_allmodesNSub4_filtered %>%
  stplanr::overline(c("Bike", "Total", "new_cyc10", "cyc10"),
                    fun = "sum") %>% 
  mutate(Bikeper = Bike / Total)
saveRDS(rnet_allmodesNSub4_overline, "routes_allmodesNSub4_overline.Rds")


## filter segments with less than X trips?
summary(rnet_allmodesNSub4_overline$new_cyc10)

rnet_allmodesNSub4_overline_morethan30 = rnet_allmodesNSub4_overline %>% filter(cyc10 >=30)
rnet_allmodesNSub4_overline_morethan100 = rnet_allmodesNSub4_overline %>% filter(cyc10 >=100)

summary(rnet_allmodesNSub4_overline_morethan30$new_cyc10)
summary(rnet_allmodesNSub4_overline_morethan30$cyc10)
summary(rnet_allmodesNSub4_overline_morethan100$new_cyc10)
summary(rnet_allmodesNSub4_overline_morethan100$cyc10)

#rnet clean to get rid of small pieces
#more than 100 trips in 1 segment
rnet_allmodesNSub4_overline_morethan100_clean = rnet_allmodesNSub4_overline_morethan100 %>%
  mutate(group = stplanr::rnet_group(rnet_allmodesNSub4_overline_morethan100, d = 100 )) #STOP HERE
summary(rnet_allmodesNSub4_overline_morethan100_clean$group)

rnet_allmodesNSub4_overline_morethan100_group = rnet_allmodesNSub4_overline_morethan100_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
summary(rnet_allmodesNSub4_overline_morethan100_group$count)
group = rnet_allmodesNSub4_overline_morethan100_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_allmodesNSub4_overline_morethan100_clean = rnet_allmodesNSub4_overline_morethan100_clean %>%
  filter(group %in% groupp) #more than 10 segments connected
rm(rnet_allmodesNSub4_overline_morethan100_group, group)

library(biclar)
tm_rnet(rnet_allmodesNSub4_overline_morethan100_clean,
        lwd = "cyc10", #Baseline, ENMAC4, ENMAC10
        # col = "Quietness",
        palette = "-burg", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15 #15? less thin
)

saveRDS(rnet_allmodesNSub4_overline_morethan100_clean, "export/rnet_allmodesNSub4_overline_morethan100_clean.Rds")

#more than 30 trips in 1 segment
rnet_allmodesNSub4_overline_morethan30_clean = rnet_allmodesNSub4_overline_morethan30 %>%
  mutate(group = stplanr::rnet_group(rnet_allmodesNSub4_overline_morethan30, d = 100 )) #STOP HERE
summary(rnet_allmodesNSub4_overline_morethan30_clean$group)

rnet_allmodesNSub4_overline_morethan30_group = rnet_allmodesNSub4_overline_morethan30_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
summary(rnet_allmodesNSub4_overline_morethan30_group$count)
group = rnet_allmodesNSub4_overline_morethan30_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_allmodesNSub4_overline_morethan30_clean = rnet_allmodesNSub4_overline_morethan30_clean %>%
  filter(group %in% groupp) #more than 10 segments connected
rm(rnet_allmodesNSub4_overline_morethan30_group, group)

tm_rnet(rnet_allmodesNSub4_overline_morethan30_clean,
        lwd = "Bike", #Baseline, ENMAC4, ENMAC10
        # col = "Quietness",
        palette = "-burg", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)


saveRDS(rnet_allmodesNSub4_overline_morethan30_clean, "export/rnet_allmodesNSub4_overline_morethan30_clean.Rds")






## LTS2
# filter for conventional bike
table(routes_r5r_100jit_lts2__intermodALL_NoSub_elev$mode)
# 32k trips by bike
summary(routes_r5r_100jit_lts2__intermodALL_NoSub_elev$distance)

routes_allmodesNSub2_intermid = routes_r5r_100jit_lts2__intermodALL_NoSub_elev %>% st_drop_geometry() %>%
  group_by(id) %>% summarise(count =n()) %>% filter(count != 1)
table(routes_allmodesNSub2_intermid$count) #only 9k intermodal
routes_allmodesNSub2_filtered = routes_r5r_100jit_lts2__intermodALL_NoSub_elev %>%
  filter(id %in% routes_allmodesNSub2_intermid$id)
table(routes_allmodesNSub2_filtered$mode)
# BICYCLE     BUS   FERRY    RAIL    TRAM 
#   18018     288      82    8086     553 


routes_allmodesNSub2_dist = routes_allmodesNSub2_filtered %>%
  filter(mode == "BICYCLE") %>%
  st_drop_geometry() %>%
  group_by(id) %>% 
  summarise(distance = sum(distance))
summary(routes_allmodesNSub2_filtered$distance) #75% less than 3500m in total
routes_allmodesNSub2_dist = routes_allmodesNSub2_dist %>% filter(distance <= 5000) # 2.5+2.5km?
# 18k trips by bike
routes_allmodesNSub2_filtered = routes_allmodesNSub2_filtered %>% filter(id %in% routes_allmodesNSub2_dist$id)
table(routes_allmodesNSub2_filtered$mode)
# BICYCLE     BUS   FERRY    RAIL    TRAM 
# 14230     224      60    6286     545 


colnames(routes_allmodesNSub2_filtered)
routes_allmodesNSub2_filtered = routes_allmodesNSub2_filtered %>% filter(mode == "BICYCLE") #drop routes with other modes

# get potential
#cycling potential function
target10 = 0.10 # 10% of bike+transit, replacing car trips
routes_allmodesNSub2_filtered$Bikeper = routes_allmodesNSub2_filtered$Bike / routes_allmodesNSub2_filtered$Total

routes_allmodesNSub2_filtered = routes_allmodesNSub2_filtered %>% mutate(
  cyc10 = ifelse(Bikeper >= 10, Bike, ENMAC10 * Total),
  new_cyc10 = ifelse(Bikeper >= ENMAC10, 0, cyc10 - Bike)
)

summary(routes_allmodesNSub2_filtered$Bike)
summary(routes_allmodesNSub2_filtered$cyc10)
summary(routes_allmodesNSub2_filtered$new_cyc10) 
sum(routes_allmodesNSub2_filtered$Bike) #6k
sum(routes_allmodesNSub2_filtered$cyc10) #135k
sum(routes_allmodesNSub2_filtered$new_cyc10) #131k


routes_allmodesNSub2_filtered[1,]
routes_allmodesNSub2_filtered[3000,]

rnet_allmodesNSub2_overline = routes_allmodesNSub2_filtered %>%
  stplanr::overline(c("Bike", "Total", "new_cyc10", "cyc10"),
                    fun = "sum") %>% 
  mutate(Bikeper = Bike / Total)
saveRDS(rnet_allmodesNSub2_overline, "routes_allmodesNSub2_overline.Rds")


## filter segments with less than X trips?
summary(rnet_allmodesNSub2_overline$new_cyc10)

rnet_allmodesNSub2_overline_morethan50 = rnet_allmodesNSub2_overline %>% filter(cyc10 >=50)
rnet_allmodesNSub2_overline_morethan100 = rnet_allmodesNSub2_overline %>% filter(cyc10 >=100)

summary(rnet_allmodesNSub2_overline_morethan50$new_cyc10)
summary(rnet_allmodesNSub2_overline_morethan50$cyc10)
summary(rnet_allmodesNSub2_overline_morethan100$new_cyc10)
summary(rnet_allmodesNSub2_overline_morethan100$cyc10)

#rnet clean to get rid of small pieces
#more than 100 trips in 1 segment
rnet_allmodesNSub2_overline_morethan100_clean = rnet_allmodesNSub2_overline_morethan100 %>%
  mutate(group = stplanr::rnet_group(rnet_allmodesNSub2_overline_morethan100, d = 100 )) #STOP HERE
summary(rnet_allmodesNSub2_overline_morethan100_clean$group)

rnet_allmodesNSub2_overline_morethan100_group = rnet_allmodesNSub2_overline_morethan100_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
summary(rnet_allmodesNSub2_overline_morethan100_group$count)
group = rnet_allmodesNSub2_overline_morethan100_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_allmodesNSub2_overline_morethan100_clean = rnet_allmodesNSub2_overline_morethan100_clean %>%
  filter(group %in% groupp) #more than 10 segments connected
rm(rnet_allmodesNSub2_overline_morethan100_group, group)

library(biclar)
tm_rnet(rnet_allmodesNSub2_overline_morethan100_clean,
        lwd = "Bike", #Baseline, ENMAC4, ENMAC10
        # col = "Quietness",
        palette = "-burg", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)

saveRDS(rnet_allmodesNSub2_overline_morethan100_clean, "export/rnet_allmodesNSub2_overline_morethan100_clean.Rds")

#more than 50 trips in 1 segment
rnet_allmodesNSub2_overline_morethan50_clean = rnet_allmodesNSub2_overline_morethan50 %>%
  mutate(group = stplanr::rnet_group(rnet_allmodesNSub2_overline_morethan50, d = 100 )) #STOP HERE
summary(rnet_allmodesNSub2_overline_morethan50_clean$group)

rnet_allmodesNSub2_overline_morethan50_group = rnet_allmodesNSub2_overline_morethan50_clean %>%
  st_drop_geometry() %>% group_by(group) %>% summarise(count = n())
summary(rnet_allmodesNSub2_overline_morethan50_group$count)
group = rnet_allmodesNSub2_overline_morethan50_group %>% filter(count >=5) #discard networks with less than 5 semnets?
groupp = group$group

rnet_allmodesNSub2_overline_morethan50_clean = rnet_allmodesNSub2_overline_morethan50_clean %>%
  filter(group %in% groupp) #more than 10 segments connected
rm(rnet_allmodesNSub2_overline_morethan50_group, group)

tm_rnet(rnet_allmodesNSub2_overline_morethan50_clean,
        lwd = "Bike", #Baseline, ENMAC4, ENMAC10
        # col = "Quietness",
        palette = "-burg", # "linear_yl_rd_bk" "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
)


saveRDS(rnet_allmodesNSub2_overline_morethan50_clean, "export/rnet_allmodesNSub2_overline_morethan50_clean.Rds")




#### round values
#### clean "group"
#### match with quietness & speed
