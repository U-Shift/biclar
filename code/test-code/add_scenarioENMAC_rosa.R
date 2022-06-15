# scenarios applied to jittered routes

# routes_jittered = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/routes_jittered_quietest_threshold_500_max_9km_total_max_total_10.Rds"))
# routes_jittered = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/routes_jittered_fastest_threshold_500_max_9km_total_max_total_10.Rds"))
routes_jittered = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/routes_jittered_quietest_threshold_500_max_9km_total_max_total_10.Rds"))


#cycling potential function
routes_enmac = routes_jittered
ENMAC4 = 0.04 # 4%
ENMAC10 = 0.10 # 10#
routes_enmac$Bikeper = routes_enmac$Bike / routes_enmac$Total

routes_enmac$new_cyc4 = ifelse(routes_enmac$Bikeper >= ENMAC4, routes_enmac$Bike, ENMAC4 * routes_enmac$Total - routes_enmac$Bike)
routes_enmac$new_cyc10 = ifelse(routes_enmac$Bikeper >= ENMAC10, routes_enmac$Bike, ENMAC10 * routes_enmac$Total - routes_enmac$Bike)
routes_enmac$new_car4 = ifelse(routes_enmac$Bikeper >= ENMAC4, routes_enmac$Car + routes_enmac$CarP, (routes_enmac$Car + routes_enmac$CarP) - routes_enmac$new_cyc4)
routes_enmac$new_car10 = ifelse(routes_enmac$Bikeper >= ENMAC10, routes_enmac$Car + routes_enmac$CarP, (routes_enmac$Car + routes_enmac$CarP) - routes_enmac$new_cyc10)

#export
routes_jittered_ENMAC410 = routes_enmac
# saveRDS(routes_jittered_ENMAC410, "routes_jittered_ENMAC410_fastest_500.Rds")
saveRDS(routes_jittered_ENMAC410, "routes_jittered_ENMAC410_balanced_500.Rds")


