# colors scenarios and quietness levels


# test map

rnet_test = sf::st_read("export_rnet/rnet_aml_3410_10.gpkg")
biclar::tm_rnet(rnet_test[MUNICIPIOSgeo %>% filter(Concelho == "Barreiro"), , op = sf::st_within],
                lwd = "cyc", #Baseline, ENMAC4, ENMAC10
                col = "quietness",
                palette = "-yl_gn_bu", # "linear_yl_rd_bk", "mako", "burg", "reds" - reds for fastest, mako for quietest
                scale = 15, #30 para lisboa?
                lwd_multiplier = 15)



rnet_test = sf::st_read("export_rnet/rnet_aml_1410_50.gpkg")
biclar::tm_rnet(rnet_test,
                lwd = "cyc", #Baseline, ENMAC4, ENMAC10
                col = "quietness",
                palette = "-yl_gn_bu", # "linear_yl_rd_bk", "mako", "burg", "reds" - reds for fastest, mako for quietest
                scale = 15, #30 para lisboa?
                lwd_multiplier = 15)
