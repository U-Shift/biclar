# Visualise the results - experiments ------

# remotes::install_github("u-shift/biclar")

library(tmap)
library(biclar)
rnet_enmac_region = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/rnet_enmac_region_fastest_top_20000.Rds"))
rnet_enmac_region = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/rnet_enmac_region_quietest_top_20000.Rds"))

m = tm_rnet(rnet_enmac_region,
        lwd = "ENMAC10", #Baseline, ENMAC4, ENMAC10
        col = "Quietness",
        palette = "-mako", # "johnson", "mako", "burg", "reds" - reds for fastest, mako for quietest
        scale = 15,
        lwd_multiplier = 15
        )
m

htmlwidgets::saveWidget(m, "pkgdown/assets/region_baseline_fast.html")
htmlwidgets::saveWidget(m, "pkgdown/assets/region_enmac4_fast.html")
htmlwidgets::saveWidget(m, "pkgdown/assets/region_enmac10_fast.html")

htmlwidgets::saveWidget(m, "pkgdown/assets/region_baseline_quiet.html")
htmlwidgets::saveWidget(m, "pkgdown/assets/region_enmac4_quiet.html")
htmlwidgets::saveWidget(m, "pkgdown/assets/region_enmac10_quiet.html")
