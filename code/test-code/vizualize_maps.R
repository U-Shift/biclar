# Visualise the results - experiments ------
library(tmap)
library(biclar)
rnet_enmac_region = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/rnet_enmac_region_fastest_top_20000.Rds"))
m = tm_rnet(rnet_enmac_region,
        lwd = "Baseline",
        col = "Quietness",
        palette = "-reds", # "johnson", "mako", "burg", "reds"
        scale = 15,
        lwd_multiplier = 15
        )
m

htmlwidgets::saveWidget(m, "pkgdown/assets/region_baseline_quiet.html")
