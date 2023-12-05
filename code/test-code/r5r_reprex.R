# system.file("jar", package = "r5r")
# tools::R_user_dir("r5r", which = "cache")

r5r::download_r5(force_update = TRUE)
r5r::r5r_sitrep()

# sudo Rscript -e "r5r::download_r5(force_update = TRUE)"

options(java.parameters = '-Xmx120G') 
library(r5r)
data_path <- system.file("extdata/poa", package = "r5r")
r5r_core <- r5r::setup_r5(data_path = data_path)
