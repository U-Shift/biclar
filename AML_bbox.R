library(tidyverse)
library(sf)
class(MUNICIPIOSgeo)
AML_bb = stplanr::geo_bb(MUNICIPIOSgeo, scale_factor = 1.02) #extra 2%

st_write(AML_bb, "AML_bb.geojson")
