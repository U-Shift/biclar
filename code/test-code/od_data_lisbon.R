# Data only Lisbon for od2net test

library(dplyr)

# od data
TRIPSmode_Lisbon = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/TRIPSmode_freguesias.Rds"))
TRIPSmode_Lisbon = TRIPSmode_Lisbon |>
  mutate(DICOFREor11 = as.character(DICOFREor11),
         DICOFREde11 = as.character(DICOFREde11)) |> 
  filter(DICOFREor11 >= "110600", DICOFREor11 < "110700") |> 
  filter(DICOFREde11 >= "110600", DICOFREde11 < "110700") 

saveRDS(TRIPSmode_Lisbon, "TRIPSmode_Lisbon.Rds")
piggyback::pb_upload("TRIPSmode_Lisbon.Rds")

# zones data
ZONESgeo_Lisbon = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/FREGUESIASgeo.Rds"))
ZONESgeo_Lisbon = ZONESgeo_Lisbon |>
  filter(Concelho == "Lisboa")

saveRDS(ZONESgeo_Lisbon, "ZONESgeo_Lisbon.Rds")
piggyback::pb_upload("ZONESgeo_Lisbon.Rds")
