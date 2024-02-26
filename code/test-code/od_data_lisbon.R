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


TRIPSmode_Lisbon = TRIPSmode_Lisbon |>
  sf::st_drop_geometry() |> 
  select(DICOFREor11, DICOFREde11, Total) |>
  rename(from = DICOFREor11, to = DICOFREde11, count = Total) |> 
  mutate(count = round(count))

write.csv(TRIPSmode_Lisbon, "od.csv", row.names = FALSE)
piggyback::pb_upload("od.csv")

# zones data
ZONESgeo_Lisbon = readRDS(url("https://github.com/U-Shift/biclar/releases/download/0.0.1/FREGUESIASgeo.Rds"))
ZONESgeo_Lisbon = ZONESgeo_Lisbon |>
  filter(Concelho == "Lisboa") |> 
  select(Dicofre, geom) |> 
  rename(name = Dicofre)

saveRDS(ZONESgeo_Lisbon, "ZONESgeo_Lisbon.Rds")
piggyback::pb_upload("ZONESgeo_Lisbon.Rds")
sf::st_write(ZONESgeo_Lisbon, "zones.geojson")
piggyback::pb_upload("zones.geojson")


