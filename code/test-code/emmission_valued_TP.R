# HEAT cenario 3 - only the TP leg


CAR_TP_TRANSF_results <- readxl::read_excel("HEAT/CAR_TP_TRANSF_results.xlsm", sheet = "Values", range = "A1:N5")

# join with HEAT AML redux

HEAT_aml_results = HEAT_aml_redux %>%
  left_join(CAR_TP_TRANSF_results %>% select(Code, CO2eq_TP, CO_PT, PM10_PT, NOx_PT, VOC_PT, money_emissions_eur))


saveRDS(HEAT_aml_results, "HEAT/HEAT_aml_results.Rds")





# verificar -----------------------------------------------------------------------------------

# sum(HEAT_bike$distance*HEAT_bike$new_cyc)
# dist_bike_0km= sum(HEAT_bike$distance*HEAT_bike$new_cyc)/1000 #148407.1 km/dia
# 
# HEAT$CO2eq #5919 ton / ano
# # 5919*1000000/365 = 16216438 g/dia
# 
# 16216438 / dist_bike_0km = 109.27 g/km 
# 
# #está certo!
