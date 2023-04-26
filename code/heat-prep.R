# prepare tables for HEAT SE estimations
library(tidyverse)
library(sf)


# locations id
HEAT_all_locations_data = read.csv("HEAT/all_locations_data.csv") #data from https://github.com/HEAT-WHO/HEAT_data/blob/main/data/heatr/locations_data/all_locations_data.csv
HEAT_all_locations_data = HEAT_all_locations_data %>% filter(country_location_name == "Portugal")
HEAT_all_locations_data = HEAT_all_locations_data %>% filter(sub1_location_name %in% c("Distrito de Lisboa", "Distrito de Setúbal"))
HEAT_all_locations_data = HEAT_all_locations_data %>% filter(is.na(city_location_id))
# saveRDS(HEAT_all_locations_data, "HEAT/HEAT_locations_data_AML.Rds")
# match location id with the 18 different locations (municipalities) in the spreadsheet, and add mean in missing ones and in AML (region).

# PM25 by municipality, incluindo AML
PM25 = openxlsx::read.xlsx("https://github.com/U-Shift/biclar/releases/download/0.0.1/Scenarios_Routing.xlsx", sheet = "PM25")
PM25$PM25 = round(PM25$PM25, 2)
PM25$location_id = as.character(PM25$location_id) # needed to use as input in HaaS

# SCENARIOS combinations
CENARIOS = openxlsx::read.xlsx("https://github.com/U-Shift/biclar/releases/download/0.0.1/Scenarios_Routing.xlsx", sheet = "CENARIOS")
CENARIOS$dist_max[CENARIOS$Estrategia == "Convencional"] = 5000 
CENARIOS$dist_max[CENARIOS$Estrategia == "Ebike"] = 10000 
CENARIOS$dist_max[CENARIOS$Estrategia == "Intermodal"] = 5000 

# File paths
PATHS = openxlsx::read.xlsx("https://github.com/U-Shift/biclar/releases/download/0.0.1/Scenarios_Routing.xlsx", sheet = "paths")
# save all files listed in "routes_filepath", "rnet_filepath", and "car_routes_filepath" in the respective folder.
# download them from https://github.com/U-Shift/biclar/releases




# For scenarios 1 and 2 (Conventional and Ebike) ---------------------------------------------------

Code_Hass = CENARIOS %>%
  filter(Estrategia != "Intermodal") %>% # make only for senários 1 & 2
  filter(Municipio !="AML") %>% # for the 18 muicipalities (and not the metro region AML)
  select(Code) 

# Code_Hass = Code_Hass[c(137:nrow(Code_Hass)),] # if it throws an error, start from the last row

# HEATbind_municipios = HEAT #last result without function

# start_time = Sys.time()
for (i in Code_Hass$Code){
  
  # i = 161304 # test
  
  HEAT = CENARIOS %>% 
    mutate(Year_ref = `0_Ano`,
           Year_cf = `1_Ano`) %>% 
    select(NMunicipio, Municipio, Code, ENMAC, dist_max, Year_ref, Year_cf) %>%
    filter(Code == i) %>% 
    mutate(CodePath = as.integer(stringr::str_sub(Code, -4, -1))) %>% 
    left_join(PATHS, by=c("CodePath" = "Code")) %>% 
    left_join(PM25) %>% 
    mutate(ENMAC = ENMAC/100)
  
  HEAT_bike = readRDS(HEAT$routes_filepath) %>%
    mutate(DTCC = substr(DICOFREor11, 1,4)) %>% # trips with origin in...
    filter(DTCC == HEAT$DTCC) %>% 
    filter(mode == "BICYCLE") %>% # the difference is here. do not consider ferry connections
    st_drop_geometry() %>%
    group_by(id) %>% 
    summarise(distance = sum(distance), 
              Bike = Bike,
              Car = Car + CarP,
              Total = Total,
              Bikeper = Bike / Total) %>% 
    ungroup() %>% 
    filter(distance <= HEAT$dist_max) %>%
    mutate(
      cyc = ifelse(Bikeper >= HEAT$ENMAC, Bike, HEAT$ENMAC * Total),
      new_cyc = ifelse(Bikeper >= HEAT$ENMAC, 0, cyc - Bike)
    )
  
  HEAT_car = routes_r5r_100jit_car %>%
    mutate(DTCC = substr(DICOFREor11, 1,4)) %>%
    filter(DTCC == HEAT$DTCC) %>% 
    filter(id %in% unique(HEAT_bike$id)) %>%  # filter by the same routing trip
    mutate(Bikeper = Bike / Total,
           Car = Car + CarP) %>% # Car as driver and as passanger
    mutate(
      cyc = ifelse(Bikeper >= HEAT$ENMAC, Bike, HEAT$ENMAC * Total),
      new_cyc = ifelse(Bikeper >= HEAT$ENMAC, 0, cyc - Bike)) %>% 
    mutate(Car_new = Car - new_cyc)
  
  
  # proportion in traffic vs. away from major roads. carspeed > 30 ?? 50 is too high, as a road in 40 is still in traffic.
  Trips_rnet = readRDS(HEAT$rnet_filepath) %>%
    st_filter(MUNICIPIOSgeo %>% filter(Concelho == HEAT$Municipio)) %>%
    filter(Bike>0)
  InTraffic = Trips_rnet %>% filter(carspeed > 30)
  
  # Table with the necessary elements to compute HEAT
  HEAT = HEAT %>% mutate(Bike = round(sum(HEAT_bike$Bike)),
                         Total = round(sum(HEAT_bike$Total)),
                         Bike_new = round(sum(HEAT_bike$new_cyc)),
                         Dist_bike = round(weighted.mean(HEAT_bike$distance, w = HEAT_bike$Bike)/1000, 3),
                         Dist_bike_new = round(weighted.mean(HEAT_bike$distance, w = HEAT_bike$cyc)/1000, 3),
                         Bike_per = round(sum(HEAT_bike$Bike)/sum(HEAT_bike$Total)*100,2),
                         Bike_per_new = round(sum(HEAT_bike$cyc)/sum(HEAT_bike$Total)*100,2),
                         Car_per = round(sum(HEAT_bike$Car)/sum(HEAT_bike$Total)*100,2),
                         Car_per_new = round((sum(HEAT_bike$Car)-sum(HEAT_bike$new_cyc))/sum(HEAT_bike$Total)*100,2),
                         Dist_car = round(weighted.mean(HEAT_car$distance, w= HEAT_car$Car)/1000, 3),
                         Dist_car_new = round(weighted.mean(HEAT_car$distance, w= HEAT_car$Car_new)/1000, 3),
                         InTraffic = as.integer(round(sum(InTraffic$Bike) /sum(Trips_rnet$Bike)*100))
  )
  
  
  ### HaaS ###
  
  # The following information was scrapped from a first run using the online HEAT app, and saving the output.
  
  # # # for MER VSL value
  # webapp_input = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/1304_webapp_input.rds"))
  # genericdata = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/1304_generic_data_relevant_with_calc_params.rds"))
  
  # for PPP VSL value
  # webapp_input = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/162410_webapp_input.rds"))
  # genericdata = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/162410_generic_data_relevant_with_calc_params.rds"))
  webapp_input = readRDS("HEAT/162410_webapp_input.rds") # better read from file instead url for several runs
  genericdata = readRDS("HEAT/162410_generic_data_relevant_with_calc_params.rds")
  
  
  # Let's change the values that need to be changed
  # Please comment (#) the values that remain the same for all runs
  
  # VSL for Portugal 2017
  usd_to_eur = 0.976430 #21nov2022
  VSL_intusdPPP = 3055358/usd_to_eur #source: ANSR 2021. original = 3355000
  # VSL_eurotousd = 2323000 #? 
  # VSL_usdMER = 2194000
  #https://www.oecd-ilibrary.org/environment/mortality-risk-valuation-in-environment-health-and-transport-policies_9789264130807-en
  
  
  ## Assessment intro
  # webapp_input[["ucc_country_location_id"]] = "2264397" #Portugal
  webapp_input[["ucc_location_id"]] = HEAT$location_id
  # webapp_input[["ucc_activemode"]] = "bike" # how to select more than 1? c("walk", "bike")
  # webapp_input[["ucc_comparisoncases"]] = "twocases" #alternative "onecase"
  webapp_input[["ucc_yearref"]] = HEAT$Year_ref # reference year
  webapp_input[["ucc_yearcf"]] = HEAT$Year_cf # comparison year
  # webapp_input[["ucc_assessmenttime"]] = 10 # over how many years should the impacts be calculated
  # webapp_input[["ucc_pathway"]] = c("pa", "ap", "crash", "carbon") # which impact to be considered?
  # webapp_input[["ucc_motormodebasic"]] = "car" # modes to be considered. opt in basic categories: c("car", "pt")
  # webapp_input[["ucc_trafficcondition"]] = "euromeancongestionurban" # alternatives: "somecongestion", "heavycongestion", "euromeancongestionrural"
  
  ## Active modes data (ref = Reference, cf = Comparison)
  # webapp_input[["raw_activemode_bike_ref_source"]] = "model" # alternatives: "scenario" (Hypotetical scenario), "popsurvey" (Population survey), "intercept" (Intercept survey), "counts" (Count data), "model" (Modeled Data), "app" (App-based data)
  # webapp_input[["raw_activemode_bike_ref_unit"]] = "modeshare" # alternatives: "min" (Minutes), "hrs" (Hours), "km" (Km), "trips" (Trips), "modeshare" (Mode Share) # other options to select after may show up, depending on the data source
  webapp_input[["raw_activemode_bike_ref_modesharepercent"]] = HEAT$Bike_per # percentage of CYCLING trips in the reference scenario
  webapp_input[["raw_activemode_bike_ref_alltrips"]] = HEAT$Total # total travel volume of ALL MODES in the reference scenario
  # webapp_input[["raw_activemode_bike_ref_alltripsunit"]] = "trips" # unit of the travel volume. alternatives: "duration" (in minutes), "distance" (in km)
  webapp_input[["raw_activemode_bike_ref_tripkm"]] = HEAT$Dist_bike # average length of CYCLING trips for reference scenario
  # webapp_input[["raw_activemode_bike_ref_denominator"]] = "person" # type of population the volume data is based on. alternative depending on previous selection: 
  # webapp_input[["raw_activemode_bike_ref_agerange"]] = "2064" # age range of the assessed population. alternatives: "2064" (Adults, 20-64yrs), "2044" (Younger adults), "45-64" (Older) 
  
  # webapp_input[["raw_activemode_bike_cf_source"]] = "model" # alternatives: "scenario" (Hypotetical scenario), "popsurvey" (Population survey), "intercept" (Intercept survey), "counts" (Count data), "model" (Modeled Data), "app" (App-based data)
  # webapp_input[["raw_activemode_bike_cf_unit"]] = "modeshare" # alternatives: "min" (Minutes), "hrs" (Hours), "km" (Km), "trips" (Trips), "modeshare" (Mode Share) # other options to select after may show up, depending on the data source
  webapp_input[["raw_activemode_bike_cf_modesharepercent"]] = HEAT$Bike_per_new # percentage of CYCLING trips in the comparison scenario
  webapp_input[["raw_activemode_bike_cf_alltrips"]] = HEAT$Total # total travel volume of ALL MODES in the comparison scenario
  # webapp_input[["raw_activemode_bike_cf_alltripsunit"]] = "trips" # unit of the travel volume. alternatives: "duration" (in minutes), "distance" (in km)
  webapp_input[["raw_activemode_bike_cf_tripkm"]] = HEAT$Dist_bike_new # average length of CYCLING trips, for comparison scenario 
  # webapp_input[["raw_activemode_bike_cf_denominator"]] = "person" # type of population the volume data is based on. alternative depending on previous selection: 
  # webapp_input[["raw_activemode_bike_cf_agerange"]] = "2064" # age range of the assessed population. alternatives: "2064" (Adults, 20-64yrs), "2044" (Younger adults), "45-64" (Older) 
  
  ## Motorized modes data (car, tp // ref, cf)
  # webapp_input[["raw_motorizedmode_car_ref_unit"]] = "modeshare" # alternatives: "min" (Minutes), "hrs" (Hours), "km" (Km), "trips" (Trips), "modeshare" (Mode Share) # other options to select after may show up, depending on the data source
  webapp_input[["raw_motorizedmode_car_ref_modesharepercent"]] = HEAT$Car_per # percentage of CAR trips in the reference scenario
  webapp_input[["raw_motorizedmode_car_ref_alltrips"]] = HEAT$Total # total travel volume of ALL MODES in the reference scenario
  # webapp_input[["raw_motorizedmode_car_ref_alltripsunit"]] = "trips" # unit of the travel volume. alternatives: "duration" (in minutes), "distance" (in km)
  webapp_input[["raw_motorizedmode_car_ref_tripkm"]] = HEAT$Dist_car # average length of CAR trips for reference scenario
  
  # webapp_input[["raw_motorizedmode_car_cf_unit"]] = "modeshare" # alternatives: "min" (Minutes), "hrs" (Hours), "km" (Km), "trips" (Trips), "modeshare" (Mode Share) # other options to select after may show up, depending on the data source
  webapp_input[["raw_motorizedmode_car_cf_modesharepercent"]] = HEAT$Car_per_new # percentage of CAR trips in the comparison scenario
  webapp_input[["raw_motorizedmode_car_cf_alltrips"]] = HEAT$Total # total travel volume of ALL MODES in the comparison scenario
  # webapp_input[["raw_motorizedmode_car_cf_alltripsunit"]] = "trips" # unit of the travel volume. alternatives: "duration" (in minutes), "distance" (in km)
  webapp_input[["raw_motorizedmode_car_cf_tripkm"]] = HEAT$Dist_car_new # average length of CAR trips, for comparison scenario 
  
  ## Population
  webapp_input[["ap_pop_tot"]] = HEAT$Pop21 # total population for city or region
  webapp_input[["ap_percentofallages_bike_ref"]] = 80.4 # percentage or total population within age range, for the reference case
  webapp_input[["raw_activemode_bike_ref_population"]] = round(80.4*HEAT$Pop21) # population size used of the assessment in reference case (or ap_pop_tot*ap_percentofallages_bike_ref/100)
  webapp_input[["ap_percentofallages_bike_cf"]] = 80.4 # percentage or total population within age range, for the comparison case
  webapp_input[["raw_activemode_bike_cf_population"]] = round(80.4*HEAT$Pop21) # population size used of the assessment in comparison case (or ap_pop_tot*ap_percentofallages_bike_cf/100)
  
  ## Gernal adjustments - some missing here
  webapp_input[["quali_activemode_bike_percenttransport"]] = 90 # proportion for transport vs. recreation, percentage of trips that are not generated and are done regardless the transport mode, as integer
  webapp_input[["quali_activemode_bike_percentintraffic"]] = HEAT$InTraffic # proportion of trips that takes place in traffic, vs. away from major roads or parks, as integer
  
  ## Air pollution
  webapp_input[["pollution_calc"]] = HEAT$PM25 # PM2.5 concentration fot the location. See: ...
  
  ## Crashes - some missing here! We did not change them. Add the others if needed
  # webapp_input[["crashdata_used_fatality_rate_bike_ref"]] = 1 # Fatality rate for cycling in the reference case, as integer of fatalities / 100 million km
  # webapp_input[["crashdata_used_fatality_rate_bike_cf"]] = 1 # Fatality rate for cycling in the comparison case, as integer of fatalities / 100 million km
  
  ## Value of Statistical Life VSL
  webapp_input[["ap_vsl_currency"]] = "ppp" # currency format of monetizaion of impacts. alternatives: "mer" and "lcu" (not showing in the twocase assesnemt)
  # webapp_input[["ap_vsl_mer_calc"]] = VSL_eurotousd # VSL in US dollars, if "mer" was the selected currency
  webapp_input[["ap_vsl_ppp_calc"]] = VSL_intusdPPP # VSL in international dollars, if "ppp" was the selected currency
  
  ## Economic discounting
  webapp_input[["ap_discountyear"]] = 2022 # year to which you want to discount (or inflate) future (or past) economic values to
  webapp_input[["ap_discountrate"]] = 5 # discount rate
  webapp_input[["ap_inflationrate"]] = 3 # inflation rate
  
  ## Adjusting default and background values
  genericdata$value[genericdata$parameter_name == "occupancy_rate_car"] = 1.6 #car occupancy rate for all purposes. default: 2
  genericdata$value[genericdata$parameter_name == "default_activemode_bike_fromcar"] = 100 # percentage of trips shifting from CAR. default: 30
  genericdata$value[genericdata$parameter_name == "default_activemode_bike_frompt"] = 0 # percentage of trips shifting from CAR. default: 50
  genericdata$value[genericdata$parameter_name == "default_activemode_bike_fromwalk"] = 0 # percentage of trips shifting from CAR. default: 20
  
  
  
  # comunicate with HEAT tool sending the webapp and generic_data
  resp <- 
    httr::POST("https://api.heatwalkingcycling.org/apiv1/heat_5_0/calc_results?output_format=rds",
               httr::set_cookies("user" = Sys.getenv("HEAT_COOKIE")), # This key is stored at .Renviron. usethis::edit_r_environ() to add it
               body = jsonlite::toJSON(
                 list("webapp_input" = webapp_input,
                      "generic_data_relevant" = genericdata)),
               encode = "multipart")
  
  # convert the response to R
  from_hass <- unserialize(httr::content(resp, "raw"))
  
  # Check final result value
  from_hass$results$moneyperyear = as.numeric(gsub(" ","",from_hass$results$moneyperyear))
  from_hass$results$moneytotaldisc = as.numeric(gsub(" ","",from_hass$results$moneytotaldisc))
  
  # Make some useful computations
  HEAT = HEAT %>% mutate(Mortality = from_hass$results$impactperyearave[30],
                         Mortality10 = from_hass$results$impacttotal[30],
                         CO2eq = from_hass$results$impactperyearaveco2[30],
                         CO2eq10 = from_hass$results$impacttotalco2[30],
                         Economic = from_hass$results$moneyperyear[26]-from_hass$results$moneyperyear[27]-from_hass$results$moneyperyear[28]+from_hass$results$moneyperyear[29],
                         Economic10 = from_hass$results$moneytotaldisc[26]-from_hass$results$moneytotaldisc[27]-from_hass$results$moneytotaldisc[28]+from_hass$results$moneytotaldisc[29]) %>% 
    mutate(value_newcyc = round(Economic10/Bike_new),
           value_newcyc_eur = round(Economic10/Bike_new*usd_to_eur))
  
  # Bind with the last run
  HEATbind_municipios = rbind(HEATbind_municipios, HEAT)
  
  print(paste0(HEAT$Municipio, i, " done"))
  
  closeAllConnections() #solves the problem of Error in url("....rds") : all connections are in use
  
}
Sys.time() - start_time


# 3.20min for 16 runs
# 14min for 61 runs
# 14.51min for 68 runs
# 2.35min for 13 runs


# HEATbind_municipios = HEATbind_municipios[-1,] # remove the first attempt
saveRDS(HEATbind_municipios, "HEAT/HEAT_municipios_scenarios12_PPP.Rds")
HEATbind_municipios_PPP = HEATbind_municipios




# For scenario 3 (Intermodality) ------------------------------------------


# For intermodality ---------------------------------------------------------------------------


Code_Hass = CENARIOS %>%
  filter(Estrategia == "Intermodal") %>% # now only for the scenario 3, that gathers a trip by bike + PT 
  filter(Municipio !="AML") %>% 
  select(Code) 

Code_Hass = Code_Hass[c(5:nrow(Code_Hass)),] # if it throws an error, start from the last row

# HEATbind_municipios_intermodal = HEAT #last result without function

# start_time = Sys.time()
for (i in Code_Hass$Code){
  
  # i = 113304 #test
  
  HEAT = CENARIOS %>% 
    mutate(Year_ref = `0_Ano`,
           Year_cf = `1_Ano`) %>% 
    select(NMunicipio, Municipio, Code, ENMAC, dist_max, Year_ref, Year_cf) %>%
    filter(Code == i) %>% 
    mutate(CodePath = as.integer(stringr::str_sub(Code, -4, -1))) %>% 
    left_join(PATHS, by=c("CodePath" = "Code")) %>% 
    left_join(PM25) %>% 
    mutate(ENMAC = ENMAC/100)
  
  HEAT_bike = readRDS(HEAT$routes_filepath) %>%
    mutate(DTCC = substr(DICOFREor11, 1,4)) %>% #trtps with origin in...
    filter(DTCC == HEAT$DTCC) %>% 
    ungroup() %>% 
    st_drop_geometry() %>%
    group_by(id) %>% 
    summarise(distance = sum(distance), 
              Bike = Bike,
              Car = Car + CarP,
              Total = Total,
              Bikeper = Bike / Total) %>% 
    ungroup() %>% 
    mutate(
      cyc = ifelse(Bikeper >= HEAT$ENMAC, Bike, HEAT$ENMAC * Total),
      new_cyc = ifelse(Bikeper >= HEAT$ENMAC, 0, cyc - Bike)
    )
  
  if (nrow(HEAT_bike) == 0) { # for municipalities without intermodality network
    
    HEATbind_municipios_intermodal = bind_rows(HEATbind_municipios_intermodal, HEAT)
    
    print(paste0(HEAT$Municipio, i, " done"))
  }
  else { 
    
    HEAT_car = readRDS(HEAT$car_routes_filepath) %>%
      mutate(DTCC = substr(DICOFREor11, 1,4)) %>%
      filter(DTCC == HEAT$DTCC) %>% 
      filter(id %in% unique(HEAT_bike$id)) %>%  # filter by the same routing trip
      group_by(id) %>% 
      summarise(distance = sum(distance), 
                Bike = Bike,
                Car = Car + CarP, # Car as driver and as passanger
                Total = Total,
                Bikeper = Bike / Total) %>% 
      mutate(
        cyc = ifelse(Bikeper >= HEAT$ENMAC, Bike, HEAT$ENMAC * Total),
        new_cyc = ifelse(Bikeper >= HEAT$ENMAC, 0, cyc - Bike)) %>% 
      mutate(Car_new = Car - new_cyc)
    
    
    # proportion in traffic vs. away from major roads. carspeed > 30 ?? 50 is too high, as a road in 40 is still in traffic.
    Trips_rnet = readRDS(HEAT$rnet_filepath) %>%
      st_filter(MUNICIPIOSgeo %>% filter(Concelho == HEAT$Municipio)) %>%
      filter(Bike>0)
    InTraffic = Trips_rnet %>% filter(carspeed > 30)
    
    HEAT = HEAT %>% mutate(Bike = round(sum(HEAT_bike$Bike)),
                           Total = round(sum(HEAT_bike$Total)),
                           Bike_new = round(sum(HEAT_bike$new_cyc)),
                           Dist_bike = round(weighted.mean(HEAT_bike$distance, w= HEAT_bike$Bike)/1000, 3),
                           Dist_bike_new = round(weighted.mean(HEAT_bike$distance, w= HEAT_bike$cyc)/1000, 3),
                           Bike_per = round(sum(HEAT_bike$Bike)/sum(HEAT_bike$Total)*100,2),
                           Bike_per_new = round(sum(HEAT_bike$cyc)/sum(HEAT_bike$Total)*100,2),
                           Car_per = round(sum(HEAT_bike$Car)/sum(HEAT_bike$Total)*100,2),
                           Car_per_new = round((sum(HEAT_bike$Car)-sum(HEAT_bike$new_cyc))/sum(HEAT_bike$Total)*100,2),
                           Dist_car = round(weighted.mean(HEAT_car$distance, w= HEAT_car$Car)/1000, 3),
                           Dist_car_new = round(weighted.mean(HEAT_car$distance, w= HEAT_car$Car_new)/1000, 3),
                           InTraffic = as.integer(round(sum(InTraffic$Bike) /sum(Trips_rnet$Bike)*100))
    )
    
    if (is.nan(HEAT$Dist_bike)){
      HEAT$Dist_bike = HEAT$Dist_bike_new
    }
    
    if (is.na(HEAT$InTraffic)){
      HEAT$InTraffic = 97
      
    }
    
    ### HaaS ### 
    
    # The following information was scrapped from a first run using the online HEAT app, and saving the output.
    
    # for PPP VSL value
    # webapp_input = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/162410_webapp_input.rds"))
    # genericdata = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/162410_generic_data_relevant_with_calc_params.rds"))
    webapp_input = readRDS("HEAT/162410_webapp_input.rds") # better read from file instead url for several runs
    genericdata = readRDS("HEAT/162410_generic_data_relevant_with_calc_params.rds")
    
    
    # Let's change the values that need to be changed (showing only those ones)
    
  
    # VSL for Portugal 2017
    usd_to_eur = 0.976430 #21nov2022
    VSL_intusdPPP = 3055358/usd_to_eur #source: ANSR 2021. original = 3355000
    #https://www.oecd-ilibrary.org/environment/mortality-risk-valuation-in-environment-health-and-transport-policies_9789264130807-en
    
    
    ## Assessment intro
    webapp_input[["ucc_location_id"]] = HEAT$location_id
    webapp_input[["ucc_yearref"]] = HEAT$Year_ref # reference year
    webapp_input[["ucc_yearcf"]] = HEAT$Year_cf # comparison year
    webapp_input[["ucc_motormodebasic"]] = "car" # modes to be considered. opt in basic categories: c("car", "pt")
    
    ## Active modes data (ref = Reference, cf = Comparison)
    webapp_input[["raw_activemode_bike_ref_modesharepercent"]] = HEAT$Bike_per # percentage of CYCLING trips in the reference scenario
    webapp_input[["raw_activemode_bike_ref_alltrips"]] = HEAT$Total # total travel volume of ALL MODES in the reference scenario
    webapp_input[["raw_activemode_bike_ref_tripkm"]] = HEAT$Dist_bike # average length of CYCLING trips for reference scenario
    
    webapp_input[["raw_activemode_bike_cf_modesharepercent"]] = HEAT$Bike_per_new # percentage of CYCLING trips in the comparison scenario
    webapp_input[["raw_activemode_bike_cf_alltrips"]] = HEAT$Total # total travel volume of ALL MODES in the comparison scenario
    webapp_input[["raw_activemode_bike_cf_tripkm"]] = HEAT$Dist_bike_new # average length of CYCLING trips, for comparison scenario 
    
    ## Motorized modes data (car, tp // ref, cf)
    webapp_input[["raw_motorizedmode_car_ref_modesharepercent"]] = HEAT$Car_per # percentage of CAR trips in the reference scenario
    webapp_input[["raw_motorizedmode_car_ref_alltrips"]] = HEAT$Total # total travel volume of ALL MODES in the reference scenario
    webapp_input[["raw_motorizedmode_car_ref_tripkm"]] = HEAT$Dist_car # average length of CAR trips for reference scenario
    
    webapp_input[["raw_motorizedmode_car_cf_modesharepercent"]] = HEAT$Car_per_new # percentage of CAR trips in the comparison scenario
    webapp_input[["raw_motorizedmode_car_cf_alltrips"]] = HEAT$Total # total travel volume of ALL MODES in the comparison scenario
    webapp_input[["raw_motorizedmode_car_cf_tripkm"]] = HEAT$Dist_car_new # average length of CAR trips, for comparison scenario 
    
    ## Population
    webapp_input[["ap_pop_tot"]] = HEAT$Pop21 # total population for city or region
    webapp_input[["ap_percentofallages_bike_ref"]] = 80.4 # percentage or total population within age range, for the reference case
    webapp_input[["raw_activemode_bike_ref_population"]] = round(80.4*HEAT$Pop21) # population size used of the assessment in reference case (or ap_pop_tot*ap_percentofallages_bike_ref/100)
    webapp_input[["ap_percentofallages_bike_cf"]] = 80.4 # percentage or total population within age range, for the comparison case
    webapp_input[["raw_activemode_bike_cf_population"]] = round(80.4*HEAT$Pop21) # population size used of the assessment in comparison case (or ap_pop_tot*ap_percentofallages_bike_cf/100)
    
    ## Gernal adjustments - some missing here
    webapp_input[["quali_activemode_bike_percenttransport"]] = 90 # proportion for transport vs. recreation, percentage of trips that are not generated and are done regardless the transport mode, as integer
    webapp_input[["quali_activemode_bike_percentintraffic"]] = HEAT$InTraffic # proportion of trips that takes place in traffic, vs. away from major roads or parks, as integer
    
    ## Air pollution
    webapp_input[["pollution_calc"]] = HEAT$PM25 # PM2.5 concentration fot the location. See: ...
    
    ## Value of Statistical Life VSL
    webapp_input[["ap_vsl_currency"]] = "ppp" # currency format of monetizaion of impacts. alternatives: "mer" and "lcu" (not showing in the twocase assesnemt)
    webapp_input[["ap_vsl_ppp_calc"]] = VSL_intusdPPP # VSL in international dollars, if "ppp" was the selected currency
    
    ## Economic discounting
    webapp_input[["ap_discountyear"]] = 2022 # year to which you want to discount (or inflate) future (or past) economic values to
    webapp_input[["ap_discountrate"]] = 5 # discount rate
    webapp_input[["ap_inflationrate"]] = 3 # inflation rate
    
    ## Adjusting default and background values
    genericdata$value[genericdata$parameter_name == "occupancy_rate_car"] = 1.6 #car occupancy rate for all purposes. default: 2
    genericdata$value[genericdata$parameter_name == "default_activemode_bike_fromcar"] = 100 # percentage of trips shifting from CAR. default: 30
    genericdata$value[genericdata$parameter_name == "default_activemode_bike_frompt"] = 0 # percentage of trips shifting from CAR. default: 50
    genericdata$value[genericdata$parameter_name == "default_activemode_bike_fromwalk"] = 0 # percentage of trips shifting from CAR. default: 20
    
    
    # comunicate with HEAT tool sending the webapp and generic_data
    resp <- 
      httr::POST("https://api.heatwalkingcycling.org/apiv1/heat_5_0/calc_results?output_format=rds",
                 httr::set_cookies("user" = Sys.getenv("HEAT_COOKIE")), # This key is stored at .Renviron. usethis::edit_r_environ() to add it
                 body = jsonlite::toJSON(
                   list("webapp_input" = webapp_input,
                        "generic_data_relevant" = genericdata)),
                 encode = "multipart")
    
    # convert the response to R
    from_hass <- unserialize(httr::content(resp, "raw"))
    
    # Check final result value
    from_hass$results$moneyperyear = as.numeric(gsub(" ","",from_hass$results$moneyperyear))
    from_hass$results$moneytotaldisc = as.numeric(gsub(" ","",from_hass$results$moneytotaldisc))
    
    # Make some useful computations
    HEAT = HEAT %>% mutate(Mortality = from_hass$results$impactperyearave[30],
                           Mortality10 = from_hass$results$impacttotal[30],
                           CO2eq = from_hass$results$impactperyearaveco2[30],
                           CO2eq10 = from_hass$results$impacttotalco2[30],
                           Economic = from_hass$results$moneyperyear[26]-from_hass$results$moneyperyear[27]-from_hass$results$moneyperyear[28]+from_hass$results$moneyperyear[29],
                           Economic10 = from_hass$results$moneytotaldisc[26]-from_hass$results$moneytotaldisc[27]-from_hass$results$moneytotaldisc[28]+from_hass$results$moneytotaldisc[29]) %>% 
      mutate(value_newcyc = round(Economic10/Bike_new),
             value_newcyc_eur = round(Economic10/Bike_new*usd_to_eur))
    
    # Bind with the last run
    HEATbind_municipios_intermodal = rbind(HEATbind_municipios_intermodal, HEAT)
    
    print(paste0(HEAT$Municipio, i, " done"))
    
    closeAllConnections() #solves the problem of Error in url("....rds") : all connections are in use
    
  }
}
Sys.time() - start_time


# HEATbind_municipios_intermodal = HEATbind_municipios_intermodal[-1,] # remove the first atempt
saveRDS(HEATbind_municipios_intermodal, "HEAT/HEAT_municipios_intermodal_PPP.Rds")
HEATbind_municipios_intermodal_PPP = HEATbind_municipios_intermodal



