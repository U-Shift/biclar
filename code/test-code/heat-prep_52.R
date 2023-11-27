# prepare tables for HEAT SE estimations


# locations id
library(readr)
HEAT_all_locations_data <- read_csv("D:/GIS/TML/HEAT_heatr_api/all_locations_data.csv") #data from https://github.com/HEAT-WHO/HEAT_data/blob/main/data/heatr/locations_data/all_locations_data.csv
HEAT_all_locations_data = HEAT_all_locations_data %>% filter(country_location_name == "Portugal")
HEAT_all_locations_data = HEAT_all_locations_data %>% filter(sub1_location_name %in% c("Distrito de Lisboa", "Distrito de Setúbal"))
HEAT_all_locations_data = HEAT_all_locations_data %>% filter(is.na(city_location_id))
saveRDS(HEAT_all_locations_data, "HEAT/HEAT_locations_data_AML.Rds")



# PM25 por municipio, incluindo AML
PM25 = readxl::read_excel("export2/Scenarios_Routing.xlsx", sheet = "PM25")
PM25$PM25 = round(PM25$PM25, 2)
PM25$location_id = as.character(PM25$location_id)

# VSL Portugal 2017
usd_to_eur = 0.976430 #21nov2022
VSL_intusdPPP = 3055358/usd_to_eur #fonte: ANSR 2021. original = 3355000
# VSL_eurotousd = 2323000 #? 
# VSL_usdMER = 2194000
#https://www.oecd-ilibrary.org/environment/mortality-risk-valuation-in-environment-health-and-transport-policies_9789264130807-en

# CENARIOS combinations
CENARIOS = readxl::read_excel("export2/Scenarios_Routing.xlsx", sheet = "CENARIOS")
CENARIOS$dist_max[CENARIOS$Estrategia == "Convencional"] = 5000 
CENARIOS$dist_max[CENARIOS$Estrategia == "Ebike"] = 10000 
CENARIOS$dist_max[CENARIOS$Estrategia == "Intermodal"] = 5000 

# File paths
PATHS = readxl::read_excel("export2/Scenarios_Routing.xlsx", sheet = "paths")


# proportion in traffic vs. away from major roads. carspeed > 30 ?? 50 is too high, as a road in 40 is still in traffic.
# População movel AML = 80.4%
# Deslocacoes / pessoa / dia = 2.6 AML



# script to get values -------------------------------------------------------------------------

# Code == X

Code_Hass = CENARIOS$Code[1:8] #fazer por enquanto só AML e senários 1 e 2
#para os cenários dos municípios, acrescentar codigo que filtre as viagens com inicio lá, e a rnet within + buffer
# HEATbind = HEAT #last result without function


for (i in Code_Hass){
  
# i = 1304

HEAT = CENARIOS %>% 
  mutate(Year_ref = `0_Ano`,
         Year_cf = `1_Ano`) %>% 
  select(NMunicipio, Municipio, Code, ENMAC, dist_max, Year_ref, Year_cf) %>%
  filter(Code == i) %>% 
  left_join(PATHS) %>% 
  left_join(PM25) %>% 
  mutate(ENMAC = ENMAC/100)

HEAT_bike = readRDS(HEAT$routes_filepath)
# HEAT_bikeOD = paste0(HEAT_bike$DICOFREor11, HEAT_bike$DICOFREde11) %>% unique()
# max_euc = max(HEAT_bike$eucl_distance)

HEAT_bike = HEAT_bike %>% 
  ungroup() %>% 
  filter(mode == "BICYCLE") %>% #the difference is here. this network does not consider ferry lines
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
  # filter(eucl_distance <= max_euc) %>% #euclidean distance of routes_ferry3_filtered
  # mutate(HEAT_carOD = paste0(DICOFREor11, DICOFREde11)) %>% 
  # filter(HEAT_carOD %in% HEAT_bikeOD) %>%  #better filter by ODs equal than by max distance
  filter(id %in% unique(HEAT_bike$id)) %>%  # even better, filter by the same routing trip
  mutate(Bikeper = Bike / Total,
         Car = Car + CarP) %>% 
  mutate(
    cyc = ifelse(Bikeper >= HEAT$ENMAC, Bike, HEAT$ENMAC * Total),
    new_cyc = ifelse(Bikeper >= HEAT$ENMAC, 0, cyc - Bike)) %>% 
  mutate(Car_new = Car + CarP - new_cyc)

Trips_rnet = readRDS(HEAT$rnet_filepath) %>% filter(Bike>0)
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
 

#car dist is about 20% more than cycling


# HaaS 

# # for MER VSL value
# webapp_input = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/1304_webapp_input.rds"))
# genericdata = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/1304_generic_data_relevant_with_calc_params.rds"))

# for PPP VSL value
webapp_input = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/162410_webapp_input.rds"))
genericdata = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/162410_generic_data_relevant_with_calc_params.rds"))



# Assessment intro
# webapp_input[["ucc_country_location_id"]] = "2264397" #Portugal
webapp_input[["ucc_location_id"]] = HEAT$location_id
# webapp_input[["ucc_activemode"]] = "bike" # how to select more than 1? c("walk", "bike")
# webapp_input[["ucc_comparisoncases"]] = "twocases" #alternative "onecase"
webapp_input[["ucc_yearref"]] = HEAT$Year_ref # reference year
webapp_input[["ucc_yearcf"]] = HEAT$Year_cf # comparison year
# webapp_input[["ucc_assessmenttime"]] = 10 # over how many years should the impacts be calculated
# webapp_input[["ucc_pathway"]] = c("pa", "ap", "crash", "carbon") # which impact to be considered?
webapp_input[["ucc_motormodebasic"]] = "car" # modes to be considered. opt in basic categories: c("car", "pt")
# webapp_input[["ucc_trafficcondition"]] = "euromeancongestionurban" # alternatives: "somecongestion", "heavycongestion", "euromeancongestionrural"

# Active modes data (ref = Reference, cf = Comparison)
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

# Motorized modes data (car, tp // ref, cf)
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

# Population
webapp_input[["ap_pop_tot"]] = HEAT$Pop21 # total population for city or region
webapp_input[["ap_percentofallages_bike_ref"]] = 80.4 # percentage or total population within age range, for the reference case
webapp_input[["raw_activemode_bike_ref_population"]] = round(80.4*HEAT$Pop21) # population size used of the assessment in reference case (or ap_pop_tot*ap_percentofallages_bike_ref/100)
webapp_input[["ap_percentofallages_bike_cf"]] = 80.4 # percentage or total population within age range, for the comparison case
webapp_input[["raw_activemode_bike_cf_population"]] = round(80.4*HEAT$Pop21) # population size used of the assessment in comparison case (or ap_pop_tot*ap_percentofallages_bike_cf/100)

# Gernal adjustments - some missing here
webapp_input[["quali_activemode_bike_percenttransport"]] = 90 # proportion for transport vs. recreation, percentage of trips that are not generated and are done regardless the transport mode, as integer
webapp_input[["quali_activemode_bike_percentintraffic"]] = HEAT$InTraffic # proportion of trips that takes place in traffic, vs. away from major roads or parks, as integer

# Air pollution
webapp_input[["pollution_calc"]] = HEAT$PM25 # PM2.5 concentration fot the location. See: ...

# Crashes - some missing here
# webapp_input[["crashdata_used_fatality_rate_bike_ref"]] = 1 # Fatality rate for cycling in the reference case, as integer of fatalities / 100 million km
# webapp_input[["crashdata_used_fatality_rate_bike_cf"]] = 1 # Fatality rate for cycling in the comparison case, as integer of fatalities / 100 million km

# Value of Statistical Life VSL
webapp_input[["ap_vsl_currency"]] = "ppp" # currency format of monetizaion of impacts. alternatives: "mer" and "lcu" (not showing in the twocase assesnemt)
# webapp_input[["ap_vsl_mer_calc"]] = VSL_eurotousd # VSL in US dollars, if "mer" was the selected currency
webapp_input[["ap_vsl_ppp_calc"]] = VSL_intusdPPP # VSL in international dollars, if "ppp" was the selected currency

# Economic discounting
webapp_input[["ap_discountyear"]] = 2022 # year to which you want to discount (or inflate) future (or past) economic values to
webapp_input[["ap_discountrate"]] = 5 # discount rate
webapp_input[["ap_inflationrate"]] = 3 # inflation rate

# Adjusting default and background values
genericdata$value[genericdata$parameter_name == "occupancy_rate_car"] = 1.6 #car occupancy rate for all purposes. default: 2
genericdata$value[genericdata$parameter_name == "default_activemode_bike_fromcar"] = 100 # percentage of trips shifting from CAR. default: 30
genericdata$value[genericdata$parameter_name == "default_activemode_bike_frompt"] = 0 # percentage of trips shifting from PT default: 50
genericdata$value[genericdata$parameter_name == "default_activemode_bike_fromwalk"] = 0 # percentage of trips shifting from Walk. default: 20




resp <- 
  httr::POST("https://api.heatwalkingcycling.org/apiv1/heat_5_0/calc_results?output_format=rds",
             httr::set_cookies("user" = Sys.getenv("HEAT_COOKIE")),
             body = jsonlite::toJSON(
               list("webapp_input" = webapp_input,
                    "generic_data_relevant" = genericdata)),
             encode = "multipart")

# convert to R
from_hass <- unserialize(httr::content(resp, "raw"))

# Check final result value
from_hass$results$moneyperyear = as.numeric(gsub(" ","",from_hass$results$moneyperyear))
from_hass$results$moneytotaldisc = as.numeric(gsub(" ","",from_hass$results$moneytotaldisc))

HEAT = HEAT %>% mutate(Mortality = from_hass$results$impactperyearave[30],
                       Mortality10 = from_hass$results$impacttotal[30],
                       CO2eq = from_hass$results$impactperyearaveco2[30],
                       CO2eq10 = from_hass$results$impacttotalco2[30],
                       Economic = from_hass$results$moneyperyear[26]-from_hass$results$moneyperyear[27]-from_hass$results$moneyperyear[28]+from_hass$results$moneyperyear[29],
                       Economic10 = from_hass$results$moneytotaldisc[26]-from_hass$results$moneytotaldisc[27]-from_hass$results$moneytotaldisc[28]+from_hass$results$moneytotaldisc[29]) %>% 
  mutate(value_newcyc = round(Economic10/Bike_new),
         value_newcyc_eur = round(Economic10/Bike_new*usd_to_eur))
                    
HEATbind = rbind(HEATbind, HEAT)

print(paste0(i, " done"))

closeAllConnections() #solves the problem of Error in url("....rds") : all connections are in use

}
#1.34min for 8 runs

HEATbind = HEATbind[-c(1:8),] #remove the trial one
# saveRDS(HEATbind, "HEAT/HEAT_AML_1e2.Rds")
saveRDS(HEATbind, "HEAT/HEAT_AML_1e2_ppp.Rds")
HEATbind_aml_PPP = HEATbind




## For Intermodality ---------------------------------------------------------------------------
# I run this again after finding thge compability issues with 5.2. Is working fine. Need to update in the previous scenarios: the TOTAL trips should be total/person

Code_Hass = CENARIOS$Code[9:12] #só AML e cenários 3

HEATbind_intermodal_correct = HEAT

for (i in Code_Hass){
  
  # i = 3304
  
  HEAT = CENARIOS %>% 
    mutate(Year_ref = `0_Ano`,
           Year_cf = `1_Ano`) %>% 
    select(NMunicipio, Municipio, Code, ENMAC, dist_max, Year_ref, Year_cf) %>%
    filter(Code == i) %>% 
    left_join(PATHS) %>% 
    left_join(PM25) %>% 
    mutate(ENMAC = ENMAC/100)
  
  HEAT_bike = readRDS(HEAT$routes_filepath) %>% 
    st_drop_geometry() %>%
    ungroup() %>% 
    group_by(id) %>%  # it shouldn't return two segments again.
    summarise(distance = sum(distance),  # we need to put both segments distance together!
              Bike = mean(Bike),
              Car = mean(Car) + mean(CarP),
              Total = mean(Total),
              Bikeper = mean(Bike) / mean(Total)) %>% 
    # unique() |> # NOT THE IDEAL BUT WORKS FOR THE PURPUSE (this was missing in the first version 2023)
    ungroup() %>% 
    mutate(
      cyc = ifelse(Bikeper >= HEAT$ENMAC, Bike, HEAT$ENMAC * Total),
      new_cyc = ifelse(Bikeper >= HEAT$ENMAC, 0, cyc - Bike)
    )
  
  
  HEAT_car = readRDS(HEAT$car_routes_filepath) %>%
    ungroup() %>% 
    filter(id %in% unique(HEAT_bike$id)) %>% 
    group_by(id) %>% 
    summarise(distance = sum(distance), 
              Bike = mean(Bike),
              Car = mean(Car) + mean(CarP),
              Total = mean(Total),
              Bikeper = mean(Bike) / mean(Total)) %>% 
    mutate(
      cyc = ifelse(Bikeper >= HEAT$ENMAC, Bike, HEAT$ENMAC * Total),
      new_cyc = ifelse(Bikeper >= HEAT$ENMAC, 0, cyc - Bike)) %>% 
    mutate(Car_new = Car - new_cyc)
  
  Trips_rnet = readRDS(HEAT$rnet_filepath) %>% filter(Bike>0)
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
  
  
  #car dist is about 20% more than cycling

  # HAAS

  # for PPP VSL value
  webapp_input = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/2023_52_1304_webapp_input.rds"))
  genericdata = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/2023_52_1304_generic_data_relevant_with_calc_params.rds"))
  
  # tripspersondayAML = 2.60
  activepop = 0.804
  
  # Assessment intro
  # webapp_input[["ucc_country_location_id"]] = "2264397" #Portugal
  webapp_input[["ucc_location_id"]] = HEAT$location_id
  # webapp_input[["ucc_activemode"]] = "bike" # how to select more than 1? c("walk", "bike")
  # webapp_input[["ucc_comparisoncases"]] = "twocases" #alternative "onecase"
  webapp_input[["ucc_yearref"]] = HEAT$Year_ref # reference year
  webapp_input[["ucc_yearcf"]] = HEAT$Year_cf # comparison year
  # webapp_input[["ucc_assessmenttime"]] = 10 # over how many years should the impacts be calculated
  # webapp_input[["ucc_pathway"]] = c("pa", "ap", "crash", "carbon") # which impact to be considered?
  webapp_input[["ucc_motormodebasic"]] = "car" # modes to be considered. opt in basic categories: c("car", "pt")
  # webapp_input[["ucc_trafficcondition"]] = "euromeancongestionurban" # alternatives: "somecongestion", "heavycongestion", "euromeancongestionrural"
  
  # Active modes data (ref = Reference, cf = Comparison)
  # webapp_input[["raw_activemode_bike_ref_source"]] = "model" # alternatives: "scenario" (Hypotetical scenario), "popsurvey" (Population survey), "intercept" (Intercept survey), "counts" (Count data), "model" (Modeled Data), "app" (App-based data)
  # webapp_input[["raw_activemode_bike_ref_unit"]] = "modeshare" # alternatives: "min" (Minutes), "hrs" (Hours), "km" (Km), "trips" (Trips), "modeshare" (Mode Share) # other options to select after may show up, depending on the data source
  webapp_input[["raw_activemode_bike_ref_modesharepercent"]] = HEAT$Bike_per # percentage of CYCLING trips in the reference scenario
  webapp_input[["raw_activemode_bike_ref_alltrips"]] = HEAT$Total/round(activepop*HEAT$Pop21) # total travel volume of ALL MODES PER PERSON in the reference scenario
  # webapp_input[["raw_activemode_bike_ref_alltrips"]] = tripspersondayAML # they say is number of trips per person per day
  # webapp_input[["raw_activemode_bike_ref_alltripsunit"]] = "trips" # unit of the travel volume. alternatives: "duration" (in minutes), "distance" (in km)
  webapp_input[["raw_activemode_bike_ref_tripkm"]] = HEAT$Dist_bike # average length of CYCLING trips for reference scenario
  # webapp_input[["raw_activemode_bike_ref_denominator"]] = "person" # type of population the volume data is based on. alternative depending on previous selection: 
  # webapp_input[["raw_activemode_bike_ref_agerange"]] = "2064" # age range of the assessed population. alternatives: "2064" (Adults, 20-64yrs), "2044" (Younger adults), "45-64" (Older) 
  
  # webapp_input[["raw_activemode_bike_cf_source"]] = "model" # alternatives: "scenario" (Hypotetical scenario), "popsurvey" (Population survey), "intercept" (Intercept survey), "counts" (Count data), "model" (Modeled Data), "app" (App-based data)
  # webapp_input[["raw_activemode_bike_cf_unit"]] = "modeshare" # alternatives: "min" (Minutes), "hrs" (Hours), "km" (Km), "trips" (Trips), "modeshare" (Mode Share) # other options to select after may show up, depending on the data source
  webapp_input[["raw_activemode_bike_cf_modesharepercent"]] = HEAT$Bike_per_new # percentage of CYCLING trips in the comparison scenario
  webapp_input[["raw_activemode_bike_cf_alltrips"]] = HEAT$Total/round(activepop*HEAT$Pop21) # total travel volume of ALL MODES PER PERSON in the comparison scenario
  # webapp_input[["raw_activemode_bike_cf_alltrips"]] = tripspersondayAML
  # webapp_input[["raw_activemode_bike_cf_alltripsunit"]] = "trips" # unit of the travel volume. alternatives: "duration" (in minutes), "distance" (in km)
  webapp_input[["raw_activemode_bike_cf_tripkm"]] = HEAT$Dist_bike_new # average length of CYCLING trips, for comparison scenario 
  # webapp_input[["raw_activemode_bike_cf_denominator"]] = "person" # type of population the volume data is based on. alternative depending on previous selection: 
  # webapp_input[["raw_activemode_bike_cf_agerange"]] = "2064" # age range of the assessed population. alternatives: "2064" (Adults, 20-64yrs), "2044" (Younger adults), "45-64" (Older) 
  
  # Motorized modes data (car, tp // ref, cf)
  # webapp_input[["raw_motorizedmode_car_ref_unit"]] = "modeshare" # alternatives: "min" (Minutes), "hrs" (Hours), "km" (Km), "trips" (Trips), "modeshare" (Mode Share) # other options to select after may show up, depending on the data source
  webapp_input[["raw_motorizedmode_car_ref_modesharepercent"]] = HEAT$Car_per # percentage of CAR trips in the reference scenario
  webapp_input[["raw_motorizedmode_car_ref_alltrips"]] = HEAT$Total/round(activepop*HEAT$Pop21) # total travel volume of ALL MODES PER PERSON in the reference scenario
  # webapp_input[["raw_motorizedmode_car_ref_alltrips"]] = tripspersondayAML
  # webapp_input[["raw_motorizedmode_car_ref_alltripsunit"]] = "trips" # unit of the travel volume. alternatives: "duration" (in minutes), "distance" (in km)
  webapp_input[["raw_motorizedmode_car_ref_tripkm"]] = HEAT$Dist_car # average length of CAR trips for reference scenario
  
  # webapp_input[["raw_motorizedmode_car_cf_unit"]] = "modeshare" # alternatives: "min" (Minutes), "hrs" (Hours), "km" (Km), "trips" (Trips), "modeshare" (Mode Share) # other options to select after may show up, depending on the data source
  webapp_input[["raw_motorizedmode_car_cf_modesharepercent"]] = HEAT$Car_per_new # percentage of CAR trips in the comparison scenario
  webapp_input[["raw_motorizedmode_car_cf_alltrips"]] = HEAT$Total/round(activepop*HEAT$Pop21) # total travel volume of ALL MODES PER PERSON in the comparison scenario
  # webapp_input[["raw_motorizedmode_car_cf_alltrips"]] = tripspersondayAML
  # webapp_input[["raw_motorizedmode_car_cf_alltripsunit"]] = "trips" # unit of the travel volume. alternatives: "duration" (in minutes), "distance" (in km)
  webapp_input[["raw_motorizedmode_car_cf_tripkm"]] = HEAT$Dist_car_new # average length of CAR trips, for comparison scenario 
  
  # Population
  webapp_input[["ap_pop_tot"]] = HEAT$Pop21 # total population for city or region
  webapp_input[["ap_percentofallages_bike_ref"]] = activepop*100 # percentage or total population within age range, for the reference case
  webapp_input[["raw_activemode_bike_ref_population"]] = round(activepop*HEAT$Pop21) # population size used of the assessment in reference case (or ap_pop_tot*ap_percentofallages_bike_ref/100)
  webapp_input[["ap_percentofallages_bike_cf"]] = activepop*100 # percentage or total population within age range, for the comparison case
  webapp_input[["raw_activemode_bike_cf_population"]] = round(activepop*HEAT$Pop21) # population size used of the assessment in comparison case (or ap_pop_tot*ap_percentofallages_bike_cf/100)
  
  # Gernal adjustments - some missing here
  webapp_input[["quali_activemode_bike_percenttransport"]] = 90 # proportion for transport vs. recreation, percentage of trips that are not generated and are done regardless the transport mode, as integer
  webapp_input[["quali_activemode_bike_percentintraffic"]] = HEAT$InTraffic # proportion of trips that takes place in traffic, vs. away from major roads or parks, as integer
  
  # Air pollution
  webapp_input[["pollution_calc"]] = HEAT$PM25 # PM2.5 concentration fot the location. See: ...
  
  # Crashes - some missing here
  # webapp_input[["crashdata_used_fatality_rate_bike_ref"]] = 1 # Fatality rate for cycling in the reference case, as integer of fatalities / 100 million km
  # webapp_input[["crashdata_used_fatality_rate_bike_cf"]] = 1 # Fatality rate for cycling in the comparison case, as integer of fatalities / 100 million km
  
  # Value of Statistical Life VSL
  webapp_input[["ap_vsl_currency"]] = "ppp" # currency format of monetizaion of impacts. alternatives: "mer" and "lcu" (not showing in the twocase assesnemt)
  # webapp_input[["ap_vsl_mer_calc"]] = VSL_eurotousd # VSL in US dollars, if "mer" was the selected currency
  webapp_input[["ap_vsl_ppp_calc"]] = VSL_intusdPPP # VSL in international dollars, if "ppp" was the selected currency
  
  # Economic discounting
  webapp_input[["ap_discountyear"]] = 2022 # year to which you want to discount (or inflate) future (or past) economic values to
  webapp_input[["ap_discountrate"]] = 5 # discount rate
  webapp_input[["ap_inflationrate"]] = 3 # inflation rate
  
  # Adjusting default and background values
  genericdata$value[genericdata$parameter_name == "occupancy_rate_car"] = 1.6 #car occupancy rate for all purposes. default: 2
  genericdata$value[genericdata$parameter_name == "default_activemode_bike_fromcar"] = 100 # percentage of trips shifting from CAR. default: 30
  genericdata$value[genericdata$parameter_name == "default_activemode_bike_frompt"] = 0 # percentage of trips shifting from CAR. default: 50
  genericdata$value[genericdata$parameter_name == "default_activemode_bike_fromwalk"] = 0 # percentage of trips shifting from CAR. default: 20
  
  
  
  
  resp <- 
    httr::POST("https://api.heatwalkingcycling.org/apiv1/heat_5_0/calc_results?output_format=rds",
               httr::set_cookies("user" = Sys.getenv("HEAT_COOKIE")),
               body = jsonlite::toJSON(
                 list("webapp_input" = webapp_input,
                      "generic_data_relevant" = genericdata)),
               encode = "multipart")
  
  # convert to R
  from_hass <- unserialize(httr::content(resp, "raw"))
  
  # Check final result value
  # from_hass$results$moneyperyear = as.numeric(gsub(" ","",from_hass$results$moneyperyear))
  # from_hass$results$moneytotaldisc = as.numeric(gsub(" ","",from_hass$results$moneytotaldisc))
  
  HEAT = HEAT %>% mutate(Mortality = from_hass$results$impactperyearave[40], #total
                         Mortality10 = from_hass$results$impacttotal[40], #total
                         CO2eq = from_hass$results$impactperyearaveco2[40], #total
                         CO2eq10 = from_hass$results$impacttotalco2[40], #total
                         Economic = from_hass$results$moneyperyear[36]+from_hass$results$moneyperyear[37]+from_hass$results$moneyperyear[38]+from_hass$results$moneyperyear[39],
                         Economic10 = from_hass$results$moneytotaldisc[36]+from_hass$results$moneytotaldisc[37]+from_hass$results$moneytotaldisc[38]+from_hass$results$moneytotaldisc[39],
                         Environmenal_only = from_hass$results$moneyperyear[39], # only the environmental impacts
                         Environmenal10_only = from_hass$results$moneytotaldisc[39]) %>% 
    mutate(value_newcyc = round(Economic10/Bike_new),
           value_newcyc_eur = round(Economic10/Bike_new*usd_to_eur))
  
  HEATbind_intermodal_correct = rbind(HEATbind_intermodal_correct, HEAT)
  
  print(paste0(i, " done"))
  
  closeAllConnections() #solves the problem of Error in url("....rds") : all connections are in use
  
}

HEATbind_intermodal_correct = HEATbind_intermodal_correct[-1,]
# HEATbind_intermodal_correct_PPP = HEATbind_intermodal_correct
saveRDS(HEATbind_intermodal_correct, "HEAT/HEAT_AML_intermodal_ppp_NEW.Rds")

piggyback::pb_upload("HEAT/HEAT_AML_intermodal_ppp_NEW.Rds")
