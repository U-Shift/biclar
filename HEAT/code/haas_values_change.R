# Assessment intro
heat1304_webappinput[["ucc_country_location_id"]] = "2264397" #Portugal
heat1304_webappinput[["ucc_location_id"]] = "2267057" # Lisboa
heat1304_webappinput[["ucc_activemode"]] = "bike" # how to select more than 1? c("walk", "bike")
heat1304_webappinput[["ucc_comparisoncases"]] = "twocases" #alternative "onecase"
heat1304_webappinput[["ucc_yearref"]] = 2018 # reference year
heat1304_webappinput[["ucc_yearcf"]] = 2024 # comparison year
heat1304_webappinput[["ucc_assessmenttime"]] = 10 # over how many years should the impacts be calculated
heat1304_webappinput[["ucc_pathway"]] = c("pa", "ap", "crash", "carbon") # which impact to be considered?
heat1304_webappinput[["ucc_motormodebasic"]] = "car" # modes to be considered. opt in basic categories: c("car", "pt")
heat1304_webappinput[["ucc_trafficcondition"]] = "euromeancongestionurban" # alternatives: "somecongestion", "heavycongestion", "euromeancongestionrural"

# Active modes data (ref = Reference, cf = Comparison)
heat1304_webappinput[["raw_activemode_bike_ref_source"]] = "model" # alternatives: "scenario" (Hypotetical scenario), "popsurvey" (Population survey), "intercept" (Intercept survey), "counts" (Count data), "model" (Modeled Data), "app" (App-based data)
heat1304_webappinput[["raw_activemode_bike_ref_unit-label"]] = "modeshare" # alternatives: "min" (Minutes), "hrs" (Hours), "km" (Km), "trips" (Trips), "modeshare" (Mode Share) # other options to select after may show up, depending on the data source
heat1304_webappinput[["raw_activemode_bike_ref_modesharepercent"]] = 0.52 # percentage of CYCLING trips in the reference scenario
heat1304_webappinput[["raw_activemode_bike_ref_alltrips"]] = 2317371 # total travel volume of ALL MODES in the reference scenario
heat1304_webappinput[["raw_activemode_bike_ref_alltripsunit"]] = "trips" # unit of the travel volume. alternatives: "duration" (in minutes), "distance" (in km)
heat1304_webappinput[["raw_activemode_bike_ref_tripkm"]] = 2.7 # average length of CYCLING trips for reference scenario
heat1304_webappinput[["raw_activemode_bike_ref_denominator"]] = "person" # type of population the volume data is based on. alternative depending on previous selection: 
heat1304_webappinput[["raw_activemode_bike_ref_agerange"]] = "2064" # age range of the assessed population. alternatives: "2064" (Adults, 20-64yrs), "2044" (Younger adults), "45-64" (Older) 

heat1304_webappinput[["raw_activemode_bike_cf_source"]] = "model" # alternatives: "scenario" (Hypotetical scenario), "popsurvey" (Population survey), "intercept" (Intercept survey), "counts" (Count data), "model" (Modeled Data), "app" (App-based data)
heat1304_webappinput[["raw_activemode_bike_cf_unit-label"]] = "modeshare" # alternatives: "min" (Minutes), "hrs" (Hours), "km" (Km), "trips" (Trips), "modeshare" (Mode Share) # other options to select after may show up, depending on the data source
heat1304_webappinput[["raw_activemode_bike_cf_modesharepercent"]] = 4.04 # percentage of CYCLING trips in the comparison scenario
heat1304_webappinput[["raw_activemode_bike_cf_alltrips"]] = 2317371 # total travel volume of ALL MODES in the comparison scenario
heat1304_webappinput[["raw_activemode_bike_cf_alltripsunit"]] = "trips" # unit of the travel volume. alternatives: "duration" (in minutes), "distance" (in km)
heat1304_webappinput[["raw_activemode_bike_cf_tripkm"]] = 2.68 # average length of CYCLING trips,f or comparison scenario 
heat1304_webappinput[["raw_activemode_bike_cf_denominator"]] = "person" # type of population the volume data is based on. alternative depending on previous selection: 
heat1304_webappinput[["raw_activemode_bike_cf_agerange"]] = "2064" # age range of the assessed population. alternatives: "2064" (Adults, 20-64yrs), "2044" (Younger adults), "45-64" (Older) 

# Motorized modes data (car, tp // ref, cf)
heat1304_webappinput[["raw_motorizedmode_car_ref_unit-label"]] = "modeshare" # alternatives: "min" (Minutes), "hrs" (Hours), "km" (Km), "trips" (Trips), "modeshare" (Mode Share) # other options to select after may show up, depending on the data source
heat1304_webappinput[["raw_motorizedmode_car_ref_modesharepercent"]] = 46.34 # percentage of CAR trips in the reference scenario
heat1304_webappinput[["raw_motorizedmode_car_ref_alltrips"]] = 2317371 # total travel volume of ALL MODES in the reference scenario
heat1304_webappinput[["raw_motorizedmode_car_ref_alltripsunit"]] = "trips" # unit of the travel volume. alternatives: "duration" (in minutes), "distance" (in km)
heat1304_webappinput[["raw_motorizedmode_car_ref_tripkm"]] = 7.404 # average length of CAR trips for reference scenario

heat1304_webappinput[["raw_motorizedmode_car_cf_unit-label"]] = "modeshare" # alternatives: "min" (Minutes), "hrs" (Hours), "km" (Km), "trips" (Trips), "modeshare" (Mode Share) # other options to select after may show up, depending on the data source
heat1304_webappinput[["raw_motorizedmode_car_cf_modesharepercent"]] = 42.81 # percentage of CAR trips in the comparison scenario
heat1304_webappinput[["raw_motorizedmode_car_cf_alltrips"]] = 2317371 # total travel volume of ALL MODES in the comparison scenario
heat1304_webappinput[["raw_motorizedmode_car_cf_alltripsunit"]] = "trips" # unit of the travel volume. alternatives: "duration" (in minutes), "distance" (in km)
heat1304_webappinput[["raw_motorizedmode_car_cf_tripkm"]] = 7.393 # average length of CAR trips,f or comparison scenario 




# Adjusting generic data
heat1304_genericdata$value[heat1304_genericdata$parameter_name == "occupancy_rate_car"] = 1.6 #car occupancy. default: 2
heat1304_genericdata$value[heat1304_genericdata$parameter_name == "default_activemode_bike_fromcar"] = 100 # percentage of trips shifting from CAR. default: 
heat1304_genericdata$value[heat1304_genericdata$parameter_name == "default_activemode_bike_fromwalk"] = 0 # percentage of trips shifting from CAR. default: 
heat1304_genericdata$value[heat1304_genericdata$parameter_name == "default_activemode_bike_frompt"] = 0 # percentage of trips shifting from CAR. default: 
