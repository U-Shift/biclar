# Assessment intro
webapp_input[["ucc_country_location_id"]] = "2264397" #Portugal
webapp_input[["ucc_location_id"]] = "2267057" # Lisboa
webapp_input[["ucc_activemode"]] = "bike" # how to select more than 1? c("walk", "bike")
webapp_input[["ucc_comparisoncases"]] = "twocases" #alternative "onecase"
webapp_input[["ucc_yearref"]] = 2018 # reference year
webapp_input[["ucc_yearcf"]] = 2024 # comparison year
webapp_input[["ucc_assessmenttime"]] = 10 # over how many years should the impacts be calculated
webapp_input[["ucc_pathway"]] = c("pa", "ap", "crash", "carbon") # which impact to be considered?
webapp_input[["ucc_motormodebasic"]] = "car" # modes to be considered. opt in basic categories: c("car", "pt")
webapp_input[["ucc_trafficcondition"]] = "euromeancongestionurban" # alternatives: "somecongestion", "heavycongestion", "euromeancongestionrural"

# Active modes data (ref = Reference, cf = Comparison)
webapp_input[["raw_activemode_bike_ref_source"]] = "model" # alternatives: "scenario" (Hypotetical scenario), "popsurvey" (Population survey), "intercept" (Intercept survey), "counts" (Count data), "model" (Modeled Data), "app" (App-based data)
webapp_input[["raw_activemode_bike_ref_unit"]] = "modeshare" # alternatives: "min" (Minutes), "hrs" (Hours), "km" (Km), "trips" (Trips), "modeshare" (Mode Share) # other options to select after may show up, depending on the data source
webapp_input[["raw_activemode_bike_ref_modesharepercent"]] = 0.52 # percentage of CYCLING trips in the reference scenario
webapp_input[["raw_activemode_bike_ref_alltrips"]] = 2317371 # total travel volume of ALL MODES in the reference scenario
webapp_input[["raw_activemode_bike_ref_alltripsunit"]] = "trips" # unit of the travel volume. alternatives: "duration" (in minutes), "distance" (in km)
webapp_input[["raw_activemode_bike_ref_tripkm"]] = 2.7 # average length of CYCLING trips for reference scenario
webapp_input[["raw_activemode_bike_ref_denominator"]] = "person" # type of population the volume data is based on. alternative depending on previous selection: 
webapp_input[["raw_activemode_bike_ref_agerange"]] = "2064" # age range of the assessed population. alternatives: "2064" (Adults, 20-64yrs), "2044" (Younger adults), "45-64" (Older) 

webapp_input[["raw_activemode_bike_cf_source"]] = "model" # alternatives: "scenario" (Hypotetical scenario), "popsurvey" (Population survey), "intercept" (Intercept survey), "counts" (Count data), "model" (Modeled Data), "app" (App-based data)
webapp_input[["raw_activemode_bike_cf_unit"]] = "modeshare" # alternatives: "min" (Minutes), "hrs" (Hours), "km" (Km), "trips" (Trips), "modeshare" (Mode Share) # other options to select after may show up, depending on the data source
webapp_input[["raw_activemode_bike_cf_modesharepercent"]] = 4.04 # percentage of CYCLING trips in the comparison scenario
webapp_input[["raw_activemode_bike_cf_alltrips"]] = 2317371 # total travel volume of ALL MODES in the comparison scenario
webapp_input[["raw_activemode_bike_cf_alltripsunit"]] = "trips" # unit of the travel volume. alternatives: "duration" (in minutes), "distance" (in km)
webapp_input[["raw_activemode_bike_cf_tripkm"]] = 2.68 # average length of CYCLING trips, for comparison scenario 
webapp_input[["raw_activemode_bike_cf_denominator"]] = "person" # type of population the volume data is based on. alternative depending on previous selection: 
webapp_input[["raw_activemode_bike_cf_agerange"]] = "2064" # age range of the assessed population. alternatives: "2064" (Adults, 20-64yrs), "2044" (Younger adults), "45-64" (Older) 

# Motorized modes data (car, tp // ref, cf)
webapp_input[["raw_motorizedmode_car_ref_unit"]] = "modeshare" # alternatives: "min" (Minutes), "hrs" (Hours), "km" (Km), "trips" (Trips), "modeshare" (Mode Share) # other options to select after may show up, depending on the data source
webapp_input[["raw_motorizedmode_car_ref_modesharepercent"]] = 46.34 # percentage of CAR trips in the reference scenario
webapp_input[["raw_motorizedmode_car_ref_alltrips"]] = 2317371 # total travel volume of ALL MODES in the reference scenario
webapp_input[["raw_motorizedmode_car_ref_alltripsunit"]] = "trips" # unit of the travel volume. alternatives: "duration" (in minutes), "distance" (in km)
webapp_input[["raw_motorizedmode_car_ref_tripkm"]] = 7.404 # average length of CAR trips for reference scenario

webapp_input[["raw_motorizedmode_car_cf_unit"]] = "modeshare" # alternatives: "min" (Minutes), "hrs" (Hours), "km" (Km), "trips" (Trips), "modeshare" (Mode Share) # other options to select after may show up, depending on the data source
webapp_input[["raw_motorizedmode_car_cf_modesharepercent"]] = 42.81 # percentage of CAR trips in the comparison scenario
webapp_input[["raw_motorizedmode_car_cf_alltrips"]] = 2317371 # total travel volume of ALL MODES in the comparison scenario
webapp_input[["raw_motorizedmode_car_cf_alltripsunit"]] = "trips" # unit of the travel volume. alternatives: "duration" (in minutes), "distance" (in km)
webapp_input[["raw_motorizedmode_car_cf_tripkm"]] = 7.393 # average length of CAR trips, for comparison scenario 

# Population
webapp_input[["ap_pop_tot"]] = 2870770 # total population for city or region
webapp_input[["ap_percentofallages_bike_ref"]] = 80.4 # percentage or total population within age range, for the reference case
webapp_input[["raw_activemode_bike_ref_population"]] = 2308099 # population size used of the assessment in reference case (or ap_pop_tot*ap_percentofallages_bike_ref/100)
webapp_input[["ap_percentofallages_bike_cf"]] = 80.4 # percentage or total population within age range, for the comparison case
webapp_input[["raw_activemode_bike_cf_population"]] = 2308099 # population size used of the assessment in comparison case (or ap_pop_tot*ap_percentofallages_bike_cf/100)

# Gernal adjustments - some missing here
webapp_input[["quali_activemode_bike_percenttransport"]] = 90 # proportion for transport vs. recreation, percentage of trips that are not generated and are done regardless the transport mode, as integer
webapp_input[["quali_activemode_bike_percentintraffic"]] = 2 # proportion of trips that takes place in traffic, vs. away from major roads or parks, as integer

# Air pollution
webapp_input[["pollution_calc"]] = 9.87 # PM2.5 concentration fot the location. See: ...

# Crashes - some missing here
webapp_input[["crashdata_used_fatality_rate_bike_ref"]] = 1 # Fatality rate for cycling in the reference case, as integer of fatalities / 100 million km
webapp_input[["crashdata_used_fatality_rate_bike_cf"]] = 1 # Fatality rate for cycling in the comparison case, as integer of fatalities / 100 million km

# Value of Statistical Life VSL
webapp_input[["ap_vsl_currency"]] = "mer" # currency format of monetizaion of impacts. alternatives: "ppp" and ??? (not showing in the twocase assesnemt)
webapp_input[["ap_vsl_mer_calc"]] = 2023000 # VSL in US dollars, if "mer" was the selected currency
# webapp_input[["ap_vsl_ppp_calc"]] = 3355000 # VSL in international dollars, if "ppp" was the selected currency

# Economic discounting
webapp_input[["ap_discountyear"]] = 2022 # year to which you want to discount (or inflate) future (or past) economic values to
webapp_input[["ap_discountrate"]] = 5 # discount rate
webapp_input[["ap_inflationrate"]] = 3 # inflation rate

# Adjusting default and background values
genericdata$value[genericdata$parameter_name == "occupancy_rate_car"] = 1.6 #car occupancy rate for all purposes. default: 2
genericdata$value[genericdata$parameter_name == "default_activemode_bike_fromcar"] = 100 # percentage of trips shifting from CAR. default: 30
genericdata$value[genericdata$parameter_name == "default_activemode_bike_frompt"] = 0 # percentage of trips shifting from CAR. default: 50
genericdata$value[genericdata$parameter_name == "default_activemode_bike_fromwalk"] = 0 # percentage of trips shifting from CAR. default: 20
