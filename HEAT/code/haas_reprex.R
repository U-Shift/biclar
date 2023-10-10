

# read RDS exported from UI
webapp_input = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/1304_webapp_input.rds"))
genericdata = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/1304_generic_data_relevant_with_calc_params.rds"))


## 1. Input and returned data as json

# to get data prepared by "results"
# input data as "json"
# returned data as "json"
resp <- 
  httr::POST("https://api.heatwalkingcycling.org/apiv1/heat_5_0/results?output_format=json",
             httr::set_cookies("user" = Sys.getenv("HEAT_COOKIE")),
             body = jsonlite::toJSON(list("webapp_input" = webapp_input)),
             encode = "multipart")

# convert from "json" to R format
from_json <- jsonlite::fromJSON(httr::content(resp, "text")) #parse?
str(from_json$results)

# Check final result value
res = as.numeric(gsub(" ","",from_json$results$moneytotaldisc)) #remove spaces and convert to to numeric
res[26]-res[27]-res[28]+res[29]







## 2. Input and returned data as jsonl-serialized

# read RDS exported from UI
webapp_input = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/1304_webapp_input.rds"))


# to get data prepared by "results"
# input data as "jsonl-serialized"
# returned data as "jsonl-serialized"
resp <- 
  httr::POST("https://api.heatwalkingcycling.org/apiv1/heat_5_0/results?output_format=jsonl_serialized",
             httr::set_cookies("user" = Sys.getenv("HEAT_COOKIE")),
             body = jsonlite::serializeJSON(list("webapp_input" = webapp_input)),
             httr::content_type("application/jsonl-serialized"),
             encode = "multipart")

# convert from "jsonl-serialized" to R format
from_serialized_json <- jsonlite::unserializeJSON(httr::content(resp, "text"))


str(from_serialized_json$results)

# Check final result value
res = as.numeric(gsub(" ","",from_serialized_json$results$moneytotaldisc)) #remove spaces and convert to to numeric
res[26]-res[27]-res[28]+res[29]




## 3. Input and returned data as rds, two inputs

# read RDS exported from UI
webapp_input = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/1304_webapp_input.rds"))
genericdata = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/1304_generic_data_relevant_with_calc_params.rds"))

# to get data prepared by "calc_results"
# input data as "json", two inputs - "generic_data_relevant" from previous step 
# returned data as "rds"
resp <- 
  httr::POST("https://api.heatwalkingcycling.org/apiv1/heat_5_0/calc_results?output_format=rds",
             httr::set_cookies("user" = Sys.getenv("HEAT_COOKIE")),
             body = jsonlite::toJSON(
               list("webapp_input" = webapp_input,
                    "generic_data_relevant" = genericdata)),
             encode = "multipart")

# convert to R
from_rds <- unserialize(httr::content(resp, "raw"))
# check the response
str(resp)

# Check final result value
res = as.numeric(gsub(" ","",from_rds$results$moneytotaldisc)) #remove spaces and convert to to numeric
res[26]-res[27]-res[28]+res[29]
#> [1] 235260000 
#> 
#> # ESTE É o unico que dá resultados semelhantes com o do UI, pois usa o genericdata (onde tem o occupancy car rate, velocidades assumidas, e % de shifting de que modos
# 


 # TRY TO USE OTHER VALUES
webapp_input[["raw_activemode_bike_ref_alltrips"]] # it use to be the sum of all TRIPS per day (not per person/day)

totaltripspax = 2.6
webapp_input[["raw_activemode_bike_ref_alltrips"]] = totaltripspax
webapp_input[["raw_activemode_bike_cf_alltrips"]] = totaltripspax
webapp_input[["raw_motorizedmode_car_ref_alltrips"]] = totaltripspax
webapp_input[["raw_motorizedmode_car_cf_alltrips"]] = totaltripspax


# DOESNT WORK
resp_reprex <- 
  httr::POST("https://api.heatwalkingcycling.org/apiv1/heat_5_0/calc_results?output_format=rds",
             httr::set_cookies("user" = Sys.getenv("HEAT_COOKIE")),
             body = jsonlite::toJSON(
               list("webapp_input" = webapp_input,
                    "generic_data_relevant" = genericdata)),
             encode = "multipart")

# convert to R
from_rds_reprex <- unserialize(httr::content(resp_reprex, "raw"))










