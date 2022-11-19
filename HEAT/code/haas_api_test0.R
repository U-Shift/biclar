# HEAT API tests

# ucc_
# The UI collects "scenario" details (aka use case criteria) (incl. geographic location, selected modes of transport,
# type of assessment, etc.). These UI inputs are prefixed with ucc_ (for use case criteria).
# In the UI all validity checks and warnings are carried out - here and only here at the present moment
# (incl. maximum levels of active travel, minimum population size, etc.)


# webapp_inputs - this is a RDS file which should be downloaded from the "Export page" of the UI.
# It stores UI inputs with values. The values of this can be modified before they are sent to the API.
# Please modify these with great care. Since the business logic is not a part of the model itself,
# improper combination or values of variables (inputs) won't be detected, resulting in incorrect results.
# The general rule is that any ucc_ UI inputs should not be modified manually
# (since these shape the UI pages and the layout of input fields, and the corresponding collection of UI
# inputs with values and the data structures used by the model). Any other values of inputs can be changed 
# - such as numeric values. Still, constraints which are available in the UI (as warnings or errors)
# should be respected to get reliable results from the model. Note that webapp_inputs as RDS has to
# be downloaded from the UI corresponding to the model version used (at the present moment only heat_5_0 is available).
# Please use "Inspect" tool in the web browser to get the names of the inputs.
# raw_





# 1304

value10yrs = 395000000
value10yrs / sum(HEAT13_bike$new_cyc4) 
#4819.92 € / novo ciclista

heat1304_webappinput = readRDS("HEAT/1304_webapp_input.rds")
heat1304_genericdata = readRDS("HEAT/1304_generic_data_relevant_with_calc_params.rds")


## Input and returned data as json

# read RDS exported from UI
webapp_input <- heat1304_webappinput

# to get data prepared by "results"
# input data as "json"
# returned data as "json"
resp <- 
  httr::POST("https://api.heatwalkingcycling.org/apiv1/heat_5_0/results?output_format=json",
             httr::set_cookies("user" = user_key),
             body = jsonlite::toJSON(list("webapp_input" = webapp_input)),
             encode = "multipart")

# convert from "json" to R format
from_json <- jsonlite::fromJSON(httr::content(resp, "text")) #parse?


# convert from "jsonl-serialized" to R format
from_serialized_json <- jsonlite::unserializeJSON(httr::content(resp, "text"))

#Error in if (encoding.mode == "NULL") { : argument is of length zero



## Input and returned data as rds, two inputs

