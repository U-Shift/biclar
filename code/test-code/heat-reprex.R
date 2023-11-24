# Heat reprex

# read RDS exported from UI - NEW VERSION
webapp_input = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/2023b_webapp_input.rds"))
genericdata = readRDS(url("https://web.tecnico.ulisboa.pt/~rosamfelix/heat/2023b_generic_data_relevant_with_calc_params.rds"))

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
from_rds_reprex <- unserialize(httr::content(resp, "raw"))

# Check final result value
from_rds_reprex$results$impactperyearave[40] #mortality per year - should be 119 preventive deaths
from_rds_reprex$results$moneytotaldisc[36]+from_rds_reprex$results$moneytotaldisc[37]+from_rds_reprex$results$moneytotaldisc[38]+from_rds_reprex$results$moneytotaldisc[39]
