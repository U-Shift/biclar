library(magrittr)
library(devtools)

source("D:/GIS/TML/HEAT_heatr_api/R/models_paths.R")
source("D:/GIS/TML/HEAT_heatr_api/R/allowed_methods.R")
source("D:/GIS/TML/HEAT_heatr_api/R/parsers.R")
source("D:/GIS/TML/HEAT_heatr_api/R/serializer.R")

user_key <- "1525a17bfa86007eb7788adcec47a4ea62414b75"
log_file <- "./logs/plum.log"

# register custom parser

if(!("jsonlite_serialized" %in% plumber::registered_parsers())){
  
  plumber::register_parser("jsonlite_serialized",
                           parser_jsonlite_serialized,
                           fixed = "application/jsonl-serialized"
  )
}

function(req) {
  log_data <-
    paste0(
      as.character(Sys.time()), "--",
      req[["REQUEST_METHOD"]], "--",
      req[["PATH_INFO"]], "--",
      req[["HTTP_USER_AGENT"]], "--",
      req[["REMOTE_ADDR"]], "##",
      names(req[["cookies"]])
    )
  
  write(log_data, log_file, append = TRUE)
  
  if (req$PATH_INFO == "/apiv1/readme.html" ||
      req$PATH_INFO == "/apiv1/favicon.ico") {
    return(plumber::forward())
  }
  
  if ("cookies" %in% names(req) &&
      length(req[["cookies"]]) > 0 &&
      "user" %in% names(req[["cookies"]]) &&
      req[["cookies"]][["user"]] == user_key
  ) {
    return(plumber::forward())
  }
  
  return(list(error = "No cookie provided!"))
}
