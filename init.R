library(tidyverse)
library(entsoeapi)


source("functions.R")

# Connection to Mongo

collection_names <- c(
  "installed_capacity",
  "generation",
  "consumption"
)

collections <- collection_names %>%
  lapply(rutils::get_mongo_con) %>%
  setNames(collection_names)

# Get ENTSOE reference data

country_codes <- en_eic() %>%
  filter(AreaTypeCode == "CTY") %>%
  dplyr::arrange(AreaName)

en_codes <- en_generation_codes() %>%
  dplyr::select(codes, meaning)

tz <- "CET"

map_codes <- country_codes$MapCode

ren_technologies <- c("Wind Offshore", "Wind Onshore", "Solar")

rds_dir <- "rds"
