source("init.R")

# Format generation

get_generation <- function(
  countries,
  technologies
)
{
  generation <- rutils::read_from_mongo(
    collections$generation,
    filters = '"country": {"$in":%s}, "technology": {"$in":%s}' %>%
      sprintf(
        countries %>% jsonlite::toJSON(),
        technologies %>% jsonlite::toJSON()
      )
  )
  generation_fm <- generation %>%
    na.omit() %>%
    dplyr::filter(lubridate::minute(date) == 0) %>%
    dplyr::arrange(country, technology, date) %>%
    dplyr::group_by(country, technology) %>%
    tidyr::nest() %>%
    dplyr::ungroup()
  generation_fm
}

ren_generation <- get_generation(
  map_codes,
  ren_technologies
)

ren_generation %>%
  dplyr::filter(country == "FR", technology == "Solar") %>%
  dplyr::pull(data) %>%
  `[[`(1) %>%
  rutils::viz_df(size = 0.1)


ren_generation %>%
  dplyr::filter(technology == "Solar", country %in% c("FR", "DE", "ES")) %>%
  tidyr::unnest(data) %>%
  dplyr::distinct(date, technology, country, .keep_all = T) %>%
  tidyr::pivot_wider(
    id_cols = "date",
    names_from = c("technology", "country"),
    values_from = "production_mw"
  ) %>%
  rutils::viz_dff(size = 0.1)




# Format capacity

get_capacities <- function(
  country,
  technologies
){
  capacities <- rutils::read_from_mongo(
    collections$installed_capacity,
    filters = '"country": {"$in":%s}, "technology": {"$in":%s}' %>%
      sprintf(
        countries %>% jsonlite::toJSON(),
        technologies %>% jsonlite::toJSON()
      )
  )
  capacities_fm <- capacities %>%
    dplyr::arrange(country, technology, start) %>%
    dplyr::group_by(country, technology) %>%
    tidyr::nest() %>%
    dplyr::ungroup()
  capacities_fm
}

ren_capacities <- get_capacities(
  map_codes,
  ren_technologies
)

ren_capacities %>%
  dplyr::filter(technology == "Wind Onshore") %>%
  dplyr::pull(data) %>%
  `[[`(1)






