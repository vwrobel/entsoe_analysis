source("init.R")

start_date <- "2014-12-25" %>%
  lubridate::ymd(tz = tz)
end_date <- "2022-01-05 23:00" %>%
  lubridate::ymd_hm(tz = tz)
start_year <- start_date %>%
  lubridate::year()
end_year <- end_date %>%
  lubridate::year()

loading_period <- lubridate::days(10)

# Load generation

map_codes <- "GB"

for (map_code in map_codes){

  print("starting generation data load for country %s" %>% sprintf(map_code))

  country_code <- country_codes %>%
    dplyr::filter(MapCode == map_code) %>%
    dplyr::pull(AreaCode)

  start_date_it <- start_date
  end_date_it <- start_date_it + loading_period - lubridate::hours(1)

  while (end_date_it <= end_date){
    print("start_date_it is %s" %>% sprintf(start_date_it))

    try({
      generation <- en_generation_agg_gen_per_type(
        eic = country_code,
        period_start = start_date_it,
        period_end = end_date_it
      )
      generation_fm <- generation %>%
        dplyr::right_join(
          en_codes,
          by = c("MktPSRType" = "codes")
        ) %>%
        dplyr::select(meaning, dt, quantity) %>%
        dplyr::rename(date = dt, technology = meaning, production_mw = quantity) %>%
        dplyr::mutate(country = map_code) %>%
        dplyr::relocate(country, .before = 1)
      generation_fm %>%
        collections$generation$insert()
    }, silent = TRUE)

    start_date_it <- end_date_it + lubridate::hours(1)
    end_date_it <- start_date_it + loading_period - lubridate::hours(1)
  }
}


# Load consumption

for (map_code in map_codes){

  print("starting consumption data load for country %s" %>% sprintf(map_code))

  country_code <- country_codes %>%
    dplyr::filter(MapCode == map_code) %>%
    dplyr::pull(AreaCode)

  start_date_it <- start_date
  end_date_it <- start_date_it + loading_period - lubridate::hours(1)

  while (end_date_it <= end_date){
    print("start_date_it is %s" %>% sprintf(start_date_it))

    try({
      consumption <- en_load_actual_total_load(
        eic = country_code,
        period_start = start_date_it,
        period_end = end_date_it
      )
      consumption_fm <- consumption %>%
        dplyr::select(dt, quantity) %>%
        dplyr::rename(date = dt, consumption_mw = quantity) %>%
        dplyr::mutate(country = map_code) %>%
        dplyr::relocate(country, .before = 1)
      consumption_fm %>%
        collections$consumption$insert()
    }, silent = TRUE)

    start_date_it <- end_date_it + lubridate::hours(1)
    end_date_it <- start_date_it + loading_period - lubridate::hours(1)
  }
}


# Load installed capacity

for (map_code in map_codes){

  print("starting capacity data load for country %s" %>% sprintf(map_code))

  country_code <- country_codes %>%
    dplyr::filter(MapCode == map_code) %>%
    dplyr::pull(AreaCode)

  year_it <- start_year

  while (year_it <= end_year){
    print("year_it is %s" %>% sprintf(year_it))

    try({
      installed_capacity <- en_generation_inst_gen_cap_agg(
        country_code,
        year_it
      )
      installed_capacity_fm <- installed_capacity %>%
        dplyr::right_join(
          en_codes,
          by = c("psr_type" = "codes")
        ) %>%
        dplyr::select(meaning, start, end, quantity) %>%
        dplyr::rename(technology = meaning, capacity_mw = quantity) %>%
        dplyr::mutate(country = map_code) %>%
        dplyr::relocate(country, .before = 1)

      installed_capacity_fm %>%
        collections$installed_capacity$insert()
    })

    year_it <- year_it + 1

  }
}

