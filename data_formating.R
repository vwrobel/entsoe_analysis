source("init.R")

start_date <- "2015-01-01" %>%
  lubridate::ymd(tz = tz)
end_date <- "2021-12-31 23:00" %>%
  lubridate::ymd_hm(tz = tz)

# Format generation

get_generation <- function(
  countries,
  technologies,
  from = NULL,
  to = NULL
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
    dplyr::filter(lubridate::minute(date) == 0)
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

sub_ren_generation <- ren_generation %>%
  dplyr::filter(
    date >= start_date,
    date <= end_date,
    country %in% country_sel
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


# Format consumption

get_consumption <- function(
  countries
)
{
  consumption <- rutils::read_from_mongo(
    collections$consumption,
    filters = '"country": {"$in":%s}' %>%
      sprintf(
        countries %>% jsonlite::toJSON()
      )
  )
  consumption_fm <- consumption %>%
    na.omit() %>%
    dplyr::filter(lubridate::minute(date) == 0) %>%
    dplyr::arrange(country, date) %>%
    dplyr::group_by(country) %>%
    tidyr::nest() %>%
    dplyr::ungroup()
  consumption_fm
}

consumption <- get_consumption(
  map_codes
)

country_sel <- c("FR", "DE", "ES")
country_sel <- map_codes

consumption_clean <- consumption %>%
  dplyr::mutate(
    data = data %>%
      purrr::map(
        function(x){
          browser()
          x %>%
            rutils::add_outlier_column("consumption_mw", nb_sd = 2) %>%
            dplyr::mutate(
              consumption_mw = ifelse(
                outlier,
                NA,
                consumption_mw
              )
            ) %>%
            dplyr::select(- outlier)
        }
      )
  ) %>%
  tidyr::unnest(data) %>%
  dplyr::distinct(date, country, .keep_all = T) %>%
  dplyr::filter(
    date >= start_date,
    date <= end_date,
    country %in% country_sel
  )

consumption_large <- consumption_clean %>%
  tidyr::pivot_wider(
    id_cols = "date",
    names_from = c("country"),
    values_from = "consumption_mw"
  )

consumption_p <- consumption_large %>%
  rutils::viz_tss(title_from_col = T)
  #rutils::viz_dff(size = 0.1, height = length(map_codes) * 200)

consumption_p %>%
  rutils::save_html_to_path(file.path(graphs_dir, "consumption_p.html"))


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






