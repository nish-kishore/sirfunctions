#' Transform ES data cleaning with additional columns
#'
#' @param es.data tibble of ES data from ctry.data
#' @param start_date start date of analysis
#' @param end_date end date of analysis
#'
#' @return tibble of cleaned ES data
clean_es_data <- function(es.data, es_start_date, es_end_date) {

  cli::cli_process_start("Cleaning ES data")
  es.data.earlidat <- es.data %>%
    group_by(site.name) %>%
    summarize(early.dat = min(collect.date)) %>%
    ungroup()

  start_date <- as_date(es_start_date)
  end_date <- as_date(es_end_date)

  es.data <- es.data %>%
    filter(between(collect.date, start_date, end_date)) %>%
    left_join(es.data.earlidat, by = c("site.name" = "site.name")) |>
    mutate(nvaccine.2 = NA)

  es.data <- es.data %>%
    mutate(vaccine.1.3 = case_when(
      is.na(vaccine.1) == F & vaccine.1 == "Yes" ~ "Sabin 1/3"
    )) %>%
    mutate(vaccine.3.1 = case_when(
      is.na(vaccine.3) == F & vaccine.3 == "Yes" ~ "Sabin 1/3"
    )) %>%
    mutate(vaccine.2 = case_when(
      is.na(vaccine.2) == F & vaccine.2 == "Yes" ~ "Sabin 2"
    )) %>%
    mutate(vdpv.1 = case_when(
      is.na(vdpv.1) == F & vdpv.1 == "Yes" ~ "VDPV1"
    )) %>%
    mutate(vdpv.2 = case_when(
      is.na(vdpv.2) == F & vdpv.2 == "Yes" ~ "VDPV2"
    )) %>%
    mutate(vdpv.3 = case_when(
      is.na(vdpv.3) == F & vdpv.3 == "Yes" ~ "VDPV3"
    )) %>%
    mutate(wild.1 = case_when(
      is.na(wild.1) == F & wild.1 == "Yes" ~ "WPV1"
    )) %>%
    mutate(wild.3 = case_when(
      is.na(wild.3) == F & wild.3 == "Yes" ~ "WPV3"
    )) %>%
    mutate(nvaccine.2 = case_when(
      is.na(nvaccine.2) == F & nvaccine.2 == "Yes" ~ "nOPV2"
    )) %>%
    unite("all_dets", c(
      "vaccine.1.3", "vaccine.3.1", "vaccine.2",
      "vdpv.1", "vdpv.2", "vdpv.3", "wild.1",
      "wild.3", "nvaccine.2"
    ),
    na.rm = TRUE, remove = FALSE, sep = " and "
    ) %>%
    mutate(all_dets = case_when(
      all_dets == "Sabin 1" ~ "Sabin 1 or Sabin 3",
      all_dets == "Sabin 3" ~ "Sabin 1 or Sabin 3",
      all_dets == "Sabin 1 and Sabin 3" ~ "Sabin 1 or Sabin 3",
      TRUE ~ all_dets
    ))

  es.data$all_dets = gsub("Sabin 1/3 and Sabin 1/3", "Sabin 1/3", es.data$all_dets)
  cli::cli_process_done()

  return(es.data)
}

#' Generate ES data with viral detection columns
#'
#' @param es.data tibble containing ES data
#'
#' @return tibble of ES data with viral detection columns
#' @export
generate_es_data_long <- function(es.data) {
  es.data.long <- es.data %>%
    select(site.name, ADM1_NAME, collect.date, early.dat, ev.detect, all_dets) %>%
    mutate(ev.detect = as.character(ev.detect)) %>%
    mutate(all_dets = case_when(
      all_dets == "" & ev.detect == "1" ~ "NPEV only",
      all_dets == "" & ev.detect == "0" ~ "No EV isolated",
      TRUE ~ all_dets
    )
    )

  es.data.long$year = year(es.data.long$collect.date)

  return(es.data.long)
}
