impute_site_coord <- function(ctry.data) {
  df01 <- ctry.data$es %>%
    distinct(ADM0_NAME, site.name, dist.guid, lat, lng) %>%
    filter(is.na(lat) | is.na(lng))

  cli::cli_process_start("Adding coordinates to sites in their home district")

  if (is.null(ctry.data$dist)) {
    message("ctry.data does not have spatial data. Please attach spatial data to continue")
    return(NULL)
  }

  dist.shape <- ctry.data$dist
  shape.dist.pop <- left_join(dist.shape,
                              ctry.data$dist.pop |> filter(year == max(year)),
                              by = c("GUID" = "adm2guid"))

  df01.shape <- right_join(shape.dist.pop %>% select(GUID),
                           df01 %>% filter(!is.na(dist.guid)),
                           by = c("GUID" = "dist.guid")) %>%
    mutate(empty.01 = st_is_empty(.)) %>%
    filter(empty.01 == 0) |>
    distinct()

  if(nrow(df01.shape) == 0) {
    cli::cli_alert_warning("Unable to find shapefile associated with site(s)")
    return(ctry.data$es)
  }

  df02 <- df01.shape %>%
    group_by(GUID) %>%
    summarise(nperarm = n()) %>%
    arrange(GUID) %>%
    mutate(id = row_number())


  pt01 <- st_sample(df02, df02$nperarm, exact = TRUE)
  pt01_sf <- st_sf(pt01)
  pt01_joined <- st_join(pt01_sf, df02)


  df03 <- pt01_joined %>%
    select(-nperarm, -id) %>%
    group_by(GUID) %>%
    arrange(GUID, .by_group = TRUE) %>%
    mutate(id = row_number()) %>%
    as.data.frame()


  df04 <- df01.shape %>%
    group_by(GUID) %>%
    arrange(GUID, .by_group = TRUE) %>%
    mutate(id = row_number())


  df05 <- full_join(df04, df03) %>%
    filter(!is.na(empty.01)) %>%
    separate(.,
             col = pt01,
             into = c("lon", "lat"),
             sep = "[,]") %>%
    mutate(lon = readr::parse_number(lon),
           lat = readr::parse_number(lat),)


  st_geometry(df05) <- NULL


  df06 <- df05 %>%
    select(ADM0_NAME, site.name, "lng" = lon, lat) %>%
    mutate_at(c("lng", "lat"), as.character)

  es.data <- ctry.data$es %>%
    left_join(., df06, by = c("ADM0_NAME", "site.name")) %>%
    mutate(lat = ifelse(is.na(lat.x), lat.y, lat.x),
           lng = ifelse(is.na(lng.x), lng.y, lng.x)) %>%
    select(-c(lat.x, lat.y, lng.x, lng.y))

  cli::cli_process_done()

  return(es.data)

}

#' Transform ES data cleaning with additional columns
#'
#' @param es.data tibble of ES data from ctry.data
#'
#' @return tibble of cleaned ES data
clean_es_data <- function(ctry.data) {
  es.data <- ctry.data$es
  cli::cli_process_start("Checking for missing site coordinates")

  df01 <- es.data |>
    distinct(ADM0_NAME, site.name, dist.guid, lat, lng) %>%
    filter(is.na(lat) | is.na(lng))

  if(nrow(df01) != 0) {
    cli::cli_alert_warning("These sites are missing coordinates:")

    for (i in df01$site.name) {
      cli::cli_alert_info(i)
    }
    es.data <- impute_site_coord(ctry.data)

  } else {
    cli::cli_alert_success("No sites with missing coordinates.")
  }

  cli::cli_process_done()

  cli::cli_process_start("Cleaning ES data")
  es.data.earlidat <- es.data %>%
    group_by(site.name) %>%
    summarize(early.dat = min(collect.date)) %>%
    ungroup()

  es.data <- es.data %>%
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
