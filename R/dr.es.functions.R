#' Adds coordinates for ES surveillance sites missing them
#' @import cli dplyr sf
#' @param ctry.data RDS object containing country polio data
#'
#' @return tibble of ES data with imputed coordinates for sites missing them
impute_site_coord <- function(ctry.data) {
  df01 <- ctry.data$es %>%
    dplyr::distinct(.data$ADM0_NAME, .data$site.name, .data$dist.guid, .data$lat, .data$lng) |>
    dplyr::filter(is.na(lat) | is.na(lng))

  cli::cli_process_start("Adding coordinates to sites in their home district")

  if (is.null(ctry.data$dist)) {
    message("ctry.data does not have spatial data. Please attach spatial data to continue")
    return(NULL)
  }

  dist.shape <- ctry.data$dist
  shape.dist.pop <- dplyr::left_join(dist.shape,
    ctry.data$dist.pop |> dplyr::filter(year == max(year)),
    by = c("GUID" = "adm2guid")
  )

  df01.shape <- dplyr::right_join(shape.dist.pop %>% dplyr::select("GUID"),
    df01 %>% dplyr::filter(!is.na(dist.guid)),
    by = c("GUID" = "dist.guid")
  ) %>%
    dplyr::mutate(empty.01 = sf::st_is_empty(.)) %>%
    dplyr::filter(empty.01 == 0) |>
    dplyr::distinct()

  if (nrow(df01.shape) == 0) {
    cli::cli_alert_warning("Unable to find shapefile associated with site(s)")
    return(ctry.data$es)
  }

  df02 <- df01.shape %>%
    dplyr::group_by(.data$GUID) %>%
    dplyr::summarize(nperarm = dplyr::n()) %>%
    dplyr::arrange(.data$GUID) %>%
    dplyr::mutate(id = dplyr::row_number())


  pt01 <- sf::st_sample(df02, df02$nperarm, exact = TRUE)
  pt01_sf <- sf::st_sf(pt01)
  pt01_joined <- sf::st_join(pt01_sf, df02)


  df03 <- pt01_joined %>%
    dplyr::select(-"nperarm", -"id") %>%
    dplyr::group_by(.data$GUID) %>%
    dplyr::arrange(.data$GUID, .by_group = TRUE) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    as.data.frame()


  df04 <- df01.shape %>%
    dplyr::group_by(.data$GUID) %>%
    dplyr::arrange(.data$GUID, .by_group = TRUE) %>%
    dplyr::mutate(id = dplyr::row_number())


  df05 <- dplyr::full_join(df04, df03) %>%
    dplyr::filter(!is.na(empty.01)) %>%
    tidyr::separate(.,
      col = pt01,
      into = c("lon", "lat"),
      sep = "[,]"
    ) %>%
    dplyr::mutate(
      lon = readr::parse_number(.data$lon),
      lat = readr::parse_number(.data$lat),
    )


  sf::st_geometry(df05) <- NULL


  df06 <- df05 %>%
    dplyr::select("ADM0_NAME", "site.name", "lng" = "lon", "lat") %>%
    dplyr::mutate_at(c("lng", "lat"), as.character)

  es.data <- ctry.data$es %>%
    dplyr::left_join(., df06, by = c("ADM0_NAME", "site.name")) %>%
    dplyr::mutate(
      lat = ifelse(is.na(.data$lat.x), .data$lat.y, .data$lat.x),
      lng = ifelse(is.na(.data$lng.x), .data$lng.y, .data$lng.x)
    ) %>%
    dplyr::select(-c("lat.x", "lat.y", "lng.x", "lng.y"))

  cli::cli_process_done()

  return(es.data)
}

#' Transform ES data cleaning with additional columns
#'
#' @param ctry.data RDS object containing country polio data
#' @import cli dplyr tidyr
#' @return tibble of cleaned ES data
#' @export
clean_es_data <- function(ctry.data) {
  es.data <- ctry.data$es
  cli::cli_process_start("Checking for missing site coordinates")

  df01 <- es.data |>
    dplyr::distinct(.data$ADM0_NAME, .data$site.name, .data$dist.guid, .data$lat, .data$lng) %>%
    dplyr::filter(is.na(lat) | is.na(lng))

  if (nrow(df01) != 0) {
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
    dplyr::group_by(.data$site.name) %>%
    dplyr::summarize(early.dat = min(collect.date)) %>%
    dplyr::ungroup()

  es.data <- es.data %>%
    dplyr::left_join(es.data.earlidat, by = c("site.name" = "site.name")) |>
    dplyr::mutate(nvaccine.2 = NA)

  es.data <- es.data %>%
    dplyr::mutate(vaccine.1.3 = dplyr::case_when(
      is.na(vaccine.1) == F & vaccine.1 == "Yes" ~ "Sabin 1/3"
    )) %>%
    dplyr::mutate(vaccine.3.1 = dplyr::case_when(
      is.na(vaccine.3) == F & vaccine.3 == "Yes" ~ "Sabin 1/3"
    )) %>%
    dplyr::mutate(vaccine.2 = dplyr::case_when(
      is.na(vaccine.2) == F & vaccine.2 == "Yes" ~ "Sabin 2"
    )) %>%
    dplyr::mutate(vdpv.1 = dplyr::case_when(
      is.na(vdpv.1) == F & vdpv.1 == "Yes" ~ "VDPV1"
    )) %>%
    dplyr::mutate(vdpv.2 = dplyr::case_when(
      is.na(vdpv.2) == F & vdpv.2 == "Yes" ~ "VDPV2"
    )) %>%
    dplyr::mutate(vdpv.3 = dplyr::case_when(
      is.na(vdpv.3) == F & vdpv.3 == "Yes" ~ "VDPV3"
    )) %>%
    dplyr::mutate(wild.1 = dplyr::case_when(
      is.na(wild.1) == F & wild.1 == "Yes" ~ "WPV1"
    )) %>%
    dplyr::mutate(wild.3 = dplyr::case_when(
      is.na(wild.3) == F & wild.3 == "Yes" ~ "WPV3"
    )) %>%
    dplyr::mutate(nvaccine.2 = dplyr::case_when(
      is.na(nvaccine.2) == F & nvaccine.2 == "Yes" ~ "nOPV2"
    )) %>%
    tidyr::unite("all_dets", c(
      "vaccine.1.3", "vaccine.3.1", "vaccine.2",
      "vdpv.1", "vdpv.2", "vdpv.3", "wild.1",
      "wild.3", "nvaccine.2"
    ),
    na.rm = TRUE, remove = FALSE, sep = " and "
    ) %>%
    dplyr::mutate(all_dets = dplyr::case_when(
      all_dets == "Sabin 1" ~ "Sabin 1 or Sabin 3",
      all_dets == "Sabin 3" ~ "Sabin 1 or Sabin 3",
      all_dets == "Sabin 1 and Sabin 3" ~ "Sabin 1 or Sabin 3",
      TRUE ~ all_dets
    ))

  es.data$all_dets <- gsub("Sabin 1/3 and Sabin 1/3", "Sabin 1/3", es.data$all_dets)
  cli::cli_process_done()

  return(es.data)
}

#' Generate ES data with viral detection columns
#' @import dplyr lubridate
#' @param es.data tibble containing ES data
#'
#' @return tibble of ES data with viral detection columns
#' @export
generate_es_data_long <- function(es.data) {
  es.data.long <- es.data %>%
    dplyr::select("site.name", "ADM1_NAME", "collect.date",
                  "early.dat", "ev.detect", "all_dets",
                  "npev") %>%
    dplyr::mutate(ev.detect = as.character(.data$ev.detect)) %>%
    dplyr::mutate(all_dets = dplyr::case_when(
      all_dets == "" & npev == "1" ~ "NPEV only",
      all_dets == "" & ev.detect == "0" ~ "No EV isolated",
      TRUE ~ all_dets
    ))

  es.data.long$year <- lubridate::year(es.data.long$collect.date)

  return(es.data.long)
}
