#' Imputes missing site coordinates in ES data
#'
#' Adds coordinates for ES surveillance sites missing them. This function is not meant
#' to be exported. This is part of [clean_es_data()].
#'
#' @import cli dplyr sf
#' @importFrom purrr map
#' @param ctry.data
#' `r lifecycle::badge("deprecated")`
#' `list` This parameter has been deprecated in favor of explicitly passing dataframes
#' into the function. This allows for greater flexibility in the function.
#'
#' @return `tibble` ES data with imputed coordinates for sites missing them.
#' @keywords internal
impute_site_coord <- function(es.data, dist.shape, ctry.data = lifecycle::deprecated()) {

  if (lifecycle::is_present(ctry.data)) {
    lifecycle::deprecate_warn(
      when = "1.3.0",
      what = "impute_site_coord(ctry.data)",
      details = "Please pass es.data and dist.shape directly instead of a list."
    )
  }
  # Add column to flag imputed coordinates
  es.data <- es.data |>
    dplyr::mutate(imputed_coord = dplyr::if_else(is.na(.data$lat) | is.na(.data$lng),
                                                 TRUE, FALSE))
  missing_coords <- es.data |>
    dplyr::filter(is.na(lat) | is.na(lng)) |>
    dplyr::distinct(.data$ADM0_NAME, .data$site.id, .data$site.name, .data$dist.guid,
                    .data$lat, .data$lng, .data$imputed_coord)
  before <- nrow(missing_coords)

  cli::cli_process_start("Imputing missing site coordinates based on district")


  if (is.null(dist.shape)) {
    message("Please attach spatial data")
    return(NULL)
  }

  dist.shape <- dist.shape |>
    dplyr::filter(!sf::st_is_empty(.data$SHAPE),
                  .data$GUID %in% missing_coords$dist.guid)
  missing_guids <- setdiff(missing_coords$dist.guid, dist.shape$GUID)

  if (length(missing_guids) != 0) {
    cli::cli_alert_warning("Unable to find shapefile of site(s) with the associated adm2guid(s)")
    for (i in missing_guids) {
      cli::cli_alert_info(paste0("adm2guid: ", i))
    }
  }

  st_sample_modified <- function(x) {
    tryCatch(
      expr = sf::st_sample(x, 1),
      error = {
        suppressMessages({
          sf::sf_use_s2(F)
          result <- sf::st_sample(x, 1)
          sf::sf_use_s2(T)
          return(result)
        })
      }
    )
  }

  missing_coords <- missing_coords |>
    dplyr::left_join(dist.shape, by = c("dist.guid" = "GUID",
                                        "ADM0_NAME")) |>
    # Must filter because st_sample() doesn't work with NULL values
    dplyr::filter(!is.na(.data$dist.guid)) |>
    dplyr::rowwise() |>
    dplyr::mutate(sampled_point = purrr::map(.data$SHAPE, st_sample_modified)) |>
    tidyr::unnest(c("SHAPE", "sampled_point")) |>
    dplyr::mutate(lat = sf::st_coordinates(.data$sampled_point)[,1],
                  lng = sf::st_coordinates(.data$sampled_point)[,2]) |>
    dplyr::select(dplyr::any_of(names(missing_coords)))

  es.data <- es.data |>
    dplyr::left_join(missing_coords,
                     dplyr::join_by("site.id", "site.name", "ADM0_NAME",
                                    "dist.guid", "imputed_coord")) |>
    dplyr::mutate(lat = dplyr::coalesce(as.numeric(.data$lat.x), .data$lat.y),
                  lng = dplyr::coalesce(as.numeric(.data$lng.x), .data$lng.y)) |>
    dplyr::select(-dplyr::ends_with(".x"), -dplyr::ends_with(".y"))

  # Final check
  no_coordinates <- es.data.test |>
    dplyr::filter(is.na(.data$lat) | is.na(.data$lng)) |>
    select(dplyr::any_of(names(missing_coords))) |>
    dplyr::distinct()

  if (nrow(no_coordinates) != 0) {
    cli::cli_alert_danger(paste0("Imputation failed for these sites:\n",
                               paste(unique(no_coordinates$site.name), collapse = ",\n")))
  }

  after <- nrow(no_coordinates)
  cli::cli_process_done()
  cli::cli_alert_success(paste0("Imputation success rate: ",
                                round((before - after) / before * 100, 2), "%"))

  return(es.data)
}

#' Clean environmental surveillance data
#'
#' The cleaning step will attempt to impute missing site coordinates and create
#' standardized columns used in the desk review.
#'
#' @import cli dplyr tidyr
#' @param es.data `tibble` Environmental surveillance data.
#' @param dist.shape `sf` District shapefile.
#' @param ctry.data
#' `r lifecycle::badge("deprecated")`
#' `list` This parameter has been deprecated in favor of explicitly passing dataframes
#' into the function. This allows for greater flexibility in the function.
#' @return `tibble` Cleaned environmental surveillance data.
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' ctry.data <- extract_country_data("algeria", raw.data)
#' ctry.data$es <- clean_es_data(ctry.data$es, ctry.data$dist)
#'
#' @export
clean_es_data <- function(es.data, dist.shape, ctry.data = lifecycle::deprecated()) {
  if (lifecycle::is_present(ctry.data)) {
    lifecycle::deprecate_warn(
      when = "1.2.0",
      what = "clean_es_data(ctry.data)",
      details = "Please pass ES data directly using the es.data parameter."
    )
  }

  cli::cli_process_start("Checking for missing site coordinates")

  df01 <- es.data |>
    dplyr::distinct(
      .data$ADM0_NAME, .data$site.name,
      .data$dist.guid, .data$lat, .data$lng
    ) |>
    dplyr::filter(is.na(lat) | is.na(lng))

  if (nrow(df01) != 0) {
    cli::cli_alert_warning("These sites are missing coordinates:")

    for (i in df01$site.name) {
      cli::cli_alert_info(i)
    }
    es.data <- impute_site_coord(es.data, dist.shape)
  } else {
    cli::cli_alert_success("No sites with missing coordinates.")
  }

  cli::cli_process_done()

  cli::cli_process_start("Cleaning ES data")
  es.data.earlidat <- es.data |>
    dplyr::group_by(.data$site.name) |>
    dplyr::summarize(early.dat = min(collect.date)) |>
    dplyr::ungroup()

  es.data <- es.data |>
    dplyr::left_join(es.data.earlidat, by = c("site.name" = "site.name")) |>
    dplyr::mutate(nvaccine.2 = NA)

  es.data <- es.data |>
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
    )) |>
    tidyr::unite("all_dets", c(
      "vaccine.1.3", "vaccine.3.1", "vaccine.2",
      "vdpv.1", "vdpv.2", "vdpv.3", "wild.1",
      "wild.3", "nvaccine.2"
    ),
    na.rm = TRUE, remove = FALSE, sep = " and "
    ) |>
    dplyr::mutate(all_dets = dplyr::case_when(
      all_dets == "Sabin 1" ~ "Sabin 1 or Sabin 3",
      all_dets == "Sabin 3" ~ "Sabin 1 or Sabin 3",
      all_dets == "Sabin 1 and Sabin 3" ~ "Sabin 1 or Sabin 3",
      TRUE ~ all_dets
    ))

  es.data <- es.data |>
    dplyr::mutate(all_dets = gsub(
      "Sabin 1/3 and Sabin 1/3", "Sabin 1/3",
      .data$all_dets
    ))

  es.data <- es.data |>
    dplyr::mutate(
      all_dets = dplyr::case_when(
        all_dets == "" & npev == 1 ~ "NPEV only",
        all_dets == "" & ev.detect == 0 ~ "No EV isolated",
        TRUE ~ all_dets
      ),
      year = lubridate::year(.data$collect.date)
    )

  cli::cli_process_done()

  return(es.data)
}

# Deprecated functions ----
#' Generate ES data with viral detection columns
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' The function importantly adds the `ed.detect`, `all_dets`, and `year` columns to the
#' environmental surveillance data.
#'
#' @details
#' This function will is now part of the [clean_es_data()].
#'
#' @import dplyr lubridate
#' @param es.data `tibble` ES data.
#'
#' @return `tibble` ES data with viral detection columns.
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' ctry.data <- extract_country_data("algeria", raw.data)
#' ctry.data$es <- clean_es_data(ctry.data)
#' es.data.long <- generate_es_data_long(ctry.data$es)
#'
#' @keywords internal
generate_es_data_long <- function(es.data) {
  lifecycle::deprecate_warn(
    "1.3.0",
    "generate_es_data_long()",
    details = "This function is now part of clean_es_data()."
  )

  es.data.long <- es.data %>%
    dplyr::select(
      "site.name", "ADM1_NAME", "collect.date",
      "early.dat", "ev.detect", "all_dets",
      "npev"
    ) %>%
    dplyr::mutate(ev.detect = as.character(.data$ev.detect)) %>%
    dplyr::mutate(all_dets = dplyr::case_when(
      all_dets == "" & npev == "1" ~ "NPEV only",
      all_dets == "" & ev.detect == "0" ~ "No EV isolated",
      TRUE ~ all_dets
    ))

  es.data.long$year <- lubridate::year(es.data.long$collect.date)

  return(es.data.long)
}
