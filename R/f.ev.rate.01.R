#' Calculate EV detection rate function
#'
#' Function to calculate the EV detection rate in sites from POLIS.
#'
#' @param es.data `tibble` ES data which includes site name (site.name),
#' country (ADM0_NAME),
#' date of collection (collect.date), and a binary ev detection variable (ev.detect)
#' that indicates absence/presence (0, 1) of enterovius in an ES sample. This is
#' `ctry.data$es` of [extract_country_data()] or [init_dr()], or `raw.data$es` of
#' [get_all_polio_data()].
#' @param start.date `str` Date in the format of `"YYYY-MM-DD"`.
#' @param end.date `str` Date in the format of `"YYYY-MM-DD"`.
#' @returns `tibble` Long format dataframe including site specific EV detection rates.
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' ctry.data <- extract_country_data("algeria", raw.data)
#' ev_rates <- f.ev.rate.01(ctry.data$es, "2021-01-01", "2023-12-31")
#'
#' @export
f.ev.rate.01 <- function(
    es.data,
    start.date,
    end.date) {
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop('Package "purrr" must be installed to use this function.',
      .call = FALSE
    )
  }

  # Analysis start and end date as defined by user (as a character)
  start.date <- lubridate::as_date(start.date)
  end.date <- lubridate::as_date(end.date)

  # Check to make sure that data has necessary variables
  necessary.es.vars <- c("collect.date", "ADM0_NAME", "site.name", "ev.detect")
  useful.es.vars <- c("ADM1_NAME", "ADM2_NAME", "lat", "lng")

  if (sum(!necessary.es.vars %in% names(es.data)) > 0) {
    missing.vars <- necessary.es.vars[!necessary.es.vars %in% names(es.data)]
    stop(paste0(
      "The following necessary variables were not found in `es.data`: ",
      paste0(missing.vars, collapse = ", ")
    ))
  }

  if (sum(!useful.es.vars %in% names(es.data)) > 0) {
    missing.vars <- useful.es.vars[!useful.es.vars %in% names(es.data)]
    readline(paste0(
      "The following useful variables were not found in `es.data`, please consider including them: ",
      paste0(missing.vars, collapse = ", "), "\n Press [ENTER] to continue"
    ))
  }

  # Warning message about non-overlapping dates
  if (start.date < lubridate::as_date(es.data$collect.date |> min(na.rm = T))) {
    print(paste0(
      "Your specified start date is ",
      start.date,
      ", however the first date of ES data available is ",
      es.data$collect.date |> min(na.rm = T),
      ". Please use caution interpreting any values calculated before ",
      es.data$collect.date |> min(na.rm = T)
    ))
  }

  # Limit ES data to the range described by the analysis start and end dates
  es.data <- es.data |>
    dplyr::filter(dplyr::between(collect.date, start.date, end.date))

  # Create site-level summary
  es.sum <- es.data |>
    dplyr::group_by(ADM0_NAME, site.name) |>
    dplyr::summarize(
      num.samples = dplyr::n(),
      num.ev.pos = sum(ev.detect == 1, na.rm = T)
    ) |>
    dplyr::mutate(
      ev.rate = num.ev.pos / num.samples,
      ev.percent = purrr::map_chr(ev.rate, scales::label_percent(accuracy = 1)),
      ev.rate.cat = dplyr::case_when(
        ev.rate < .50 ~ "<50%",
        ev.rate >= .50 & ev.rate < .80 ~ "50 to <80%",
        ev.rate >= .80 ~ "80-100%"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      start.date = start.date,
      end.date = end.date,
      interval = end.date - start.date
    )

  es.sum <- dplyr::left_join(
    es.sum,
    es.data |>
      dplyr::select(dplyr::any_of(c("site.name",
        "ADM0_NAME", "ADM1_NAME", "ADM2_NAME",
        "adm0guid" = "ctry.guid",
        "adm1guid" = "prov.guid",
        "adm2guid" = "dist.guid",
        "lat", "lng"
      ))) |>
      dplyr::distinct()
  )

  return(es.sum) # Data frame to return from function
}
