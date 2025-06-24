# Private functions ----

#' Find established sites
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Finds established ES sites defined as those that has been active for for
#' at least 12 months since the end date with at least 10 collections.
#'
#'
#' @param es_data `tibble` ES data
#' @param end_date `str` End date to anchor analysis to
#' @returns `tibble` A summary of established ES sites
#' @keywords internal
get_es_site_age <- function(es_data, end_date) {
  end_date <- lubridate::as_date(end_date)

  established_sites <- es_data |>
    dplyr::select(ADM0_NAME, collect.date, site.name) |>
    dplyr::filter(collect.date <= end_date) |>
    dplyr::group_by(ADM0_NAME, site.name) |>
    # n_samples_12_mo is strictly looking at samples collected in the past 12 months
    # this will be different from num.samples in the f.ev.rate.01() calculation
    # if that calculation does not analyze a 12 month rolling period
    dplyr::summarize(n_samples_12_mo = sum(dplyr::between(collect.date,
                                                          end_date %m-% months(12, FALSE) %m+% days(1),
                                                          end_date)),
                     sampling_interval = lubridate::interval(min(collect.date),
                                                    max(collect.date)),
                     site_age = sampling_interval / months(1, FALSE)
                     ) |>
    dplyr::ungroup() |>
    dplyr::arrange(ADM0_NAME, site.name)

  return(established_sites)

  }



#' Add the risk category based on the country
#'
#' @param df `tibble` Dataframe with at least a column for country
#' @param risk_table  `tibble` Risk category table for each country. Defaults to `NULL`.
#' When set to `NULL`, attempts to download the risk category table from EDAV.
#'
#' @returns `tibble` A dataframe with risk category columns added.
#' @keywords internal
#'
add_risk_category <- function(df, risk_table = NULL, ctry_col = "ctry") {
  if (is.null(risk_table)) {
    risk_table <- suppressMessages(edav_io("read", file_loc = get_constant("CTRY_RISK_CAT")))
  }

  cli::cli_process_start("Adding country risk categories")
  risk_table <- risk_table |>
    mutate(Country = if_else(.data$Country == "TÜRKIYE", "TURKEY", .data$Country))

  df <- df |>
    dplyr::left_join(risk_table, by = setNames("Country", ctry_col))
  cli::cli_process_done()
  return(df)
}

#' Title
#'
#' @param df `tibble` Dataset with at least a country column to join to.
#' @param lab_locs `tibble` Lab testing information table. Defaults to `NULL`. If
#' `NULL`, attempts to download the table from EDAV.
#'
#' @returns `tibble` A dataset with country lab information attached.
#' @keywords internal
add_seq_capacity <- function(df, ctry_col = "ctry", lab_locs = NULL) {
  if (is.null(lab_locs)) {
    lab_locs <- suppressMessages(get_lab_locs())
  }

  cli::cli_process_start("Adding country lab information")
  df <- df |>
    dplyr::left_join(
      lab_locs |>
        dplyr::filter(!is.na(.data$country)) |>
        dplyr::select("country":"num.ship.seq.samples"),
      by = setNames("country", ctry_col)
    )

  df <- df |>
    dplyr::mutate(seq.capacity = dplyr::if_else(!!dplyr::sym(ctry_col) == "NEPAL", "no",
                                                seq.capacity))

  cli::cli_process_done()
  return(df)
}

#' Generate timeliness columns
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The function generates timeliness columns in the positives dataset.
#' @details
#' This function is used in [generate_c1_table()] and [generate_timely_det_violin()].
#'
#' @inheritParams generate_wild_vdpv_summary
#'
#' @returns `tibble` Columns added
#' @keywords internal
generate_pos_timeliness <- function(raw_data, start_date, end_date,
                                        risk_table = NULL, lab_locs = NULL) {

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  # Filtering
  afp_data <- raw_data$afp |>
    dplyr::filter(
      dplyr::between(dateonset, start_date, end_date),
      cdc.classification.all2 != "NOT-AFP"
    )
  es_data <- raw_data$es |>
    dplyr::filter(
      dplyr::between(collect.date, start_date, end_date),
      ev.detect == 1
    )

  # Subsetting
  afp_data <- afp_data |>
    dplyr::select(dplyr::any_of(c(
      "epid", "place.admin.0", "whoregion",
      "cdc.classification.all2", "dateonset",
      "datenotificationtohq"
    ))) |>
    dplyr::mutate(source = "AFP")

  es_data <- es_data |>
    dplyr::select(dplyr::any_of(c(
      "env.sample.id", "ADM0_NAME", "who.region",
      "virus.type",
      "collect.date", "date.notification.to.hq"
    ))) |>
    dplyr::mutate(source = "ENV")

  pos <- dplyr::full_join(afp_data, es_data,
                          by = c(
                            "epid" = "env.sample.id",
                            "place.admin.0" = "ADM0_NAME",
                            "whoregion" = "who.region",
                            "cdc.classification.all2" = "virus.type",
                            "dateonset" = "collect.date",
                            "datenotificationtohq" = "date.notification.to.hq",
                            "source"
                          )
  )

  # Adding required columns
  pos <- add_risk_category(pos, risk_table, ctry_col = "place.admin.0")
  pos <- add_rolling_years(pos, start_date, end_date, "dateonset")
  pos <- add_seq_capacity(pos, ctry_col = "place.admin.0", lab_locs)

  pos_summary <- pos |>
    dplyr::mutate(
      ontonothq = as.numeric(lubridate::as_date(.data$datenotificationtohq) -
                               .data$dateonset),
      timely_cat =
        case_when(stringr::str_detect(.data$seq.capacity, "[Yy]es") & ontonothq <= 35 ~ "<=35 days from onset",
                  stringr::str_detect(.data$seq.capacity, "[Yy]es") & ontonothq > 35 ~ ">35 days from onset",
                  seq.capacity == "no" & ontonothq <= 46 ~ "<=46 days from onset",
                  seq.capacity == "no" & ontonothq > 46 ~ ">46 days from onset",
                  is.na(ontonothq) | ontonothq < 0 ~ "Missing or bad data",
                  .default = NA
        ),
      is_timely = dplyr::if_else(.data$timely_cat %in%
                                   c(
                                     "<=35 days from onset",
                                     "<=46 days from onset"
                                   ), TRUE, FALSE),
      is_target = dplyr::if_else(
        stringr::str_detect(.data$cdc.classification.all2, "WILD|VDPV"),
        TRUE, FALSE
      )
    )

  return(pos_summary)

}

#' Generates a summary table regarding wild and VDPV cases
#'
#' @inheritParams generate_c1_table
#' @param .group_by How to group the results by.
#'
#' @returns `tibble` Summary of wild and VDPV cases
#' @keywords internal
generate_wild_vdpv_summary <- function(raw_data, start_date, end_date,
                                       risk_table = NULL, lab_locs = NULL,
                                       .group_by = c(
                                         "whoregion",
                                         "SG Priority Level",
                                         "place.admin.0",
                                         "rolling_period",
                                         "year_label",
                                         "analysis_year_start",
                                         "analysis_year_end"
                                       )) {
  pos <- generate_pos_timeliness(raw_data, start_date, end_date,
                                             risk_table, lab_locs)
  pos_summary <- pos |>
    dplyr::mutate(
      whoregion = get_region(place.admin.0),
      ontonothq = as.numeric(lubridate::as_date(.data$datenotificationtohq) -
        .data$dateonset),
      timely_cat =
        case_when(stringr::str_detect(.data$seq.capacity, "[Yy]es") & ontonothq <= 35 ~ "<=35 days from onset",
                  stringr::str_detect(.data$seq.capacity, "[Yy]es") & ontonothq > 35 ~ ">35 days from onset",
          seq.capacity == "no" & ontonothq <= 46 ~ "<=46 days from onset",
          seq.capacity == "no" & ontonothq > 46 ~ ">46 days from onset",
          is.na(ontonothq) | ontonothq < 0 ~ "Missing or bad data",
          .default = NA
        ),
      is_timely = dplyr::if_else(.data$timely_cat %in%
        c(
          "<=35 days from onset",
          "<=46 days from onset"
        ), TRUE, FALSE),
      is_target = dplyr::if_else(
        stringr::str_detect(.data$cdc.classification.all2, "WILD|VDPV"),
        TRUE, FALSE
      )
    ) |>
    dplyr::filter(!is.na(rolling_period)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.group_by))) |>
    dplyr::summarise(
      afp_cases = sum(.data$source == "AFP", na.rm = TRUE),
      env_detect = sum(.data$source == "ENV", na.rm = TRUE),
      wild_vdpv_cases = sum(.data$source == "AFP" & .data$is_target, na.rm = TRUE),
      wild_vdpv_env = sum(.data$source == "ENV" & .data$is_target, na.rm = TRUE),
      wild_vdpv_samples = sum(.data$is_target, na.rm = TRUE),
      timely_samples = sum(.data$is_timely & timely_cat != "Missing or bad data", na.rm = TRUE),
      timely_cases = sum(.data$source == "AFP" & .data$is_timely & timely_cat != "Missing or bad data"),
      timely_env = sum(.data$source == "ENV" & .data$is_timely & timely_cat != "Missing or bad data"),
      timely_wild_vdpv_samples = sum(.data$is_target & .data$is_timely & timely_cat != "Missing or bad data"),
      prop_timely_samples = .data$timely_samples / dplyr::n() * 100,
      prop_timely_samples_label = paste0(.data$timely_samples, "/", dplyr::n()),
      prop_timely_cases = .data$timely_cases / .data$wild_vdpv_cases * 100,
      prop_timely_cases_label = paste0(.data$timely_cases, "/", .data$wild_vdpv_cases),
      prop_timely_env = .data$timely_env / .data$wild_vdpv_env * 100,
      prop_timely_env_label = paste0(.data$timely_env, "/", .data$wild_vdpv_env),
      prop_timely_wild_vdpv = .data$timely_wild_vdpv_samples / .data$wild_vdpv_samples * 100,
      prop_timely_wild_vdpv_label = paste0(.data$timely_wild_vdpv_samples, "/", .data$wild_vdpv_samples)
    )

  pos_summary <- pos_summary |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c(
      "prop_timely_samples",
      "prop_timely_cases",
      "prop_timely_env",
      "prop_timely_wild_vdpv"
    )), \(x) if_else(x %in% c(Inf), 0, x))) |>
    dplyr::ungroup()

  return(pos_summary)
}

#' Generate KPI lab intervals
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Generates lab intervals related to the KPI code.
#' @details
#' This function is used in both [generate_c4_table] and in the lab timeliness
#' KPI violin plots.
#'
#' @inheritParams generate_c4_table
#'
#' @returns `tibble` lab data with timeliness columns.
#'
#' @keywords internal
generate_kpi_lab_timeliness <- function(lab_data, start_date, end_date, afp_data) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  lab_data <- clean_lab_data(lab_data, start_date, end_date, afp_data)

  lab_data <- lab_data |>
    dplyr::mutate(
      # Timeliness of virus isolation results
      # Start date: receipt at WHO-accredited lab, end date: culture results
      # Target: ≤14 days
      days.lab.culture = .data$DateFinalCellCultureResult - .data$DateStoolReceivedinLab,
      t1 = dplyr::if_else(!is.na(.data$days.lab.culture) &
                            (.data$days.lab.culture >= 0 & .data$days.lab.culture <= 365),
                          TRUE, FALSE),
      # Timeliness of ITD results (Amanda added this)
      # Start date: culture results, end date: ITD results
      # Target: ≤7 days
      days.culture.itd = .data$DateFinalrRTPCRResults - .data$DateFinalCellCultureResult,
      t2 = dplyr::if_else(!is.na(.data$days.culture.itd) &
                            (.data$days.culture.itd >= 0 & .data$days.culture.itd <= 365),
                          TRUE, FALSE),
      # Timeliness of shipment for sequencing
      # Start date: ITD result, end date: arrival at sequencing lab (
      # (Amanda updated start date here to be consistent with GPSAP 2025-26 indicator)
      # Target: ≤7 days
      days.seq.ship = .data$DateIsolateRcvdForSeq - .data$DateFinalrRTPCRResults,
      t3 = dplyr::if_else(!is.na(.data$days.seq.ship) &
                            (.data$days.seq.ship >= 0 & .data$days.seq.ship <= 365),
                          TRUE, FALSE),
      # Timeliness of sequencing results
      # Start date: arrival at sequencing lab, end.date: sequencing results
      # Target: ≤7 days
      days.seq.rec.res = .data$DateofSequencing - .data$DateIsolateRcvdForSeq,
      t4 = dplyr::if_else(!is.na(.data$days.seq.rec.res) &
                            (.data$days.seq.rec.res >= 0 & .data$days.seq.rec.res <= 365),
                          TRUE, FALSE),

      # ALTERNATIVE for Timeliness of sequencing results
      # Start date: ITD results, end date: sequencing results
      days.itd.res.seq.res = .data$DateofSequencing - .data$DateFinalrRTPCRResults,
      t5 =  dplyr::if_else(!is.na(.data$days.itd.res.seq.res) &
                             (.data$days.itd.res.seq.res >= 0 & .data$days.itd.res.seq.res <= 365),
                           TRUE, FALSE)
    )
}

#' Adjusts the end final year of the rolling period
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' In some instances, the final 12-month rolling period year goes beyond the
#' stated end date of the analysis. The function adjusts the final year rolling
#' period so that it is only up to the end date.
#'
#' @details
#' As a consequence of the adjustment, the function will also ensure that the
#' final data output also only contains records that are only up to the end date.
#'
#' @param data `tibble` Data with rolling period columns.
#' @param end_date `str` The specified end date.
#' @param date_col `str` Column used when filtering by the end date.
#'
#' @returns `tibble` Tibble with adjusted rolling period for the final year.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data(attach.spatial.data = F)
#' afp_data <- raw_data$afp
#' afp_data <- add_rolling_years(afp_data, start_date = "2022-01-01")
#' afp_data <- adjust_rolling_years(afp_data, "2025-02-25")
#' }
adjust_rolling_years <- function(data, end_date, date_col) {
  if (!"rolling_period" %in% names(data)) {
    cli::cli_abort("Please pass data with rolling period columns added.")
  }

  end_date <- lubridate::as_date(end_date)

  latest_period <- data |>
    dplyr::select(
      year_label, rolling_period,
      analysis_year_start, analysis_year_end
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(year_number = as.integer(stringr::str_extract_all(
      year_label,
      "\\d+"
    ))) |>
    dplyr::filter(year_number == max(year_number, na.rm =  TRUE))

  if (max(data[[date_col]], na.rm = TRUE) < end_date) {
    cli::cli_alert_info(paste0("Note: the dataset only contains data up to ",
                               max(data[[date_col]]), ". However, the specified",
                                   " end date is ", end_date,
                               ". Use caution when interpreting results."))

    return(data)
  } else if (is.na(max(data[[date_col]], na.rm = TRUE))) {
    cli::cli_alert(paste0("'", date_col, "'", " is an empty vector."))
    return(data)
  } else {
    # Adjustment made to the latest year in instances of when there are data greater
    # than the end date
    data_adj <- data |>
      dplyr::mutate(year_number = as.integer(stringr::str_extract_all(
        year_label,
        "[-+]?\\d+"
      ))) |>
      dplyr::filter(!!rlang::sym(date_col) <= end_date,
                    year_number > 0) |>
      dplyr::mutate(
        analysis_year_end = dplyr::if_else(year_label == latest_period$year_label,
                                           end_date, analysis_year_end
        ),
        rolling_period = dplyr::if_else(year_label == latest_period$year_label,
                                        paste0(
                                          lubridate::month(.data$analysis_year_start, label = TRUE, abbr = TRUE),
                                          " ", lubridate::year(.data$analysis_year_start),
                                          " - ",
                                          lubridate::month(.data$analysis_year_end, label = TRUE, abbr = TRUE),
                                          " ", lubridate::year(.data$analysis_year_end)
                                        ),
                                        rolling_period
        )
      )

    return(data_adj)
  }

}

# Public functions ----

#' Label rolling year periods
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The function labels and categorizes dates based on the rolling period specified.
#' The start year will always be Year 1 and the rolling period is defined by
#' the start date and the number of periods to account for in a given rolling year.
#' For example, if the start date is defined as Jan 1, 2021 and we would like to
#' calculate a 12-month rolling period, the end date would be Dec 31, 2021.
#'
#' @details
#' The function will filter data using the column specified by `date_col` up to
#' the end date specified.
#'
#' @param df `tibble` A dataset containing at least one date column.
#' @param start_date `str` Start date of Year 1. All years are classified in
#' reference to this date.
#' @param end_date `str` End date to filter to.
#' @param date_col `str` The name of the date column.
#' @param period `period` A [lubridate::period()] object. Defaults to
#' `months(12, FALSE)`.
#'
#' @returns `tibble` A tibble with rolling year information.
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' afp_data <- add_rolling_years(raw_data$afp, "2021-01-01", "2024-05-02", "dateonset")
#' }
#' @export
add_rolling_years <- function(df, start_date, end_date, date_col, period = months(12, FALSE)) {
  start_date <- lubridate::as_date(start_date)

  n_rows <- nrow(df)
  n_row_col <- sum(is.na(df[[date_col]]))

  if (n_rows == n_row_col) {
    cli::cli_abort("The date_col selected is an NA vector. Please check your data.")
  }

  df <- df |>
    dplyr::mutate(
      date_interval = lubridate::interval(start_date, !!rlang::sym(date_col)),
      year_num = floor(date_interval / period),
      year_label = paste0("Year ", year_num + 1),
      analysis_year_start = start_date %m+% lubridate::years(year_num),
      analysis_year_start = dplyr::if_else(lubridate::leap_year(analysis_year_start) &
                                             lubridate::month(analysis_year_start) == 2 &
                                             lubridate::day(analysis_year_start) == 28 &
                                             year_num != 0,
                                           start_date %m+% lubridate::years(year_num) %m+% lubridate::days(1),
                                           analysis_year_start),
      analysis_year_end = analysis_year_start %m+% period %m-% days(1),
      analysis_year_end = dplyr::if_else(lubridate::leap_year(analysis_year_end) &
                                           lubridate::month(analysis_year_end) == 2 &
                                           lubridate::day(analysis_year_end) == 27,
                                         analysis_year_end %m+% days(1),
                                         analysis_year_end),
      rolling_period = paste0(lubridate::month(analysis_year_start, label = TRUE, abbr = TRUE),
                              " ", lubridate::year(analysis_year_start),
                              " - ",
                              lubridate::month(analysis_year_end, label = TRUE, abbr = TRUE),
                              " ", lubridate::year(analysis_year_end))
    ) |>
    dplyr::select(-"year_num")

  df <- adjust_rolling_years(df, end_date, date_col)

  return(df)
}

#' GPEI Strategy surveillance KPIs
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Monitoring surveillance KPIs for Certification of Poliomyelitis Eradication at country
#' regional and global levels.
#'
#' @param raw_data `list` Global polio surveillance dataset.
#' Output of [get_all_polio_data()].
#' @param start_date `str` Start date of the analysis in YYYY-MM-DD format.
#' @param end_date `str` End date of the analysis in YYYY-MM-DD format.
#' @param risk_category `str` Risk category or a list of categories.
#' Defaults to `NULL`. Valid values are: `"LOW, LOW (WATCHLIST), MEDIUM, HIGH`.
#' @param risk_table `tibble` Priority level of each country. Defaults to `NULL`,
#' which will download the information directly from EDAV.
#' @param lab_locs `tibble` Summary of the sequencing capacities of labs.
#' Output of [get_lab_locs()]. Defaults to `NULL`, which will download the information
#' directly from EDAV.
#' .
#' @returns `tibble` Summary table of GPSAP KPIs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data(attach.spatial.data = FALSE)
#' c1 <- generate_c1_table(raw_data, "2021-01-01", "2023-12-31")
#' }
generate_c1_table <- function(raw_data, start_date, end_date,
                              risk_category = NULL,
                              risk_table = NULL,
                              lab_locs = NULL) {
  cli::cli_progress_bar("Creating C1 table", total = 8)

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  # Filtering
  afp_data <- raw_data$afp |>
    dplyr::filter(
      dplyr::between(dateonset, start_date, end_date),
      cdc.classification.all2 != "NOT-AFP"
    ) |>
    dplyr::select(-dplyr::starts_with("pons")) |>
    dplyr::mutate(whoregion = get_region(place.admin.0))

  es_data <- raw_data$es |>
    dplyr::filter(collect.date <= end_date) |>
    dplyr::mutate(who.region = get_region(ADM0_NAME))
  cli::cli_progress_update()

  # Ensure that if using raw_data, required renamed columns are present. Borrowed from
  # extract.country.data()
  afp_data <- dplyr::rename_with(afp_data, recode,
    place.admin.0 = "ctry",
    place.admin.1 = "prov",
    place.admin.2 = "dist",
    person.sex = "sex",
    dateonset = "date",
    yronset = "year",
    datenotify = "date.notify",
    dateinvest = "date.invest",
    cdc.classification.all = "cdc.class"
  )

  if (!is.null(risk_category)) {
    risk_category <- stringr::str_trim(stringr::str_to_upper(risk_category))
    afp_data <- suppressMessages(add_risk_category(afp_data, risk_table)) |>
      dplyr::filter(.data$`SG Priority Level` %in% risk_category)
    es_data <- suppressMessages(add_risk_category(es_data, risk_table,
      ctry_col = "ADM0_NAME"
    )) |>
      dplyr::filter(.data$`SG Priority Level` %in% risk_category)
  }
  cli::cli_progress_update()
  # Include required columns
  afp_data <- suppressMessages(col_to_datecol(afp_data))
  afp_data <- add_rolling_years(afp_data, start_date, end_date, "date")
  es_data <- add_rolling_years(es_data, start_date, end_date, "collect.date")

  cli::cli_progress_update()

  # Calculate country indicators
  afp_indicators <- afp_data |>
    dplyr::group_by(
      year_label, rolling_period,
      analysis_year_start, analysis_year_end
    ) |>
    dplyr::summarise(
      npafp_dist = list(f.npafp.rate.01(dplyr::pick(dplyr::everything()),
        raw_data$dist.pop,
        min(.data$analysis_year_start),
        max(.data$analysis_year_end),
        "dist",
        rolling = TRUE,
        sp_continuity_validation = FALSE
      )),
      stoolad_dist = list(f.stool.ad.01(dplyr::pick(dplyr::everything()),
        raw_data$dist.pop,
        min(.data$analysis_year_start),
        max(.data$analysis_year_end),
        "dist",
        rolling = TRUE,
        sp_continuity_validation = FALSE
      ))
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(
      -dplyr::any_of(c(
        "year_label", "rolling_period",
        "analysis_year_start", "analysis_year_end"
      )),
      \(x) list(dplyr::tibble(x) |>
        dplyr::mutate(
          year_label = year_label,
          rolling_period = rolling_period,
          analysis_year_start = analysis_year_start,
          analysis_year_end = analysis_year_end
        ))
    )) |>
    dplyr::ungroup()
  cli::cli_progress_update()

  # Get established ES sites first and then filter to appropriate start and end dates
  # Have to do this outside of es_indicators because grouping by year also
  # effectively filter data belonging to that year label
  es_end_dates <- es_data |>
    dplyr::select(
      year_label, rolling_period,
      analysis_year_start, analysis_year_end
    ) |>
    dplyr::distinct() |>
    dplyr::filter(dplyr::between(analysis_year_end, start_date, max(analysis_year_end)))
  # this does NOT filter to established sites, it will list the number of samples
  # in the past 12 months since the end date and the age of the site from
  # the very first collection in the ES dataset to the latest collection
  # available that may or may not be the end date but not go beyond that
  es_site_age <- es_end_dates |>
    dplyr::group_by(year_label, rolling_period) |>
    dplyr::mutate(es_sites_w_age = list(get_es_site_age(raw_data$es, max(analysis_year_end)))) |>
    dplyr::ungroup() |>
    # not necessary anymore
    dplyr::select(-analysis_year_start, -analysis_year_end)

  es_indicators <- es_data |>
    dplyr::filter(dplyr::between(analysis_year_end, start_date, end_date)) |>
    dplyr::group_by(
      .data$year_label, .data$rolling_period,
      analysis_year_start, analysis_year_end
    ) |>
    dplyr::summarize(ev_rate = list(f.ev.rate.01(
      dplyr::pick(dplyr::everything()),
      min(.data$analysis_year_start),
      max(.data$analysis_year_end)
    ))) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(
      -dplyr::any_of(c(
        "year_label", "rolling_period",
        "analysis_year_start", "analysis_year_end"
      )),
      \(x) list(dplyr::tibble(x) |>
        dplyr::mutate(
          year_label = year_label,
          rolling_period = rolling_period,
          analysis_year_start = analysis_year_start,
          analysis_year_end = analysis_year_end
        ))
    )) |>
    dplyr::ungroup()

  # Combine both, then able to do an inner join for the final dataset
  # This makes sure for each rolling year, that only the sites selected
  # are in the final ES dataset
  es_indicators <- dplyr::left_join(es_indicators, es_site_age) |>
    dplyr::mutate(final_es_dataset = purrr::map2(
      ev_rate, es_sites_w_age,
      \(x, y) dplyr::left_join(x, y)
    ))

  cli::cli_progress_update()

  # This is a country level calculation by default so no need to remove
  # inconsistent adm2guids
  timely_det_indicator <- generate_wild_vdpv_summary(raw_data,
    start_date, end_date,
    risk_table = risk_table,
    lab_locs = lab_locs
  )
  cli::cli_progress_update()

  # Calculate meeting indicators
  dist_lookup_table <- raw_data$dist.pop |>
    dplyr::select(ctry = ADM0_NAME, prov = ADM1_NAME, dist = ADM2_NAME, adm2guid) |>
    dplyr::distinct()

  # Flag any inconsistent GUIDs to say any calculations are invalid
  # get_incomplete_adm() is a borrowed function from f.stool.ad.01()
  inconsistent_guids <- es_end_dates |>
    dplyr::group_by(
      year_label, rolling_period,
      analysis_year_start, analysis_year_end
    ) |>
    # Filter only GUIDs relevant for the start date
    dplyr::mutate(adm2guid = list(get_incomplete_adm(
      raw_data$dist.pop |>
        dplyr::filter(dplyr::between(
          year,
          lubridate::year(analysis_year_start),
          lubridate::year(analysis_year_end)
        )),
      "dist",
      min(.data$analysis_year_start),
      max(.data$analysis_year_end)
    ))) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(
      -dplyr::any_of(c(
        "year_label", "rolling_period",
        "analysis_year_start", "analysis_year_end"
      )),
      \(x) list(dplyr::tibble(x) |>
        dplyr::mutate(
          year_label = year_label,
          rolling_period = rolling_period,
          analysis_year_start = analysis_year_start,
          analysis_year_end = analysis_year_end,
          consistent_guid = FALSE
        ))
    )) |>
    dplyr::ungroup()

  # This creates a long version of GUIDs with invalid rates due to them not
  # existing for the entirety of their rolling period. We combine this to
  # met_npafp, met_stool, etc... and then perform a filter so they are excluded
  # in summary calculations
  inconsistent_guids <- dplyr::bind_rows(inconsistent_guids$adm2guid)

  if (nrow(inconsistent_guids) != 0) {
    cli::cli_alert_info(paste0(
      cli::col_blue(
        "There were ", nrow(inconsistent_guids),
        " adm2guids not present for the entirety of their 1 year rolling periods.",
        " This typically occurs when a GUID expires between rolling periods that encompasses multiple years.",
        " These adm2guids are removed from the country summaries."
      )
    ))
  }

  # Summarise
  met_npafp <- dplyr::bind_rows(afp_indicators$npafp_dist) |>
    dplyr::mutate(whoregion = get_region(ctry)) |>
    # invalid GUIDS will have consistnet_guid = FALSE while
    # valid ones will be NA
    dplyr::left_join(inconsistent_guids) |>
    dplyr::filter(is.na(consistent_guid)) |>
    dplyr::filter(!is.na(ctry)) |>
    dplyr::group_by(
      year_label, rolling_period,
      analysis_year_start, analysis_year_end,
      whoregion, ctry
    ) |>
    dplyr::summarise(
      dist_w_100k = sum(par >= 1e5, na.rm = TRUE),
      dist_npafp = sum(par != 0, na.rm = TRUE), # remove districts without populations
      met_npafp = sum(
        (par >= 1e5 & npafp_rate >= 3 &
          ctry %in% c("AFGHANISTAN", "PAKISTAN")),
        (par >= 1e5 & npafp_rate >= 2 &
          whoregion %in% c("AFRO", "EMRO", "SEARO") &
          !ctry %in% c("AFGHANISTAN", "PAKISTAN")),
        (par >= 1e5 & npafp_rate >= 1 &
          whoregion %in% c("AMRO", "EURO", "WPRO")),
        na.rm = T
      ),
      prop_met_npafp = met_npafp / dist_w_100k * 100,
      npafp_label = paste0(met_npafp, "/", dist_w_100k)
    ) |>
    ungroup()

  met_stool <- dplyr::bind_rows(afp_indicators$stoolad_dist) |>
    dplyr::left_join(dist_lookup_table) |>
    dplyr::mutate(whoregion = get_region(ctry)) |>
    # invalid GUIDS will have consistnet_guid = FALSE while
    # valid ones will be NA
    dplyr::left_join(inconsistent_guids) |>
    dplyr::filter(is.na(consistent_guid)) |>
    dplyr::filter(!is.na(ctry)) |>
    dplyr::group_by(
      year_label, rolling_period,
      analysis_year_start, analysis_year_end,
      whoregion, ctry
    ) |>
    dplyr::summarise(
      dist_stool = sum(adequacy.denominator >= 5, na.rm = TRUE),
      met_stool = sum(per.stool.ad >= 80 & adequacy.denominator >= 5, na.rm = TRUE),
      prop_met_stool = met_stool / dist_stool * 100,
      stool_label = paste0(met_stool, "/", dist_stool)
    )

  met_ev <- dplyr::bind_rows(es_indicators$final_es_dataset) |>
    dplyr::rename(
      "ctry" = ADM0_NAME,
      "prov" = ADM1_NAME,
      "dist" = ADM2_NAME
    ) |>
    dplyr::left_join(dist_lookup_table) |>
    dplyr::mutate(whoregion = get_region(ctry)) |>
    # invalid GUIDS will have consistnet_guid = FALSE while
    # valid ones will be NA
    dplyr::left_join(inconsistent_guids) |>
    dplyr::filter(is.na(consistent_guid)) |>
    dplyr::group_by(
      year_label, rolling_period,
      analysis_year_start, analysis_year_end,
      whoregion, ctry
    ) |>
    dplyr::summarise(
      es_sites = sum(n_samples_12_mo >= 10 & site_age >= 12, na.rm = T),
      met_ev = sum(num.samples >= 10 & site_age >= 12 & ev.rate >= 0.5, na.rm = T),
      prop_met_ev = met_ev / es_sites * 100,
      ev_label = paste0(met_ev, "/", es_sites)
    )
  cli::cli_progress_update()

  combine <- dplyr::full_join(met_npafp, met_stool) |>
    dplyr::full_join(met_ev) |>
    dplyr::full_join(timely_det_indicator |>
      rename("ctry" = "place.admin.0")) |>
    dplyr::select(dplyr::any_of(c(
      "year_label", "rolling_period",
      "analysis_year_start", "analysis_year_end",
      "whoregion", "SG Priority Level", "ctry",
      "prop_met_npafp", "prop_met_stool", "prop_met_ev",
      # "prop_timely_samples",
      "prop_timely_wild_vdpv",
      "npafp_label", "stool_label", "ev_label",
      "prop_timely_wild_vdpv_label",
      # "prop_timely_samples_label",
      "prop_timely_cases_label",
      "prop_timely_env_label"
    )))

  # Clean up
  combine <- combine |>
    dplyr::filter(!is.na(ctry)) |>
    # If no priority level, default to low
    dplyr::mutate(`SG Priority Level` = dplyr::if_else(is.na(`SG Priority Level`),
      "LOW", `SG Priority Level`
    )) |>
    # NAs should be replace with NaNs and not be empty
    dplyr::mutate(dplyr::across(dplyr::starts_with("prop") & !dplyr::ends_with("label"), \(x) round(tidyr::replace_na(x, NaN), 2))) |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("label"), \(x) tidyr::replace_na(x, "0/0"))) |>
    dplyr::rename(Region = whoregion)

  cli::cli_progress_update()
  cli::cli_progress_done()

  return(combine)
}

#' Generate C1 rollup for high-priority countries
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Generates a summary of how many of the high priority countries have met their
#' AFP and ES indicators.
#'
#' @param c1 `tibble` The output of [generate_c1_table()].
#' @param priority_level `str or list` Priority level. Defaults to `"HIGH"`. Valid
#' values are `"LOW", "LOW (WATCHLIST)", "MEDIUM", "HIGH"`
#' @param who_region `str` WHO region to summarize the data to.
#' @param .group_by `str` How the rollup should be grouped. Defaults to the column `"rolling_period"`.
#' @param npafp_target `num` Target used when calculating the proportion of districts in a country
#' that meets NPAFP rate.
#' @param stool_target `num` Target used when calculating the proportion of districts in a country
#' that meets stool adequacy rate.
#' @param ev_target `num` Target used when calculating the proportion of ES sites in a country
#' that meets EV detection rate.
#' @param timely_wpv_vdpv_target `num` Target used when calculating the proportion of ES sites in a country
#' that meets timeliness of detection of WPV and VDPV cases.
#'
#' @returns `tibble` A summary rollup
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' c1 <- generate_c1_table(raw_data, "2022-01-01", "2024-12-31")
#' c1_rollup <- generate_c1_rollup(c1)
#' }
generate_c1_rollup <- function(c1,
                               priority_level = "HIGH",
                               who_region = NULL,
                               .group_by = "rolling_period",
                               npafp_target = 80,
                               stool_target = 80,
                               ev_target = 80,
                               timely_wpv_vdpv_target = 80) {


  if (!is.null(priority_level)) {
    c1 <- c1 |>
      dplyr::filter(`SG Priority Level` %in% priority_level)
  }

  if (!is.null(who_region)) {
    c1 <- c1 |>
      dplyr::filter(Region %in% who_region)
  }

  c1_rollup <- c1 |>
    dplyr::filter(`SG Priority Level` %in% priority_level) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.group_by))) |>
    dplyr::summarize(
      met_npafp = sum(prop_met_npafp >= npafp_target, na.rm = TRUE),
      npafp_denom = sum(!is.na(prop_met_npafp)),
      stool_denom = sum(!is.na(prop_met_stool)),
      ev_denom = sum(!is.na(prop_met_ev)),
      det_denom = sum(!is.na(prop_timely_wild_vdpv)),
      met_stool = sum(prop_met_stool >= stool_target, na.rm = TRUE),
      met_ev = sum(prop_met_ev >= ev_target, na.rm = TRUE),
      met_timely_wild_vdpv = sum(prop_timely_wild_vdpv >= timely_wpv_vdpv_target, na.rm = TRUE),
      prop_met_npafp = met_npafp / npafp_denom * 100,
      prop_met_stool = met_stool / stool_denom * 100,
      prop_met_ev = met_ev / ev_denom * 100,
      prop_met_timely_wild_vdpv = met_timely_wild_vdpv / det_denom * 100,
      met_npafp_label = paste0(met_npafp, "/",npafp_denom),
      met_stool_label = paste0(met_stool, "/", stool_denom),
      met_ev_label = paste0(met_ev, "/", ev_denom),
      met_timely_wild_vdpv_label = paste0(met_timely_wild_vdpv, "/", det_denom)
    ) |>
    dplyr::ungroup()

  return(c1_rollup)
}

#' AFP surveillance KPI summary
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function creates a summary table of AFP surveillance KPIs.
#'
#' @param afp_data `tibble` AFP linelist data.
#' @param pop_data `tibble` Population data.
#' @param spatial_scale `str` Either `"ctry", "prov", "dist"`.
#' @inheritParams generate_c1_table
#'
#' @returns `tibble` Summary table containing AFP KPIs.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data(attach.spatial.data = FALSE)
#' c2 <- generate_c2_table(raw_data$afp, raw_data$ctry.pop, "2021-01-01", "2023-12-31", "ctry")
#' }
generate_c2_table <- function(afp_data, pop_data, start_date, end_date,
                              spatial_scale,
                              risk_category = NULL,
                              lab_locs = NULL,
                              risk_table = NULL) {
  check_spatial_scale(pop_data, spatial_scale)

  cli::cli_progress_bar("Creating C2 table", total = 5)

  # Standardize data
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  afp_data <- dplyr::rename_with(afp_data, recode,
    place.admin.0 = "ctry",
    place.admin.1 = "prov",
    place.admin.2 = "dist",
    person.sex = "sex",
    dateonset = "date",
    yronset = "year",
    datenotify = "date.notify",
    dateinvest = "date.invest",
    cdc.classification.all = "cdc.class"
  )
  pop_data <- dplyr::rename_with(pop_data, recode,
    ADM0_NAME = "ctry",
    ADM1_NAME = "prov",
    ADM2_NAME = "dist",
    ADM0_GUID = "adm0guid",
    u15pop.prov = "u15pop",
    WHO_REGION = "who_region"
  )
  cli::cli_progress_update()

  # Add required columns
  # NOTE: NAs are removed from rolling calculations as they can't be tagged
  afp_data <- afp_data |>
    add_seq_capacity(lab_locs = lab_locs) |>
    col_to_datecol() |>
    add_rolling_years(start_date, end_date, "date") |>
    # Removes years earlier than the start date
    dplyr::filter(dplyr::between(
      analysis_year_end, start_date,
      max(analysis_year_end, na.rm = TRUE)
    ))

  # Defaults to missing = good, bad.data = inadequate
  # Start and end dates only filter afp_data and isn't used in calculations
  # inside the function
  afp_data <- generate_stool_data(afp_data, start_date, end_date)

  cli::cli_progress_update()

  # NPAFP and Stool Adequacy
  afp_indicators <- afp_data |>
    dplyr::group_by(
      year_label, rolling_period,
      analysis_year_start, analysis_year_end
    ) |>
    dplyr::summarize(
      npafp = list(f.npafp.rate.01(dplyr::pick(dplyr::everything()),
        pop_data,
        min(.data$analysis_year_start),
        max(.data$analysis_year_end),
        spatial_scale,
        rolling = TRUE,
        sp_continuity_validation = FALSE
      )),
      stoolad = list(f.stool.ad.01(dplyr::pick(dplyr::everything()),
        pop_data,
        min(.data$analysis_year_start),
        max(.data$analysis_year_end),
        spatial_scale,
        rolling = TRUE,
        sp_continuity_validation = FALSE
      ))
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(
      -dplyr::any_of(c(
        "year_label", "rolling_period",
        "analysis_year_start",
        "analysis_year_end"
      )),
      \(x) list(dplyr::tibble(x) |>
        dplyr::mutate(
          year_label = year_label,
          rolling_period = rolling_period,
          analysis_year_start = analysis_year_start,
          analysis_year_end = analysis_year_end
        ))
    )) |>
    dplyr::ungroup()
  # Stool Condition
  group_stool_cond <- switch(spatial_scale,
    "ctry" = c(
      "year_label", "analysis_year_start",
      "analysis_year_end", "rolling_period",
      "ctry", "adm0guid"
    ),
    "prov" = c(
      "year_label", "analysis_year_start",
      "analysis_year_end", "rolling_period",
      "ctry", "adm0guid",
      "prov", "adm1guid"
    ),
    "dist" = c(
      "year_label", "analysis_year_start",
      "analysis_year_end", "rolling_period",
      "ctry", "adm0guid",
      "prov", "adm1guid", "dist", "adm2guid"
    )
  )
  stool_condition <- afp_data |>
    dplyr::group_by(dplyr::across(dplyr::any_of(group_stool_cond))) |>
    dplyr::summarize(
      afp_cases = sum(.data$cdc.classification.all2 != "NOT-AFP"),
      good_samples = sum(.data$cdc.classification.all2 != "NOT-AFP" &
        .data$adequacy.final2 == "Adequate"),
      prop_good_condition = good_samples / afp_cases * 100
    )
  cli::cli_progress_update()

  # Completeness of Contact Sampling
  # Calculated in future versions
  group_60_day <- switch(spatial_scale,
    "ctry" = c(
      "year_label",
      "analysis_year_start",
      "analysis_year_end",
      "rolling_period",
      "epid", "adm0guid", "ctry"
    ),
    "prov" = c(
      "year_label",
      "analysis_year_start",
      "analysis_year_end",
      "rolling_period",
      "epid", "adm0guid", "adm1guid", "ctry",
      "prov"
    ),
    "dist" = c(
      "year_label",
      "analysis_year_start",
      "analysis_year_end",
      "rolling_period",
      "epid", "adm0guid", "adm1guid", "adm2guid",
      "ctry", "prov", "dist"
    )
  )

  # Completeness of 60-day follow-ups
  complete_60_day <- afp_data |>
    # Note that this also filters out "NOT-AFP" cases
    # Start and end dates also just filters dates and not used in calculations
    generate_60_day_table_data(start_date, end_date) |>
    dplyr::left_join(afp_data |>
      dplyr::select(dplyr::all_of(group_60_day))) |>
    dplyr::select(-"epid") |>
    dplyr::group_by(dplyr::across(dplyr::any_of(group_60_day))) |>
    dplyr::summarize(prop_complete_60_day = sum(.data$ontime.60day == 1, na.rm = TRUE) /
      sum(.data$ontime.60day != 99, na.rm = TRUE) * 100) |>
    ungroup()
  cli::cli_progress_update()

  # Timeliness indicators
  timeliness_summary <- afp_data |>
    dplyr::filter(
      .data$cdc.classification.all2 != "NOT-AFP",
      dplyr::between(.data$date, start_date, end_date)
    ) |>
    dplyr::mutate(
      ontonothq = as.numeric(lubridate::as_date(.data$datenotificationtohq) -
        .data$date),
      # prefer stool 2 unless missing then use stool 1
      ontostool_final = dplyr::if_else(!is.na(ontostool2), ontostool2, ontostool1),
      ontolab = as.numeric(difftime(
        lubridate::as_date(.data$stooltolabdate),
        .data$date
      ), units = "days"),
      timely_cat =
        dplyr::case_when(seq.capacity == "yes" & ontonothq <= 35 ~ "<=35 days from onset",
          seq.capacity == "yes" & ontonothq > 35 ~ ">35 days from onset",
          seq.capacity == "no" & ontonothq <= 46 ~ "<=46 days from onset",
          seq.capacity == "no" & ontonothq > 46 ~ ">46 days from onset",
          is.na(ontonothq) | ontonothq < 0 ~ "Missing or bad data",
          .default = "Missing or bad data"
        ),
      is_timely = dplyr::if_else(.data$timely_cat %in%
        c(
          "<=35 days from onset",
          "<=46 days from onset"
        ), TRUE, FALSE),
      is_target = dplyr::if_else(
        stringr::str_detect(.data$cdc.classification.all2, "WILD|VDPV"),
        TRUE, FALSE
      ),
      is_opt_timely = dplyr::case_when(
        (.data$culture.itd.cat == "In-country culture/ITD" & .data$ontolab <= 14) ~ "yes",
        (.data$culture.itd.cat == "In-country culture/ITD" & .data$ontolab > 14) ~ "no",
        (.data$culture.itd.cat == "International culture/ITD" & .data$ontolab <= 18) ~ "yes",
        (.data$culture.itd.cat == "International culture/ITD" & .data$ontolab > 18) ~ "no",
        .default = "unable to assess"
      )
    ) |>
    dplyr::group_by(dplyr::across(dplyr::any_of(group_stool_cond))) |>
    dplyr::summarize(
      timely_not = sum(.data$ontonot <= 7, na.rm = TRUE) / sum(!is.na(.data$ontonot)) * 100,
      timely_inv = sum(.data$ontoinvest <= 2, na.rm = TRUE) / sum(!is.na(.data$ontoinvest)) * 100,
      timely_field = sum(.data$ontostool_final <= 11 &
        .data$stool1tostool2 >= 1, na.rm = TRUE) / sum(!is.na(.data$ontostool_final) & !is.na(.data$stool1tostool2)) * 100,
      timely_stool_shipment = sum(
        (.data$culture.itd.cat == "In-country culture/ITD" & .data$daysstooltolab <= 3) |
          (.data$culture.itd.cat == "International culture/ITD" & .data$daysstooltolab <= 7),
        na.rm = TRUE
      ) / sum(!is.na(.data$culture.itd.cat) & !is.na(.data$daysstooltolab)) * 100,

      median_timely_not = median(ontonot, na.rm = TRUE),
      median_timely_inv = median(ontoinvest, na.rm = TRUE),
      median_timely_field = median(ontostool_final, na.rm = TRUE),
      median_stool_shipment = median(daysstooltolab, na.rm = TRUE),
      median_onto_lab = median(ontolab, na.rm = TRUE),

      timely_opt_field_shipment = sum(.data$is_opt_timely == "yes") / sum(.data$is_opt_timely != "unable to assess") * 100,
      timely_wpv_vdpv = sum(.data$is_timely & .data$is_target & .data$timely_cat != "Missing or bad data", na.rm = TRUE) /
        sum(.data$is_timely & .data$timely_cat != "Missing or bad data", na.rm = TRUE) * 100
    )

  # Need to calculate outside because calculation should only be done for VDPV/WPV samples
  median_wpv_vdpv <- afp_data |>
    dplyr::filter(
      .data$cdc.classification.all2 != "NOT-AFP",
      dplyr::between(.data$date, start_date, end_date)
    ) |>
    dplyr::mutate(
      ontonothq = as.numeric(lubridate::as_date(.data$datenotificationtohq) -
                               .data$date),
      is_target = dplyr::if_else(
        stringr::str_detect(.data$cdc.classification.all2, "WILD|VDPV"),
        TRUE, FALSE
      )) |>
    dplyr::filter(is_target == TRUE) |>
    dplyr::group_by(dplyr::across(dplyr::any_of(group_stool_cond))) |>
    dplyr::summarize(
      median_ontonothq = median(ontonothq)
    )

  timeliness_summary <- timeliness_summary |> dplyr::left_join(median_wpv_vdpv)

  cli::cli_progress_update()

  # Completeness of weekly zero reporting
  # Timeliness of WZR
  # Adequacy of active surveillance sites
  # AFP case encounters

  # Mark inconsistent GUIDs
  afp_end_dates <- afp_data |>
    dplyr::select(
      year_label, rolling_period,
      analysis_year_start, analysis_year_end
    ) |>
    dplyr::distinct() |>
    dplyr::filter(dplyr::between(analysis_year_end, start_date, max(analysis_year_end)))

  # Flag any inconsistent GUIDs to say any calculations are invalid
  # get_incomplete_adm() is a borrowed function from f.stool.ad.01()
  inconsistent_guids <- afp_end_dates |>
    dplyr::group_by(
      year_label, rolling_period,
      analysis_year_start, analysis_year_end
    ) |>
    # Filter only GUIDs relevant for the start date
    dplyr::mutate(guid = list(get_incomplete_adm(
      pop_data |>
        dplyr::filter(dplyr::between(
          year,
          lubridate::year(analysis_year_start),
          lubridate::year(analysis_year_end)
        )),
      spatial_scale,
      min(.data$analysis_year_start),
      max(.data$analysis_year_end)
    ))) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(
      -dplyr::any_of(c(
        "year_label", "rolling_period",
        "analysis_year_start", "analysis_year_end"
      )),
      \(x) list(dplyr::tibble(x) |>
        dplyr::mutate(
          year_label = year_label,
          rolling_period = rolling_period,
          analysis_year_start = analysis_year_start,
          analysis_year_end = analysis_year_end,
          consistent_guid = FALSE
        ))
    )) |>
    dplyr::ungroup()

  inconsistent_guids <- dplyr::bind_rows(inconsistent_guids$guid)

  inconsistent_guids <- switch(spatial_scale,
    "ctry" = {
      inconsistent_guids |>
        dplyr::rename(adm0guid = "guid")
    },
    "prov" = {
      inconsistent_guids |>
        dplyr::rename(adm1guid = "guid")
    },
    "dist" = {
      inconsistent_guids |>
        dplyr::rename(adm2guid = "guid")
    }
  )

  if (nrow(inconsistent_guids) != 0) {
    cli::cli_alert_info(paste0(
      cli::col_blue(
        "There were ", nrow(inconsistent_guids),
        " GUIDs not present for the entirety of their 1 year rolling periods.",
        " This typically occurs when a GUID expires between rolling periods that encompasses multiple years.",
        " You can find these GUIDs by filtering the column `consistent_guid == FALSE`."
      )
    ))
  }

  results <- dplyr::full_join(
    dplyr::bind_rows(afp_indicators$npafp),
    dplyr::bind_rows(afp_indicators$stoolad)
  ) |>
    dplyr::full_join(stool_condition) |>
    dplyr::full_join(complete_60_day) |>
    dplyr::full_join(timeliness_summary) |>
    dplyr::left_join(inconsistent_guids)

  # Select only required columns
  results <- results |>
    dplyr::mutate(
      npafp_cat = dplyr::case_when(
        .data$consistent_guid == FALSE ~ "Area did not exist during the entire rolling period",
        (.data$n_npafp == 0) & .data$par >= 100000 ~ "Silent (u15pop >= 100K)",
        (.data$n_npafp == 0) & (.data$par > 0 & .data$par < 100000) ~ "No cases (u15pop < 100K)",
        (.data$par == 0 | is.na(.data$par)) ~ "Missing Pop",
        # Calculate regardless of population size
        .data$npafp_rate < 1 ~ "< 1",
        (.data$npafp_rate >= 1 & .data$npafp_rate < 2) ~ ">= 1 & <2",
        (.data$npafp_rate >= 2 & .data$npafp_rate < 3) ~ ">= 2 & <3",
        .data$npafp_rate >= 3 ~ ">=3",
        .default = "Unable to Assess"
      ),
      stool_cat = dplyr::case_when(
        .data$consistent_guid == FALSE ~ "Area did not exist during the entire rolling period",
        .data$afp.cases == 0 | is.na(.data$afp.cases) ~ "Zero AFP cases",
        .data$afp.cases != 0 & .data$per.stool.ad < 50 ~ "<50%",
        .data$afp.cases != 0 & (.data$per.stool.ad >= 50 & .data$per.stool.ad <= 79) ~ "50%-79%",
        afp.cases != 0 & per.stool.ad > 79 ~ "80%+",
        .default = "Unable to Assess"
      )
    ) |>
    dplyr::select(
      dplyr::any_of(group_stool_cond),
      "npafp_cat", "stool_cat",
      "npafp_rate", "per.stool.ad",
      "prop_good_condition":"timely_wpv_vdpv",
      "consistent_guid"
    )

  # Formatting
  results <- results |>
    dplyr::mutate(dplyr::across("npafp_rate":"timely_wpv_vdpv", \(x) tidyr::replace_na(x, NaN))) |>
    # NPAFP is only NA if it's missing population
    dplyr::mutate(npafp_rate = dplyr::if_else(npafp_cat != "Missing Pop" & is.nan(npafp_rate), 0, npafp_rate))

  # Add region and risk levels
  region_lookup_table <- pop_data |>
    dplyr::select(dplyr::any_of(c("ctry", "prov", "dist", "who_region"))) |>
    dplyr::distinct()

  results <- add_risk_category(results, risk_table) |>
    dplyr::left_join(region_lookup_table) |>
    dplyr::select(-Region) |>
    dplyr::rename(Region = who_region) |>
    tidyr::replace_na(list(`SG Priority Level` = "LOW"))

  cli::cli_progress_done()

  return(results)
}

#' Environmental surveillance KPIs by site
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Environmental surveillance KPIs summarized by site.
#'
#'
#' @param es_data `tibble` Environmental surveillance data.
#' @inheritParams generate_c1_table
#'
#' @returns `tibble` A summary table of environmental surveillance KPIs.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data(attach.spatial.data = FALSE)
#' c3 <- generate_c3_table(raw_data$es, "2021-01-01", "2023-12-31")
#' }
generate_c3_table <- function(es_data, start_date, end_date,
                              risk_category = NULL,
                              lab_locs = NULL,
                              risk_table = NULL) {

  es_original <- es_data
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  es_data <- es_data |>
    add_seq_capacity("ADM0_NAME", lab_locs) |>
    add_rolling_years(start_date, end_date, "collect.date")

  # Get established ES sites first and then filter to appropriate start and end dates
  # Have to do this outside of es_indicators because grouping by year also
  # effectively filter data belonging to that year label
  es_end_dates <- es_data |>
    dplyr::select(year_label, rolling_period,
                  analysis_year_start, analysis_year_end) |>
    dplyr::distinct() |>
    dplyr::filter(dplyr::between(analysis_year_end, start_date, max(analysis_year_end)))
  # this does NOT filter to established sites, it will list the number of samples
  # in the past 12 months since the end date and the age of the site from
  # the very first collection in the ES dataset to the latest collection
  # available that may or may not be the end date but not go beyond that
  es_site_age <- es_end_dates |>
    dplyr::group_by(year_label, rolling_period) |>
    dplyr::mutate(es_sites_w_age = list(get_es_site_age(es_original, max(analysis_year_end)))) |>
    dplyr::ungroup() |>
    # not necessary anymore
    dplyr::select(-analysis_year_start, -analysis_year_end) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(-dplyr::any_of(c("year_label", "rolling_period")),
                                \(x) list(dplyr::tibble(x) |>
                                            dplyr::mutate(year_label = year_label,
                                                          rolling_period = rolling_period
                                            ))))
  # Ages based on rolling years
  es_site_age_long <- dplyr::bind_rows(es_site_age$es_sites_w_age)

  # Add additional timeliness columns
  es_data <- es_data |>
    dplyr::mutate(coltolab = difftime(.data$date.received.in.lab,
                                      .data$collect.date, units = "day"),
                  coltoresults = difftime(.data$date.notification.to.hq,
                                          .data$date.received.in.lab, units = "days"),
                  timely_ship = dplyr::case_when(
                    (is.na(.data$culture.itd.cat) | is.na(.data$coltolab) |
                       .data$coltolab < 0 | .data$coltolab > 365) ~ "unable to assess",
                    (.data$culture.itd.cat == "In-country culture/ITD" & .data$coltolab <= 3) ~ "yes",
                    (.data$culture.itd.cat == "In-country culture/ITD" & .data$coltolab > 3) ~ "no",
                    (.data$culture.itd.cat == "International culture/ITD" & .data$coltolab <= 7) ~ "yes",
                    (.data$culture.itd.cat == "International culture/ITD" & .data$coltolab > 7) ~ "no",
                    .default = NA),

                  timely_det = dplyr::case_when(
                    (is.na(.data$seq.capacity) | is.na(.data$coltoresults) |
                       .data$coltoresults < 0 | .data$coltoresults > 365) ~ "unable to assess",
                    (stringr::str_detect(.data$seq.capacity, "[Yy]es") & .data$coltoresults <= 35) ~ "yes",
                    (stringr::str_detect(.data$seq.capacity, "[Yy]es") & .data$coltoresults > 35) ~ "no",
                    (.data$seq.capacity == "no" & .data$coltoresults <= 46) ~ "yes",
                    (.data$seq.capacity == "no" & .data$coltoresults > 46) ~ "no",
                    .default = NA),

                  is_target = dplyr::if_else(.data$wpv == 1 | .data$vdpv == 1, TRUE, FALSE)
                  )

  es_summary <- es_data |>
    dplyr::filter(dplyr::between(analysis_year_end, start_date, max(analysis_year_end))) |>
    dplyr::group_by(year_label, rolling_period,
                    analysis_year_start, analysis_year_end,
                    ADM0_NAME, site.name, lat, lng) |>
    dplyr::summarize(
      es_samples = sum(!is.na(.data$ev.detect), na.rm = TRUE),
      ev_rate = sum(.data$ev.detect == 1, na.rm = TRUE) / es_samples * 100,
      prop_good_es = sum(.data$sample.condition == "Good", na.rm = TRUE) / sum(!is.na(.data$sample.condition)) * 100,
      prop_timely_ship = sum(timely_ship == "yes", na.rm = TRUE) /
        sum(timely_ship != "unable to assess" | !is.na(timely_ship), na.rm = TRUE) * 100,
      wpv_vdpv_detections = sum(is_target == TRUE & (timely_det != "unable to assess" |
                                                       !is.na(timely_det)), na.rm = TRUE),
      timely_wpv_vdpv_detections = sum(is_target == TRUE &
                                         timely_det == "yes", na.rm = TRUE),
      prop_timely_det_wpv_vdpv = timely_wpv_vdpv_detections /
        wpv_vdpv_detections * 100
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(ev_det_cat = dplyr::case_when(
      .data$es_samples < 5 ~ "<5 samples collected",
      .data$ev_rate < 50 & .data$es_samples >= 5 ~ "<50%",
      .data$ev_rate >= 50 & .data$ev_rate < 80 & es_samples >= 5 ~ "50% to <80%",
      .data$ev_rate >= 80 & .data$es_samples >= 5 ~ "80-100%",
      .default = NA
    )) |>
    dplyr::mutate(ev_det_cat = factor(.data$ev_det_cat,
                                      levels = c("<5 samples collected", "<50%",
                                                 "50% to <80%", "80-100%")))

  es_summary <- add_risk_category(es_summary, risk_table, ctry_col = "ADM0_NAME") |>
    dplyr::mutate(`SG Priority Level` = dplyr::if_else(is.na(`SG Priority Level`),
                                                       "LOW", `SG Priority Level`),
                  Region = get_region(ADM0_NAME))

  es_summary <- dplyr::left_join(es_summary, es_site_age_long) |>
    dplyr::filter(analysis_year_end <= end_date)

  return(es_summary)

}

#' Create a country level rollup of the C3 label
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Create a country level summary of ES site performance with respect to
#' meeting established targets for EV detection rates, good samples. Note, this
#' country roll up will only consider sites that have at least 10 collections
#' and open for at least 12 months, consistent with guidelines in the 2025-2026
#' GPSAP indicators.
#'
#'
#' @param c3 `tibble` Output of [generate_c3_table()].
#' @param include_labels `logical` Include columns for the labels? Default TRUE.
#' @param min_sample `num` Only consider sites with at least this number
#' of ES samples. Default is `10`.
#' @param timely_spv_vdpv_target Target used when determining whether a country
#' meets EV detection target.
#'
#' @returns `tibble` A summary of the c3 table at the country level
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' c3 <- generate_c3_table(raw_data$es, "2021-01-01", "2023-12-31")
#' c3_rollup <- generate_c3_rollup(c3)
#' }
generate_c3_rollup <- function(c3, include_labels = TRUE, min_sample = 10,
                               timely_wpv_vdpv_target = 80) {

  if (!"site.name" %in% names(c3)) {
    cli::cli_abort("Please summarize c3 at the site level and try again.")
  }

  c3_rollup <- c3 |>
    dplyr::rename(ctry = "ADM0_NAME",
                  sg_priority_level = "SG Priority Level",
                  region = "Region") |>
    dplyr::group_by(year_label, rolling_period,
                    analysis_year_start, analysis_year_end,
                    ctry, region, sg_priority_level) |>
    dplyr::summarize(
      met_ev = sum(ev_rate >= 50 & n_samples_12_mo >= min_sample & site_age >= 12, na.rm = TRUE),
      met_ev_5_samples = sum(ev_rate >= 50 & n_samples_12_mo >= 5, na.rm = TRUE),
      met_good_samples = sum(prop_good_es >= 80, na.rm = TRUE),
      met_timely_wpv_vdpv_det = sum(prop_timely_det_wpv_vdpv >= timely_wpv_vdpv_target, na.rm = TRUE),
      median_timely_shipment_per_site = median(prop_timely_ship, na.rm = TRUE),
      es_sites = sum(n_samples_12_mo >= 1, na.rm = TRUE),
      active_sites = sum(n_samples_12_mo >= min_sample & site_age >= 12, na.rm = TRUE),
      prop_met_ev = met_ev / active_sites * 100,
      prop_met_ev_5_samples = met_ev_5_samples / es_sites * 100,
      prop_met_good_samples = met_good_samples / es_sites * 100,
      prop_met_timely_wpv_vdpv_det = met_timely_wpv_vdpv_det / es_sites * 100,
      prop_met_ev_label = paste0(met_ev, "/", active_sites),
      prop_met_ev_5_samples_label = paste0(met_ev_5_samples, "/", es_sites),
      prop_met_good_samples_label = paste0(met_good_samples, "/", es_sites),
      prop_met_timely_wpv_vdpv_det_label = paste0(met_timely_wpv_vdpv_det, "/", es_sites)
    ) |>
    dplyr::select(-dplyr::starts_with("met_"), "es_sites") |>
    dplyr::ungroup()

  return(c3_rollup)

}

#' Laboratory surveillance KPIs
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Summarizes the timeliness of samples as it arrives in the lab and to sequencing
#' results. Samples may come from both AFP and ES samples.
#'
#' @param lab_data `tibble` Lab data containing information of ES or AFP samples.
#' @param afp_data `tibble` AFP surveillance data.
#' @param start_date `str` Start date of the analysis in YYYY-MM-DD format.
#' @param end_date `str` End date of the analysis in YYYY-MM-DD format.
#'
#' @returns `list` A summary of timeliness KPIs for lab data.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data(attach.spatial.data = FALSE)
#' lab_data <- readr::read_csv("C:/Users/ABC1/Desktop/lab_data.csv")
#' c4 <- generate_c4_table(lab_data, raw_data$afp, "2021-01-01", "2024-12-31")
#' }
generate_c4_table <- function(lab_data, afp_data, start_date, end_date) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  lab_data <- generate_kpi_lab_timeliness(lab_data, start_date, end_date, afp_data)

  lab_data <- lab_data |>
    dplyr::filter(dplyr::between(CaseDate, start_date, end_date))
  lab_data <- add_rolling_years(lab_data, start_date, end_date, "CaseDate")

  lab_data <- lab_data |> dplyr::filter(!is.na(seq.lab), !is.na(culture.itd.lab),
                                        seq.lab != "-")

  itd_lab_summary <- lab_data |>
    dplyr::group_by(year_label, rolling_period,
                    analysis_year_start, analysis_year_end, culture.itd.lab) |>
    dplyr::summarize(
      # t1, t2, t3, filters for valid samples only for each timeliness interval
      # Timeliness of virus isolation results
      timely_isolation = sum(.data$days.lab.culture <= 14 & t1 == TRUE, na.rm = TRUE),
      t1 = sum(t1 == TRUE),
      prop_timely_isolation = timely_isolation / t1 * 100,

      # Timeliness of ITD results
      timely_itd = sum(.data$days.culture.itd <= 7 & t2 == TRUE, na.rm = TRUE),
      # only include those with positive culture results referred to an ITD lab
      t2 = sum(t2 == TRUE, na.rm = TRUE),
      prop_timely_itd = timely_itd / t2 * 100,

      # Timeliness of shipment for sequencing
      timely_seqship = sum(.data$days.seq.ship <= 7 & t3 == TRUE, na.rm = TRUE),
      t3 = sum(t3 == TRUE, na.rm = TRUE),
      prop_timely_seqship = timely_seqship / t3 * 100,

      # Labels
      prop_t1_label = paste0(timely_isolation, "/", t1),
      prop_t2_label = paste0(timely_itd, "/", t2),
      prop_t3_label = paste0(timely_seqship, "/", t3)
    ) |>
    dplyr::ungroup()

  seq_lab_summary <- lab_data |>
    dplyr::group_by(year_label, rolling_period,
                    analysis_year_start, analysis_year_end, seq.lab) |>
    dplyr::summarize(
      # t4 filters for valid samples only for each timeliness interval
      #Timeliness of sequencing results
      # We only have AFP lab
      timely_seqres = sum(.data$days.seq.rec.res <= 7 & t4 == TRUE, na.rm = TRUE),
      t4 = sum(t4 == TRUE, na.rm = TRUE),
      prop_timely_seqres = timely_seqres / t4 * 100,

      # Labels
      prop_t4_label = paste0(timely_seqres, "/", t4),

      #t5
      #Timeliness of sequencing results using ITD result as start date
      timely_itdres_seqres = sum((
        (.data$seq.cat == "Not shipped for sequencing" & .data$days.itd.res.seq.res <= 7) |
          (.data$seq.cat == "Shipped for sequencing" & .data$days.itd.res.seq.res <= 14)) &
          t5 == TRUE,
        na.rm = TRUE),
      t5 = sum(t5 == TRUE, na.rm = TRUE),
      prop_timely_itdres_seqres = timely_itdres_seqres / t5 * 100,

      # Labels
      prop_t5_label = paste0(timely_itdres_seqres, "/", t5),
    ) |>
    dplyr::ungroup()

  return(list(itd_lab_summary = itd_lab_summary,
              seq_lab_summary = seq_lab_summary))

  }

#' Export KPI tables
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Performs formatting and export of the C1-C4 KPI tables.
#'
#' @param c1 `tibble` Output of [generate_c1_table()]. Defaults to `NULL`.
#' @param c2 `tibble` Output of [generate_c2_table()]. Defaults to `NULL`.
#' @param c3 `tibble` Output of [generate_c3_table()]. Defaults to `NULL`.
#' @param c4 `tibble` Output of [generate_c4_table()]. Defaults to `NULL`.
#' @param output_path `str` Path to output the table to. Defaults to the path
#' initiated after running [init_kpi()].
#' @param drop_label_cols `logical` Keep or discard label columns. Defaults to
#' `TRUE`.
#' @param sc_targets `logical` Whether to use SC targets when exporting the table. Defaults to FALSE.
#' @param pos_data `tibble` Positives dataset.
#' @param risk_table `tibble` The risk table. Required if using sc_targets and outside of CDC.
#'
#' @returns None.
#' @export
#'
#' @examples
#' \dontrun{
#' init_kpi()
#' c1 <- generate_c1_table(raw_data, "2021-01-01", "2023-12-31")
#' c2 <- generate_c2_table(raw_data$afp, raw_data$ctry.pop, "2021-01-01", "2023-12-31", "ctry")
#' c3 <- generate_c3_table(raw_data$es, "2021-01-01", "2023-12-31")
#' c4 <- generate_c4_table(lab_data, raw_data$afp, "2021-01-01", "2024-12-31")
#' export_kpi_table(c1, c2, c3, c4)
#' }
export_kpi_table <- function(c1 = NULL, c2 = NULL, c3 = NULL, c4 = NULL,
                             output_path = Sys.getenv("KPI_TABLES"),
                             drop_label_cols = FALSE,
                             sc_targets = FALSE, pos_data = NULL,
                             risk_table = NULL) {

  if (sc_targets) {
    if (is.null(pos_data)) {
      cli::cli_abort("Please pass the positives dataset (i.e., raw_data$pos) to the pos_data param.")
    } else {
      pos_data <- add_risk_category(pos_data, risk_table, "place.admin.0")
      high_risk <- pos_data |>
        dplyr::filter(`SG Priority Level` == "HIGH") |>
        dplyr::pull(place.admin.0)
      ctry_pos_wpv_vdpv <- pos_data |>
        mutate(is_target = dplyr::if_else(
          stringr::str_detect(measurement, "WILD|VDPV"),
          TRUE, FALSE)) |>
        filter(source %in% c("AFP", "ENV"),
               is_target == TRUE,
               yronset >= 2022,
               `SG Priority Level` != "LOW") |>
        pull(place.admin.0) |>
        unique()
      combined_ctry <- c(high_risk, ctry_pos_wpv_vdpv) |> unique()
    }
  }

  format_table <- function(x) {

    tryCatch(
      {x <- x |>
        dplyr::rename_with(\(x) stringr::str_to_lower(x)) |>
        dplyr::rename_with(\(x) stringr::str_replace_all(x, "[\\. ]", "_")) |>
        dplyr::mutate(dplyr::across(
          (dplyr::starts_with("prop") |
             dplyr::starts_with("timely") | dplyr::starts_with("median")) &
            -dplyr::ends_with("label"),
          \(x) round(x, 0)
        ))
      return(x)
      },
      error = function(e) { return(NULL) }
    )
  }

  drop_labels <- function(x) {
    tryCatch(
      {
        x <- x |>
          dplyr::select(-dplyr::ends_with("label"),
                        -dplyr::ends_with("cat"))
        return(x)
      },
      error = function(e) { return(NULL) }
    )
  }

  if (!is.null(c1)) {
    if (sc_targets) {
      c1 <- c1 |>
        dplyr::filter(ctry %in% combined_ctry)
      c1_rollup <- generate_c1_rollup(c1,
                                      priority_level = c("LOW (WATCHLIST)", "MEDIUM", "HIGH"),
                                      npafp_target = 90,
                                      stool_target = 90,
                                      ev_target = 90,
                                      timely_wpv_vdpv_target = 90)
    } else {
      c1_rollup <- generate_c1_rollup(c1)
    }
  } else {
    c1_rollup <- NULL
  }

  if (!is.null(c2)) {
    if (sc_targets) {
      c2 <- c2 |> dplyr::filter(ctry %in% combined_ctry)
    }
  }

  if (!is.null(c3)) {
    if (sc_targets) {
      c3 <- c3 |> dplyr::filter(ADM0_NAME %in% combined_ctry)
      c3 <- generate_c3_rollup(c3, timely_wpv_vdpv_target = 90)
    } else {
      c3 <- generate_c3_rollup(c3)
    }
  } else {
    c3 <- NULL
  }

  if (!is.null(c4)) {
    c4_itd <- c4$itd_lab_summary
    c4_seq <- c4$seq_lab_summary
  } else {
    c4_itd <- NULL
    c4_seq <- NULL
  }

  export_list <- list(`c1 - kpi` = c1, `c1 - high risk summary` = c1_rollup,
                      `c2 -  national AFP indicators` = c2,
                      `c3 - es indicators by site` = c3,
                      `c4 - culture itd lab summary` = c4_itd,
                      `c4 - seq lab summary` = c4_seq)
  export_list <- export_list |>
    purrr::keep(\(x) !is.null(x)) |>
    purrr::map(format_table)

  # c1 formatting
  if (!is.null(c1)) {

    export_list$`c1 - kpi` <- export_list$`c1 - kpi` |>
      dplyr::select(dplyr::any_of(c(
        "sg_priority_level", "region", "ctry",
        "rolling_period",
        "prop_met_npafp", "prop_met_stool", "prop_met_ev",
        "prop_timely_wild_vdpv"
      )), dplyr::ends_with("label"), dplyr::ends_with("cat")) %>%
      {
        if (!drop_label_cols) {
          dplyr::mutate(., prop_met_npafp = paste0(prop_met_npafp, " (", npafp_label, ")"),
                        prop_met_stool = paste0(prop_met_stool, " (", stool_label, ")"),
                        prop_met_ev = paste0(prop_met_ev, " (", ev_label, ")"),
                        prop_timely_wild_vdpv = paste0(prop_timely_wild_vdpv, " (",
                                                       prop_timely_wild_vdpv_label, ")"))
        } else {
          .
        }
      } |>
      dplyr::arrange(sg_priority_level, region, ctry, dplyr::desc(rolling_period)) |>
      dplyr::rename_with(recode,
                       rolling_period = "Rolling 12 Months",
                       region = "WHO Region",
                       sg_priority_level = "GPSAP Risk Category",
                       ctry = "Country",
                       prop_met_npafp = "Non-polio AFP rate – subnational, %",
                       prop_met_stool = "Stool adequacy – subnational, %",
                       prop_met_ev = "ES EV detection rate – national, %",
                       prop_timely_wild_vdpv = "Timeliness of detection for WPV/VDPV, %"
                      )
  }

  # c1 high risk formatting
  if (!is.null(c1_rollup)) {
    export_list$`c1 - high risk summary` <- export_list$`c1 - high risk summary` %>%
      {
        if (!drop_label_cols) {
          dplyr::mutate(.,
                        prop_met_npafp = paste0(prop_met_npafp, " (", met_npafp_label, ")"),
                        prop_met_stool = paste0(prop_met_stool, " (", met_stool_label, ")"),
                        prop_met_ev = paste0(prop_met_ev, " (", met_ev_label, ")"),
                        prop_met_timely_wild_vdpv = paste0(prop_met_timely_wild_vdpv, " (",
                                                           met_timely_wild_vdpv_label, ")")
                        )
        } else {
          .
        }
      } |>
      dplyr::arrange(dplyr::desc(rolling_period)) |>
      dplyr::rename_with(recode,
                         rolling_period = "Rolling 12 Months",
                         prop_met_npafp = "Non-polio AFP rate – subnational, %",
                         prop_met_stool = "Stool adequacy – subnational, %",
                         prop_met_ev = "ES EV detection rate – national, %",
                         prop_met_timely_wild_vdpv = "Timeliness of detection for WPV/VDPV, %"
      ) |>
      dplyr::select(-dplyr::starts_with("met"), -dplyr::ends_with("denom"))
  }

  # c2 formatting
  if (!is.null(c2)) {
    export_list$`c2 -  national AFP indicators` <- export_list$`c2 -  national AFP indicators` |>
      dplyr::select(dplyr::any_of(
        c(
          "sg_priority_level", "region",
          "ctry", "prov", "dist", "rolling_period", "npafp_rate", "per_stool_ad",
          "prop_good_condition", "prop_complete_60_day",
          "timely_not", "median_timely_not",
          "timely_inv", "median_timely_inv",
          "timely_field", "median_timely_field",
          "timely_stool_shipment", "median_stool_shipment",
          "timely_opt_field_shipment", "median_onto_lab", "timely_wpv_vdpv",
          "median_ontonothq"
        )
      )) |>
      dplyr::mutate(npafp_rate = round(npafp_rate, 0),
                    per_stool_ad = round(per_stool_ad, 0)) |>
      dplyr::arrange(sg_priority_level, region, ctry, dplyr::desc(rolling_period)) |>
      dplyr::rename_with(recode,
                         rolling_period = "Rolling 12 Months",
                         region = "WHO Region",
                         sg_priority_level = "GPSAP Risk Category",
                         ctry = "Country",
                         prov = "Province",
                         dist = "District",
                         npafp_rate = "Non-polio AFP rate",
                         per_stool_ad = "Stool adequacy, %",
                         prop_good_condition = "Stool condition, %",
                         prop_complete_60_day = "Completeness of 60-day follow-ups, %",
                         timely_not = "Timeliness of notification, %",
                         median_timely_not = "Median timeliness of notification",
                         timely_inv = "Timeliness of investigation, %",
                         median_timely_inv = "Median timeliness of investigation",
                         timely_field = "Timeliness of field activities, %",
                         median_timely_field = "Median timeliness of field activities",
                         timely_stool_shipment = "Timeliness of stool specimen shipment, %",
                         median_stool_shipment = "Median timeliness of stool shipment",
                         timely_opt_field_shipment = "Timeliness of optimized field and shipment, %",
                         median_onto_lab = "Median timeliness of optimized field and shipment",
                         timely_wpv_vdpv = "Timeliness of detection for WPV/VDPV – AFP, %",
                         median_ontonothq = "Median timeliness of detection for WPV/VDPV"
      )
  }

  # c3 formatting
  if (!is.null(c3)) {
    export_list$`c3 - es indicators by site` <- export_list$`c3 - es indicators by site` |>
      dplyr::select(dplyr::any_of(
        c(
          "sg_priority_level", "region",
          "ctry", "prov", "dist", "rolling_period",
          "es_sites", "active_sites", "prop_met_ev",
          "prop_met_ev_5_samples",
          "prop_met_good_samples", "median_timely_shipment_per_site",
          "prop_met_timely_wpv_vdpv_det"
        )
      ), dplyr::ends_with("label"), dplyr::ends_with("cat")) %>%
      {
        if (!drop_label_cols) {
          dplyr::mutate(.,
                        prop_met_ev = paste0(prop_met_ev, " (", prop_met_ev_label, ")"),
                        prop_met_ev_5_samples = paste0(prop_met_ev_5_samples, " (",
                                                       prop_met_ev_5_samples_label, ")"),
                        prop_met_good_samples = paste0(prop_met_good_samples, " (",
                                                       prop_met_good_samples_label, ")"),
                        prop_met_timely_wpv_vdpv_det = paste0(prop_met_timely_wpv_vdpv_det,
                                                              " (", prop_met_timely_wpv_vdpv_det_label,
                                                              ")"))
        } else {
          .
        }
      } |>
      dplyr::arrange(sg_priority_level, region, ctry, dplyr::desc(rolling_period)) |>
      dplyr::rename_with(recode,
                         rolling_period = "Rolling 12 Months",
                         region = "WHO Region",
                         sg_priority_level = "GPSAP Risk Category",
                         ctry = "Country",
                         prov = "Province",
                         dist = "District",
                         es_sites = "ES Sites",
                         active_sites = "Active Sites",
                         prop_met_ev = "ES EV detection rate, % ",
                         prop_met_ev_5_samples = "ES EV detection rate, % (>= 5 samples)",
                         prop_met_good_samples = "Condition of ES sample, %",
                         median_timely_shipment_per_site = "Median Timeliness of ES sample, %",
                         prop_met_timely_wpv_vdpv_det = "Timeliness of detection for WPV/VDPV – ES, %"
      )

  }

  # c4 formatting
  if (!is.null(c4)) {
    export_list$`c4 - culture itd lab summary` <- export_list$`c4 - culture itd lab summary` |>
      dplyr::select(dplyr::any_of(
        c(
          "culture_itd_lab", "rolling_period",
          "prop_timely_isolation",
          "prop_timely_itd",
          "prop_timely_seqship"
        )
      ), dplyr::ends_with("label"), dplyr::ends_with("cat")) %>%
      {
        if (!drop_label_cols) {
          dplyr::mutate(.,
                        prop_timely_isolation = paste0(prop_timely_isolation,
                                                       " (", prop_t1_label, ")"),
                        prop_timely_itd = paste0(prop_timely_itd, " (",
                                                       prop_t2_label, ")"),
                        prop_timely_seqship = paste0(prop_timely_seqship, " (",
                                                       prop_t3_label, ")"))
        } else {
          .
        }
      } |>
      dplyr::arrange(culture_itd_lab, dplyr::desc(rolling_period)) |>
      dplyr::rename_with(recode,
                         rolling_period = "Rolling 12 Months",
                         culture_itd_lab = "Culture ITD Lab",
                         prop_timely_isolation = "Timeliness of virus isolation results",
                         prop_timely_itd = "Timeliness of ITD results",
                         prop_timely_seqship = "Timeliness of shipment for sequencing"
      )

    export_list$`c4 - seq lab summary` <- export_list$`c4 - seq lab summary` |>
      dplyr::select(dplyr::any_of(
        c("seq_lab", "rolling_period","prop_timely_seqres", "prop_timely_itdres_seqres")),
        dplyr::ends_with("label"), dplyr::ends_with("cat")) %>%
      {
        if (!drop_label_cols) {
          dplyr::mutate(.,
                        prop_timely_seqres = paste0(prop_timely_seqres,
                                                       " (", prop_t4_label, ")"),
                        prop_timely_itdres_seqres = paste0(prop_timely_itdres_seqres,
                                                           " (", prop_t5_label, ")"))
        } else {
          .
        }
      } |>
      dplyr::arrange(seq_lab, dplyr::desc(rolling_period)) |>
      dplyr::rename_with(recode,
                         rolling_period = "Rolling 12 Months",
                         seq_lab = "Sequencing Lab",
                         prop_timely_seqres = "Timeliness of sequencing results (using date arrived at sequencing lab as the start date)",
                         prop_timely_itdres_seqres = "Timeliness of sequencing results (using date of rRTPCR Results as the start date)"
      )

  }

  # Final formatting
  export_list <- purrr::map(export_list, drop_labels)

  # indicator guide
  c1_indicators <- c(
    "C1. Non-polio AFP rate – subnational, %",
    "C1. Stool adequacy – subnational, %",
    "C1. ES EV detection rate – national, %",
    "C1. Timeliness of detection for WPV/VDPV, %"
  )

  c1_stool_description <- switch(as.character(sc_targets),
                                 "TRUE" = "Proportion of districts with >=5 AFP cases and stool adequacy >= 90%",
                                 "FALSE" = "Proportion of districts with >=5 AFP cases and stool adequacy >= 80%")

  c1_indicators_explanation <- c(
    "Proportion of districts with >=100K U15 population meeting regional NPAFP rate targets (AFR, EMR, SEAR: >=2, AMR, EUR, WPR: >=1, Endemics: >=3)",
    c1_stool_description,
    "Proportion of active surveillance sites (sites open >=12 months with >=10 samples collected in the last 12 months) meeting EV detection sensitivity target of >=50%",
    "Proportion of WPVs and VDPVs with final lab results within 35 days (full laboratory capacity) or 46 days (without full laboratory capacity) of onset for AFP cases or collection date for ES samples"
  )
  c2_indicators <- c(
    "C2. Non-polio AFP rate",
    "C2. Stool adequacy, %",
    "C2. Stool condition, %",
    "C2. Completeness of 60-day follow-ups, %",
    "C2. Timeliness of notification, %",
    "C2. Median timeliness of notification",
    "C2. Timeliness of investigation, %",
    "C2. Median timeliness of investigation",
    "C2. Timeliness of field activities, %",
    "C2. Median timeliness of field activities",
    "C2. Timeliness of stool specimen shipment, %",
    "C2. Median timeliness of stool shipment",
    "C2. Timeliness of optimized field and shipment, %",
    "C2. Median timeliness of optimized field and shipment",
    "C2. Timeliness of detection for WPV/VDPV – AFP, %",
    "C2: Median timeliness of detection for WPV/VDPV"
  )
  c2_indicators_explanation <- c(
    "NPAFP cases per 100 000 population aged <15 years summarized at the country level",
    "Proportion of AFP cases with 2 stool specimens collected ≥24 hours apart, both within 14 days of paralysis onset, AND received in good condition in a WHO-accredited laboratory summarized at the country level",
    "Proportion of AFP cases with two stool specimens arriving in good condition at a WHO accredited lab",
    "Proportion of inadequate AFP cases with a follow up exam for residual paralysis completed within 60–90 days of paralysis onset",
    "Proportion of AFP cases reported within 7 days of paralysis onset",
    "Median days AFP cases reported since paralysis onset",
    "Proportion of AFP cases investigated within 48 hours of notification",
    "Median days AFP cases investigated after notification",
    "Proportion of AFP cases with field activities completed within 11 days of paralysis onset",
    "Median days of AFP cases with field activities completed after paralysis onset",
    "Proportion of cases with stools that arrive at a WHO-accredited lab within 3 days (domestic shipment) or 7 days (international shipment) of specimen collection",
    "Median days stools arrived at a WHO-accredited lab after specimen collection",
    "Proportion of cases with samples that arrive in the lab within 14 days (domestic shipment) or 18 days (international shipment) of paralysis onset",
    "Median days samples arrive in the lab after paralysis onset",
    "Proportion of AFP cases with WPV/VDPV final lab results within 35 days (full laboratory capacity) or 46 days (without full laboratory capacity) of paralysis onset",
    "Median days from onset to final lab results for WPV/VDPV samples"
  )

  c3_indicators <- c(
      "C3. ES Sites",
      "C3. Active Sites",
      "C3. ES EV detection rate, % ",
      "C3. ES EV detection rate, % (>= 5 samples)",
      "C3. Condition of ES sample, %",
      "C3. Median Timeliness of ES sample, %",
      "C3. Timeliness of detection for WPV/VDPV – ES, %"
    )

  es_cond_description <- switch(as.character(sc_targets),
                                "TRUE" = "Proportion of ES sites with 90% of samples arriving the lab in good condition",
                                "FALSE" = "Proportion of ES sites with 80% of samples arriving the lab in good condition")
  es_vdpv_description <- switch(as.character(sc_targets),
                                "TRUE" = "Proportion of ES sites with >=90% of samples having WPV/VDPV final lab results within 35 (full laboratory capacity) or 46 days (without full laboratory capacity) of collection",
                                "FALSE" = "Proportion of ES sites with >=80% of samples having WPV/VDPV final lab results within 35 (full laboratory capacity) or 46 days (without full laboratory capacity) of collection")

  c3_indicators_explanation <- c(
    "ES Sites with at least one collection in the past twelve months",
    "ES Sites open >=12 months with >=10 samples collected in the last 12 months",
    "Proportion of active ES sites that met an EV detection rate of >=50%",
    "Proportion of ES sites with >=5 samples collected in the last 12 months that met an EV detection rate of >=50%",
    es_cond_description,
    "Median proportion of samples across ES sites that arrive at a WHO-accredited lab within 3 days (domestic shipment) or 7 days (international) of sample collection",
    es_vdpv_description
  )
  c4_indicators <- c(
    "C4. Timeliness of virus isolation results",
    "C4. Timeliness of ITD results",
    "C4. Timeliness of shipment for sequencing",
    "C4. Timeliness of sequencing results",
    "c4. Timeliness of sequencing results (using date of ITD results as start date)"
  )
  c4_indicators_explanation <- c(
    "Proportion of specimens with virus isolation results within 14 days of receipt of AFP specimen or availability of ES concentrate at WHO-accredited lab",
    "Proportion of specimens with ITD results within 7 days of virus isolation results",
    "Proportion of specimens that arrive at the sequencing lab within 7 days of ITD results",
    "Proportion of specimens with sequencing results available within 7 days (AFP) from arrival at the sequencing lab",
    "Proportion of specimens with sequencing results available within 14 days (AFP) from date of ITD results"
  )

  indicator_tibble <- dplyr::tibble(Indicators = NULL, Description = NULL)
  if (!is.null(c1)) {
    indicator_tibble <- dplyr::bind_rows(indicator_tibble,
                                         dplyr::tibble(Indicators = c1_indicators, Description = c1_indicators_explanation))
  }
  if (!is.null(c2)) {
    indicator_tibble <- dplyr::bind_rows(indicator_tibble,
                                         dplyr::tibble(Indicators = c2_indicators, Description = c2_indicators_explanation))
  }
  if (!is.null(c3)) {
    indicator_tibble <- dplyr::bind_rows(indicator_tibble,
                                         dplyr::tibble(Indicators = c3_indicators, Description = c3_indicators_explanation))
  }
  if (!is.null(c4)) {
    indicator_tibble <- dplyr::bind_rows(indicator_tibble,
                                         dplyr::tibble(Indicators = c4_indicators, Description = c4_indicators_explanation))
  }

  export_list$`indicator dictionary` <- indicator_tibble

  export_list <- purrr::keep(export_list, \(x) !is.null(x))

  if (drop_label_cols) {
    file_name <- paste0("kpi_tables_export_dropped_cols_", Sys.Date(), ".xlsx")
  } else {
    file_name <- paste0("kpi_tables_export_", Sys.Date(), ".xlsx")
  }

  openxlsx::write.xlsx(export_list, file.path(output_path, file_name), colWidths = "auto",
                       headerStyle = openxlsx::createStyle(textDecoration = "Bold"))
}

# Helper functions to diagnose datasets ----


#' Get missingness of date variables in the lab dataset
#' @description
#' Obtains the percentage of missingness in date variables within the lab dataset
#'
#' @param lab_data `tibble` Lab data.
#' @param group_by `str` A column or a vector of columns to group results by.
#'
#' @returns `tibble` Summary of missingness of date variables
#' @export
#'
#' @examples
#' \dontrun{
#' get_lab_date_col_missingness(lab_data)
#' }
get_lab_date_col_missingness <- function(lab_data, group_by = NULL) {
  lab_data |>
    mutate(wild_vdpv = if_else(!is.na(WILD1) |  !is.na(VDPV1) |
                                 !is.na(VDPV2) | !is.na(VDPV3), 1, 0)) |>
    filter(wild_vdpv == 1) |>
    tidyr::pivot_longer(cols = contains("Date", ignore.case = FALSE), names_to = "date_column",
                        values_to = "value") |>
    dplyr::group_by(dplyr::across(dplyr::any_of(c(group_by, "date_column")))) |>
    dplyr::summarise(n = n(),
                     n_na = sum(is.na(value)),
                     prop_na = paste0(round(n_na / n * 100, 2), "%")
    )
}
