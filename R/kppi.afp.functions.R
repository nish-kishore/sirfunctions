# Private functions ----
#' Calculates the year a week number falls into.
#'
#' @param wk `numeric` Week number.
#'
#' @returns `str` The year the week falls into.
#'
#' @examples
#' f.year.roll(32)
#' @keywords internal
f.year.roll <- function(wk) {
  x <- NA
  x[wk <= 52.00] <- 1
  x[wk > 52.00 & wk <= 104.15] <- 2
  x[wk > 104.15 & wk <= 156] <- 3
  x[wk > 156 & wk <= 208] <- 4
  x[wk > 208 & wk <= 260] <- 5
  x[wk > 260 & wk <= 312] <- 6
  x[wk > 312 & wk <= 364] <- 7
  x[wk > 364 & wk <= 416] <- 8
  x[wk > 416 & wk <= 468] <- 9
  x[wk > 468 & wk <= 520] <- 10

  x <- factor(x,
    levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    labels = c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5", "Year 6", "Year 7", "Year 8", "Year 9", "Year 10")
  )

  return(x)
}


#' Adds rolling date information
#'
#' @param df `tibble` Dataframe to add rolling date info to.
#' @param onset_col `str` Column used for the onset date. Mainly used to calculate
#' `weeks.from.end`.
#' @param start_date `str` Start date of the analysis.
#' @param end_date `str` End date of the analysis.
#'
#' @returns `tibble` Lab data with rolling date period information added.
#' @keywords internal
#'
add_rolling_date_info <- function(df, start_date, end_date, onset_col = "ParalysisOnsetDate") {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  cli::cli_process_start("Adding rolling period date information")
  end.year <- lubridate::year(end_date)
  st.year <- lubridate::year(start_date)

  month.start <- lubridate::month(start_date, label = TRUE, abbr = TRUE)
  month.end <- lubridate::month(end_date, label = TRUE, abbr = TRUE)
  # prior_period = paste0(month.start, " ", st.year, " - ", month.end, " ", year(start_date + days(364)) )
  # current_period = paste0(month.start, " ", st.year + 1, " - ", month.end, " ", end.year)

  # Calculate prior_period correctly accounting for leap years
  prior_year_end <- start_date + lubridate::years(1) - lubridate::days(1)
  prior_period <- paste0(
    month.start, " ", st.year,
    " - ", month.end,
    " ", lubridate::year(prior_year_end)
  )

  current_period <- paste0(
    month.start,
    " ", st.year + 1,
    " - ", month.end,
    " ", end.year
  )

  df <- df |>
    # renaming culture.itd.lab for Nigeria which has two labs in lab.locs, simply naming Nigeria
    dplyr::mutate(
      weeks.analysis = ((end_date - start_date) / 7), # weeks in the analysis
      weeks.from.end = ((end_date - get(onset_col)) / 7), # weeks from onset to end of analysis
      case.week = (.data$weeks.analysis - .data$weeks.from.end), # estimating week in the analysis for each case
      year.analysis = f.year.roll(.data$case.week), # analysis year
      year.analysis.start = start_date + (365 * (as.integer(.data$year.analysis) - 1)),
      year.analysis.end = .data$year.analysis.start + 364,
      rolling_period =
        case_when(
          year.analysis == "Year 1" ~ prior_period,
          year.analysis == "Year 2" ~ current_period
        )
    )
  cli::cli_process_done()

  return(df)
}

#' Add the risk category based on the country
#'
#' @param df `tibble` Dataframe with at least a column for country
#' @param risk_table  `tibble` Risk category table for each country. Defaults to `NULL`.
#' When set to `NULL`, attempts to download the risk category table from EDAV.
#'
#' @return `tibble` A dataframe with risk category columns added.
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
  cli::cli_process_done()
  return(df)
}

# Public functions ----
generate_e1_table <- function(pos, start_date, end_date) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  pos <- pos |>
    dplyr::filter(dplyr::between(.data$dateonset, start_date, end_date)) |>
    dplyr::select(dplyr::any_of(c(
      "dateonset", "datenotificationtohq",
      "place.admin.0", "whoregion",
      "measurement", "source"
    )))

  pos <- add_rolling_date_info(pos, start_date, end_date, "dateonset")
  pos <- add_risk_category(pos, ctry_col = "place.admin.0")
  pos <- add_seq_capacity(pos, ctry_col = "place.admin.0")
  cli::cli_process_start("Generating E1 summary table")
  pos_summary <- pos |>
    dplyr::mutate(
      ontonothq = as.numeric(lubridate::as_date(.data$datenotificationtohq) -
        .data$dateonset),
      timely_cat =
        case_when(seq.capacity == "yes" & ontonothq <= 35 ~ "<=35 days from onset",
          seq.capacity == "yes" & ontonothq > 35 ~ ">35 days from onset",
          seq.capacity == "no" & ontonothq <= 49 ~ "<=49 days from onset",
          seq.capacity == "no" & ontonothq > 49 ~ ">49 days from onset",
          is.na(ontonothq) | ontonothq < 0 ~ "Missing or bad data",
          .default = NA
        ),
      is_timely = dplyr::if_else(.data$timely_cat %in%
        c(
          "<=35 days from onset",
          "<=49 days from onset"
        ), 1, 0),
      is_virus_interest = dplyr::if_else((stringr::str_detect(.data$measurement, "^WILD") |
        stringr::str_detect(.data$measurement, "^VDPV")
      ), 1, 0)
    ) |>
    dplyr::filter(!is.na(rolling_period)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(
      "whoregion",
      "SG Priority Level",
      "place.admin.0",
      "rolling_period"
    )))) |>
    dplyr::summarise(
      afp_cases = sum(source == "AFP", na.rm = TRUE),
      es_detections = sum(source == "ENV", na.rm = TRUE),
      wild_vdpv_cases = sum(source == "AFP" &
        is_virus_interest == 1),
      wild_vdpv_env = sum(source == "ENV" &
        is_virus_interest == 1),
      timely_samples = sum(is_timely, na.rm = TRUE),
      timely_wild_vdpv_cases = sum(
        source == "AFP" & is_timely == 1 & is_virus_interest == 1
      ),
      timely_wild_vdpv_env = sum(
        source == "AFP" & is_timely == 1 & is_virus_interest == 1
      ),
      prop_timely_wild_vdpv_cases = round(.data$timely_wild_vdpv_cases / .data$wild_vdpv_cases, 2),
      prop_timely_wild_vdpv_env = round(.data$timely_wild_vdpv_env / .data$wild_vdpv_env, 2)
    )

  pos_summary <- pos_summary |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c(
      "prop_timely_wild_vdpv_cases",
      "prop_timely_wild_vdpv_env"
    )), \(x) if_else(x %in% c(Inf, NaN), 0, x))) |>
    dplyr::ungroup()

  cli::cli_process_done()

  return(pos_summary)
}
generate_e2_table <- function() {}
generate_e3_table <- function() {}
generate_e4_table <- function() {}
