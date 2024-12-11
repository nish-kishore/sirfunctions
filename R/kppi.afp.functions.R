#Private functions ----
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
add_rolling_date_info <- function(df, onset_col="ParalysisOnsetDate", start_date, end_date) {

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
  prior_period = paste0(month.start, " ", st.year,
                        " - ", month.end,
                        " ", lubridate::year(prior_year_end))

  current_period = paste0(month.start,
                          " ", st.year + 1,
                          " - ", month.end,
                          " ", end.year)

  df <- df |>
    #renaming culture.itd.lab for Nigeria which has two labs in lab.locs, simply naming Nigeria
    dplyr::mutate(
      weeks.analysis = ((end_date - start_date) / 7), # weeks in the analysis
      weeks.from.end = ((end_date - get(onset_col)) / 7),  # weeks from onset to end of analysis
      case.week = (.data$weeks.analysis - .data$weeks.from.end),  # estimating week in the analysis for each case
      year.analysis = f.year.roll(.data$case.week), # analysis year
      year.analysis.start = start_date + (365 * (as.integer(.data$year.analysis) - 1)),
      year.analysis.end = .data$year.analysis.start + 364,
      rolling_period =
        case_when(year.analysis == 'Year 1' ~ prior_period,
                  year.analysis == 'Year 2' ~ current_period
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
add_risk_category <- function(df, risk_table = NULL) {
  if(is.null(risk_table)) {
    risk_table <- edav_io("read", file_loc = "Data/misc/country_prioritization/SG_country_prioritization_update_21_june_2023.csv")
  }
}

# Public functions ----
generate_e1_table <- function(pos) {

}
generate_e2_table <- function() {}
generate_e3_table <- function() {}
generate_e4_table <- function() {}
