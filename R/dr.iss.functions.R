#' Read ISS/eSURV data
#'
#' The function is written to assist in load the ISS data from a path specified
#' by the user during [init_dr()]. This function is not meant to be exported.
#'
#' @param iss_path `str` Path to the excel or csv file.
#' @param sheet_name `str` Optional name of the ISS data. This is mainly used if
#' the path is to an Excel file and that Excel file has multiple tabs.
#'
#' @returns `tibble` ISS/eSURV data loaded into a tibble.
#' @examples
#' \dontrun{
#' iss_path <- "C:/Users/ABC1/Desktop/iss_data.csv"
#' iss_data <- load_iss_data(iss_path)
#' }
load_iss_data <- function(iss_path, sheet_name = NULL) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop('Package "readxl" must be installed to use this function.',
      .call = FALSE
    )
  }
  if (stringr::str_ends(iss_path, ".csv")) {
    return(readr::read_csv(iss_path))
  } else if (stringr::str_ends(iss_path, ".xlsx")) {
    return(readxl::read_excel(iss_path, sheet = sheet_name))
  } else {
    stop("Not a csv or .xlsx file. Try again.")
  }
}

#' Perform common cleaning tasks for ISS/eSURV data
#'
#' ISS/eSURV data often needs to be cleaned and standardized before analysis. Because
#' these datasets may vary from country to country, reviewing the data first and its columns
#' is the first step. In general, there are eight required columns. These are the parameters
#' with a suffix `_col`. Modify the passed arguments as necessary so the function can
#' successfully run. Priority levels are set to automatically detect high, medium, low, and not a focal site.
#' Ensure that priority level column categories have these specification:
#'  - `High`: begins with "h".
#'  - `Medium`: begins with "m".
#'  - `Low`: begins with "l".
#'  - `Not Focal Site`: begins with "n" or "x".
#' @param iss_data `tibble` ISS data.
#' @param start_date `str` Start date of desk review.
#' @param end_date `str` End date of desk review.
#' @param priority_col `str` Column representing priority level.
#' @param start_time_col `str` Column representing start time.
#' @param unreported_cases_col `str` Column representing unreported cases.
#' @param prov_col `str` Column representing province.
#' @param dist_col `str` Column representing district.
#' @param hf_col `str` Column representing the health facility name.
#' @param today_col `str` Column representing when info was recorded.
#' @param date_of_visit_col `str` Column representing date of visit.
#' @param ctry.data `list` `r lifecycle::badge("deprecated")` Please pass the
#' ISS data directly to the iss.data parameter.
#'
#' @returns `tibble` Cleaned eSurv/ISS data.
#' @examples
#' \dontrun{
#' iss_path <- "C:/Users/ABC1/Desktop/iss_data.csv"
#' ctry.data <- init_dr("somalia", iss_data_path = iss_path)
#' ctry.data$iss.data <- clean_iss_data(ctry.data$iss.data, start_date, end_date)
#' }
#'
#' @export
clean_iss_data <- function(iss_data, start_date, end_date,
                           priority_col = "priority_level",
                           start_time_col = "starttime",
                           unreported_cases_col = "num_unreportedcases",
                           prov_col = "states",
                           dist_col = "districts",
                           hf_col = "name_of_facility_visited",
                           today_col = "today",
                           date_of_visit_col = "date_of_visit",
                           ctry.data = lifecycle::deprecated()) {
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop(
      'Package "zoo" must be installed to use this function.',
      call. = FALSE
    )
  }

  if (lifecycle::is_present(ctry.data)) {
    lifecycle::deprecate_warn(
      when = "1.3.0",
      what = "clean_iss_data(ctry.data)",
      details = "Please pass the dataframe directly to iss_data."
    )

    if (is.null(ctry.data$iss.data)) {
      message("No ISS data attached.")
      return(ctry.data)
    }

    iss_data <- ctry.data$iss.data
  }

  # check if already cleaned
  if ("monyear" %in% names(iss_data)) {
    cli::cli_alert_warning("ISS data already cleaned.")
    return(iss_data)
  }

  cli::cli_process_start("Standardizing priority levels")
  iss.02 <- iss_data |>
    dplyr::mutate(priority_level = dplyr::case_when(
      stringr::str_to_lower(substr(get(priority_col), 1, 1)) == "h" ~ "High",
      stringr::str_to_lower(substr(get(priority_col), 1, 1)) == "m" ~ "Medium",
      stringr::str_to_lower(substr(get(priority_col), 1, 1)) == "l" ~ "Low",
      stringr::str_to_lower(substr(get(priority_col), 1, 1)) %in% c("n", "x") ~ "Not Focal Site",
      TRUE ~ get(priority_col)
    )) |>
    dplyr::mutate(priority_level = factor(priority_level, levels = c(
      "High", "Medium", "Low", "Not Focal Site"
    ))) |>
    dplyr::mutate(priority_level = dplyr::if_else(is.na(priority_level), "Not Focal Site", priority_level))
  cli::cli_process_done()

  cli::cli_process_start("Adding date columns")
  iss.02 <- iss.02 |>
    dplyr::mutate(
      monyear = zoo::as.yearmon(as.Date(.data[[start_time_col]])),
      month = lubridate::month(as.Date(.data[[start_time_col]])),
      year = lubridate::year(as.Date(.data[[start_time_col]]))
    ) |>
    dplyr::mutate(
      today_date = lubridate::as_date(.data[[today_col]], format = "%m/%d/%Y"),
      date_of_visit = lubridate::as_date(.data[[date_of_visit_col]], format = "%m/%d/%Y")
    )
  cli::cli_process_done()

  cli::cli_process_start("counting unreported AFP cases")
  if (is.null(unreported_cases_col)) {
    message("Column for unreported AFP cases unavailable.")
  } else {
    # Unreported AFP
    iss.02 <- iss.02 |>
      dplyr::mutate(unrep_afp = as.numeric(.data[[unreported_cases_col]])) |>
      suppressWarnings()
  }
  cli::cli_process_done()

  # Province and District
  cli::cli_process_start("Standardizing province and district names")
  iss.02 <- iss.02 |>
    dplyr::mutate(
      prov = toupper(.data[[prov_col]]),
      dists = toupper(iconv(.data[[dist_col]], to = "ASCII//TRANSLIT"))
    )
  cli::cli_process_done()

  # Convert "n/a" characters to actual null values
  cli::cli_process_start("Converting n/a characters to actual null values")
  iss.02 <- iss.02 |>
    dplyr::mutate(
      dists = dplyr::if_else(dists == "N/A", NA, dists),
      prov = dplyr::if_else(prov == "N/A", NA, prov)
    )
  cli::cli_process_done()

  # Remove accents
  cli::cli_process_start("Performing cleaning for names")
  iss.02 <- iss.02 |>
    dplyr::mutate(facility_name2 = iconv(.data[[hf_col]],
      to = "ASCII//TRANSLIT"
    ))

  # Make all capital letters and remove extra white space
  iss.02 <- iss.02 |>
    dplyr::mutate(facility_name2 = toupper(facility_name2)) |>
    dplyr::mutate(facility_name2 = stringr::str_squish(facility_name2))
  cli::cli_process_done()

  return(iss.02)
}

#' Checks for errors in the ISS data
#'
#' Currently, the function reports the number of missing priority levels.
#'
#' @param iss_data `tibble` ISS data.
#' @param error_path `str` Path to error folder. The function defaults to a global environment
#' variable called `DR_ERROR_PATH`, as it is assumed ISS data error checking is done as part of
#' the desk review template. The setting of desk review environmental variables is automatically
#' handled by [init_dr()]. Otherwise, users should manually specify the error folder.
#' @param ctry.data `list` `r lifecycle::badge("deprecated")` Please pass the ISS
#' data directly to the iss.data parameter.
#' @returns Status messages on the checks completed and results.
#' @examples
#' \dontrun{
#' iss_path <- "C:/Users/ABC1/Desktop/iss_data.csv"
#' ctry.data <- init_dr("somalia", iss_data_path = iss_path)
#' iss_data_errors(ctry.data$iss.data)
#' }
#'
#' @export
iss_data_errors <- function(iss_data, error_path = Sys.getenv("DR_ERROR_PATH"),
                            ctry.data = lifecycle::deprecated()) {
  if (lifecycle::is_present(ctry.data)) {
    lifecycle::deprecate_warn(
      when = "1.3.0",
      what = "iss_data_errors(ctry.data)",
      details = "Please pass the dataframe directly to iss_data."
    )

    # Check if ISS data is attached
    if (is.null(ctry.data$iss.data)) {
      return(message("ISS data not attached to ctry.data. Please attach and try again."))
    }
    iss_data <- ctry.data$iss.data
  }
  # Check for rows without any priority_levels (N/A)
  cli::cli_process_start("Checking for missing priority levels.")
  total_records <- nrow(iss_data)
  na_priority <- NULL
  if ("priority_level" %in% names(iss_data)) {
    na_priority <- iss_data |>
      dplyr::mutate(priority_level = stringr::str_to_lower(priority_level)) |>
      dplyr::filter(priority_level %in% c("na", "n/a", ""))
  } else if ("hf_rating" %in% names(iss_data)) {
    na_priority <- iss_data |>
      dplyr::mutate(hf_rating = stringr::str_to_lower(hf_rating)) |>
      dplyr::filter(hf_rating %in% c("na", "n/a", ""))
  }

  if (nrow(na_priority) > 0) {
    message <- paste0(
      "There are ", nrow(na_priority),
      " records missing priority levels (",
      round(nrow(na_priority) / total_records * 100, 2),
      "% of the total dataset.)"
    )
    cli::cli_alert_warning(message)
    readr::write_csv(na_priority, file.path(error_path, "missing_priority_iss.csv"))
  } else {
    cli::cli_alert_success("No records missing priority levels.")
  }

  cli::cli_process_done()
}
