#' Read ISS/eSURV data
#'
#' @param iss_path path to the excel or csv file
#' @param sheet_name optional name of the ISS data
#'
#' @return a tibble containing ISS/eSURV data
#' @export
load_iss_data <- function(iss_path, sheet_name=NULL) {
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
#' @param ctry.data ctry.data containing iss.data
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#' @param priority_col column representing priority level as a string
#' @param start_time_col column representing start time as a string
#' @param unreported_cases_col column representing unreported cases as a string
#' @param prov_col column representing province as a string
#' @param dist_col column representing district as a string
#' @param hf_col column representing the health facility name as a string
#' @param today_col column representing when info was recorded
#' @param date_of_visit_col column representing date of visit
#'
#' @return a tibble of cleaned ISS data
#' @export
clean_iss_data <- function(ctry.data, start_date, end_date,
                           priority_col="priority_level",
                           start_time_col="starttime",
                           unreported_cases_col="num_unreportedcases",
                           prov_col="states",
                           dist_col="districts",
                           hf_col="name_of_facility_visited",
                           today_col="today",
                           date_of_visit_col="date_of_visit") {

  # check if already cleaned
  if ("monyear" %in% names(ctry.data$iss.data)) {
    cli::cli_alert_warning("ISS data already cleaned.")
    return(ctry.data$iss.data)
  }

  if (is.null(ctry.data$iss.data)) {
    message("No ISS data attached.")
    return(NULL)
  }

  cli::cli_process_start("Standardizing priority levels")
  iss.02 <- ctry.data$iss.data %>%
    mutate(priority_level = case_when(
      str_to_lower(substr(get(priority_col), 1, 1)) == "h" ~ "High",
      str_to_lower(substr(get(priority_col), 1, 1)) == "m" ~ "Medium",
      str_to_lower(substr(get(priority_col), 1, 1)) == "l" ~ "Low",
      str_to_lower(substr(get(priority_col), 1, 1)) %in% c("n", "x") ~ "Not Focal Site",
      TRUE ~ get(priority_col)
    )) %>%
    mutate(priority_level = factor(.data$priority_level, levels = c(
      "High", "Medium", "Low", "Not Focal Site"
    ))) |>
    mutate(priority_level = if_else(is.na(.data$priority_level), "Not Focal Site", .data$priority_level))
  cli::cli_process_done()

  cli::cli_process_start("Adding date columns")
  iss.02 <- iss.02 |>
    mutate(
      monyear = zoo::as.yearmon(as.Date(.data[[start_time_col]])),
      month = month(as.Date(.data[[start_time_col]])),
      year = year(as.Date(.data[[start_time_col]]))
    ) |>
    mutate(today_date = as_date(.data[[today_col]], format = "%m/%d/%Y"),
           date_of_visit = as_date(.data[[date_of_visit_col]], format = "%m/%d/%Y"))
  cli::cli_process_done()

  cli::cli_process_start("counting unreported AFP cases")
  if (is.null(unreported_cases_col)) {
    message("Column for unreported AFP cases unavailable.")
  } else {
    # Unreported AFP
    iss.02 <- iss.02 %>%
      mutate(unrep_afp = as.numeric(.data[[unreported_cases_col]])) |>
      suppressWarnings()
  }
  cli::cli_process_done()

  # Province and District
  cli::cli_process_start("Standardizing province and district names")
  iss.02 <- iss.02 %>%
    mutate(
      prov = toupper(.data[[prov_col]]),
      dists = toupper(iconv(.data[[dist_col]], to = "ASCII//TRANSLIT"))
    )
  cli::cli_process_done()

  # Convert "n/a" characters to actual null values
  cli::cli_process_start("Converting n/a characters to actual null values")
  iss.02 <- iss.02 |>
    mutate(dists = if_else(.data$dists == "N/A", NA, .data$dists),
           prov = if_else(.data$prov == "N/A", NA, .data$prov)
    )
  cli::cli_process_done()

  # Remove accents
  cli::cli_process_start("Performing cleaning for names")
  iss.02 <- iss.02 %>%
    mutate(facility_name2 = iconv(.data[[hf_col]],
                                  to = "ASCII//TRANSLIT"))

  # Make all capital letters and remove extra white space
  iss.02 <- iss.02 %>%
    mutate(facility_name2 = toupper(.data$facility_name2)) %>%
    mutate(facility_name2 = str_squish(.data$facility_name2))
  cli::cli_process_done()

  return(iss.02)
}

#' Checks for errors in the ISS data.
#' Currently reports the number of missing priority levels.
#'
#' @param ctry.data ctry.data with ISS/eSurv data attached
#' @param error_path path to error folder
#'
#' @return error message
#' @export
iss_data_errors <- function(ctry.data, error_path=Sys.getenv("DR_ERROR_PATH")) {

  # Check if ISS data is attached
  if (is.null(ctry.data$iss.data)) {
    stop("ISS data not attached to ctry.data. Please attach and try again.")
  }

  iss.data <- ctry.data$iss.data
  # Check for rows without any priority_levels (N/A)
  cli::cli_process_start("Checking for missing priority levels.")
  total_records <- nrow(iss.data)
  na_priority <- NULL
  if ("priority_level" %in% names(iss.data)) {

    na_priority <- iss.data |>
      mutate(priority_level = stringr::str_to_lower(.data$priority_level)) |>
      filter(priority_level %in% c("na", "n/a", ""))

  } else if ("hf_rating" %in% names(iss.data)) {

    na_priority <- iss.data |>
      mutate(hf_rating = stringr::str_to_lower(.data$hf_rating)) |>
      filter(hf_rating %in% c("na", "n/a", ""))
  }

  if (nrow(na_priority) > 0) {
    message <- paste0("There are ", nrow(na_priority),
                      " records missing priority levels (",
                      round(nrow(na_priority)/total_records * 100, 2),
                      "% of the total dataset.)")
    cli::cli_alert_warning(message)
    readr::write_csv(na_priority, file.path(error_path, "missing_priority_iss.csv"))
  } else {
    cli::cli_alert_success("No records missing priority levels.")
  }

  cli::cli_process_done()
}
