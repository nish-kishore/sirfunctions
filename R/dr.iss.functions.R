#' Read ISS/eSURV data
#'
#' @param iss_path path to the .csv file
#'
#' @return a tibble containing ISS/eSURV data
#' @export
load_iss_data <-function(iss_path) {
  return(read_csv(iss_path))
}

#' Perform common cleaning tasks for ISS/eSURV data
#'
#' @param iss.data tibble of ISS data
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#' @param priority_col column representing priority level as a string
#' @param start_time_col column representing start time as a string
#' @param unreported_cases_col column representing unreported cases as a string
#' @param prov_col column representing province as a string
#' @param dist_col column representing district as a string
#' @param hf_col column representing the health facility name as a string
#'
#' @return a tibble of cleaned ISS data
#' @export
clean_iss_data <- function(ctry.data, start_date = start_date, end_date = end_date,
                           priority_col="priority_level",
                           start_time_col="starttime",
                           unreported_cases_col="num_unreportedcases",
                           prov_col="states",
                           dist_col="districts",
                           hf_col="name_of_facility_visited") {

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
    mutate(priority_level = factor(priority_level, levels = c(
      "High", "Medium", "Low", "Not Focal Site"
    ))) |>
    mutate(priority_level = if_else(is.na(priority_level), "Not Focal Site", priority_level))
  cli::cli_process_done()

  cli::cli_process_start("Adding date columns")
  iss.02 <- iss.02 |>
    mutate(
      monyear = as.yearmon(as.Date(.data[[start_time_col]])),
      month = month(as.Date(.data[[start_time_col]])),
      year = year(as.Date(.data[[start_time_col]]))
    ) |>
    mutate(today_date = as_date(today, format = "%m/%d/%Y"),
           date_of_visit = as_date(date_of_visit, format = "%m/%d/%Y"))
  cli::cli_process_done()

  cli::cli_process_start("counting unreported AFP cases")
  # Unreported AFP
  iss.02 <- iss.02 %>%
    mutate(unrep_afp = as.numeric(.data[[unreported_cases_col]])) |>
    suppressWarnings()
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
    mutate(dists = if_else(dists == "N/A", NA, dists),
           prov = if_else(prov == "N/A", NA, prov)
    )
  cli::cli_process_done()

  # Remove accents
  cli::cli_process_start("Performing cleaning for names")
  iss.02 <- iss.02 %>%
    mutate(facility_name2 = iconv(.data[[hf_col]],
                                  to = "ASCII//TRANSLIT"))

  # Make all capital letters and remove extra whitespace
  iss.02 <- iss.02 %>%
    mutate(facility_name2 = toupper(facility_name2)) %>%
    mutate(facility_name2 = str_squish(facility_name2))
  cli::cli_process_done()

  return(issy.02)
}
