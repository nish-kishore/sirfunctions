#' Title
#'
#' @param country_name name of the country
#' @param start_date start date of the desk review
#' @param end_date end date of the desk review
#' @param file_path local path of where the desk review data is stored
#'
#' @return
#' @export
dr_init <- function(country_name, start_date, end_date, file_path) {

  country_name <- stringr::str_trim(stringr::str_to_upper(country_name))
  tryCatch(
    {
      start_date <- lubridate::as_date(start_date)
      end_date <- lubridate::as_date(end_date)
    },
    error = function(e) {
      e$message <- "Invalid format for 'start_date' or 'end_date'. Try again."
      stop(e)
    }
  )

  df_name <- stringr::str_c(country_name, start_date, end_date, sep = "_")
  data_path <- paste0("data/country/", df_name)
  start_date_year <- lubridate::year(start_date)
  data_size <- if (start_date_year >= 2019) {
    "small"
  } else if (start_date_year >= 2016) {
    "medium"
  } else {
    "large"
  }

  if (file.exists(data_path)) {
      response = T
      while (response) {
        answer <- readline("Previous save found. Update this file? Type 'y','n', or 'q' to exit.")
        if (answer %in% c("y", "n", "q") ) {

          if (answer == "y") {
            raw_data <- sirfunctions::get_all_polio_data(size = data_size)
            country_data <- sirfunctions::extract_country_data(country_name, raw_data)
            readr::write_rds(country_data, data_path)
            response = F
            break

          } else if (answer == "n") {
            message("Loading country data from previous save.")
            country_data <- readr::read_rds(data_path)
            response = F
            break
          } else if (answer == "q") {
            message("Country data not loaded.")
            return()
          }

        } else {
          print("Invalid response, please try again.")
        }
      }
  } else {
    print("Previous save not found. Creating new local data file and metadata file.")
    raw_data <- sirfunctions::get_all_polio_data(size = data_size)
    country_data <- sirfunctions::extract_country_data(country_name, raw_data)
    readr::write_rds(country_data, data_path)
  }

  return(country_data)
}
