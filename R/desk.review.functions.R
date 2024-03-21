# "private" methods ----

#' Pull new data and write to the specified file path
#'
#' @param data_size `str` size of polio data to pull.
#' "small" (>= 2019), "medium" (>= 2016), large" (full)
#' @param country_name `str` name of the country to pull data from
#' @param dr_data_path `str` path to save the data set to`
#'
#' @return `list` large list containing polio data
copy_new_data <- function(data_size, country_name, dr_data_path) {
  message("Saving a new copy of the dataset.")
  path_to_save <- paste0(paste(dr_data_path, "saved_on", Sys.Date(), sep = "_"), ".Rds")

  # Overwrite the data
  if (file.exists(path_to_save)) {
    file.remove(path_to_save)
  }

  raw_data <- sirfunctions::get_all_polio_data(size = data_size)
  country_data <- sirfunctions::extract_country_data(country_name, raw_data)
  readr::write_rds(country_data, path_to_save)
  message(paste0("Data saved at:\n", dr_data_path))
  return(country_data)
}

#' Creates a meta data file
#'
#' @param path location where the .txt file is located/should be created
#'
#' @return this function does not return anything
create_metadata <- function(path) {
  date_updated <- paste("Updated:", Sys.time(), sep = " ")
  file_location <- paste("Project working directory:", getwd(), sep = " ")

  if (!exists(path)) {
    file.create(path)
  }

  write_lines(file = path,
              x = c(date_updated, file_location))
}

#' Loads .Rds file
#'
#' @param data_dir_path `str` data path
#'
#' @return `list` large list containing the polio data
load_data <- function(data_dir_path) {
  cli::cli_alert_info("Choose the file to load by entering the line number from above or type 'q' to quit: ")
  load_data_response <- T
  while (load_data_response) {
    load_data <- readline("> ")
    load_data <- stringr::str_trim(load_data)
    if (load_data == "q") {
      return(message("Aborting dataset loading..."))
    }

    load_data <- suppressWarnings(as.integer(load_data))
    if (is.na(load_data) | load_data == 0) {
      message("Invalid response. Please try again.")
    } else if (load_data <= length(list.files(data_dir_path))) {
      chosen_file <- list.files(data_dir_path)[load_data]
      chosen_file_path <- file.path(data_dir_path, chosen_file)
      message(paste0("Loading ", chosen_file))
      country_data <- readr::read_rds(chosen_file_path)
      return(country_data)
    } else {
      message("Invalid response. Please try again.")
    }
  }
}


#' Determine the size of data to pull
#'
#' @param year `int` start year of the desk review
#'
#' @return `str` size of the data
set_data_size <- function(year) {
  if (year >= 2019) {
    "small"
  } else if (year >= 2016) {
    "medium"
  } else {
    "large"
  }
}

#' Sets up local folders in the current working directory for storing data and metadata
#'
#' @param data_path `str` path where data is/to be stored
#' @param country_name `str` name of the country
#'
#' @return `str` returns the path containing data for the country
setup_local_folders <- function(data_path, country_name) {
  # Check if required directories exist locally and create it if it does not
  if (!dir.exists(data_path)) {
    dir.create(data_path, recursive = T)
  }

  # Check if a country folder already exists and create it if it does not
  local_country_path <- file.path(data_path, country_name)
  if (!dir.exists(local_country_path)) {
    dir.create(local_country_path, recursive = T)
  }

  # Check if a country folder contains data and metadata folders and create it if
  # it does not
  data_dir_path <- file.path(local_country_path, "data")
  metadata_dir_path <- file.path(local_country_path, "metadata")
  if (!dir.exists(data_dir_path)) {
    dir.create(data_dir_path, recursive = T)
  }
  if (!dir.exists(metadata_dir_path)) {
    dir.create(metadata_dir_path, recursive = T)
  }
  return(local_country_path)
}


#' Handles the logic of which files to load in the current R session
#'
#' @param data_dir_path `str` path to the dataset
#' @param data_size `str` "small", "medium", or "large"
#' @param country_name `str` name of the country
#' @param dr_data_path `str` path to save the dataset to
#'
#' @return `list` large list containing polio data
initiate_research_logic <- function(data_dir_path, data_size, country_name, dr_data_path) {

  data_exists <- length(list.files(data_dir_path)) != 0
  if (data_exists) {
    response <- T
    while (response) {
      cli::cli_alert_info("Previous save(s) found:")

      for (i in 1:length(list.files(data_dir_path))) {
        message(paste0(i, ") ", list.files(data_dir_path)[i]))
      }

      message("Save a new copy? Type 'y','n', or 'q' to quit.")
      create_new_save <- readline("> ")
      create_new_save <- stringr::str_trim(stringr::str_to_lower(create_new_save))

      if (create_new_save == "y") {
        message(paste0("Data saving at:\n", data_dir_path))
        return(copy_new_data(data_size, country_name, dr_data_path))
      } else if (create_new_save == "n") {
        return(load_data(data_dir_path))
      } else if (create_new_save == "q") {
        message("Aborting dataset loading...")
        response <- F
      } else {
        message("Invalid response. Please type 'y', 'n', or 'q' to quit.")
      }
    }
  } else {
    message(paste0("Data saved at:\n", data_dir_path))
    return(copy_new_data(data_size, country_name, dr_data_path))
  }
}

# "public" methods ----
#' Set up the folders and load polio data
#'
#' @param country_name `str` name of the country
#' @param start_date `str` start date of the desk review
#' @param end_date `str` end date of the desk review
#'
#' @return `list` large list containing all dataframe for all polio data
initiate_research <- function(country_name, start_date, end_date) {
  country_name <- stringr::str_trim(stringr::str_to_upper(country_name))
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  # Set up local directory for storing for data and metadata
  df_name <- stringr::str_c(country_name, start_date, end_date, sep = "_")

  # Relative path of where data and metadata is stored
  data_path <- file.path("data", "countries")

  # Set up local folders
  country_dir_path <- setup_local_folders(data_path, country_name)

  # Create a local copy of the dataset in the folder, or update it.
  dr_data_path <- file.path(country_dir_path, "data", df_name)

  # Determine the file size of the data to be downloaded
  start_date_year <- lubridate::year(start_date)
  data_size <- set_data_size(start_date_year)

  # Create meta data
  metadata_path <- file.path(country_dir_path,
                             "metadata",
                             paste0(paste(df_name, "metadata", sep = "_"), ".txt"))
  create_metadata(file.path(metadata_path))

  # Instantiate variable containing country data and desk review meta data
  data_path <- file.path(country_dir_path, "data")
  country_data <- initiate_research_logic(
    data_path, data_size, country_name,
    dr_data_path
  )
  return(country_data)
}

initiate_desk_review <- function(country_name, start_date, end_date) {
  return(0)
}
