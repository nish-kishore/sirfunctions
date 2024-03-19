
initiate_research <- function(country_name, start_date, end_date) {
  # Check for valid arguments
  country_name <- stringr::str_trim(stringr::str_to_upper(country_name))
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  # Set up local directory for storing for data and metadata
  df_name <- stringr::str_c(country_name, start_date, end_date, sep = "_")

  # Relative path of where data and metadata is stored
  data_path <- file.path("data", "countries")

  # Determine the file size of the data to be downloaded
  start_date_year <- lubridate::year(start_date)
  data_size <- if (start_date_year >= 2019) {
    "small"
  } else if (start_date_year >= 2016) {
    "medium"
  } else {
    "large"
  }

  # Set up folders
  data_dir_path <- setup_local_folders(data_path, country_name)

  # Create a local copy of the dataset in the folder, or update it.
  dr_data_path <- file.path(data_dir_path, df_name)

  # Instantiate variable containing country data
  country_data <- initiate_research_main(data_dir_path, data_size, country_name, dr_data_path)

  return(country_data)
}

# helper function
copy_new_data <- function(data_size, country_name, dr_data_path) {
  print("Saving a new copy of the dataset.")
  raw.data <- sirfunctions::get_all_polio_data(size = data_size)
  country_data <- sirfunctions::extract_country_data(country_name, raw.data)
  readr::write_rds(country_data, paste(dr_data_path, "saved_on", Sys.Date(), sep = "_"))
  return(country_data)
}

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
  return(data_dir_path)
}

initiate_research_main <- function(data_dir_path, data_size, country_name, dr_data_path) {
  country_data <- NA
  data_exists <- length(list.files(data_dir_path)) != 0
  if (data_exists) {
    response <- T
    while (response) {
      create_new_save <- readline("Previous save(s) found. Save a new copy? Type 'y','n', or 'q' to exit. > ")
      create_new_save <- stringr::str_trim(stringr::str_to_lower(create_new_save))
      if (create_new_save == "y") {
        country_data <- copy_new_data(data_size, country_name, dr_data_path)
        response <- F
        break
      } else if (create_new_save == "n") {
        print("Please choose the file to load: ")
        tryCatch(
          {
            load_data <- file.choose()
            country_data <- readr::read_rds(load_data)
            response <- F
            break
          },
          error = function(cond) {
            cond$message <- "Aborting dataset loading..."
            stop(cond)
          }
        )
      } else if (create_new_save == "q") {
        print("Aborting dataset loading...")
        response <- F
        break
      } else {
        print("Invalid response. Please type 'y', 'n', or 'q' to exit.")
      }
    }
  } else {
    country_data <- copy_new_data(data_size, country_name, dr_data_path)
  }

  return(country_data)
}


initiate_desk_review <- function(country_name, start_date, end_date) {
  return(0)
}
