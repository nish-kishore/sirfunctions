# "private" methods ----

#' Sets up local folders in the current working directory for desk review
#' data_path is expected to be ~/country/year
#'
#' @param data_path `str` path where folders are to be created
#'
set_dr_local_folders <- function(path) {
  # Check if required directories exist locally and create it if it does not
  if (!dir.exists(path)) {
    dir.create(path, recursive = T)
  }

  # Check if a country folder already exists and create it if it does not
  local_country_path <- file.path(path)
  if (!dir.exists(local_country_path)) {
    dir.create(local_country_path, recursive = T)
  }

  # Check if a country folder contains data and metadata folders and create it if
  # it does not
  data_dir_path <- file.path(local_country_path, "data")
  metadata_dir_path <- file.path(local_country_path, "metadata")
  fig_dir_path <- file.path(local_country_path, "figures")
  error_dir_path <- file.path(local_country_path, "errors")
  param_dir_path <- file.path(local_country_path, "paramaters")

  for (i in c(data_dir_path,
              metadata_dir_path,
              fig_dir_path,
              error_dir_path,
              param_dir_path)) {
    if (!dir.exists(i)) {
      dir.create(i, recursive = T)
    }
  }
  message(paste0("Successfully created local folder for desk review at:\n", path))
  return(path)
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

#' Pull new data and write to the specified file path
#'
#' @param data_size `str` size of polio data to pull.
#' "small" (>= 2019), "medium" (>= 2016), large" (full)
#' @param country_name `str` name of the country to pull data from
#' @param dr_data_path `str` path to save the data set to.
#' Expected path is ./country/year/data
#' @param attach_spatial_data whether to attach spatial data
#'
#' @return `list` large list containing polio data
update_data <-
  function(data_size,
           country_name,
           dr_data_path,
           attach_spatial_data) {
    message("Saving a new copy of the dataset.")
    path_to_save <-
      paste0(paste(dr_data_path, "saved_on", Sys.Date(), sep = "_"), ".Rds")

    # Overwrite the data
    if (file.exists(path_to_save)) {
      file.remove(path_to_save)
    }

    raw_data <-
      get_all_polio_data(size = data_size, attach.spatial.data = attach_spatial_data)
    country_data <- extract_country_data(country_name, raw_data)
    readr::write_rds(country_data, path_to_save)
    message(paste0("Data saved at:\n", dr_data_path))
    return(country_data)
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

#' Creates a meta data file
#'
#' @param path location where the .txt file is located/should be created
#'
#' @return this function does not return anything
create_metadata <- function(path) {
  date_updated <- paste("Updated:", Sys.time(), sep = " ")
  file_location <-
    paste("Project working directory:", path, sep = " ")

  if (!exists(path)) {
    file.create(path)
  }

  write_lines(file = path,
              x = c(date_updated, file_location))
}

#' Handles the logic of which files to load in the current R session
#'
#' @param dr_data_path `str` path to the dataset
#' @param data_size `str` "small", "medium", or "large"
#' @param country_name `str` name of the country
#' @param data_path path of the data folder used in the desk review
#' @param attach_spatial_data whether to attach spatial data
#'
#' @return `list` large list containing polio data
generate_data <-
  function(data_path,
           data_size,
           country_name,
           dr_data_path,
           attach_spatial_data) {
    data_exists <- length(list.files(data_path)) != 0
    if (data_exists) {
      response <- T
      while (response) {
        cli::cli_alert_info("Previous save(s) found:")

        for (i in 1:length(list.files(data_path))) {
          message(paste0(i, ") ", list.files(data_path)[i]))
        }

        message("Save a new copy? Type 'y','n', or 'q' to quit.")
        create_new_save <- readline("> ")
        create_new_save <-
          stringr::str_trim(stringr::str_to_lower(create_new_save))

        if (create_new_save == "y") {
          message(paste0("Data saving at:\n", dr_data_path))
          return(update_data(
            data_size,
            country_name,
            dr_data_path,
            attach_spatial_data
          ))
        } else if (create_new_save == "n") {
          return(load_data(data_path))
        } else if (create_new_save == "q") {
          message("Aborting dataset loading...")
          response <- F
        } else {
          message("Invalid response. Please type 'y', 'n', or 'q' to quit.")
        }
      }
    } else {
      message(paste0("Data saved at:\n", dr_data_path))
      return(update_data(
        data_size,
        country_name,
        dr_data_path,
        attach_spatial_data
      ))
    }
  }

generate_pptx_assumptions <- function(date_first, date_last) {
  pptx.assumptions <- c(
    'Data sources:',
    paste0(
      'POLIS (Data as of ',
      format(start_date, '%d-%b-%Y'),
      ' to ',
      format(end_date, '%d-%b-%Y') ,
      ')'
    ),
    'Missing population estimated from the UNDP growth factor from previous year’s population.',
    paste0(
      'Timeframe for analysis: ',
      format(start_date, '%d-%b-%Y'),
      ' to ',
      format(end_date, '%d-%b-%Y')
    ),
    paste0(
      'Some selected figures include additional data (',
      format(start_date, "%b %Y"),
      "-",
      format(Sys.Date(), "%b %Y"),
      ")"
    ),
    # UPDATE AS NEEDED
    'NPAFP Assumptions',
    'NPAFP cases with all pending cases included (Pending Lab and Pending Classification)',
    'Stool Adequacy Assumptions',
    'All AFP cases',
    'Samples with missing stool condition were considered good quality',
    'Samples with bad date data (e.g. collection before onset) were considered inadequate'
  )

  assump = unordered_list(
    level_list = c(1, 2, 2, 1, 2, 1, 2, 1, 2, 2, 2),
    #Indentation level for each bullet
    str_list = pptx.assumptions,
    style = fp_text(color = "black", font.size = 18)
  )

  return(assump)
}

# "public" methods ----
#' Set up the folders and load polio data
#'
#' @param country_name `str` name of the country
#' @param start_date `str` start date of the desk review
#' @param end_date `str` end date of the desk review
#' @param local_dr_repo folder where the desk review code is located
#' @param attach_spatial_data boolean whether to include spatial data
#'
#' @return `list` large list containing all dataframe for all polio data
#' @export
init_dr <-
  function(country_name,
           start_date,
           end_date,
           local_dr_repo,
           attach_spatial_data) {
    country_name <-
      stringr::str_trim(stringr::str_to_upper(country_name))
    start_date <- lubridate::as_date(start_date)
    end_date <- lubridate::as_date(end_date)
    year <- lubridate::year(start_date)

    # Set up local directory for storing for data and metadata
    df_name <-
      stringr::str_c(stringr::str_to_lower(country_name),
                     start_date,
                     end_date,
                     sep = "_")

    # Relative path of where data and metadata is stored
    dr_path <-
      file.path(local_dr_repo,
                stringr::str_to_lower(country_name),
                lubridate::year(Sys.Date()))

    # Set up local folders
    country_dir_path <- set_dr_local_folders(dr_path)

    # Create a local copy of the dataset in the folder, or update it.
    dr_data_path <- file.path(country_dir_path, "data", df_name)

    # Determine the file size of the data to be downloaded
    data_size <- set_data_size(year)

    # Create meta data
    metadata_path <- file.path(dr_path,
                               "metadata",
                               paste0(paste(df_name, "metadata", sep = "_"), ".txt"))
    create_metadata(file.path(metadata_path))

    # Instantiate variable containing country data and desk review meta data
    data_path <- file.path(country_dir_path, "data")
    country_data <- generate_data(data_path,
                                  data_size,
                                  country_name,
                                  dr_data_path,
                                  attach_spatial_data)
    return(country_data)
  }

#' Upload desk review script to the sg-desk-review Github repository
#'
#' @param file_path location of the file to upload to the sg-desk-review repo
#' @param repo_path local path of the sg-desk-review repo
#' @param message message to include in the commit
#'
#' @return Status message
#' @export
upload_dr_to_github <-
  function(file_path, repo_path, message = "updating file") {
    # Check if the repository is initialized
    if (is.null(git2r::discover_repository(repo_path))) {
      stop("Git repository not found at ", repo_path)
    }

    # Open the repository
    repo <- git2r::repository(repo_path)

    # Get the name of the current file
    file_name <- basename(file_path)

    # Copy over the file in the file_path to the repo_path
    file.copy(file_path, repo_path, recursive = T)

    # Add the file to the repository
    tryCatch({
      git2r::add(repo, path = file.path(repo_path, file_name))

      # Commit the changes with a message including the file name
      commit_msg <- paste0(message, ": ", file_name)
      git2r::commit(repo, message = commit_msg)
      message("Changes committed successfully. Please push using GitHub Desktop or command line ")
    }, error = function(e) {
      message("No changes to the file.")
    })

    # tryCatch({
    #   git2r::push(repo)
    #   message("Changes pushed successfully.")
    # },
    # error = function(e) {
    #   stop(message("Unable to push changes. Check if Git is set up correctly."))
    # })
  }


#' Freeze desk review data to the desk review folder in EDAV
#'
#' @param rds_obj Rds object loaded in R
#' @param file_name name given to the Rds object, do not append .rds
#' @param country country as a string
#' @param year year as an integer
#'
#' @return A status message
#' @export
freeze_dr_data <- function(rds_obj, country, year, file_name) {
  sirfunctions::edav_io(
    io = "write",
    default_dir = NULL,
    file_loc = file.path(
      "GID/PEB/SIR/Data/desk_review",
      country,
      year,
      paste0(file_name, ".rds")
    ),
    obj = rds_obj
  )
  message("File saved successfully.")
}

#' Fetch the Rds file to be used for a desk review
#'
#' @param country country name as a string
#' @param year year as an integer
#' @param local_dr_repo the local desk review repository
#'
#' @return A status message
#' @export
fetch_dr_data <- function(country, year, local_dr_repo) {
  country <- stringr::str_to_lower(str_trim(country))
  files <- sirfunctions::edav_io(
    io = "list",
    default_dir = NULL,
    file_loc = file.path("GID/PEB/SIR/Data/desk_review",
                         country, year)
  ) |>
    pull(name)

  file_names <- basename(files)

  if (length(file_names) == 0) {
    stop("This directory is empty.")
  }

  cli::cli_alert_info("Previous save(s) found:")
  for (i in 1:length(file_names)) {
    message(paste0(i, ") ", file_names[i]))
  }

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
    } else if (load_data <= length(file_names)) {
      chosen_file <- file_names[load_data]
      message(paste0("Loading ", chosen_file))
      rds_data <-
        sirfunctions::edav_io(
          io = "read",
          default_dir = NULL,
          file_loc = file.path(
            file.path("GID/PEB/SIR/Data/desk_review",
                      country, year),
            chosen_file
          )
        )
      message(paste0(
        "Saving Rds data to: ",
        file.path(country, year, "data", chosen_file)
      ))

      repo_path <- file.path(local_dr_repo, country, year, "data")
      file_path <-
        file.path(local_dr_repo, country, year, "data", chosen_file)

      if (!dir.exists(repo_path)) {
        dir.create(repo_path, recursive = T)
      }

      if (file.exists(file_path)) {
        file.remove(file_path)
      }

      readr::write_rds(file = file_path, x = rds_data)
      message("Rds file saved successfully.")
    } else {
      message("Invalid response. Please try again.")
    }
  }
}

#' Checks data quality errors from the country data
#'
#' @param ctry.data RDS object containing polio country data
#'
#' @return a list containing all the errors that are checked for
#' @export
dr_data_errors <- function(ctry.data) {
  message("Checking for data quality issues")

  # afp.all.2
  cli::cli_progress_step("Performing checks for afp.all.2")
  missing_ctry <- check_missing_geo(ctry.data$afp.all.2, "ctry")
  missing_prov <- check_missing_geo(ctry.data$afp.all.2, "prov")
  missing_dist <- check_missing_geo(ctry.data$afp.all.2, "dist")

  # population check
  cli::cli_progress_step("Performing checks for population files.\n")
  pop_file <- check_pop_rollout(ctry.data)

  # spatial validation check across country, province, lab
  cli::cli_progress_step("Spatial validation for country")
  incomplete.adm.ctry <-
    spatial_validation(ctry.data$ctry.pop, "ctry")

  cli::cli_progress_step("Spatial validation for province")
  incomplete.adm.prov <-
    spatial_validation(ctry.data$prov.pop, "prov")

  cli::cli_progress_step("Spatial validation for district")
  incomplete.adm.dist <-
    spatial_validation(ctry.data$dist.pop, "dist")

  cli::cli_process_done(msg_done = "Check returned list for error specifics.")
  cli::cli_alert("Run dr_data_cleaning() to attempt data fixes and perform the check again.")

  error_log <- list()
  error_log$missing_ctry <- missing_ctry
  error_log$missing_prov <- missing_prov
  error_log$missing_dist <- missing_dist
  error_log$pop_rollup_diff <- pop_file
  error_log$invalid_adm0 <- incomplete.adm.ctry
  error_log$invalid_adm1 <- incomplete.adm.prov
  error_log$invalid_adm2 <- incomplete.adm.dist

  return(error_log)

}

#' Cleans and adds additional columns used in the desk reviews
#'
#' @param ctry.data country data RDS object
#' @param start_date start date for desk review
#' @param end_date end date for desk review
#' @param es_start_date start date for environmental surveillance data
#' @param es_end_date end date for environmental surveillance data
#'
#' @return cleaned country data RDS object
#' @export
clean_ctry_data <- function(ctry.data, start_date, end_date, es_start_date, es_end_date) {
  ctry.data$afp.all.2 <- impute_dist_afp(ctry.data$afp.all.2)
  ctry.data$afp.all.2 <- col_to_datecol(ctry.data$afp.all.2)
  ctry.data$afp.all.2 <- add_zero_dose_col(ctry.data$afp.all.2)
  ctry.data$es <- clean_es_data(ctry.data$es, es_start_date, es_end_date)

  return(ctry.data)
}


#' Function to load the raw lab data
#'
#' @param lab_data_path file path as a string to the lab data
#'
#' @return a tibble containing lab data
#' @export
load_lab_data <- function(lab_data_path) {
  lab_data <- readxl::read_excel(lab_data_path)
  return(lab_data)
}

#' Function to load the raw ISS/eSurv data
#'
#' @param iss_data_path
#'
#' @return a tibble
#' @export
load_iss_data <- function(iss_data_path) {
  iss_data <- read_csv(iss_data_path)
  return(iss_data)
}