# "private" methods ----

#' Downloads the desk review template code
#'
#' @param output_path where to download the desk review template code
#' @keywords internal
#'
copy_dr_template_code <- function(output_path = Sys.getenv("DR_PATH")) {
  dr_template_name <- "desk_review_template.Rmd"
  github_raw_url <- "https://raw.githubusercontent.com/nish-kishore/sg-desk-reviews/main/resources/desk_review_template.Rmd"

  # download only if it doesn't already exist
  data_folder_files <- list.files(output_path)
  if ((stringr::str_detect(data_folder_files, "_template") |> sum()) == 0) {
    cli::cli_process_start("Downloading the desk review template.")
    download.file(github_raw_url, file.path(output_path, dr_template_name))
    cli::cli_process_done()
  } else {
    message("Template file already exists. Skipping download.")
    return(NULL)
  }
}

#' Get functions used for the desk review from Github
#'
#' @param branch which branch to use
#' @param output_folder where the function scripts should be stored
#' @keywords internal
#'
#' @export
copy_dr_functions <- function(branch = "main", output_folder = Sys.getenv("DR_FUNC_PATH")) {
  repo <- "nish-kishore/sirfunctions"
  github_raw_url <- "https://raw.githubusercontent.com"
  github_folder_url <- "https://api.github.com/repos/nish-kishore/sirfunctions/git/trees"
  github_folder_url <- file.path(github_folder_url, paste0(branch, "?recursive=1"))
  req <- httr::GET(github_folder_url)
  file_path <- data.frame("paths" = unlist(lapply(httr::content(req)$tree, function(x) x$path)))

  # Filter only the relevant files
  file_path <- file_path |>
    dplyr::filter(stringr::str_starts(.data$paths, "R/dr.")) |>
    tidyr::separate(.data$paths, into = c("folder", "name"), remove = F, sep = "/")

  if (nrow(file_path) == 0) {
    stop("No desk review functions in this branch.")
  }

  # Check local folder for existing dr function scripts
  dr_func_files <- list.files(output_folder)
  diff <- intersect(dr_func_files, file_path$name)

  if (length(diff) > 0) {
    alert_message <- paste0(
      "There are existing function scripts in:\n",
      Sys.getenv("DR_FUNC_PATH"),
      "\n\nWould you like to update them? (Y/N)\n\n",
      "(NOTE: Updating will overwrite changes to the function scripts in the R folder of the associated country desk review. ",
      "If you changed a function, it is recommended to place changes in a new script file."
    )
    cli::cli_alert_warning(alert_message)

    wait <- T
    response <- NULL
    while (wait) {
      response <- readline("> ")
      response <- stringr::str_trim(stringr::str_to_lower(response))
      if (!response %in% c("y", "n")) {
        message("Invalid response. Try again.")
      } else {
        wait <- F
      }
    }
    if (response == "n") {
      message("Keeping current desk review function scripts.")
      return(NULL)
    }
  }


  # Loop and download the relevant files
  cli::cli_process_start("Downloading desk review functions")
  for (i in seq(1, nrow(file_path))) {
    file_url <- file.path(github_raw_url, repo, branch, file_path$paths[i])
    suppressWarnings(utils::download.file(file_url, file.path(output_folder, file_path$name[i])))
    Sys.sleep(1)
  }
  cli::cli_alert_success(paste0("Desk review functions downloaded successfully at:\n", output_folder))
  cli::cli_process_done()
}

#' Checks the cache file and see if it should load parameters from it
#'
#' @param param_path path to parameters.RData
#' @param start_date start date of the desk review
#' @param end_date  end date of the desk review
#' @param country_name name of the country
#' @keywords internal
#'
#' @returns boolean whether to use cache or not
#' @export
check_cache <- function(param_path, start_date, end_date, country_name = Sys.getenv("DR_COUNTRY")) {
  if (file.exists(param_path)) {
    cli::cli_alert_info("There is a param file from a previous run. Use this? (Y/N)")
    response <- T
    use <- NULL
    while (response) {
      use <- readline("> ")
      use <- stringr::str_trim(stringr::str_to_lower(use))

      if (!use %in% c("y", "n")) {
        message("Invalid response. Try again.")
      } else {
        response <- F
      }
    }

    if (use == "y") {
      cli::cli_alert("Loading previous parameters")
      load(param_path, envir = .GlobalEnv)
    } else {
      cli::cli_alert("Update the previous param file? (Y/N)")

      response <- T
      update <- NULL

      while (response) {
        update <- readline("> ")
        update <- stringr::str_trim(stringr::str_to_lower(update))

        if (!update %in% c("y", "n")) {
          message("Invalid response. Try again.")
        } else {
          response <- F
        }
      }

      if (update == "y") {
        save(start_date, end_date, country_name, file = param_path)
      } else {
        return(FALSE)
      }
    }
  } else {
    cli::cli_alert("Creating paramaters file")
    save(start_date, end_date, country_name, file = param_path)
  }
}

#' Sets up local folders in the current working directory for desk review
#' data_path is expected to be ~/country/year
#'
#' @param path location of the local desk review folder
#' @keywords internal
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
  tables_dir_path <- file.path(local_country_path, "tables")
  powerpoint_dir_path <- file.path(local_country_path, "powerpoint")
  error_dir_path <- file.path(local_country_path, "errors")
  param_dir_path <- file.path(local_country_path, "parameters")
  fun_dir_path <- file.path(local_country_path, "R")

  for (i in c(
    data_dir_path,
    metadata_dir_path,
    fig_dir_path,
    tables_dir_path,
    powerpoint_dir_path,
    error_dir_path,
    param_dir_path,
    fun_dir_path
  )) {
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
#' @keywords internal
#'
#' @returns `str` size of the data
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
#' @keywords internal
#'
#' @returns `list` large list containing polio data
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
      sirfunctions::get_all_polio_data(size = data_size, attach.spatial.data = attach_spatial_data)
    country_data <- sirfunctions::extract_country_data(country_name, raw_data)
    readr::write_rds(country_data, path_to_save)
    message(paste0("Data saved at:\n", dr_data_path))
    return(country_data)
  }

#' Loads .Rds file
#'
#' @param data_dir_path `str` data path
#' @keywords internal
#'
#' @returns `list` large list containing the polio data
load_data <- function(data_dir_path) {
  cli::cli_alert_info("Choose the file to load by entering the line number from above or type 'q' to quit: ")
  load_data_response <- T
  while (load_data_response) {
    load_data <- readline("> ")
    load_data <- stringr::str_trim(load_data)
    if (load_data == "q") {
      return(message("Aborting dataset loading..."))
    }

    file_names <- list.files(data_dir_path, pattern = "\\.Rds$", ignore.case = T)

    load_data <- suppressWarnings(as.integer(load_data))
    if (is.na(load_data) | load_data == 0) {
      message("Invalid response. Please try again.")
    } else if (load_data <= length(file_names)) {
      chosen_file <- file_names[load_data]
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
#' @keywords internal
#'
#' @returns this function does not return anything
create_metadata <- function(path) {
  date_updated <- paste("Updated:", Sys.time(), sep = " ")
  file_location <-
    paste("Project working directory:", path, sep = " ")

  if (!exists(path)) {
    file.create(path)
  }

  readr::write_lines(
    file = path,
    x = c(date_updated, file_location)
  )
}

#' Handles the logic of which files to load in the current R session
#'
#' @param dr_data_path `str` path to the dataset
#' @param data_size `str` "small", "medium", or "large"
#' @param country_name `str` name of the country
#' @param data_path path of the data folder used in the desk review
#' @param attach_spatial_data whether to attach spatial data
#' @keywords internal
#'
#' @returns `list` large list containing polio data
generate_data <-
  function(data_path,
           data_size,
           country_name,
           dr_data_path,
           attach_spatial_data) {
    file_names <- list.files(data_path, pattern = "\\.rds$", ignore.case = T)

    data_exists <- length(file_names) != 0
    if (data_exists) {
      response <- T
      while (response) {
        cli::cli_alert_info("Previous save(s) found:")

        for (i in 1:length(file_names)) {
          message(paste0(i, ") ", file_names[i]))
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

#' Fetch the Rds file to be used for a desk review
#'
#' @param country country name as a string
#' @param year year as an integer
#' @param local_dr_repo the local desk review repository
#' @keywords internal
#'
#' @returns A status message
fetch_dr_data <- function(country, year, local_dr_repo) {
  country <- stringr::str_to_lower(stringr::str_trim(country))
  files <- sirfunctions::edav_io(
    io = "list",
    default_dir = NULL,
    file_loc = file.path(
      "GID/PEB/SIR/Data/desk_review",
      country, year
    )
  ) |>
    dplyr::pull(.data$name)

  file_names <- basename(files)

  if (length(file_names) == 0) {
    message("No files related to the desk review found on EDAV.")
    return(NULL)
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
            file.path(
              "GID/PEB/SIR/Data/desk_review",
              country, year
            ),
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

# "public" methods ----
#' Set up the folders and load polio data for the desk review
#'
#' Prepares the folders and files required for the desk review. The function primarily
#' serves to organize the files used for the desk review and set standardized
#' environmental variables (i.e., [Sys.getenv()], where values for `x` related to
#' the desk review is prefixed with `"DR"`) . The function only supports running one country at a time.
#'
#' @param country_name `str` Name of the country.
#' @param start_date `str` Start date of the desk review. If `NULL`, defaults to four years
#' from when the function was ran on January 1st.
#' @param end_date `str` End date of the desk review. If `NULL`, defaults to six weeks from when
#' the function is ran.
#' @param local_dr_folder `str` Folder where the desk review code is located.
#' Defaults to the current working directory.
#' @param attach_spatial_data `bool` Whether to include spatial data. Defaults to `TRUE`.
#' @param sg_dr_folder `str` Folder where the local git repository is located. Defaults to `NULL`.
#' @param lab_data_path `str` Location of the lab data. Defaults to `NULL`.
#' @param iss_data_path `str` Location of the ISS data. Defaults to `NULL`.
#' @param branch `str` What branch to download the DR functions from GitHub.
#' `"main"` is the default, which contains the official version of the package. Other branches,
#' like `"dev"` may contain experimental features not yet available in the `"main"` branch.
#' @param source `bool` Whether to source local functions or use sirfunctions. Defaults to `TRUE`.
#'
#' @returns `list` A list containing all dataframe for all polio data.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria", source = F) # Sets up folder in the current working directory
#' ctry.data <- init_dr("algeria", branch = "dev") # Use functions from the dev branch
#' }
#'
#' @export
init_dr <-
  function(country_name,
           start_date = NULL,
           end_date = NULL,
           local_dr_folder = getwd(),
           sg_dr_folder = NULL,
           lab_data_path = NULL,
           iss_data_path = NULL,
           attach_spatial_data = T,
           branch = "main",
           source = T) {
    country_name <-
      stringr::str_trim(stringr::str_to_upper(country_name))

    if (is.null(end_date)) {
      end_date <- Sys.Date() - lubridate::weeks(6)
      end_date <<- end_date
    } else {
      end_date <- lubridate::as_date(end_date)
      end_date <<- end_date
    }

    if (is.null(start_date)) {
      start_date <- (end_date - lubridate::years(3)) |>
        lubridate::floor_date("year")
      start_date <<- start_date
    } else {
      start_date <- lubridate::as_date(start_date)
      start_date <<- start_date
    }

    year <- lubridate::year(end_date)

    # Set up local directory for storing for data and metadata
    df_name <-
      stringr::str_c(stringr::str_to_lower(country_name))

    # Relative path of where data and metadata is stored
    dr_path <-
      file.path(
        local_dr_folder,
        stringr::str_to_lower(country_name),
        year
      )

    # Set up local folders
    country_dir_path <- set_dr_local_folders(dr_path)

    # Create a local copy of the dataset in the folder, or update it.
    dr_data_path <- file.path(country_dir_path, "data", df_name)

    # Determine the file size of the data to be downloaded
    data_size <- set_data_size(year)

    # Create meta data
    metadata_path <- file.path(
      dr_path,
      "metadata",
      paste0(paste(df_name, "metadata", sep = "_"), ".txt")
    )
    create_metadata(file.path(metadata_path))

    # Instantiate variable containing country data and desk review meta data
    data_path <- file.path(country_dir_path, "data")

    country_data <- NULL
    response <- TRUE

    while (response) {
      cli::cli_alert_info("Load data from EDAV? y/n")
      edav_response <- readline("Enter a response: ")
      edav_response <- stringr::str_trim(stringr::str_to_lower(edav_response))

      if (!(edav_response %in% c("y", "n"))) {
        message("Invalid response, please try again.")
        next
      } else if (edav_response == "y") {
        tryCatch(
          {
            country_data <- fetch_dr_data(country_name, year, local_dr_folder)
          },
          error = function(error) {
            message("No relevant files available for this desk review on EDAV.")
            response <- FALSE
          }
        )
        response <- FALSE
      } else if (edav_response == "n") {
        response <- FALSE
      }
    }

    country_data <- generate_data(
      data_path,
      data_size,
      country_name,
      dr_data_path,
      attach_spatial_data
    )

    # Attaching lab data if available and creating a copy to data folder
    data_folder_files <- list.files(data_path)


    if (!is.null(lab_data_path)) {
      country_data$lab.data <- load_lab_data(lab_data_path)
      # check if there is already a copy of lab data in the data folder
      if ((stringr::str_detect(data_folder_files, "_lab_data_") |> sum()) == 0) {
        cli::cli_process_start("Saving a copy of the lab data to the data folder.")
        dr_lab_data_path <- file.path(
          data_path,
          paste0(country_data$ctry$ISO_3_CODE, "_lab_data_", Sys.Date(), ".Rds")
        )
        saveRDS(country_data$lab.data, dr_lab_data_path)
        Sys.setenv(DR_LAB_PATH = dr_lab_data_path)
      } else {
        Sys.setenv(DR_LAB_PATH = lab_data_path)
      }
    }

    # Attaching ISS data if available and creating a copy to data folder
    if (!is.null(iss_data_path)) {
      country_data$iss.data <- load_iss_data(iss_data_path)
      # check if there is already a copy of ISS data in the data folder
      if ((stringr::str_detect(data_folder_files, "_iss_data_") |> sum()) == 0) {
        cli::cli_process_start("Saving a copy of the ISS/eSurv data to the data folder.")
        dr_iss_data_path <- file.path(
          data_path,
          paste0(country_data$ctry$ISO_3_CODE, "_iss_data_", Sys.Date(), ".Rds")
        )
        saveRDS(country_data$iss.data, dr_iss_data_path)
        Sys.setenv(DR_ISS_PATH = dr_iss_data_path)
      } else {
        Sys.setenv(DR_ISS_PATH = iss_data_path)
      }
      cli::cli_process_done()
    }

    # Check if previous params should be loaded or update a new cache file
    check_cache(
      file.path(country_dir_path, "parameters", "parameters.RData"),
      start_date, end_date, country_name
    )

    end_date <<- lubridate::as_date(end_date)
    start_date <<- lubridate::as_date(start_date)
    # Setting environmental variables
    Sys.setenv(DR_PATH = file.path(country_dir_path))
    Sys.setenv(DR_DATA_PATH = file.path(country_dir_path, "data"))
    Sys.setenv(DR_ERROR_PATH = file.path(country_dir_path, "errors"))
    Sys.setenv(DR_TABLE_PATH = file.path(country_dir_path, "tables"))
    Sys.setenv(DR_POWERPOINT_PATH = file.path(country_dir_path, "powerpoint"))
    Sys.setenv(DR_FIGURE_PATH = file.path(country_dir_path, "figures"))
    Sys.setenv(DR_FUNC_PATH = file.path(country_dir_path, "R"))
    Sys.setenv(DR_COUNTRY = country_name)

    # Copy functions in R
    copy_dr_functions(branch)

    # Copy desk review template
    copy_dr_template_code()

    # Source the functions used in the Desk Review
    if (source) {
      cli::cli_process_start("Sourcing functions")
      dr_func_scripts <- list.files(Sys.getenv("DR_FUNC_PATH"))
      for (i in dr_func_scripts) {
        source(file.path(Sys.getenv("DR_FUNC_PATH"), i))
      }
      cli::cli_process_done()
    }

    return(country_data)
  }

#' Upload desk review script to the sg-desk-reviews GitHub repository
#'
#' Upload the desk review template script to the [sg-desk-reviews](https://github.com/nish-kishore/sg-desk-reviews)
#' repository, which houses the code for the desk reviews.
#' This function can be used in a general sense to upload files to
#' a github repository. Note that the function will only commit, and that the user must push themselves.
#'
#' @param file_path `str` Location of the file to upload to the sg-desk-reviews repo.
#' @param repo_path `str` Local path of the sg-desk-review repo.
#' @param message `str` Message to include in the commit.
#'
#' @returns A status message.
#' @examples
#' \dontrun{
#' dr_template_path <- "C:/Users/ABC1/Desktop/local_dr/algeria/2024/algeria_template.Rmd"
#' repo_path <- "C:/Users/ABC1/Desktop/github/sg-desk-reviews"
#' str_message <- "Added algeria to the SG folder"
#' upload_dr_to_github(dr_template_path, repo_path, str_message)
#' }
#' @export
upload_dr_to_github <-
  function(file_path, repo_path, message = "updating file") {
    if (!requireNamespace("git2r", quietly = TRUE)) {
      stop('Package "git2r" must be installed to use this function.',
        .call = FALSE
      )
    }

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
    tryCatch(
      {
        git2r::add(repo, path = file.path(repo_path, file_name))

        # Commit the changes with a message including the file name
        commit_msg <- paste0(message, ": ", file_name)
        git2r::commit(repo, message = commit_msg)
        message("Changes committed successfully. Please push using GitHub Desktop or command line ")
      },
      error = function(e) {
        message("No changes to the file.")
      }
    )

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
#' Data from the desk review can be stored in EDAV so there's an exact copy of the
#' dataset used in the desk review. This ensures that even after years,
#' the desk reviews can be ran exactly as it was.
#' @param rds_obj `Robj` Object loaded in R. This would be `ctry.data`, for example.
#' @param file_name `str` Name given to the Rds object, do not append `.rds`. This is
#' what gets stored in EDAV.
#' @param country `str` Country as a string.
#' @param year `int` It is recommended to set this to the year
#' when the desk review was ran.
#'
#' @returns A status message.
#' @examples
#' \dontrun{
#' raw.data <- get_all_polio_data()
#' ctry.data <- init_dr("algeria")
#' freeze_dr_data(ctry.data, "algeria_ctry_data")
#' }
#'
#' @export
#' @seealso [init_dr()]
freeze_dr_data <- function(rds_obj,
                           file_name,
                           country = Sys.getenv("DR_COUNTRY"),
                           year = as.numeric(format(Sys.Date(), "%Y"))) {
  country <- stringr::str_to_lower(country)

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
