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

#' Initialize the KPI analysis pipeline
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Sets up folder structures and environmental variables, as well as download
#' global polio data.
#'
#' @param path `str` Path to the folder containing the KPI analysis folders.
#' Defaults to current working directory.
#' @param name `str` Name of the KPI analysis folder. If not given any names, the
#' folder will be named the date the function is ran.
#'
#' @return Does not return anything
#' @export
#'
#' @examples
#' \dontrun{
#' init_kpi(name = "kpi_jan_2024")
#' }
init_kpi <- function(path = getwd(), name = NULL) {

  # Date created
  today <- Sys.Date()

  # Check if required directories exist
  if (!dir.exists(path)) {
    cli::cli_abort("Invalid folder path, please try again.")
  }



  # Main folder
  cli::cli_process_start("Setting up folder structure")
  if (is.null(name)) {
    analysis_path <- file.path(path, Sys.Date())
  } else {
    analysis_path <- file.path(path, name)
  }
  cli::cli_process_done()

  # Path to data
  if (!dir.exists(file.path(analysis_path, "data"))) {
    dir.create(file.path(analysis_path, "data"), recursive = T)
  }

  # Path to export
  if (!dir.exists(file.path(analysis_path, "tables"))) {
    dir.create(file.path(analysis_path, "tables"), recursive = T)
  }

  # Path to figures
  if (!dir.exists(file.path(analysis_path, "figures"))) {
    dir.create(file.path(analysis_path, "figures"), recursive = T)
  }

  # Set environment variables
  Sys.setenv(KPI_DATA = file.path(analysis_path, "data"))
  Sys.setenv(KPI_TABLES = file.path(analysis_path, "tables"))
  Sys.setenv(KPI_FIGURES = file.path(analysis_path, "figures"))

  # List files
  data_files <- list.files(Sys.getenv("KPI_DATA"))
  global_files <- data_files[stringr::str_detect(data_files, "raw_data")]
  lab_files <- data_files[stringr::str_detect(data_files, "lab_data")]

  # Load global polio data
  cli::cli_process_start("Loading global polio dataset")
  if (length(global_files) == 0) {
    raw_data <<- get_all_polio_data()
    saveRDS(raw_data, file.path(Sys.getenv("KPI_DATA"), paste0("raw_data_", today, ".rds")))
  } else if (length(global_files) == 1) {
    raw_data <<- readRDS(file.path(Sys.getenv("KPI_DATA"), global_files[1]))
  } else {
    for (i in 1:length(global_files)) {
      print(paste0(i, ". ", global_files[i]))
    }

    while (TRUE) {
      cli::cli_alert_info("Please choose a file to load")
      response <- readline("> ")
      tryCatch(
        expr = {
          response <- as.numeric(stringr::str_trim(response))
          if (response == 0 | response > length(global_files)) {
            "Invalid response."
            next
          } else {
            raw_data <<- readRDS(file.path(Sys.getenv("KPI_DATA"), global_files[response]))
            break
          }
        },
        error = function(e) {cli::cli_alert_warning("Invalid response.")}
      )
    }
  }
  cli::cli_process_done()

  # Load lab data
  cli::cli_process_start("Loading lab data")
  if (length(lab_files) == 0) {
    lab_data <<- edav_io("read", file_loc = get_constant("CLEANED_LAB_DATA"))
    saveRDS(raw_data, file.path(Sys.getenv("KPI_DATA"), paste0("lab_data_", today, ".rds")))
  } else if (length(lab_files) == 1) {
    lab_data <<- readRDS(file.path(Sys.getenv("KPI_DATA"), lab_files[1]))
  } else {

    for (i in 1:length(lab_files)) {
      print(paste0(i, ". ", lab_files[i]))
    }

    while (TRUE) {
      cli::cli_alert_info("Please  a file to load")
      response <- readline("> ")
      tryCatch(
        expr = {
          response <- as.numeric(stringr::str_trim(response))
          if (response == 0 | response > length(lab_files)) {
            "Invalid response."
            next
          } else {
            raw_data <<- readRDS(file.path(Sys.getenv("KPI_DATA"), lab_files[response]))
            break
          }
        },
        error = function(e) {cli::cli_alert_warning("Invalid response.")}
      )
    }
  }
  cli::cli_process_done()

  cli::cli_alert_success("KPI analysis set up complete.")
}
