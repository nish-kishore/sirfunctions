# Main ----

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

  cli::cli_process_done()
  cli::cli_alert_success(paste0("KPI analysis folder initialized at: ",
                                analysis_path))

  # Create KPI template
  if (!file.exists(file.path(analysis_path, "kpi_template.R"))) {
    cli::cli_process_start("Generating KPI code template")
    generate_kpi_template(path, name)
    cli::cli_process_done()
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
        error = function(e) {
          cli::cli_alert_warning("Invalid response.")
        }
      )
    }
  }
  cli::cli_process_done()

  # Load lab data
  cli::cli_process_start("Loading lab data")
  if (length(lab_files) == 0) {
    lab_data <<- edav_io("read", file_loc = get_constant("CLEANED_LAB_DATA"))
    saveRDS(lab_data, file.path(Sys.getenv("KPI_DATA"), paste0("lab_data_", today, ".rds")))
  } else if (length(lab_files) == 1) {
    lab_data <<- readRDS(file.path(Sys.getenv("KPI_DATA"), lab_files[1]))
  } else {
    for (i in 1:length(lab_files)) {
      print(paste0(i, ". ", lab_files[i]))
    }

    while (TRUE) {
      cli::cli_alert_info("Please choose a file to load")
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
        error = function(e) {
          cli::cli_alert_warning("Invalid response.")
        }
      )
    }
  }
  cli::cli_process_done()

  cli::cli_alert_success("KPI analysis set up complete.")
  cli::cli_text(paste0("Click here to access the template file: ",
                       "{.file ", file.path(analysis_path, "kpi_template.R"), "}"))
}

# Utility functions ----

#' Get country abbreviations
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Gets the country abbreviation from the AFP dataset.
#'
#' @param afp_data `tibble` AFP dataset
#'
#' @return `tibble` A tibble with the country, abbreviation, and WHO region
#' @export
#'
#' @examples
#' raw_data <- get_all_polio_data(attach.spatial.data = FALSE)
#' ctry_abbrev <- get_ctry_abbrev(raw_data$afp)
get_ctry_abbrev <- function(afp_data) {
  ctry_abbrev <- afp_data |>
    dplyr::mutate(
      ctry.abbrev.epid = substr(.data$epid, 1, 3),
      ctry.short = case_when(
        .data$whoregion == "AFRO" ~ .data$ctry.abbrev.epid,
        .data$whoregion != "AFRO" ~ .data$country.iso3
      )
    ) |>
    dplyr::select("ctry.short", "place.admin.0", "whoregion") |>
    # fixing bad abbreviation in Gabon and turning all to upper case to eliminate dupes
    dplyr::mutate(
      ctry.short = ifelse(.data$place.admin.0 == "GABON" & .data$ctry.short == "BUU",
        "GAB", .data$ctry.short
      ),
      ctry.short = toupper(.data$ctry.short)
    ) |>
    dplyr::distinct() |>
    dplyr::filter(
      !is.na(.data$ctry.short), !is.na(.data$place.admin.0), !str_starts(ctry.short, "-")
    )

  return(ctry_abbrev)
}

#' Create the KPI template
#'
#' @param output_path `str` Where to output the script to.
#' @param name `str` Name of the KPI folder.
#'
#' @return None.
#'
#' @examples
#' \dontrun{
#' generate_kpi_template(getwd(), "test_folder")
#' }
#' @keywords internal
generate_kpi_template <- function(output_path, name) {

  conn <- file(file.path(output_path, name, "kpi_template.R"))

  # Initialization path
  init <- paste0("init_kpi(", '"', output_path, '"', ",\n",
                 '         "', name, '"', ")")

  # Shapefiles
  ctry_sf <- 'ctry_sf <- load_clean_ctry_sp(st_year = 2022, type = "long")'
  dist_sf <- 'dist_sf <- load_clean_dist_sp(st_year = 2022, type = "long")'

  # Generate tables
  c1 <- 'c1 <- generate_c1_table(raw_data, "2022-01-01", "2024-12-31")'
  c2 <- 'c2 <- generate_c2_table(raw_data$afp, raw_data$dist.pop, "2022-01-01", "2024-12-31")'
  c3 <- 'c3 <- generate_c3_table(raw_data$es, "2022-01-01", "2024-12-31")'
  c4 <- 'c4 <- generate_c4_table(lab_data, raw_data$afp, "2022-01-01", "2024-12-31")'

  # Generate figures
  sg_priority_map <- "generate_sg_priority_map()"
  npafp_kpi_map <- 'generate_kpi_npafp_map(c2, 2024, "AFRO", ctry_sf = ctry_sf, dist_sf = dist_sf)'
  stool_kpi_map <- 'generate_kpi_stool_map(c2, 2024, "AFRO", ctry_sf = ctry_sf, dist_sf = dist_sf)'
  ev_kpi_map <- 'generate_kpi_ev_map(c3, 2024, "AFRO", ctry_sf = ctry_sf)'

  npafp_bar <- "generate_kpi_npafp_bar(c1, raw_data$afp)"
  stool_bar <- "generate_kpi_stoolad_bar(c1, raw_data$afp)"
  ev_bar <- "generate_kpi_evdetect_bar(c1, raw_data$afp)"

  timely_violin <- 'generate_timely_det_violin(raw_data, "2022-01-01", "2024-12-31")'
  culture_violin <- 'generate_lab_culture_violin(lab_data, raw_data$afp, "2022-01-01", "2024-12-31")'
  itd_violin <- 'generate_lab_itd_violin(lab_data, raw_data$afp, "2022-01-01", "2024-12-31")'
  seqship_violin <- 'generate_lab_seqship_violin(lab_data, raw_data$afp, "2022-01-01", "2024-12-31")'
  seqres_violin <- 'generate_lab_seqres_violin(lab_data, raw_data$afp, "2022-01-01", "2024-12-31")'

  export_table <- "export_kpi_table(c1, c2, c3, c4, drop_label_cols = FALSE)"

  # Write template
  output_string <- c(
    "# KPI Code Template",
    paste0("# Downloaded on: ", Sys.Date()), "\n\n\n",
    init, "\n",
    "# Obtaining shapefiles in long format ----",
    "# Ensure st.year is the year of your start date.",
    ctry_sf, dist_sf, "\n",
    "# Cleaning lab data ----",
    'lab_data <- clean_lab_data(lab_data, "2022-01-01", "2024-12-31", raw_data$afp)\n',
    "# Generate GPSAP C1-C4 tables ----",
    "# Except for c1, every other table can be specified a custom grouping",
    "# You may also specify and filter countries based on risk category ",
    c1, c2, c3, c4, "\n",
    "# Generate figures ----",
    "# Ensure c2 is grouped at the district level.",
    sg_priority_map, npafp_kpi_map, stool_kpi_map, ev_kpi_map, "\n",
    npafp_bar, stool_bar, ev_bar, "\n",
    timely_violin, culture_violin, itd_violin, seqship_violin,
    seqres_violin, "\n",
    "# Export GPSAP tables ----",
    export_table, "\n"
  )

  writeLines(output_string, conn)
}
