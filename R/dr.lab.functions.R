#' Table of information regarding testing labs in each country
#'
#' Imports information on testing labs for each country, either from a CSV file
#' or downloaded from EDAV. If no argument is passed, the function will download
#' the table from EDAV.
#'
#' @import cli dplyr
#' @param path `str` Path to the lab location file. Defaults to `NULL`.
#'
#' @return `tibble` A table containing the test lab location information.
#' @examples
#' ctry.seq <- get_lab_locs()
#'
#' @export

get_lab_locs <- function(path = NULL) {
  lab.locs <- NULL
  if (is.null(path)) {
    tryCatch(
      {
        cli::cli_process_start("Downloading lab testing location file from EDAV.")
        lab.locs <- sirfunctions::edav_io("read", file_loc = "Data/lab/Routine_lab_testing_locations.csv")
        cli::cli_process_done()
      },
      error = function(e) {
        stop(paste0(
          "Download of lab testing location file from EDAV failed.",
          "Please specify the path to the Routine lab testing location file ",
          "and try again."
        ))
      }
    )
  } else {
    lab.locs <- readr::read_csv(path)
    lab.locs <- lab.locs |>
      dplyr::mutate(country = stringr::str_to_upper(.data$country))
  }

  return(lab.locs)
}

#' Determines whether lab data is EMRO or AFRO
#'
#' Outputs the name of the region which a country belongs to.
#'
#' @import stringr
#' @param country_name `str` Name of the country.
#'
#' @return `str` A string, either `"EMRO"` or `"AFRO"`.
#' @examples
#' get_region("algeria")
#'
#' @export
get_region <- function(country_name = Sys.getenv("DR_COUNTRY")) {

  # Format country_name
  country_name <- stringr::str_trim(stringr::str_to_upper(country_name))

  # Countries that belong in a region
  emro_ctry <- c(
    "EGYPT", "AFGHANISTAN", "PAKISTAN", "IRAN (ISLAMIC REPUBLIC OF)",
    "KUWAIT", "SYRIAN ARAB REPUBLIC", "MOROCCO", "IRAQ", "YEMEN",
    "SOMALIA", "BAHRAIN", "LEBANON",
    "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM",
    "QATAR", "SUDAN", "SAUDI ARABIA", "UNITED ARAB EMIRATES",
    "DJIBOUTI"
  )
  afro_ctry <- c(
    "CHAD", "ANGOLA", "BENIN", "NIGERIA", "ALGERIA", "GUINEA", "CAMEROON",
    "KENYA", "BURKINA FASO", "MOZAMBIQUE", "ETHIOPIA",
    "SOUTH AFRICA", "SENEGAL", "MADAGASCAR", "CENTRAL AFRICAN REPUBLIC",
    "BURUNDI", "CONGO", "UNITED REPUBLIC OF TANZANIA", "CABO VERDE", "NIGER",
    "MALAWI", "SOUTH SUDAN", "LIBERIA", "TOGO", "UGANDA", "BOTSWANA", "ZAMBIA",
    "MAURITANIA", "GABON", "ERITREA", "GUINEA-BISSAU", "LESOTHO", "NAMIBIA",
    "SIERRA LEONE", "ZIMBABWE", "EQUATORIAL GUINEA", "MAURITIUS", "RWANDA",
    "ESWATINI", "COTE D'IVOIRE", "COTE D IVOIRE"
  )

  region <- dplyr::case_match(
    country_name,
    emro_ctry ~ "EMRO",
    afro_ctry ~ "AFRO",
    .default = NA
  )

  return(region)
}


#' Function to load the raw lab data
#'
#' This a function to load lab data that are either CSVs or Excel files.
#'
#' @import stringr readr
#' @param lab_data_path `str` File path as a string to the lab data.
#' @param sheet_name `str` Name of the sheet to load. This is optional in cases
#' of an Excel sheet with multiple tabs.
#'
#' @return `tibble` Lab data loaded from the CSV or Excel file path.
#' @examples
#' \dontrun{
#' lab_data_path <- "C:/Users/ABC1/Desktop/lab_data.csv"
#' lab.data <- load_lab_data(lab_data_path)
#' }
#'
#' @export
load_lab_data <- function(lab_data_path, sheet_name = NULL) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop('Package "readxl" must be installed to use this function.',
      .call = FALSE
    )
  }

  if (stringr::str_ends(lab_data_path, ".csv")) {
    return(readr::read_csv(lab_data_path))
  } else if (stringr::str_ends(lab_data_path, ".xlsx")) {
    return(readxl::read_excel(lab_data_path, sheet = sheet_name))
  } else {
    stop("Not a csv or .xlsx file. Try again.")
  }
}


#' Generate a log of potential errors in the lab data
#'
#' Checks the loaded lab data for potential issues. The function will detect whether
#' the lab data loaded either came from the regional office or from global.
#'
#' @param lab.data `tibble` Polio lab data.
#' @param afp.data `tibble` AFP linelist.
#' @param start.date `str` Start date of the analysis.
#' @param end.date `str` End date of the analysis.
#' @param error_path `str` File path to store the error log.
#' @param ctry.data `list` `r lifecycle::badge("deprecated")`
#' @param ctry_name `list` or `str` A name of a country or a list of countries.
#' Please pass lab data directly into lab.data parameter instead.
#' @returns None. It outputs locally an Excel file containing the error log.
#' @examples
#' \dontrun{
#' lab_path <- "C:/Users/ABC1/Desktop/lab_data.xlsx"
#' start_date <- "2021-01-01"
#' end_date <- "2023-12-31"
#' ctry.data <- init_dr("algeria", lab_data_path = lab_path)
#' lab_data_errors(ctry.data)
#' }
#' @export
lab_data_errors <- function(lab.data, afp.data,
                            start.date = start_date, end.date = end_date,
                            ctry_name = Sys.getenv("DR_COUNTRY"),
                            error_path = Sys.getenv("DR_ERROR_PATH"),
                            ctry.data = lifecycle::deprecated()) {

  if (lifecycle::is_present(ctry.data)) {
    lifecycle::deprecate_warn(
      when = "1.3.0",
      what = "lab_data_errors(ctry.data)",
      details = paste0("Using ctry.data will be deprecated in the next release",
                       "Please pass lab data directly to lab.data.")
    )

    # Check if the lab data is attached
    if (is.null(ctry.data$lab.data)) {
      cli::cli_abort("Lab data not attached to ctry.data. Please attach and try again.")
    }

    lab.data <- ctry.data$lab.data
  }


  start.date <- lubridate::as_date(start.date)
  end.date <- lubridate::as_date(end.date)

  # Determine the type of cleaning to do
  lab.data.cols <- names(lab.data)

  if ("MasterKey" %in% lab.data.cols) {
    lab_data_errors_who(lab.data, start.date, end.date)
  } else {
    lab_data_errors_region(lab.data, ctry_name, start.date, end.date)
  }
}

#' Check common errors in the regional lab data.
#'
#' Error checking for regional lab data. This is a helper function meant to be used inside
#' [lab_data_errors()].
#' @import dplyr stringr cli writexl
#' @param lab.data `tibble` Regional lab data.
#' @param ctry_name `str` Name of the country. Defaults to the desk review country.
#' @param start.date `str` Start date of analysis.
#' @param end.date `str` End date of analysis.
#' @param error_path `str` Path to folder to save the error log.
#' @param ctry.data `list` `r lifecycle::badge("deprecated")` Passing lab data
#' attached to country data is deprecated. Please pass lab data directly to the
#' lab.data parameter.
#' @returns None. It outputs an Excel file locally containing the error log.
#' @examples
#' \dontrun{
#' lab_path <- "C:/Users/XRG9/lab_data_region.csv"
#' ctry.data <- init_dr("algeria", lab_data_path = lab_path)
#' lab_data_errors_region(ctry.data$lab.data, "2021-01-01", "2023-12-31")
#' }
#' @keywords internal
lab_data_errors_region <- function(lab.data, ctry_name = Sys.getenv("DR_COUNTRY"),
                                   start.date, end.date,
                                   error_path = Sys.getenv("DR_ERROR_PATH"),
                                   ctry.data = lifecycle::deprecated()) {

  if (lifecycle::is_present(ctry.data)) {
    lifecycle::deprecate_warn(
      when = "1.3.0",
      what = "lab_data_errors_region(ctry.data)",
      details = "Please pass the dataframe directly to lab.data."
    )

    lab.data <- ctry.data$lab.data
  }

  start.date <- lubridate::as_date(start.date)
  end.date <- lubridate::as_date(end.date)
  ctry_name <- stringr::str_to_upper(ctry_name)

  # Converting character dates to date columns
  lab.data <- lab.data |>
    dplyr::rename("country" = "Name") |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c(
        "CaseDate",
        "ParalysisOnsetDate",
        "DateStoolCollected",
        "StoolDateSentToLab",
        "DateStoolReceivedinLab",
        "DateFinalCellCultureResult",
        "DateFinalrRTPCRResults",
        "ReportDateSequenceResultSent",
        "DateIsolateRcvdForSeq",
        "DateLArmIsolate",
        "DateRArmIsolate",
        "DateofSequencing",
        "DateNotificationtoHQ"
      )), \(x) as.Date.character(x, "%m/%d/%Y"))
    )

  # Cleaning for Cote D'Ivoire
  if (stringr::str_detect(ctry_name, "(?i)IVOIRE")
  ) {
    lab.data <- lab.data |>
      dplyr::mutate(country = dplyr::if_else(stringr::str_detect(.data$country,
                                                                 "(?i)IVOIRE"),
                                          "COTE D'IVOIRE", .data$country
      ))
  }

  # Filtering to the country of interest
  cli::cli_process_start("Filtering to country of interest")
  lab.data <- lab.data |>
    dplyr::filter(country %in% ctry_name)
  cli::cli_process_done()

  # Check for duplicates
  cli::cli_process_start("Checking for duplicate data")

  duplicate.02 <- lab.data |>
    dplyr::filter(dplyr::between(ParalysisOnsetDate, start.date, end.date)) |>
    dplyr::distinct()

  # Additional cleaning steps
  duplicate.03 <- duplicate.02 |>
    # Dropping rows with Specimen number 0 or >2
    dplyr::filter(SpecimenNumber %in% c(1, 2)) |>
    # replacing "NULL" with NA
    # dplyr::mutate_at(vars(DateStoolCollected:VDPV3), ~na_if(., "NULL")) %>%
    dplyr::mutate(
      country = stringr::str_to_upper(.data$country),
      year = lubridate::year(.data$ParalysisOnsetDate),
      whoregion = get_region(.data$country)
    )

  # Join lab locations
  duplicate.04 <- duplicate.03 |>
    # count duplicates with same EPID and specimen number
    # there should be 2 records for each EPID, specimen 1 and 2
    dplyr::group_by(.data$EPID, .data$SpecimenNumber) |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::ungroup()

  duplicated <- duplicate.04[duplicated(duplicate.04[c("EPID", "SpecimenNumber")]), ]

  if (nrow(duplicated) > 0) {
    cli::cli_alert_warning(paste0("There are ", nrow(duplicated), " duplicate lab entries."))
  } else {
    cli::cli_alert_success("No duplicate entries found.")
  }

  cli::cli_process_done()

  # Check for invalid dates
  cli::cli_process_start("Checking for negative time and N/A intervals")
  lab.data <- lab.data |>
    dplyr::mutate(
      # Intervals
      days.collect.lab = .data$DateStoolReceivedinLab - .data$DateStoolCollected,
      days.lab.culture = .data$DateFinalCellCultureResult - .data$DateStoolReceivedinLab,
      days.seq.ship = .data$DateIsolateRcvdForSeq - .data$ReportDateSequenceResultSent,
      days.lab.seq = .data$DateofSequencing - .data$DateStoolReceivedinLab,
      days.itd.seqres = .data$DateofSequencing - .data$DateFinalrRTPCRResults,
      days.itd.arriveseq = .data$DateIsolateRcvdForSeq - .data$DateFinalrRTPCRResults,
      days.seq.rec.res = .data$DateofSequencing - .data$DateIsolateRcvdForSeq,

      # Met target yes/no
      met.targ.collect.lab = ifelse(.data$days.collect.lab < 3, 1, 0),
      negative.spec = ifelse(!str_detect(.data$FinalCellCultureResult, "ITD") & .data$FinalITDResult == "NULL", 1, 0),
      met.lab.culture = ifelse(.data$days.lab.culture < 14, 1, 0),
    )

  invalid_intervals <- lab.data |>
    # filtering out negative time intervals
    dplyr::filter((days.collect.lab < 0) |
      (days.lab.culture < 0) |
      (days.seq.ship < 0) |
      (days.lab.seq < 0) |
      (days.itd.seqres < 0) |
      (days.itd.arriveseq < 0) |
      (days.seq.rec.res < 0))

  if (nrow(invalid_intervals) > 0) {
    cli::cli_alert_warning(paste0("There are ", nrow(invalid_intervals),
                                  " records with negative intervals."))
  } else {
    cli::cli_alert_success("No invalid intervals found.")
  }

  cli::cli_process_done()

  cli::cli_process_start("Checking records where stool collection date is before paralysis.")
  collection_before_paralysis <- lab.data |>
    dplyr::filter((DateStoolCollected < ParalysisOnsetDate))

  if (nrow(collection_before_paralysis) > 0) {
    cli::cli_alert_warning(paste0(
      "There are ", nrow(collection_before_paralysis),
      " records where stool collection is before paralysis."
    ))
  } else {
    cli::cli_alert_success("No records where stool collection is before paralysis.")
  }

  cli::cli_process_done()


  # Check for missing EPIDs in the AFP linelist
  cli::cli_process_start("Checking for missing EPIDs in the AFP dataset.")
  missing_epids <- lab.data |> dplyr::filter(!(EPID %in% ctry.data$afp.all.2$epid))

  if (nrow(missing_epids) != 0) {
    cli::cli_alert_warning(paste0("There are ", nrow(missing_epids), " lab cases not in the AFP linelist."))
  } else {
    cli::cli_alert_success("No lab cases missing in the AFP linelist.")
  }
  cli::cli_process_done()

  error_log <- list()
  error_log$duplicates <- duplicated
  error_log$invalid_intervals <- invalid_intervals
  error_log$collection_before_paralysis <- collection_before_paralysis
  error_log$missing_epids <- missing_epids

  writexl::write_xlsx(error_log, path = file.path(error_path, "lab.errors.xlsx"))
  cli::cli_alert("Run clean_lab_data() to attempt data fixes and perform the check again. Log saved in the errors folder.")
}

#' Checks for common data errors in WHO lab data
#'
#' This function is used internally within [lab_data_errors()]. This checks for potential errors in the
#' WHO lab data.
#'
#' @import dplyr cli writexl
#' @param lab.data `tibble` Lab data from WHO global.
#' @param start.date `str` Start date of analysis.
#' @param end.date `str` End date of analysis.
#' @param error_path `str` Path to folder to save the error log to.
#' @param ctry.data `list` `r lifecycle::badge("deprecated")`
#' Please pass lab.data directly to the lab.data parameter.
#' @examples
#' \dontrun{
#' lab_path <- "C:/Users/XRG9/lab_data_who.csv"
#' ctry.data <- init_dr("algeria", lab_data_path = lab_path)
#' lab_data_errors_who(ctry.data, "2021-01-01", "2023-12-31")
#' }
#' @keywords internal
lab_data_errors_who <- function(lab.data, ctry_name = Sys.getenv("DR_COUNTRY"),
                                start.date, end.date,
                                error_path = Sys.getenv("DR_ERROR_PATH"),
                                ctry.data = lifecycle::badge("deprecated")) {
  if (lifecycle::is_present(ctry.data)) {
    lifecycle::deprecate_warn(
      when = "1.3.0",
      what = "lab_data_errors_who(ctry.data)",
      details = "Please pass the dataframe directly to lab.data."
    )

    lab.data <- ctry.data$lab.data
  }

  start.date <- lubridate::as_date(start.date)
  end.date <- lubridate::as_date(end.date)
  ctry_name <- stringr::str_to_upper(ctry_name)

  cli::cli_process_start("Filtering to country of interest")
  lab.data <- lab.data |>
    dplyr::filter(stringr::str_to_upper(.data$Name) %in% ctry_name)
  cli::cli_process_done()

  cli::cli_process_start("Checking for invalid dates from cases.")
  invalid_dates <- lab.data |>
    dplyr::filter((days.collect.lab < 0) &
      (days.lab.culture < 0) &
      (days.seq.ship < 0) &
      (days.lab.seq < 0) &
      (days.itd.seqres < 0) &
      (days.itd.arriveseq < 0) &
      (days.seq.rec.res < 0)) |>
    dplyr::filter(
      year >= lubridate::year(start.date) & year <= lubridate::year(end.date),
      CaseOrContact == "1-Case"
    )

  if (nrow(invalid_dates) != 0) {
    cli::cli_alert_warning(paste0("There are ", nrow(invalid_dates),
                                  " cases with invalid dates."))
  } else {
    cli::cli_alert_success("No invalid dates detected.")
  }

  cli::cli_process_done()

  cli::cli_process_start("Checking for missing years")
  missing_years <- lab.data |>
    dplyr::filter(is.na(year)) |>
    dplyr::filter(
      dplyr::between(year, lubridate::year(start.date), lubridate::year(end.date)),
      CaseOrContact == "1-Case"
    )

  if (nrow(missing_years) != 0) {
    cli::cli_alert_warning(paste0("There are ", nrow(missing_years),
                                  " cases with missing years."))
  } else {
    cli::cli_alert_success("No cases with missing years.")
  }
  cli::cli_process_done()

  cli::cli_process_start("Checking for missing EPIDs in the AFP dataset.")
  missing_epids <- lab.data |> dplyr::filter(!(EpidNumber %in% ctry.data$afp.all.2$epid))

  if (nrow(missing_epids) != 0) {
    cli::cli_alert_warning(paste0("There are ", nrow(missing_epids), " lab cases not in the AFP linelist."))
  } else {
    cli::cli_alert_success("No lab cases missing in the AFP linelist.")
  }
  cli::cli_process_done()

  error_log <- list()
  error_log$invalid_dates <- invalid_dates
  error_log$missing_years <- missing_years
  error_log$missing_epids <- missing_epids

  writexl::write_xlsx(error_log, path = file.path(error_path, "lab.errors.xlsx"))
  cli::cli_alert("Run clean_lab_data() to attempt data fixes and perform the check again. Log saved in the errors folder.")
}

#' Clean polio lab data from WHO
#'
#' Cleans the lab data from WHO. This is used in [clean_lab_data()], but can be used on its own.
#'
#' @import dplyr cli stringr tidyr
#' @param ctry.data `list` Large list of country polio data. Output of either
#' [extract_country_data()] or [init_dr()]. Ensure that lab data is attached to `ctry.data`.
#' @param start.date `str` Start date of analysis.
#' @param end.date `str` End date of analysis.
#' @param delim `str` Delimiter used in the EPIDs. Defaults to `"-"`.
#'
#' @returns `tibble` Cleaned lab data.
#' @examples
#' \dontrun{
#' lab_path <- "C:/Users/XRG9/lab_data_who.csv"
#' ctry.data <- init_dr("algeria", lab_data_path = lab_path)
#' ctry.data$lab.data <- clean_lab_data_who(ctry.data, "2021-01-01", "2023-12-31")
#'
#' # Not using the desk review pipeline
#' raw.data <- get_all_polio_data()
#' ctry.data <- extract_country_data("algeria", raw.data)
#' ctry.data$lab.data <- read_csv(lab_path)
#' ctry.data$lab.data <- clean_lab_data_who(ctry.data, "2021-01-01", "2023-12-31")
#' }
#'
#' @export
clean_lab_data_who <- function(ctry.data, start.date, end.date, delim = "-") {
  start.date <- lubridate::as_date(start.date)
  end.date <- lubridate::as_date(end.date)

  if (is.null(ctry.data$lab.data)) {
    message("Lab data not attached to country data.")
    return(NULL)
  }

  if ("prov" %in% names(ctry.data$lab.data)) {
    cli::cli_alert_warning("Lab data already cleaned.")
    return(ctry.data$lab.data)
  }

  if (nrow(ctry.data$lab.data) == 0) {
    message("There are no entries for lab data.")
    return(NULL)
  }

  cli::cli_process_start("Filtering country-specific lab data")

  lab.data <- ctry.data$lab.data |>
    dplyr::filter(ctry.code2 == ctry.data$ctry$WHO_CODE) |>
    dplyr::mutate(ctry = ctry.data$ctry$ADM0_NAME)

  if (nrow(lab.data) == 0) {
    message("Filtering resulted in zero records. Please check that the ctry.code2 in lab.data matches ctry.data$ctry$ISO_3_CODE")
    return(ctry.data$lab.data)
  }

  cli::cli_process_done()

  cli::cli_process_start("Filtering for cases with valid dates")
  lab.data2 <- lab.data %>%
    dplyr::filter((days.collect.lab >= 0 | is.na(days.collect.lab)) &
      (days.lab.culture >= 0 | is.na(days.lab.culture)) &
      (days.seq.ship >= 0 | is.na(days.seq.ship)) &
      (days.lab.seq >= 0 | is.na(days.lab.seq)) &
      (days.itd.seqres >= 0 | is.na(days.itd.seqres)) &
      (days.itd.arriveseq >= 0 | is.na(days.itd.arriveseq)) &
      (days.seq.rec.res >= 0 | is.na(days.seq.rec.res))) |>
    dplyr::filter(
      year >= lubridate::year(start.date) & year <= lubridate::year(end.date),
      CaseOrContact == "1-Case"
    )
  cli::cli_process_done()

  # remove time portion of any date time columns
  cli::cli_process_start("Converting date/date-time character columns to date columns")
  lab.data2 <- lab.data2 |>
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("Date"),
      \(x) as.Date.character(x, tryFormats = c("%Y-%m-%d", "%Y/%m%/%d", "%m/%d/%Y"))
    ))
  cli::cli_process_done()

  # Don't run additional cleaning steps if no data is present
  if (nrow(lab.data2) == 0) {
    cli::cli_alert_danger("There are zero lab data after filtering. Aborting cleaning steps and returning the original lab.data.")
    return(ctry.data$lab.data)
  }

  cli::cli_process_start("Imputing missing years")
  miss <- dplyr::filter(lab.data2, is.na(year))

  miss <- miss %>%
    dplyr::mutate(year2 = substr(.data$EpidNumber, 13, 14)) %>%
    dplyr::mutate(year2 = as.numeric(paste0("20", .data$year2)))

  lab.data2 <- lab.data2 %>%
    dplyr::mutate(year = dplyr::case_when(
      is.na(year) ~ miss$year2[match(lab.data2$EpidNumber, miss$EpidNumber)],
      T ~ year
    ))
  cli::cli_process_done()

  cli::cli_process_start("Correcting district and province names.")
  lab.data2$Province <- stringr::str_to_upper(lab.data2$Province)
  lab.data2$District <- stringr::str_to_upper(lab.data2$District)
  lab.data2$Province <- iconv(lab.data2$Province, to = "ASCII//TRANSLIT")
  lab.data2$District <- iconv(lab.data2$District, to = "ASCII//TRANSLIT")

  # Match province and district by EPID number -
  lab.data2$prov <- NA
  lab.data2$dist <- NA
  lab.data2$adm1guid <- NA
  lab.data2$adm2guid <- NA

  # !!! changed place.admin.1 = prov and place.admin.2 = dist from original code
  # reason is that those were renamed at the top of the script
  lab.data2$prov <- ctry.data$afp.all.2$prov[match(lab.data2$EpidNumber, ctry.data$afp.all.2$epid)]
  lab.data2$dist <- ctry.data$afp.all.2$dist[match(lab.data2$EpidNumber, ctry.data$afp.all.2$epid)]

  lab.data2$adm1guid <- ctry.data$afp.all.2$adm1guid[match(lab.data2$EpidNumber, ctry.data$afp.all.2$epid)]
  lab.data2$adm2guid <- ctry.data$afp.all.2$adm2guid[match(lab.data2$EpidNumber, ctry.data$afp.all.2$epid)]

  potential_errors <- lab.data2
  potential_errors <- lab.data2 %>%
    dplyr::filter(is.na(prov))
  dim(potential_errors)
  potential_errors$EpidNumber
  potential_errors$Province

  # By province name
  potential_errors$adm1guid <- ctry.data$afp.all.2$adm1guid[match(potential_errors$Province, ctry.data$afp.all.2$prov)]
  potential_errors$prov <- ctry.data$afp.all.2$prov[match(potential_errors$Province, ctry.data$afp.all.2$prov)]

  potential_errors2 <- potential_errors %>%
    dplyr::filter(is.na(prov))

  potential_errors2$Province
  potential_errors2$EpidNumber

  # Totally random cases in potential_errors2
  lab.data2 <- lab.data2 %>%
    dplyr::mutate(prov = ifelse(is.na(.data$prov), potential_errors$prov[match(
      lab.data2$EpidNumber,
      potential_errors$EpidNumber
    )], .data$prov)) %>%
    dplyr::mutate(adm1guid = ifelse(is.na(.data$adm1guid), potential_errors$adm1guid[match(
      lab.data2$EpidNumber,
      potential_errors$EpidNumber
    )], .data$adm1guid))

  #---- Additional data cleaning steps
  geo_lookup_table <- ctry.data$afp.all.2 |>
    dplyr::select("epid", dplyr::matches("guid"), dplyr::contains("$adm"), "ctry", "prov", "dist", "year") |>
    tidyr::separate_wider_delim(
      cols = "epid", delim = delim,
      names = c(
        "epid_ctry", "epid_prov", "epid_dist",
        "epid_04", "epid_05"
      ),
      too_many = "merge",
      too_few = "align_start"
    ) |>
    dplyr::select(dplyr::contains("epid"), "ctry", "prov", "dist", dplyr::matches("adm[0-3]guid"), "year") |>
    dplyr::distinct()

  prov_lookup_table <- geo_lookup_table |>
    dplyr::select("epid_prov", "prov", "adm0guid", "adm1guid", "year") |>
    dplyr::distinct() |>
    tidyr::drop_na("prov")

  # Check look up table for potential duplicated rows
  prov_lookup_row_dups <- prov_lookup_table |>
    dplyr::mutate(epid_comb = str_c(epid_prov, year, sep = "-")) |>
    dplyr::group_by(.data$epid_comb, .data$epid_prov, .data$year) |>
    dplyr::summarise(n = n()) |>
    dplyr::filter(n > 1) |>
    ungroup()

  # Remove duplicates from the look up table
  prov_lookup_row_dups <- prov_lookup_row_dups |>
    dplyr::select(!dplyr::any_of(c("epid_comb", "n")))
  prov_lookup_table <- anti_join(prov_lookup_table, prov_lookup_row_dups)


  dist_lookup_table <- geo_lookup_table |>
    dplyr::select("epid_dist", "dist", "adm2guid", "year") |>
    dplyr::distinct() |>
    tidyr::drop_na("dist")

  # Check look up table for potential duplicated rows
  dist_lookup_row_dups <- dist_lookup_table |>
    dplyr::mutate(epid_comb = str_c(epid_dist, year, sep = "-")) |>
    dplyr::group_by(.data$epid_comb, .data$epid_dist, .data$year) |>
    dplyr::summarise(n = n()) |>
    dplyr::filter(n > 1) |>
    ungroup()

  # Remove duplicates from the look up table
  dist_lookup_row_dups <- dist_lookup_row_dups |>
    dplyr::select(!dplyr::any_of(c("epid_comb", "n")))
  dist_lookup_table <- anti_join(dist_lookup_table, dist_lookup_row_dups)

  # geomatching algorithm
  lab.data2 <- lab.data2 |>
    tidyr::separate_wider_delim(
      cols = "EpidNumber", delim = delim,
      names = c(
        "epid_ctry", "epid_prov", "epid_dist",
        "epid_04", "epid_05"
      ),
      names_repair = "unique",
      too_many = "merge",
      too_few = "align_start", cols_remove = F
    )
  test <- lab.data2

  test <- test |>
    dplyr::left_join(prov_lookup_table) |>
    dplyr::left_join(prov_lookup_table, by = dplyr::join_by(epid_prov, year)) |>
    dplyr::mutate(
      prov = dplyr::coalesce(.data$prov.x, .data$prov.y),
      adm1guid = dplyr::coalesce(.data$adm1guid.x, .data$adm1guid.y)
    ) |>
    dplyr::left_join(dist_lookup_table) |>
    dplyr::left_join(dist_lookup_table, by = dplyr::join_by(epid_dist, year)) |>
    dplyr::mutate(
      dist = dplyr::coalesce(.data$dist.x, .data$dist.y),
      adm2guid = dplyr::coalesce(.data$adm2guid.x, .data$adm2guid.y)
    ) |>
    dplyr::rename(
      adm0guid = "adm0guid.x"
    ) |>
    dplyr::select(-dplyr::ends_with(".y"), -dplyr::ends_with(".x"))

  # check for correctness
  check <- test |>
    dplyr::select(dplyr::starts_with("epid_"), dplyr::matches("adm[1-2]"), "prov", "dist", "EpidNumber", "year")
  mismatch_dist <- dplyr::anti_join(check, dist_lookup_table)
  # 14 mismatches in prov
  mismatch_prov <- dplyr::anti_join(check, prov_lookup_table)

  lab.data2 <- test

  # Message for values without any province or district information
  cli::cli_alert_warning(paste0("Remaining records with missing province: ", sum(is.na(lab.data$Province))))
  cli::cli_alert_warning(paste0("Remaining records with missing district: ", sum(is.na(lab.data$District))))

  cli::cli_process_done()

  # adding additional subintervals

  lab.data2 <- lab.data2 |>
    dplyr::mutate(
      days.coll.sent.field = as.numeric(.data$DateStoolSentfromField - .data$DateStoolCollected),
      days.sent.field.rec.nat = as.numeric(.data$DateStoolReceivedNatLevel - .data$DateStoolSentfromField),
      days.rec.nat.sent.lab = as.numeric(.data$DateStoolSentToLab - .data$DateStoolReceivedNatLevel),
      days.sent.lab.rec.lab = as.numeric(.data$DateStoolReceivedinLab - .data$DateStoolSentToLab),
      days.rec.lab.culture = as.numeric(.data$DateFinalCellCultureResults - .data$DateStoolReceivedinLab),
    )

  return(lab.data2)
}


#' Clean lab data from the regional offices.
#'
#' Regional data have different columns compared to the WHO lab data. The cleaning
#' of the regional data is adapted from the lab data cleaning code from the GPSAP
#' indicator script.
#'
#' @import cli dplyr stringr
#'
#' @param ctry.data `list` Large list of country polio data with lab data attached. Either from
#' [extract_country_data()] or [init_dr()].
#' @param start.date `str` Start date of analysis.
#' @param end.date `str` End date of analysis.
#' @param delim `str` Delimiter used for EPIDs. Default is `"-"`.
#' @param ctry_name `str` Name of the country. Defaults to the desk review country.
#' @param lab_locs_path `str` Path to CSV file containing lab location. Will pull from EDAV if not attached.
#'
#' @returns `tibble` Cleaned lab data.
#' @examples
#' \dontrun{
#' lab_path <- "C:/Users/XRG9/lab_data_region.csv"
#' ctry.data <- init_dr("algeria", lab_data_path = lab_path)
#' ctry.data$lab.data <- clean_lab_data_regional(ctry.data, "2021-01-01", "2023-12-31")
#'
#' # Not using the desk review pipeline
#' raw.data <- get_all_polio_data()
#' ctry.data <- extract_country_data("algeria", raw.data)
#' ctry.data$lab.data <- read_csv(lab_path)
#' ctry.data$lab.data <- clean_lab_data_regional(ctry.data, "2021-01-01", "2023-12-31")
#' }
#' @export

clean_lab_data_regional <- function(ctry.data, ctry_name = Sys.getenv("DR_COUNTRY"),
                                    start.date, end.date, delim = "-", lab_locs_path = NULL) {
  start.date <- lubridate::as_date(start.date)
  end.date <- lubridate::as_date(end.date)

  # Check if the lab data is attached
  if (is.null(ctry.data$lab.data)) {
    stop("Lab data not attached to ctry.data. Please attach and try again.")
  }

  if ("country" %in% names(ctry.data$lab.data)) {
    cli::cli_alert_warning("Lab data already cleaned.")
    return(ctry.data$lab.data)
  }

  lab.data <- ctry.data$lab.data

  # Cleaning for Cote D'Ivoire
  if (stringr::str_detect(ctry_name, "(?i)IVOIRE")
  ) {
    lab.data <- lab.data |>
      dplyr::mutate(Name = dplyr::if_else(stringr::str_detect(.data$Name, "(?i)IVOIRE"),
        "COTE D'IVOIRE", .data$Name
      ))
  }

  # Filter to only the country of interest
  if (stringr::str_detect(ctry_name, "(?i)IVOIRE")) {
    lab.data <- lab.data |>
      dplyr::filter(Name == "COTE D'IVOIRE")
  } else {
    lab.data <- lab.data |>
      dplyr::filter(Name == ctry_name)
  }

  # Assign the region
  region <- get_region(ctry_name)

  # Download lab.locs if not assigned
  lab.locs <- get_lab_locs(lab_locs_path)

  if (region == "EMRO") {
    cli::cli_process_start("Converting date character columns to date types.")
    emro.lab.01 <- lab.data %>%
      dplyr::rename(country = "Name") %>%
      # make country names long
      dplyr::mutate(
        country = ifelse(.data$country == "AFG", "AFGHANISTAN", .data$country),
        country = ifelse(.data$country == "BAH", "BAHRAIN", .data$country),
        country = ifelse(.data$country == "DJI", "DJIBOUTI", .data$country),
        country = ifelse(.data$country == "EGY", "EGYPT", .data$country),
        country = ifelse(.data$country == "IRN", "IRAN (ISLAMIC REPUBLIC OF)", .data$country),
        country = ifelse(.data$country == "IRQ", "IRAQ", .data$country),
        country = ifelse(.data$country == "JOR", "JORDAN", .data$country),
        country = ifelse(.data$country == "KUW", "KUWAIT", .data$country),
        country = ifelse(.data$country == "LEB", "LEBANON", .data$country),
        country = ifelse(.data$country == "LIB", "LIBYA", .data$country),
        country = ifelse(.data$country == "MOR", "MOROCCO", .data$country),
        country = ifelse(.data$country == "OMA", "OMAN", .data$country),
        country = ifelse(.data$country == "PAK", "PAKISTAN", .data$country),
        country = ifelse(.data$country == "PNA",
          "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM",
          .data$country
        ),
        country = ifelse(.data$country == "QAT", "QATAR", .data$country),
        country = ifelse(.data$country == "SAA", "SAUDI ARABIA", .data$country),
        country = ifelse(.data$country == "SOM", "SOMALIA", .data$country),
        country = ifelse(.data$country == "SUD", "SUDAN", .data$country),
        country = ifelse(.data$country == "SYR", "SYRIAN ARAB REPUBLIC", .data$country),
        country = ifelse(.data$country == "TUN", "TUNISIA", .data$country),
        country = ifelse(.data$country == "UAE", "UNITED ARAB EMIRATES", .data$country),
        country = ifelse(.data$country == "YEM", "YEMEN", .data$country)
      ) %>%
      dplyr::mutate(
        dplyr::across(dplyr::any_of(c(
          "CaseDate",
          "ParalysisOnsetDate",
          "DateStoolCollected",
          "StoolDateSentToLab",
          "DateStoolReceivedinLab",
          "DateFinalCellCultureResult",
          "DateFinalrRTPCRResults",
          "ReportDateSequenceResultSent",
          "DateIsolateRcvdForSeq",
          "DateLArmIsolate",
          "DateRArmIsolate",
          "DateofSequencing",
          "DateNotificationtoHQ"
        )), \(x) as.Date.character(x, tryFormats = c("%Y-%m-%d", "%Y/%m%/%d", "%m/%d/%Y")))
      )
    cli::cli_process_done()


    # lab locations read in with AFRO cleaning script

    # clean lab data -
    # This is a very quick clean and can be improved upon with futher steps such as:
    #  - eliminating nonsensical dates
    #  - check for more duplicates (same epid and specimen number)
    #  - if a date is missing, replace it with a proxy date
    #  - clean/match all countries with lab loc df; I filtered both df to priority countries


    # de-dup
    cli::cli_process_start("Deduplicating data")
    emro.lab.02 <- emro.lab.01 %>%
      dplyr::filter(
        dplyr::between(ParalysisOnsetDate, start.date, end.date),
        country == ctry.data$ctry.pop$ctry[1]
      ) %>%
      dplyr::distinct()

    # Additional cleaning steps
    # need data dictionary, in order to standardize names
    emro.lab.03 <- emro.lab.02 %>%
      # Dropping rows with Specimen number 0 or >2
      dplyr::filter(SpecimenNumber %in% c(1, 2)) %>%
      dplyr::mutate(
        country = stringr::str_to_upper(.data$country),
        year = ifelse(!is.na(.data$ParalysisOnsetDate), lubridate::year(.data$ParalysisOnsetDate), .data$YYYY),
        whoregion = "EMRO"
      )

    # Join lab locations
    emro.lab.04 <- dplyr::full_join(
      emro.lab.03,
      lab.locs |> dplyr::filter(who.region == "EMRO") |>
        dplyr::select("country":"num.ship.seq.samples"),
      by = "country"
    ) %>%
      # count duplicates with same epid and specimen number
      # there should be 2 records for each EPID, specimen 1 and 2

      dplyr::group_by(.data$EPID, .data$SpecimenNumber) %>%
      dplyr::mutate(n = dplyr::n()) %>%
      dplyr::ungroup()

    # seperate blank epids from rest of emro.lab.04 in order to de dupe based on epid and specimen number, join back after dedup
    blank.epid <- emro.lab.04 %>%
      dplyr::filter(is.na(EPID))

    emro.lab.04 <- emro.lab.04 %>%
      dplyr::filter(!is.na(EPID)) %>%
      dplyr::select(-"n")

    emro.lab.04 <- emro.lab.04[!duplicated(emro.lab.04[c("EPID", "SpecimenNumber")]), ]
    cli::cli_process_done()

    # Create intervals (currently using subset of those I need for SC PPT)
    cli::cli_process_start("Creating timeliness interval columns")
    emro.lab.05 <- emro.lab.04 %>%
      dplyr::mutate(
        # Intervals
        days.collect.lab = .data$DateStoolReceivedinLab - .data$DateStoolCollected,
        days.lab.culture = .data$DateFinalCellCultureResult - .data$DateStoolReceivedinLab,
        days.seq.ship = .data$DateIsolateRcvdForSeq - .data$ReportDateSequenceResultSent,
        days.lab.seq = .data$DateofSequencing - .data$DateStoolReceivedinLab,
        days.itd.seqres = .data$DateofSequencing - .data$DateFinalrRTPCRResults,
        days.itd.arriveseq = .data$DateIsolateRcvdForSeq - .data$DateFinalrRTPCRResults,
        days.seq.rec.res = .data$DateofSequencing - .data$DateIsolateRcvdForSeq,

        # Met target yes/no
        met.targ.collect.lab = ifelse(.data$days.collect.lab < 3, 1, 0),
        negative.spec = ifelse(!str_detect(.data$FinalCellCultureResult, "ITD") & .data$FinalITDResult == "NULL", 1, 0),
        met.lab.culture = ifelse(.data$days.lab.culture < 14, 1, 0),
      )
    cli::cli_process_done()

    cli::cli_process_start("Filtering out negative time intervals")
    emro.lab.05 <- emro.lab.05 |>
      # filtering out negative time intervals
      dplyr::filter((days.collect.lab >= 0 | is.na(days.collect.lab)) &
        (days.lab.culture >= 0 | is.na(days.lab.culture)) &
        (days.seq.ship >= 0 | is.na(days.seq.ship)) &
        (days.lab.seq >= 0 | is.na(days.lab.seq)) &
        (days.itd.seqres >= 0 | is.na(days.itd.seqres)) &
        (days.itd.arriveseq >= 0 | is.na(days.itd.arriveseq)) &
        (days.seq.rec.res >= 0 | is.na(days.seq.rec.res)))
    cli::cli_process_done()

    cli::cli_process_start("Filtering nonsensical dates")
    emro.lab.05 <- emro.lab.05 |>
      # filtering out nonsensical dates
      # 1. stool can't be collected before Paralysis
      dplyr::filter((DateStoolCollected >= ParalysisOnsetDate | is.na(ParalysisOnsetDate))) |>
      dplyr::mutate(seq.capacity = ifelse(.data$seq.capacity == "yes", "Sequencing capacity", "No sequencing capacity")) %>%
      dplyr::select(-dplyr::contains("cIntratypeIs"))
    cli::cli_process_done()

    lab.data <- emro.lab.05
    rm(emro.lab.01, emro.lab.02, emro.lab.03, emro.lab.04, emro.lab.05)
  } else if (region == "AFRO") {
    cli::cli_process_start("Converting date character columns to date types.")
    afro.lab.01b <- lab.data %>%
      dplyr::mutate(
        DateStoolCollected = ifelse(.data$DateStoolCollected == "NULL", NA, .data$DateStoolCollected),
        StoolDateSentToLab = ifelse(.data$StoolDateSentToLab == "NULL", NA, .data$StoolDateSentToLab),
        DateStoolReceivedinLab = ifelse(.data$DateStoolReceivedinLab == "NULL", NA, .data$DateStoolReceivedinLab),
        DateFinalCellCultureResult = ifelse(
          .data$DateFinalCellCultureResult == "NULL",
          NA,
          .data$DateFinalCellCultureResult
        ),
        DateFinalrRTPCRResults = ifelse(.data$DateFinalrRTPCRResults == "NULL", NA, .data$DateFinalrRTPCRResults),
        ReportDateSequenceResultSent = ifelse(
          .data$ReportDateSequenceResultSent == "NULL",
          NA,
          .data$ReportDateSequenceResultSent
        ),
        DateIsolateRcvdForSeq = ifelse(.data$DateIsolateRcvdForSeq == "NULL", NA, .data$DateIsolateRcvdForSeq),
        DateLArmIsolate = ifelse(.data$DateLArmIsolate == "NULL", NA, .data$DateLArmIsolate),
        DateRArmIsolate = ifelse(.data$DateRArmIsolate == "NULL", NA, .data$DateRArmIsolate),
        DateofSequencing = ifelse(.data$DateofSequencing == "NULL", NA, .data$DateofSequencing),
        DateNotificationtoHQ = ifelse(.data$DateNotificationtoHQ == "NULL", NA, .data$DateNotificationtoHQ)
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of(c(
          "CaseDate",
          "ParalysisOnsetDate",
          "DateStoolCollected",
          "StoolDateSentToLab",
          "DateStoolReceivedinLab",
          "DateFinalCellCultureResult",
          "DateFinalrRTPCRResults",
          "ReportDateSequenceResultSent",
          "DateIsolateRcvdForSeq",
          "DateLArmIsolate",
          "DateRArmIsolate",
          "DateofSequencing",
          "DateNotificationtoHQ"
        )), \(x) as.Date.character(x, tryFormats = c("%Y-%m-%d", "%Y/%m%/%d", "%m/%d/%Y")))
      )
    cli::cli_process_done()
    # This is a very quick clean and can be improved upon with futher steps such as:
    #  - eliminating nonsensical dates
    #  - check for more duplicates (same epid and specimen number)
    #  - if a date is missing, replace it with a proxy date
    #  - clean/match all countries with lab loc df; I filtered both df to priority countries


    # de-dup
    cli::cli_process_start("Removing duplicates")
    afro.lab.02 <- afro.lab.01b %>%
      dplyr::filter(dplyr::between(ParalysisOnsetDate, start.date, end.date)) %>%
      dplyr::distinct()


    # Additional cleaning steps
    afro.lab.03 <- afro.lab.02 %>%
      # Dropping rows with Specimen number 0 or >2
      dplyr::filter(SpecimenNumber %in% c(1, 2)) %>%
      # replacing "NULL" with NA
      # dplyr::mutate_at(vars(DateStoolCollected:VDPV3), ~na_if(., "NULL")) %>%
      dplyr::mutate(
        country = stringr::str_to_upper(.data$Name),
        country = ifelse(stringr::str_detect(.data$country, "IVOIRE"), "COTE D IVOIRE", .data$country),
        year = lubridate::year(.data$ParalysisOnsetDate),
        whoregion = "AFRO"
      ) %>%
      dplyr::filter(country == ctry.data$ctry.pop$ctry[1]) %>%
      dplyr::select(-dplyr::any_of(c("Name")))


    # Join lab locations
    afro.lab.04 <- dplyr::full_join(
      afro.lab.03,
      lab.locs %>% dplyr::select("country":"num.ship.seq.samples"),
      by = "country"
    ) %>%
      # count duplicates with same epid and specimen number
      # there should be 2 records for each EPID, specimen 1 and 2
      dplyr::group_by(.data$EPID, .data$SpecimenNumber) %>%
      dplyr::mutate(n = dplyr::n()) %>%
      dplyr::ungroup()


    afro.lab.04 <- afro.lab.04[!duplicated(afro.lab.04[c("EPID", "SpecimenNumber")]), ]
    cli::cli_process_done()

    # Create intervals (currently using subset of those I need for SC PPT)
    cli::cli_process_start("Creating timeliness interval columns")
    afro.lab.05 <- afro.lab.04 %>%
      dplyr::select(-"n") %>%
      dplyr::mutate(
        # Intervals
        days.collect.lab = .data$DateStoolReceivedinLab - .data$DateStoolCollected,
        days.lab.culture = .data$DateFinalCellCultureResult - .data$DateStoolReceivedinLab,
        days.seq.ship = .data$DateIsolateRcvdForSeq - .data$ReportDateSequenceResultSent,
        days.lab.seq = .data$DateofSequencing - .data$DateStoolReceivedinLab,
        days.itd.seqres = .data$DateofSequencing - .data$DateFinalrRTPCRResults,
        days.itd.arriveseq = .data$DateIsolateRcvdForSeq - .data$DateFinalrRTPCRResults,
        days.seq.rec.res = .data$DateofSequencing - .data$DateIsolateRcvdForSeq,

        # Met target yes/no
        met.targ.collect.lab = ifelse(.data$days.collect.lab < 3, 1, 0),
        negative.spec = ifelse(!str_detect(.data$FinalCellCultureResult, "ITD") & .data$FinalITDResult == "NULL", 1, 0),
        met.lab.culture = ifelse(.data$days.lab.culture < 14, 1, 0),
      )
    cli::cli_process_done()
    cli::cli_process_start("Filtering negative time intervals")
    afro.lab.05 <- afro.lab.05 |>
      # filtering out negative time intervals
      dplyr::filter((days.collect.lab >= 0 | is.na(days.collect.lab)) &
        (days.lab.culture >= 0 | is.na(days.lab.culture)) &
        (days.seq.ship >= 0 | is.na(days.seq.ship)) &
        (days.lab.seq >= 0 | is.na(days.lab.seq)) &
        (days.itd.seqres >= 0 | is.na(days.itd.seqres)) &
        (days.itd.arriveseq >= 0 | is.na(days.itd.arriveseq)) &
        (days.seq.rec.res >= 0 | is.na(days.seq.rec.res)))
    cli::cli_process_done()
    cli::cli_process_start("Filtering nonsensical dates")
    afro.lab.05 <- afro.lab.05 |>
      # filtering out nonsensical dates
      # 1. stool can't be collected before Paralysis
      # 2. remove out of bounds years in Date final cell culture result
      dplyr::filter(
        (DateStoolCollected >= ParalysisOnsetDate | is.na(ParalysisOnsetDate)),
        (lubridate::year(DateFinalCellCultureResult) <= 2023 | is.na(DateFinalCellCultureResult)),
        # remove a blank specimen row
        !is.na(EPID)
      ) %>%
      # renaming culture.itd.lab for Nigeria which has two labs in lab.locs, simply naming Nigeria
      dplyr::mutate(
        culture.itd.lab = ifelse(.data$country == "NIGERIA", "Nigeria", .data$culture.itd.lab),
        ParalysisOnsetDate = lubridate::ymd(.data$ParalysisOnsetDate),
        seq.capacity = ifelse(.data$seq.capacity == "yes", "Sequencing capacity", "No sequencing capacity")
      ) %>%
      dplyr::select(-dplyr::contains("cIntratypeIs"))

    lab.data <- afro.lab.05
    rm(afro.lab.01b, afro.lab.02, afro.lab.03, afro.lab.04, afro.lab.05)
  }

  lab.data <- lab.data |>
    tidyr::separate_wider_delim(
      cols = "EPID",
      delim = delim,
      names = c(
        "epid_ctry", "epid_prov", "epid_dist",
        "epid_04", "epid_05"
      ),
      too_many = "debug",
      too_few = "debug"
    )

  cli::cli_process_start("Imputing missing province and district data from AFP linelist")
  # Match province and district by EPID number -
  lab.data$prov <- NA
  lab.data$dist <- NA
  lab.data$adm1guid <- NA
  lab.data$adm2guid <- NA

  # !!! changed place.admin.1 = prov and place.admin.2 = dist from original code
  # reason is that those were renamed at the top of the script
  lab.data$prov <- ctry.data$afp.all.2$prov[match(lab.data$EPID, ctry.data$afp.all.2$epid)]
  lab.data$dist <- ctry.data$afp.all.2$dist[match(lab.data$EPID, ctry.data$afp.all.2$epid)]

  lab.data$adm1guid <- ctry.data$afp.all.2$adm1guid[match(lab.data$EPID, ctry.data$afp.all.2$epid)]
  lab.data$adm2guid <- ctry.data$afp.all.2$adm2guid[match(lab.data$EPID, ctry.data$afp.all.2$epid)]

  # Additional cleaning steps
  #---- Additional data cleaning steps
  geo_lookup_table <- ctry.data$afp.all.2 |>
    dplyr::select("epid", dplyr::matches("guid"), dplyr::contains("$adm"), "ctry", "prov", "dist", "year") |>
    tidyr::separate_wider_delim(
      cols = "epid", delim = delim,
      names = c(
        "epid_ctry", "epid_prov", "epid_dist",
        "epid_04", "epid_05"
      ),
      too_many = "merge",
      too_few = "align_start"
    ) |>
    dplyr::select(dplyr::contains("epid"), "ctry", "prov", "dist", dplyr::matches("adm[0-3]guid"), "year") |>
    dplyr::distinct()

  prov_lookup_table <- geo_lookup_table |>
    dplyr::select("epid_prov", "prov", "adm0guid", "adm1guid", "year") |>
    dplyr::distinct() |>
    tidyr::drop_na("prov")

  # Check look up table for potential duplicated rows
  prov_lookup_row_dups <- prov_lookup_table |>
    dplyr::mutate(epid_comb = str_c(epid_prov, year, sep = "-")) |>
    dplyr::group_by(.data$epid_comb, .data$epid_prov, .data$year) |>
    dplyr::summarise(n = n()) |>
    dplyr::filter(n > 1) |>
    ungroup()

  # Remove duplicates from the look up table
  prov_lookup_row_dups <- prov_lookup_row_dups |>
    dplyr::select(!dplyr::any_of(c("epid_comb", "n")))
  prov_lookup_table <- anti_join(prov_lookup_table, prov_lookup_row_dups)


  dist_lookup_table <- geo_lookup_table |>
    dplyr::select("epid_dist", "dist", "adm2guid", "year") |>
    dplyr::distinct() |>
    tidyr::drop_na("dist")

  # Check look up table for potential duplicated rows
  dist_lookup_row_dups <- dist_lookup_table |>
    dplyr::mutate(epid_comb = str_c(epid_dist, year, sep = "-")) |>
    dplyr::group_by(.data$epid_comb, .data$epid_dist, .data$year) |>
    dplyr::summarise(n = n()) |>
    dplyr::filter(n > 1) |>
    ungroup()

  # Remove duplicates from the look up table
  dist_lookup_row_dups <- dist_lookup_row_dups |>
    dplyr::select(!dplyr::any_of(c("epid_comb", "n")))
  dist_lookup_table <- anti_join(dist_lookup_table, dist_lookup_row_dups)

  # Geomatching algorithm
  lab.data <- lab.data |>
    dplyr::left_join(prov_lookup_table) |>
    dplyr::left_join(prov_lookup_table, by = dplyr::join_by(epid_prov, year)) |>
    dplyr::mutate(
      prov = dplyr::coalesce(.data$prov.x, .data$prov.y),
      adm1guid = dplyr::coalesce(.data$adm1guid.x, .data$adm1guid.y)
    ) |>
    dplyr::left_join(dist_lookup_table) |>
    dplyr::left_join(dist_lookup_table, by = dplyr::join_by(epid_dist, year)) |>
    dplyr::mutate(
      dist = dplyr::coalesce(.data$dist.x, .data$dist.y),
      adm2guid = dplyr::coalesce(.data$adm2guid.x, .data$adm2guid.y)
    ) |>
    dplyr::rename(
      adm0guid = "adm0guid.x",
    ) |>
    dplyr::select(-dplyr::ends_with(".y"), -dplyr::ends_with(".x"))

  lab.data <- lab.data |>
    dplyr::rename(ctry.code2 = "epid_ctry")
  lab.data <- lab.data |>
    dplyr::mutate(CaseOrContact = "1-Case")
  lab.data <- lab.data |>
    dplyr::rename(EpidNumber = "EPID")
  lab.data <- lab.data |>
    dplyr::rename(District = "dist", Province = "prov")
  lab.data <- lab.data |>
    dplyr::rename(DateOfOnset = "CaseDate")
  cli::cli_process_done()

  # Message for values without any province or district information
  cli::cli_alert_warning(paste0("Remaining records with missing province: ", sum(is.na(lab.data$Province))))
  cli::cli_alert_warning(paste0("Remaining records with missing district: ", sum(is.na(lab.data$District))))

  # adding additional subintervals (these aren't present in regional lab data, so are created as dummy variables)
  lab.data <- lab.data |>
    mutate(
      days.coll.sent.field = NA,
      days.sent.field.rec.nat = NA,
      days.rec.nat.sent.lab = NA,
      days.sent.lab.rec.lab = NA,
      days.rec.lab.culture = NA,
    )

  return(lab.data)
}

#' Clean lab data
#'
#' Main lab data cleaning function. Automatically detects whether the dataset
#' came from WHO or the regional office.
#'
#' @param ctry.data `list` Large list containing country polio data with lab data attached. Either from
#' [extract_country_data()] or [init_dr()].
#' @param ctry_name `str` Name of the country. Defaults to the desk review country.
#' @param start.date `str` Start date of analysis.
#' @param end.date `str` End date of analysis.
#' @param delim `str` Delimiter for EPIDs. Default is `"-"`.
#' @param lab_locs_path `str` Location of testing lab locations. Default is `NULL`. Will download from EDAV, if necessary.
#' @returns `tibble` Cleaned lab data.
#' @examples
#' \dontrun{
#' lab_path <- "C:/Users/XRG9/lab_data_who.csv"
#' ctry.data <- init_dr("algeria", lab_data_path = lab_path)
#' ctry.data$lab.data <- clean_lab_data(ctry.data, "2021-01-01", "2023-12-31")
#'
#' # Not using the desk review pipeline
#' raw.data <- get_all_polio_data()
#' ctry.data <- extract_country_data("algeria", raw.data)
#' ctry.data$lab.data <- read_csv(lab_path)
#' ctry.data$lab.data <- clean_lab_data(ctry.data, "2021-01-01", "2023-12-31")
#' }
#' @export
clean_lab_data <- function(ctry.data, ctry_name = Sys.getenv("DR_COUNTRY"),
                           start.date = start_date, end.date = end_date,
                           delim = "-", lab_locs_path = NULL) {
  ctry_name <- stringr::str_to_upper(ctry_name)
  # Check if the lab data is attached
  if (is.null(ctry.data$lab.data)) {
    stop("Lab data not attached to ctry.data. Please attach and try again.")
  }

  # Variable to hold onto lab data
  lab.data <- NULL

  # Determine the type of cleaning to do
  lab.data.cols <- names(ctry.data$lab.data)

  if ("MasterKey" %in% lab.data.cols) {
    if ("prov" %in% names(ctry.data$lab.data)) {
      cli::cli_alert_warning("Lab data already cleaned.")
      return(ctry.data$lab.data)
    }

    if (nrow(ctry.data$lab.data) == 0) {
      message("There are no entries for lab data.")
      return(NULL)
    }

    lab.data <- clean_lab_data_who(ctry.data, start.date, end.date, delim)
  } else {
    if ("Province" %in% names(ctry.data$lab.data)) {
      cli::cli_alert_warning("Lab data already cleaned.")
      return(ctry.data$lab.data)
    }
    lab.data <- clean_lab_data_regional(ctry.data, start.date, end.date, delim, lab_locs_path)
  }

  return(lab.data)
}


#' Summary of lab sample timeliness
#'
#' Generates a summary of the timeliness of samples for specific intervals.
#'
#' @import dplyr
#' @param lab.data `tibble` Lab data. Ensure that this lab data is cleaned using
#' [clean_lab_data()] before running the function.
#' @param spatial.scale `str` Spatial scale to analyze the data. Valid values are `"ctry", "prov", "dist"`.
#' @param start.date `str` Start date of analysis.
#' @param end.date `str` End date of analysis.
#'
#' @return `tibble` A table with timeliness data summary.
#' @returns `tibble` Cleaned lab data.
#' @examples
#' \dontrun{
#' lab_path <- "C:/Users/XRG9/lab_data_who.csv"
#' ctry.data <- init_dr("algeria", lab_data_path = lab_path)
#' ctry.data$lab.data <- clean_lab_data(ctry.data, "2021-01-01", "2023-12-31")
#' lab.timeliness.ctry <- generate_lab_timeliness(ctry.data$lab.data, "ctry", start_date, end_date)
#' }
#' @export
generate_lab_timeliness <-
  function(lab.data,
           spatial.scale,
           start.date,
           end.date) {
    geo <- switch(spatial.scale,
      "ctry" = "adm0guid",
      "prov" = "adm1guid",
      "dist" = "adm2guid"
    )

    start.date <- lubridate::as_date(start.date)
    end.date <- lubridate::as_date(end.date)

    # Check if the lab data is attached
    if (is.null(lab.data)) {
      stop("Lab data not attached. Please attach and try again.")
    }

    lab_medians <- lab.data |>
      dplyr::filter(dplyr::between(as.Date(DateOfOnset), start.date, end.date)) |>
      dplyr::group_by(year, get(geo)) |>
      dplyr::summarise(dplyr::across(
        dplyr::starts_with("days."),
        \(x) as.numeric(median(x, na.rm = T))
      )) |>
      tidyr::pivot_longer(
        cols = dplyr::starts_with("days."),
        names_to = "type", values_to = "medi"
      )
    lab_counts <- lab.data |>
      dplyr::filter(dplyr::between(as.Date(DateOfOnset), start.date, end.date)) |>
      dplyr::group_by(year, get(geo)) |>
      dplyr::summarise(dplyr::across(
        dplyr::starts_with("days."),
        \(x) sum(!is.na(x))
      )) |>
      tidyr::pivot_longer(
        cols = dplyr::starts_with("days."),
        names_to = "type", values_to = "freq"
      )

    lab <- dplyr::full_join(lab_counts, lab_medians)

    lab <- lab |> dplyr::filter(!is.na(`get(geo)`))

    lab <- switch(spatial.scale,
      "ctry" = {
        lab <- lab |> dplyr::rename(adm0guid = "get(geo)")
      },
      "prov" = {
        lab <- lab |> dplyr::rename(adm1guid = "get(geo)")
      },
      "dist" = {
        lab <- lab |> dplyr::rename(adm2guid = "get(geo)")
      }
    )

    return(lab)
  }
