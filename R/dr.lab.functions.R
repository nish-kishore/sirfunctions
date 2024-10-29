#' Gets information related to testing labs
#' @import cli dplyr
#' @param path path the lab location file
#'
#' @return tibble containing the test lab locations

get_lab_locs <- function(path) {
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
#' @import stringr
#' @param country_name name of the country
#'
#' @return string "EMRO" or "AFRO"
#' @export
get_region <- function(country_name = Sys.getenv("DR_COUNTRY")) {
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



  # Assign the region
  region <- NULL
  if (stringr::str_to_upper(country_name) %in% emro_ctry) {
    region <- "EMRO"
  } else if (stringr::str_to_upper(country_name) %in% afro_ctry | stringr::str_detect(country_name, "(?i)IVOIRE")) {
    region <- "AFRO"
  } else {
    stop(paste0(
      "Country does not belong in either AFRO or EMRO. ",
      "Countries outside these regions are not supported by the cleaning function at this time."
    ))
  }

  return(region)
}


#' Function to load the raw lab data
#' @import stringr readr
#' @param lab_data_path file path as a string to the lab data
#' @param sheet_name name of the sheet to load
#'
#' @return a tibble containing lab data
#' @export
load_lab_data <- function(lab_data_path, sheet_name = NULL) {

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop('Package "readxl" must be installed to use this function.',
         .call = FALSE)
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
#' @param ctry.data RDS object containing country polio data
#' @param start.date start date of the desk review
#' @param end.date end date of the desk review
#' @param error_path file path to the error folder
#'
#' @export
lab_data_errors <- function(ctry.data, start.date = start_date, end.date = end_date,
                            error_path = Sys.getenv("DR_ERROR_PATH")) {
  # Check if the lab data is attached
  if (is.null(ctry.data$lab.data)) {
    stop("Lab data not attached to ctry.data. Please attach and try again.")
  }

  # Determine the type of cleaning to do
  lab.data.cols <- names(ctry.data$lab.data)

  if ("ctry.code2" %in% lab.data.cols) {
    lab_data_errors_who(ctry.data, start.date, end.date)
  } else {
    lab_data_errors_region(ctry.data, start.date, end.date)
  }
}

#' Check common errors in the regional lab data
#' @import dplyr stringr cli writexl
#' @param ctry.data Rds file containing polio country data
#' @param start.date start date of the desk review
#' @param end.date end date of the desk review
#' @param error_path path to folder to save the error log
#'
lab_data_errors_region <- function(ctry.data, start.date, end.date,
                                   error_path = Sys.getenv("DR_ERROR_PATH")) {
  lab.data <- ctry.data$lab.data

  # Filter to only the country of interest
  lab.data <- lab.data |>
    dplyr::filter(Name == Sys.getenv("DR_COUNTRY"))

  # Cleaning for Cote D'Ivoire
  if (stringr::str_detect(Sys.getenv("DR_COUNTRY"), "(?i)IVIORE")) {
    lab.data <- lab.data |>
      dplyr::mutate(Name = dplyr::if_else(stringr::str_detect(.data$Name, "(?i)IVOIRE"), "COTE D'IVIORE", .data$Name))
  }

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
      )), \(x) as.Date.character(x, "%m/%d/%Y")
    ))

  # Check for duplicates
  cli::cli_process_start("Checking for duplicate data")

  duplicate.02 <- lab.data %>%
    dplyr::filter(dplyr::between(ParalysisOnsetDate, start.date, end.date)) %>%
    dplyr::distinct()

  # Additional cleaning steps
  duplicate.03 <- duplicate.02 %>%
    # Dropping rows with Specimen number 0 or >2
    dplyr::filter(SpecimenNumber %in% c(1, 2)) %>%
    # replacing "NULL" with NA
    # dplyr::mutate_at(vars(DateStoolCollected:VDPV3), ~na_if(., "NULL")) %>%
    dplyr::mutate(
      country = stringr::str_to_upper(.data$country),
      country = ifelse(stringr::str_detect(.data$country, "IVOIRE"), "COTE D IVOIRE", .data$country),
      year = lubridate::year(.data$ParalysisOnsetDate),
      whoregion = "AFRO"
    ) %>%
    dplyr::filter(country == ctry.data$ctry$ADM0_NAME)


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
    cli::cli_alert_warning(paste0("There are ", nrow(invalid_intervals), " records with negative intervals."))
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

#' Checks for common data errors in lab data
#' @import dplyr cli writexl
#' @param start.date start date of the desk review
#' @param end.date end date of the desk review
#' @param error_path folder to save the log to
#' @param ctry.data RDS object containing polio data from a country
#'
lab_data_errors_who <- function(ctry.data, start.date, end.date,
                                error_path = Sys.getenv("DR_ERROR_PATH")) {
  if (is.null(ctry.data$lab.data)) {
    message("No lab data attached.")
    return(NULL)
  }

  lab.data <- ctry.data$lab.data |> dplyr::filter(ctry.code2 == ctry.data$ctry$WHO_CODE)

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
    cli::cli_alert_warning(paste0("There are ", nrow(invalid_dates), " cases with invalid dates."))
  } else {
    cli::cli_alert_success("No invalid dates detected.")
  }

  cli::cli_process_done()

  cli::cli_process_start("Checking for missing years")
  missing_years <- lab.data |>
    dplyr::filter(is.na(year)) |>
    dplyr::filter(
      year >= lubridate::year(start.date) & year <= lubridate::year(end.date),
      CaseOrContact == "1-Case"
    )

  if (nrow(missing_years) != 0) {
    cli::cli_alert_warning(paste0("There are ", nrow(missing_years), " cases with missing years."))
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
#' @import dplyr cli stringr tidyr
#' @param ctry.data country data RDS object
#' @param start.date start date of the desk review
#' @param end.date end date of the desk review
#' @param delim delimiter used in the EPIDs. Defaults to "-".
#'
#' @return a tibble containing clean lab data
#' @export
clean_lab_data_who <- function(ctry.data, start.date, end.date, delim = "-") {

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
      \(x) lubridate::as_date(x)
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
    dplyr::group_by(epid_comb, epid_prov, year) |>
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
    dplyr::group_by(epid_comb, epid_dist, year) |>
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
      prov.x = dplyr::if_else(is.na(.data$prov.x) & !is.na(.data$prov.y), .data$prov.y, .data$prov.x),
      adm1guid.x = dplyr::if_else(is.na(.data$adm1guid.x) & !is.na(.data$adm1guid.y), .data$adm1guid.y, .data$adm1guid.y)
    ) |>
    dplyr::left_join(dist_lookup_table) |>
    dplyr::left_join(dist_lookup_table, by = dplyr::join_by(epid_dist, year)) |>
    dplyr::mutate(
      dist.x = dplyr::if_else(is.na(.data$dist.x) & !is.na(.data$dist.y), .data$dist.y, .data$dist.x),
      adm2guid.x = dplyr::if_else(is.na(.data$adm2guid.x) & !is.na(.data$adm2guid.y), .data$adm2guid.y, .data$adm2guid.y)
    ) |>
    dplyr::rename(
      adm0guid = "adm0guid.x",
      adm1guid = "adm1guid.x",
      adm2guid = "adm2guid.x",
      prov = "prov.x",
      dist = "dist.x"
    ) |>
    dplyr::select(-dplyr::ends_with(".y"))

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
#' Regional data have different columns compared to the WHO lab data. The cleaning
#' of the regional data is adapted from the lab data cleaning code from the GPSAP
#' indicator script.
#' @import cli dplyr stringr
#' @param ctry.data Rds polio data at the country level with lab data attached
#' @param start.date start date of the desk review
#' @param end.date end date of the desk review
#' @param delim delimiter used for EPIDs. Default is "-".
#' @param lab_locs_path path to CSV file containing lab location. Will pull from EDAV if not attached
#'
#' @return tibble of lab data
#' @export

clean_lab_data_regional <- function(ctry.data, start.date, end.date, delim = "-", lab_locs_path = NULL) {

  lab.data <- ctry.data$lab.data

  # Cleaning for Cote D'Ivoire
  if (stringr::str_detect(Sys.getenv("DR_COUNTRY"), "(?i)IVOIRE")
  ) {
    lab.data <- lab.data |>
      dplyr::mutate(Name = dplyr::if_else(stringr::str_detect(.data$Name, "(?i)IVOIRE"),
        "COTE D'IVOIRE", .data$Name
      ))
  }

  # Filter to only the country of interest
  if (stringr::str_detect(Sys.getenv("DR_COUNTRY"), "(?i)IVOIRE")) {
    lab.data <- lab.data |>
      dplyr::filter(Name == "COTE D'IVOIRE")
  } else {
    lab.data <- lab.data |>
      dplyr::filter(Name == Sys.getenv("DR_COUNTRY"))
  }

  # Assign the region
  region <- get_region()

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
        country = ifelse(.data$country == "PNA", "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM", .data$country),
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
        country == ctry.data$ctry$ADM0_NAME
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
        )), \(x) as.Date.character(x, tryFormats = c("%Y-%m-%d", "%Y/%m%/%d", "%m/%d/%Y"))
      ))
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
      dplyr::filter(country == ctry.data$ctry$ADM0_NAME) %>%
      dplyr::select(-"Name")


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
    dplyr::group_by(epid_comb, epid_prov, year) |>
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
    dplyr::group_by(epid_comb, epid_dist, year) |>
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

#' Main lab data cleaning function. Automatically detects whether the dataset
#' came from WHO or the regional office
#'
#' @param ctry.data Rds file containing country polio data with lab.data attached
#' @param start.date start date of the desk review
#' @param end.date end date of the desk review
#' @param delim delimiter for EPIDs. Default is "-".
#' @param lab_locs_path location of testing lab locations. Default is NULL. Will download from EDAV, if necessary.
#'
#' @return a tibble containing the cleaned lab data
#' @export
clean_lab_data <- function(ctry.data, start.date = start_date, end.date = end_date,
                           delim = "-", lab_locs_path = NULL) {
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


#' Generate timeliness intervals with lab data
#' @import dplyr
#' @param lab.data lab data
#' @param spatial.scale spatial scale to analyze the data. Valid values are "ctry", "prov", "dist"
#' @param start.date start date of analysis
#' @param end.date end date of analysis
#'
#' @return a table with timeliness data summary
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

    # Check if the lab data is attached
    if (is.null(lab.data)) {
      stop("Lab data not attached to ctry.data. Please attach and try again.")
    }

    lab1 <- lab.data |>
      dplyr::filter(dplyr::between(as.Date(DateOfOnset), start.date, end.date)) |>
      dplyr::group_by(year, get(geo)) |>
      dplyr::summarize(
        medi = median(days.collect.lab, na.rm = T),
        freq = dplyr::n()
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(type = "days.collect.lab") |>
      dplyr::mutate(medi = as.numeric(.data$medi))

    lab2 <- lab.data |>
      dplyr::filter(as.Date(DateOfOnset) >= start.date &
        as.Date(DateOfOnset) <= end.date) |>
      dplyr::group_by(year, get(geo)) |>
      dplyr::summarize(
        medi = median(days.lab.culture, na.rm = T),
        freq = dplyr::n()
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(type = "days.lab.culture") |>
      dplyr::mutate(medi = as.numeric(.data$medi))


    lab3 <- lab.data |>
      dplyr::filter(as.Date(DateOfOnset) >= start.date &
        as.Date(DateOfOnset) <= end.date) |>
      dplyr::group_by(year, get(geo)) |>
      dplyr::summarize(
        medi = median(days.seq.ship, na.rm = T),
        freq = dplyr::n()
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(type = "days.seq.ship") |>
      dplyr::mutate(medi = as.numeric(.data$medi))

    lab4 <- lab.data |>
      dplyr::filter(as.Date(DateOfOnset) >= start.date &
        as.Date(DateOfOnset) <= end.date) |>
      dplyr::group_by(year, get(geo)) |>
      dplyr::summarize(
        medi = median(days.lab.seq, na.rm = T),
        freq = dplyr::n()
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(type = "days.lab.seq") |>
      dplyr::mutate(medi = as.numeric(.data$medi))

    lab5 <- lab.data |>
      dplyr::filter(as.Date(DateOfOnset) >= start.date &
        as.Date(DateOfOnset) <= end.date) |>
      dplyr::group_by(year, get(geo)) |>
      dplyr::summarize(
        medi = median(days.itd.seqres, na.rm = T),
        freq = dplyr::n()
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(type = "days.itd.seqres") |>
      dplyr::mutate(medi = as.numeric(.data$medi))

    lab6 <- lab.data |>
      dplyr::filter(as.Date(DateOfOnset) >= start.date &
        as.Date(DateOfOnset) <= end.date) |>
      dplyr::group_by(year, get(geo)) |>
      dplyr::summarize(
        medi = median(days.itd.arriveseq, na.rm = T),
        freq = dplyr::n()
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(type = "days.itd.arriveseq") |>
      dplyr::mutate(medi = as.numeric(.data$medi))

    lab7 <- lab.data |>
      dplyr::filter(as.Date(DateOfOnset) >= start.date &
        as.Date(DateOfOnset) <= end.date) |>
      dplyr::group_by(year, get(geo)) |>
      dplyr::summarize(
        medi = median(days.seq.rec.res, na.rm = T),
        freq = dplyr::n()
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(type = "days.seq.rec.res") |>
      dplyr::mutate(medi = as.numeric(.data$medi))

    # Additional sub-intervals that may be of interest
    lab8 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start.date &
               as.Date(DateOfOnset) <= end.date) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.coll.sent.field, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.coll.sent.field") |>
      mutate(medi = as.numeric(.data$medi))

    lab9 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start.date &
               as.Date(DateOfOnset) <= end.date) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.sent.field.rec.nat, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.sent.field.rec.nat") |>
      mutate(medi = as.numeric(.data$medi))

    lab10 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start.date &
               as.Date(DateOfOnset) <= end.date) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.rec.nat.sent.lab, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.rec.nat.sent.lab") |>
      mutate(medi = as.numeric(.data$medi))

    lab11 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start.date &
               as.Date(DateOfOnset) <= end.date) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.sent.lab.rec.lab, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.sent.lab.rec.lab") |>
      mutate(medi = as.numeric(medi))

    lab12 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start.date &
               as.Date(DateOfOnset) <= end.date) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.rec.lab.culture, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.rec.lab.culture") |>
      mutate(medi = as.numeric(.data$medi))

    lab <- dplyr::bind_rows(lab1, lab2, lab3, lab4, lab5, lab6, lab7,
                            lab8, lab9, lab10, lab11, lab12)
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
