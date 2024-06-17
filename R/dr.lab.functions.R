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


#' Checks for common data errors in lab data
#'
#' @param lab.data lab.data from WHO
#' @param ctry.data RDS object containing polio data from a country
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#' @export
lab_data_errors_who <- function(ctry.data, start.date = start_date, end.date = end_date,
                                error_path = Sys.getenv("DR_ERROR_PATH")) {

  if (is.null(ctry.data$lab.data)) {
    message("No lab data attached.")
    return(NULL)
  }

  lab.data <- ctry.data$lab.data |> filter(ctry.code2 == ctry.data$ctry$ISO_3_CODE)

  cli::cli_process_start("Checking for invalid dates from cases.")
  invalid_dates <- lab.data |>
    filter((days.collect.lab < 0) &
             (days.lab.culture < 0) &
             (days.seq.ship < 0) &
             (days.lab.seq < 0) &
             (days.itd.seqres < 0) &
             (days.itd.arriveseq < 0) &
             (days.seq.rec.res < 0)
  ) |> filter(year >= year(start_date) & year <= year(end_date),
                         CaseOrContact == "1-Case")

  if (nrow(invalid_dates) != 0) {
    cli::cli_alert_warning(paste0("There are ", nrow(invalid_dates), " cases with invalid dates."))
  } else {
    cli::cli_alert_success("No invalid dates detected.")
  }

  cli::cli_process_done()

  cli::cli_process_start("Checking for missing years")
  missing_years <- lab.data |> filter(is.na(year)) |>
    filter(year >= year(start.date) & year <= year(end.date),
           CaseOrContact == "1-Case")

  if (nrow(missing_years) != 0) {
    cli::cli_alert_warning(paste0("There are ", nrow(missing_years), " cases with missing years."))
  } else {
    cli::cli_alert_success("No cases with missing years.")
  }
  cli::cli_process_done()

  cli::cli_process_start("Checking for missing EPIDs in the AFP dataset.")
  missing_epids = lab.data |> filter(!(EpidNumber %in% ctry.data$afp.all.2$epid))

  if (nrow(missing_epids) != 0) {
    cli::cli_alert_warning(paste0("There are ", nrow(missing_epids), " lab cases not in the AFP linelist."))
  } else {
    cli::cli_alert_success("No lab cases missing in the AFP linelist.")
  }
  cli::cli_process_done()

  cli::cli_alert("Run clean_lab_data() to attempt data fixes and perform the check again.")

  error_log <- list()
  error_log$invalid_dates <- invalid_dates
  error_log$missing_years <- missing_years
  error_log$missing_epids <- missing_epids

  writexl::write_xlsx(error_log, path = file.path(error_path, "lab.errors.xlsx"))
}

#' Clean polio lab data from WHO
#'
#' @param lab.data raw lab data
#' @param ctry.data country data RDS object
#'
#' @return a tibble containing clean lab data
#' @export
clean_lab_data_who <- function(ctry.data, start.date = start_date,
                               end.date = end_date, delim = "-") {

  if (is.null(ctry.data$lab.data)) {
    message("Lab data not attached to country data.")
    return(NULL)
  }

  cli::cli_process_start("Filtering country-specific lab data")
  lab.data <- ctry.data$lab.data |> filter(ctry.code2 == ctry.data$ctry$ISO_3_CODE)
  cli::cli_process_done()

  cli::cli_process_start("Filtering for cases with valid dates")
  lab.data2 = lab.data %>%
    filter((days.collect.lab >= 0 | is.na(days.collect.lab)) &
             (days.lab.culture >= 0 | is.na(days.lab.culture)) &
             (days.seq.ship >= 0 | is.na(days.seq.ship)) &
             (days.lab.seq >= 0 | is.na(days.lab.seq)) &
             (days.itd.seqres >= 0 | is.na(days.itd.seqres)) &
             (days.itd.arriveseq >= 0 | is.na(days.itd.arriveseq)) &
             (days.seq.rec.res >= 0 | is.na(days.seq.rec.res))
    ) |>
    filter(year >= year(start.date) & year <= year(end.date),
           CaseOrContact == "1-Case")
  cli::cli_process_done()

  cli::cli_process_start("Imputing missing years")
  miss = filter(lab.data2, is.na(year))

  miss = miss %>%
    mutate(year2 = substr(EpidNumber, 13,14)) %>%
    mutate(year2 = as.numeric(paste0("20", year2)))

  lab.data2 = lab.data2 %>%
    mutate(year = case_when(
      is.na(year) ~ miss$year2[match(lab.data2$EpidNumber, miss$EpidNumber)],
      T ~ year
    ))
  cli::cli_process_done()

  # !!! after operation no more missing years
  count(lab.data2, year)
  # !!! District in the original dataset and distict names are incorrect
  count(lab.data2, District)

  cli::cli_process_start("Correcting district and province names.")
  lab.data2$Province = str_to_upper(lab.data2$Province)
  lab.data2$District = str_to_upper(lab.data2$District)
  lab.data2$Province = iconv(lab.data2$Province, to='ASCII//TRANSLIT')
  lab.data2$District = iconv(lab.data2$District, to='ASCII//TRANSLIT')
  # !!! missing values for province in 13 entries for the date range specified
  count(lab.data2, Province, year)

  # Match province and district by epid number -
  lab.data2$prov = NA
  lab.data2$dist = NA
  lab.data2$adm1guid = NA
  lab.data2$adm2guid = NA

  # !!! changed place.admin.1 = prov and place.admin.2 = dist from original code
  # reason is that those were renamed at the top of the script
  lab.data2$prov = ctry.data$afp.all.2$prov[match(lab.data2$EpidNumber, ctry.data$afp.all.2$epid)]
  lab.data2$dist = ctry.data$afp.all.2$dist[match(lab.data2$EpidNumber, ctry.data$afp.all.2$epid)]

  lab.data2$adm1guid = ctry.data$afp.all.2$adm1guid[match(lab.data2$EpidNumber, ctry.data$afp.all.2$epid)]
  lab.data2$adm2guid = ctry.data$afp.all.2$adm2guid[match(lab.data2$EpidNumber, ctry.data$afp.all.2$epid)]


  # matching by Epid
  # !!! first pass at reconciling missing values
  count(lab.data2, prov, year)

  potential_errors = lab.data2
  potential_errors %>% count(EpidNumber %in% ctry.data$afp.all.2$epid)
  potential_errors = lab.data2 %>%
    filter(is.na(prov))
  dim(potential_errors)
  potential_errors$EpidNumber
  potential_errors$Province

  # By province name
  potential_errors$adm1guid = ctry.data$afp.all.2$adm1guid[match(potential_errors$Province, ctry.data$afp.all.2$prov)]
  potential_errors$prov = ctry.data$afp.all.2$prov[match(potential_errors$Province, ctry.data$afp.all.2$prov)]


  count(potential_errors, prov, adm1guid)

  potential_errors2 = potential_errors %>%
    filter(is.na(prov))

  potential_errors2$Province
  potential_errors2$EpidNumber

  # Totally random cases in potential_errors2
  lab.data2 = lab.data2 %>%
    mutate(prov = ifelse(is.na(prov), potential_errors$prov[match(lab.data2$EpidNumber,
                                                     potential_errors$EpidNumber)], prov)) %>%
    mutate(adm1guid = ifelse(is.na(adm1guid), potential_errors$adm1guid[match(lab.data2$EpidNumber,
                                                                 potential_errors$EpidNumber)], adm1guid))
  count(lab.data2, prov, adm1guid)

  count(lab.data2, prov)
  count(lab.data2, dist)

  #---- Additional data cleaning steps
  geo_lookup_table <- ctry.data$afp.all.2 |>
    select(epid, matches("guid"), contains("$adm"), ctry, prov, dist, year) |>
    separate_wider_delim(cols=epid, delim = delim,
                         names = c("epid_ctry", "epid_prov", "epid_dist",
                                   "epid_04", "epid_05"),
                         too_many = "merge"
    ) |>
    select(contains("epid"), ctry, prov, dist, matches("adm[0-3]guid"), year) |>
    distinct()

  prov_lookup_table <- geo_lookup_table |>
    select(epid_prov, prov, adm0guid, adm1guid, year) |>
    distinct()

  dist_lookup_table <- geo_lookup_table |>
    select(epid_dist, dist, adm2guid, year) |>
    distinct()

  # geomatching algorithm
  lab.data2 <- lab.data2 |>
    separate_wider_delim(cols=EpidNumber, delim = delim,
                         names = c("epid_ctry", "epid_prov", "epid_dist",
                                   "epid_04", "epid_05"),
                         names_repair = "unique",
                         too_many = "merge",
                         too_few = "debug")
  test <- lab.data2

  test <- test |>
    left_join(prov_lookup_table) |>
    left_join(prov_lookup_table, by = join_by(epid_prov, year)) |>
    mutate(prov.x = if_else(is.na(prov.x) & !is.na(prov.y), prov.y, prov.x),
           adm1guid.x = if_else(is.na(adm1guid.x) & !is.na(adm1guid.y), adm1guid.y, adm1guid.y)
    ) |>
    left_join(dist_lookup_table) |>
    left_join(dist_lookup_table, by = join_by(epid_dist, year)) |>
    mutate(dist.x = if_else(is.na(dist.x) & !is.na(dist.y), dist.y, dist.x),
           adm2guid.x = if_else(is.na(adm2guid.x) & !is.na(adm2guid.y), adm2guid.y, adm2guid.y)
    ) |>
    rename(adm0guid = adm0guid.x,
           adm1guid = adm1guid.x,
           adm2guid = adm2guid.x,
           prov = prov.x,
           dist = dist.x) |>
    select(-ends_with(".y"))

  # check for correctness
  check <- test |>
    select(starts_with("epid_"), matches("adm[1-2]"), prov, dist, EpidNumber, year)
  mismatch_dist <- anti_join(check, dist_lookup_table)
  # 14 mismatches in prov
  mismatch_prov <- anti_join(check, prov_lookup_table)

  lab.data2 <- test

  cli::cli_process_done()
  return(lab.data2)
}

#' Clean lab data from the regional offices.
#' Regional data have different columns compared to the WHO lab data. The cleaning
#' of the regional data is adapted from the lab data cleaning code from the GPSAP
#' indicator script.
#'
#' @param ctry.data Rds polio data at the country level
#' @param region AFRO or EMRO
#' @param lab.locs location of labs
#' @param start_date start date of the desk review
#' @param end_date end date of the desk review
#' @param delim delimiter used for EPIDs. Default is "-".
#'
#' @return tibble of lab data
#' @export

clean_lab_data_regional <- function(ctry.data, region, lab.locs,
                                    start.date=start_date, end.date=end_date,
                                    delim = "-") {

  if (is.null(ctry.data$lab.data)) {
    message("No lab data attached.")
    return(NULL)
  }

  lab.data <- ctry.data$lab.data

  # Check for correct region
  if (!region %in% c("EMRO", "AFRO")) {
    stop("Valid regions are EMRO or AFRO. Please try again.")
  }

  lab.locs <- lab.locs |>
    mutate(country = str_to_upper(country))

  if (region == "EMRO") {

    emro.lab.01 <- lab.data |>
      dplyr::rename(country = "Name") |>
      #make country names long
      mutate(country = ifelse(country == "AFG", "AFGHANISTAN", country),
             country = ifelse(country == "BAH", "BAHRAIN", country),
             country = ifelse(country == "DJI", "DJIBOUTI", country),
             country = ifelse(country == "EGY", "EGYPT", country),
             country = ifelse(country == "IRN", "IRAN (ISLAMIC REPUBLIC OF)", country),
             country = ifelse(country == "IRQ", "IRAQ", country),
             country = ifelse(country == "JOR", "JORDAN", country),
             country = ifelse(country == "KUW", "KUWAIT", country),
             country = ifelse(country == "LEB", "LEBANON", country),
             country = ifelse(country == "LIB", "LIBYA", country),
             country = ifelse(country == "MOR", "MOROCCO", country),
             country = ifelse(country == "OMA", "OMAN", country),
             country = ifelse(country == "PAK", "PAKISTAN", country),
             country = ifelse(country == "PNA", "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM", country),
             country = ifelse(country == "QAT", "QATAR", country),
             country = ifelse(country == "SAA", "SAUDI ARABIA", country),
             country = ifelse(country == "SOM", "SOMALIA", country),
             country = ifelse(country == "SUD", "SUDAN", country),
             country = ifelse(country == "SYR", "SYRIAN ARAB REPUBLIC", country),
             country = ifelse(country == "TUN", "TUNISIA", country),
             country = ifelse(country == "UAE", "UNITED ARAB EMIRATES", country),
             country = ifelse(country == "YEM", "YEMEN", country)
      ) %>%
      mutate_at(
        c(
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
        ),
        as.Date.character, "%m/%d/%Y")


    #lab locations read in with AFRO cleaning script

    # clean lab data -
    # This is a very quick clean and can be improved upon with futher steps such as:
    #  - eliminating nonsensical dates
    #  - check for more duplicates (same epid and specimen number)
    #  - if a date is missing, replace it with a proxy date
    #  - clean/match all countries with lab loc df; I filtered both df to priority countries


    # de-dup
    emro.lab.02 <- emro.lab.01 |>
      dplyr::filter(dplyr::between(ParalysisOnsetDate, start.date, end.date)) |>
      dplyr::filter(country == ctry.data$ctry$ADM0_NAME) |>
      dplyr::distinct()

    # Additional cleaning steps
    # need data dictionary, in order to standardize names
    emro.lab.03 <- emro.lab.02 %>%
      # Dropping rows with Specimen number 0 or >2
      filter(SpecimenNumber %in% c(1,2)) %>%
      mutate(
        country = str_to_upper(country),
        year = ifelse(!is.na(ParalysisOnsetDate), year(ParalysisOnsetDate), YYYY),
        whoregion = "EMRO"
      )

    # Join lab locations
    emro.lab.04 <- full_join(
      emro.lab.03,
      lab.locs %>% filter(who.region == "EMRO") %>% select(country:num.ship.seq.samples),
      by = "country") %>%

      #count duplicates with same epid and specimen number
      #there should be 2 records for each EPID, specimen 1 and 2

      group_by(EPID, SpecimenNumber) %>%
      mutate(n= n()) %>%
      ungroup()

    #seperate blank epids from rest of emro.lab.04 in order to de dupe based on epid and specimen number, join back after dedup
    blank.epid <- emro.lab.04 %>%
      filter(is.na(EPID))

    emro.lab.04 <- emro.lab.04 %>%
      filter(!is.na(EPID)) %>%
      select(-n)

    emro.lab.04 <- emro.lab.04[!duplicated(emro.lab.04[c("EPID", "SpecimenNumber")]), ]

    # Create intervals (currently using subset of those I need for SC PPT)
    emro.lab.05 <- emro.lab.04 %>%
      mutate(
        # Intervals
        days.collect.lab = DateStoolReceivedinLab - DateStoolCollected,
        days.lab.culture = DateFinalCellCultureResult - DateStoolReceivedinLab,
        days.seq.ship = DateIsolateRcvdForSeq- ReportDateSequenceResultSent,
        days.lab.seq = DateofSequencing - DateStoolReceivedinLab,

        days.itd.seqres = DateofSequencing -DateFinalrRTPCRResults,
        days.itd.arriveseq = DateIsolateRcvdForSeq -DateFinalrRTPCRResults,
        days.seq.rec.res = DateofSequencing - DateIsolateRcvdForSeq,

        # Met target yes/no
        met.targ.collect.lab = ifelse(days.collect.lab<3, 1, 0),
        negative.spec = ifelse(!str_detect(FinalCellCultureResult, "ITD") & FinalITDResult=="NULL", 1, 0),
        met.lab.culture= ifelse(days.lab.culture<14, 1, 0),
      ) %>%
      #filtering out negative time intervals
      filter((days.collect.lab >= 0 | is.na(days.collect.lab)) &
               (days.lab.culture >= 0 | is.na(days.lab.culture)) &
                (days.seq.ship >= 0 | is.na(days.seq.ship)) &
               (days.lab.seq >= 0 | is.na(days.lab.seq)) &
               (days.itd.seqres >= 0 | is.na(days.itd.seqres)) &
              (days.itd.arriveseq >= 0 | is.na(days.itd.arriveseq)) &
              (days.seq.rec.res >= 0 | is.na(days.seq.rec.res))
      ) %>%
      #filtering out nonsensical dates
      # 1. stool can't be collected before Paralysis
      filter((DateStoolCollected >= ParalysisOnsetDate | is.na(ParalysisOnsetDate))) %>%
      mutate(seq.capacity = ifelse(seq.capacity == "yes", "Sequencing capacity", "No sequencing capacity")
      ) %>%
      select(-contains("cIntratypeIs"))

    lab.data <- emro.lab.05
    rm(emro.lab.01, emro.lab.02, emro.lab.03, emro.lab.04, emro.lab.05)
  } else if (region == "AFRO") {

    afro.lab.01b <- lab.data %>%
      mutate(
        DateStoolCollected = ifelse(DateStoolCollected == "NULL", NA, DateStoolCollected),
        StoolDateSentToLab = ifelse(StoolDateSentToLab == "NULL", NA, StoolDateSentToLab),
        DateStoolReceivedinLab = ifelse(DateStoolReceivedinLab == "NULL", NA, DateStoolReceivedinLab),
        DateFinalCellCultureResult = ifelse(
          DateFinalCellCultureResult == "NULL",
          NA,
          DateFinalCellCultureResult
        ),
        DateFinalrRTPCRResults = ifelse(DateFinalrRTPCRResults == "NULL", NA, DateFinalrRTPCRResults),
        ReportDateSequenceResultSent = ifelse(
          ReportDateSequenceResultSent == "NULL",
          NA,
          ReportDateSequenceResultSent
        ),
        DateIsolateRcvdForSeq = ifelse(DateIsolateRcvdForSeq == "NULL", NA, DateIsolateRcvdForSeq),
        DateLArmIsolate = ifelse(DateLArmIsolate == "NULL", NA, DateLArmIsolate),
        DateRArmIsolate = ifelse(DateRArmIsolate == "NULL", NA, DateRArmIsolate),
        DateofSequencing = ifelse(DateofSequencing == "NULL", NA, DateofSequencing),
        DateNotificationtoHQ = ifelse(DateNotificationtoHQ == "NULL", NA, DateNotificationtoHQ)) |>
      mutate_at(
        c(
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
        ),
        as.Date.character, "%m/%d/%Y")

    # This is a very quick clean and can be improved upon with futher steps such as:
    #  - eliminating nonsensical dates
    #  - check for more duplicates (same epid and specimen number)
    #  - if a date is missing, replace it with a proxy date
    #  - clean/match all countries with lab loc df; I filtered both df to priority countries


    # de-dup
    afro.lab.02 <- afro.lab.01b %>%
      dplyr::filter(dplyr::between(ParalysisOnsetDate, start.date, end.date)) |>
      distinct()


    # Additional cleaning steps
    afro.lab.03 <- afro.lab.02 %>%
      # Dropping rows with Specimen number 0 or >2
      filter(SpecimenNumber %in% c(1,2)) %>%
      # replacing "NULL" with NA
      # mutate_at(vars(DateStoolCollected:VDPV3), ~na_if(., "NULL")) %>%
      mutate(
        country = str_to_upper(Name),
        country = ifelse(str_detect(country, "IVOIRE"), "COTE D IVOIRE", country),
        year = year(ParalysisOnsetDate),
        whoregion = "AFRO"
      ) %>%
      filter(country == ctry.data$ctry$ADM0_NAME) %>%
      select(-Name)


    # Join lab locations
    afro.lab.04 <- full_join(
      afro.lab.03,
      lab.locs %>% select(country:num.ship.seq.samples),
      by = "country") %>%

      #count duplicates with same epid and specimen number
      #there should be 2 records for each EPID, specimen 1 and 2
      group_by(EPID, SpecimenNumber) %>%
      mutate(n= n()) %>%
      ungroup()


    afro.lab.04 <- afro.lab.04[!duplicated(afro.lab.04[c("EPID", "SpecimenNumber")]), ]

    # Create intervals (currently using subset of those I need for SC PPT)
    afro.lab.05 <- afro.lab.04 %>%
      select(-n) %>%
      mutate(
        # Intervals
        days.collect.lab = DateStoolReceivedinLab - DateStoolCollected,
        days.lab.culture = DateFinalCellCultureResult - DateStoolReceivedinLab,
        days.seq.ship = DateIsolateRcvdForSeq- ReportDateSequenceResultSent,
        days.lab.seq = DateofSequencing - DateStoolReceivedinLab,

        days.itd.seqres = DateofSequencing -DateFinalrRTPCRResults,
        days.itd.arriveseq = DateIsolateRcvdForSeq -DateFinalrRTPCRResults,
        days.seq.rec.res = DateofSequencing - DateIsolateRcvdForSeq,

        # Met target yes/no
        met.targ.collect.lab = ifelse(days.collect.lab<3, 1, 0),
        negative.spec = ifelse(!str_detect(FinalCellCultureResult, "ITD") & FinalITDResult=="NULL", 1, 0),
        met.lab.culture= ifelse(days.lab.culture<14, 1, 0),
      ) %>%
      #filtering out negative time intervals
      filter((days.collect.lab >= 0 | is.na(days.collect.lab)) &
               (days.lab.culture >= 0 | is.na(days.lab.culture)) &
               (days.seq.ship >= 0 | is.na(days.seq.ship)) &
               (days.lab.seq >= 0 | is.na(days.lab.seq)) &
               (days.itd.seqres >= 0 | is.na(days.itd.seqres)) &
               (days.itd.arriveseq >= 0 | is.na(days.itd.arriveseq)) &
               (days.seq.rec.res >= 0 | is.na(days.seq.rec.res))
      ) %>%
      #filtering out nonsensical dates
      # 1. stool can't be collected before Paralysis
      # 2. remove out of bounds years in Date final cell culture result
      filter((DateStoolCollected >= ParalysisOnsetDate | is.na(ParalysisOnsetDate)),
             (year(DateFinalCellCultureResult) <= 2023 | is.na(DateFinalCellCultureResult)),
             #remove a blank specimen row
             !is.na(EPID)) %>%
      #renaming culture.itd.lab for Nigeria which has two labs in lab.locs, simply naming Nigeria
      mutate(culture.itd.lab = ifelse(country == "NIGERIA", "Nigeria", culture.itd.lab),
             ParalysisOnsetDate = ymd(ParalysisOnsetDate),
             seq.capacity = ifelse(seq.capacity == "yes", "Sequencing capacity", "No sequencing capacity")
      ) %>%
      select(-contains("cIntratypeIs"))

    lab.data <- afro.lab.05
    rm(afro.lab.01b, afro.lab.02, afro.lab.03, afro.lab.04, afro.lab.05)
  }

  lab.data <- lab.data |>
    separate_wider_delim(
      cols = EPID,
      delim = "/",
      names = c("epid_ctry", "epid_prov", "epid_dist",
                "epid_04", "epid_05"),
      too_many = "debug",
      too_few = "debug"
    )

  geo_lookup_table <- ctry.data$afp.all.2 |>
    select(epid, matches("guid"), contains("$adm"), ctry, prov, dist, year) |>
    separate_wider_delim(
      cols = epid,
      delim = delim,
      names = c("epid_ctry", "epid_prov", "epid_dist",
                "epid_04", "epid_05"),
      too_many = "debug",
      too_few = "align_start"
    ) |>
    select(contains("epid"),
           ctry,
           prov,
           dist,
           matches("adm[0-3]guid"),
           year) |>
    distinct()

  lab.data <- lab.data |> left_join(geo_lookup_table, by = join_by(epid_ctry, epid_prov, epid_dist, year))
  lab.data <- lab.data |>
    rename(ctry.code2 = epid_ctry)
  lab.data <- lab.data |>
    mutate(CaseOrContact = "1-Case")
  lab.data <- lab.data |>
    rename(EpidNumber  = EPID)
  lab.data <- lab.data |>
    rename(District = dist, Province = prov)
  lab.data <- lab.data |>
    rename(DateOfOnset = CaseDate)

  return(lab.data)
}

#' Main lab data cleaning function. Automatically detects whether the dataset
#' came from WHO or the regional office
#'
#' @param ctry.data Rds file containing country polio data with lab.data attached
#' @param start_date start date of the desk review
#' @param end_date end date of the desk review
#' @param delim delimiter for EPIDs. Default is "-".
#' @param lab_locs_path location of testing lab locations. Default is NULL. Will download from EDAV, if necessary.
#'
#' @return a tibble containing the cleaned lab data
#' @export
clean_lab_data <- function(ctry.data, start_date, end_date, delim="-", lab_locs_path=NULL) {
  # Check if the lab data is attached
  if (is.null(ctry.data$lab.data)) {
    stop("Lab data not attached to ctry.data. Please attach and try again.")
  }

  # Variable to hold onto lab data
  lab.data <- NULL

  # Determine the type of cleaning to do
  lab.data.cols <- names(ctry.data$lab.data)

  if ("ctry.code2" %in% lab.data.cols) {
    lab.data <- clean_lab_data_who(ctry.data, start_date, end_date, delim)
  } else {
    lab.data <- clean_lab_data_regional(ctry.data, start_date, end_date, delim, lab_locs_path)
  }

  return(lab.data)
}


#' Generate timeliness intervals with lab data
#'
#' @param lab.data lab data
#' @param spatial.scale spatial scale to analyze the data. Valid values are "ctry", "prov", "dist"
#' @param start_date start date of analysis
#' @param end_date end date of analysis
#'
#' @return a table with timeliness data summary
#' @export
generate_lab_timeliness <-
  function(lab.data,
           spatial.scale,
           start.date,
           end.date) {
    geo <- switch(
      spatial.scale,
      "ctry" = "adm0guid",
      "prov" = "adm1guid",
      "dist" = "adm2guid"
    )

    lab1 <- lab.data |>
      filter(between(as.Date(DateOfOnset), start.date, end.date)) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.collect.lab, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.collect.lab") |>
      mutate(medi = as.numeric(medi))

    lab2 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start.date &
               as.Date(DateOfOnset) <= end.date) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.lab.culture, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.lab.culture") |>
      mutate(medi = as.numeric(medi))


    lab3 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start.date &
               as.Date(DateOfOnset) <= end.date) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.seq.ship, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.seq.ship") |>
      mutate(medi = as.numeric(medi))

    lab4 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start.date &
               as.Date(DateOfOnset) <= end.date) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.lab.seq, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.lab.seq") |>
      mutate(medi = as.numeric(medi))

    lab5 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start.date &
               as.Date(DateOfOnset) <= end.date) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.itd.seqres, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.itd.seqres") |>
      mutate(medi = as.numeric(medi))

    lab6 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start.date &
               as.Date(DateOfOnset) <= end.date) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.itd.arriveseq, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.itd.arriveseq") |>
      mutate(medi = as.numeric(medi))

    lab7 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start.date &
               as.Date(DateOfOnset) <= end.date) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.seq.rec.res, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.seq.rec.res") |>
      mutate(medi = as.numeric(medi))

    lab <- bind_rows(lab1, lab2, lab3, lab4, lab5, lab6, lab7)
    lab <- lab |> filter(!is.na(`get(geo)`))

    lab <- switch(spatial.scale,
                  "ctry" = {
                    lab <- lab |> rename(adm0guid = `get(geo)`)
                  },
                  "prov" = {
                    lab <- lab |> rename(adm1guid = `get(geo)`)
                  },
                  "dist" = {
                    lab <- lab |> rename(adm2guid = `get(geo)`)
                  }
                  )

    return(lab)
  }
