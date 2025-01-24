# Private functions ----

#' Impute missing geographic information from the AFP linelist
#'
#' @param lab_data `tibble` Lab data to clean.
#' @param afp_data `tibble` AFP data.
#'
#' @returns `tibble` Lab data set with imputed geographic columns based on the
#' AFP table.
#' @keywords internal
impute_missing_lab_geo <- function(lab_data, afp_data = NULL) {
  lab_data <- dplyr::rename_with(lab_data, recode,
    EPID = "EpidNumber"
  )
  lab_data <- lab_data |>
    tidyr::separate_wider_regex(
      cols = "EpidNumber",
      c(
        epid_ctry = ".*", "[-/]",
        epid_prov = ".*", "[-/]",
        epid_dist = ".*", "[-/]",
        epid_04 = ".*", "[-/]",
        epid_05 = ".*"
      ),
      names_repair = "check_unique",
      too_few = "align_start",
      cols_remove = F
    )

  if (!is.null(afp_data)) {
    afp_data <- dplyr::rename_with(afp_data, recode,
      place.admin.0 = "ctry",
      place.admin.1 = "prov",
      place.admin.2 = "dist",
      person.sex = "sex",
      dateonset = "date",
      yronset = "year",
      datenotify = "date.notify",
      dateinvest = "date.invest",
      cdc.classification.all = "cdc.class"
    )

    cli::cli_process_start("Obtaining geographic information based on matching EPIDs from the AFP linelist")
    lab_data$ctry <- afp_data$ctry[match(lab_data$EpidNumber, afp_data$epid)]
    lab_data$prov <- afp_data$prov[match(lab_data$EpidNumber, afp_data$epid)]
    lab_data$dist <- afp_data$dist[match(lab_data$EpidNumber, afp_data$epid)]

    lab_data$adm0guid <- afp_data$adm0guid[match(lab_data$EpidNumber, afp_data$epid)]
    lab_data$adm1guid <- afp_data$adm1guid[match(lab_data$EpidNumber, afp_data$epid)]
    lab_data$adm2guid <- afp_data$adm2guid[match(lab_data$EpidNumber, afp_data$epid)]
    cli::cli_process_done()

    # If these columns are available (in WHO lab)
    if ("Province" %in% names(lab_data)) {
      lab_data <- lab_data |>
        dplyr::mutate(
          prov = dplyr::if_else(is.na(prov),
            afp_data$prov[match(lab_data$Province, afp_data$prov)],
            prov
          )
        )
    }

    if ("District" %in% names(lab_data)) {
      lab_data <- lab_data |>
        dplyr::mutate(
          dist = dplyr::if_else(is.na(dist),
            afp_data$dist[match(lab_data$District, afp_data$dist)],
            dist
          )
        )
    }

    # Additional data cleaning steps
    geo_lookup_table <- afp_data |>
      dplyr::select(
        "epid", dplyr::matches("guid"),
        dplyr::contains("$adm"), "ctry", "prov", "dist", "year"
      ) |>
      tidyr::separate_wider_regex(
        cols = "epid",
        c(
          epid_ctry = ".*", "[-/]",
          epid_prov = ".*", "[-/]",
          epid_dist = ".*", "[-/]",
          epid_04 = ".*", "[-/]",
          epid_05 = ".*"
        ),
        too_few = "align_start"
      ) |>
      dplyr::select(
        "epid_ctry", "epid_prov", "epid_dist",
        "ctry", "prov", "dist",
        dplyr::matches("adm[0-3]guid"), "year"
      ) |>
      dplyr::distinct()

    # Geomatching algorithm
    cli::cli_process_start("Beginning geomatching based on AFP lookup table")

    # Imputing missing countries
    cli::cli_process_start("Imputing ctry and adm0guid")
    cli::cli_alert_info(paste0("Initial records missing ctry: ", sum(is.na(lab_data$ctry))))
    cli::cli_alert_info(paste0("Initial records missing adm0guid: ", sum(is.na(lab_data$adm0guid))))
    ctry_lookup_table <- geo_lookup_table |>
      dplyr::select("epid_ctry", "ctry", "adm0guid", "year") |>
      dplyr::distinct() |>
      tidyr::drop_na("ctry")

    # Check look up table for potential duplicated rows
    ctry_lookup_row_dups <- ctry_lookup_table |>
      dplyr::mutate(epid_comb = str_c(epid_ctry, year, sep = "-")) |>
      dplyr::group_by(epid_comb, epid_ctry, year) |>
      dplyr::summarise(n = n()) |>
      dplyr::filter(n > 1) |>
      ungroup()

    # Remove duplicates from the look up table
    ctry_lookup_row_dups <- ctry_lookup_row_dups |>
      dplyr::select(!dplyr::any_of(c("epid_comb", "n")))
    ctry_lookup_table <- dplyr::anti_join(
      ctry_lookup_table,
      ctry_lookup_row_dups
    )
    lab_data <- lab_data |>
      dplyr::left_join(ctry_lookup_table, by = dplyr::join_by(epid_ctry, year)) |>
      dplyr::mutate(
        ctry = dplyr::coalesce(ctry.x, ctry.y),
        adm0guid = dplyr::coalesce(adm0guid.x, adm0guid.y)
      )
    cli::cli_alert_info(paste0("Final records missing ctry: ", sum(is.na(lab_data$ctry))))
    cli::cli_alert_info(paste0("Final records missing adm0guid: ", sum(is.na(lab_data$adm0guid))))
    cli::cli_process_done()

    # Imputing missing provinces
    cli::cli_process_start("Imputing prov and adm1guid")
    cli::cli_alert_info(paste0("Initial records missing prov: ", sum(is.na(lab_data$prov))))
    cli::cli_alert_info(paste0("Initial records missing adm1guid: ", sum(is.na(lab_data$adm1guid))))
    prov_lookup_table <- geo_lookup_table |>
      dplyr::select("epid_ctry", "epid_prov", "ctry", "prov", "adm1guid", "year") |>
      dplyr::distinct() |>
      tidyr::drop_na("prov")

    # Check look up table for potential duplicated rows
    prov_lookup_row_dups <- prov_lookup_table |>
      dplyr::mutate(epid_comb = str_c(epid_ctry, epid_prov, ctry, year, sep = "-")) |>
      dplyr::group_by(epid_comb, epid_ctry, ctry, epid_prov, year) |>
      dplyr::summarise(n = n()) |>
      dplyr::filter(n > 1) |>
      ungroup()

    # Remove duplicates from the look up table
    prov_lookup_row_dups <- prov_lookup_row_dups |>
      dplyr::select(!dplyr::any_of(c("epid_comb", "n")))
    prov_lookup_table <- anti_join(prov_lookup_table, prov_lookup_row_dups)

    lab_data <- lab_data |>
      dplyr::left_join(prov_lookup_table, by = dplyr::join_by(epid_ctry, ctry, epid_prov, year)) |>
      dplyr::mutate(
        prov = dplyr::coalesce(prov.x, prov.y),
        adm1guid = dplyr::coalesce(adm1guid.x, adm1guid.y)
      )
    cli::cli_alert_info(paste0("Final records missing prov: ", sum(is.na(lab_data$prov))))
    cli::cli_alert_info(paste0("Final records missing adm1guid: ", sum(is.na(lab_data$adm1guid))))
    cli::cli_process_done()

    # Imputing district
    cli::cli_process_start("Imputing dist and adm2guid")
    cli::cli_alert_info(paste0("Initial records missing dist: ", sum(is.na(lab_data$dist))))
    cli::cli_alert_info(paste0("Initial records missing adm2guid: ", sum(is.na(lab_data$adm2guid))))
    dist_lookup_table <- geo_lookup_table |>
      dplyr::select("epid_ctry", "epid_prov", "epid_dist", "ctry", "prov", "dist", "adm2guid", "year") |>
      dplyr::distinct() |>
      tidyr::drop_na("dist")

    # Check look up table for potential duplicated rows
    dist_lookup_row_dups <- dist_lookup_table |>
      dplyr::mutate(epid_comb = str_c(epid_ctry, epid_prov, epid_dist, ctry, prov, year, sep = "-")) |>
      dplyr::group_by(
        epid_comb, epid_ctry, ctry,
        epid_prov, prov,
        epid_dist,
        year
      ) |>
      dplyr::summarise(n = n()) |>
      dplyr::filter(n > 1) |>
      ungroup()

    # Remove duplicates from the look up table
    dist_lookup_row_dups <- dist_lookup_row_dups |>
      dplyr::select(!dplyr::any_of(c("epid_comb", "n")))
    dist_lookup_table <- anti_join(dist_lookup_table, dist_lookup_row_dups)

    lab_data <- lab_data |>
      dplyr::left_join(dist_lookup_table,
        by = dplyr::join_by(epid_ctry, epid_prov, ctry, prov, epid_dist, year)
      ) |>
      dplyr::mutate(
        dist = dplyr::coalesce(dist.x, dist.y),
        adm2guid = dplyr::coalesce(adm2guid.x, adm2guid.y)
      ) |>
      dplyr::select(-dplyr::ends_with(".y"), -dplyr::ends_with(".x"))
    cli::cli_alert_info(paste0("Final records missing dist: ", sum(is.na(lab_data$dist))))
    cli::cli_alert_info(paste0("Final records missing adm2guid: ", sum(is.na(lab_data$adm2guid))))
    cli::cli_process_done()

    # check for correctness
    check <- lab_data |>
      dplyr::select(
        dplyr::starts_with("epid_"), dplyr::matches("adm[0-2]"),
        "ctry", "prov", "dist", "EpidNumber", "year"
      )
    mismatch_ctry <- dplyr::anti_join(check, ctry_lookup_table)
    mismatch_dist <- dplyr::anti_join(check, dist_lookup_table)
    mismatch_prov <- dplyr::anti_join(check, prov_lookup_table)

    cli::cli_process_done()

    # Message for values without any province or district information
  } else {
    cli::cli_alert_warning("AFP linelist not attached. Geographic columns will be empty.")
    lab_data$ctry <- NA
    lab_data$prov <- NA
    lab_data$dist <- NA
    lab_data$adm0guid <- NA
    lab_data$adm1guid <- NA
    lab_data$adm2guid <- NA
  }

  return(lab_data)
}

#' Clean polio lab data from WHO
#'
#' Cleans the lab data from WHO. This is used in [clean_lab_data()], but can be used on its own.
#'
#' @inheritParams clean_lab_data
#' @returns `tibble` Cleaned lab data.
#' @examples
#' \dontrun{
#' lab_path <- "C:/Users/XRG9/lab_data_who.csv"
#' ctry.data <- init_dr("algeria", lab_data_path = lab_path)
#' ctry.data$lab_data <- clean_lab_data_who(ctry.data, "2021-01-01", "2023-12-31")
#'
#' # Not using the desk review pipeline
#' raw.data <- get_all_polio_data()
#' ctry.data <- extract_country_data("algeria", raw.data)
#' ctry.data$lab_data <- read_csv(lab_path)
#' ctry.data$lab_data <- clean_lab_data_who(ctry.data, "2021-01-01", "2023-12-31")
#' }
#' @keywords internal
clean_lab_data_who <- function(lab_data, start_date, end_date,
                               afp_data = NULL, ctry_name = NULL) {
  # Static vars
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  if (is.na(start_date) | is.na(end_date)) {
    cli::cli_abort("start_date and end_date must be in YYYY-MM-DD format.")
    return(lab_data)
  }

  if (nrow(lab_data) == 0) {
    message("There are no entries for lab data.")
    return(NULL)
  }

  # Remove time portion of any date time columns
  cli::cli_process_start("Converting date/date-time character columns to date columns")
  lab_data <- lab_data |>
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("Date"),
      \(x) as.Date.character(x, tryFormats = c("%Y-%m-%d", "%Y/%m%/%d", "%m/%d/%Y"))
    ))
  cli::cli_process_done()

  # Check if timeliness columns are precalculated
  if (!"days.collect.lab" %in% names(lab_data)) {
    cli::cli_process_start("Generating timeliness columns")
    lab_data <- lab_data |>
      dplyr::mutate(
        # Intervals
        days.collect.lab = DateStoolReceivedinLab - DateStoolCollected,
        days.lab.culture = DateFinalCellCultureResults - DateStoolReceivedinLab,
        days.seq.ship = DateIsolateRcvdForSeq - DateSeqResultsEntered,
        days.lab.seq = DateSeqResult - DateStoolReceivedinLab,
        days.itd.seqres = DateSeqResult - DateFinalrRTPCRResults,
        days.itd.arriveseq = DateIsolateRcvdForSeq - DateFinalrRTPCRResults,
        days.seq.rec.res = DateSeqResult - DateIsolateRcvdForSeq,

        # Met target yes/no
        met.targ.collect.lab = ifelse(days.collect.lab < 3, 1, 0),
        negative.spec = ifelse(!stringr::str_detect(DateFinalCellCultureResults, "ITD") &
          FinalITDResult == "NULL", 1, 0),
        met.lab.culture = ifelse(days.lab.culture < 14, 1, 0),
      )
    cli::cli_process_done()
  }

  if (!"year" %in% names(lab_data)) {
    lab_data <- lab_data |>
      dplyr::mutate(year = lubridate::year(DateOfOnset))
  }

  cli::cli_process_start("Filtering for cases with valid dates")
  lab_data2 <- lab_data |>
    dplyr::filter((days.collect.lab >= 0 | is.na(days.collect.lab)) &
      (days.lab.culture >= 0 | is.na(days.lab.culture)) &
      (days.seq.ship >= 0 | is.na(days.seq.ship)) &
      (days.lab.seq >= 0 | is.na(days.lab.seq)) &
      (days.itd.seqres >= 0 | is.na(days.itd.seqres)) &
      (days.itd.arriveseq >= 0 | is.na(days.itd.arriveseq)) &
      (days.seq.rec.res >= 0 | is.na(days.seq.rec.res))) |>
    dplyr::filter(
      dplyr::between(
        year,
        lubridate::year(start_date), lubridate::year(end_date)
      ),
      CaseOrContact == "1-Case"
    )
  cli::cli_process_done()

  cli::cli_process_start("Imputing missing years")
  miss <- lab_data2 |> dplyr::filter(is.na(year))

  miss <- miss |>
    dplyr::mutate(year2 = substr(EpidNumber, 13, 14)) |>
    dplyr::mutate(year2 = as.numeric(paste0("20", year2)))

  lab_data2 <- lab_data2 |>
    dplyr::mutate(year = dplyr::case_when(
      is.na(year) ~ miss$year2[match(lab_data2$EpidNumber, miss$EpidNumber)],
      T ~ year
    ))
  cli::cli_process_done()

  cli::cli_process_start("Correcting district and province names.")
  lab_data2$Province <- stringr::str_to_upper(lab_data2$Province)
  lab_data2$District <- stringr::str_to_upper(lab_data2$District)
  lab_data2$Province <- iconv(lab_data2$Province, to = "ASCII//TRANSLIT")
  lab_data2$District <- iconv(lab_data2$District, to = "ASCII//TRANSLIT")

  # Match province and district by EPID number -
  lab_data2 <- impute_missing_lab_geo(lab_data2, afp_data)
  lab_data2 <- lab_data2 |>
    dplyr::mutate(whoregion = get_region(ctry))

  if (!is.null(ctry_name)) {
    ctry_name <- stringr::str_to_upper(stringr::str_trim(ctry_name))
    cli::cli_process_start("Filtering country-specific lab data")
    cli::cli_alert_warning(paste0(
      "NOTE: Filtering will include rows where ctry is",
      " N/A. Please review the dataset carefully after cleaning."
    ))
    lab_data2 <- lab_data2 |>
      dplyr::filter(ctry %in% ctry_name | is.na(ctry))
    cli::cli_process_done()
  }
  cli::cli_process_done()

  cli::cli_process_start("Adding additional timeliness subintervals")
  lab_data2 <- lab_data2 |>
    dplyr::mutate(
      days.coll.sent.field = as.numeric(DateStoolSentfromField - DateStoolCollected),
      days.sent.field.rec.nat = as.numeric(DateStoolReceivedNatLevel - DateStoolSentfromField),
      days.rec.nat.sent.lab = as.numeric(DateStoolSentToLab - DateStoolReceivedNatLevel),
      days.sent.lab.rec.lab = as.numeric(DateStoolReceivedinLab - DateStoolSentToLab),
      days.rec.lab.culture = as.numeric(DateFinalCellCultureResults - DateStoolReceivedinLab),
    )
  cli::cli_process_done()

  return(lab_data2)
}

#' Clean lab data from the regional offices.
#'
#' Regional data have different columns compared to the WHO lab data. The cleaning
#' of the regional data is adapted from the lab data cleaning code from the GPSAP
#' indicator script.
#'
#' @inheritParams clean_lab_data
#' @returns `tibble` Cleaned lab data.
#' @examples
#' \dontrun{
#' lab_path <- "C:/Users/XRG9/lab_data_region.csv"
#' ctry.data <- init_dr("algeria", lab_data_path = lab_path)
#' ctry.data$lab_data <- clean_lab_data_regional(
#'   lab.data, "2021-01-01", "2023-12-31",
#'   ctry.data$afp.all.2, "algeria"
#' )
#'
#' # Not using the desk review pipeline
#' raw.data <- get_all_polio_data()
#' ctry.data <- extract_country_data("algeria", raw.data)
#' ctry.data$lab_data <- read_csv(lab_path)
#' ctry.data$lab_data <- clean_lab_data_regional(ctry.data, "2021-01-01", "2023-12-31")
#' }
#' @keywords internal

clean_lab_data_regional <- function(lab_data,
                                    start_date, end_date,
                                    afp_data = NULL,
                                    ctry_name = NULL,
                                    lab_locs_path = NULL) {
  # Static vars
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  if (is.na(start_date) | is.na(end_date)) {
    cli::cli_abort("start_date and end_date must be in YYYY-MM-DD format.")
    return(lab_data)
  }

  lab_locs <- get_lab_locs(lab_locs_path)

  lab_data <- dplyr::rename_with(lab_data, recode,
    Name = "country"
  )

  lab_data <- lab_data |>
    dplyr::mutate(country = dplyr::case_match(
      country,
      "AFG" ~ "AFGHANISTAN",
      "BAH" ~ "BAHRAIN",
      "DJI" ~ "DJIBOUTI",
      "EGY" ~ "EGYPT",
      "IRN" ~ "IRAN (ISLAMIC REPUBLIC OF)",
      "IRQ" ~ "IRAQ",
      "JOR" ~ "JORDAN",
      "KUW" ~ "KUWAIT",
      "LEB" ~ "LEBANON",
      "LIB" ~ "LIBYA",
      "MOR" ~ "MOROCCO",
      "OMA" ~ "OMAN",
      "PAK" ~ "PAKISTAN",
      "PNA" ~ "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM",
      "QAT" ~ "QATAR",
      "SAA" ~ "SAUDI ARABIA",
      "SOM" ~ "SOMALIA",
      "SUD" ~ "SUDAN",
      "SYR" ~ "SYRIAN ARAB REPUBLIC",
      "TUN" ~ "TUNISIA",
      "UAE" ~ "UNITED ARAB EMIRATES",
      "YEM" ~ "YEMEN",
      .default = country
    ))

  cli::cli_process_start("Converting date character columns to date types.")
  lab_data <- lab_data |>
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
      )), \(x) as.Date.character(x,
        tryFormats = c(
          "%Y-%m-%d",
          "%Y/%m%/%d",
          "%m/%d/%Y"
        ),
        optional = T
      ))
    )
  cli::cli_process_done()

  cli::cli_process_start("Filtering to date range specified")
  lab_data <- lab_data |>
    dplyr::filter(dplyr::between(ParalysisOnsetDate, start_date, end_date))
  cli::cli_process_done()

  cli::cli_process_start("Deduplicating data")
  lab_data2 <- lab_data |>
    dplyr::distinct()

  # Additional cleaning steps
  # need data dictionary, in order to standardize names
  lab_data3 <- lab_data2 |>
    # Dropping rows with Specimen number 0 or >2
    dplyr::filter(SpecimenNumber %in% c(1, 2)) |>
    dplyr::mutate(
      country = stringr::str_to_upper(country),
      country = ifelse(stringr::str_detect(country, "IVOIRE"),
        "COTE D IVOIRE", country
      ),
      year = lubridate::year(ParalysisOnsetDate),
      whoregion = get_region(country)
    )

  lab_data4 <- lab_data3 |>
    dplyr::left_join(
      lab_locs |> dplyr::select("country":"num.ship.seq.samples")
    ) |>
    dplyr::group_by(EPID, SpecimenNumber) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::ungroup()

  lab_data4 <- lab_data4 |>
    dplyr::mutate(culture.itd.lab = dplyr::case_when(
      country == "NIGERIA" ~ "Nigeria",
      culture.itd.lab == "NOGUCHI- Ghana" ~ "NOGUCHI-Ghana",
      .default = culture.itd.lab
    ))

  # Seperate blank epids from rest of lab_data4 in order to de-dupe
  # based on epid and specimen number, join back after dedup
  blank_epid <- lab_data4 |>
    dplyr::filter(is.na(EPID))

  lab_data4 <- lab_data4 |>
    dplyr::filter(!is.na(EPID)) |>
    dplyr::select(-"n")

  lab_data4 <- lab_data4[!duplicated(lab_data4[c("EPID", "SpecimenNumber")]), ]
  cli::cli_process_done()

  # Create intervals (currently using subset of those I need for SC PPT)
  cli::cli_process_start("Creating timeliness interval columns")
  lab_data5 <- lab_data4 |>
    dplyr::mutate(
      # Intervals
      days.collect.lab = DateStoolReceivedinLab - DateStoolCollected,
      days.lab.culture = DateFinalCellCultureResult - DateStoolReceivedinLab,
      days.seq.ship = DateIsolateRcvdForSeq - ReportDateSequenceResultSent,
      days.lab.seq = DateofSequencing - DateStoolReceivedinLab,
      days.itd.seqres = DateofSequencing - DateFinalrRTPCRResults,
      days.itd.arriveseq = DateIsolateRcvdForSeq - DateFinalrRTPCRResults,
      days.seq.rec.res = DateofSequencing - DateIsolateRcvdForSeq,

      # Met target yes/no
      met.targ.collect.lab = ifelse(days.collect.lab < 3, 1, 0),
      negative.spec = ifelse(!str_detect(FinalCellCultureResult, "ITD") &
        FinalITDResult == "NULL", 1, 0),
      met.lab.culture = ifelse(days.lab.culture < 14, 1, 0),
    )
  cli::cli_process_done()

  cli::cli_process_start("Filtering out negative time intervals")
  lab_data5 <- lab_data5 |>
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
  lab_data5 <- lab_data5 |>
    dplyr::filter(
      (DateStoolCollected >= ParalysisOnsetDate | is.na(ParalysisOnsetDate)),
      # (lubridate::year(DateFinalCellCultureResult) <= 2023 | is.na(DateFinalCellCultureResult)),
      # remove a blank specimen row
      !is.na(EPID)
    ) |>
    dplyr::mutate(
      seq.capacity = ifelse(seq.capacity == "yes",
        "Sequencing capacity",
        "No sequencing capacity"
      ),
      culture.itd.lab = ifelse(country == "NIGERIA",
        "Nigeria", culture.itd.lab
      ),
    ) |>
    dplyr::select(-dplyr::contains("cIntratypeIs"))
  cli::cli_process_done()

  lab_data <- lab_data5
  rm(lab_data2, lab_data3, lab_data4, lab_data5)

  cli::cli_process_start("Correcting district and province names.")
  lab_data <- impute_missing_lab_geo(lab_data, afp_data)
  cli::cli_process_done()

  # Filter to only the country of interest
  if (!is.null(ctry_name)) {
    ctry_name <- stringr::str_trim(stringr::str_to_upper(ctry_name))
    # Recode for COTE D'IVOIRE
    ctry_name <- dplyr::if_else(stringr::str_detect(ctry_name, "(?i)IVOIRE"),
      "COTE D'IVOIRE", ctry_name
    )
    cli::cli_process_start("Filtering country-specific lab data")
    cli::cli_alert_warning(paste0(
      "NOTE: Filtering will include rows where ctry is",
      " N/A. Please review the dataset carefully after cleaning."
    ))
    lab_data <- lab_data |>
      dplyr::filter(ctry %in% ctry_name | is.na(ctry))
    cli::cli_process_done()
  }

  # adding additional subintervals (these aren't present in regional lab data, so are created as dummy variables)
  lab_data <- lab_data |>
    mutate(
      days.coll.sent.field = NA,
      days.sent.field.rec.nat = NA,
      days.rec.nat.sent.lab = NA,
      days.sent.lab.rec.lab = NA,
      days.rec.lab.culture = NA,
    )

  return(lab_data)
}



# Public functions ----
#' Table of information regarding testing labs in each country
#'
#' Imports information on testing labs for each country, either from a CSV file
#' or downloaded from EDAV. If no argument is passed, the function will download
#' the table from EDAV.
#'
#' @param path `str` Path to the lab location file. Defaults to `NULL`.
#'
#' @returns `tibble` A table containing the test lab location information.
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
      dplyr::mutate(country = stringr::str_to_upper(country))
  }

  return(lab.locs)
}

#' Determines whether lab data is EMRO or AFRO
#'
#' Outputs the name of the region which a country belongs to.
#'
#' @param country_name `str` Name of the country.
#'
#' @returns `str` A string, either `"EMRO"` or `"AFRO"`.
#' @examples
#' get_region("algeria")
#'
#' @export
get_region <- function(country_name = Sys.getenv("DR_COUNTRY")) {
  # Format country_name
  country_name <- stringr::str_trim(stringr::str_to_upper(country_name))

  # Countries that belong in a region
  afro_ctry <- c(
    "CHAD", "ANGOLA", "BENIN", "NIGERIA", "ALGERIA", "GUINEA", "CAMEROON",
    "KENYA", "BURKINA FASO", "MOZAMBIQUE", "ETHIOPIA",
    "SOUTH AFRICA", "SENEGAL", "MADAGASCAR", "CENTRAL AFRICAN REPUBLIC",
    "BURUNDI", "CONGO", "UNITED REPUBLIC OF TANZANIA", "CABO VERDE", "NIGER",
    "MALAWI", "SOUTH SUDAN", "LIBERIA", "TOGO", "UGANDA", "BOTSWANA", "ZAMBIA",
    "MAURITANIA", "GABON", "ERITREA", "GUINEA-BISSAU", "LESOTHO", "NAMIBIA",
    "SIERRA LEONE", "ZIMBABWE", "EQUATORIAL GUINEA", "MAURITIUS", "RWANDA",
    "ESWATINI", "COTE D'IVOIRE", "COTE D IVOIRE",
    "DEMOCRATIC REPUBLIC OF THE CONGO", "GHANA", "GAMBIA", "MALI",
    "SEYCHELLES"
  )

  amro_ctry <- c(
    "ARGENTINA", "BOLIVIA (PLURINATIONAL STATE OF)", "BRAZIL", "CUBA",
    "DOMINICAN REPUBLIC", "EL SALVADOR", "MEXICO", "NICARAGUA", "PERU",
    "VENEZUELA (BOLIVARIAN REPUBLIC OF)", "CHILE", "HAITI", "HONDURAS",
    "PARAGUAY", "BARBADOS", "COLOMBIA", "ECUADOR", "UNITED STATES OF AMERICA",
    "BELIZE", "JAMAICA", "GUATEMALA", "CANADA", "COSTA RICA", "GUYANA", "PANAMA",
    "URUGUAY", "TURKS AND CAICOS ISLANDS", "SURINAME", "GRENADA",
    "TRINIDAD AND TOBAGO", "SAINT VINCENT AND THE GRENADINES", "ANGUILLA",
    "FRENCH GUIANA"
  )

  emro_ctry <- c(
    "EGYPT", "AFGHANISTAN", "PAKISTAN", "IRAN (ISLAMIC REPUBLIC OF)",
    "KUWAIT", "SYRIAN ARAB REPUBLIC", "MOROCCO", "IRAQ", "YEMEN",
    "SOMALIA", "BAHRAIN", "LEBANON",
    "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM",
    "QATAR", "SUDAN", "SAUDI ARABIA", "UNITED ARAB EMIRATES",
    "DJIBOUTI", "JORDAN"
  )

  euro_ctry <- c(
    "SPAIN", "AZERBAIJAN", "ARMENIA", "BELARUS", "GEORGIA", "KYRGYZSTAN",
    "KAZAKHSTAN", "NORTH MACEDONIA", "POLAND", "RUSSIAN FEDERATION",
    "TAJIKISTAN", "TURKMENISTAN", "UKRAINE", "UZBEKISTAN", "ISRAEL", "ITALY",
    "SERBIA", "AUSTRIA", "BOSNIA AND HERZEGOVINA", "CZECHIA", "GREECE", "LATVIA",
    "SLOVAKIA", "ESTONIA", "SLOVENIA", "NORWAY", "ROMANIA", "CROATIA", "LITHUANIA",
    "PORTUGAL", "TÜRKIYE", "ALBANIA", "REPUBLIC OF MOLDOVA", "SWITZERLAND",
    "HUNGARY", "BULGARIA", "MONTENEGRO", "TURKEY",
    "THE UNITED KINGDOM", "GERMANY", "FINLAND"
  )

  searo_ctry <- c(
    "BANGLADESH", "MYANMAR", "BHUTAN", "SRI LANKA", "INDIA",
    "DEMOCRATIC PEOPLE'S REPUBLIC OF KOREA", "NEPAL", "THAILAND",
    "TIMOR-LESTE", "INDONESIA", "MALDIVES"
  )

  wpro_ctry <- c(
    "AUSTRALIA", "FIJI", "REPUBLIC OF KOREA", "LAO PEOPLE'S DEMOCRATIC REPUBLIC",
    "MALAYSIA", "PHILIPPINES", "TONGA", "CHINA", "SOLOMON ISLANDS", "CAMBODIA",
    "NEW ZEALAND", "PAPUA NEW GUINEA", "VIET NAM", "BRUNEI DARUSSALAM",
    "MONGOLIA", "JAPAN", "SAMOA", "MICRONESIA (FEDERATED STATES OF)", "TUVALU",
    "SINGAPORE", "NEW CALEDONIA"
  )

  region <- dplyr::case_match(
    country_name,
    emro_ctry ~ "EMRO",
    afro_ctry ~ "AFRO",
    amro_ctry ~ "AMRO",
    euro_ctry ~ "EURO",
    wpro_ctry ~ "WPRO",
    searo_ctry ~ "SEARO",
    .default = NA
  )

  return(region)
}


#' Function to load the raw lab data
#'
#' This a function to load lab data that are either CSVs or Excel files.
#'
#' @param lab_data_path `str` File path as a string to the lab data.
#' @param sheet_name `str` Name of the sheet to load. This is optional in cases
#' of an Excel sheet with multiple tabs.
#'
#' @returns `tibble` Lab data loaded from the CSV or Excel file path.
#' @examples
#' \dontrun{
#' lab_data_path <- "C:/Users/ABC1/Desktop/lab_data.csv"
#' lab_data <- load_lab_data(lab_data_path)
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
#' lab_data_errors(ctry.data$lab.data, ctry.data$afp.data)
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
      details = paste0(
        "Using ctry.data will be deprecated in the next release",
        "Please pass lab data directly to lab.data."
      )
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
    lab_data_errors_who(lab.data, afp.data, ctry_name, start.date, end.date)
  } else {
    lab_data_errors_region(lab.data, afp.data, ctry_name, start.date, end.date)
  }
}

#' Check common errors in the regional lab data.
#'
#' Error checking for regional lab data. This is a helper function meant to be used inside
#' [lab_data_errors()].
#'
#' @inheritParams lab_data_errors
#'
#' @param ctry.data `list` Large list containing polio country data. Output of either
#' [extract_country_data()] or [init_dr()]. Note that lab data must be attached to`ctry.data` as
#' `lab_data`.
#' @param ctry_name `str` Name of the country. Defaults to the desk review country.
#' @param start.date `str` Start date of analysis.
#' @param end.date `str` End date of analysis.
#' @param error_path `str` Path to folder to save the error log.
#'
#' @returns None. It outputs an Excel file locally containing the error log.
#' @examples
#' \dontrun{
#' lab_path <- "C:/Users/XRG9/lab_data_region.csv"
#' ctry.data <- init_dr("algeria", lab_data_path = lab_path)
#' lab_data_errors_region(
#'   ctry.data$lab.data, ctry.data$afp.all.2,
#'   "2021-01-01", "2023-12-31"
#' )
#' }
#' @keywords internal
lab_data_errors_region <- function(lab.data,
                                   afp.data,
                                   start.date, end.date,
                                   ctry_name = Sys.getenv("DR_COUNTRY"),
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


  # Filter to only the country of interest
  lab_data <- lab_data |>
    dplyr::filter(Name == ctry_name)

  # Cleaning for Cote D'Ivoire
  if (stringr::str_detect(ctry_name, "(?i)IVIORE")) {
    lab_data <- lab_data |>
      dplyr::mutate(Name = dplyr::if_else(stringr::str_detect(Name, "(?i)IVOIRE"), "COTE D'IVIORE", Name))
  }

  # Converting character dates to date columns
  lab_data <- lab_data |>
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
      dplyr::mutate(country = dplyr::if_else(stringr::str_detect(
        country,
        "(?i)IVOIRE"
      ),
      "COTE D'IVOIRE", country
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
      country = stringr::str_to_upper(country),
      year = lubridate::year(ParalysisOnsetDate),
      whoregion = get_region(country)
    )

  # Join lab locations
  duplicate.04 <- duplicate.03 |>
    # count duplicates with same EPID and specimen number
    # there should be 2 records for each EPID, specimen 1 and 2
    dplyr::group_by(EPID, SpecimenNumber) |>
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
  lab_data <- lab_data |>
    dplyr::mutate(
      # Intervals
      days.collect.lab = DateStoolReceivedinLab - DateStoolCollected,
      days.lab.culture = DateFinalCellCultureResult - DateStoolReceivedinLab,
      days.seq.ship = DateIsolateRcvdForSeq - ReportDateSequenceResultSent,
      days.lab.seq = DateofSequencing - DateStoolReceivedinLab,
      days.itd.seqres = DateofSequencing - DateFinalrRTPCRResults,
      days.itd.arriveseq = DateIsolateRcvdForSeq - DateFinalrRTPCRResults,
      days.seq.rec.res = DateofSequencing - DateIsolateRcvdForSeq,

      # Met target yes/no
      met.targ.collect.lab = ifelse(days.collect.lab < 3, 1, 0),
      negative.spec = ifelse(!str_detect(FinalCellCultureResult, "ITD") & FinalITDResult == "NULL", 1, 0),
      met.lab.culture = ifelse(days.lab.culture < 14, 1, 0),
    )

  invalid_intervals <- lab_data |>
    # filtering out negative time intervals
    dplyr::filter((days.collect.lab < 0) |
      (days.lab.culture < 0) |
      (days.seq.ship < 0) |
      (days.lab.seq < 0) |
      (days.itd.seqres < 0) |
      (days.itd.arriveseq < 0) |
      (days.seq.rec.res < 0))

  if (nrow(invalid_intervals) > 0) {
    cli::cli_alert_warning(paste0(
      "There are ", nrow(invalid_intervals),
      " records with negative intervals."
    ))
  } else {
    cli::cli_alert_success("No invalid intervals found.")
  }

  cli::cli_process_done()

  cli::cli_process_start("Checking records where stool collection date is before paralysis.")
  collection_before_paralysis <- lab_data |>
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
  missing_epids <- lab.data |> dplyr::filter(!(EPID %in% afp.data$epid))


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
#' @inheritParams lab_data_errors
#' @examples
#' \dontrun{
#' lab_path <- "C:/Users/XRG9/lab_data_who.csv"
#' ctry.data <- init_dr("algeria", lab_data_path = lab_path)
#' lab_data_errors_who(
#'   ctry.data$lab.data, ctry.data$afp.all.2,
#'   "2021-01-01", "2023-12-31"
#' )
#' }
#' @keywords internal
lab_data_errors_who <- function(lab.data, afp.data,
                                start.date, end.date,
                                ctry_name = Sys.getenv("DR_COUNTRY"),
                                error_path = Sys.getenv("DR_ERROR_PATH"),
                                ctry.data = lifecycle::deprecated()) {
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

  cli::cli_process_start("Filtering to the country of interest")
  lab.data <- suppressMessages(impute_missing_lab_geo(lab.data, afp.data))
  lab.data <- lab.data |> dplyr::filter(ctry %in% ctry_name)
  cli::cli_process_done()

  cli::cli_process_start("Filtering for years of interest and cases")
  lab.data <- lab.data |>
    dplyr::filter(
      dplyr::between(year, lubridate::year(start.date), lubridate::year(end.date)),
      CaseOrContact == "1-Case"
    )
  cli::cli_process_done()


  cli::cli_process_start("Checking for invalid dates from cases.")
  invalid_dates <- lab_data |>
    dplyr::filter((days.collect.lab < 0) &
      (days.lab.culture < 0) &
      (days.seq.ship < 0) &
      (days.lab.seq < 0) &
      (days.itd.seqres < 0) &
      (days.itd.arriveseq < 0) &
      (days.seq.rec.res < 0))

  if (nrow(invalid_dates) != 0) {
    cli::cli_alert_warning(paste0(
      "There are ", nrow(invalid_dates),
      " cases with invalid dates."
    ))
  } else {
    cli::cli_alert_success("No invalid dates detected.")
  }

  cli::cli_process_done()

  cli::cli_process_start("Checking for missing years")
  missing_years <- lab_data |>
    dplyr::filter(is.na(year)) |>
    dplyr::filter(
      dplyr::between(year, lubridate::year(start.date), lubridate::year(end.date)),
      CaseOrContact == "1-Case"
    )

  if (nrow(missing_years) != 0) {
    cli::cli_alert_warning(paste0(
      "There are ", nrow(missing_years),
      " cases with missing years."
    ))
  } else {
    cli::cli_alert_success("No cases with missing years.")
  }
  cli::cli_process_done()

  cli::cli_process_start("Checking for missing EPIDs in the AFP dataset.")
  missing_epids <- lab.data |> dplyr::filter(!(EpidNumber %in% afp.data$epid))


  if (nrow(missing_epids) != 0) {
    cli::cli_alert_warning(paste0(
      "There are ", nrow(missing_epids),
      " lab cases not in the AFP linelist."
    ))
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

#' Clean lab data
#'
#' Main lab data cleaning function. Automatically detects whether the dataset
#' came from WHO or the regional office.
#'
#' @param lab_data `tibble` Lab dataset.
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis.
#' @param afp_data `tibble` AFP linelist. Either `ctry.data$afp.all.2` or `raw.data$afp`.
#' @param ctry_name `str` or `list` Name or a list of countries. Defaults to `NULL`.
#' @param lab_locs_path `str` Location of testing lab locations. Default is `NULL`. Will download from EDAV, if necessary.
#' @returns `tibble` Cleaned lab data.
#' @examples
#' \dontrun{
#' lab_path <- "C:/Users/XRG9/lab_data_who.csv"
#' ctry.data <- init_dr("algeria", lab_data_path = lab_path)
#' ctry.data$lab_data <- clean_lab_data(ctry.data, "2021-01-01", "2023-12-31")
#'
#' # Not using the desk review pipeline
#' raw.data <- get_all_polio_data()
#' ctry.data <- extract_country_data("algeria", raw.data)
#' ctry.data$lab_data <- read_csv(lab_path)
#' ctry.data$lab_data <- clean_lab_data(
#'   ctry.data$lab.data, "2021-01-01", "2023-12-31",
#'   ctry.data$afp.all.2, "algeria"
#' )
#' }
#' @export
clean_lab_data <- function(lab_data, start_date, end_date,
                           afp_data = NULL, ctry_name = NULL,
                           lab_locs_path = NULL) {
  # Determine the type of cleaning to do
  lab_data_cols <- names(lab_data)

  if ("MasterKey" %in% lab_data_cols) {
    if ("prov" %in% names(lab_data)) {
      cli::cli_alert_warning("Lab data already cleaned.")
      return(lab_data)
    }

    if (nrow(lab_data) == 0) {
      message("There are no entries for lab data.")
      return(NULL)
    }

    lab_data <- clean_lab_data_who(
      lab_data, start_date, end_date,
      afp_data, ctry_name
    )
    # lab_data <- add_rolling_years(lab_data, start_date, "DateOfOnset")
  } else {
    if ("prov" %in% lab_data_cols) {
      cli::cli_alert_warning("Lab data already cleaned.")
      return(lab_data)
    }
    lab_data <- clean_lab_data_regional(
      lab_data, start_date, end_date,
      afp_data, ctry_name, lab_locs_path
    )
    # lab_data <- add_rolling_years(lab_data, start_date, "CaseDate")
  }

  return(lab_data)
}


#' Summary of lab sample timeliness
#'
#' Generates a summary of the timeliness of samples for specific intervals.
#'
#' @param lab_data `tibble` Lab data. Ensure that this lab data is cleaned using
#' [clean_lab_data()] before running the function.
#' @param spatial.scale `str` Spatial scale to analyze the data. Valid values are `"ctry", "prov", "dist"`.
#' @param start.date `str` Start date of analysis.
#' @param end.date `str` End date of analysis.
#'
#' @returns `tibble` A table with timeliness data summary.
#' @examples
#' \dontrun{
#' lab_path <- "C:/Users/XRG9/lab_data_who.csv"
#' ctry.data <- init_dr("algeria", lab_data_path = lab_path)
#' ctry.data$lab_data <- clean_lab_data(ctry.data, "2021-01-01", "2023-12-31")
#' lab.timeliness.ctry <- generate_lab_timeliness(ctry.data$lab_data, "ctry", start_date, end_date)
#' }
#' @export
generate_lab_timeliness <-
  function(lab_data,
           spatial.scale,
           start.date,
           end.date) {
    spatial_groupby <- switch(spatial.scale,
      "ctry" = c("year", "ctry", "adm0guid"),
      "prov" = c("year", "ctry", "prov", "adm1guid")
    )


    start.date <- lubridate::as_date(start.date)
    end.date <- lubridate::as_date(end.date)

    # Check if the lab data is attached
    if (is.null(lab_data)) {
      stop("Lab data not attached. Please attach and try again.")
    }

    lab_data <- dplyr::rename_with(lab_data, recode,
      CaseDate = "DateOfOnset"
    )

    lab_medians <- lab_data |>
      dplyr::filter(dplyr::between(as.Date(DateOfOnset), start.date, end.date)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(spatial_groupby))) |>
      dplyr::summarise(dplyr::across(
        dplyr::starts_with("days."),
        \(x) as.numeric(median(x, na.rm = T))
      )) |>
      tidyr::pivot_longer(
        cols = dplyr::starts_with("days."),
        names_to = "type", values_to = "medi"
      )
    lab_counts <- lab_data |>
      dplyr::filter(dplyr::between(as.Date(DateOfOnset), start.date, end.date)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(spatial_groupby))) |>
      dplyr::summarise(dplyr::across(
        dplyr::starts_with("days."),
        \(x) sum(!is.na(x))
      )) |>
      tidyr::pivot_longer(
        cols = dplyr::starts_with("days."),
        names_to = "type", values_to = "freq"
      )

    lab <- dplyr::full_join(lab_counts, lab_medians)

    return(lab)
  }
