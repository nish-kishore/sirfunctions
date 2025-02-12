# Private functions ----

#' Find established sites
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Finds established ES sites defined as those that has been active for for
#' at least 12 months since the end date with at least 10 collections.
#'
#'
#' @param es_data `tibble` ES data
#' @param end_date `str` End date to anchor analysis to
#' @return `tibble` A summary of established ES sites
#' @keywords internal
get_es_site_age <- function(es_data, end_date) {
  end_date <- lubridate::as_date(end_date)

  established_sites <- es_data |>
    dplyr::select(ADM0_NAME, collect.date, site.name) |>
    dplyr::filter(collect.date <= end_date) |>
    dplyr::group_by(ADM0_NAME, site.name) |>
    # n_samples_12_mo is strictly looking at samples collected in the past 12 months
    # this will be different from num.samples in the f.ev.rate.01() calculation
    # if that calculation does not analyze a 12 month rolling period
    dplyr::summarize(n_samples_12_mo = sum(dplyr::between(collect.date,
                                                          end_date %m-% months(12, FALSE) %m+% days(1),
                                                          end_date)),
                     sampling_interval = lubridate::interval(min(collect.date),
                                                    max(collect.date)),
                     site_age = sampling_interval / months(1, FALSE)
                     ) |>
    dplyr::ungroup() |>
    dplyr::arrange(ADM0_NAME, site.name)

  return(established_sites)

  }



#' Add the risk category based on the country
#'
#' @param df `tibble` Dataframe with at least a column for country
#' @param risk_table  `tibble` Risk category table for each country. Defaults to `NULL`.
#' When set to `NULL`, attempts to download the risk category table from EDAV.
#'
#' @return `tibble` A dataframe with risk category columns added.
#' @keywords internal
#'
add_risk_category <- function(df, risk_table = NULL, ctry_col = "ctry") {
  if (is.null(risk_table)) {
    risk_table <- suppressMessages(edav_io("read", file_loc = get_constant("CTRY_RISK_CAT")))
  }

  cli::cli_process_start("Adding country risk categories")
  risk_table <- risk_table |>
    mutate(Country = if_else(.data$Country == "TÜRKIYE", "TURKEY", .data$Country))

  df <- df |>
    dplyr::left_join(risk_table, by = setNames("Country", ctry_col))
  cli::cli_process_done()
  return(df)
}

#' Title
#'
#' @param df `tibble` Dataset with at least a country column to join to.
#' @param lab_locs `tibble` Lab testing information table. Defaults to `NULL`. If
#' `NULL`, attempts to download the table from EDAV.
#'
#' @returns `tibble` A dataset with country lab information attached.
#' @keywords internal
add_seq_capacity <- function(df, ctry_col = "ctry", lab_locs = NULL) {
  if (is.null(lab_locs)) {
    lab_locs <- suppressMessages(get_lab_locs())
  }

  cli::cli_process_start("Adding country lab information")
  df <- df |>
    dplyr::left_join(
      lab_locs |>
        dplyr::filter(!is.na(.data$country)) |>
        dplyr::select("country":"num.ship.seq.samples"),
      by = setNames("country", ctry_col)
    )
  cli::cli_process_done()
  return(df)
}

#' Generate timeliness columns
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The function generates timeliness columns in the positives dataset.
#' @details
#' This function is used in [generate_c1_table()] and [generate_timely_det_violin()].
#'
#' @inheritParams generate_wild_vdpv_summary
#'
#' @return `tibble` Columns added
#' @keywords internal
generate_pos_timeliness <- function(raw_data, start_date, end_date,
                                        risk_table = NULL, lab_locs = NULL) {

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  # Filtering
  afp_data <- raw_data$afp |>
    dplyr::filter(
      dplyr::between(dateonset, start_date, end_date),
      cdc.classification.all2 != "NOT-AFP"
    )
  es_data <- raw_data$es |>
    dplyr::filter(
      dplyr::between(collect.date, start_date, end_date),
      ev.detect == 1
    )

  # Subsetting
  afp_data <- afp_data |>
    dplyr::select(dplyr::any_of(c(
      "epid", "place.admin.0", "whoregion",
      "cdc.classification.all2", "dateonset",
      "datenotificationtohq"
    ))) |>
    dplyr::mutate(source = "AFP")

  es_data <- es_data |>
    dplyr::select(dplyr::any_of(c(
      "env.sample.id", "ADM0_NAME", "who.region",
      "virus.type",
      "collect.date", "date.notification.to.hq"
    ))) |>
    dplyr::mutate(source = "ENV")

  pos <- dplyr::full_join(afp_data, es_data,
                          by = c(
                            "epid" = "env.sample.id",
                            "place.admin.0" = "ADM0_NAME",
                            "whoregion" = "who.region",
                            "cdc.classification.all2" = "virus.type",
                            "dateonset" = "collect.date",
                            "datenotificationtohq" = "date.notification.to.hq",
                            "source"
                          )
  )

  # Adding required columns
  pos <- add_risk_category(pos, risk_table, ctry_col = "place.admin.0")
  pos <- add_rolling_years(pos, start_date, "dateonset")
  pos <- add_seq_capacity(pos, ctry_col = "place.admin.0", lab_locs)

  pos_summary <- pos |>
    dplyr::mutate(
      ontonothq = as.numeric(lubridate::as_date(.data$datenotificationtohq) -
                               .data$dateonset),
      timely_cat =
        case_when(stringr::str_detect(.data$seq.capacity, "[Yy]es") & ontonothq <= 35 ~ "<=35 days from onset",
                  stringr::str_detect(.data$seq.capacity, "[Yy]es") & ontonothq > 35 ~ ">35 days from onset",
                  seq.capacity == "no" & ontonothq <= 46 ~ "<=46 days from onset",
                  seq.capacity == "no" & ontonothq > 46 ~ ">46 days from onset",
                  is.na(ontonothq) | ontonothq < 0 ~ "Missing or bad data",
                  .default = NA
        ),
      is_timely = dplyr::if_else(.data$timely_cat %in%
                                   c(
                                     "<=35 days from onset",
                                     "<=46 days from onset"
                                   ), TRUE, FALSE),
      is_target = dplyr::if_else(
        stringr::str_detect(.data$cdc.classification.all2, "WILD|VDPV"),
        TRUE, FALSE
      )
    )

  return(pos_summary)

}

#' Generates a summary table regarding wild and VDPV cases
#'
#' @inheritParams generate_c1_table
#' @param .group_by How to group the results by.
#'
#' @return `tibble` Summary of wild and VDPV cases
#' @keywords internal
generate_wild_vdpv_summary <- function(raw_data, start_date, end_date,
                                       risk_table = NULL, lab_locs = NULL,
                                       .group_by = c(
                                         "whoregion",
                                         "SG Priority Level",
                                         "place.admin.0",
                                         "rolling_period",
                                         "year_label",
                                         "analysis_year_start",
                                         "analysis_year_end"
                                       )) {
  pos <- generate_pos_timeliness(raw_data, start_date, end_date,
                                             risk_table, lab_locs)
  pos_summary <- pos |>
    dplyr::mutate(
      ontonothq = as.numeric(lubridate::as_date(.data$datenotificationtohq) -
        .data$dateonset),
      timely_cat =
        case_when(stringr::str_detect(.data$seq.capacity, "[Yy]es") & ontonothq <= 35 ~ "<=35 days from onset",
                  stringr::str_detect(.data$seq.capacity, "[Yy]es") & ontonothq > 35 ~ ">35 days from onset",
          seq.capacity == "no" & ontonothq <= 46 ~ "<=46 days from onset",
          seq.capacity == "no" & ontonothq > 46 ~ ">46 days from onset",
          is.na(ontonothq) | ontonothq < 0 ~ "Missing or bad data",
          .default = NA
        ),
      is_timely = dplyr::if_else(.data$timely_cat %in%
        c(
          "<=35 days from onset",
          "<=46 days from onset"
        ), TRUE, FALSE),
      is_target = dplyr::if_else(
        stringr::str_detect(.data$cdc.classification.all2, "WILD|VDPV"),
        TRUE, FALSE
      )
    ) |>
    dplyr::filter(!is.na(rolling_period)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.group_by))) |>
    dplyr::summarise(
      afp_cases = sum(.data$source == "AFP", na.rm = TRUE),
      env_detect = sum(.data$source == "ENV", na.rm = TRUE),
      wild_vdpv_cases = sum(.data$source == "AFP" & .data$is_target, na.rm = TRUE),
      wild_vdpv_env = sum(.data$source == "ENV" & .data$is_target, na.rm = TRUE),
      wild_vdpv_samples = sum(.data$is_target, na.rm = TRUE),
      timely_samples = sum(.data$is_timely, na.rm = TRUE),
      timely_cases = sum(.data$source == "AFP" & .data$is_timely),
      timely_env = sum(.data$source == "ENV" & .data$is_timely),
      timely_wild_vdpv_samples = sum(.data$is_target & .data$is_timely),
      prop_timely_samples = .data$timely_samples / dplyr::n() * 100,
      prop_timely_samples_label = paste0(.data$timely_samples, "/", dplyr::n()),
      prop_timely_cases = .data$timely_cases / .data$wild_vdpv_cases * 100,
      prop_timely_cases_label = paste0(.data$timely_cases, "/", .data$wild_vdpv_cases),
      prop_timely_env = .data$timely_env / .data$wild_vdpv_env * 100,
      prop_timely_env_label = paste0(.data$timely_env, "/", .data$wild_vdpv_env),
      prop_timely_wild_vdpv = .data$timely_wild_vdpv_samples / .data$wild_vdpv_samples * 100,
      prop_timely_wild_vdpv_label = paste0(.data$timely_wild_vdpv_samples, "/", .data$wild_vdpv_samples)
    )

  pos_summary <- pos_summary |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c(
      "prop_timely_samples",
      "prop_timely_cases",
      "prop_timely_env",
      "prop_timely_wild_vdpv"
    )), \(x) if_else(x %in% c(Inf, NaN), 0, x))) |>
    dplyr::ungroup()

  return(pos_summary)
}

#' Generate KPI lab intervals
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Generates lab intervals related to the KPI code.
#' @details
#' This function is used in both [generate_c4_table] and in the lab timeliness
#' KPI violin plots.
#'
#' @inheritParams generate_c4_table
#'
#' @return `tibble` lab data with timeliness columns.
#'
#' @keywords internal
generate_kpi_lab_timeliness <- function(lab_data, start_date, end_date, afp_data) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  lab_data <- clean_lab_data(lab_data, start_date, end_date, afp_data)

  lab_data <- lab_data |>
    dplyr::mutate(
      # Timeliness of virus isolation results
      # Start date: receipt at WHO-accredited lab, end date: culture results
      # Target: ≤14 days
      days.lab.culture = .data$DateFinalCellCultureResult - .data$DateStoolReceivedinLab,
      t1 = dplyr::if_else(!is.na(.data$days.lab.culture) &
                            (.data$days.lab.culture >= 0 & .data$days.lab.culture <= 365),
                          TRUE, FALSE),
      # Timeliness of ITD results (Amanda added this)
      # Start date: culture results, end date: ITD results
      # Target: ≤7 days
      days.culture.itd = .data$DateFinalrRTPCRResults - .data$DateFinalCellCultureResult,
      t2 = dplyr::if_else(!is.na(.data$days.culture.itd) &
                            (.data$days.culture.itd >= 0 & .data$days.culture.itd <= 365),
                          TRUE, FALSE),
      # Timeliness of shipment for sequencing
      # Start date: ITD result, end date: arrival at sequencing lab (
      # (Amanda updated start date here to be consistent with GPSAP 2025-26 indicator)
      # Target: ≤7 days
      days.seq.ship = .data$DateIsolateRcvdForSeq - .data$DateFinalrRTPCRResults,
      t3 = dplyr::if_else(!is.na(.data$days.seq.ship) &
                            (.data$days.seq.ship >= 0 & .data$days.seq.ship <= 365),
                          TRUE, FALSE),
      # Timeliness of sequencing results
      # Start date: arrival at sequencing lab, end.date: sequencing results
      days.itd.seqres = .data$DateofSequencing - .data$DateIsolateRcvdForSeq,
      t4 = dplyr::if_else(!is.na(.data$days.itd.seqres) &
                            (.data$days.itd.seqres >= 0 & .data$days.itd.seqres <= 365),
                          TRUE, FALSE)
    )
}

# Public functions ----

#' Label rolling year periods
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The function labels and categorizes dates based on the rolling period specified.
#' The start year will always be Year 1 and the rolling period is defined by
#' the start date and the number of periods to account for in a given rolling year.
#' For example, if the start date is defined as Jan 1, 2021 and we would like to
#' calculate a 12-month rolling period, the end date would be Dec 31, 2021.
#'
#' @param df `tibble` A dataset containing at least one date column.
#' @param start_date `str` Start date of Year 1. All years are classified in
#' reference to this date.
#' @param date_col `str` The name of the date column.
#' @param period `period` A [lubridate::period()] object. Defaults to
#' `months(12, FALSE)`.
#'
#' @returns `tibble` A tibble with rolling year information.
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' afp_data <- add_rolling_years(raw_data$afp, "2021-01-01", "dateonset")
#' }
#' @export
add_rolling_years <- function(df, start_date, date_col, period = months(12, FALSE)) {
  start_date <- lubridate::as_date(start_date)

  df <- df |>
    dplyr::mutate(
      date_interval = lubridate::interval(start_date, !!rlang::sym(date_col)),
      year_num = floor(.data$date_interval / period),
      year_label = paste0("Year ", year_num + 1),
      analysis_year_start = start_date %m+% years(.data$year_num),
      analysis_year_end = .data$analysis_year_start %m+% period %m-% days(1),
      analysis_year_end = dplyr::if_else(lubridate::leap_year(.data$analysis_year_end) &
                                           lubridate::month(.data$analysis_year_end) == 2 &
                                           lubridate::day(.data$analysis_year_end) == 27,
                                         .data$analysis_year_end %m+% days(1),
                                         .data$analysis_year_end),
      rolling_period = paste0(lubridate::month(.data$analysis_year_start, label = TRUE, abbr = TRUE),
                              " ", lubridate::year(.data$analysis_year_start),
                              " - ",
                              lubridate::month(.data$analysis_year_end, label = TRUE, abbr = TRUE),
                              " ", lubridate::year(.data$analysis_year_end))
    ) |>
    dplyr::select(-"year_num")

  return(df)
}

#' GPEI Strategy surveillance KPIs
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Monitoring surveillance KPIs for Certification of Poliomyelitis Eradication at country
#' regional and global levels.
#'
#' @param raw_data `list` Global polio surveillance dataset.
#' Output of [get_all_polio_data()].
#' @param start_date `str` Start date of the analysis in YYYY-MM-DD format.
#' @param end_date `str` End date of the analysis in YYYY-MM-DD format.
#' @param risk_category `str` Risk category or a list of categories.
#' Defaults to `NULL`. Valid values are: `"LOW, LOW (WATCHLIST), MEDIUM, HIGH`.
#' @param lab_locs `tibble` Summary of the sequencing capacities of labs.
#' Output of [get_lab_locs()]. Defaults to `NULL`, which will download the information
#' directly from EDAV.
#' @param risk_table `tibble` GPSAP risk categorization for each country
#' .
#' @return `tibble` Summary table of GPSAP KPIs.
#'
#' @export
#'
#' @examples
#' raw_data <- get_all_polio_data(attach.spatial.data = FALSE)
#' c1 <- generate_c1_table(raw_data, "2021-01-01", "2023-12-31")
generate_c1_table <- function(raw_data, start_date, end_date,
                              risk_category = NULL,
                              lab_locs = NULL,
                              risk_table = NULL) {

  cli::cli_progress_bar("Creating C1 table", total = 8)

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  # Filtering
  afp_data <- raw_data$afp |>
    dplyr::filter(
      dplyr::between(dateonset, start_date, end_date),
      cdc.classification.all2 != "NOT-AFP"
    ) |>
    dplyr::select(-dplyr::starts_with("pons"))

  es_data <- raw_data$es
  cli::cli_progress_update()

  # Ensure that if using raw_data, required renamed columns are present. Borrowed from
  # extract.country.data()
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

  if (!is.null(risk_category)) {
    risk_category <- stringr::str_trim(stringr::str_to_upper(risk_category))
    afp_data <- suppressMessages(add_risk_category(afp_data)) |>
      dplyr::filter(.data$`SG Priority Level` %in% risk_category)
    es_data <- suppressMessages(add_risk_category(es_data,
                                                  ctry_col = "ADM0_NAME")) |>
      dplyr::filter(.data$`SG Priority Level` %in% risk_category)
  }
  cli::cli_progress_update()
  # Include required columns
  afp_data <- suppressMessages(col_to_datecol(afp_data))
  afp_data <- add_rolling_years(afp_data, start_date, "date")
  es_data <- add_rolling_years(es_data, start_date, "collect.date")
  cli::cli_progress_update()

  # Calculate country indicators
  afp_indicators <- afp_data |>
    dplyr::group_by(year_label, rolling_period,
                    analysis_year_start, analysis_year_end) |>
    dplyr::summarise(
      npafp_dist = list(f.npafp.rate.01(dplyr::pick(dplyr::everything()),
                                        raw_data$dist.pop,
                                        min(.data$analysis_year_start),
                                        max(.data$analysis_year_end),
                                        "dist", rolling = TRUE,
                                        sp_continuity_validation = FALSE)),
      stoolad_dist = list(f.stool.ad.01(dplyr::pick(dplyr::everything()),
                                        raw_data$dist.pop,
                                        min(.data$analysis_year_start),
                                        max(.data$analysis_year_end),
                                        "dist", rolling = TRUE,
                                        sp_continuity_validation = FALSE))
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(-dplyr::any_of(c("year_label", "rolling_period",
                                                 "analysis_year_start", "analysis_year_end")),
                                \(x) list(dplyr::tibble(x) |>
                                            dplyr::mutate(year_label = year_label,
                                                          rolling_period = rolling_period,
                                                          analysis_year_start = analysis_year_start,
                                                          analysis_year_end = analysis_year_end
                                            )))
    ) |>
    dplyr::ungroup()
  cli::cli_progress_update()

  # Get established ES sites first and then filter to appropriate start and end dates
  # Have to do this outside of es_indicators because grouping by year also
  # effectively filter data belonging to that year label
  es_end_dates <- es_data |>
    dplyr::select(year_label, rolling_period,
                  analysis_year_start, analysis_year_end) |>
    dplyr::distinct() |>
    dplyr::filter(dplyr::between(analysis_year_end, start_date, max(analysis_year_end)))
  # this does NOT filter to established sites, it will list the number of samples
  # in the past 12 months since the end date and the age of the site from
  # the very first collection in the ES dataset to the latest collection
  # available that may or may not be the end date but not go beyond that
  es_site_age <- es_end_dates |>
    dplyr::group_by(year_label, rolling_period) |>
    dplyr::mutate(es_sites_w_age = list(get_es_site_age(es_data, max(analysis_year_end)))) |>
    dplyr::ungroup() |>
    # not necessary anymore
    dplyr::select(-analysis_year_start, -analysis_year_end)

  es_indicators <- es_data |>
    dplyr::filter(between(analysis_year_end, start_date, end_date)) |>
    dplyr::group_by(.data$year_label, .data$rolling_period,
                    analysis_year_start, analysis_year_end) |>
    dplyr::summarize(ev_rate = list(f.ev.rate.01(dplyr::pick(dplyr::everything()),
                                                 min(.data$analysis_year_start),
                                                 max(.data$analysis_year_end)))
                     ) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(-dplyr::any_of(c("year_label", "rolling_period",
                                                 "analysis_year_start", "analysis_year_end")),
                                \(x) list(dplyr::tibble(x) |>
                                            dplyr::mutate(year_label = year_label,
                                                          rolling_period = rolling_period,
                                                          analysis_year_start = analysis_year_start,
                                                          analysis_year_end = analysis_year_end
                                            )))) |>
    dplyr::ungroup()

  # Combine both, then able to do an inner join for the final dataset
  # This makes sure for each rolling year, that only the sites selected
  # are in the final ES dataset
  es_indicators <- dplyr::left_join(es_indicators, es_site_age) |>
    dplyr::mutate(final_es_dataset = purrr::map2(ev_rate, es_sites_w_age,
                                                 \(x, y) dplyr::left_join(x, y)))

  cli::cli_progress_update()

  timely_det_indicator <- generate_wild_vdpv_summary(raw_data,
                                                     start_date, end_date,
                                                     risk_table = risk_table,
                                                     lab_locs = lab_locs)
  cli::cli_progress_update()

  # Calculate meeting indicators
  region_lookup_table <- raw_data$ctry.pop |>
    dplyr::select(ctry = ADM0_NAME, adm0guid, whoregion = WHO_REGION) |>
    dplyr::distinct()
  dist_lookup_table <- raw_data$dist.pop |>
    dplyr::select(ctry = ADM0_NAME, prov = ADM1_NAME, dist = ADM2_NAME, adm2guid) |>
    dplyr::distinct()

  # Summarise
  met_npafp <- dplyr::bind_rows(afp_indicators$npafp_dist) |>
    dplyr::left_join(region_lookup_table) |>
    dplyr::group_by(year_label, rolling_period,
                    analysis_year_start, analysis_year_end,
                    whoregion, ctry) |>
    dplyr::summarise(dist_w_100k = sum(par >= 1e5, na.rm = TRUE),
                     dist_npafp = sum(par != 0, na.rm = TRUE), # remove districts without populations
                     met_npafp = sum(
                       (par >= 1e5 & npafp_rate >= 3 &
                          ctry %in% c("AFGHANISTAN", "PAKISTAN")),
                       (par >= 1e5 & npafp_rate >= 2 &
                          whoregion %in% c("AFRO", "EMRO", "SEARO") &
                          !ctry %in% c("AFGHANISTAN", "PAKISTAN")),
                       (par >= 1e5 & npafp_rate >= 1 &
                          whoregion %in% c("AMRO", "EURO", "WPRO")),
                       na.rm = T),
                     prop_met_npafp = met_npafp / dist_w_100k * 100,
                     npafp_label = paste0(met_npafp, "/", dist_w_100k)) |>
    ungroup()

  met_stool <- dplyr::bind_rows(afp_indicators$stoolad_dist) |>
    dplyr::left_join(dist_lookup_table) |>
    dplyr::left_join(region_lookup_table) |>
    dplyr::filter(!is.na(ctry)) |>
    dplyr::group_by(year_label, rolling_period,
                    analysis_year_start, analysis_year_end,
                    whoregion, ctry) |>
    dplyr::summarise(dist_stool = n(),
                     met_stool = sum(per.stool.ad >= 0.8 & adequacy.denominator >= 5, na.rm = T),
                     prop_met_stool = met_stool / dist_stool * 100,
                     stool_label = paste0(met_stool, "/", dist_stool))

  met_ev <- dplyr::bind_rows(es_indicators$final_es_dataset) |>
    dplyr::rename("ctry" = ADM0_NAME,
                  "prov" = ADM1_NAME,
                  "dist" = ADM2_NAME) |>
    dplyr::left_join(dist_lookup_table) |>
    dplyr::left_join(region_lookup_table) |>
    dplyr::group_by(year_label, rolling_period,
                    analysis_year_start, analysis_year_end,
                    whoregion, ctry) |>
    dplyr::summarise(es_sites = sum(n_samples_12_mo >= 10 & site_age >= 12, na.rm = T),
                     met_ev = sum(num.samples >= 10 & site_age >= 12 & ev.rate >= 0.5, na.rm = T),
                     prop_met_ev = met_ev / es_sites * 100,
                     ev_label = paste0(met_ev, "/", es_sites))
  cli::cli_progress_update()

  combine <- dplyr::full_join(met_npafp, met_stool) |>
    dplyr::full_join(met_ev) |>
    dplyr::full_join(timely_det_indicator |>
                       rename("ctry" = "place.admin.0")) |>
    dplyr::select(dplyr::any_of(c(
      "year_label", "rolling_period",
      "analysis_year_start", "analysis_year_end",
      "whoregion", "SG Priority Level", "ctry",
      "prop_met_npafp", "prop_met_stool", "prop_met_ev",
      "prop_timely_samples", "prop_timely_wild_vdpv",
      "npafp_label", "stool_label", "ev_label",
      "prop_timely_wild_vdpv_label",
      "prop_timely_samples_label", "prop_timely_cases_label",
      "prop_timely_env_label")
    ))

  # Clean up
  combine <- combine |>
    dplyr::filter(!is.na(ctry)) |>
    # If no priority level, default to low
    dplyr::mutate(`SG Priority Level` = dplyr::if_else(is.na(`SG Priority Level`),
                                                       "LOW", `SG Priority Level`)
                  ) |>
    # NAs should be replace with NaNs and not be empty
    dplyr::mutate(dplyr::across(dplyr::starts_with("prop") & !dplyr::ends_with("label"), \(x) round(tidyr::replace_na(x, NaN), 2))) |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("label"), \(x) tidyr::replace_na(x, "0/0")))

  cli::cli_progress_update()
  cli::cli_progress_done()

  return(combine)

}

#' AFP surveillance KPI summary
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function creates a summary table of AFP surveillance KPIs.
#'
#' @param afp_data `tibble` AFP linelist data.
#' @param pop_data `tibble` Population data.
#' @param start_date `str` Start date of analysis in YYYY-MM-DD format.
#' @param end_date `str` End date of analysis in YYYY-MM-DD format.
#' @param spatial_scale `str` Either `"ctry", "prov", "dist"`.
#'
#' @return `tibble` Summary table containing AFP KPIs.
#' @export
#'
#' @examples
#' raw_data <- get_all_polio_data(attach.spatial.data = FALSE)
#' c2 <- generate_c2_table(raw_data$afp, raw_data$ctry.pop, "2021-01-01", "2023-12-31", "ctry")
generate_c2_table <- function(afp_data, pop_data, start_date, end_date,
                              spatial_scale,
                              risk_category = NULL) {

  check_spatial_scale(pop_data, spatial_scale)

  cli::cli_progress_bar("Creating C2 table", total = 5)

  # Standardize data
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
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
  pop_data <- dplyr::rename_with(pop_data, recode,
    ADM0_NAME = "ctry",
    ADM1_NAME = "prov",
    ADM2_NAME = "dist",
    ADM0_GUID = "adm0guid",
    u15pop.prov = "u15pop"
  )
  cli::cli_progress_update()

  # Add required columns
  # NOTE: NAs are removed from rolling calculations as they can't be tagged
  afp_data <- afp_data |>
    add_seq_capacity() |>
    col_to_datecol() |>
    add_rolling_years(start_date, "date") |>
    dplyr::filter(dplyr::between(analysis_year_end, start_date,
                                 max(analysis_year_end, na.rm = TRUE)))

  # Defaults to missing = good, bad.data = inadequate
  # Start and end dates only filter afp_data and isn't used in calculations
  # inside the function
  afp_data <- generate_stool_data(afp_data, start_date, end_date)

  cli::cli_progress_update()

  # NPAFP and Stool Adequacy
  afp_indicators <- afp_data |>
    dplyr::group_by(year_label, rolling_period,
                    analysis_year_start, analysis_year_end) |>
    dplyr::summarize(
      npafp = list(f.npafp.rate.01(dplyr::pick(dplyr::everything()),
                                        pop_data,
                                        min(.data$analysis_year_start),
                                        max(.data$analysis_year_end),
                                        spatial_scale, rolling = TRUE,
                                        sp_continuity_validation = FALSE)),
      stoolad = list(f.stool.ad.01(dplyr::pick(dplyr::everything()),
                                        pop_data,
                                        min(.data$analysis_year_start),
                                        max(.data$analysis_year_end),
                                        spatial_scale, rolling = TRUE,
                                        sp_continuity_validation = FALSE))
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(-dplyr::any_of(c("year_label", "rolling_period",
                                                 "analysis_year_start",
                                                 "analysis_year_end")),
                                \(x) list(dplyr::tibble(x) |>
                                            dplyr::mutate(year_label = year_label,
                                                          rolling_period = rolling_period,
                                                          analysis_year_start = analysis_year_start,
                                                          analysis_year_end = analysis_year_end
                                            )))
    ) |>
    dplyr::ungroup()
  # Stool Condition
  group_stool_cond <- switch(spatial_scale,
                             "ctry" = c("year_label", "analysis_year_start",
                                        "analysis_year_end", "rolling_period",
                                        "ctry", "adm0guid"),
                             "prov" = c("year_label", "analysis_year_start",
                                        "analysis_year_end", "rolling_period",
                                        "ctry", "adm0guid",
                                        "prov", "adm1guid"),
                             "dist" = c("year_label", "analysis_year_start",
                                        "analysis_year_end", "rolling_period",
                                        "ctry", "adm0guid",
                                        "prov", "adm1guid", "dist", "adm2guid"))
  stool_condition <- afp_data |>
    dplyr::group_by(dplyr::across(dplyr::any_of(group_stool_cond))) |>
    dplyr::summarize(
      afp_cases = sum(.data$cdc.classification.all2 != "NOT-AFP"),
      good_samples = sum(.data$cdc.classification.all2 != "NOT-AFP" &
                           .data$adequacy.final2 == "Adequate"),
      prop_good_condition = good_samples / afp_cases * 100
    )
  cli::cli_progress_update()

  # Completeness of Contact Sampling
  # Calculated in future versions
  group_60_day <- switch(spatial_scale,
                         "ctry" = c("year_label",
                                    "analysis_year_start",
                                    "analysis_year_end",
                                    "rolling_period",
                                    "epid", "adm0guid", "ctry"),
                         "prov" = c("year_label",
                                    "analysis_year_start",
                                    "analysis_year_end",
                                    "rolling_period",
                                    "epid", "adm0guid", "adm1guid", "ctry",
                                    "prov"
                                    ),
                         "dist" = c("year_label",
                                    "analysis_year_start",
                                    "analysis_year_end",
                                    "rolling_period",
                                    "epid", "adm0guid", "adm1guid", "adm2guid",
                                    "ctry", "prov", "dist"))

  # Completeness of 60-day follow-ups
  complete_60_day <- afp_data |>
    # Note that this also filters out "NOT-AFP" cases
    # Start and end dates also just filters dates and not used in calculations
    generate_60_day_table_data(start_date, end_date) |>
    dplyr::left_join(afp_data |>
      dplyr::select(dplyr::all_of(group_60_day))) |>
    dplyr::select(-"epid") |>
    dplyr::group_by(dplyr::across(dplyr::any_of(group_60_day))) |>
    dplyr::summarize(prop_complete_60_day = sum(.data$ontime.60day == 1, na.rm = TRUE) /
      sum(adequacy.final2 == "Inadequate") * 100) |>
    ungroup()
  cli::cli_progress_update()
  # Timeliness indicators
  timeliness_summary <- afp_data |>
    dplyr::filter(
      .data$cdc.classification.all2 != "NOT-AFP",
      dplyr::between(.data$date, start_date, end_date)
    ) |>
    dplyr::mutate(
      ontonothq = as.numeric(lubridate::as_date(.data$datenotificationtohq) -
        .data$date),
      ontolab = as.numeric(difftime(
        lubridate::as_date(.data$stooltolabdate),
        .data$date
      ), units = "days"),
      timely_cat =
        dplyr::case_when(seq.capacity == "yes" & ontonothq <= 35 ~ "<=35 days from onset",
          seq.capacity == "yes" & ontonothq > 35 ~ ">35 days from onset",
          seq.capacity == "no" & ontonothq <= 46 ~ "<=46 days from onset",
          seq.capacity == "no" & ontonothq > 46 ~ ">46 days from onset",
          is.na(ontonothq) | ontonothq < 0 ~ "Missing or bad data",
          .default = "Missing or bad data"
        ),
      is_timely = dplyr::if_else(.data$timely_cat %in%
        c(
          "<=35 days from onset",
          "<=46 days from onset"
        ), TRUE, FALSE),
      is_target = dplyr::if_else(
        stringr::str_detect(.data$cdc.classification.all2, "WILD|VDPV"),
        TRUE, FALSE
      ),
      is_opt_timely = dplyr::case_when(
        (.data$culture.itd.cat == "In-country culture/ITD" & .data$ontolab <= 14) ~ "yes",
        (.data$culture.itd.cat == "In-country culture/ITD" & .data$ontolab > 14) ~ "no",
        (.data$culture.itd.cat == "International culture/ITD" & .data$ontolab <= 18) ~ "yes",
        (.data$culture.itd.cat == "International culture/ITD" & .data$ontolab > 18) ~ "no",
        .default = "unable to assess"
      )
    ) |>
    dplyr::group_by(dplyr::across(dplyr::any_of(group_stool_cond))) |>
    dplyr::summarize(
      timely_not = sum(.data$ontonot <= 7, na.rm = TRUE) / sum(!is.na(.data$ontonot)) * 100,
      timely_inv = sum(.data$ontoinvest <= 2, na.rm = TRUE) / sum(!is.na(.data$ontoinvest)) * 100,
      timely_field = sum(.data$ontostool1 <= 11 &
        .data$stool1tostool2 >= 1, na.rm = TRUE) / sum(!is.na(.data$ontostool1) & !is.na(.data$stool1tostool2)) * 100,
      timely_stool_shipment = sum(
        (.data$culture.itd.cat == "In-country culture/ITD" & .data$daysstooltolab <= 3) |
          (.data$culture.itd.cat == "International culture/ITD" & .data$daysstooltolab <= 7),
        na.rm = TRUE
      ) / sum(!is.na(.data$culture.itd.cat) & !is.na(.data$daysstooltolab)) * 100,
      timely_opt_field_shipment = sum(.data$is_opt_timely == "yes") / sum(.data$is_opt_timely != "unable to assess") * 100,
      timely_wpv_vdpv = sum(.data$is_timely & .data$is_target & .data$timely_cat != "Missing or bad data", na.rm = TRUE) /
        sum(.data$is_timely & .data$timely_cat != "Missing or bad data", na.rm = TRUE) * 100
    )
  cli::cli_progress_update()

  # Completeness of weekly zero reporting
  # Timeliness of WZR
  # Adequacy of active surveillance sites
  # AFP case encounters

  results <- dplyr::full_join(dplyr::bind_rows(afp_indicators$npafp),
                              dplyr::bind_rows(afp_indicators$stoolad)) |>
    dplyr::full_join(stool_condition) |>
    dplyr::full_join(complete_60_day) |>
    dplyr::full_join(timeliness_summary)

  # Select only required columns
  results <- results |>
    dplyr::mutate(
      npafp_cat = dplyr::case_when(
        (.data$n_npafp == 0) & .data$par >= 100000 ~ "Silent (u15pop >= 100K)",
        (.data$n_npafp == 0) & (.data$par > 0 & .data$par < 100000) ~ "No cases (u15pop < 100K)",
        (.data$par == 0 | is.na(.data$par)) ~ "Missing Pop",
        # Calculate regardless of population size
        .data$npafp_rate < 1 ~ "< 1",
        (.data$npafp_rate >= 1 & .data$npafp_rate < 2) ~ ">= 1 & <2",
        (.data$npafp_rate >= 2 & .data$npafp_rate < 3) ~ ">= 2 & <3",
        .data$npafp_rate >= 3 ~ ">=3"
      ),
      stool_cat = dplyr::case_when(
        .data$afp.cases == 0 | is.na(.data$afp.cases) ~ "Zero AFP cases",
        .data$afp.cases != 0 & .data$per.stool.ad < 50 ~ "<50%",
        .data$afp.cases != 0 & (.data$per.stool.ad >= 50 & .data$per.stool.ad <= 79) ~ "50%-79%",
        afp.cases != 0 & per.stool.ad > 79 ~ "80%+"
      )
    ) |>
    dplyr::select(
      dplyr::any_of(group_stool_cond),
      "npafp_cat", "stool_cat",
      "npafp_rate", "per.stool.ad",
      "prop_good_condition":"timely_wpv_vdpv"
    )

  # Formatting
  results <- results |>
    dplyr::mutate(dplyr::across("npafp_rate":"timely_wpv_vdpv", \(x) tidyr::replace_na(x, NaN))) |>
    # NPAFP is only NA if it's missing population
    dplyr::mutate(npafp_rate = dplyr::if_else(npafp_cat != "Missing Pop" & is.nan(npafp_rate), 0, npafp_rate))

  cli::cli_progress_done()

  return(results)
}

#' Environmental surveillance KPIs by site
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Environmental surveillance KPIs summarized by site.
#'
#'
#' @param es_data `tibble` Environmental surveillance data.
#' @param start_date `str` Start date of the analysis in YYYY-MM-DD format.
#' @param end_date `str` End date of the analysis in YYYY-MM-DD format.
#'
#' @return `tibble` A summary table of environmental surveillance KPIs.
#' @export
#'
#' @examples
#' raw_data <- get_all_polio_data(attach.spatial.data = FALSE)
#' c3 <- generate_c3_table(raw_data$es, "2021-01-01", "2023-12-31")
generate_c3_table <- function(es_data, start_date, end_date,
                              risk_category = NULL) {

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  es_data <- es_data |>
    add_seq_capacity("ADM0_NAME") |>
    add_rolling_years(start_date, "collect.date")

  # Get established ES sites first and then filter to appropriate start and end dates
  # Have to do this outside of es_indicators because grouping by year also
  # effectively filter data belonging to that year label
  es_end_dates <- es_data |>
    dplyr::select(year_label, rolling_period,
                  analysis_year_start, analysis_year_end) |>
    dplyr::distinct() |>
    dplyr::filter(dplyr::between(analysis_year_end, start_date, max(analysis_year_end)))
  # this does NOT filter to established sites, it will list the number of samples
  # in the past 12 months since the end date and the age of the site from
  # the very first collection in the ES dataset to the latest collection
  # available that may or may not be the end date but not go beyond that
  es_site_age <- es_end_dates |>
    dplyr::group_by(year_label, rolling_period) |>
    dplyr::mutate(es_sites_w_age = list(get_es_site_age(es_data, max(analysis_year_end)))) |>
    dplyr::ungroup() |>
    # not necessary anymore
    dplyr::select(-analysis_year_start, -analysis_year_end) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(-dplyr::any_of(c("year_label", "rolling_period")),
                                \(x) list(dplyr::tibble(x) |>
                                            dplyr::mutate(year_label = year_label,
                                                          rolling_period = rolling_period
                                            ))))
  # Ages based on rolling years
  es_site_age_long <- dplyr::bind_rows(es_site_age$es_sites_w_age)

  # Add additional timeliness columns
  es_data <- es_data |>
    dplyr::mutate(coltolab = difftime(.data$date.received.in.lab,
                                      .data$collect.date, units = "day"),
                  coltoresults = difftime(.data$date.notification.to.hq,
                                          .data$date.received.in.lab, units = "days"),
                  timely_ship = dplyr::case_when(
                    (is.na(.data$culture.itd.cat) | is.na(.data$coltolab) |
                       .data$coltolab < 0 | .data$coltolab > 365) ~ "unable to assess",
                    (.data$culture.itd.cat == "In-country culture/ITD" & .data$coltolab <= 3) ~ "yes",
                    (.data$culture.itd.cat == "In-country culture/ITD" & .data$coltolab > 3) ~ "no",
                    (.data$culture.itd.cat == "International culture/ITD" & .data$coltolab <= 7) ~ "yes",
                    (.data$culture.itd.cat == "International culture/ITD" & .data$coltolab > 7) ~ "no",
                    .default = NA),

                  timely_det = dplyr::case_when(
                    (is.na(.data$seq.capacity) | is.na(.data$coltoresults) |
                       .data$coltoresults < 0 | .data$coltoresults > 365) ~ "unable to assess",
                    (stringr::str_detect(.data$seq.capacity, "[Yy]es") & .data$coltoresults <= 35) ~ "yes",
                    (stringr::str_detect(.data$seq.capacity, "[Yy]es") & .data$coltoresults > 35) ~ "no",
                    (.data$seq.capacity == "no" & .data$coltoresults <= 46) ~ "yes",
                    (.data$seq.capacity == "no" & .data$coltoresults > 46) ~ "no",
                    .default = NA),

                  is_target = dplyr::if_else(.data$wpv == 1 | .data$vdpv == 1, TRUE, FALSE)
                  )

  es_summary <- es_data |>
    dplyr::filter(dplyr::between(analysis_year_end, start_date, max(analysis_year_end))) |>
    dplyr::group_by(year_label, rolling_period,
                    analysis_year_start, analysis_year_end,
                    ADM0_NAME, site.name) |>
    dplyr::summarize(
      es_samples = sum(!is.na(.data$ev.detect), na.rm = TRUE),
      ev_rate = sum(.data$ev.detect == 1, na.rm = TRUE) / es_samples * 100,
      prop_good_es = sum(.data$sample.condition == "Good", na.rm = TRUE) / sum(!is.na(.data$sample.condition)) * 100,
      prop_timely_ship = sum(timely_ship == "yes", na.rm = TRUE) /
        sum(timely_ship != "unable to assess" | !is.na(timely_ship), na.rm = TRUE) * 100,
      wpv_vdpv_detections = sum(is_target == TRUE & (timely_det != "unable to assess" |
                                                       !is.na(timely_det)), na.rm = TRUE),
      timely_wpv_vdpv_detections = sum(is_target == TRUE &
                                         timely_det == "yes", na.rm = TRUE),
      prop_timely_det_wpv_vdpv = timely_wpv_vdpv_detections /
        wpv_vdpv_detections * 100
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(ev_det_cat = dplyr::case_when(
      .data$es_samples < 5 ~ "<5 samples collected",
      .data$ev_rate < 50 & .data$es_samples >= 5 ~ "<50%",
      .data$ev_rate >= 50 & .data$ev_rate < 80 & es_samples >= 5 ~ "50% to <80%",
      .data$ev_rate >= 80 & .data$es_samples >= 5 ~ "80-100%",
      .default = NA
    )) |>
    dplyr::mutate(ev_det_cat = factor(.data$ev_det_cat,
                                      levels = c("<5 samples collected", "<50%",
                                                 "50% to <80%", "80-100%")))

  es_summary <- add_risk_category(es_summary, ctry_col = "ADM0_NAME") |>
    dplyr::mutate(`SG Priority Level` = dplyr::if_else(is.na(`SG Priority Level`),
                                                       "LOW", `SG Priority Level`),
                  Region = get_region(ADM0_NAME))

  es_summary <- dplyr::left_join(es_summary, es_site_age_long)

  return(es_summary)

}

#' Create a country level rollup of the C3 label
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Create a country level summary of ES site performance with respect to
#' meeting established targets for EV detection rates, good samples. Note, this
#' country roll up will only consider sites that have at least 10 collections
#' and open for at least 12 months, consistent with guidelines in the 2025-2026
#' GPSAP indicators.
#'
#'
#' @param c3 `tibble` Output of [generate_c3_table()].
#' @param include_labels `bool` Include columns for the labels? Default TRUE.
#' @param min_sample `num` Only consider sites with at least this number
#' of ES samples. Default is `10`.
#'
#' @return `tibble` A summary of the c3 table at the country level
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' c3 <- generate_c3_table(raw_data$es, "2021-01-01", "2023-12-31")
#' c3_rollup <- generate_c3_rollup(c3)
#' }
generate_c3_rollup <- function(c3, include_labels = TRUE, min_sample = 10) {

  if (!"site.name" %in% names(c3)) {
    cli::cli_abort("Please summarize c3 at the site level and try again.")
  }

  c3_rollup <- c3 |>
    dplyr::rename(ctry = "ADM0_NAME",
                  priority_level = "SG Priority Level",
                  who_region = "Region") |>
    dplyr::group_by(year_label, rolling_period,
                    analysis_year_start, analysis_year_end,
                    ctry, who_region, priority_level) |>
    dplyr::filter(n_samples_12_mo >= min_sample, site_age >= 12) |>
    dplyr::summarize(
      met_ev = sum(ev_rate >= 50, na.rm = TRUE),
      met_good_samples = sum(prop_good_es >= 80, na.rm = TRUE),
      met_timely_wpv_vdpv_det = sum(prop_timely_det_wpv_vdpv >= 80, na.rm = TRUE),
      median_timely_shipment_per_site = median(prop_timely_ship, na.rm = TRUE),
      es_sites = n(),
      prop_met_ev = met_ev / es_sites,
      prop_met_good_samples = met_good_samples / es_sites,
      prop_met_timely_wpv_vdpv_det = met_timely_wpv_vdpv_det / es_sites,
      prop_met_ev_label = paste0(met_ev, "/", es_sites),
      prop_met_good_samples_label = paste0(met_good_samples, "/", es_sites),
      prop_met_timely_wpv_vdpv_det_label = paste0(met_timely_wpv_vdpv_det, "/", es_sites)
    ) |>
    dplyr::select(-dplyr::starts_with("met_"), "es_sites")

  return(c3_rollup)

}

#' Laboratory surveillance KPIs
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Summarizes the timeliness of samples as it arrives in the lab and to sequencing
#' results. Samples may come from both AFP and ES samples.
#'
#' @param lab_data `tibble` Lab data containing information of ES or AFP samples.
#' @param afp_data `tibble` AFP surveillance data.
#' @param start_date `str` Start date of the analysis in YYYY-MM-DD format.
#' @param end_date `str` End date of the analysis in YYYY-MM-DD format.
#' @param .group_by `str` or `list` What columns to group the results by.
#'
#' @return `tibble` A summary of timeliness KPIs for lab data.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data(attach.spatial.data = FALSE)
#' lab_data <- readr::read_csv("C:/Users/ABC1/Desktop/lab_data.csv")
#' c4 <- generate_c4_table(lab_data, raw_data$afp, "2021-01-01", "2024-12-31")
#' }
generate_c4_table <- function(lab_data, afp_data, start_date, end_date, .group_by = "year") {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  lab_data <- generate_kpi_lab_timeliness(lab_data, start_date, end_date, afp_data)

  lab_summary <- lab_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.group_by))) |>
    dplyr::summarize(
      # Timeliness of virus isolation results
      timely_isolation = sum(.data$days.lab.culture <= 14 & t1 == TRUE, na.rm = TRUE) /
        sum(t1 == TRUE) * 100,
      # Timeliness of ITD results
      timely_itd = sum(.data$days.culture.itd <= 7 & t2 == TRUE & (!.data$FinalCellCultureResult %in% c(NA, "Negative")), na.rm = TRUE) /
        sum(t2 == TRUE & (!.data$FinalCellCultureResult %in% c(NA, "Negative"))) * 100,
      # Timeliness of shipment for sequencing
      timely_seqship = sum(.data$days.seq.ship <= 7 & t3 == TRUE & !is.na(.data$FinalITDResult), na.rm = TRUE) /
        sum(t3 == TRUE & !is.na(.data$FinalITDResult)) * 100,
      #Timeliness of sequencing results
      timely_seqres = sum(.data$days.itd.seqres <= 7 & t4 == TRUE & !is.na(.data$FinalITDResult), na.rm = TRUE) /
        sum(t4 == TRUE & !is.na(.data$FinalITDResult)) * 100,
    )

  return(lab_summary)

  }

#' Export KPI tables
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Performs formatting and export of the C1-C4 KPI tables.
#'
#' @param c1 `tibble` Output of [generate_c1_table()]. Defaults to `NULL`.
#' @param c2 `tibble` Output of [generate_c2_table()]. Defaults to `NULL`.
#' @param c3 `tibble` Output of [generate_c3_table()]. Defaults to `NULL`.
#' @param c4 `tibble` Output of [generate_c4_table()]. Defaults to `NULL`.
#' @param output_path `str` Path to output the table to. Defaults to the path
#' initiated after running [init_kpi()].
#' @param drop_label_cols `bool` Keep or discard label columns. Defaults to
#' `TRUE`.
#'
#' @return None.
#' @export
#'
#' @examples
#' \dontrun{
#' init_kpi()
#' c1 <- generate_c1_table(raw_data, "2021-01-01", "2023-12-31")
#' c2 <- generate_c2_table(raw_data$afp, raw_data$ctry.pop, "2021-01-01", "2023-12-31")
#' c3 <- generate_c3_table(raw_data$es, "2021-01-01", "2023-12-31")
#' c4 <- generate_c4_table(lab_data, raw_data$afp, "2021-01-01", "2024-12-31")
#' export_kpi_table(c1, c2, c3, c4, drop_label_cols = FALSE)
#' }
export_kpi_table <- function(c1 = NULL, c2 = NULL, c3 = NULL, c4 = NULL,
                             output_path = Sys.getenv("KPI_TABLES"),
                             drop_label_cols = TRUE) {
  format_table <- function(x) {
    x <- x |>
      dplyr::rename_with(\(x) stringr::str_to_lower(x)) |>
      dplyr::rename_with(\(x) stringr::str_replace_all(x, "[\\. ]", "_")) |>
      dplyr::mutate(dplyr::across(
        (dplyr::starts_with("prop") |
          dplyr::starts_with("timely")) &
          -dplyr::ends_with("label"),
        \(x) round(x, 0)
      ))
    return(x)
  }

  drop_labels <- function(x, .drop_label_cols = drop_label_cols) {
    if (.drop_label_cols) {
      x <- x |>
        dplyr::select(-dplyr::ends_with("label"),
                      -dplyr::ends_with("cat"))
    }
    return(x)
  }

  export_list <- list(c1 = c1, c2 = c2, c3 = c3, c4 = c4)
  export_list <- export_list |>
    purrr::keep(\(x) !is.null(x)) |>
    purrr::map(format_table) |>
    purrr::map(drop_labels)

  file_name <- paste0("kpi_tables_",
                      paste0(names(export_list), collapse = "_"),
                      "_", Sys.Date(), ".xlsx")
  writexl::write_xlsx(export_list, file.path(output_path, file_name))
}

# Deprecated functions ----
# These functions have been deprecated in favor of a more flexible and
# streamlined approach to labeling rolling periods via add_rolling_years().

#' Adds rolling date information
#'
#' @param df `tibble` Dataframe to add rolling date info to.
#' @param onset_col `str` Column used for the onset date. Mainly used to calculate
#' `weeks.from.end`.
#' @param start_date `str` Start date of the analysis.
#' @param end_date `str` End date of the analysis.
#'
#' @returns `tibble` Lab data with rolling date period information added.
#' @keywords internal
#'
add_rolling_date_info <- function(df, start_date, end_date, onset_col = "ParalysisOnsetDate") {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  cli::cli_process_start("Adding rolling period date information")
  end.year <- lubridate::year(end_date)
  st.year <- lubridate::year(start_date)

  month.start <- lubridate::month(start_date, label = TRUE, abbr = TRUE)
  month.end <- lubridate::month(end_date, label = TRUE, abbr = TRUE)
  # prior_period = paste0(month.start, " ", st.year, " - ", month.end, " ", year(start_date + days(364)) )
  # current_period = paste0(month.start, " ", st.year + 1, " - ", month.end, " ", end.year)

  # # Calculate prior_period correctly accounting for leap years
  # prior_year_end <- start_date + lubridate::years(1) - lubridate::days(1)
  # prior_period <- paste0(
  #   month.start, " ", st.year,
  #   " - ", month.end,
  #   " ", lubridate::year(prior_year_end)
  # )
  #
  # current_period <- paste0(
  #   month.start,
  #   " ", st.year + 1,
  #   " - ", month.end,
  #   " ", end.year
  # )

  df <- df |>
    # renaming culture.itd.lab for Nigeria which has two labs in lab.locs, simply naming Nigeria
    dplyr::mutate(
      weeks.analysis = ((end_date - start_date) / 7), # weeks in the analysis
      weeks.from.end = ((end_date - get(onset_col)) / 7), # weeks from onset to end of analysis
      case.week = (.data$weeks.analysis - .data$weeks.from.end), # estimating week in the analysis for each case
      year.analysis = f.year.roll(.data$case.week), # analysis year
      year.analysis.start = start_date + (365 * (as.integer(.data$year.analysis) - 1)),
      year.analysis.end = .data$year.analysis.start + 364,
      rolling_period = paste0(lubridate::month(year.analysis.start, label = TRUE, abbr = TRUE),
                              " ", lubridate::year(year.analysis.start),
                              " - ",
                              lubridate::month(year.analysis.end, label = TRUE, abbr = TRUE),
                              " ", lubridate::year(year.analysis.end))
    )

  cli::cli_process_done()

  return(df)
}
#' Add rolling year label for each week number
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
  # the hardcoded 52 is problematic given some years are 52 weeks and other 53
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
              labels = c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5",
                         "Year 6", "Year 7", "Year 8", "Year 9", "Year 10")
  )

  return(x)
}

