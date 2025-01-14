# Private functions ----
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

#' Generates a summary table regarding wild and VDPV cases
#'
#' @inheritParams generate_c1_table
#'
#' @return `tibble` Summary of wild and VDPV cases
#' @keywords internal
generate_wild_vdpv_summary <- function(raw_data, start_date, end_date, risk_table = NULL, lab_locs = NULL) {
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
  pos <- add_rolling_date_info(pos, start_date, end_date, "dateonset")
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
    ) |>
    dplyr::filter(!is.na(rolling_period)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(
      "whoregion",
      "SG Priority Level",
      "place.admin.0",
      "rolling_period",
      "year.analysis"
    )))) |>
    dplyr::summarise(
      afp_cases = sum(source == "AFP", na.rm = TRUE),
      env_detect = sum(source == "ENV", na.rm = TRUE),
      wild_vdpv_cases = sum(source == "AFP" & is_target, na.rm = TRUE),
      wild_vdpv_env = sum(source == "ENV" & is_target, na.rm = TRUE),
      timely_samples = sum(is_timely, na.rm = TRUE),
      timely_cases = sum(source == "AFP" & is_timely),
      timely_env = sum(source == "ENV" & is_timely),
      prop_timely_samples = .data$timely_samples / dplyr::n(),
      prop_timely_cases = .data$timely_cases / .data$wild_vdpv_cases,
      prop_timely_env = .data$timely_env / .data$wild_vdpv_env
    )

  pos_summary <- pos_summary |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c(
      "prop_timely_cases",
      "prop_timely_env"
    )), \(x) if_else(x %in% c(Inf, NaN), 0, x))) |>
    dplyr::ungroup()

  return(pos_summary)
}

# Public functions ----

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
#' @param lab_locs `tibble` Summary of the sequencing capacities of labs.
#' Output of [get_lab_locs()]. Defaults to `NULL`, which will download the information
#' directly from EDAV.
#' @param risk_table `tibble` GPSAP risk categorization for each country.
#' @param rolling `bool` Should the data be summarized for 12-month rolling averages. Defaults to `TRUE`.
#'
#' @return `tibble` Summary table of GPSAP KPIs.
#' @export
#'
#' @examples
#' raw_data <- get_all_polio_data(attach.spatial.data = FALSE)
#' c1 <- generate_c1_table(raw_data, "2021-01-01", "2023-12-31")
generate_c1_table <- function(raw_data, start_date, end_date,
                              lab_locs = NULL, risk_table = NULL,
                              rolling = T) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  # Filtering
  afp_data <- raw_data$afp |>
    dplyr::filter(
      dplyr::between(dateonset, start_date, end_date),
      cdc.classification.all2 != "NOT-AFP"
    ) |>
    dplyr::select(-dplyr::starts_with("pons"))

  es_data <- raw_data$es |>
    dplyr::filter(dplyr::between(collect.date, start_date, end_date))

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

  # Include required columns
  afp_data <- col_to_datecol(afp_data)
  afp_data <- add_rolling_date_info(afp_data, start_date, end_date, "date")
  es_data <- add_rolling_date_info(es_data, start_date, end_date, "collect.date")

  # Calculate country indicators
  afp_indicators <- afp_data |>
    dplyr::group_by(.data$year.analysis, .data$rolling_period) |>
    dplyr::summarise(
      npafp_dist = list(f.npafp.rate.01(dplyr::pick(dplyr::everything()),
                                        raw_data$dist.pop,
                                        min(.data$year.analysis.start),
                                        max(.data$year.analysis.end),
                                        "dist", rolling = TRUE,
                                        sp_continuity_validation = FALSE)),
      stoolad_dist = list(f.stool.ad.01(dplyr::pick(dplyr::everything()),
                                        raw_data$dist.pop,
                                        min(year.analysis.start),
                                        max(year.analysis.end),
                                        "dist", rolling = TRUE,
                                        sp_continuity_validation = FALSE))
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(-dplyr::any_of(c("year.analysis", "rolling.period")),
                                \(x) list(dplyr::tibble(x) |>
                                            dplyr::mutate(year.analysis = year.analysis,
                                                          rolling_period = rolling_period
                                            )))
    ) |>
    dplyr::ungroup()

  es_indicators <- es_data |>
    dplyr::group_by(.data$year.analysis, .data$rolling_period) |>
    dplyr::summarise(ev_rate = list(f.ev.rate.01(dplyr::pick(dplyr::everything()),
                                                 min(.data$year.analysis.start),
                                                 max(.data$year.analysis.end)))) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(-dplyr::any_of(c("year.analysis", "rolling.period")),
                                \(x) list(dplyr::tibble(x) |>
                                            dplyr::mutate(year.analysis = year.analysis,
                                                          rolling_period = rolling_period
                                            )))
    ) |>
    dplyr::ungroup()

  timely_det_indicator <- generate_wild_vdpv_summary(raw_data,
                                                     start_date, end_date,
                                                     risk_table = risk_table,
                                                     lab_locs = lab_locs)

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
    dplyr::group_by(year.analysis, rolling_period, whoregion, ctry) |>
    dplyr::summarise(dist_w_100k = sum(par >= 1e5),
                     dist_npafp = n(),
                     met_npafp = sum(
                       (par >= 1e5 & npafp_rate >= 2 & whoregion %in% c("AFRO", "EMRO", "SEARO")),
                       (par >= 1e5 & npafp_rate >= 1 & whoregion %in% c("AMRO", "EURO", "WPRO")),
                       # !!! NEED Endemics and OBX affected logic here
                       na.rm = T),
                     prop_met_npafp = met_npafp / dist_w_100k) |>
    ungroup()

  met_stool <- dplyr::bind_rows(afp_indicators$stoolad_dist) |>
    dplyr::left_join(dist_lookup_table) |>
    dplyr::left_join(region_lookup_table) |>
    dplyr::filter(!is.na(ctry)) |>
    dplyr::group_by(year.analysis, rolling_period, whoregion, ctry) |>
    dplyr::summarise(dist_stool = n(),
                     met_stool = sum(per.stool.ad >= 0.8 & adequacy.denominator >= 5, na.rm = T),
                     prop_met_stool = met_stool / dist_stool)

  met_ev <- dplyr::bind_rows(es_indicators$ev_rate) |>
    dplyr::rename("ctry" = ADM0_NAME,
                  "prov" = ADM1_NAME,
                  "dist" = ADM2_NAME) |>
    dplyr::left_join(dist_lookup_table) |>
    dplyr::left_join(region_lookup_table) |>
    dplyr::group_by(year.analysis, rolling_period, whoregion, ctry) |>
    dplyr::summarise(es_sites = sum(num.samples >= 10),
                     met_ev = sum(num.samples >= 10 & ev.rate >= 0.5, na.rm = T),
                     prop_met_ev = met_ev / es_sites)

  combine <- dplyr::full_join(met_npafp, met_stool) |>
    dplyr::full_join(met_ev) |>
    dplyr::full_join(timely_det_indicator |>
                       rename("ctry" = .data$place.admin.0)) |>
    dplyr::select(dplyr::any_of(c(
      "year.analysis", "rolling_period", "whoregion", "SG Priority Level", "ctry",
      "prop_met_npafp", "prop_met_stool", "prop_met_ev",
      "prop_timely_samples")
    ))

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
#' @param .group_by `list` a list of strings to group the data by. Defaults to
#' `adm0guid, ctry, year`.
#'
#' @return `tibble` Summary table containing AFP KPIs.
#' @export
#'
#' @examples
#' raw_data <- get_all_polio_data(attach.spatial.data = FALSE)
#' c2 <- generate_c2_table(raw_data$afp, raw_data$ctry.pop, "2021-01-01", "2023-12-31")
generate_c2_table <- function(afp_data, pop_data, start_date, end_date,
                              .group_by = c("adm0guid", "ctry", "year")) {
  # Adjust spatial scale for stool adequacy and NPAFP functions
  .spatial_scale <- dplyr::case_when(
    c("dist", "adm2guid") %in% .group_by ~ "dist",
    c("prov", "adm1guid") %in% .group_by ~ "prov",
    c("ctry", "adm0guid") %in% .group_by ~ "ctry"
  )
  .spatial_scale <- dplyr::case_when(
    "dist" %in% .spatial_scale ~ "dist",
    "prov" %in% .spatial_scale ~ "prov",
    "ctry" %in% .spatial_scale ~ "ctry"
  )

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

  # Add required columns
  afp_data <- afp_data |>
    add_seq_capacity() |>
    col_to_datecol()

  # NPAFP
  npafp <- f.npafp.rate.01(afp_data, pop_data, start_date, end_date, .spatial_scale,
    pending = TRUE, rolling = FALSE,
    sp_continuity_validation = FALSE
  )
  # Stool Adequacy
  stool_ad <- f.stool.ad.01(afp_data, pop_data, start_date, end_date, .spatial_scale,
    missing = "good", bad.data = "inadequate",
    rolling = FALSE, sp_continuity_validation = FALSE
  )
  # Stool Condition
  stool_condition <- generate_stool_data(afp_data, start_date, end_date,
    missing = "good",
    bad.data = "inadequate"
  ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.group_by))) |>
    dplyr::summarize(
      afp_cases = sum(.data$cdc.classification.all2 != "NOT-AFP"),
      good_samples = sum(.data$adequacy.final2 == "Adequate"),
      prop_good_condition = good_samples / afp_cases
    )

  # Completeness of Contact Sampling
  # Calculated in future versions

  # Completeness of 60-day follow-ups
  complete_60_day <- generate_stool_data(afp_data, start_date, end_date,
    missing = "good",
    bad.data = "inadequate"
  ) |>
    # Note that this also filters out "NOT-AFP" cases
    generate_60_day_table_data(start_date, end_date) |>
    dplyr::left_join(afp_data |>
      dplyr::select(dplyr::all_of(c("epid", "adm0guid")))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.group_by))) |>
    dplyr::summarize(prop_complete_60_day = sum(.data$ontime.60day == 1, na.rm = TRUE) /
      sum(adequacy.final2 == "Inadequate"))

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
    dplyr::group_by(dplyr::across(dplyr::all_of(.group_by))) |>
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

  # Completeness of weekly zero reporting
  # Timeliness of WZR
  # Adequacy of active surveillance sites
  # AFP case encounters

  results <- dplyr::full_join(npafp, stool_ad) |>
    dplyr::full_join(stool_condition) |>
    dplyr::full_join(complete_60_day) |>
    dplyr::full_join(timeliness_summary)

  # Select only required columns
  results <- results |>
    dplyr::select(
      dplyr::any_of(.group_by),
      "npafp_rate", "per.stool.ad",
      "prop_good_condition":"timely_wpv_vdpv"
    )

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
#' @param .group_by `list` How results should be grouped. By default, the summary
#' is grouped by country, site, and reporting year.
#'
#' @return `tibble` A summary table of environmental surveillance KPIs.
#' @export
#'
#' @examples
#' raw_data <- get_all_polio_data(attach.spatial.data = FALSE)
#' c3 <- generate_c3_table(raw_data$es, "2021-01-01", "2023-12-31")
generate_c3_table <- function(es_data, start_date, end_date,
                              .group_by = c("ADM0_NAME", "ctry.guid", "reporting.year")) {

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  es_data <- es_data |>
    add_seq_capacity("ADM0_NAME")

  es_summary <- es_data |>
    dplyr::filter(dplyr::between(.data$collect.date, start_date, end_date)) |>
    dplyr::mutate(coltolab = difftime(.data$date.received.in.lab,
                                      .data$collect.date, units = "days"),
                  coltoresults = difftime(.data$date.final.culture.result,
                                          .data$collect.date, units = "days"),

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
                  ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.group_by)),
                                  .data$site.id, .data$site.name) |>
    dplyr::summarize(
      ev_rate = sum(.data$ev.detect == 1, na.rm = TRUE) / sum(!is.na(.data$ev.detect)) * 100,
      prop_good_es = sum(.data$sample.condition == "Good") / sum(!is.na(.data$sample.condition)) * 100,
      prop_timely_ship = sum(timely_ship == "yes") / sum(timely_ship != "unable to assess", na.rm = TRUE) * 100,
      prop_timely_det_wpv_vdpv = sum(timely_det == "yes" & is_target == TRUE, na.rm = TRUE) /
        sum(is_target == TRUE & timely_det != "unable to assess", na.rm = TRUE) * 100
    )

  return(es_summary)

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


