# Helper functions ----

#' Checks for missing geographic data
#' @description checks the AFP dataset for rows with missing geographic data.
#' It checks for missing data based on the scale passed through spatial.scale.
#' @importFrom dplyr filter
#' @importFrom cli cli_alert_warning
#' @param afp.data tibble containing AFP data
#' @param spatial.scale what geographic level to check for. Valid values are "ctry","prov", "dist".
#' @keywords internal
#'
#' @returns tibble containing records with missing geographic data
check_missing_geo <- function(afp.data, spatial.scale) {
  if (!spatial.scale %in% c("ctry", "dist", "prov")) {
    stop("Please enter a valid spatial scale.")
  }

  missing_geo <- afp.data |>
    dplyr::filter(is.na(get(spatial.scale)))

  if (nrow(missing_geo) > 0) {
    cli::cli_alert_warning(paste0(
      "There are ",
      nrow(missing_geo),
      " rows with missing ",
      spatial.scale,
      " data."
    ))
  }

  return(missing_geo)
}

#' Imputation algorithm to fill in missing district data from EPID
#'
#' @param afp.data tibble of AFP data
#' @keywords internal
#' @import cli dplyr
#' @returns tibble of AFP data with filled district data
impute_dist_afp <- function(afp.data) {
  cli::cli_process_start("Filling in missing district data in the AFP linelist")
  missing <- afp.data |>
    dplyr::filter(is.na(dist))

  cli::cli_alert_info(paste0("There are ", nrow(missing), " records missing district data"))

  # Pull out any geographic info from the epid
  missing2 <- afp.data |>
    dplyr::filter(epid %in% missing$epid) |>
    dplyr::mutate(epid.check = substr(epid, 1, 11))

  # Match epid.check with epid and year onset to pull out district and guid
  missing2 <- missing2 |>
    dplyr::mutate(adm2guid.2 = NA)

  for (i in seq_len(nrow(missing2))) {
    x <- afp.data |>
      dplyr::filter(
        year == missing2$year[i] &
          grepl(missing2$epid.check[i], afp.data$epid) == T &
          !is.na(adm2guid)
      )

    # print(dim(x))
    if (dim(x)[1] > 0 & length(unique(x$adm2guid)) == 1) {
      missing2$adm2guid.2[i] <- unique(x$adm2guid)
    }
  }

  # back fill to adm2guid
  missing2 <- missing2 |>
    dplyr::mutate(adm2guid = dplyr::if_else(is.na(adm2guid), as.character(adm2guid.2), adm2guid))

  missing3 <- missing2 |>
    dplyr::filter(is.na(adm2guid))

  for (i in seq_len(nrow(missing3))) {
    x <- afp.data |>
      dplyr::filter(grepl(missing3$epid.check[i], afp.data$epid) == T &
        !is.na(adm2guid))

    # print(dim(x))
    if (dim(x)[1] > 0 & length(unique(x$adm2guid)) == 1) {
      missing3$adm2guid.2[i] <- unique(x$adm2guid)
    }
  }

  # Backfill missing2 with new data
  missing2$dist[match(missing3$epid, missing2$epid)] <- missing3$dist
  missing2$adm2guid[match(missing3$epid, missing2$epid)] <- missing3$adm2guid.2

  # For those that have multiple adm2guids
  # Match province and take the adm2guid that goes with the province
  missing3 <- missing2 |>
    dplyr::filter(is.na(adm2guid))

  for (i in seq_len(nrow(missing3))) {
    x <- afp.data |>
      dplyr::filter(grepl(missing3$epid.check[i], afp.data$epid) == T &
        !is.na(adm2guid))

    y <- x |>
      dplyr::filter(prov == missing3$prov[i])

    if (dim(x)[1] > 0 & dim(y)[1] > 0) {
      missing3$adm2guid.2[i] <- unique(y$adm2guid)
    }
  }

  missing2$adm2guid[match(missing3$epid, missing2$epid)] <- missing3$adm2guid.2

  # add dist in to missing2
  missing2 <- missing2 |>
    dplyr::mutate(dist = dplyr::case_when(
      is.na(dist) & is.na(adm2guid) == F ~
        afp.data$dist[match(
          missing2$adm2guid,
          afp.data$adm2guid
        )],
      T ~ dist
    ))

  # Update dist and adm2guid
  afp.data$dist[match(missing2$epid, afp.data$epid)] <- missing2$dist
  afp.data$adm2guid[match(missing2$epid, afp.data$epid)] <- missing2$adm2guid

  # !!! no NAs should be present anymore
  dist_na <- sum(is.na(afp.data$dist))
  adm2guid_na <- sum(is.na(afp.data$adm2guid))


  cli::cli_alert_info("Imputation process complete.")

  if (dist_na != 0) {
    cli::cli_alert_warning(paste0("There are now ", dist_na, " records with missing district data."))
  } else {
    cli::cli_alert_success("Imputation of missing `dist` successful.")
  }

  if (adm2guid_na != 0) {
    cli::cli_alert_warning(paste0("There are now ", adm2guid_na, " records with missing adm2guid data."))
  } else {
    cli::cli_alert_success("Imputation of missing `dist` successful.")
  }

  cli::cli_progress_done()

  return(afp.data)
}

#' Convert character date columns to date columns in the AFP linelist
#'
#' @param afp.data tibble: AFP data
#' @import cli dplyr
#' @keywords internal
#' @returns AFP linelist with converted date columns and additional timeliness cols
col_to_datecol <- function(afp.data) {
  cli::cli_process_start("Converting date columns from character to dates")
  afp.data <- afp.data |>
    dplyr::mutate(dplyr::across(dplyr::contains("date"), \(x) as.Date(x))) |>
    dplyr::mutate(daysstooltolab = dplyr::case_when(
      !is.na(datestool2) ~ as.numeric(difftime(stooltolabdate, datestool2), units = "days"),
      is.na(datestool2) ~ as.numeric(difftime(stooltolabdate, datestool1), units = "days")
    )) |>
    dplyr::mutate(
      noti.7d.on = dplyr::if_else(date.notify - date.onset <= 7, T, F),
      inv.2d.noti = dplyr::if_else(date.invest - date.notify <= 2, T, F),
      coll.3d.inv = dplyr::if_else(datestool1 - date.invest <= 3, T, F),
      ship.3d.coll = dplyr::if_else(stool.date.sent.to.lab - datestool2 <= 3, T, F)
    )
  cli::cli_process_done()

  return(afp.data)
}

#' Add columns in the AFP data for number of doses and dose category
#'
#' @param afp.data AFP linelist (specifically, afp.all.2)
#' @import cli dplyr
#' @keywords internal
#' @returns tibble with AFP data with columns for number of doses and dose category
add_zero_dose_col <- function(afp.data) {
  cli::cli_process_start("Cleaning and adding columns for zero-dose children data")
  afp.data <- afp.data %>%
    dplyr::mutate(dplyr::across(
      c(
        doses.opv.routine,
        doses.opv.sia,
        doses.ipv.routine,
        doses.ipv.sia,
        doses.ipv.number
      ),
      as.numeric
    ))
  afp.data <- afp.data |>
    dplyr::mutate(dose.num.calc = rowSums(
      afp.data[, c(
        "doses.opv.routine",
        "doses.opv.sia",
        "doses.ipv.routine",
        "doses.ipv.sia",
        "doses.ipv.number"
      )] *
        (afp.data[, c(
          "doses.opv.routine",
          "doses.opv.sia",
          "doses.ipv.routine",
          "doses.ipv.sia",
          "doses.ipv.number"
        )] < 99),
      na.rm = T
    )) |>
    dplyr::mutate(
      dose.num.calc = dplyr::case_when(
        doses.opv.routine == 99 &
          doses.opv.sia == 99 &
          doses.ipv.routine == 99 &
          doses.ipv.sia == 99 &
          doses.ipv.number == 99 ~ 999,
        is.na(doses.opv.routine) &
          is.na(doses.opv.sia) &
          is.na(doses.ipv.routine) &
          is.na(doses.ipv.sia) &
          is.na(doses.ipv.number) ~ NA,
        T ~ dose.num.calc
      )
    )

  afp.data <- afp.data |>
    dplyr::mutate(
      dose.cat = dplyr::case_when(
        doses.total == 0 ~ "0",
        doses.total > 0 & doses.total < 3 ~ "1-2",
        doses.total == 3 ~ "3",
        doses.total > 3 ~ "4+",
        T ~ as.character(doses.total)
      )
    ) %>%
    dplyr::mutate(dose.cat = factor(dose.cat, levels = c(
      "Unknown",
      "4+",
      "3",
      "1-2",
      "0"
    )))

  afp.data$dose.cat <- forcats::fct_na_value_to_level(afp.data$dose.cat, "Missing")
  cli::cli_process_done()
  return(afp.data)
}


#' Create age group categories
#' @import cli dplyr
#' @param age.months column containing age in months
#' @keywords internal
#' @returns a column containing age group categories
#'
add_age_group <- function(age.months) {
  cli::cli_process_start("Adding age_group column")
  age.months <- dplyr::tibble(age.months) |>
    dplyr::mutate(age.months = as.integer(age.months)) |>
    dplyr::mutate(age_group = dplyr::case_when(
      age.months < 6 ~ "<6",
      age.months >= 6 & age.months < 60 ~ "6-59",
      age.months >= 60 & age.months < 145 ~ "60-144",
      age.months >= 145 & age.months < 180 ~ "145-179",
      age.months >= 180 ~ ">=180"
    )) |>
    dplyr::select("age_group") |>
    dplyr::pull("age_group")
  cli::cli_process_done()

  return(age.months)
}

#' Add province name back to those without district names
#' @import dplyr
#' @param npafp.output tibble output after running f.npafp.rate.01 at the district
#' level
#' @keywords internal
#' @returns tibble of output with province names for those without district names
add_prov_npafp_table <- function(npafp.output) {
  prov_na <- npafp.output |> dplyr::filter(is.na(prov))
  prov_no_na <- npafp.output |> dplyr::filter(!is.na(prov))
  prov_na$prov <- prov_no_na$prov[match(prov_na$adm1guid, prov_no_na$adm1guid)]

  npafp.output <- dplyr::bind_rows(prov_na, prov_no_na)

  return(npafp.output)
}

# Exported functions ----
#' Generate `mon.year` column in the AFP linelist.
#'
#' Generates the `mon.year` column in the AFP linelist. This column is used in
#' subsequent functions summarizing AFP case counts by geography and year. This function
#' will most likely be moved to `clean_ctry_data()`.
#' `r lifecycle::badge("deprecated")`
#'
#' The function was combined into [generate_afp_by_month_summary()].
#'
#' @import tidyr dplyr lubridate
#' @param afp.data `tibble` AFP data.
#' @param start_date `str` Start date of analysis.
#' @param end_date  `str` End date of analysis.
#'
#' @returns `tibble` AFP case count with.
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' ctry.data <- extract_country_data("algeria", raw.data)
#' afp.by.month <- generate_afp_by_month(ctry.data$afp.all.2, "2021-01-01", "2023-12-31")
#' @seealso [generate_afp_by_month_summary]
#'
generate_afp_by_month <- function(afp.data, start_date, end_date) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  # Ensure that if using raw.data, required renamed columns are present. Borrowed from
  # extract.country.data()

  afp.data <- dplyr::rename_with(afp.data, recode,
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

  summary <- afp.data |>
    dplyr::filter(dplyr::between(date, start_date, end_date)) |>
    dplyr::mutate(mon.year = lubridate::floor_date(date, "month"))

  return(summary)
}

#' Generate AFP case count summary
#'
#' Summarize AFP case counts by month and another grouping variable.
#' @import dplyr tidyr lubridate
#' @importFrom zoo as.yearmon
#' @param afp_data `tibble` AFP dataset.
#' @param pop_data `tibble` Population dataset.
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis.
#' @param by `str` How to group the data by. Either `"prov"`, `"dist"`, or `"year"`.
#' @param ctry.data `r lifecycle::badge("deprecated")` `ctry.data` is no longer supported;
#' the function will explicitly ask for the AFP dataset instead of accessing it from a list.
#' @returns `tibble` Summary table of AFP cases by month and another grouping variable.
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' ctry.data <- extract_country_data("algeria", raw.data)
#' afp.by.month <- generate_afp_by_month_summary(
#'   raw.data$afp, raw.data$ctry.pop,
#'   "2021-01-01", "2023-12-31", "ctry"
#' )
#'
#' @export
generate_afp_by_month_summary <- function(afp_data, pop_data = NULL, start_date, end_date, by,
                                          ctry.data = lifecycle::deprecated()) {
  if (lifecycle::is_present(ctry.data)) {
    lifecycle::deprecate_warn(
      when = "1.3.0",
      what = "generate_afp_by_month_summary(ctry.data)",
      details = "Arguments must be passed to `afp_data` and `pop_data` directly."
    )
  }

  # Check if population data is the same
  switch(by,
    "ctry" = {
      if (sum(c("adm0guid") %in% names(pop_data)) != 1) {
        cli::cli_abort("Please check if the pop data matches the argument passed in the `by` parameter.")
      }
    },
    "prov" = {
      if (sum(c("adm1guid", "adm2guid") %in% names(pop_data)) != 1) {
        cli::cli_abort("Please check if the pop data matches the argument passed in the `by` parameter.")
      }
    },
    "dist" = {
      if (sum(c("adm1guid", "adm2guid") %in% names(pop_data)) != 2) {
        cli::cli_abort("Please check if the pop data matches the argument passed in the `by` parameter.")
      }
    }
  )

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

  if (!is.null(pop_data)) {
    pop_data <- dplyr::rename_with(pop_data, recode,
                                   u15pop.prov = "u15pop",
                                   ADM0_NAME = "ctry",
                                   ADM1_NAME = "prov",
                                   ADM2_NAME = "dist")
    pop_data <- pop_data |>
      dplyr::filter(dplyr::between(year, lubridate::year(start_date), lubridate::year(end_date)))

    mon_year <- dplyr::tibble(
      mon.year = seq(lubridate::floor_date(start_date, "month"), end_date, by = "month"),
      year = lubridate::year(.data$mon.year)) |>
      dplyr::full_join(pop_data)
  }

  afp_data <- afp_data |>
    dplyr::filter(cdc.classification.all2 != "NOT-AFP",
                  # NOTE: this also filters NA values
                  dplyr::between(date, start_date, end_date)) |>
    dplyr::mutate(mon.year = lubridate::floor_date(date, "month"))

  afp_summary <- switch(by,
    "ctry" = {
      afp_data |>
        dplyr::group_by(adm0guid, mon.year, year) |>
        dplyr::summarize(cases = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::full_join(mon_year |>
                           dplyr::select("mon.year", "year", "ctry", "adm0guid", "u15pop")) |>
        tidyr::replace_na(list(cases = 0))
    },
    "prov" = {
      afp_data |>
        dplyr::group_by(adm1guid, mon.year, year) |>
        dplyr::summarize(cases = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::full_join(mon_year |>
                            select("mon.year", "year", "ctry", "prov", "adm1guid", "u15pop")) |>
        tidyr::replace_na(list(cases = 0))
    },
    "dist" = {
      afp_data |>
        dplyr::group_by(adm2guid, mon.year, year) |>
        dplyr::summarize(cases = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::full_join(mon_year |>
                            select("mon.year", "year", "ctry", "prov", "dist", "adm2guid", "u15pop")) |>
        tidyr::replace_na(list(cases = 0))
    },
    "year" = {
      afp_data |>
        dplyr::filter(dplyr::between(.data$date, start_date, end_date)) |>
        dplyr::group_by(.data$year) |>
        dplyr::summarize(afp.case = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::full_join(dplyr::tibble(year = seq(lubridate::year(start_date),
                                                  lubridate::year(end_date)))) |>
        tidyr::replace_na(list(afp.case = 0))
    }
  )

  if (by != "year") {
    afp_summary <- afp_summary |>
      dplyr::mutate(
        mon.year = lubridate::as_date(zoo::as.yearmon(.data$mon.year, "%b-%y")),
        case.cat = dplyr::case_when(
          cases == 0 ~ "0",
          cases == 1 ~ "1",
          cases > 1 & cases < 6 ~ "2-5",
          cases >= 6 & cases < 10 ~ "6-9",
          cases >= 10 ~ "10+"
        ),
        year = lubridate::year(.data$mon.year),
        mononset = lubridate::month(.data$mon.year)
      ) |>
      dplyr::mutate(case.cat = factor(
        .data$case.cat,
        levels = c("0", "1", "2-5", "6-9", "10+"),
        labels = c("0", "1", "2-5", "6-9", "10+")
      )) |>
      dplyr::filter(!is.na(.data$year))
  }

  return(afp_summary)
}

#' NPAFP indicator tables with additional columns
#'
#' The function adds additional information to the NPAFP table. In particular,
#' the number of AFP cases based on the geographic grouping selected. It also
#' adds a column for the number of WPV, VDPV1-3 cases. However, in a future release,
#' these columns might be incorporated within the [f.npafp.rate.01()] function.
#'
#' @import dplyr
#' @param npafp.output `tibble` Output of running [f.npafp.rate.01()].
#' @param afp.data `tibble` AFP linelist. Either `raw.data$afp` or `ctry.data$afp.all.2`.
#' @param spatial.scale `str` Spatial scale to analyze. Valid values are `"ctry", "prov", "dist"`.
#' @param start_date `str` Start date of the desk review.
#' @param end_date `str` End date of the desk review.
#'
#' @returns `tibble` NPAFP rate table with additional columns related to case counts.
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' ctry.ind <- f.npafp.rate.01(raw.data$afp, raw.data$ctry.pop,
#'   "2021-01-01", "2023-12-31", "ctry",
#'   sp_continuity_validation = FALSE
#' )
#' ctry.ind <- prep_npafp_table(
#'   ctry.ind, raw.data$afp,
#'   "2021-01-01", "2023-12-31", "ctry"
#' )
#'
#' @export
prep_npafp_table <- function(npafp.output, afp.data, start_date, end_date, spatial.scale) {
  geo <- switch(spatial.scale,
    "ctry" = "adm0guid",
    "prov" = "adm1guid",
    "dist" = "adm2guid"
  )
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  # If using raw.data ensure required columns are present
  afp.data <- dplyr::rename_with(afp.data, recode,
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



  # afp.data should have already been filtered for start and end dates
  afp.data <- afp.data |> dplyr::filter(dplyr::between(date, start_date, end_date))
  cases <- afp.data |>
    dplyr::group_by(get(geo), year) |>
    dplyr::summarize(
      afp.case = sum(!is.na(cdc.classification.all2), na.rm = T),
      num.wpv.cases = sum(wild.1 == TRUE, wild.3 == TRUE, na.rm = T),
      num.vdpv1.cases = sum(vdpv.1 == TRUE, na.rm = T),
      num.vdpv2.cases = sum(vdpv.2 == TRUE, na.rm = T),
      num.vdpv3.cases = sum(vdpv.3 == TRUE, na.rm = T)
    )

  cases <- switch(spatial.scale,
    "ctry" = {
      cases <- cases |>
        dplyr::rename(adm0guid = "get(geo)")
    },
    "prov" = {
      cases <- cases |>
        dplyr::rename(adm1guid = "get(geo)")
    },
    "dist" = {
      cases <- cases |>
        dplyr::rename(adm2guid = "get(geo)")
    }
  )

  case.ind <- switch(spatial.scale,
    "ctry" = {
      case.ind <-
        dplyr::left_join(npafp.output, cases, by = c(
          "adm0guid" = "adm0guid",
          "year" = "year"
        ))
    },
    "prov" = {
      case.ind <- dplyr::full_join(npafp.output,
        cases,
        by = c(
          "adm1guid" = "adm1guid",
          "year" = "year"
        )
      ) |>
        dplyr::select(-"adm1guid")
    },
    "dist" = {
      case.ind <-
        dplyr::full_join(npafp.output,
          cases,
          by = c(
            "adm2guid" = "adm2guid",
            "year" = "year"
          )
        ) |>
        dplyr::select(-"adm1guid", -"adm2guid")
    }
  )

  return(case.ind)
}

#' Generate a summary table for sample timeliness intervals
#'
#' The summary table will output timeliness intervals of samples from collection to
#' lab testing. Lab timeliness will only be calculated if the lab data is attached. Otherwise,
#' by default, the function will return only the timeliness intervals up to when the samples were
#' sent to lab.
#'
#' @import dplyr lubridate tidyr
#' @param ctry.data `list` Large list containing polio country data. This is the output of
#' [init_dr()] or [extract_country_data()]. Make sure that `afp.all.2` has been cleaned.
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis.
#' @param spatial.scale `str` Scale to summarize to. Valid values are: `"ctry" or "prov"`. `"dist"` not available currently.
#' @param lab.data `tibble` Summarized lab data, if available. This parameter will calculate timeliness intervals in the lab. Otherwise,
#' only the field component will be presented. This is the output of [generate_lab_timeliness()].
#' @returns `tibble` A table summarizing median days for different timeliness intervals.
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' ctry.data <- extract_country_data("algeria", raw.data)
#' ctry.data <- clean_ctry_data(ctry.data)
#' int.data <- generate_int_data(ctry.data, "2021-01-01", "2023-12-31", "ctry") # lab data not attached
#' \dontrun{
#' # If lab data is available. Assume ctry.data is loaded.
#' lab_path <- "C:/Users/ABC1/Desktop/algeria_lab.csv"
#' lab.data <- readr::read_csv(lab_path)
#' int.data <- generate_int_data(ctry.data, "2021-01-01", "2023-12-31", "ctry", lab.data)
#' }
#'
#' @seealso [clean_ctry_data()]
#' @export
generate_int_data <- function(ctry.data, start_date, end_date, spatial.scale, lab.data = NULL) {
  if (!"daysstooltolab" %in% names(ctry.data$afp.all.2)) {
    cli::cli_abort("'daysstooltolab' column is missing.
                   Please run 'clean_ctry_data()' and try again.")
  }

  stool_to_lab_name <- dplyr::if_else(is.null(lab.data), "daysstooltolab", "days.collect.lab")

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  afp.data <- ctry.data$afp.all.2 |>
    dplyr::filter(dplyr::between(date, start_date, end_date))

  select_criteria <- NULL
  select_criteria <- switch(spatial.scale,
    "ctry" = {
      c(
        "epid", "ontonot", "nottoinvest", "investtostool1",
        "stool1tostool2", stool_to_lab_name, "year",
        "adm0guid", "ctry"
      )
    },
    "prov" = {
      c(
        "epid", "ontonot", "nottoinvest", "investtostool1",
        "stool1tostool2", stool_to_lab_name, "year",
        "adm0guid", "adm1guid", "ctry", "prov"
      )
    }
  )
  as_num_conversion <- c(stool_to_lab_name, "ontonot", "nottoinvest", "investtostool1", "stool1tostool2")


  int.data <- switch(spatial.scale,
    "ctry" = {
      int.data <- afp.data |>
        dplyr::mutate(year = lubridate::year(date)) |>
        dplyr::group_by(adm0guid, year) |>
        dplyr::select(any_of(select_criteria)) |>
        dplyr::mutate(dplyr::across(
          dplyr::any_of(as_num_conversion), \(x) as.numeric(x)
        ))
    },
    "prov" = {
      int.data <- afp.data |>
        dplyr::mutate(year = lubridate::year(date)) |>
        dplyr::group_by(adm1guid, year) |>
        dplyr::select(dplyr::any_of(select_criteria)) |>
        dplyr::mutate(dplyr::across(
          dplyr::any_of(as_num_conversion), \(x) as.numeric(x)
        ))
    }
  )

  int.data <- switch(spatial.scale,
    "ctry" = {
      int.data <- int.data |>
        tidyr::pivot_longer(!c("epid", "year", "adm0guid", "ctry"),
          names_to = "type",
          values_to = "value"
        ) |>
        dplyr::group_by(year, type, adm0guid, ctry) |>
        dplyr::summarize(
          medi = median(value, na.rm = T),
          freq = sum(!is.na(value))
        )
    },
    "prov" = {
      int.data <- int.data |>
        tidyr::pivot_longer(
          !c("epid", "year", "adm0guid", "adm1guid", "prov", "ctry"),
          names_to = "type",
          values_to = "value"
        ) |>
        dplyr::group_by(year, type, adm1guid, prov, ctry) |>
        dplyr::summarize(
          medi = median(value, na.rm = T),
          freq = sum(!is.na(value))
        )
    }
  )

  if (!is.null(lab.data)) {
    int.data <- dplyr::bind_rows(lab.data, int.data)
  }

  if (spatial.scale == "prov") {
    int.data$prov <- ctry.data$prov.pop$prov[match(int.data$adm1guid, ctry.data$prov.pop$adm1guid)]
  } else if (spatial.scale == "ctry") {
    int.data$ctry <- ctry.data$ctry.pop$ctry[match(int.data$adm0guid, ctry.data$ctry.pop$adm0guid)]
  }

  # Filtering based on whether labs are attached
  if (is.null(lab.data)) {
    int.data <- int.data |>
      dplyr::filter(type %in% c(
        "ontonot",
        "nottoinvest",
        "investtostool1",
        "stool1tostool2",
        "daysstooltolab"
      ))
  } else {
    who.additional.cols <- c(
      "days.coll.sent.field",
      "days.sent.field.rec.nat",
      "days.rec.nat.sent.lab",
      "days.sent.lab.rec.lab",
      "days.rec.lab.culture"
    )
    int.data.filter <- int.data |>
      dplyr::filter(type %in% who.additional.cols) |>
      dplyr::summarise(sum = sum(.data$medi)) |>
      dplyr::pull()

    if (is.na(int.data.filter)) {
      int.data <- int.data |>
        dplyr::filter(!type %in% who.additional.cols)
    } else {
      int.data <- int.data |>
        dplyr::filter(!type %in% c(
          "days.collect.lab",
          "days.lab.culture"
        ))
    }
  }

  levs <- c(
    # For no lab data attached or using regional lab data
    "ontonot" = "Paralysis onset to notification",
    "nottoinvest" = "Case notification to investigation",
    "investtostool1" = "Case investigation to stool 1 collection",
    "stool1tostool2" = "Stool 1 collection to stool 2 collection",
    "daysstooltolab" = "Last stool collection sent to lab", # if no lab data
    "days.collect.lab" = "Last stool collection to received in lab",
    "days.lab.culture" = "Stool received lab to final culture results",

    # For WHO data
    "days.coll.sent.field" = "Collection to sent from field",
    "days.sent.field.rec.nat" = "Sent from field to received nat level",
    "days.rec.nat.sent.lab" = "Received nat level to sent to lab",
    "days.sent.lab.rec.lab" = "Sent to lab to received at lab",
    "days.rec.lab.culture" = "Received at lab to culture results",

    # Extra intervals we don't use, filtered
    "days.seq.ship" = "Isolate received for sequencing to sequence results available",
    "days.lab.seq" = "Stool received in lab to sequence result",
    "days.itd.seqres" = "Final rRT-PCR results to sequence result",
    "days.itd.arriveseq" = "Final rRT-PCR results to isolate received for sequencing",
    "days.seq.rec.res" = "Isolate received for sequencing to sequence result"
  )

  int.data <- int.data |>
    dplyr::filter(!type %in% c(
      "days.seq.ship",
      "days.lab.seq",
      "days.itd.seqres",
      "days.itd.arriveseq",
      "days.seq.rec.res"
    )) |>
    dplyr::mutate(type = ordered(.data$type,
      levels = names(levs),
      labels = levs
    )) |>
    dplyr::arrange(year)

  # Remove columns containing only NA values
  int.data <- int.data |>
    dplyr::select(dplyr::where(function(x) !all(is.na(x))))

  return(int.data)
}

#' Generate summary table for those requiring 60-day follow-up
#'
#' The 60-day table highlights the number of cases per year that need 60-day follow-up. It
#' summarizes the number of cases due for follow up, those with recorded follow ups, number missing
#' follow ups, and compatible cases.
#'
#' @import dplyr lubridate
#' @param stool.data `tibble` AFP data with stool adequacy columns. This is the output of
#' [generate_stool_data()].
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis.
#' @returns `tibble` A summary table for those requiring 60-day follow-up.
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' ctry.data <- extract_country_data("algeria", raw.data)
#' stool.data <- generate_stool_data(
#'   ctry.data$afp.all.2,
#'   "2021-01-01", "2023-12-31",
#'   "good", "inadequate"
#' )
#' table60.days <- generate_60_day_table_data(stool.data, "2021-01-01", "2023-12-31")
#'
#' @export
generate_60_day_table_data <- function(stool.data, start_date, end_date) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  stool.data.inad <- stool.data |>
    dplyr::filter(cdc.classification.all2 != "NOT-AFP") |>
    dplyr::mutate(
      stl.adeq.02 = dplyr::case_when(
        bad.stool1 == "data entry error" | bad.stool1 == "date before onset" | bad.stool1 == "date onset missing" ~ 77,
        bad.stool2 == "data entry error" | bad.stool2 == "date before onset" | bad.stool2 == "date onset missing" ~ 77,
        ontostool1 <= 13 & ontostool1 >= 0 & ontostool2 <= 14 & ontostool2 >= 1 & stool1tostool2 >= 1 &
          is.na(stool1tostool2) == F &
          (stool.1.condition == "Good" |
            is.na(stool.1.condition)) &
          (stool.2.condition == "Good" |
            is.na(stool.2.condition)) ~ 1,
        ontostool1 > 13 |
          ontostool1 < 0 | ontostool2 > 14 |
          ontostool2 < 1 |
          stool1tostool2 < 1 |
          is.na(stool1tostool2) == T |
          stool.1.condition == "Poor" |
          stool.2.condition == "Poor" ~ 0
      )
    )

  cases.need60day <- stool.data.inad |>
    dplyr::as_tibble() |>
    # filter onset to be >120 days from system date
    # dplyr::filter(date <= lubridate::as_date(dplyr::if_else(
    #   end_date > Sys.Date() - 120, (Sys.Date() - 120),
    #   end_date
    # ))) |>
    dplyr::mutate(
      due.60followup = dplyr::if_else(date <= (Sys.Date() - days(120)), 1, 0),
      need60.sys.date = Sys.Date()
    ) |> # needed to record when the table was created
    dplyr::filter(dplyr::between(date, start_date, end_date)) |>
    dplyr::mutate(need60day.v2 = dplyr::if_else(.data$adequacy.final == "Inadequate" &
      .data$due.60followup == 1, 1, 0)) |>
    # dplyr::filter(need60day.v2 == 1 |
    #   cdc.classification.all2 == "COMPATIBLE") |>
    dplyr::mutate(
      got60day =
        dplyr::case_when(
          need60day.v2 == 1 & is.na(followup.date) == F ~ 1,
          need60day.v2 == 1 &
            is.na(followup.date) == T & is.na(followup.findings) == F ~ 1,
          # If follow up date is missing, but findings are recorded, counts as getting follow up
          need60day.v2 == 1 &
            is.na(followup.date) == T & is.na(followup.findings) == T ~ 0,
          need60day.v2 == 0 ~ 99
        ),
      timeto60day = followup.date - date,
      ontime.60day =
        dplyr::case_when(
          need60day.v2 == 0 ~ 99,
          # excluded timely cases
          need60day.v2 == 1 &
            timeto60day >= 60 & timeto60day <= 90 ~ 1,
          (
            need60day == 1 &
              timeto60day < 60 | timeto60day > 90 | is.na(timeto60day) == T
          ) ~ 0
        )
    ) |>
    # note if variables are all missing then this definition needs to be adjusted

    dplyr::mutate(
      pot.compatible = dplyr::if_else(
        (
          followup.findings == "Residual weakness/paralysis" |
            followup.findings == "Died before follow-up" |
            followup.findings == "Lost to follow-up" |
            (
              is.na(followup.date) &
                followup.findings != "No residual weakness/paralysis"
            )
        ) &
          (doses.total < 3 | is.na(doses.total) == T) &
          (
            classification %in% c("Discarded", "Pending") |
              is.na(classification) == T
          ) &
          (age.months < 60 | is.na(age.months)),
        1,
        0
      ),
      pot.compatible = dplyr::if_else(is.na(pot.compatible) == T, 0, pot.compatible),
      hot.case.no.review = dplyr::if_else(
        hot.case == 1 &
          is.na(followup.date) == T &
          (
            cdc.classification.all2 == "PENDING" |
              cdc.classification.all2 == "LAB PENDING"
          ),
        1,
        0
      )
    ) |>
    dplyr::mutate(missing.fu.date = dplyr::if_else(
      need60day.v2 == 1 &
        is.na(followup.date) == T & is.na(followup.findings) == F,
      1,
      0
    )) |>
    dplyr::select(
      "epid",
      "year",
      "age.months",
      "hot.case",
      "hot.case.no.review",
      "got60day",
      "ontime.60day",
      "ctry",
      "prov",
      "dist",
      "date",
      "date.notify",
      "date.invest",
      "datestool1",
      "datestool2",
      "stool.1.condition",
      "stool.2.condition",
      "adequacy.03",
      "paralysis.asymmetric",
      "paralysis.rapid.progress",
      "paralysis.onset.fever",
      "pot.compatible",
      "doses.total",
      "timeto60day",
      "followup.date",
      "classification",
      "cdc.classification.all2",
      "missing.fu.date",
      "adm1guid",
      "adm2guid",
      "need60day",
      "due.60followup",
      "need60.sys.date",
      "need60day.v2",
      "adequacy.final",
      "adequacy.final2"
    )

  return(cases.need60day)
}

#' Generate a summary of AFP samples by year
#'
#' Generates a summary table of the number of AFP cases per country and year.
#' This function is used primarily with [generate_ctry_timeliness_graph()] as a
#' label of the y-axis.
#' @param ctry.data `list` Large list containing country polio data. This is the output
#' of [init_dr()] or [extract_country_data()].
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis.
#' @returns `tibble` A table containing summary of AFP cases by year and country.
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' ctry.data <- extract_country_data("algeria", raw.data)
#' ctry.labels <- generate_year_lab(ctry.data, "2021-01-01", "2023-12-31")
#'
#' @export
generate_year_lab <- function(ctry.data, start_date, end_date) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  afp.year.lab <- ctry.data$afp.all.2 |>
    dplyr::filter(
      dplyr::between(.data$date, start_date, end_date),
      cdc.classification.all2 != "NOT-AFP"
    ) |>
    dplyr::count(.data$ctry, .data$adm0guid, .data$year) |>
    dplyr::mutate(labs = paste0(
      year,
      " (N=", n, ")"
    ))

  return(afp.year.lab)
}

#' Generate a summary of samples by year and province
#'
#' Generates a summary table of the number of AFP cases per province and year.
#' This function is used primarily with [generate_prov_timeliness_graph()] as a
#' label of the y-axis.
#' @param ctry.data `list` Large list containing country polio data. This is the output
#' of [init_dr()] or [extract_country_data()].
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis.
#' @returns `tibble` A table containing summary of AFP cases by year and province.
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' ctry.data <- extract_country_data("algeria", raw.data)
#' prov.labels <- generate_prov_year_lab(ctry.data, "2021-01-01", "2023-12-31")
#'
#' @export
generate_prov_year_lab <- function(ctry.data, start_date, end_date) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  afp.prov.year.lab <- ctry.data$afp.all.2 |>
    dplyr::filter(dplyr::between(.data$date, start_date, end_date)) |>
    dplyr::count(prov, adm1guid, year) |>
    dplyr::mutate(labs = paste0(
      year,
      " (N=", n, ")"
    ))

  return(afp.prov.year.lab)
}

#' Creating a table of compatible and potentially compatible cases
#'
#' Creates a table of compatible and potentially compatible cases, with an
#' optional parameter to run a clustering algorithm.
#' @import dplyr
#' @param cases.need60day `tibble` Summary table of cases that need 60-day follow-up.
#' This is the output of [generate_60_day_table_data()].
#' @param create_cluster `bool` Add column for clusters? Default to `FALSE`.
#' @returns `tibble` A summary table of cases.
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' ctry.data <- extract_country_data("algeria", raw.data)
#' stool.data <- generate_stool_data(
#'   ctry.data$afp.all.2, "2021-01-01", "2023-12-31",
#'   "good", "inadequate"
#' )
#' table60.days <- generate_60_day_table_data(stool.data, "2021-01-01", "2023-12-31")
#' pot.c.clust <- generate_potentially_compatibles_cluster(table60.days,
#'   create_cluster = TRUE
#' )
#'
#' @export
generate_potentially_compatibles_cluster <- function(cases.need60day, create_cluster = F) {
  pot.c.clust <- cases.need60day |>
    dplyr::filter(pot.compatible == 1 | classification == "Compatible") |>
    dplyr::mutate(
      class = dplyr::case_when(
        pot.compatible == 1 ~ "Potentially compatible",
        classification == "Compatible" ~ "Compatible"
      )
    )

  # add 30 days to the first case --> pull out all of the cases that fall within
  # 30 days of that case --> label them as #1 --> then take the subset that is #1 -->
  # add 30 days to the max of that --> pull out any that fit etc --> once you
  # don't have anymore that match - pick the minimum date that is NA --> start the
  # process over

  # All compatible and potentially compatibles that are within 30 days of another case - clust
  pot.c.clust <- dplyr::arrange(pot.c.clust, date) # arrange by onset date

  if (!create_cluster) {
    return(pot.c.clust)
  }

  cli::cli_process_start("Performing clustering assignment.")
  pot.c.clust$clust <- NA
  pot.c.clust$clust[1] <- 1

  if (nrow(pot.c.clust) > 1) {
    for (i in 2:nrow(pot.c.clust)) {
      pot.c.clust$clust[i] <- dplyr::if_else(pot.c.clust$date[i] <= pot.c.clust$date[i - 1] + 30,
        max(pot.c.clust$clust[1:i - 1], na.rm = T),
        max(pot.c.clust$clust[1:i - 1], na.rm = T) + 1
      )
    }
  }

  # Rolling cluster assignment
  x <- NULL
  y <- NULL
  for (i in 1:length(unique(pot.c.clust$clust))) {
    x <- dplyr::filter(pot.c.clust, clust == i)
    x$geo.clust.prov <- vctrs::vec_duplicate_detect(x$prov)
    y <- dplyr::bind_rows(y, x)
  }
  # If there is a province that is the same, geo.clust.prov will be TRUE

  cli::cli_process_done()

  return(y)
}

#' Check data quality errors from the country data
#'
#' Performs a check for different errors in the AFP linelist and population files.
#' It also alerts the users for GUIDs that have changed.
#'
#' @import cli writexl
#' @param ctry.data `list` Large list containing polio country data. This is the output of
#' [extract_country_data()] or [init_dr()].
#' @param error_path `str` Path where to store checks in `ctry.data`.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' ctry_data_errors(ctry.data)
#' }
#'
#' @export
ctry_data_errors <- function(ctry.data,
                             error_path = Sys.getenv("DR_ERROR_PATH")) {
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
  cli::cli_progress_step("Checking for GUIDs at the country level that changed in the desk review time period.")
  incomplete.adm.ctry <-
    spatial_validation(ctry.data$ctry.pop, "ctry")
  missing.pop.ctry <-
    check_missing_pop(ctry.data$ctry.pop, "ctry")

  cli::cli_progress_step("Checking for GUIDs at the province level that changed in the desk review time period.")
  incomplete.adm.prov <-
    spatial_validation(ctry.data$prov.pop, "prov")
  missing.pop.prov <-
    check_missing_pop(ctry.data$prov.pop, "prov")

  cli::cli_progress_step("Checking for GUIDs at the district level that changed in the desk review time period.")
  incomplete.adm.dist <-
    spatial_validation(ctry.data$dist.pop, "dist")
  missing.pop.dist <-
    check_missing_pop(ctry.data$dist.pop, "dist")

  cli::cli_process_done(msg_done = "Check returned list for error specifics.")
  cli::cli_alert("Run clean_ctry_data() to attempt data fixes and perform the check again.")

  error_log <- list()
  error_log$missing_ctry <- missing_ctry
  error_log$missing_prov <- missing_prov
  error_log$missing_dist <- missing_dist
  error_log$pop_rollup_diff <- pop_file
  error_log$guid_changes_adm0 <- incomplete.adm.ctry
  error_log$guid_changes_adm1 <- incomplete.adm.prov
  error_log$guid_changes_adm2 <- incomplete.adm.dist
  error_log$missing_pop_ctry <- missing.pop.ctry
  error_log$missing_pop_prov <- missing.pop.prov
  error_log$missing_pop_dist <- missing.pop.dist

  writexl::write_xlsx(error_log, path = file.path(error_path, "ctry_data_error_log.xlsx"))
}

#' Cleans and adds additional age and dosage number columns to the AFP linelist
#'
#' The function does additional cleaning of the `ctry.data` list. It fills in
#' missing districts, convert character date columns to a date data type, calculates
#' age group, add columns for the number of doses per case, and cleans the environmental
#' surveillance data.
#' @import cli dplyr
#' @param ctry.data `list` Large list containing polio country data. This is the output of
#' [extract_country_data()] or [init_dr()].
#'
#' @returns `list` Cleaned country data list.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' ctry.data <- clean_ctry_data(ctry.data)
#' }
#'
#' @export
clean_ctry_data <- function(ctry.data) {
  # Check if the data has already been cleaned
  if ("noti.7d.on" %in% names(ctry.data$afp.all.2) &
    "all_dets" %in% names(ctry.data$es)) {
    cli::cli_alert_warning("ctry.data already cleaned.")
    return(ctry.data)
  }

  ctry.data$afp.all.2 <- impute_dist_afp(ctry.data$afp.all.2)
  ctry.data$afp.all.2 <- col_to_datecol(ctry.data$afp.all.2)
  ctry.data$afp.all.2 <- ctry.data$afp.all.2 |>
    dplyr::mutate(age_group = add_age_group(age.months))
  ctry.data$afp.all.2 <- add_zero_dose_col(ctry.data$afp.all.2)
  ctry.data$es <- clean_es_data(ctry.data)

  return(ctry.data)
}

#' Generate stool adequacy columns in the AFP dataset
#'
#' The function adds the adequacy final column called `adequacy.final` and `adequacy.final2` into the AFP linelist. The function borrows
#' in part from [f.stool.ad.01()], so that the adequacy final column generated can match with how the stool adequacy
#' function treats bad or missing data and classify the adequacy final column. `adequacy.final` contains the original
#' classification of the sample and `adequacy.final2` contains the final classification according to how missing and bad
#' data are treated.
#'
#' Unlike the stool adequacy function, this will not filter out `NOT-AFP` cases, as it is expected for other functions
#' that use the output of this function to do the filtering. For example, [generate_60_day_table_data()].
#'
#' @import dplyr lubridate
#'
#' @param afp.data `tibble` AFP linelist. Either `ctry.data$afp.all.2`
#' @param start_date `str` Start date of the analysis.
#' @param end_date `str` End date of the analysis.
#' @param missing `str` How to treat missing data. Valid values are: `"good", "bad", "remove"`. Defaults to `"good"`.
#' When calculating the `adequacy.final` column:
#' - `"good"` uses `adequacy.03`
#' - `"bad"` uses `adequacy.01`
#' - `"exclude"` uses `adequacy.02`
#' @param bad.data `str` How to  treat bad data. Valid values are:`"remove", "inadequate"`. Defaults to `"inadequate"`.
#' `"inadequate"` treats samples with bad data as inadequate.
#' @returns `tibble` AFP linelist with stool adequacy columns.
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' stool.data <- generate_stool_data(raw.data$afp, "2021-01-01", "2023-12-31")
#'
#' @seealso [f.stool.ad.01()]
#' @export
generate_stool_data <- function(afp.data, start_date, end_date, missing = "good", bad.data = "inadequate") {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  # Ensure that if using raw.data, required renamed columns are present. Borrowed from
  # extract.country.data()

  afp.data <- dplyr::rename_with(afp.data, recode,
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

  afp.data <- afp.data |>
    dplyr::filter(dplyr::between(date, start_date, end_date))

  # generate_ad_final_col() is borrowed from f.stool.ad.01()
  stool.data <- generate_ad_final_col(afp.data)

  # Select how to treat bad data
  stool.data <- switch(bad.data,
    "remove" = {
      cli::cli_alert_warning("AFP cases with bad data excluded from stool adequacy calculation.")
      stool.data
    },
    "inadequate" = {
      stool.data |>
        dplyr::mutate(adequacy.final = dplyr::if_else(.data$adequacy.final == 77, 0, .data$adequacy.final))
    }
  )

  # Select how to treat missing data
  # adequacy.01, adequacy.02, adequacy.03 are generated in pre-processing
  stool.data <- switch(missing,
    "good" = {
      stool.data |>
        dplyr::mutate(adequacy.final2 = dplyr::if_else(.data$adequacy.final == 99, .data$adequacy.03, .data$adequacy.final))
    },
    "bad" = {
      stool.data |>
        dplyr::mutate(adequacy.final2 = dplyr::if_else(.data$adequacy.final == 99, .data$adequacy.01, .data$adequacy.final))
    },
    "exclude" = {
      cli::cli_alert_warning("AFP cases with missing adequacy excluded from stool adequacy calculation.")
      stool.data |>
        dplyr::mutate(adequacy.final2 = dplyr::if_else(.data$adequacy.final == 99, .data$adequacy.02, .data$adequacy.final))
    }
  )

  # Rename values for stool adequacy final
  stool.data <- stool.data |>
    dplyr::mutate(
      adequacy.final = dplyr::case_when(
        adequacy.final == 0 ~ "Inadequate",
        adequacy.final == 1 ~ "Adequate",
        adequacy.final == 77 ~ "Bad data",
        adequacy.final == 99 ~ "Missing",
      ),
      adequacy.final2 = dplyr::case_when(
        adequacy.final2 == 0 ~ "Inadequate",
        adequacy.final2 == 1 ~ "Adequate",
        adequacy.final2 == 77 ~ "Bad data",
        adequacy.final2 == 99 ~ "Missing",
      )
    )

  return(stool.data)
}
