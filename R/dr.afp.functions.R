
#' Check for missing geographic data from afp data
#'
#' @param afp.data tibble containing AFP data
#' @param spatial.scale what geographic level to check for
#'
#' @return tibble containing records with missing geographic data
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
#'
#' @return tibble of AFP data with filled district data
impute_dist_afp <- function(afp.data) {

  cli::cli_process_start("Filling in missing district data in the AFP linelist")
  missy <- afp.data |>
    dplyr::filter(is.na(dist))

  cli::cli_alert_info(paste0("There are ", nrow(missy), " records missing district data"))

  # Pull out any geographic info from the epid
  missy2 = afp.data |>
    dplyr::filter(epid %in% missy$epid) |>
    dplyr::mutate(epid.check = substr(epid, 1, 11))

  # Match epid.check with epid and year onset to pull out district and guid
  missy2 <- missy2 |>
    dplyr::mutate(adm2guid.2 = NA)

  for (i in seq_len(nrow(missy2))) {
    x <- afp.data |>
      dplyr::filter(
        year == missy2$year[i] &
          grepl(missy2$epid.check[i], afp.data$epid) == T &
          !is.na(adm2guid)
      )

    #print(dim(x))
    if (dim(x)[1] > 0 & length(unique(x$adm2guid)) == 1) {
      missy2$adm2guid.2[i] = unique(x$adm2guid)
    }
  }

  # back fill to adm2guid
  missy2 <- missy2 |>
    mutate(adm2guid = dplyr::if_else(is.na(adm2guid), as.character(adm2guid.2), adm2guid))

  missy3 <- missy2 |>
    dplyr::filter(is.na(adm2guid))

  for (i in seq_len(nrow(missy3))) {
    x <- afp.data |>
      dplyr::filter(grepl(missy3$epid.check[i], afp.data$epid) == T &
                      !is.na(adm2guid))

    #print(dim(x))
    if (dim(x)[1] > 0 & length(unique(x$adm2guid)) == 1) {
      missy3$adm2guid.2[i] <- unique(x$adm2guid)
    }

  }

  # Backfill missy2 with new data
  missy2$dist[match(missy3$epid, missy2$epid)] <- missy3$dist
  missy2$adm2guid[match(missy3$epid, missy2$epid)] <- missy3$adm2guid.2

  # For those that have multiple adm2guids
  # Match province and take the adm2guid that goes with the province
  missy3  <- missy2 |>
    dplyr::filter(is.na(adm2guid))

  for (i in seq_len(nrow(missy3))) {
    x <- afp.data |>
      dplyr::filter(grepl(missy3$epid.check[i], afp.data$epid) == T &
                      !is.na(adm2guid))

    y <- x |>
      dplyr::filter(prov == missy3$prov[i])

    if (dim(x)[1] > 0 & dim(y)[1] > 0) {
      missy3$adm2guid.2[i] <- unique(y$adm2guid)
    }
  }

  missy2$adm2guid[match(missy3$epid, missy2$epid)] = missy3$adm2guid.2

  # add dist in to missy2
  missy2 = missy2 |>
    dplyr::mutate(dist = dplyr::case_when(is.na(dist) & is.na(adm2guid) == F ~
                                            afp.data$dist[match(missy2$adm2guid,
                                                                afp.data$adm2guid)],
                                          T ~ dist))

  # Update dist and adm2guid
  afp.data$dist[match(missy2$epid, afp.data$epid)] <- missy2$dist
  afp.data$adm2guid[match(missy2$epid, afp.data$epid)] <- missy2$adm2guid

  #!!! no NAs should be present anymore
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
#' @param afp.data
#'
#' @return AFP linelist with converted date columns and additional timeliness cols
col_to_datecol <- function(afp.data) {

  cli::cli_process_start("Converting date columns from character to dates")
  afp.data <- afp.data |>
    dplyr::mutate(dplyr::across(contains("date"), as.Date)) |>
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

add_zero_dose_col <- function(afp.data) {
  cli::cli_process_start("Cleaning and adding columns for zero-dose children data")
  afp.data = afp.data %>%
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
    dplyr::mutate(dose.num.calc = rowSums(afp.data[, c(
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
    na.rm = T)) |>
    dplyr::mutate(
      dose.num.calc = case_when(
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
    dplyr::mutate(dose.cat = factor(dose.cat, levels = c("Unknown",
                                                         "4+",
                                                         "3",
                                                         "1-2",
                                                         "0")))

  afp.data$dose.cat = forcats::fct_na_value_to_level(afp.data$dose.cat, "Missing")
  cli::cli_process_done()
  return(afp.data)
}


#' Create age group categories
#'
#' @param age.months column containing age in months
#'
#' @return a column containing age group categories
#'
add_age_group <- function(age.months) {
  cli::cli_process_start("Adding age_group column")
  age.months <- tibble::tibble(age.months) |>
    dplyr::mutate(age.months = as.integer(age.months)) |>
    dplyr::mutate(age_group = dplyr::case_when(
      age.months < 6 ~ "<6",
      age.months >= 6 & age.months < 60 ~ "6-59",
      age.months >= 60 & age.months < 145 ~ "60-144",
      age.months >= 145 & age.months < 180 ~ "145-179",
      age.months >= 180 ~ "≥180"
    )) |>
    dplyr::select(age_group)
  cli::cli_process_done()

  return(age.months)
}


#' Generate AFP case counts by month
#' @param afp.data tibble containing AFP data (afp.all.2)
#'
#' @return tibble containing summary of AFP data
#' @export
generate_afp_by_month <- function(afp.data, start_date, end_date) {
  summary <- afp.data |>
    tidyr::drop_na(date.onset) |>
    dplyr::filter(dplyr::between(as_date(date.onset), start_date, end_date)) |>
    dplyr::mutate(mon.year = lubridate::floor_date(date, "month"))

  return(summary)
}

#' Generate summary from the afp.by.month summary tibble
#'
#' @param afp.by.month summary table of AFP cases
#' @param ctry.data RDS file containing country data
#' @param start_date start date of analysis
#' @param end_date end date of analysis
#' @param by either prov, dist, or year
#'
#' @return tibble summary table of AFP cases by month
#' @export
generate_afp_by_month_summary <- function(afp.by.month, ctry.data, start_date, end_date, by) {

  afp.by.month.summary <- switch(by,
                                 "prov" = {
                                   afp.by.month |>
                                     dplyr::group_by(adm1guid, mon.year, year) |> #changed to guid
                                     dplyr::summarise(cases = n()) |>
                                     dplyr::ungroup() |>
                                     tidyr::complete(
                                       mon.year = seq(lubridate::floor_date(start_date, "month"),
                                                      end_date, by = "month"),
                                       adm1guid = unique(ctry.data$prov.pop$adm1guid),
                                       fill = list(cases = 0)
                                     ) |>
                                     dplyr::ungroup() |>
                                     dplyr::mutate(year = year(mon.year)) |>
                                     # join this to the original  province population dataset
                                     dplyr::left_join(ctry.data$prov.pop[, c("adm1guid", "year", "prov", "u15pop")],
                                                      by = c("adm1guid" = "adm1guid",
                                                             "year" = "year")) |>
                                     dplyr::mutate(cases = dplyr::if_else(is.na(cases), 0, cases)) |>
                                     dplyr::mutate(
                                       mon.year2 = lubridate::as_date(zoo::as.yearmon(mon.year, "%b-%y")),
                                       case.cat = dplyr::case_when(
                                         cases == 0 ~ "0",
                                         cases == 1 ~ "1",
                                         cases > 1 & cases < 6 ~ "2-5",
                                         cases >= 6 & cases < 10 ~ "6-9",
                                         cases >= 10 ~ "10+"
                                       ),
                                       year = lubridate::year(mon.year2),
                                       mononset = lubridate::month(mon.year2)
                                     ) |>
                                     dplyr::mutate(case.cat = factor(
                                       case.cat,
                                       levels = c("0", "1", "2-5", "6-9", "10+"),
                                       labels = c("0", "1", "2-5", "6-9", "10+")
                                     ))
                                 },
                                 "dist" = {
                                   afp.by.month |>
                                     dplyr::group_by(dist, adm2guid, mon.year) |> #changed to guid
                                     dplyr::summarise(cases = n()) |>
                                     dplyr::ungroup() |>
                                     tidyr::complete(
                                       mon.year = seq(lubridate::floor_date(start_date, "month"),
                                                      end_date, by = "month"),
                                       adm2guid = unique(ctry.data$dist.pop$adm2guid),
                                       fill = list(cases = 0)
                                     ) |>
                                     dplyr::ungroup() |>
                                     dplyr::mutate(year = lubridate::year(mon.year)) |>
                                     dplyr::left_join(ctry.data$dist.pop[, c("adm2guid", "year", "prov", "u15pop")],
                                                      by = c("adm2guid" = "adm2guid",
                                                             "year" = "year")) |>
                                     dplyr::mutate(cases = dplyr::if_else(is.na(cases), 0, cases)) |>
                                     dplyr::mutate(
                                       mon.year2 = lubridate::as_date(zoo::as.yearmon(mon.year, "%b-%y")),
                                       case.cat = dplyr::case_when(
                                         cases == 0 ~ "0",
                                         cases == 1 ~ "1",
                                         cases > 1 & cases < 6 ~ "2-5",
                                         cases >= 6 & cases < 10 ~ "6-9",
                                         cases >= 10 ~ "10+"
                                       ),
                                       year = lubridate::year(mon.year2),
                                       mononset = lubridate::month(mon.year2)
                                     ) |>
                                     dplyr::mutate(case.cat = factor(
                                       case.cat,
                                       levels = c("0", "1", "2-5", "6-9", "10+"),
                                       labels = c("0", "1", "2-5", "6-9", "10+")
                                     ))
                                 },
                                 "year" = {
                                   afp.by.month |>
                                     dplyr::filter(dplyr::between(as.Date(date.onset), start_date, end_date)) |>
                                     dplyr::group_by(year) |>
                                     dplyr::summarise(afp.case = n())
                                 })
  return(afp.by.month.summary)
}

#' Add province name back to those without district names
#'
#' @param npafp.output tibble output after running f.npafp.rate.01 at the district
#' level
#'
#' @return tibble of output with province names for those without didtrict names
add_prov_npafp_table <- function(npafp.output) {
  prov_na <- npafp.output |> dplyr::filter(is.na(prov))
  prov_no_na <- npafp.output |> dplyr::filter(!is.na(prov))
  prov_na$prov <- prov_no_na$prov[match(prov_na$adm1guid, prov_no_na$adm1guid)]

  npafp.output <- dplyr::bind_rows(prov_na, prov_no_na)

  return(npafp.output)
}

#' Create the *.case.ind tables in desk reviews
#'
#' @param npafp.output output of running f.npafp.rate.01
#' @param afp.all.2 AFP linelist
#' @param spatial.scale spatial scale to analyze. Valid values are "ctry", "prov", "dist"
#'
#' @return table to be used in summary tables for NPAFP rates
#' @export
prep_npafp_table <- function(npafp.output, afp.all.2, start_date, end_date, spatial.scale) {

  geo <- switch(
    spatial.scale,
    "ctry" = "adm0guid",
    "prov" = "adm1guid",
    "dist" = "adm2guid"
  )
  #afp.all.2 should have already been filtered for start and end dates
  afp.all.2 <- afp.all.2 |> dplyr::filter(dplyr::between(date, start_date, end_date))
  cases <- afp.all.2 |> dplyr::group_by(get(geo), year) |>
    dplyr::summarize(afp.case = n(),
                     num.wpv.cases = sum(wild.1 == TRUE, wild.3 == TRUE, na.rm = T),
                     num.vdpv1.cases	= sum(vdpv.1  == TRUE,na.rm = T),
                     num.vdpv2.cases	= sum(vdpv.2 == TRUE, na.rm = T),
                     num.vdpv3.cases = sum(vdpv.3  == TRUE, na.rm = T))

  cases <- switch(spatial.scale,
                  "ctry" = {
                    cases <- cases |>
                      dplyr::rename(ctry = `get(geo)`)
                  },
                  "prov" = {
                    cases <- cases |>
                      dplyr::rename(adm1guid = `get(geo)`)
                  },
                  "dist" = {
                    cases <- cases |>
                      dplyr::rename(adm2guid = `get(geo)`)
                  }
  )

  case.ind <- switch(spatial.scale,
                     "ctry" = {
                       case.ind <-
                         dplyr::left_join(npafp.output, cases, by = c("ctry" = "ctry",
                                                                      "year" = "year"))
                     },
                     "prov" = {
                       case.ind <- dplyr::full_join(npafp.output,
                                                    cases,
                                                    by = c("adm1guid" = "adm1guid",
                                                           "year" = "year")) |>
                         dplyr::select(-adm1guid)
                     },
                     "dist" = {
                       case.ind <-
                         dplyr::full_join(npafp.output,
                                          cases,
                                          by = c("adm2guid" = "adm2guid",
                                                 "year" = "year")) |>
                         dplyr::select(-adm1guid, -adm2guid)
                     })

  return(case.ind)
}

#' Generate a summary table for sample timeliness intervals
#'
#' @param ctry.data RDS object containing polio country data
#' @param start_date start date of the desk review
#' @param end_date end date of the desk review
#' @param spatial.scale Scale to summarize to. Either "ctry" or "prov". "dist" not available currently.
#' @param lab.data tibble of lab data
#'
#' @return a tibble summarizing median days for different intervals
#' @export
generate_int_data <- function(ctry.data, start_date, end_date, spatial.scale, lab.data=NULL) {


  stool_to_lab_name <- dplyr::if_else(is.null(lab.data), "daysstooltolab", "days.collect.lab")

  afp.data <- ctry.data$afp.all.2 |>
    dplyr::filter(dplyr::between(lubridate::as_date(date.onset), start_date, end_date))

  select_criteria <- NULL
  select_criteria <- switch(spatial.scale,
                            "ctry" = {
                              c("epid", "ontonot", "nottoinvest", "investtostool1",
                                "stool1tostool2", stool_to_lab_name, "year",
                                "adm0guid", "ctry")
                            },
                            "prov" = {
                              c("epid", "ontonot", "nottoinvest", "investtostool1",
                                "stool1tostool2", stool_to_lab_name, "year",
                                "adm0guid", "adm1guid", "ctry", "prov")
                            })
  as_num_conversion <- c(stool_to_lab_name, "ontonot", "nottoinvest", "investtostool1", "stool1tostool2")


  int.data <- switch(spatial.scale,
                     "ctry" = {
                       int.data <- afp.data |>
                         dplyr::mutate(year = lubridate::year(date)) |>
                         dplyr::group_by(adm0guid, year) |>
                         dplyr::select(any_of(select_criteria)) %>%
                         mutate(across(
                           dplyr::any_of(as_num_conversion),
                           as.numeric
                         ))
                     },
                     "prov" = {
                       int.data <- afp.data |>
                         dplyr::mutate(year = lubridate::year(date)) |>
                         dplyr::group_by(adm1guid, year) |>
                         dplyr::select(dplyr::any_of(select_criteria)) %>%
                         dplyr::mutate(dplyr::across(
                           dplyr::any_of(as_num_conversion),
                           as.numeric
                         ))
                     })

  int.data <- switch(spatial.scale,
                     "ctry" = {
                       int.data <- int.data |>
                         tidyr::pivot_longer(!c(epid, year, adm0guid, ctry),
                                             names_to = "type",
                                             values_to = "value") |>
                         dplyr::group_by(year, type, adm0guid, ctry) |>
                         dplyr::summarize(medi = median(value, na.rm = T), freq = n())
                     },
                     "prov" = {
                       int.data <- int.data |>
                         tidyr::pivot_longer(
                           !c(epid, year, adm0guid, adm1guid, prov, ctry),
                           names_to = "type",
                           values_to = "value"
                         ) %>%
                         dplyr::group_by(year, type, adm1guid, prov, ctry) %>%
                         dplyr::summarize(medi = median(value, na.rm = T), freq = n())
                     })

  if(!is.null(lab.data)) {
    int.data = dplyr::bind_rows(lab.data, int.data)
  }

  if(spatial.scale == "prov") {
    int.data$prov = ctry.data$prov.pop$prov[match(int.data$adm1guid, ctry.data$prov.pop$adm1guid)]
  }

  levs <- NULL
  if (is.null(lab.data)) {
    levs <- c(
      "ontonot" = "Paralysis onset to notification",
      "nottoinvest" = "Case notification to investigation",
      "investtostool1" = "Case investigation to stool 1 collection",
      "stool1tostool2" = "Stool 1 collection to stool 2 collection",
      "daysstooltolab" = "Last stool collection to received in lab",
      "days.lab.culture" = "Stool received lab to final culture results",
      "days.seq.ship" = "Isolate received for sequencing to sequence results available",
      "days.lab.seq" = "Stool received in lab to sequence result",
      "days.itd.seqres" = "Final rRT-PCR results to sequence result",
      "days.itd.arriveseq" = "Final rRT-PCR results to isolate received for sequencing",
      "days.seq.rec.res" = "Isolate received for sequencing to sequence result"
    )
  } else {
    levs <- c(
      "ontonot" = "Paralysis onset to notification",
      "nottoinvest" = "Case notification to investigation",
      "investtostool1" = "Case investigation to stool 1 collection",
      "stool1tostool2" = "Stool 1 collection to stool 2 collection",
      "days.collect.lab" = "Last stool collection to received in lab",
      "days.lab.culture" = "Stool received lab to final culture results",
      "days.seq.ship" = "Isolate received for sequencing to sequence results available",
      "days.lab.seq" = "Stool received in lab to sequence result",
      "days.itd.seqres" = "Final rRT-PCR results to sequence result",
      "days.itd.arriveseq" = "Final rRT-PCR results to isolate received for sequencing",
      "days.seq.rec.res" = "Isolate received for sequencing to sequence result"
    )
  }

  int.data$type <- ordered(int.data$type, levels = names(levs), labels = levs)

  int.data <- int.data |>
    dplyr::filter(
      type %in% c(
        "Paralysis onset to notification",
        "Case notification to investigation",
        "Case investigation to stool 1 collection",
        "Stool 1 collection to stool 2 collection",
        "Last stool collection to received in lab",
        "Stool received lab to final culture results"
      )
    )

  # Remove columns containing only NA values
  int.data <- int.data |>
    dplyr::select(dplyr::where(function(x) !all(is.na(x))))

  return(int.data)
}

#' Generate summary table for those requiring 60 day follow up
#'
#' @param stool.data AFP data with stool adequacy columns
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#'
#' @return a summary table for those requiring 60 day follow up
#' @export
generate_60_day_table_data <- function(stool.data, start_date, end_date) {
  stool.data.inad = stool.data |>
    dplyr::mutate(
      stl.adeq.02 = dplyr::case_when(
        bad.stool1 == "data entry error" |
          bad.stool1 == "date before onset" |
          bad.stool1 == "date onset missing" ~ 77,
        bad.stool2 == "data entry error" |
          bad.stool2 == "date before onset" |
          bad.stool2 == "date onset missing" ~ 77,
        ontostool1 <= 13 &
          ontostool1 >= 0 & ontostool2 <= 14 &
          ontostool2 >= 1 &
          stool1tostool2 >= 1 &
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
    tibble::as_tibble() |>
    # filter onset to be >120 days from system date
    dplyr::filter(date <= lubridate::as_date(dplyr::if_else(
      end_date > Sys.Date() - 120, (Sys.Date() - 120),
      end_date
    ))) |>
    dplyr::filter(date >= start_date) |>
    dplyr::mutate(need60day.v2 = dplyr::if_else(adequacy.final2 != "Adequate", 1, 0)) |>
    dplyr::filter(need60day.v2 == 1 |
                    cdc.classification.all2 == "COMPATIBLE") |>
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
          (age.months < 60),
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
      epid,
      year,
      age.months,
      hot.case,
      hot.case.no.review,
      got60day,
      ontime.60day,
      ctry,
      prov,
      dist,
      date,
      date.notify,
      date.invest,
      datestool1,
      datestool2,
      stool.1.condition,
      stool.2.condition,
      adequacy.03,
      paralysis.asymmetric,
      paralysis.rapid.progress,
      paralysis.onset.fever,
      pot.compatible,
      doses.total,
      timeto60day,
      followup.date,
      classification,
      cdc.classification.all2,
      missing.fu.date,
      adm1guid,
      adm2guid
    )

  return(cases.need60day)
}

#' Generate a summary of samples sent to lab by year and province
#'
#' @param ctry.data Rds file countaining country polio data
#' @param start_date start date of the desk review
#' @param end_date end date of the desk review
#'
#' @return a tibble containing summary of samples sent to lab by year and province
#' @export
generate_prov_year_lab <- function(ctry.data, start_date, end_date) {
  afp.prov.year.lab <- ctry.data$afp.all.2 |>
    filter(between(date.onset, start_date, end_date)) |>
    count(prov, adm1guid, year) |>
    mutate(labs = paste0(year,
                         " (N=", n, ")"))

  return(afp.prov.year.lab)
}


#' Creating a table of compatible and potentially compatible cases.
#'
#' @param cases.need60day summary table of cases that need 60 day follow-up
#' @param create_cluster whether to use clustering algorithm. Default to F.
#'
#' @return summary table with clustering info
#' @export
generate_potentially_compatibles_cluster <- function(cases.need60day, create_cluster  = F) {
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
  pot.c.clust = dplyr::arrange(pot.c.clust, date) # arrange by onset date

  if (!create_cluster) {
    return(pot.c.clust)
  }

  cli::cli_process_start("Performing clustering assignment.")
  pot.c.clust$clust = NA
  pot.c.clust$clust[1] = 1

  if (nrow(pot.c.clust) > 1) {
    for (i in 2:nrow(pot.c.clust)) {
      pot.c.clust$clust[i] = dplyr::if_else(pot.c.clust$date[i] <= pot.c.clust$date[i - 1] + 30,
                                            max(pot.c.clust$clust[1:i - 1], na.rm = T),
                                            max(pot.c.clust$clust[1:i - 1], na.rm = T) + 1)
    }
  }

  # Rolling cluster assignment
  x = NULL
  y = NULL
  for (i in 1:length(unique(pot.c.clust$clust))) {
    x = dplyr::filter(pot.c.clust, clust == i)
    x$geo.clust.prov = vctrs::vec_duplicate_detect(x$prov)
    y = dplyr::bind_rows(y, x)
  }
  #If there is a province that is the same, geo.clust.prov will be TRUE

  cli::cli_process_done()

  return(y)

}

#' Checks data quality errors from the country data
#'
#' @param ctry.data RDS object containing polio country data
#' @param error_path path where to store errors in ctry.data
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
  cli::cli_progress_step("Spatial validation for country")
  incomplete.adm.ctry <-
    spatial_validation(ctry.data$ctry.pop, "ctry")
  missing.pop.ctry <-
    check_missing_pop(ctry.data$ctry.pop, "ctry")

  cli::cli_progress_step("Spatial validation for province")
  incomplete.adm.prov <-
    spatial_validation(ctry.data$prov.pop, "prov")
  missing.pop.prov <-
    check_missing_pop(ctry.data$prov.pop, "prov")

  cli::cli_progress_step("Spatial validation for district")
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
  error_log$invalid_adm0 <- incomplete.adm.ctry
  error_log$invalid_adm1 <- incomplete.adm.prov
  error_log$invalid_adm2 <- incomplete.adm.dist
  error_log$missing_pop_ctry <- missing.pop.ctry
  error_log$missing_pop_prov <- missing.pop.prov
  error_log$missing_pop_dist <- missing.pop.dist

  writexl::write_xlsx(error_log, path = file.path(error_path, "ctry_data_error_log.xlsx"))

}

#' Cleans and adds additional columns used in the desk reviews
#'
#' @param ctry.data country data RDS object
#' @param start_date start date for desk review
#' @param end_date end date for desk review
#' @param es_start_date start date for environmental surveillance data
#' @param es_end_date end date for environmental surveillance data
#'
#' @return cleaned country data RDS object
#' @export
clean_ctry_data <- function(ctry.data) {
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
#' @param afp.data tibble of AFP data (afp.all.2)
#' @param start_date start date of the desk review
#' @param end_date end date of the desk review
#'
#' @return a tibble containing stool adequacy columns
#' @export
generate_stool_data <- function(afp.data, start_date, end_date) {

  afp.data <- afp.data |>
    filter(year >= year(start_date) & year <= year(end_date))

  stool.data <- afp.data |> # IF FUNCTION CHANGES, THIS WILL NEED TO CHANGE AS WELL
    as_tibble() |>
    filter(cdc.classification.all2 != "NOT-AFP") |>
    mutate(adequacy.final = case_when(#Conditions for Bad Data
      bad.stool1 == "data entry error" |
        bad.stool1 == "date before onset" |
        bad.stool1 == "date onset missing" ~ 77
    )) %>%
    mutate(adequacy.final = case_when(#Conditions for Bad Data
      is.na(adequacy.final)==TRUE & (bad.stool2 == "data entry error" |
                                       bad.stool2 == "date before onset" |
                                       bad.stool2 == "date onset missing") ~ 77,
      TRUE ~ adequacy.final
    )) %>%
    mutate(adequacy.final = case_when(#Conditions for Poor Adequacy
      is.na(adequacy.final)==TRUE & (ontostool1 > 13 | ontostool1 < 0 |
                                       is.na(stool1tostool2) == T |
                                       ontostool2 > 14 | ontostool2 < 1 | stool1tostool2 < 1 |
                                       stool.1.condition == "Poor" | stool.2.condition == "Poor") ~ 0,
      TRUE ~ adequacy.final)) %>%
    mutate(adequacy.final = case_when(#Conditions for Good Adequacy
      is.na(adequacy.final)==TRUE & (ontostool1 <= 13 & ontostool1 >= 0 &
                                       ontostool2 <= 14 & ontostool2 >= 1 &
                                       stool1tostool2 >= 1 & stool.1.condition == "Good" &
                                       stool.2.condition == "Good") ~ 1,
      TRUE ~ adequacy.final
    )) %>%
    mutate(adequacy.final = case_when(#Conditions for Missing Adequacy
      is.na(adequacy.final)==TRUE & (is.na(stool.1.condition) == T |
                                       is.na(stool.2.condition) == T |
                                       stool.1.condition == "Unknown" | stool.2.condition == "Unknown") ~ 99,
      TRUE ~ adequacy.final
    )) |>
    mutate(adequacy.final = case_when(
      adequacy.final == 0 ~ "Inadequate",
      adequacy.final == 1 ~ "Adequate",
      adequacy.final == 77 ~ "Bad data",
      adequacy.final == 99 ~ "Missing",
    ))|>
    mutate(adequacy.final2 = ifelse(adequacy.final == "Missing", "Adequate", adequacy.final)
    )

  return(stool.data)
}
