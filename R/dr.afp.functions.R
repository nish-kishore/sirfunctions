
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

  missing_geo <- afp.data |> filter(is.na(get(spatial.scale)))

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
  missy <- afp.data |> filter(is.na(dist))

  cli::cli_alert_info(paste0("There are ", nrow(missy), " records missing district data"))

  # Pull out any geographic info from the epid
  missy2 = afp.data |>
    filter(epid %in% missy$epid) |>
    mutate(epid.check = substr(epid, 1, 11))

  # Match epid.check with epid and year onset to pull out district and guid
  missy2 <- missy2 |>
    mutate(adm2guid.2 = NA)

  for (i in seq_len(nrow(missy2))) {
    x <- afp.data |>
      filter(
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
    mutate(adm2guid = if_else(is.na(adm2guid), as.character(adm2guid.2), adm2guid))

  missy3 <- missy2 |>
    filter(is.na(adm2guid))

  for (i in seq_len(nrow(missy3))) {
    x <- afp.data |>
      filter(grepl(missy3$epid.check[i], afp.data$epid) == T &
               !is.na(adm2guid))

    #print(dim(x))
    if (dim(x)[1] > 0 & length(unique(x$adm2guid)) == 1) {
      missy3$adm2guid.2[i] = unique(x$adm2guid)
    }

  }

  # Backfill missy2 with new data
  missy2$dist[match(missy3$epid, missy2$epid)] = missy3$dist
  missy2$adm2guid[match(missy3$epid, missy2$epid)] = missy3$adm2guid.2

  # For those that have multiple adm2guids
  # Match province and take the adm2guid that goes with the province
  missy3  <- missy2 |>
    filter(is.na(adm2guid))

  for (i in seq_len(nrow(missy3))) {
    x <- afp.data |>
      filter(grepl(missy3$epid.check[i], afp.data$epid) == T &
               !is.na(adm2guid))

    y <- x |> filter(prov == missy3$prov[i])

    if (dim(x)[1] > 0 & dim(y)[1] > 0) {
      print(dim(y))
      missy3$adm2guid.2[i] = unique(y$adm2guid)
    }
  }

  missy2$adm2guid[match(missy3$epid, missy2$epid)] = missy3$adm2guid.2

  # add dist in to missy2
  missy2 = missy2 |>
    mutate(dist = case_when(is.na(dist) & is.na(adm2guid) == F ~
                              afp.data$dist[match(missy2$adm2guid,
                                                  afp.data$adm2guid)],
                            T ~ dist))

  # Update dist and adm2guid
  afp.data$dist[match(missy2$epid, afp.data$epid)] = missy2$dist
  afp.data$adm2guid[match(missy2$epid, afp.data$epid)] = missy2$adm2guid

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
    mutate(across(
      c(
        doses.opv.routine,
        doses.opv.sia,
        doses.ipv.routine,
        doses.ipv.sia,
        doses.ipv.number
      ),
      as.numeric
    ))
  afp.data = afp.data %>%
    mutate(dose.num.calc = rowSums(afp.data[, c(
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
    na.rm = T)) %>%
    mutate(
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

  afp.data = afp.data %>%
    mutate(
      dose.cat = case_when(
        doses.total == 0 ~ "0",
        doses.total > 0 & doses.total < 3 ~ "1-2",
        doses.total == 3 ~ "3",
        doses.total > 3 ~ "4+",
        T ~ as.character(doses.total)
      )
    ) %>%
    mutate(dose.cat = factor(dose.cat, levels = c("Unknown",
                                                  "4+",
                                                  "3",
                                                  "1-2",
                                                  "0")))

  afp.data$dose.cat = forcats::fct_na_value_to_level(afp.data$dose.cat, "Missing")
  cli::cli_process_done()
  return(afp.data)
}


#' Generate AFP case counts by month
#' @param afp.data tibble containing AFP data (afp.all.2)
#'
#' @return tibble containing summary of AFP data
#' @export
generate_afp_by_month <- function(afp.data, start_date, end_date) {
  summary <- afp.data |>
    drop_na(date.onset) |>
    filter(between(as_date(date.onset), start_date, end_date)) |>
    mutate(mon.year = floor_date(date, "month"))

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
                                     group_by(adm1guid, mon.year, year) |> #changed to guid
                                     summarise(cases = n()) |>
                                     ungroup() |>
                                     complete(
                                       mon.year = seq(floor_date(start_date, "month"),
                                                      end_date, by = "month"),
                                       adm1guid = unique(ctry.data$prov.pop$adm1guid),
                                       fill = list(cases = 0)
                                     ) |>
                                     ungroup() |>
                                     mutate(year = year(mon.year)) |>
                                     # join this to the original  province population dataset
                                     left_join(ctry.data$prov.pop[, c("adm1guid", "year", "prov", "u15pop")],
                                               by = c("adm1guid" = "adm1guid",
                                                      "year" = "year")) |>
                                     mutate(cases = ifelse(is.na(cases), 0, cases)) |>
                                     mutate(
                                       mon.year2 = as_date(as.yearmon(mon.year, "%b-%y")),
                                       case.cat = case_when(
                                         cases == 0 ~ "0",
                                         cases == 1 ~ "1",
                                         cases > 1 & cases < 6 ~ "2-5",
                                         cases >= 6 & cases < 10 ~ "6-9",
                                         cases >= 10 ~ "10+"
                                       ),
                                       year = year(mon.year2),
                                       mononset = month(mon.year2)
                                     ) |>
                                     mutate(case.cat = factor(
                                       case.cat,
                                       levels = c("0", "1", "2-5", "6-9", "10+"),
                                       labels = c("0", "1", "2-5", "6-9", "10+")
                                     ))
                                 },
                                 "dist" = {
                                   afp.by.month |>
                                     group_by(dist, adm2guid, mon.year) |> #changed to guid
                                     summarise(cases = n()) |>
                                     ungroup() |>
                                     complete(
                                       mon.year = seq(floor_date(start_date, "month"),
                                                      end_date, by = "month"),
                                       adm2guid = unique(ctry.data$dist.pop$adm2guid),
                                       fill = list(cases = 0)
                                     ) |>
                                     ungroup() |>
                                     mutate(year = year(mon.year)) |>
                                     left_join(ctry.data$dist.pop[, c("adm2guid", "year", "prov", "u15pop")],
                                               by = c("adm2guid" = "adm2guid",
                                                      "year" = "year")) |>
                                     mutate(cases = ifelse(is.na(cases), 0, cases)) |>
                                     mutate(
                                       mon.year2 = as_date(as.yearmon(mon.year, "%b-%y")),
                                       case.cat = case_when(
                                         cases == 0 ~ "0",
                                         cases == 1 ~ "1",
                                         cases > 1 & cases < 6 ~ "2-5",
                                         cases >= 6 & cases < 10 ~ "6-9",
                                         cases >= 10 ~ "10+"
                                       ),
                                       year = year(mon.year2),
                                       mononset = month(mon.year2)
                                     ) |>
                                     mutate(case.cat = factor(
                                       case.cat,
                                       levels = c("0", "1", "2-5", "6-9", "10+"),
                                       labels = c("0", "1", "2-5", "6-9", "10+")
                                     ))
                                 },
                                 "year" = {
                                   afp.by.month |>
                                     filter(between(as.Date(date.onset), start_date, end_date)) |>
                                     group_by(year) |>
                                     summarize(afp.case = n())
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
  prov_na <- npafp.output |> filter(is.na(prov))
  prov_no_na <- npafp.output |> filter(!is.na(prov))
  prov_na$prov <- prov_no_na$prov[match(prov_na$adm1guid, prov_no_na$adm1guid)]

  npafp.output <- bind_rows(prov_na, prov_no_na)

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
  afp.all.2 <- afp.all.2 |> filter(between(date, start_date, end_date))
  cases <- afp.all.2 |> group_by(get(geo), year) |>
    summarize(afp.case = n(),
              num.wpv.cases = sum(wild.1 == TRUE, wild.3 == TRUE, na.rm = T),
              num.vdpv1.cases	= sum(vdpv.1  == TRUE,na.rm = T),
              num.vdpv2.cases	= sum(vdpv.2 == TRUE, na.rm = T),
              num.vdpv3.cases = sum(vdpv.3  == TRUE, na.rm = T))

  cases <- switch(spatial.scale,
                "ctry" = {
                  cases <- cases |> rename(ctry = `get(geo)`)
                },
                "prov" = {
                  cases <- cases |> rename(adm1guid = `get(geo)`)
                },
                "dist" = {
                  cases <- cases |> rename(adm2guid = `get(geo)`)
                }
  )

  case.ind <- switch(spatial.scale,
         "ctry" = {
           case.ind <-
             left_join(npafp.output, cases, by = c("ctry" = "ctry",
                                                       "year" = "year"))
         },
         "prov" = {
           case.ind = full_join(npafp.output,
                                        cases,
                                        by = c("adm1guid" = "adm1guid",
                                               "year" = "year")) |>
           select(-adm1guid)
         },
         "dist" = {
           case.ind <-
             full_join(npafp.output,
                       cases,
                       by = c("adm2guid" = "adm2guid",
                              "year" = "year")) |>
             select(-adm1guid, -adm2guid)
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


  stool_to_lab_name <- if_else(is.null(lab.data), "daysstooltolab", "days.collect.lab")

  afp.data <- ctry.data$afp.all.2 |> filter(between(as_date(date.onset), start_date, end_date))

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
                         mutate(year = year(date)) |>
                         group_by(adm0guid, year) |>
                         select(any_of(select_criteria)) %>%
                         mutate(across(
                           any_of(as_num_conversion),
                           as.numeric
                         ))
                     },
                     "prov" = {
                       int.data <- afp.data |>
                         mutate(year = year(date)) |>
                         group_by(adm1guid, year) |>
                         select(any_of(select_criteria)) %>%
                         mutate(across(
                           any_of(as_num_conversion),
                           as.numeric
                         ))
                     })

  int.data <- switch(spatial.scale,
                     "ctry" = {
                       int.data <- int.data |>
                         pivot_longer(!c(epid, year, adm0guid, ctry),
                                      names_to = "type",
                                      values_to = "value") |>
                         group_by(year, type, adm0guid, ctry) |>
                         summarize(medi = median(value, na.rm = T), freq = n())
                     },
                     "prov" = {
                       int.data <- int.data |>
                         pivot_longer(
                           !c(epid, year, adm0guid, adm1guid, prov, ctry),
                           names_to = "type",
                           values_to = "value"
                         ) %>%
                         group_by(year, type, adm1guid, prov, ctry) %>%
                         summarize(medi = median(value, na.rm = T), freq = n())
                     })

  if(!is.null(lab.data)) {
    int.data = bind_rows(lab.data, int.data)
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

  int.data$type = ordered(int.data$type, levels = names(levs), labels = levs)

  int.data = filter(
    int.data,
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
  int.data <- int.data %>%
    select(where(function(x) !all(is.na(x))))

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
  stool.data.inad = stool.data %>%
    mutate(
      stl.adeq.02 = case_when(
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
    as_tibble() |>
    # filter onset to be >120 days from system date
    filter(date <= as_date(ifelse(
      end_date > Sys.Date() - 120, (Sys.Date() - 120),
      end_date
    ))) |>
    filter(date >= start_date) |>
    mutate(need60day.v2 = ifelse(adequacy.final2 != "Adequate", 1, 0)) |>
    filter(need60day.v2 == 1 |
             cdc.classification.all2 == "COMPATIBLE") |>
    filter(age.months < 60 |
             is.na(age.months)) |> # only children under 5 or missing age
    mutate(
      got60day =
        case_when(
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
        case_when(
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

    mutate(
      pot.compatible = ifelse(
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
          ),
        1,
        0
      ),
      pot.compatible = ifelse(is.na(pot.compatible) == T, 0, pot.compatible),
      hot.case.no.review = ifelse(
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
    mutate(missing.fu.date = ifelse(
      need60day.v2 == 1 &
        is.na(followup.date) == T & is.na(followup.findings) == F,
      1,
      0
    )) |>
    select(
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

#' Creating a table of compatible and potentially compatible cases.
#'
#' @param cases.need60day summary table of cases that need 60 day follow-up
#' @param create_cluster whether to use clustering algorithm. Default to F.
#'
#' @return summary table with clustering info
#' @export
generate_potentially_compatibles_cluster <- function(cases.need60day, create_cluster  = F) {
  pot.c.clust = filter(cases.need60day,
                       pot.compatible == 1 | classification == "Compatible") %>%
    mutate(
      class = case_when(
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
  pot.c.clust = arrange(pot.c.clust, date) # arrange by onset date

  if (!create_cluster) {
    return(pot.c.clust)
  }

  cli::cli_process_start("Performing clustering assignment.")
  pot.c.clust$clust = NA
  pot.c.clust$clust[1] = 1

  if (nrow(pot.c.clust) > 1) {
    for(i in 2:nrow(pot.c.clust)){
      pot.c.clust$clust[i] = if_else(pot.c.clust$date[i]<=pot.c.clust$date[i-1]+30,
                                     max(pot.c.clust$clust[1:i-1], na.rm = T),
                                     max(pot.c.clust$clust[1:i-1], na.rm = T)+1)
    }
  }

  # Rolling cluster assignment
  x = NULL
  y = NULL
  for (i in 1:length(unique(pot.c.clust$clust))) {
    x = filter(pot.c.clust, clust == i)
    x$geo.clust.prov = vctrs::vec_duplicate_detect(x$prov)
    y = bind_rows(y, x)
  }
  #If there is a province that is the same, geo.clust.prov will be TRUE

  cli::cli_process_done()

  return(y)

}
