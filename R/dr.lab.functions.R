#' Checks for common data errors in lab data
#'
#' @param lab.data lab.data from WHO
#' @param ctry.data RDS object containing polio data from a country
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#'
#' @return a list containing the errors
#' @export
lab_data_errors <- function(lab.data, ctry.data, start_date, end_date) {
  lab.data <- lab.data |> filter(ctry.code2 == ctry.data$ctry$ISO_3_CODE)

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
    filter(year >= year(start_date) & year <= year(end_date),
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

  return(error_log)
}

#' Clean polio lab data
#'
#' @param lab.data raw lab data
#' @param ctry.data country data RDS object
#'
#' @return a tibble containing clean lab data
#' @export
clean_lab_data <- function(lab.data, ctry.data, start_date, end_date, delim = "-") {

  cli::cli_process_start("Filtering country-specific lab data")
  lab.data <- lab.data |> filter(ctry.code2 == ctry.data$ctry$ISO_3_CODE)
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
    filter(year >= year(start_date) & year <= year(end_date),
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
           start_date,
           end_date) {
    geo <- switch(
      spatial.scale,
      "ctry" = "adm0guid",
      "prov" = "adm1guid",
      "dist" = "adm2guid"
    )

    lab1 <- lab.data |>
      filter(between(as.Date(DateOfOnset), start_date, end_date)) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.collect.lab, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.collect.lab") |>
      mutate(medi = as.numeric(medi))

    lab2 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start_date &
               as.Date(DateOfOnset) <= end_date) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.lab.culture, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.lab.culture") |>
      mutate(medi = as.numeric(medi))


    lab3 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start_date &
               as.Date(DateOfOnset) <= end_date) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.seq.ship, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.seq.ship") |>
      mutate(medi = as.numeric(medi))

    lab4 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start_date &
               as.Date(DateOfOnset) <= end_date) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.lab.seq, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.lab.seq") |>
      mutate(medi = as.numeric(medi))

    lab5 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start_date &
               as.Date(DateOfOnset) <= end_date) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.itd.seqres, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.itd.seqres") |>
      mutate(medi = as.numeric(medi))

    lab6 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start_date &
               as.Date(DateOfOnset) <= end_date) |>
      group_by(year, get(geo)) |>
      summarize(medi = median(days.itd.arriveseq, na.rm = T),
                freq = n()) |>
      ungroup() |>
      mutate(type = "days.itd.arriveseq") |>
      mutate(medi = as.numeric(medi))

    lab7 <- lab.data |>
      filter(as.Date(DateOfOnset) >= start_date &
               as.Date(DateOfOnset) <= end_date) |>
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
