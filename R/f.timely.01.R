# Calculate timeliness of stool collection using only epi variables (DRAFT)

#' Calculate stool collection timeliness using only AFP data (DRAFT)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Creates a table for timeliness by geographic unit including the
#' number of timely stool samples for each interval and percent timeliness. Currently,
#' the function will only work on `ctry.data` (output of [extract_country_data()]).
#'
#' @details
#' This function is no longer maintained as it is not used in any of the analytic pipelines.
#' An equivalent function is [generate_int_data()], which expands and simplifies this
#' function by also being able to take lab data to calculate lab timeliness intervals.
#'
#' @param afp.data `tibble` AFP data which includes GUID at a given spatial scale
#' formated as `adm(0,1,2)guid`, onset date as `date`.
#' @param admin.data `tibble` Full list of country administrative units by a given
#' spatial scale including `year`, `adm(0,1,2)guid`, and `ctry/prov/dist`
#' as appropriate.
#' @param start.date `str` Start date of the analysis formatted as `"YYYY-MM-DD"`.
#' @param end.date `str` End date of the analysis formatted as `"YYYY-MM-DD"`.
#' @param spatial.scale `str` Spatial scale to group analysis by. Valid values are:
#' -`"prov"` Province level.
#' - `"dist"` District level.
#' - `"ctry"` Country level.
#' @param intervals.manual `logical` Should user input their own timeliness interval
#' requirements? Default is `FALSE`. This is only required if timeliness column such as
#' `noti.7d.on`, `inv.2d.noti` are not already calculated. This draft function will currently fail if
#' this parameter is set to `TRUE`.
#' @returns `tibble` A summary table of timeliness of stool collection.
#' @examples
#' \dontrun{
#' raw.data <- get_all_polio_data()
#' ctry.data <- extract_country_data("algeria", raw.data)
#' stool.summary <- f.timely.01(
#'   ctry.data$afp.all.2, ctry.data$ctry.pop,
#'   lubridate::as_date("2021-01-01"),
#'   lubridate::as_date("2023-12-31"),
#'   "ctry"
#' )
#' }
#'
#' @keywords internal

f.timely.01 <- function(
    afp.data,
    admin.data,
    start.date,
    end.date,
    spatial.scale,
    intervals.manual = F) {
  lifecycle::deprecate_warn(
    "1.3.0",
    "f.timely.01()",
    details = "Please use generate_int_data() instead."
  )



  # What do i want this function to do?
  # 1. timeliness for each person in line list
  # Will need to define what is timely for each set of variables
  # Have default be what is in GPSAP, but have option to change?
  # 2. timeliness at national level
  # By year
  # Median timeliness for each category
  # Categories in useful factor levels
  # 3. Timeliness at province level
  # By year
  # Median timeliness for each category
  # Categories in useful factor levels
  # 4. Timeliness at district level
  # By year
  # Median timeliness for each category
  # Categories in useful factor levels

  # AFP data needs to have, in R DATE FORMAT....
  # EPID
  # GUID 0,1,2
  # Country name
  # Province name
  # District name
  # Full list of country, prov, dist
  # Onset date
  # Notification date
  # Investigation date
  # Stool 1 collection date
  # Stool 2 collection date
  # Date stool received in lab

  start.date <- lubridate::as_date(start.date)
  end.date <- lubridate::as_date(end.date)

  admin.data <- admin.data %>%
    dplyr::filter(dplyr::between(
      year, lubridate::year(start.date),
      lubridate::year(end.date)
    ))

  afp.data1 <- afp.data

  afp.data1 <- afp.data %>%
    dplyr::mutate(
      ontonot = afp.data1$date.notify - afp.data1$date,
      nottoinvest = afp.data1$date.invest - afp.data1$date.notify,
      investtostool1 = afp.data1$datestool1 - afp.data1$date.invest,
      stool1tostool2 = afp.data1$datestool2 - afp.data1$datestool1
    ) %>%
    dplyr::mutate(daysstooltolab = dplyr::case_when(
      is.na(datestool2) == FALSE ~ as.numeric(difftime(stooltolabdate, datestool2), units = "days"),
      is.na(datestool2) ~ as.numeric(difftime(stooltolabdate, datestool1), units = "days")
    )) %>%
    dplyr::filter(dplyr::between(date, start.date, end.date))

  if (intervals.manual == F) {
    afp.data1 <- afp.data1 %>%
      dplyr::mutate(noti.7d.on = dplyr::case_when(
        ## Cases notified within 7 days of onset
        is.na(ontonot) == FALSE & ontonot <= 7 ~ 1,
        TRUE ~ 0
      )) %>%
      dplyr::mutate(inv.2d.noti = dplyr::case_when(
        ## Investigated within 2 days of notification
        is.na(nottoinvest) == FALSE & nottoinvest <= 2 ~ 1,
        TRUE ~ 0
      )) %>%
      dplyr::mutate(coll.3d.inv = dplyr::case_when(
        ## Collected within 3 days of investigation
        is.na(investtostool1) == FALSE & investtostool1 <= 3 ~ 1,
        TRUE ~ 0
      )) %>%
      dplyr::mutate(stool.col.int = ifelse(
        is.na(stool1tostool2) == FALSE & stool1tostool2 >= 1 & as.numeric(datestool2 - date) <= 11,
        1, 0
      )) %>%
      dplyr::mutate(ship.3d.coll = dplyr::case_when(
        ## Received in lab within 3 days of collection
        is.na(daysstooltolab) == FALSE & daysstooltolab <= 3 ~ 1,
        is.na(daysstooltolab) == FALSE & daysstooltolab > 3 ~ 0
        # is.na(daysstooltolab) == TRUE ~ _is_na
        # TRUE ~ 0
      ))
  }
  # Add some code here for if intervals.manual == T
  # Then you have to define noti.7d.on,inv.2d.noti, coll.3d.inv, stool.col.int,
  # ship.3d.coll yourself

  if (spatial.scale == "ctry") { # Analysis at country level
    admin.data <- dplyr::ungroup(admin.data) %>%
      dplyr::select(adm0guid, year) %>%
      dplyr::distinct(.)

    int.data <- afp.data1 |>
      dplyr::mutate(year = lubridate::year(date)) |>
      dplyr::group_by(adm0guid, year) |>
      dplyr::select(
        epid,
        ontonot,
        nottoinvest,
        investtostool1,
        stool1tostool2,
        daysstooltolab,
        year,
        adm0guid,
        ctry
      ) %>%
      dplyr::mutate(dplyr::across(c(ontonot, nottoinvest, investtostool1, stool1tostool2, daysstooltolab), as.numeric)) %>%
      tidyr::pivot_longer(!c(epid, year, adm0guid, ctry), names_to = "type", values_to = "value") %>%
      dplyr::group_by(year, type, adm0guid, ctry) %>%
      dplyr::summarize(medi = median(value, na.rm = T), freq = dplyr::n())

    levs <- c(
      "daysstooltolab" = "Last stool collection to received in lab",
      "stool1tostool2" = "Stool 1 collection to stool 2 collection",
      "investtostool1" = "Case investigation to stool 1 collection",
      "nottoinvest" = "Case notification to investigation",
      "ontonot" = "Paralysis onset to notification"
    )
    int.data$type <- ordered(int.data$type, levels = names(levs), labels = levs)

    timeliness <- afp.data1 %>%
      dplyr::group_by(adm0guid, year) %>%
      dplyr::summarize(
        t.ontonot = sum(noti.7d.on, na.rm = T),
        t.nottoinvest = sum(inv.2d.noti, na.rm = T),
        t.investtostool1 = sum(coll.3d.inv, na.rm = T),
        t.stool1tostool2 = sum(stool.col.int, na.rm = T),
        t.daysstooltolab = sum(ship.3d.coll, na.rm = T)
      )

    int.data <- dplyr::full_join(int.data, admin.data,
      by = c(
        "year" = "year",
        "adm0guid" = "adm0guid"
      ),
      relationship = "many-to-many"
    ) %>%
      dplyr::full_join(., timeliness,
        by = c(
          "year" = "year",
          "adm0guid" = "adm0guid"
        ),
        relationship = "many-to-many"
      )
    int.data <- int.data %>%
      dplyr::mutate(
        per.t.ontonot = round(100 * t.ontonot / freq, 1),
        per.t.nottoinvest = round(100 * t.nottoinvest / freq, 1),
        per.t.investtostool1 = round(100 * t.investtostool1 / freq, 1),
        per.t.stool1tostool2 = round(100 * t.stool1tostool2 / freq, 1),
        per.t.daysstooltolab = round(100 * t.daysstooltolab / freq, 1)
      )
  }

  if (spatial.scale == "prov") { # Analysis at province level
    admin.data <- dplyr::ungroup(admin.data) %>%
      dplyr::select(adm1guid, year) %>%
      dplyr::distinct(.)

    int.data <- afp.data1 |>
      dplyr::mutate(year = lubridate::year(date)) |>
      dplyr::group_by(adm0guid, adm1guid, year) |>
      dplyr::select(
        epid,
        ontonot,
        nottoinvest,
        investtostool1,
        stool1tostool2,
        daysstooltolab,
        year,
        adm0guid,
        adm1guid,
        ctry,
        prov
      ) %>%
      dplyr::mutate(dplyr::across(c(ontonot, nottoinvest, investtostool1, stool1tostool2, daysstooltolab), as.numeric)) %>%
      tidyr::pivot_longer(!c(
        epid, year, adm0guid, adm1guid,
        ctry, prov
      ), names_to = "type", values_to = "value") %>%
      dplyr::group_by(
        year, type, adm0guid, adm1guid,
        ctry, prov
      ) %>%
      dplyr::summarize(
        medi = median(value, na.rm = T),
        freq = dplyr::n()
      )

    levs <- c(
      "daysstooltolab" = "Last stool collection to received in lab",
      "stool1tostool2" = "Stool 1 collection to stool 2 collection",
      "investtostool1" = "Case investigation to stool 1 collection",
      "nottoinvest" = "Case notification to investigation",
      "ontonot" = "Paralysis onset to notification"
    )
    int.data$type <- ordered(int.data$type, levels = names(levs), labels = levs)

    timeliness <- afp.data1 %>%
      dplyr::group_by(adm0guid, adm1guid, year) %>%
      # Number meeting the criteria
      dplyr::summarize(
        t.ontonot = sum(noti.7d.on, na.rm = T),
        t.nottoinvest = sum(inv.2d.noti, na.rm = T),
        t.investtostool1 = sum(coll.3d.inv, na.rm = T),
        t.stool1tostool2 = sum(stool.col.int, na.rm = T),
        t.daysstooltolab = sum(ship.3d.coll, na.rm = T)
      )

    int.data <- dplyr::full_join(int.data, admin.data,
      by = c(
        "year" = "year",
        "adm1guid" = "adm1guid"
      ),
      relationship = "many-to-many"
    ) %>%
      dplyr::full_join(., timeliness,
        by = c(
          "year" = "year",
          "adm0guid" = "adm0guid",
          "adm1guid" = "adm1guid"
        ),
        relationship = "many-to-many"
      )
    int.data <- int.data %>%
      dplyr::mutate(
        per.t.ontonot = round(100 * t.ontonot / freq, 1),
        per.t.nottoinvest = round(100 * t.nottoinvest / freq, 1),
        per.t.investtostool1 = round(100 * t.investtostool1 / freq, 1),
        per.t.stool1tostool2 = round(100 * t.stool1tostool2 / freq, 1),
        per.t.daysstooltolab = round(100 * t.daysstooltolab / freq, 1)
      )
  }

  if (spatial.scale == "dist") { # Analysis at province level
    admin.data <- dplyr::ungroup(admin.data) %>%
      dplyr::select(adm2guid, year) %>%
      dplyr::distinct(.)

    int.data <- afp.data1 |>
      dplyr::mutate(year = lubridate::year(date)) |>
      dplyr::group_by(adm0guid, adm1guid, adm2guid, year) |>
      dplyr::select(
        epid,
        ontonot,
        nottoinvest,
        investtostool1,
        stool1tostool2,
        daysstooltolab,
        year,
        adm0guid,
        adm1guid,
        adm2guid,
        ctry,
        prov,
        dist
      ) %>%
      dplyr::mutate(dplyr::across(c(ontonot, nottoinvest, investtostool1, stool1tostool2, daysstooltolab), as.numeric)) %>%
      tidyr::pivot_longer(!c(
        epid, year, adm0guid, adm1guid, adm2guid,
        ctry, prov, dist
      ), names_to = "type", values_to = "value") %>%
      dplyr::group_by(
        year, type, adm0guid, adm1guid, adm2guid,
        ctry, prov, dist
      ) %>%
      dplyr::summarize(medi = median(value, na.rm = T), freq = dplyr::n())

    levs <- c(
      "daysstooltolab" = "Last stool collection to received in lab",
      "stool1tostool2" = "Stool 1 collection to stool 2 collection",
      "investtostool1" = "Case investigation to stool 1 collection",
      "nottoinvest" = "Case notification to investigation",
      "ontonot" = "Paralysis onset to notification"
    )
    int.data$type <- ordered(int.data$type, levels = names(levs), labels = levs)

    timeliness <- afp.data1 %>%
      dplyr::group_by(adm0guid, adm1guid, adm2guid, year) %>%
      dplyr::summarize(
        t.ontonot = sum(noti.7d.on, na.rm = T),
        t.nottoinvest = sum(inv.2d.noti, na.rm = T),
        t.investtostool1 = sum(coll.3d.inv, na.rm = T),
        t.stool1tostool2 = sum(stool.col.int, na.rm = T),
        t.daysstooltolab = sum(ship.3d.coll, na.rm = T)
      )

    int.data <- dplyr::full_join(int.data, admin.data,
      by = c(
        "year" = "year",
        "adm2guid" = "adm2guid"
      ),
      relationship = "many-to-many"
    ) %>%
      dplyr::full_join(., timeliness,
        by = c(
          "year" = "year",
          "adm0guid" = "adm0guid",
          "adm1guid" = "adm1guid",
          "adm2guid" = "adm2guid"
        ),
        relationship = "many-to-many"
      )
    int.data <- int.data %>%
      dplyr::mutate(
        per.t.ontonot = round(100 * t.ontonot / freq, 1),
        per.t.nottoinvest = round(100 * t.nottoinvest / freq, 1),
        per.t.investtostool1 = round(100 * t.investtostool1 / freq, 1),
        per.t.stool1tostool2 = round(100 * t.stool1tostool2 / freq, 1),
        per.t.daysstooltolab = round(100 * t.daysstooltolab / freq, 1)
      )
  }
  return(int.data)
}
