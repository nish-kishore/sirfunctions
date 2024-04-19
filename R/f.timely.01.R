# Calculate timeliness of stool collection using only epi variables DRAFT - f.timely.01----
#' Calculate timeliness of stool collection using only epi variables
#'
#' @name f.timely.01
#' @description creates a table for timeliness by geographic unit including the
#' number of timely stool samples for each interval and percent timely
#' @import dplyr
#' @import lubridate
#' @import tibble
#' @param afp.data tibble: AFP data which includes GUID at a given spatial scale
#' formated as "adm{0,1,2}guid, onset date as "date"
#' @param admin.data tibble: Full list of country administrative units by a given
#' spatial scale including "year", "adm{0,1,2}guid, and "{ctry/prov/dist}"
#' as appropriate
#' @param start.date date: "YYYY-MM-DD"
#' @param end.date date: "YYYY-MM-DD"
#' @param spatial.scale chr: "prov" or "dist" or "ctry"
#' @param intervals.manual boolean: Should user input their own timeliness interval
#' requirements? Default is false
#' @returns tibble
#' @export

f.timely.01 <- function(
    afp.data,
    admin.data,
    start.date,
    end.date,
    spatial.scale,
    intervals.manual = F
){
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

  admin.data <- admin.data %>%
    filter(year >= year(start.date) &
             year <= year(end.date))

  afp.data1 = afp.data

  afp.data1 = afp.data %>%
    mutate(ontonot = afp.data1$date.notify - afp.data1$date,
           nottoinvest = afp.data1$date.invest - afp.data1$date.notify,
           investtostool1 = afp.data1$datestool1 - afp.data1$date.invest,
           stool1tostool2 = afp.data1$datestool2 - afp.data1$datestool1) %>%
    mutate(daysstooltolab = case_when(
      is.na(datestool2) == FALSE ~ as.numeric(difftime(stooltolabdate, datestool2), units = "days"),
      is.na(datestool2) ~ as.numeric(difftime(stooltolabdate, datestool1), units = "days")
    )) %>%
    filter(date >= date_first & date <= date_last)

  if(intervals.manual == F){
    afp.data1 <- afp.data1 %>%
      mutate(noti.7d.on = case_when(
        ## Cases notified within 7 days of onset
        is.na(ontonot) == FALSE & ontonot <= 7 ~ 1,
        TRUE ~ 0
      )) %>%
      mutate(inv.2d.noti = case_when(
        ## Investigated within 2 days of notification
        is.na(nottoinvest) == FALSE & nottoinvest <= 2 ~ 1,
        TRUE ~ 0
      )) %>%
      mutate(coll.3d.inv = case_when(
        ## Collected within 3 days of investigation
        is.na(investtostool1) == FALSE & investtostool1 <= 3 ~ 1,
        TRUE ~ 0
      )) %>%
      mutate(stool.col.int = ifelse(
        is.na(stool1tostool2) == FALSE & stool1tostool2>=1 & as.numeric(datestool2 - date)<=11,
        1,0)
      ) %>%
      mutate(ship.3d.coll = case_when(
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

  if(spatial.scale == "ctry"){ # Analysis at country level
    admin.data <- ungroup(admin.data) %>%
      select(adm0guid, year) %>%
      distinct(.)

    int.data <- afp.data1 |>
      mutate(year = year(date)) |>
      group_by(adm0guid, year) |>
      select(
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
      mutate(across(c(ontonot, nottoinvest, investtostool1, stool1tostool2, daysstooltolab), as.numeric)) %>%
      pivot_longer(!c(epid, year, adm0guid, ctry), names_to = "type", values_to = "value") %>%
      group_by(year, type, adm0guid, ctry) %>%
      summarize(medi = median(value, na.rm = T), freq = n())

    levs <- c(
      "daysstooltolab" = "Last stool collection to received in lab",
      "stool1tostool2" = "Stool 1 collection to stool 2 collection",
      "investtostool1" = "Case investigation to stool 1 collection",
      "nottoinvest" = "Case notification to investigation",
      "ontonot" = "Paralysis onset to notification"
    )
    int.data$type = ordered(int.data$type, levels = names(levs), labels = levs)

    timeliness = afp.data1 %>%
      group_by(adm0guid, year) %>%
      summarize(
        t.ontonot = sum(noti.7d.on, na.rm = T),
        t.nottoinvest = sum(inv.2d.noti, na.rm = T),
        t.investtostool1 = sum(coll.3d.inv,na.rm = T),
        t.stool1tostool2 = sum(stool.col.int, na.rm = T),
        t.daysstooltolab = sum(ship.3d.coll, na.rm = T))

    int.data <- full_join(int.data, admin.data,
                          by = c("year" = "year",
                                 "adm0guid" = "adm0guid"),
                          relationship = "many-to-many") %>%
      full_join(., timeliness, by = c("year" = "year",
                                      "adm0guid" = "adm0guid"),
                relationship = "many-to-many")
    int.data <- int.data %>%
      mutate(per.t.ontonot = round(100*t.ontonot/freq,1),
             per.t.nottoinvest = round(100*t.nottoinvest/freq,1),
             per.t.investtostool1 = round(100*t.investtostool1/freq,1),
             per.t.stool1tostool2 = round(100*t.stool1tostool2/freq,1),
             per.t.daysstooltolab = round(100*t.daysstooltolab/freq,1))
  }

  if(spatial.scale == "prov"){ # Analysis at province level
    admin.data <- ungroup(admin.data) %>%
      select(adm1guid, year) %>%
      distinct(.)

    int.data <- afp.data1 |>
      mutate(year = year(date)) |>
      group_by(adm0guid, adm1guid,year) |>
      select(
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
      mutate(across(c(ontonot, nottoinvest, investtostool1, stool1tostool2, daysstooltolab), as.numeric)) %>%
      pivot_longer(!c(epid, year, adm0guid, adm1guid,
                      ctry, prov), names_to = "type", values_to = "value") %>%
      group_by(year, type, adm0guid, adm1guid,
               ctry, prov) %>%
      summarize(medi = median(value, na.rm = T),
                freq = n())

    levs <- c(
      "daysstooltolab" = "Last stool collection to received in lab",
      "stool1tostool2" = "Stool 1 collection to stool 2 collection",
      "investtostool1" = "Case investigation to stool 1 collection",
      "nottoinvest" = "Case notification to investigation",
      "ontonot" = "Paralysis onset to notification"
    )
    int.data$type = ordered(int.data$type, levels = names(levs), labels = levs)

    timeliness = afp.data1 %>%
      group_by(adm0guid, adm1guid, year) %>%
      # Number meeting the criteria
      summarize(
        t.ontonot = sum(noti.7d.on, na.rm = T),
        t.nottoinvest = sum(inv.2d.noti, na.rm = T),
        t.investtostool1 = sum(coll.3d.inv,na.rm = T),
        t.stool1tostool2 = sum(stool.col.int, na.rm = T),
        t.daysstooltolab = sum(ship.3d.coll, na.rm = T))

    int.data <- full_join(int.data, admin.data,
                          by = c("year" = "year",
                                 "adm1guid" = "adm1guid"),
                          relationship = "many-to-many") %>%
      full_join(., timeliness, by = c("year" = "year",
                                      "adm0guid" = "adm0guid",
                                      "adm1guid" = "adm1guid"),
                relationship = "many-to-many")
    int.data <- int.data %>%
      mutate(per.t.ontonot = round(100*t.ontonot/freq,1),
             per.t.nottoinvest = round(100*t.nottoinvest/freq,1),
             per.t.investtostool1 = round(100*t.investtostool1/freq,1),
             per.t.stool1tostool2 = round(100*t.stool1tostool2/freq,1),
             per.t.daysstooltolab = round(100*t.daysstooltolab/freq,1))
  }

  if(spatial.scale == "dist"){ # Analysis at province level
    admin.data <- ungroup(admin.data) %>%
      select(adm2guid, year) %>%
      distinct(.)

    int.data <- afp.data1 |>
      mutate(year = year(date)) |>
      group_by(adm0guid, adm1guid,adm2guid,year) |>
      select(
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
      mutate(across(c(ontonot, nottoinvest, investtostool1, stool1tostool2, daysstooltolab), as.numeric)) %>%
      pivot_longer(!c(epid, year, adm0guid, adm1guid, adm2guid,
                      ctry, prov, dist), names_to = "type", values_to = "value") %>%
      group_by(year, type, adm0guid, adm1guid, adm2guid,
               ctry, prov, dist) %>%
      summarize(medi = median(value, na.rm = T), freq = n())

    levs <- c(
      "daysstooltolab" = "Last stool collection to received in lab",
      "stool1tostool2" = "Stool 1 collection to stool 2 collection",
      "investtostool1" = "Case investigation to stool 1 collection",
      "nottoinvest" = "Case notification to investigation",
      "ontonot" = "Paralysis onset to notification"
    )
    int.data$type = ordered(int.data$type, levels = names(levs), labels = levs)

    timeliness = afp.data1 %>%
      group_by(adm0guid, adm1guid, adm2guid, year) %>%
      summarize(
        t.ontonot = sum(noti.7d.on, na.rm = T),
        t.nottoinvest = sum(inv.2d.noti, na.rm = T),
        t.investtostool1 = sum(coll.3d.inv,na.rm = T),
        t.stool1tostool2 = sum(stool.col.int, na.rm = T),
        t.daysstooltolab = sum(ship.3d.coll, na.rm = T))

    int.data <- full_join(int.data, admin.data,
                          by = c("year" = "year",
                                 "adm2guid" = "adm2guid"),
                          relationship = "many-to-many") %>%
      full_join(., timeliness, by = c("year" = "year",
                                      "adm0guid" = "adm0guid",
                                      "adm1guid" = "adm1guid",
                                      "adm2guid" = "adm2guid"),
                relationship = "many-to-many")
    int.data <- int.data %>%
      mutate(per.t.ontonot = round(100*t.ontonot/freq,1),
             per.t.nottoinvest = round(100*t.nottoinvest/freq,1),
             per.t.investtostool1 = round(100*t.investtostool1/freq,1),
             per.t.stool1tostool2 = round(100*t.stool1tostool2/freq,1),
             per.t.daysstooltolab = round(100*t.daysstooltolab/freq,1))
  }
  return(int.data)
}
