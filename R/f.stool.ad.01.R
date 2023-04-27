#' Calculate percent stool adequacy on a case basis
#'
#' @name f.stool.ad.01
#' @description creates an adequacy variable - 'missing' parameter defines how
#' missing data is treated - "Good" classifies missing data as good quality
#' (POLIS method)
#' "Bad" classifies all missing as bad quality, "missing" excludes missing from
#' the calculations
#' @import dplyr
#' @import lubridate
#' @import tibble
#' @param afp.data tibble: AFP data which includes GUID at a given spatial scale
#' formated as "adm{0,1,2}guid, onset date as "date" and cdc.classification.all2
#' which includes "NOT-AFP"
#' @param ctry.data tibble: Full list of country administrative units by a given
#' spatial scale including "year", "adm{0,1,2}guid, and "{ctry/prov/dist}"
#' as appropriate
#' @param start.date chr: "YYYY-MM-DD"
#' @param end.date chr: "YYYY-MM-DD"
#' @param spatial.scale chr: "prov" or "dist" or "ctry"
#' @param prov.dist chr: "prov" or "dist" or "ctry"
#' @param missing chr: "good" or "bad" or "missing"; default "good"
#' @param bad.data chr: "remove" or "inadequate" or "adequate"; default "inadequate"
#' @returns tibble
#' @export
#' @examples f.stool.ad.01(
#' afp.data = data$afp.data,
#' admin.data = data$pop.data.prov,
#' start.date = "2019-04-01",
#' end.date = "2020-09-01",
#' spatial.scale = "prov",
#' missing = "good",
#' bad.data = "inadequate",
#' rolling = F
#' )

f.stool.ad.01 <- function(
    afp.data,
    admin.data,
    start.date,
    end.date,
    spatial.scale,
    missing = "good",
    bad.data = "inadequate",
    rolling = F
    ){

  #file names
  names.afp.ctry <- c("adm0guid", "date", "cdc.classification.all2")
  names.afp.prov <- c(names.afp.ctry, "adm1guid")
  names.afp.dist <- c(names.afp.prov, "adm2guid")

  names.ctry <- c("adm0guid", "year", "ctry")
  names.prov <- c(names.ctry, "adm1guid", "prov")
  names.dist <- c(names.prov, "adm2guid", "dist")
  # Note that the pop files are used to fill in all the province or district names
  # not for any population based calculation - SHOULD THIS BE A DIFFERENT FILE??

  #check that data inputs have the same country data
  if(!sort(unique(admin.data$adm0guid)) == sort(unique(afp.data$adm0guid))){
    stop("Please make sure that your `afp.data` and `admin.data` are subset for
         the same countries")
  }


  #Check data inputs
  # Analysis start and end date as defined by user (as a character)
  start.date <- as_date(start.date)
  end.date <- as_date(end.date)

  years <- year(start.date):year(end.date)
  ctry.years <- sort(unique(admin.data$year))

  #check that country data contains all the years necessary
  if(sum(!years %in% ctry.years) > 0){
    unfound.years <- years[!(years %in% ctry.years)]
    stop(paste0("There are no years of administrative level data for: ",
                paste0(unfound.years, collapse = ", ")))
  }

  #Check dates and return warning if afp data out of range

  # Limit AFP data to the range described by the analysis start and end dates
  afp.data <- afp.data |>
    filter(date >= start.date & date <= end.date)

  #check if spatial.data param in appropriate format
  if(!spatial.scale %in% c("ctry", "prov", "dist")){
    stop("'spatial.scale' can only be 'ctry', 'prov', or 'dist'")
  }

  #check input data based on spatial format
  if(spatial.scale == "ctry"){
    if(sum(!names.afp.ctry %in% names(afp.data)) > 0){
      stop(paste0("The follow variables were not found in afp.data: ",
                  paste0(names.afp.ctry[!names.afp.ctry %in% names(afp.data)],
                         collapse = ", ")))
    }
    if(sum(!names.ctry %in% names(admin.data)) > 0){
      stop(paste0("The follow variables were not found in admin.data: ",
                  paste0(names.ctry[!names.ctry %in% names(admin.data)],
                         collapse = ", ")))
    }
    #check if ctry admin data is of a larger spatial scale than the parameter chosen
    if(sum(c("adm1guid", "adm2guid") %in% names(admin.data)) > 0){
      stop("Please check that the spatial scale of the country admin data provided
      matches the `spatial.scale` chosen.")
    }

    incomplete.adm <- admin.data |>
      group_by(adm0guid) |>
      summarize(freq = n()) |>
      filter(freq < length(year(start.date):year(end.date))) |>
      pull(adm0guid)

    admin.data <- filter(admin.data, !adm0guid %in% incomplete.adm)
    afp.data <- filter(afp.data, !adm0guid %in% incomplete.adm)

  }

  if(spatial.scale == "prov"){
    if(sum(!names.afp.prov %in% names(afp.data)) > 0){
      stop(paste0("The follow variables were not found in afp.data: ",
                  paste0(names.afp.prov[!names.afp.prov %in% names(afp.data)],
                         collapse = ", ")))
    }
    if(sum(!names.prov %in% names(admin.data)) > 0){
      stop(paste0("The follow variables were not found in admin.data: ",
                  paste0(names.prov[!names.prov %in% names(admin.data)],
                         collapse = ", ")))
    }

    #check if country admin data is of a larger spatial scale than the parameter
    #chosen
    if(sum(c("adm2guid") %in% names(admin.data)) > 0){
      stop("Please check that the spatial scale of the country admin data provided
      matches the `spatial.scale` chosen.")
    }


    incomplete.adm <- admin.data |>
      group_by(adm1guid) |>
      summarize(freq = n()) |>
      filter(freq < length(year(start.date):year(end.date))) |>
      pull(adm1guid)

    admin.data <- filter(admin.data, !adm1guid %in% incomplete.adm)
    afp.data <- filter(afp.data, !adm1guid %in% incomplete.adm)

  }

  if(spatial.scale == "dist"){
    if(sum(!names.afp.dist %in% names(afp.data)) > 0){
      stop(paste0("The follow variables were not found in afp.data: ",
                  paste0(names.afp.dist[!names.afp.dist %in% names(afp.data)],
                         collapse = ", ")))
    }
    if(sum(!names.dist %in% names(admin.data)) > 0){
      stop(paste0("The follow variables were not found in admin.data: ",
                  paste0(names.dist[!names.dist %in% names(admin.data)],
                         collapse = ", ")))
    }

    incomplete.adm <- admin.data |>
      group_by(adm2guid) |>
      summarize(freq = n()) |>
      filter(freq < length(year(start.date):year(end.date))) |>
      pull(adm2guid)

    admin.data <- filter(admin.data, !adm2guid %in% incomplete.adm)
    afp.data <- filter(afp.data, !adm2guid %in% incomplete.adm)

  }

  if(length(incomplete.adm) > 0){

    readline(paste0("The following GUIDS at the ", spatial.scale,
                    " were not valid across the temporal scale:\n",
                    paste0(incomplete.adm, collapse = ", "),
                    "\n Please hit [ENTER] to continue"))

  }


  #Warning message about non-overlapping dates
  if(start.date < as_date(afp.data$date |> min(na.rm = T))){
    print(paste0(
      "Your specified start date is ",
      start.date,
      ", however the first date of AFP data available is ",
      afp.data$date |> min(na.rm = T),
      ". Please use caution interpreting any values calculated before ",
      afp.data$date |> min(na.rm = T)
    ))
  }



  #This function works in XXX stages:
  #1)
  #2)
  #3)


  # country of interest
  admin.data <- admin.data %>%
    filter(year >= year(start.date) &
             year <= year(end.date)) # Only years of analysis # Only country of analysis


  year.data <- tibble(
    "year" = year(start.date):year(end.date) # Defines year as the amount of
    # time between the start date and end date by calendar year (eg 2019:2020)
  ) |>
    rowwise() |>
    mutate(
      days_in_year = length(seq(as_date(paste0(year, "-01-01")),
                                as_date(paste0(year, "-12-31")),
                                by = "day"
      )),
      # Number of days in calendar year
      earliest_date = ifelse(year(start.date) == year, start.date,
                             as_date(paste0(year, "-01-01"))
      ),
      # Create earliest date per calendar year - if it is the year of start
      # date, then use the exact start date, otherwise use January 1
      earliest_date = as_date(earliest_date),
      latest_date = ifelse(year(end.date) == year,
                           end.date, as_date(paste0(year, "-12-31"))
      ),
      # Create latest date per calendar year - if it is the year of end date,
      # then use the exact end date, otherwise use December 31
      latest_date = as_date(latest_date),
      n_days = as.integer(latest_date - earliest_date + 1), # Calculate number
      # of days in calendar year between earliest and latest date of that
      # calendar year
    )


  # Coding data to be adequate (1), inadequate (0), data missing (99), or data error (77)
  stool.data <- afp.data
  stool.data$adequacy.final <- NA

stool.data <- afp.data |>
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
    mutate(year = year(date)) |>
    select(year, adm0guid, adm1guid, adm2guid,
           adequacy.final, cdc.classification.all2)

  stool.data <- full_join( # Merge stool data with days in year
    stool.data, year.data,
    by = c("year" = "year"))

  # Preserving the original classifications
  stool.data1 <- stool.data


  if(bad.data == "remove"){
    stool.data = stool.data %>%
      filter(adequacy.final != 77)
  }
  if(bad.data == "inadequate"){
    stool.data$adequacy.final = replace(stool.data$adequacy.final, stool.data$adequacy.final==77,0)
  }
  if(bad.data == "adequate"){
    stool.data$adequacy.final = replace(stool.data$adequacy.final, stool.data$adequacy.final==77,1)
  }

  if(missing == "good"){
    stool.data$adequacy.final = replace(stool.data$adequacy.final, stool.data$adequacy.final==99,1)
  }
  if(missing == "bad"){
    stool.data$adequacy.final = replace(stool.data$adequacy.final, stool.data$adequacy.final==99,0)
  }
  if(missing == "missing"){
    stool.data = stool.data %>%
      filter(adequacy.final != 99)
  }


  if (spatial.scale == "ctry") {
    if(rolling){
      admin.data <- ungroup(admin.data) %>%
        select(names.ctry, -"year") %>%
        distinct(.)

      baddy = stool.data1 %>%
        group_by(adm0guid, adequacy.final)%>%
        summarize(bad.data = n()) %>%
        ungroup()%>%
        filter(adequacy.final == 77) %>%
        select(-adequacy.final)

      missy = stool.data1 %>%
        group_by(adm0guid,adequacy.final)%>%
        summarize(missing.data = n()) %>%
        ungroup()%>%
        filter(adequacy.final == 99) %>%
        select(-adequacy.final)

      int.data <- stool.data |>
        group_by(adm0guid, adequacy.final) |>
        summarize(freq = n()) |>
        ungroup()

      # Total number of cases
      tots <- stool.data |>
        group_by(adm0guid) |>
        summarize(toty = n())

      int.data <- full_join(int.data, tots, by = c("adm0guid" = "adm0guid"))

      int.data.ad <- int.data |>
        filter(adequacy.final == 1) |>
        mutate(stoolad = 100 * freq / toty) %>%
        rename(num.ad = freq)

      int.data <- full_join(int.data.ad, missy,
                            by = c("adm0guid" = "adm0guid")
      ) %>%
        full_join(baddy, by = c("adm0guid" = "adm0guid"))


      # Join with admin file to fill in missing
      int.data <- full_join(int.data, admin.data,
                            by = c("adm0guid" = "adm0guid"))


      # Create number of days at risk
      int.data <- int.data %>%
        ungroup() |>
        mutate(
          days_at_risk = as.numeric(end.date - start.date+1),
        )

      # Replace NAs with 0
      int.data <- int.data %>%
        mutate(across(c(num.ad, toty, stoolad, missing.data,
                        bad.data), ~ replace_na(.x,0)))

      # Create number of adequate only
      int.data$ad.only <- int.data$num.ad - int.data$missing.data
      # Create inadequate only
      int.data$inad.only <-
        int.data$toty - int.data$missing.data - int.data$ad.only - int.data$bad.data

      int.data <- int.data %>%
        rename(
          "afp.cases" = "toty",
          "num.adequate" = "ad.only",
          "num.inadequate" = "inad.only",
          "num.missing.data" = "missing.data",
          "num.data.error" = "bad.data",
          "num.adj.w.miss" = "num.ad",
          "per.stool.ad" = "stoolad",
          "days.at.risk" = "days_at_risk"
        ) %>%
        select(
          adm0guid, ctry, num.adequate, num.missing.data, num.data.error,
          num.adj.w.miss, per.stool.ad, afp.cases, num.inadequate,
          days.at.risk
        )
      int.data <- int.data[,c("days.at.risk","ctry","afp.cases",
                              "num.adj.w.miss","num.adequate","num.inadequate",
                              "num.missing.data", "per.stool.ad",
                              "adm0guid")]


    }else{
    baddy = stool.data1 %>%
      group_by(adm0guid, year, adequacy.final)%>%
      summarize(bad.data = n()) %>%
      ungroup()%>%
      filter(adequacy.final == 77) %>%
      select(-adequacy.final)


    missy = stool.data1 %>%
      group_by(adm0guid, year, adequacy.final)%>%
      summarize(missing.data = n()) %>%
      ungroup()%>%
      filter(adequacy.final == 99) %>%
      select(-adequacy.final)

    int.data <- stool.data |>
      group_by(year, adm0guid, adequacy.final) |>
      summarize(freq = n()) |>
      ungroup()

    tots <- stool.data |>
      group_by(adm0guid, year) |>
      summarize(toty = n())

    int.data <- full_join(int.data, tots)

    int.data.ad <- int.data |>
      filter(adequacy.final == 1) |>
      mutate(stoolad = 100 * freq / toty) %>%
      rename(num.ad = freq)

    int.data <- full_join(int.data.ad, missy,
                          by = c("year" = "year", "adm0guid" = "adm0guid")
    ) %>%
      full_join(baddy, by = c("year" = "year", "adm0guid" = "adm0guid"))


    # Join with admin file to fill in missing
    int.data <- full_join(int.data, admin.data,
                                by = c("year" = "year",
                                       "adm0guid" = "adm0guid"))

    # Create number of days at risk
    int.data <- int.data %>%
      left_join(., year.data, by = c("year" = "year")) %>%
      mutate(days_at_risk = n_days)



      # Replace NAs with 0
      int.data <- int.data %>%
        mutate(across(c(num.ad, toty, stoolad, missing.data,
                        bad.data), ~ replace_na(.x,0)))

      # Create number of adequate only
      int.data$ad.only <- int.data$num.ad - int.data$missing.data
      # Create inadequate only
      int.data$inad.only <-
        int.data$toty - int.data$missing.data - int.data$ad.only - int.data$bad.data

      int.data <- int.data %>%
        rename(
          "afp.cases" = "toty",
          "num.adequate" = "ad.only",
          "num.inadequate" = "inad.only",
          "num.missing.data" = "missing.data",
          "num.data.error" = "bad.data",
          "num.adj.w.miss" = "num.ad",
          "per.stool.ad" = "stoolad",
          "days.at.risk" = "days_at_risk"
        ) %>%
        select(
          year, adm0guid, ctry, num.adequate, num.missing.data, num.data.error,
          num.adj.w.miss, per.stool.ad, afp.cases, num.inadequate,
          days.at.risk
        )
      int.data <- int.data[,c("year", "days.at.risk","ctry","afp.cases",
                              "num.adj.w.miss","num.adequate","num.inadequate",
                              "num.missing.data", "per.stool.ad",
                              "adm0guid")]

    }


  }


  if (spatial.scale == "prov") {
    if(rolling){
      admin.data <- ungroup(admin.data) %>%
        select(names.prov, -"year") %>%
        distinct(.)

      baddy = stool.data1 %>%
        group_by(adm1guid, adequacy.final)%>%
        summarize(bad.data = n()) %>%
        ungroup()%>%
        filter(adequacy.final == 77) %>%
        select(-adequacy.final)

      missy = stool.data1 %>%
        group_by(adm1guid,adequacy.final)%>%
        summarize(missing.data = n()) %>%
        ungroup()%>%
        filter(adequacy.final == 99) %>%
        select(-adequacy.final)

      int.data <- stool.data |>
        group_by(adm1guid, adequacy.final) |>
        summarize(freq = n()) |>
        ungroup()

      # Total number of cases
      tots <- stool.data |>
        group_by(adm1guid) |>
        summarize(toty = n())

      int.data <- full_join(int.data, tots, by = c("adm1guid" = "adm1guid"))

      int.data.ad <- int.data |>
        filter(adequacy.final == 1) |>
        mutate(stoolad = 100 * freq / toty) %>%
        rename(num.ad = freq)

      int.data <- full_join(int.data.ad, missy,
                            by = c("adm1guid" = "adm1guid")
      ) %>%
        full_join(baddy, by = c("adm1guid" = "adm1guid"))


      # Join with admin file to fill in missing
      int.data <- full_join(int.data, admin.data,
                            by = c("adm1guid" = "adm1guid"))


      # Create number of days at risk
      int.data <- int.data %>%
        ungroup() |>
        mutate(
          days_at_risk = as.numeric(end.date - start.date+1),
        )

      # Replace NAs with 0
      int.data <- int.data %>%
        mutate(across(c(num.ad, toty, stoolad, missing.data,
                        bad.data), ~ replace_na(.x,0)))

      # Create number of adequate only
      int.data$ad.only <- int.data$num.ad - int.data$missing.data
      # Create inadequate only
      int.data$inad.only <-
        int.data$toty - int.data$missing.data - int.data$ad.only - int.data$bad.data

      int.data <- int.data %>%
        rename(
          "afp.cases" = "toty",
          "num.adequate" = "ad.only",
          "num.inadequate" = "inad.only",
          "num.missing.data" = "missing.data",
          "num.data.error" = "bad.data",
          "num.adj.w.miss" = "num.ad",
          "per.stool.ad" = "stoolad",
          "days.at.risk" = "days_at_risk"
        ) %>%
        select(
          adm0guid, adm1guid, ctry, prov, num.adequate, num.missing.data,
          num.data.error, num.adj.w.miss, per.stool.ad, afp.cases, num.inadequate,
          days.at.risk
        )
      int.data <- int.data[,c("days.at.risk","ctry", "prov","afp.cases",
                              "num.adj.w.miss","num.adequate","num.inadequate",
                              "num.missing.data", "per.stool.ad",
                              "adm0guid", "adm1guid")]


    }else{
      baddy = stool.data1 %>%
        group_by(adm1guid, year, adequacy.final)%>%
        summarize(bad.data = n()) %>%
        ungroup()%>%
        filter(adequacy.final == 77) %>%
        select(-adequacy.final)


      missy = stool.data1 %>%
        group_by(adm1guid, year, adequacy.final)%>%
        summarize(missing.data = n()) %>%
        ungroup()%>%
        filter(adequacy.final == 99) %>%
        select(-adequacy.final)

      int.data <- stool.data |>
        group_by(year, adm1guid, adequacy.final) |>
        summarize(freq = n()) |>
        ungroup()

      tots <- stool.data |>
        group_by(adm1guid, year) |>
        summarize(toty = n())

      int.data <- full_join(int.data, tots)

      int.data.ad <- int.data |>
        filter(adequacy.final == 1) |>
        mutate(stoolad = 100 * freq / toty) %>%
        rename(num.ad = freq)

      int.data <- full_join(int.data.ad, missy,
                            by = c("year" = "year",
                                   "adm1guid" = "adm1guid")
      ) %>%
        full_join(baddy, by = c("year" = "year",
                                "adm1guid" = "adm1guid"))


      # Join with admin file to fill in missing
      int.data <- full_join(int.data, admin.data,
                            by = c("year" = "year",
                                   "adm1guid" = "adm1guid"))

      # Create number of days at risk
      int.data <- int.data %>%
        left_join(., year.data, by = c("year" = "year")) %>%
        mutate(days_at_risk = n_days)



      # Replace NAs with 0
      int.data <- int.data %>%
        mutate(across(c(num.ad, toty, stoolad, missing.data,
                        bad.data), ~ replace_na(.x,0)))

      # Create number of adequate only
      int.data$ad.only <- int.data$num.ad - int.data$missing.data
      # Create inadequate only
      int.data$inad.only <-
        int.data$toty - int.data$missing.data - int.data$ad.only - int.data$bad.data

      int.data <- int.data %>%
        rename(
          "afp.cases" = "toty",
          "num.adequate" = "ad.only",
          "num.inadequate" = "inad.only",
          "num.missing.data" = "missing.data",
          "num.data.error" = "bad.data",
          "num.adj.w.miss" = "num.ad",
          "per.stool.ad" = "stoolad",
          "days.at.risk" = "days_at_risk"
        ) %>%
        select(
          year, adm0guid, adm1guid, ctry, prov, num.adequate, num.missing.data,
          num.data.error, num.adj.w.miss, per.stool.ad, afp.cases, num.inadequate,
          days.at.risk
        )
      int.data <- int.data[,c("year","days.at.risk","ctry", "prov","afp.cases",
                              "num.adj.w.miss","num.adequate","num.inadequate",
                              "num.missing.data", "per.stool.ad",
                              "adm0guid", "adm1guid")]

    }
  }

  if (spatial.scale == "dist") {
    if(rolling){
      admin.data <- ungroup(admin.data) %>%
        select(names.dist, -"year") %>%
        distinct(.)

      baddy = stool.data1 %>%
        group_by(adm2guid, adequacy.final)%>%
        summarize(bad.data = n()) %>%
        ungroup()%>%
        filter(adequacy.final == 77) %>%
        select(-adequacy.final)

      missy = stool.data1 %>%
        group_by(adm2guid,adequacy.final)%>%
        summarize(missing.data = n()) %>%
        ungroup()%>%
        filter(adequacy.final == 99) %>%
        select(-adequacy.final)

      int.data <- stool.data |>
        group_by(adm2guid, adequacy.final) |>
        summarize(freq = n()) |>
        ungroup()

      # Total number of cases
      tots <- stool.data |>
        group_by(adm2guid) |>
        summarize(toty = n())

      int.data <- full_join(int.data, tots, by = c("adm2guid" = "adm2guid"))

      int.data.ad <- int.data |>
        filter(adequacy.final == 1) |>
        mutate(stoolad = 100 * freq / toty) %>%
        rename(num.ad = freq)

      int.data <- full_join(int.data.ad, missy,
                            by = c("adm2guid" = "adm2guid")
      ) %>%
        full_join(baddy, by = c("adm2guid" = "adm2guid"))


      # Join with admin file to fill in missing
      int.data <- full_join(int.data, admin.data,
                            by = c("adm2guid" = "adm2guid"))


      # Create number of days at risk
      int.data <- int.data %>%
        ungroup() |>
        mutate(
          days_at_risk = as.numeric(end.date - start.date+1),
        )

      # Replace NAs with 0
      int.data <- int.data %>%
        mutate(across(c(num.ad, toty, stoolad, missing.data,
                        bad.data), ~ replace_na(.x,0)))

      # Create number of adequate only
      int.data$ad.only <- int.data$num.ad - int.data$missing.data
      # Create inadequate only
      int.data$inad.only <-
        int.data$toty - int.data$missing.data - int.data$ad.only - int.data$bad.data

      int.data <- int.data %>%
        rename(
          "afp.cases" = "toty",
          "num.adequate" = "ad.only",
          "num.inadequate" = "inad.only",
          "num.missing.data" = "missing.data",
          "num.data.error" = "bad.data",
          "num.adj.w.miss" = "num.ad",
          "per.stool.ad" = "stoolad",
          "days.at.risk" = "days_at_risk"
        ) %>%
        select(
          adm0guid, adm1guid, adm2guid, ctry, prov, dist, num.adequate,
          num.missing.data,
          num.data.error, num.adj.w.miss, per.stool.ad, afp.cases, num.inadequate,
          days.at.risk
        )
      int.data <- int.data[,c("days.at.risk","ctry", "prov", "dist",
                              "afp.cases",
                              "num.adj.w.miss","num.adequate","num.inadequate",
                              "num.missing.data", "per.stool.ad",
                              "adm0guid", "adm1guid", "adm2guid")]


    }else{
      baddy = stool.data1 %>%
        group_by(adm2guid, year, adequacy.final)%>%
        summarize(bad.data = n()) %>%
        ungroup()%>%
        filter(adequacy.final == 77) %>%
        select(-adequacy.final)


      missy = stool.data1 %>%
        group_by(adm2guid, year, adequacy.final)%>%
        summarize(missing.data = n()) %>%
        ungroup()%>%
        filter(adequacy.final == 99) %>%
        select(-adequacy.final)

      int.data <- stool.data |>
        group_by(year, adm2guid, adequacy.final) |>
        summarize(freq = n()) |>
        ungroup()

      tots <- stool.data |>
        group_by(adm2guid, year) |>
        summarize(toty = n())

      int.data <- full_join(int.data, tots)

      int.data.ad <- int.data |>
        filter(adequacy.final == 1) |>
        mutate(stoolad = 100 * freq / toty) %>%
        rename(num.ad = freq)

      int.data <- full_join(int.data.ad, missy,
                            by = c("year" = "year",
                                   "adm2guid" = "adm2guid")
      ) %>%
        full_join(baddy, by = c("year" = "year",
                                "adm2guid" = "adm2guid"))


      # Join with admin file to fill in missing
      int.data <- full_join(int.data, admin.data,
                            by = c("year" = "year",
                                   "adm2guid" = "adm2guid"))

      # Create number of days at risk
      int.data <- int.data %>%
        left_join(., year.data, by = c("year" = "year")) %>%
        mutate(days_at_risk = n_days)



      # Replace NAs with 0
      int.data <- int.data %>%
        mutate(across(c(num.ad, toty, stoolad, missing.data,
                        bad.data), ~ replace_na(.x,0)))

      # Create number of adequate only
      int.data$ad.only <- int.data$num.ad - int.data$missing.data
      # Create inadequate only
      int.data$inad.only <-
        int.data$toty - int.data$missing.data - int.data$ad.only - int.data$bad.data

      int.data <- int.data %>%
        rename(
          "afp.cases" = "toty",
          "num.adequate" = "ad.only",
          "num.inadequate" = "inad.only",
          "num.missing.data" = "missing.data",
          "num.data.error" = "bad.data",
          "num.adj.w.miss" = "num.ad",
          "per.stool.ad" = "stoolad",
          "days.at.risk" = "days_at_risk"
        ) %>%
        select(
          year, adm0guid, adm1guid, adm2guid, ctry, prov, dist,
          num.adequate, num.missing.data,
          num.data.error, num.adj.w.miss, per.stool.ad, afp.cases, num.inadequate,
          days.at.risk
        )
      int.data <- int.data[,c("year","days.at.risk","ctry", "prov", "dist",
                              "afp.cases",
                              "num.adj.w.miss","num.adequate","num.inadequate",
                              "num.missing.data", "per.stool.ad",
                              "adm0guid", "adm1guid", "adm2guid")]

    }
  }
  return(int.data)
}
