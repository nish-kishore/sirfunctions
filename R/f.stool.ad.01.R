# Helper functions----

#' Helper function to generate year.data
#'
#' @param start_date start date
#' @param end_date  end date
#' @keywords internal
#'
#' @return a tibble with year data
generate_year_data <- function(start_date, end_date) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  year.data <- dplyr::tibble(
    "year" = lubridate::year(start_date):lubridate::year(end_date) # Defines year as the amount of
    # time between the start date and end date by calendar year (eg 2019:2020)
  ) |>
    dplyr::rowwise() |>
    mutate(
      days_in_year = length(seq(lubridate::as_date(paste0(year, "-01-01")),
                                lubridate::as_date(paste0(year, "-12-31")),
                                by = "day"
      )),
      # Number of days in calendar year
      earliest_date = ifelse(lubridate::year(start_date) == year, start_date,
                             lubridate::as_date(paste0(year, "-01-01"))
      ),
      # Create earliest date per calendar year - if it is the year of start
      # date, then use the exact start date, otherwise use January 1
      earliest_date = lubridate::as_date(earliest_date),
      latest_date = ifelse(lubridate::year(end_date) == year,
                           end_date, lubridate::as_date(paste0(year, "-12-31"))
      ),
      # Create latest date per calendar year - if it is the year of end date,
      # then use the exact end date, otherwise use December 31
      latest_date = lubridate::as_date(latest_date),
      n_days = as.integer(latest_date - earliest_date + 1), # Calculate number
      # of days in calendar year between earliest and latest date of that
      # calendar year
      weight = n_days /days_in_year
    )

  return(year.data)

}

#' Helper function to add the adequacy.final column
#'
#' @param afp.data AFP dataset
#'
#' @return AFP dataset with the adequacy final column
#' @examples
#' \dontrun{
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' stool.data <- generate_ad_final_col(raw.data$afp)
#' }
#'
#' @export
generate_ad_final_col <- function(afp.data) {
  stool.data <- afp.data |>
    dplyr::as_tibble() |>
    dplyr::mutate(adequacy.final = dplyr::case_when( # Conditions for Bad Data
      bad.stool1 == "data entry error" |
        bad.stool1 == "date before onset" |
        bad.stool1 == "date onset missing" ~ 77
    )) |>
    dplyr::mutate(adequacy.final = dplyr::case_when( # Conditions for Bad Data
      is.na(adequacy.final) == TRUE & (bad.stool2 == "data entry error" |
        bad.stool2 == "date before onset" |
        bad.stool2 == "date onset missing") ~ 77,
      TRUE ~ adequacy.final
    )) |>
    dplyr::mutate(adequacy.final = dplyr::case_when( # Conditions for Poor Adequacy
      is.na(adequacy.final) == TRUE & (ontostool1 > 13 | ontostool1 < 0 |
        is.na(stool1tostool2) |
        ontostool2 > 14 | ontostool2 < 1 | stool1tostool2 < 1 |
        stool.1.condition == "Poor" | stool.2.condition == "Poor") ~ 0,
      TRUE ~ adequacy.final
    )) |>
    dplyr::mutate(adequacy.final = dplyr::case_when( # Conditions for Good Adequacy
      is.na(adequacy.final) == TRUE & (ontostool1 <= 13 & ontostool1 >= 0 &
        ontostool2 <= 14 & ontostool2 >= 1 &
        stool1tostool2 >= 1 & stool.1.condition == "Good" &
        stool.2.condition == "Good") ~ 1,
      TRUE ~ adequacy.final
    )) |>
    dplyr::mutate(adequacy.final = dplyr::case_when( # Conditions for Missing Adequacy
      is.na(adequacy.final) == TRUE & (is.na(stool.1.condition) |
        is.na(stool.2.condition) |
        stool.1.condition == "Unknown" | stool.2.condition == "Unknown") ~ 99,
      TRUE ~ adequacy.final
    ))

  return(stool.data)
}

#' Helper function to generate stool adequacy on a rolling basis
#'
#' @param stool.data AFP dataset containing the adequacy final column
#' @param pop.data population data
#' @param start_date start date
#' @param end_date  end date
#' @param spatial_scale "ctry", "prov", or "dist".
#' @keywords internal
#'
#' @return summary table containing stool adequacy on a rolling basis
stool_ad_rolling <- function(stool.data, pop.data, start_date, end_date, spatial_scale) {

  # static local vars
  names.ctry <- c("adm0guid", "year", "ctry")
  names.prov <- c(names.ctry, "adm1guid", "prov")
  names.dist <- c(names.prov, "adm2guid", "dist")

  geo <- switch(spatial_scale,
                "ctry" = "adm0guid",
                "prov" = "adm1guid",
                "dist" = "adm2guid"
  )

  pop_cols <- switch(
    spatial_scale,
    "ctry" = names.ctry,
    "prov" = names.prov,
    "dist" = names.dist
  )

  pop.data <- dplyr::ungroup(pop.data) |>
    dplyr::select(dplyr::all_of(pop_cols), -"year") |>
    dplyr::distinct()

  int.data <- stool.data |>
    dplyr::group_by(get(geo)) |>
    summarize(
      afp.cases = sum(!is.na(cdc.classification.all2)),
      num.ad.plus.inad = sum(.data$adequacy.final == 1 | adequacy.final == 0, na.rm = T),
      num.adequate = sum(.data$adequacy.final == 1, na.rm = T),
      num.inadequate = sum(.data$adequacy.final == 0, na.rm = T),
      bad.data = sum(.data$adequacy.03 == 77, na.rm = T),
      same.day.stool.collection = sum(.data$stool1tostool2 == 0, na.rm = T),
      late.collection = sum(.data$ontostool1 > 14 | .data$ontostool2 > 14, na.rm = T),
      bad.condition = sum(.data$stool.1.condition == "Poor" | .data$stool.2.condition == "Poor", na.rm = T),
      missing.condition = sum((is.na(.data$stool.1.condition) | is.na(.data$stool.2.condition)) & afp.cases != 0),
      missing.stool1.condition = sum(is.na(.data$stool.1.condition) & afp.cases != 0),
      missing.stool2.condition = sum(is.na(.data$stool.2.condition) & afp.cases != 0),
      missing.stool1 = sum(.data$stool1missing == 1),
      missing.stool2 = sum(.data$stool2missing == 1),
      one.or.no.stool = sum(.data$stool1missing == 1 | .data$stool2missing == 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(per.stool.ad = 100 * (.data$num.adequate / .data$num.ad.plus.inad),
                  days.at.risk = as.numeric(end_date - start_date + 1))

  int.data <- switch(spatial_scale,
                     "ctry" = int.data |> dplyr::rename("adm0guid" = "get(geo)"),
                     "prov" = int.data |> dplyr::rename("adm1guid" = "get(geo)"),
                     "dist" = int.data |> dplyr::rename("adm2guid" = "get(geo)")
  )

  int.data <- int.data |>
    suppressMessages(dplyr::left_join(pop.data))

  int.data <- int.data |>
    mutate(per.stool.ad = dplyr::if_else(.data$afp.cases == 0, NA, .data$per.stool.ad)) |>
    dplyr::mutate(dplyr::across("afp.cases":"one.or.no.stool", \(x) tidyr::replace_na(x, 0)))

  return(int.data)

}

#' Helper function to generate stool adequacy on a non-rolling basis
#'
#' @param stool.data AFP data with adequacy final column
#' @param pop.data population data
#' @param year.data table generated by the generate_year_data() function
#' @param spatial_scale "ctry", "prov", or "dist
#' @keywords internal
#'
#' @return summary table of stool adequacy on a yearly basis
stool_ad_year <- function(stool.data, pop.data, year.data, spatial_scale) {
  geo <- switch(spatial_scale,
                "ctry" = "adm0guid",
                "prov" = "adm1guid",
                "dist" = "adm2guid"
  )

  int.data <- stool.data |>
    dplyr::group_by(get(geo), .data$year) |>
    summarize(
      afp.cases = sum(!is.na(cdc.classification.all2)),
      num.ad.plus.inad = sum(.data$adequacy.final == 1 | adequacy.final == 0, na.rm = T),
      num.adequate = sum(.data$adequacy.final == 1, na.rm = T),
      num.inadequate = sum(.data$adequacy.final == 0, na.rm = T),
      bad.data = sum(.data$adequacy.03 == 77, na.rm = T),
      same.day.stool.collection = sum(.data$stool1tostool2 == 0, na.rm = T),
      late.collection = sum(.data$ontostool1 > 14 | .data$ontostool2 > 14, na.rm = T),
      bad.condition = sum(.data$stool.1.condition == "Poor" | .data$stool.2.condition == "Poor", na.rm = T),
      missing.condition = sum((is.na(.data$stool.1.condition) | is.na(.data$stool.2.condition)) & afp.cases != 0),
      missing.stool1.condition = sum(is.na(.data$stool.1.condition) & afp.cases != 0),
      missing.stool2.condition = sum(is.na(.data$stool.2.condition) & afp.cases != 0),
      missing.stool1 = sum(.data$stool1missing == 1),
      missing.stool2 = sum(.data$stool2missing == 1),
      one.or.no.stool = sum(.data$stool1missing == 1 | .data$stool2missing == 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(per.stool.ad = 100 * (.data$num.adequate / .data$num.ad.plus.inad))

  int.data <- switch(spatial_scale,
                     "ctry" = int.data |> dplyr::rename("adm0guid" = "get(geo)"),
                     "prov" = int.data |> dplyr::rename("adm1guid" = "get(geo)"),
                     "dist" = int.data |> dplyr::rename("adm2guid" = "get(geo)")
                     )


  int.data <- suppressMessages(dplyr::full_join(int.data, pop.data)) |>
    dplyr::left_join(year.data, by = join_by("year")) |>
    dplyr::rename("days.at.risk" = "n_days")

  int.data <- int.data |>
    dplyr::mutate(per.stool.ad = dplyr::if_else(.data$afp.cases == 0, NA, .data$per.stool.ad)) |>
    dplyr::mutate(dplyr::across("afp.cases":"one.or.no.stool", \(x) tidyr::replace_na(x, 0)))

  return(int.data)

}

#' Helper function to check missing variables in the AFP linelist based on spatial scale
#'
#' @param afp_data AFP dataset
#' @param spatial_scale "ctry", "prov", or "dist"
#' @keywords internal
#'
check_missing_afp_var <- function(afp_data, spatial_scale) {
  # file names
  names.afp.ctry <- c("adm0guid", "date", "cdc.classification.all2")
  names.afp.prov <- c(names.afp.ctry, "adm1guid")
  names.afp.dist <- c(names.afp.prov, "adm2guid")

  afp_cols <- switch(
    spatial_scale,
    "ctry" = names.afp.ctry,
    "prov" = names.afp.prov,
    "dist" = names.afp.dist
  )

  missing_afp_vars <- setdiff(afp_cols, names(afp_data))

  if (length(missing_afp_vars) > 0) {
    stop(paste0(
      "The following variables were not found in afp.data: ",
      paste(missing_afp_vars, collapse = ", ")
    ))
  }
}

#' Helper function to check missing variables in the pop file based on spatial scale
#'
#' @param pop_data population dataset
#' @param spatial_scale "ctry", "prov", or "dist"
#' @keywords internal
#'
check_missing_pop_var <- function(pop_data, spatial_scale) {
  # file names
  names.ctry <- c("adm0guid", "year", "ctry")
  names.prov <- c(names.ctry, "adm1guid", "prov")
  names.dist <- c(names.prov, "adm2guid", "dist")

  pop_cols <- switch(
    spatial_scale,
    "ctry" = names.ctry,
    "prov" = names.prov,
    "dist" = names.dist
  )

  missing_pop_vars <- setdiff(pop_cols, names(pop_data))
  if (length(missing_pop_vars) > 0) {
    stop(paste0(
      "The following variables were not found in pop.data: ",
      paste(missing_pop_vars, collapse = ", ")
    ))
  }
}

#' Helper function to check if spatial scale of data matches spatial scale chosen
#'
#' @param admin_data population file dataset
#' @param spatial_scale "ctry", "prov", or "dist"
#' @keywords internal
#'
check_spatial_scale <- function(admin_data, spatial_scale) {
  # Define a list for spatial scales to columns that should not be present
  invalid_columns <- list(
    ctry = c("adm1guid", "adm2guid"),
    prov = "adm2guid",
    dist = NULL # No invalid columns for district level
  )

  # Get the relevant set of invalid columns for the current spatial scale
  cols_to_check <- invalid_columns[[spatial_scale]]

  if (!is.null(cols_to_check) && any(cols_to_check %in% names(admin_data))) {
    stop("Please check that the spatial scale of the country admin data provided matches the `spatial.scale` chosen.")
  }
}

#' Helper function to filter GUIDs that are not consistent across temporal scale
#'
#' @param admin_data population dataset
#' @param spatial_scale "ctry", "prov", or "dist"
#' @param start_date start date
#' @param end_date end date
#' @keywords internal
#'
#' @return a list of GUIDs not present in the time period
get_incomplete_adm <- function(admin_data, spatial_scale, start_date, end_date) {
  guid_col <- paste0("adm", match(spatial_scale, c("ctry", "prov", "dist")) - 1, "guid")
  expected_freq <- length(lubridate::year(start_date):lubridate::year(end_date))

  incomplete_adm <- incomplete_adm <- admin_data |>
    group_by(dplyr::across(dplyr::all_of(guid_col))) |>
    dplyr::summarize(freq = dplyr::n(), .groups = "drop") |>
    dplyr::filter(freq < expected_freq) |>
    dplyr::pull(guid_col)

  return(incomplete_adm)
}

# Main function ----
#' Calculate percent stool adequacy on a case basis
#'
#' Creates an adequacy variable - 'missing' parameter defines how
#' missing data is treated - "Good" classifies missing data as good quality
#' (POLIS method)
#' "Bad" classifies all missing as bad quality, "missing" excludes missing from
#' the calculations
#' @import dplyr
#' @import lubridate
#'
#' @param afp.data `tibble` AFP data which includes GUID at a given spatial scale
#' formatted as `adm(0,1,2)guid`, onset date as `date` and `cdc.classification.all2`
#' which includes "NOT-AFP".
#' @param admin.data `tibble` Full list of country administrative units by a given
#' spatial scale including `year`, `adm(0,1,2)guid`, and `ctry/prov/dist` (as appropriate).
#' @param start.date `str` Starting date for analysis formatted as "YYYY-MM-DD".
#' @param end.date `str` Ending date for analysis as "YYYY-MM-DD".
#' @param spatial.scale `str` Geographic level to group analysis on.
#' - `"prov"` Province level.
#' - `"dist"` District level.
#' - `"ctry"` Country level.
#' @param missing `str` How to treat missing data. Valid values are: `"good", "bad", "remove"`. Defaults to `"good"`.
#' When calculating the `adequacy.final` column:
#' - `"good"` uses `adequacy.03`
#' - `"bad"` uses `adequacy.01`
#' - `"exclude"` uses `adequacy.02`
#' @param bad.data `str` How to  treat bad data. Valid values are:`"remove", "inadequate"`. Defaults to `"inadequate"`.
#' `"inadequate"` treats samples with bad data as inadequate.
#' @param rolling `bool` Should data be analyzed on a rolling bases? Defaults to `FALSE`.
#' @param sp_continuity_validation `bool` Should GUIDs not present in all years of the dataset be excluded? Default `TRUE`.
#'
#' @returns `tibble` Long format stool adequacy evaluations.
#' @examples
#' raw.data <- get_all_polio_data()
#' stool.ads <- f.stool.ad.01(raw.data$afp, raw.data$ctry.pop,
#'                            "2021-01-01", "2023-12-31",
#'                            "ctry", sp_continuity_validation = FALSE)
#'
#' @export

f.stool.ad.01 <- function(
    afp.data,
    admin.data,
    start.date,
    end.date,
    spatial.scale,
    missing = "good",
    bad.data = "inadequate",
    rolling = F,
    sp_continuity_validation = T) {


  # Local static vars
  names.ctry <- c("adm0guid", "year", "ctry")
  names.prov <- c(names.ctry, "adm1guid", "prov")
  names.dist <- c(names.prov, "adm2guid", "dist")

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
  admin.data <- dplyr::rename_with(admin.data, recode,
                                   ADM0_NAME = "ctry",
                                   ADM1_NAME = "prov",
                                   ADM2_NAME = "dist",
                                   ADM0_GUID = "adm0guid",
                                   u15pop.prov = "u15pop"
  )

  # Check data inputs
  # Analysis start and end date as defined by user (as a character)
  tryCatch(
    {
      start.date <- lubridate::as_date(start.date)
      end.date <- lubridate::as_date(end.date)
      years <- lubridate::year(start.date):year(end.date)
      ctry.years <- sort(unique(admin.data$year))
    },
    error = function(cond) {
      cond$message <- paste0(
        "Invalid date in either start.date or end.date. ",
        "Check if they are in 'YYYY-MM-DD' format and try again."
      )
      stop(cond)
    }
  )

  # Check that country data contains all the years necessary
  if (sum(!years %in% ctry.years) > 0) {
    unfound.years <- years[!(years %in% ctry.years)]
    stop(paste0(
      "There are no years of administrative level data for: ",
      paste0(unfound.years, collapse = ", ")
    ))
  }

  # Check if spatial.data param in appropriate format
  if (!spatial.scale %in% c("ctry", "prov", "dist")) {
    stop("'spatial.scale' can only be 'ctry', 'prov', or 'dist'")
  }

  # Check that bad.data param contains appropriate arguments
  if (!bad.data %in% c("remove", "inadequate")) {
    stop('Only "remove" and "adequate" are valid arguments for the bad.data parameter.')
  }

  # Check that missing param contains appropriate arguments
  if (!missing %in% c("good", "bad", "missing")) {
    stop('Only "good", "bad", and "exclude" are valid arguments for the missing parameter.')
  }

  # Perform checks
  switch(spatial.scale,
         "ctry" = {
           check_missing_afp_var(afp.data, "ctry")
           check_missing_pop_var(admin.data, "ctry")
           check_spatial_scale(admin.data, "ctry")
         },
         "prov" = {
           check_missing_afp_var(afp.data, "prov")
           check_missing_pop_var(admin.data, "prov")
           check_spatial_scale(admin.data, "prov")
         },
         "dist" = {
           check_missing_afp_var(afp.data, "dist")
           check_missing_pop_var(admin.data, "dist")
           check_spatial_scale(admin.data, "dist")
         }
  )

  # Get inconsistent GUIDs across temporal scale
  incomplete.adm <- get_incomplete_adm(admin.data, spatial.scale, start.date, end.date)

  # Filter data if spatial validation is true
  if (sp_continuity_validation) {
    # Output warning message
    if (length(incomplete.adm) > 0) {
      readline(paste0(
        "The following GUIDS at the ", spatial.scale,
        " were not valid across the temporal scale:\n",
        paste0(incomplete.adm, collapse = ", "),
        "\n Please hit [ENTER] to continue"
      ))
    }

    guid_col_name <- paste0("adm", match(spatial.scale, c("ctry", "prov", "dist")) - 1, "guid")
    admin.data <- admin.data |>
      dplyr::filter(!(.data[[guid_col_name]] %in% incomplete.adm))

    afp.data <- afp.data |>
      dplyr::filter(!(.data[[guid_col_name]] %in% incomplete.adm))
  }

  # Warning message about non-overlapping dates
  if (start.date < lubridate::as_date(afp.data$date |> min(na.rm = T))) {
    print(paste0(
      "Your specified start date is ",
      start.date,
      ", however the first date of AFP data available is ",
      afp.data$date |> min(na.rm = T),
      ". Please use caution interpreting any values calculated before ",
      afp.data$date |> min(na.rm = T)
    ))
  }

  # Filter AFP and population data based on start and end dates
  afp.data <- afp.data |>
    dplyr::filter(dplyr::between(date, start.date, end.date),
                  cdc.classification.all2 != "NOT-AFP")
  # Only years of analysis
  admin.data <- admin.data %>%
    dplyr::filter(dplyr::between(year,
                                 lubridate::year(start.date),
                                 lubridate::year(end.date)))

  # Generate days in a year table
  year.data <- generate_year_data(start.date, end.date)

  # Coding data to be adequate (1), inadequate (0), data missing (99), or data error (77)
  stool.data <- generate_ad_final_col(afp.data)

  # Selecting only relevant columns
  stool.data <- stool.data |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::select(
      "year", "adm0guid", "adm1guid", "adm2guid",
      "stool1tostool2", "ontostool1", "ontostool2",
      "stool.1.condition", "stool.2.condition",
      "adequacy.01", "adequacy.02", "adequacy.03",
      "stool1missing", "stool2missing",
      "adequacy.final", "cdc.classification.all2"
    )

  # Merge stool data with days in year
  year.pop.data <- suppressMessages(dplyr::left_join(year.data, admin.data))
  stool.data <- suppressMessages(dplyr::full_join(stool.data, year.pop.data))


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
                           dplyr::mutate(adequacy.final = dplyr::if_else(.data$adequacy.final == 99 , .data$adequacy.03, .data$adequacy.final))
                       },
                       "bad" = {
                         stool.data |>
                           dplyr::mutate(adequacy.final = dplyr::if_else(.data$adequacy.final == 99, .data$adequacy.01, .data$adequacy.final))
                       },
                       "exclude" = {
                         cli::cli_alert_warning("AFP cases with missing adequacy excluded from stool adequacy calculation.")
                         stool.data |>
                           dplyr::mutate(adequacy.final = dplyr::if_else(.data$adequacy.final == 99, .data$adequacy.02, .data$adequacy.final))
                       })

  # Calculate stool adequacy
  int.data <- NULL
  if (rolling) {
    int.data <- stool_ad_rolling(stool.data, admin.data, start.date, end.date, spatial.scale)
  } else {
    int.data <- stool_ad_year(stool.data, admin.data, year.data, spatial.scale)
  }

  int.data <- int.data |>
    dplyr::rename("adequacy.denominator" = "num.ad.plus.inad") |>
    tidyr::drop_na(dplyr::any_of(spatial.scale))

  return(int.data)
}
