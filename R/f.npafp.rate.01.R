# Helper functions ----
#' Calculate NPAFP by year and geographic level
#'
#' Helper function for calculating the NPAFP rate based on the geographic level.
#' This function is used inside [f.npafp.rate.01()].
#'
#' @param afp.data `tibble` AFP linelist. Either from `raw.data$afp` of [get_all_polio_data()]
#' or `ctry.data$afp.all.2` of [extract_country_data()].
#' @param pop.data `tibble` Population data. Either from `raw.data${ctry/prov/dist}.pop` of [get_all_polio_data()]
#' or `ctry.data${ctry/prov/dist}.pop` of [extract_country_data()].
#' @param year.data `tibble` Summary table. Created from [generate_year_data()].
#' @param spatial_scale Spatial scale. Valid arguments are: `"ctry", "prov", "dist`.
#' @keywords internal
#'
#' @return `tibble` A summary table including NPAFP rates and population data.
npafp_year <- function(afp.data, pop.data, year.data, spatial_scale) {
  # static local vars
  names.ctry <- c("adm0guid", "year", "ctry")
  names.prov <- c(names.ctry, "adm1guid", "prov")
  names.dist <- c(names.prov, "adm2guid", "dist")

  geo <- switch(spatial_scale,
    "ctry" = "adm0guid",
    "prov" = "adm1guid",
    "dist" = "adm2guid"
  )

  pop_cols <- switch(spatial_scale,
    "ctry" = names.ctry,
    "prov" = names.prov,
    "dist" = names.dist
  )

  int.data <- afp.data |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::group_by(get(geo), .data$year) |>
    dplyr::summarise(
      n_npafp = sum(!is.na(.data$epid)),
      afp.case = sum(!is.na(cdc.classification.all2), na.rm = T),
      num.wpv.cases = sum(wild.1 == TRUE, wild.3 == TRUE, na.rm = T),
      num.vdpv1.cases = sum(vdpv.1 == TRUE, na.rm = T),
      num.vdpv2.cases = sum(vdpv.2 == TRUE, na.rm = T),
      num.vdpv3.cases = sum(vdpv.3 == TRUE, na.rm = T)
    ) |>
    dplyr::ungroup()

  int.data <- switch(spatial_scale,
    "ctry" = int.data |> dplyr::rename("adm0guid" = "get(geo)"),
    "prov" = int.data |> dplyr::rename("adm1guid" = "get(geo)"),
    "dist" = int.data |> dplyr::rename("adm2guid" = "get(geo)")
  )

  int.data <- dplyr::full_join(int.data, pop.data) |>
    dplyr::left_join(year.data) |>
    dplyr::mutate(npafp_rate = .data$n_npafp / .data$u15pop * 100000 / .data$weight) |>
    dplyr::select(dplyr::all_of(c(
      "year", "n_npafp", "u15pop",
      "n_days", "days_in_year", "weight",
      "earliest_date", "latest_date", "npafp_rate",
      pop_cols
    ))) |>
    arrange(!!!dplyr::syms(spatial_scale), .data$year)

  return(int.data)
}

#' Calculate the NPAFP rate on a rolling basis
#'
#' Calculates the NPAFP rate on a rolling basis, based on the start and end dates
#' specified.
#'
#' @param afp.data `tibble` AFP linelist, Either from `raw.data$afp` of [get_all_polio_data()]
#' or `ctry.data$afp.all.2` of [extract_country_data()].
#' @param year.pop.data `tibble` Summary table containing year and pop data. This is created
#' inside [f.npafp.rate.01()].
#' @param start_date `str` Start date to calculate the rolling interval for.
#' @param end_date `str` End date to calculate the rolling interval for.
#' @param spatial_scale Spatial scale. Valid arguments are: `"ctry", "prov", "dist`.
#' @keywords internal
#'
#' @return A summary table including NPAFP rates and population data.
npafp_rolling <- function(afp.data, year.pop.data, start_date, end_date, spatial_scale) {
  # static local vars
  names.ctry <- c("adm0guid", "year", "ctry")
  names.prov <- c(names.ctry, "adm1guid", "prov")
  names.dist <- c(names.prov, "adm2guid", "dist")
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  geo <- switch(spatial_scale,
    "ctry" = "adm0guid",
    "prov" = "adm1guid",
    "dist" = "adm2guid"
  )

  pop_cols <- switch(spatial_scale,
    "ctry" = names.ctry,
    "prov" = names.prov,
    "dist" = names.dist
  )

  # Calculate par
  par.data <- year.pop.data |>
    dplyr::group_by(get(geo)) |>
    summarise(par = sum(.data$weight * .data$u15pop)) |>
    ungroup()


  # Count the number of NPAFP cases
  int.data <- afp.data |>
    dplyr::group_by(get(geo)) |>
    dplyr::summarise(
      n_npafp = sum(!is.na(.data$epid)),
      earliest_date = min(.data$earliest_date),
      latest_date = max(.data$latest_date),
      afp.case = sum(!is.na(cdc.classification.all2), na.rm = T),
      num.wpv.cases = sum(wild.1 == TRUE, wild.3 == TRUE, na.rm = T),
      num.vdpv1.cases = sum(vdpv.1 == TRUE, na.rm = T),
      num.vdpv2.cases = sum(vdpv.2 == TRUE, na.rm = T),
      num.vdpv3.cases = sum(vdpv.3 == TRUE, na.rm = T)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(days.at.risk = as.numeric(end_date - start_date + 1))

  # Join to par data
  int.data <- int.data |>
    dplyr::left_join(par.data)

  int.data <- switch(spatial_scale,
    "ctry" = int.data |> dplyr::rename("adm0guid" = "get(geo)"),
    "prov" = int.data |> dplyr::rename("adm1guid" = "get(geo)"),
    "dist" = int.data |> dplyr::rename("adm2guid" = "get(geo)")
  )

  int.data <- int.data |>
    dplyr::mutate(npafp_rate = .data$n_npafp / .data$par * 100000)

  # Get population information
  int.data <- int.data |>
    dplyr::left_join(year.pop.data |>
      dplyr::select(dplyr::all_of(pop_cols), -year) |>
      dplyr::distinct())

  return(int.data)
}
# Main function ----

#' Calculate non-polio AFP rate
#'
#' Calculate the NPAFP rate from POLIS data. Can either pass `raw.data` to calculate NPAFP rates
#' on the global dataset, or a `ctry.data` dataset.
#' @import dplyr
#' @import lubridate
#' @import tidyr
#' @param afp.data `tibble` AFP data which includes GUID at a given spatial scale
#' formatted as `adm(0,1,2)guid`, onset date as `date` and `cdc.classification.all2` which includes
#' `"NPAFP", "PENDING", "LAB PENDING"`. This is either `ctry.data$afp.all.2` of [extract_country_data()] or
#' [init_dr()] or `raw.data$afp` of [get_all_polio_data()].
#' @param pop.data `tibble` Under 15 population data by a given spatial scale including
#' `year`, `adm(0,1,2)guid`, `u15pop`, and `ctry/prov/dist` as appropriate. This is part of the output of
#' [get_all_polio_data()] and [extract_country_data()].
#' @param start.date `str` Start date with the format `"YYYY-MM-DD"`.
#' @param end.date `str` Start date with the format `"YYYY-MM-DD"`.
#' @param spatial.scale `str` Spatial scale for analysis.
#' - `"prov"` Province level.
#' - `"dist"` District level.
#' - `"ctry"` Country level.
#' @param pending `bool` Should cases classified as `PENDING` or `LAB PENDING` be included in calculations? Default `TRUE`.
#' @param rolling `bool` Should the analysis be performed on a rolling bases? Default `FALSE`.
#' @param sp_continuity_validation Should we filter places that are not present
#' for the entirety of the analysis dates? Default `TRUE`.
#' @returns `tibble` A table containing NPAFP rates as well as additional information relevant to each location analyzed.
#' @export
f.npafp.rate.01 <- function(
    afp.data,
    pop.data,
    start.date,
    end.date,
    spatial.scale,
    pending = T,
    rolling = F,
    sp_continuity_validation = T) {
  # Check if afp.data and pop.data has arguments
  if (!(hasArg(afp.data) & hasArg(pop.data))) {
    stop("Please include both afp.data and pop.data as arguments to the function.")
  }

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
  pop.data <- dplyr::rename_with(pop.data, recode,
    ADM0_NAME = "ctry",
    ADM1_NAME = "prov",
    ADM2_NAME = "dist",
    ADM0_GUID = "adm0guid",
    u15pop.prov = "u15pop"
  )


  # Local static vars
  names.ctry <- c("adm0guid", "year", "ctry")
  names.prov <- c(names.ctry, "adm1guid", "prov")
  names.dist <- c(names.prov, "adm2guid", "dist")

  # Check data inputs
  # Analysis start and end date as defined by user (as a character)
  tryCatch(
    {
      start.date <- lubridate::as_date(start.date)
      end.date <- lubridate::as_date(end.date)
      years <- lubridate::year(start.date):year(end.date)
      ctry.years <- sort(unique(pop.data$year))
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

  # Perform checks
  check_missing_afp_var(afp.data, spatial.scale)
  check_missing_pop_var(pop.data, spatial.scale)
  check_spatial_scale(pop.data, spatial.scale)

  # Get inconsistent GUIDs across temporal scale
  incomplete.adm <- get_incomplete_adm(pop.data, spatial.scale, start.date, end.date)

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
    pop.data <- pop.data |>
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
    dplyr::filter(dplyr::between(date, start.date, end.date), age.months < 180)

  # Only years of analysis
  pop.data <- pop.data %>%
    dplyr::filter(dplyr::between(
      year,
      lubridate::year(start.date),
      lubridate::year(end.date)
    ))

  # Filter AFP data based on pending param
  if (pending) {
    afp.data <- afp.data |>
      dplyr::filter(cdc.classification.all2 %in% c("NPAFP", "PENDING", "LAB PENDING"))
  } else {
    afp.data <- afp.data |>
      dplyr::filter(cdc.classification.all2 == "NPAFP")
  }

  # Generate days in a year table
  year.data <- generate_year_data(start.date, end.date)

  # Select only relevant columns
  afp.data <- afp.data |>
    dplyr::select(dplyr::any_of(c(
      "epid", "date", "ctry", "adm0guid",
      "prov", "adm1guid", "dist", "adm2guid"
    ))) |>
    dplyr::mutate(year = lubridate::year(date))

  # Merge afp data with days in year
  year.pop.data <- suppressMessages(dplyr::left_join(year.data, pop.data))
  afp.data <- suppressMessages(dplyr::full_join(afp.data, year.pop.data))

  # Calculate NPAFP rate
  int.data <- NULL
  if (rolling) {
    int.data <- suppressMessages(npafp_rolling(afp.data, year.pop.data, start.date, end.date, spatial.scale))
  } else {
    int.data <- suppressMessages(npafp_year(afp.data, pop.data, year.data, spatial.scale))
    int.data <- int.data |>
      dplyr::filter(!is.na(.data$year))
  }

  numeric_cols <- c("n_npafp", "u15pop", "npafp_rate", "par")
  int.data <- int.data |>
    dplyr::mutate(dplyr::across(dplyr::any_of(numeric_cols), \(x) tidyr::replace_na(x, 0))) |>
    tidyr::drop_na(dplyr::any_of(spatial.scale))

  return(int.data)
}
