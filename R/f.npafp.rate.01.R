#' Calculate Non-Polio AFP Rate Function
#'
#' @name f.npafp.rate.01
#' @description Calculate the NPAFP rate from POLIS data
#' @import dplyr
#' @import lubridate
#' @import tidyr
#' @param afp.data tibble: AFP data which includes GUID at a given spatial scale
#' formated as "adm(0,1,2)guid, onset date as "date" and cdc.classification.all2 which includes
#' c("NPAFP", "PENDING", "LAB PENDING")
#' @param pop.data tibble: Under 15 population data by a given spatial scale including
#' "year", "adm(0,1,2)guid, "u15pop", and "(ctry/prov/dist)" as appropriate
#' @param start.date chr: "YYYY-MM-DD"
#' @param end.date chr: "YYYY-MM-DD"
#' @param spatial.scale chr: "prov" or "dist" or "ctry"
#' @param pending boolean: default TRUE
#' @param rolling boolean: default FALSE
#' @param sp_continuity_validation boolean: default TRUE
#' @returns tibble
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
  # check if afp.data and pop.data has arguments
  if (!(hasArg(afp.data) & hasArg(pop.data))) {
    stop("Please include both afp.data and pop.data as arguments to the function.")
  }

  # file names
  names.afp.ctry <- c("adm0guid", "date", "cdc.classification.all2")
  names.afp.prov <- c(names.afp.ctry, "adm1guid")
  names.afp.dist <- c(names.afp.prov, "adm2guid")

  names.pop.ctry <- c("adm0guid", "year", "u15pop", "ctry")
  names.pop.prov <- c(names.pop.ctry, "adm1guid", "prov")
  names.pop.dist <- c(names.pop.prov, "adm2guid", "dist")

  # check that data inputs have the same country data
  if (!sort(unique(pop.data$adm0guid)) == sort(unique(afp.data$adm0guid))) {
    stop("Please make sure that your `afp.data` and `pop.data` are subset for the same countries")
  }

  # Check data inputs
  # Analysis start and end date as defined by user (as a character)
  tryCatch(
    {
      start.date <- lubridate::as_date(start.date)
      end.date <- lubridate::as_date(end.date)
      years <- lubridate::year(start.date):lubridate::year(end.date)
      pop.years <- sort(unique(pop.data$year))
    },
    error = function(cond) {
      cond$message <- paste0(
        "Invalid date in either start.date or end.date. ",
        "Check if they are in 'YYYY-MM-DD' format and try again."
      )
      stop(cond)
    }
  )

  # Check that pop data contains all the years necessary
  if (sum(!years %in% pop.years) > 0) {
    unfound.years <- years[!(years %in% pop.years)]
    stop(paste0("There are no years of population data for: ", paste0(unfound.years, collapse = ", ")))
  }

  # Check dates and return warning if afp data out of range

  # Limit AFP data to the range described by the analysis start and end dates
  afp.data <- afp.data |>
    dplyr::filter(dplyr::between(date, start.date, end.date))

  # check if spatial.data param in appropriate format
  if (!spatial.scale %in% c("ctry", "prov", "dist")) {
    stop("'spatial.scale' can only be 'ctry', 'prov', or 'dist'")
  }

  # Helper function to check for any missing variables and spatial scale validity
  check_spatial_data <- function(spatial.scale, afp.names, pop.names) {
    afp_missing <- setdiff(afp.names, names(afp.data))
    pop_missing <- setdiff(pop.names, names(pop.data))

    if (length(afp_missing) > 0) {
      stop(paste0("The following variables were not found in afp.data: ", paste0(afp_missing, collapse = ", ")))
    }

    if (length(pop_missing) > 0) {
      stop(paste0("The following variables were not found in pop.data: ", paste0(pop_missing, collapse = ", ")))
    }

    # Check if pop data is of a larger spatial scale than the parameter chosen
    if (spatial.scale == "ctry" && any(c("adm1guid", "adm2guid") %in% names(pop.data))) {
      stop("Please check that the spatial scale of the pop data provided matches the 'spatial.scale' chosen.")
    } else if (spatial.scale == "prov" && any(c("adm2guid") %in% names(pop.data))) {
      stop("Please check that the spatial scale of the pop data provided matches the 'spatial.scale' chosen.")
    }
  }

  # Check data set for missing required variables
  switch(spatial.scale,
    "ctry" = check_spatial_data("ctry", names.afp.ctry, names.pop.ctry),
    "prov" = check_spatial_data("prov", names.afp.prov, names.pop.prov),
    "dist" = check_spatial_data("dist", names.afp.dist, names.pop.dist)
  )

  # Check for invalid GUIDs and remove them if sp_continuity_validation = TRUE
  incomplete.adm <- switch(spatial.scale,
    "ctry" = {
      pop.data |>
        dplyr::group_by(adm0guid) |>
        dplyr::summarize(freq = dplyr::n()) |>
        dplyr::filter(freq < length(lubridate::year(start.date):lubridate::year(end.date))) |>
        dplyr::pull(adm0guid)
    },
    "prov" = {
      pop.data |>
        dplyr::group_by(adm1guid) |>
        dplyr::summarize(freq = dplyr::n()) |>
        dplyr::filter(freq < length(lubridate::year(start.date):lubridate::year(end.date))) |>
        dplyr::pull(adm1guid)
    },
    "dist" = {
      pop.data |>
        dplyr::group_by(adm2guid) |>
        dplyr::summarize(freq = dplyr::n()) |>
        dplyr::filter(freq < length(lubridate::year(start.date):lubridate::year(end.date))) |>
        dplyr::pull(adm2guid)
    }
  )

  if (sp_continuity_validation) {
    pop.data <- switch(spatial.scale,
      "ctry" = {
        pop.data |> dplyr::filter(!adm0guid %in% incomplete.adm)
      },
      "prov" = {
        pop.data |> dplyr::filter(!adm1guid %in% incomplete.adm)
      },
      "dist" = {
        pop.data |> dplyr::filter(!adm2guid %in% incomplete.adm)
      }
    )
    afp.data <- switch(spatial.scale,
      "ctry" = {
        afp.data |> dplyr::filter(!adm0guid %in% incomplete.adm)
      },
      "prov" = {
        afp.data |> dplyr::filter(!adm1guid %in% incomplete.adm)
      },
      "dist" = {
        afp.data |> dplyr::filter(!adm2guid %in% incomplete.adm)
      }
    )

    if (length(incomplete.adm) > 0) {
      readline(paste0(
        "The following GUIDS at the ", spatial.scale,
        " were not valid across the temporal scale:\n",
        paste0(incomplete.adm, collapse = ", "),
        "\n Please hit [ENTER] to continue"
      ))
    }
  } else {
    if (length(incomplete.adm) > 0) {
      paste0(
        "The following GUIDS at the ", spatial.scale,
        " were not valid across the temporal scale:\n",
        paste0(incomplete.adm, collapse = ", ")
      )
    }
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

  # This function works in three stages:
  # 1) subset afp (incorporating pending information) and population data for the time periods and regions of interest
  # 2) calculate the year weights based on the start and end dates
  # 3) Calculate the npafp rate based on spatial scale and "rolling" parameter

  # Filter population data to the years of the analysis date and to the
  # country of interest
  pop.data <- pop.data |>
    dplyr::filter(year >= lubridate::year(start.date) &
      year <= lubridate::year(end.date)) # Only years of analysis # Only country of analysis

  # check status of pending and filter data appropriately
  if (pending) {
    npafp.data <- afp.data |>
      dplyr::as_tibble() |>
      dplyr::filter(
        cdc.classification.all2 %in% c("NPAFP", "PENDING", "LAB PENDING")
      ) |> # filter all AFP data such that only cases listed as NPAFP,

      # PENDING, or LAB PENDING are counted
      dplyr::select(epid, date, ctry, adm0guid, prov, adm1guid, dist, adm2guid)
    # Keep only the listed variables (removes all others)
  } else {
    npafp.data <- afp.data |>
      dplyr::as_tibble() |>
      dplyr::filter(cdc.classification.all2 == "NPAFP") |> # filter all AFP data such
      # that only cases listed as NPAFP are counted
      dplyr::select(epid, date, ctry, adm0guid, prov, adm1guid, dist, adm2guid)
    # Keep only the listed variables (removes all others)
  }

  # calculate year weight table
  # convert start and end date into year-wise chunks with year weights
  year.data <- dplyr::tibble(
    "year" = lubridate::year(start.date):lubridate::year(end.date) # Defines year as the amount of
    # time between the start date and end date by calendar year (eg 2019:2020)
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      days_in_year = length(seq(lubridate::as_date(paste0(year, "-01-01")),
        lubridate::as_date(paste0(year, "-12-31")),
        by = "day"
      )),
      # Number of days in calendar year
      earliest_date = ifelse(lubridate::year(start.date) == year, start.date,
        lubridate::as_date(paste0(year, "-01-01"))
      ),
      # Create earliest date per calendar year - if it is the year of start
      # date, then use the exact start date, otherwise use January 1
      earliest_date = lubridate::as_date(earliest_date),
      latest_date = ifelse(lubridate::year(end.date) == year,
        end.date, lubridate::as_date(paste0(year, "-12-31"))
      ),
      # Create latest date per calendar year - if it is the year of end date,
      # then use the exact end date, otherwise use December 31
      latest_date = lubridate::as_date(latest_date),
      n_days = as.integer(latest_date - earliest_date + 1), # Calculate number
      # of days in calendar year between earliest and latest date of that
      # calendar year
      weight = n_days / days_in_year # Calculate weight adjustment for number
      # of cases based on number of days in given calendar year
    )

  # subset afp.data tibble into time chunks based on previous response
  # along with geographic chunks

  # algo npafp cases / pop * 100000 * time_weight

  # bind all together and return tibble


  # For merging on country - Country name is used from the ctry.data set -
  # this pulled from extract_ctry_info - merge is then subsequently by year
  # alone.
  if (spatial.scale == "ctry") { # Analysis at country level
    int.data <- npafp.data |>
      dplyr::mutate(year = lubridate::year(date)) |>
      dplyr::group_by(adm0guid, year) |>
      dplyr::group_split() |>
      lapply(function(x) { # Create dataframe with unique country name,
        # adm0guid, year, and number of NPAFP cases
        dplyr::tibble(
          "ctry" = unique(x$ctry),
          "adm0guid" = unique(x$adm0guid),
          "year" = unique(x$year),
          "n_npafp" = nrow(x)
        )
      }) |>
      dplyr::bind_rows()

    int.data <- dplyr::full_join( # Merge AFP case data with population data by year
      pop.data,
      int.data |> dplyr::select(adm0guid, year, n_npafp),
      by = c("year", "adm0guid")
    ) |>
      dplyr::full_join( # Merge AFP case and population data with calculated yearly
        # weights
        year.data,
        by = c("year")
      ) |>
      tidyr::replace_na(list("n_npafp" = 0)) |> # Change all NAs to 0 for calculation
      # Calculate NPAFP rate per 100,000 annualized by calendar year
      dplyr::select(
        year, ctry, n_npafp, u15pop, n_days, days_in_year,
        weight, adm0guid, earliest_date, latest_date
      ) |> # Select columns to keep for output
      {
        function(.) {
          if (rolling) {
            dplyr::mutate(., par = weight * u15pop) |>
              dplyr::group_by(ctry, adm0guid) |>
              dplyr::summarize(
                n_npafp = sum(n_npafp, na.rm = T),
                par = sum(par),
                earliest_date = min(earliest_date),
                latest_date = max(latest_date)
              ) |>
              dplyr::ungroup() |>
              dplyr::mutate(
                days_at_risk = as.numeric(latest_date - earliest_date),
                npafp_rate = n_npafp / par * 100000
              )
          } else {
            dplyr::mutate(., npafp_rate = n_npafp / u15pop * 100000 / weight)
          }
        }
      }()
  }

  # Province level calculation
  if (spatial.scale == "prov") { # Analysis at province level
    int.data <- npafp.data |>
      dplyr::mutate(year = lubridate::year(date)) |>
      dplyr::group_by(adm0guid, adm1guid, year) |>
      dplyr::group_split() |>
      lapply(function(x) { # Create dataframe with unique country name,
        # adm0guid, province name, adm1guid, year, and number of NPAFP cases
        dplyr::tibble(
          "ctry" = unique(x$ctry),
          "adm0guid" = unique(x$adm0guid),
          "prov" = unique(x$prov),
          "adm1guid" = unique(x$adm1guid),
          "year" = unique(x$year),
          "n_npafp" = nrow(x)
        )
      }) |>
      dplyr::bind_rows()

    int.data <- dplyr::full_join( # Merge AFP case data with population data by year
      # and adm1guid
      pop.data,
      int.data |> dplyr::select(adm0guid, adm1guid, year, n_npafp),
      by = c("adm0guid", "adm1guid", "year")
    ) |>
      tidyr::replace_na(list("n_npafp" = 0)) |> # Change all NAs to 0 for calculation
      dplyr::full_join( # Merge AFP case and population data with calculated yearly
        # weights
        year.data,
        by = c("year")
      ) |>
      # Calculate NPAFP rate per 100,000 annualized by calendar year
      dplyr::select( # Select columns to keep for output
        year, n_npafp, u15pop, n_days, days_in_year,
        weight, adm0guid, adm1guid, ctry, prov, earliest_date, latest_date
      ) |>
      {
        function(.) {
          if (rolling) {
            dplyr::mutate(., par = weight * u15pop) |>
              dplyr::group_by(ctry, prov, adm0guid, adm1guid) |>
              dplyr::summarize(
                n_npafp = sum(n_npafp, na.rm = T),
                par = sum(par),
                earliest_date = min(earliest_date),
                latest_date = max(latest_date)
              ) |>
              dplyr::ungroup() |>
              dplyr::mutate(
                days_at_risk = as.numeric(latest_date - earliest_date),
                npafp_rate = n_npafp / par * 100000
              )
          } else {
            dplyr::mutate(., npafp_rate = n_npafp / u15pop * 100000 / weight)
          }
        }
      }()
  }

  # District level calculation
  if (spatial.scale == "dist") { # Analysis at district level
    int.data <- npafp.data |>
      dplyr::mutate(year = lubridate::year(date)) |>
      dplyr::group_by(adm0guid, adm1guid, adm2guid, year) |>
      dplyr::group_split() |>
      lapply(function(x) { # Create dataframe with unique country name,
        # adm0guid, province name, adm1guid, district name, adm2guid, year,
        # and number of NPAFP cases
        dplyr::tibble(
          "ctry" = unique(x$ctry),
          "adm0guid" = unique(x$adm0guid),
          "adm1guid" = unique(x$adm1guid),
          "adm2guid" = unique(x$adm2guid),
          "prov" = unique(x$prov),
          "dist" = unique(x$dist),
          "year" = unique(x$year),
          "n_npafp" = nrow(x)
        )
      }) |>
      dplyr::bind_rows()

    int.data <- dplyr::full_join( # Merge AFP case data with population data by year
      # and adm1guid and adm2guid
      pop.data,
      int.data |> dplyr::select(adm0guid, adm1guid, adm2guid, year, n_npafp),
      by = c("adm0guid", "adm1guid", "adm2guid", "year")
    ) |>
      tidyr::replace_na(list("n_npafp" = 0)) |>
      dplyr::full_join( # Merge AFP case and population data with calculated yearly
        # weights
        year.data,
        by = c("year")
      ) |>
      dplyr::select(
        year, ctry, prov, dist, n_npafp, u15pop, weight, n_days,
        days_in_year, earliest_date, latest_date, adm0guid, adm1guid, adm2guid
      ) |>
      {
        function(.) {
          if (rolling) {
            dplyr::mutate(., par = weight * u15pop) |>
              dplyr::group_by(ctry, prov, dist, adm0guid, adm1guid, adm2guid) |>
              dplyr::summarize(
                n_npafp = sum(n_npafp, na.rm = T),
                par = sum(par),
                earliest_date = min(earliest_date),
                latest_date = max(latest_date)
              ) |>
              dplyr::ungroup() |>
              dplyr::mutate(
                days_at_risk = as.numeric(latest_date - earliest_date),
                npafp_rate = n_npafp / par * 100000
              )
          } else {
            dplyr::mutate(., npafp_rate = n_npafp / u15pop * 100000 / weight)
          }
        }
      }()
  }

  return(int.data) # Data frame to return from function
}
