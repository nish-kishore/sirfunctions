#' Checks missing population at each geographic level
#'
#' @param pop.data population data
#' @param spatial.scale "ctry", "prov", or "dist"
#'
#' @returns tibble with missing population data
check_missing_pop <- function(pop.data, spatial.scale) {
  valid.spatial.scales <- c("ctry", "prov", "dist")

  if (!spatial.scale %in% valid.spatial.scales) {
    stop("Please enter a valid spatial scale.")
  }

  geo <- switch(spatial.scale,
    "ctry" = "country",
    "prov" = "province",
    "dist" = "district"
  )

  missing.pop <- pop.data |>
    dplyr::filter(is.na(u15pop))

  if (nrow(missing.pop) > 0) {
    cli::cli_alert_warning(
      paste0(
        nrow(missing.pop),
        " ",
        geo,
        "(s)",
        " missing population data.\n"
      )
    )
  }

  return(missing.pop)
}


#' Check the population data in country data from a roll up in province and
#' district population data
#' @import dplyr cli
#' @param ctry.data country data to check for
#'
#' @returns tibble containing differences in population data
check_pop_rollout <- function(ctry.data) {
  # compare difference in country vs. prov vs. dist roll up
  prov_pop <- dplyr::summarize(dplyr::group_by(ctry.data$prov.pop, year),
    tot.freq = sum(u15pop, na.rm = T)
  )
  dist_pop <- dplyr::summarize(dplyr::group_by(ctry.data$dist.pop, year),
    tot.freq = sum(u15pop, na.rm = T)
  )

  pop_file <- dplyr::left_join(ctry.data$ctry.pop, prov_pop, by = c("year" = "year"))
  pop_file <- dplyr::left_join(pop_file, dist_pop, by = c("year" = "year"))
  pop_file$ctryvprov_diff <- pop_file$u15pop - pop_file$tot.freq.x
  pop_file$ctryvdist_diff <- pop_file$u15pop - pop_file$tot.freq.y
  pop_file$ctryvprov_per <- round(100 * (pop_file$ctryvprov_diff / pop_file$u15pop), 1)
  pop_file$ctryvdist_per <- round(100 * (pop_file$ctryvdist_diff / pop_file$u15pop), 1)

  pop_file <- pop_file |>
    dplyr::mutate(dplyr::across(
      c(tot.freq.x, tot.freq.y, ctryvprov_diff, ctryvdist_diff),
      as.integer
    ))

  pop_file <- pop_file |>
    dplyr::rename(
      "Country pop" = "u15pop",
      "Total pop from province rollup" = "tot.freq.x",
      "Total pop from district rollup" = "tot.freq.y",
      "Difference in country and province rollup" = "ctryvprov_diff",
      "Difference in country and district rollup" = "ctryvdist_diff",
      "Percent difference in country and province rollup" = "ctryvprov_per",
      "Percent difference in country and district rollup" = "ctryvdist_per"
    )

  pop_file <- pop_file[, c(1, 3, 6, 7, 8, 9, 10, 11, 5, 2)]

  diff_prov <-
    sum(pop_file$`Percent difference in country and province rollup`)
  diff_dist <-
    sum(pop_file$`Percent difference in country and district rollup`)

  if (diff_prov != 0) {
    cli::cli_alert_warning("Differences in population country and province roll up found.")
  }

  if (diff_dist != 0) {
    cli::cli_alert_warning("Differences in population country and district roll up found.")
  }

  cli::cli_alert("Please check the error logs of ctry.data for differences in population roll ups.")

  return(pop_file)
}

#' Check for GUIDs that has changed due to redistricting
#' @import dplyr cli
#' @param pop.data population data
#' @param spatial.scale spatial scale of the data
#'
#' @returns list containing invalid GUIDs
spatial_validation <- function(pop.data, spatial.scale) {
  valid.spatial.scales <- c("ctry", "prov", "dist")

  if (!spatial.scale %in% valid.spatial.scales) {
    stop("Please enter a valid spatial scale.")
  }

  geo <- switch(spatial.scale,
    "ctry" = "country",
    "prov" = "province",
    "dist" = "district"
  )

  incomplete.adm <- switch(spatial.scale,
    "ctry" = {
      pop.data |>
        dplyr::group_by(.data$adm0guid) |>
        dplyr::summarize(
          freq = dplyr::n(), years_active = paste0(min(year), "-", max(year)),
          ctry = unique(ctry)
        ) |>
        dplyr::filter(freq < length(min(pop.data$year):max(pop.data$year))) |>
        dplyr::select("ctry", "adm0guid", "years_active") |>
        dplyr::arrange(.data$ctry, .data$years_active)
    },
    "prov" = {
      pop.data |>
        dplyr::group_by(.data$adm1guid) |>
        dplyr::summarize(
          freq = dplyr::n(), years_active = paste0(min(year), "-", max(year)),
          prov = unique(prov)
        ) |>
        dplyr::filter(freq < length(min(pop.data$year):max(pop.data$year))) |>
        dplyr::select("prov", "adm1guid", "years_active") |>
        dplyr::arrange(.data$prov, .data$years_active)
    },
    "dist" = {
      pop.data |>
        dplyr::group_by(.data$adm2guid) |>
        dplyr::summarize(
          freq = dplyr::n(), years_active = paste0(min(year), "-", max(year)),
          dist = unique(dist)
        ) |>
        dplyr::filter(freq < length(min(pop.data$year):max(pop.data$year))) |>
        dplyr::select("dist", "adm2guid", "years_active") |>
        dplyr::arrange(.data$dist, .data$years_active)
    }
  )

  if (nrow(incomplete.adm) > 0) {
    cli::cli_alert_warning(
      paste0(
        nrow(incomplete.adm),
        " GUIDs at the ",
        geo,
        " level",
        " were not consistent across the temporal scale of the desk review.\n"
      )
    )
  }

  return(incomplete.adm)
}

#' Get a long version of shapefile at a given spatial scale
#'
#' This function was used primarily as a way to build maps in the desk review.
#'  However, the map generation functions in the desk review will now use the long
#'  formatted shapefile as of `sirfunctions v1.2`.
#'  This function will be changed in the next version.
#'
#' [load_clean_ctry_sp()], [load_clean_prov_sp()], [load_clean_dist_sp()] with
#' `type = long` are the longform shapefiles.
#'
#' @import dplyr
#' @param ctry.data `list` List containing country data. Either the result of
#' [extract_country_data()] or [init_dr()].
#' @param spatial.scale `str` Either `"ctry", "prov", "dist"`.
#'
#' @returns `sf` Data frame including the most recent shapefile.
#' @examples
#' \dontrun{
#' raw.data <- get_all_polio_data()
#' ctry.data <- extract_country_data("algeria", raw.data)
#' ctry.shape <- set_shapefiles(ctry.data, "ctry")
#'
#' # Using init_dr()
#' ctry.data <- init_dr("algeria")
#' ctry.shape <- set_shapefiles(ctry.data, "ctry")
#' }
#'
#' @export
set_shapefiles <- function(ctry.data, spatial.scale) {
  valid.spatial.scales <- c("ctry", "prov", "dist")

  if (!spatial.scale %in% valid.spatial.scales) {
    stop("Please enter a valid spatial scale.")
  }

  pop.shape <- ctry.data[[spatial.scale]] %>%
    dplyr::filter(yr.end == max(yr.end))

  return(pop.shape)
}
