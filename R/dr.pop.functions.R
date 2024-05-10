#' Check the population data in country data from a roll up in province and
#' district population data
#'
#' @param ctry.data country data to check for
#'
#' @return tibble containing differences in population data
check_pop_rollout <- function(ctry.data) {
  # compare difference in country vs. prov vs. dist roll up
  prov_pop = summarize(group_by(ctry.data$prov.pop, year),
                       tot.freq = sum(u15pop, na.rm = T))
  dist_pop = summarize(group_by(ctry.data$dist.pop, year),
                       tot.freq = sum(u15pop, na.rm = T))

  pop_file = left_join(ctry.data$ctry.pop, prov_pop, by = c("year" = "year"))
  pop_file = left_join(pop_file, dist_pop, by = c("year" = "year"))
  pop_file$ctryvprov_diff = pop_file$u15pop - pop_file$tot.freq.x
  pop_file$ctryvdist_diff = pop_file$u15pop - pop_file$tot.freq.y
  pop_file$ctryvprov_per = round(100 * (pop_file$ctryvprov_diff / pop_file$u15pop), 1)
  pop_file$ctryvdist_per = round(100 * (pop_file$ctryvdist_diff / pop_file$u15pop), 1)

  pop_file = pop_file |>
    mutate(across(
      c(tot.freq.x, tot.freq.y, ctryvprov_diff, ctryvdist_diff),
      as.integer
    ))

  pop_file = pop_file |>
    rename(
      "Country pop" = "u15pop",
      "Total pop from province rollup" = "tot.freq.x",
      "Total pop from district rollup" = "tot.freq.y",
      "Difference in country and province rollup" = "ctryvprov_diff",
      "Difference in country and district rollup" = "ctryvdist_diff",
      "Percent difference in country and province rollup" = "ctryvprov_per",
      "Percent difference in country and district rollup" = "ctryvdist_per"
    )

  pop_file = pop_file[, c(1, 3, 6, 7, 8, 9, 10, 11, 5, 2)]

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

  return(pop_file)
}

#' Check for invalid spatial GUIDs
#'
#' @param pop.data population data
#' @param spatial.scale spatial scale of the data
#'
#' @return list containing invalid GUIDs
spatial_validation <- function(pop.data, spatial.scale) {
  valid.spatial.scales <- c("ctry", "prov", "dist")

  if (!spatial.scale %in% valid.spatial.scales) {
    stop("Please enter a valid spatial scale.")
  }

  geo <- switch(
    spatial.scale,
    "ctry" = "country",
    "prov" = "province",
    "dist" = "district"
  )

  incomplete.adm <- switch(
    spatial.scale,
    "ctry" = {
      pop.data |>
        group_by(adm0guid) |>
        summarize(freq = n()) |>
        filter(freq < length(min(pop.data$year):max(pop.data$year))) |>
        pull(adm0guid)
    },
    "prov" = {
      pop.data |>
        group_by(adm1guid) |>
        summarize(freq = n()) |>
        filter(freq < length(min(pop.data$year):max(pop.data$year))) |>
        pull(adm1guid)
    },
    "dist" = {
      pop.data |>
        group_by(adm2guid) |>
        summarize(freq = n()) |>
        filter(freq < length(min(pop.data$year):max(pop.data$year))) |>
        pull(adm2guid)
    }
  )

  if (length(incomplete.adm) > 0) {
    cli::cli_alert_warning(
      paste0(
        length(incomplete.adm),
        " GUIDs at the ",
        geo,
        " level",
        " were not valid across the temporal scale.\n"
      )
    )
  }

  return(incomplete.adm)
}

#' Get the most recent shapefile at a given spatial scale
#'
#' @param ctry.data RDS file containing country data
#' @param spatial.scale either "ctry", "prov", "dist"
#'
#' @return sf object of the most recent shapefile
#' @export
set_shapefiles <- function(ctry.data, spatial.scale) {
  valid.spatial.scales <- c("ctry", "prov", "dist")

  if (!spatial.scale %in% valid.spatial.scales) {
    stop("Please enter a valid spatial scale.")
  }

  pop.shape <- ctry.data[[spatial.scale]] %>%
    filter(yr.end == max(yr.end))

  return(pop.shape)

}
