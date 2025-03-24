#' Obtain available variables for VPD data
#'
#' @description
#' `lifecycle::badge("experimental")`
#'
#' Obtains the variable names available for each vaccine preventable disease (VPD)
#' in the `variable` column.
#'
#' @returns `tibble` A tibble with the available variable choices.
#' @export
#'
#' @examples
#' \dontrun{
#' vpd_vars <- get_vpd_vars()
#' }
get_vpd_vars <- function() {
  vpd_data <- edav_io(io = "read",
                      default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_clean/country_vpd_year_casedata.rds")
  summary <- vpd_data |>
    dplyr::select(vpd, vpd_short_name, variable, year) |>
    dplyr::group_by(vpd) |>
    dplyr::summarize(variable = paste0(sort(unique(variable)), collapse = ", "))

  return(summary)

}

#' Get vaccine preventable diseases dataset
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Gets the VPD dataset from EDAV.
#'
#'
#' @param vpd_name `str` A VPD or a list of VPDs. Defaults to `NULL`, which
#' returns the full VPD dataset.
#' @param variable_name `str` A variable or a list of variables. Defaults to `NULL`,
#' which returns a dataset containing all the variable names.
#' @param years `int` A year or a list of years. Defaults to `NULL`, which
#' returns a dataset containing all the years available.
#' @param ctry_name `str` A country or a list country names. Defaults to `NULL`,
#' which returns all the countries in the dataset.
#' @param iso3_codes `str` An ISO3 code or a list of ISO3 codes. Defaults to `NULL`,
#' which returns all the ISO3 codes in the dataset.
#' @param add_ctry_sf `bool` Attach the country shapefile? Defaults to `TRUE`.
#' @param add_ctry_pop `bool` Attach the country population data? Defaults to `TRUE`.
#'
#' @returns `list` A list containing the VPD data, and optionally the shapefile
#' and population data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' vpd_data <- get_vpd_data()
#' }
get_vpd_data <- function(vpd_name = NULL, variable_name = NULL, years = NULL,
                         ctry_name = NULL, iso3_codes = NULL,
                         add_ctry_sf = TRUE, add_ctry_pop = TRUE) {

  vpd_data_list <- list()
  vpd_data_list$vpd_data <- edav_io(io = "read",
                      default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_clean/country_vpd_year_casedata.rds")
  if (add_ctry_sf) {
    cli::cli_process_start("Add country shapefiles")
    vpd_data_list$ctry_sf <- edav_io(io = "read",
                                     default_dir = "GID/GIDMEA/giddatt",
                                     file_loc = "data_clean/ctry_shapes_updated.rds")
    cli::cli_process_done()
  }

  if (add_ctry_pop) {
    cli::cli_process_start("Adding country population data")
    vpd_data_list$ctry_pop <- edav_io(io = "read",
                                      default_dir = "GID/GIDMEA/giddatt",
                                      file_loc = "data_clean/ctry_pop.rds")
    vpd_data_list$ctry_pop <- vpd_data_list$ctry_pop |>
      dplyr::rename(c(country_name = "ADM0_NAME", iso3_code = "ISO_3_CODE")) |>
      dplyr::mutate(country_name = stringr::str_to_title(country_name))
    cli::cli_process_done()
  }

  # Filter VPD
  if (!is.null(vpd_name)) {
    invalid <- setdiff(vpd_name, vpd_data_list$vpd_data$vpd)

    if (length(invalid) != 0) {
      cli::cli_abort(paste0("Invalid VPD name passed: ",
                            paste0(invalid, collapse = ", ")))
    }

    vpd_data_list$vpd_data <- vpd_data_list$vpd_data |>
      dplyr::filter(vpd %in% vpd_name)
  }

  # Filter variable name
  if (!is.null(variable_name)) {
    invalid <- setdiff(variable_name, vpd_data_list$vpd_data$variable)

    if (length(invalid) != 0) {
      cli::cli_abort(paste0("Invalid VPD variable passed: ",
                            paste0(invalid, collapse = ", ")))
    }

    vpd_data_list$vpd_data <- vpd_data_list$vpd_data |>
      dplyr::filter(variable %in% variable_name)
  }

  # Filter year
  if (!is.null(years)) {
    invalid <- setdiff(year, vpd_data_list$vpd_data$year)

    if (length(invalid) != 0) {
      cli::cli_abort(paste0("Invalid year value passed: ",
                            paste0(invalid, collapse = ", ")))
    }

    vpd_data_list$vpd_data <- vpd_data_list$vpd_data |>
      dplyr::filter(year %in% years)
  }

  # Filter country name
  if (!is.null(ctry_name)) {
    invalid <- setdiff(ctry_name, vpd_data_list$vpd_data$country_name)

    if (length(invalid) != 0) {
      cli::cli_abort(paste0("Invalid VPD country passed: ",
                            paste0(invalid, collapse = ", ")))
    }

    vpd_data_list$vpd_data <- vpd_data_list$vpd_data |>
      dplyr::filter(country_name %in% ctry_name)

    if (add_ctry_sf) {
      vpd_data_list$ctry_sf <- vpd_data_list$ctry_sf |>
        dplyr::filter(country_name %in% ctry_name)
    }

    if (add_ctry_pop) {
      vpd_data_list$ctry_pop <- vpd_data_list$ctry_sf |>
        dplyr::filter(country_name %in% ctry_name)
    }
  }

  # Filter iso3_code
  if (!is.null(iso3_codes)) {
    invalid <- setdiff(iso3_codes, vpd_data_list$vpd_data$iso3_code)

    if (length(invalid) != 0) {
      cli::cli_abort(paste0("Invalid iso3_code passed: ",
                            paste0(invalid, collapse = ", ")))
    }

    vpd_data_list$vpd_data <- vpd_data_list$vpd_data |>
      dplyr::filter(iso3_code %in% iso3_codes)

    if (add_ctry_sf) {
      vpd_data_list$ctry_sf <- vpd_data_list$ctry_sf |>
        dplyr::filter(iso3_code %in% iso3_codes)
      }

    if (add_ctry_pop) {
      vpd_data_list$ctry_pop <- vpd_data_list$ctry_sf |>
        dplyr::filter(iso3_code %in% iso3_codes)
      }
  }

  if (!add_ctry_sf & !add_ctry_pop) {

    return(vpd_data_list$vpd_data)

  } else {

    return(vpd_data_list)
  }

}

#' Get years with missing data based on variable name and VPD
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' For each pair of VPD and variable name, returns the years with missing data
#' for each country.
#'
#'
#' @param vpd_name `str` A VPD name or a list of names.
#' @param variable_name `str` A variable name or a list of names.
#' @param min_year `int` Minimum year to analyze. Defaults to `1980`.
#' @param max_year `int` Maximum year to analyze. Defaults to the current year.
#'
#' @return `tibble` A summary table of years with missing data for each country
#' for a particular VPD and variable name.
#' @export
#'
#' @examples
#' \dontrun{
#' missing_years <- get_vpd_missingness("Cholera", "cases")
#' }
get_vpd_missingness <- function(vpd_name = NULL, variable_name = NULL,
                                min_year = 1980,
                                max_year = lubridate::year(Sys.Date())) {
  cli::cli_process_start("Getting VPD data")
  vpd_data <- edav_io(io = "read",
                      default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_clean/country_vpd_year_casedata.rds")
  cli::cli_process_done()
  # only 1980 and above
  vpd_data <- vpd_data |>
    dplyr::filter(dplyr::between(year, min_year, max_year))

  cli::cli_process_start("Getting full VPD list")
  full_vpd <- edav_io(io = "read",
                      default_dir = "GID/GIDMEA/giddatt",
                      file_loc = "data_clean/ref_vpd.rds") |>
    dplyr::select(vpd)
  cli::cli_process_done()

  cli::cli_process_start("Getting full country list")
  full_ctry <- edav_io(io = "read",
                       default_dir = "GID/GIDMEA/giddatt",
                       file_loc = "data_clean/ref_country.rds") |>
    dplyr::select(country_name)
  cli::cli_process_done()

  full_variables <- vpd_data |>
    dplyr::select(variable) |>
    dplyr::distinct()

  invalid_variable_name <- setdiff(variable_name, full_variables$variable)
  if (length(invalid_variable_name) != 0) {
    cli::cli_abort(paste0("Invalid variable name: ",
                          paste0(invalid_variable_name, collapse = ", ")))
  }

  years <- dplyr::tibble(year = min_year:max_year)

  complete <- dplyr::cross_join(full_vpd, full_ctry) |>
    dplyr::cross_join(full_variables) |>
    dplyr::cross_join(years)

  if (!is.null(vpd_name)) {
    complete <- complete |>
      dplyr::filter(vpd %in% vpd_name)
    vpd_data <- vpd_data |>
      dplyr::filter(vpd %in% vpd_name)
  }

  if (!is.null(variable_name)) {
    complete <- complete |>
      dplyr::filter(variable %in% variable_name)
    vpd_data <- vpd_data |>
      dplyr::filter(variable %in% variable_name)
  }

  no_values <- dplyr::anti_join(complete,
                                vpd_data |>
                                  select(country_name, year, vpd, variable),
                                by = c("vpd", "country_name", "variable", "year"))

  years_with_data <- vpd_data |>
    dplyr::group_by(vpd, country_name, variable) |>
    dplyr::summarize(years_with_data = paste0(unique(year), collapse = ", "))
  no_values_summary <- no_values |>
    dplyr::group_by(vpd, country_name, variable) |>
    dplyr::summarize(missing_years = paste0(unique(year), collapse = ", "))

  summary <- dplyr::full_join(years_with_data, no_values_summary,
                              by = c("vpd", "country_name", "variable")) |>
    dplyr::ungroup()

  return(summary)
}
