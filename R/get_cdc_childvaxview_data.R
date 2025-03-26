#' Pull CDC NCIRD childvaxview immunization coverage data
#' @description
#' Pull coverage data from API and filter by desired geographic level and vaccines.
#' @param limit `int` Number of rows to download, defaults to max allowed (1000).
#' @param geo_level `str` Geographic categories of coverage data.
#' Choose from: 'national', 'regional', 'state', or 'substate'.
#' @param vaccines `str` A string or vector of strings of vaccines for which to provide coverage data.
#' Choose from: 'DTaP', 'Polio', 'Hep B', 'PCV', 'Varicella', 'MMR', 'Hib', 'Hep A', 'Influenza', 'Rotavirus', 'Combined 7 series'.
#' @param base_url `str` URL to US CDC NCIRD API endpoint.
#' Defaults to "https://data.cdc.gov/resource/fhky-rtsk.json".
#'
#' @return `tibble` Dataframe of vaccine coverage estimates for all VPDs.
#' @export
#'
#' @examples
#' cdc_data <- get_cdc_childvaxview_data(geo_level="substate")
#' cdc_data <- get_cdc_childvaxview_data(geo_level="national", vaccines=c("Polio","MMR"))
#'
get_cdc_childvaxview_data <- function(geo_level=NULL, vaccines=NULL,
                                      limit = 1000,
                                      base_url="https://data.cdc.gov/resource/fhky-rtsk.json") {

  # Check parameter validity ####
  valid_geo_levels <- c("national", "regional", "state", "substate")
  if (!(is.null(geo_level)) && !(geo_level %in% valid_geo_levels) ) {
    stop("Invalid geo_level. Choose from: 'national', 'regional', 'state', or 'substate', or set to NULL to include all geographic categories.")
  }

  valid_vaccines <- c("DTaP", "Polio", "Hep B", "PCV", "Varicella","MMR", "Hib","Hep A", "Influenza","Rotavirus", "Combined 7 Series")
  if (!(is.null(vaccines)) && !all(vaccines %in% valid_vaccines)) {
    stop("At least one invalid vaccine name provided. Choose from: 'DTaP', 'Polio', 'Hep B', 'PCV', 'Varicella','MMR', 'Hib','Hep A', 'Influenza','Rotavirus', 'Combined 7 Series', or set to NULL to include all geographic categories.")
  }

  # Count records to get and define all urls ####

  # Total # of records to get
  count_response <- httr::GET(sprintf("%s?$select=count(*)", base_url))

  if (httr::status_code(count_response) != 200) {
    stop("Failed to retrieve total record count: ", httr::status_code(count_response), "\nResponse: ", httr::content(count_response, "text"))
  }

  total_count <- jsonlite::fromJSON(httr::content(count_response, "text"))[[1]] %>%
    as.numeric()

  # Generate all URLs
  num_requests <- ceiling(total_count / limit) # requests needed
  urls <- purrr::map(0:(num_requests - 1), function(i) {
    sprintf("%s?$limit=%d&$offset=%d", base_url, limit, i * limit)
  }) %>% unlist()

  # Get paginated data from url ####
  all_data <- dplyr::tibble() # Create empty df to store data

  for (url in urls){
    response <- httr::GET(url)

    # Check if request was successful (status code 200)
    if (httr::status_code(response) == 200) {
      data_chunk <- jsonlite::fromJSON(httr::content(response, "text")) # Parse JSON content

      if (length(data_chunk) == 0) {
        break() # Exit loop if no more data is returned
      }

      # Combine chunks into dataframe
      all_data <- all_data %>% dplyr::bind_rows(data_chunk)

    } else {
      stop("Failed to retrieve data: ", httr::status_code(response), "\nResponse: ", httr::content(response, "text"))
      # Error if request failed; include response text for debugging
    }

    Sys.sleep(1) # Add a delay between requests to avoid hitting rate limits.
  }

  # Define geo_level filtering ####
  if (!is.null(geo_level)) {

    substate_areas <- c("IL-City of Chicago","IL-Rest of state",
                        "NY-City of New York","NY-Rest of state",
                        "PA-Philadelphia", "PA-Rest of state",
                        "TX-Bexar County", "TX-City of Houston", "TX-Dallas County", "TX-El Paso County",
                        "TX-Hidalgo County", "TX-Rest of state", "TX-Tarrant County","TX-Travis County")

    states_disagg <- c("Illinois", "New York","Pennsylvania","Texas")

    all_data <- all_data %>%
      dplyr::filter(
          (geo_level == "national" & geography == "United States") |
          (geo_level == "regional" & geography %in% c(paste0("Region ", 1:10))) |
          (geo_level == "state" & !(geography %in% c("United States", paste0("Region ", 1:10), substate_areas))) |
          (geo_level == "substate" & !(geography %in% c("United States", paste0("Region ", 1:10), states_disagg)))
      )
  }

  # Clean vaccine data  ####

  # All cols imported as chr; change "NA" and "" to missing before further conversion
  all_data <- all_data %>% dplyr::mutate(
    dplyr::across(dplyr::everything(), ~ na_if(., "NA")),
    dplyr::across(dplyr::everything(), ~ na_if(.,""))
  )

  all_data <- all_data %>%
    dplyr::mutate(
      # Assign VPDs (matching vpd_ref data to each vaccine)
      vpd = dplyr::case_when(
        vaccine == "Hib" ~ "H. influenza type B disease",
        vaccine == "≥1 Dose MMR" ~ "Multiple", # Measles, Mumps, Rubella
        vaccine == "Influenza" ~ "Influenza",
        vaccine == "Rotavirus" ~ "Rotavirus",
        vaccine == "PCV" ~ "Pneumococcal disease",
        vaccine == "≥1 Dose Varicella" ~ "Varicella",
        vaccine == "Hep B" ~ "Hepatitis B",
        vaccine == "Hep A" ~ "Hepatitis A",
        vaccine == "Polio" ~ "Poliomyelitis",
        vaccine == "DTaP" ~ "Multiple", # Diphtheria, Tetanus, Pertussis
        vaccine == "Combined 7 Series" ~ "Multiple", # received combined 7-vaccine series by age 24 months
        TRUE ~ NA_character_
      ),
      # simplify where dose value included under 'vaccine' field
      dose = dplyr::case_when(
        vaccine == "≥1 Dose MMR" ~ "≥1 Dose",
        vaccine == "≥1 Dose Varicella" ~ "≥1 Dose",
        vaccine == "Combined 7 Series" ~ "Full series",
        TRUE ~ dose
      ),
      # Simplify values by separating dose from vaccine, where together
      vaccine = dplyr::case_when(
        vaccine == "≥1 Dose MMR" ~ "MMR",
        vaccine == "≥1 Dose Varicella" ~ "Varicella",
        TRUE ~ vaccine
      ),
      # convert to numeric values and extract upper and lower CI bounds
      coverage_estimate = as.numeric(coverage_estimate),
      population_sample_size = as.numeric(population_sample_size),
      lb_95ci = stringr::str_extract(`_95_ci`, "^[0-9]+\\.[0-9]+") %>% as.numeric(), # extract numbers before 'to'
      ub_95ci = stringr::str_extract(`_95_ci`, "(?<=to )([0-9]+\\.[0-9]+)") %>% as.numeric(), # extract numbers after 'to'

      # add data source
      source = paste0("US CDC ChildVaxView, as of ", lubridate::today())
      )

  ## If country level, add iso3 code to ensure appropriate merging with other GID datasets
  if (!(is.null(geo_level)) && geo_level=="national"){
    all_data <- all_data %>% dplyr::mutate(iso3_code = "USA")
    }

  # Define vaccines filtering ####
  if (!is.null(vaccines)) {
    all_data <- all_data %>% dplyr::filter(vaccine %in% vaccines)
  }

  # Return dataframe ####
  return(all_data)
}
