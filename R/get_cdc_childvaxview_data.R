#' Pull CDC NCIRD childvaxview immunization coverage data
#' @description
#' Pull coverage data from API and filter by desired geographic level
#' @param limit `int` Number of rows to download, defaults to max allowed (1000).
#' @param geo_level `str` Geographic categories of coverage data.
#' Choose from: 'national', 'regional', 'state', or 'substate'.
#'
#' @return `tibble` Dataframe of vaccine coverage estimates for all VPDs.
#' @export
#'
#' @examples
#' cdc_data <- get_cdc_childvaxview_data(geo_level="substate")
#'
get_cdc_childvaxview_data <- function(limit = 1000, geo_level=NULL) {

  # # Load necessary libraries
  # library(httr)
  # library(jsonlite)

  ## Get data from API (>1000 rows) ####
  base_url <- "https://data.cdc.gov/resource/fhky-rtsk.json" # NCIRD API endpoint

  # Initialize variables for pagination
  offset <- 0
  all_data <- list() # Create a list to store data

  repeat {
    url <- sprintf("%s?$limit=%d&$offset=%d", base_url, limit, offset)
    response <- httr::GET(url)

    # Check if request was successful (status code 200)
    if (httr::status_code(response) == 200) {
      data_chunk <- jsonlite::fromJSON(httr::content(response, "text")) # Parse JSON content

      if (length(data_chunk) == 0) {
        break # Exit loop if no more data is returned
      }

      all_data[[length(all_data) + 1]] <- data_chunk # Append new chunk of data

      offset <- offset + limit # Increment offset for next request

    } else {
      stop("Failed to retrieve data: ", httr::status_code(response), "\nResponse: ", httr::content(response, "text"))
      # Error if request failed; include response text for debugging
    }

    Sys.sleep(1) # Optional: Add a delay between requests to avoid hitting rate limits.
  }

  # Combine all chunks into a single dataframe
  final_data <- do.call(rbind, all_data)

  # Define geo_level filtering ####
  if (!is.null(geo_level)) {
    valid_geo_levels <- c("national", "regional", "state", "substate")

    if (!(geo_level %in% valid_geo_levels)) {
      stop("Invalid geo_level. Choose from: 'national', 'regional', 'state', or 'substate'.")
    }

    substate_areas <- c("IL-City of Chicago","IL-Rest of state",
                        "NY-City of New York","NY-Rest of state",
                        "PA-Philadelphia", "PA-Rest of state",
                        "TX-Bexar County", "TX-City of Houston", "TX-Dallas County", "TX-El Paso County",
                        "TX-Hidalgo County", "TX-Rest of state", "TX-Tarrant County","TX-Travis County")

    states_disagg <- c("Illinois", "New York","Pennsylvania","Texas")

    filtered_data <- final_data %>%
      filter(
        (geo_level == "national" & geography == "United States") |
          (geo_level == "regional" & geography %in% c(paste0("Region ", 1:10))) |
          (geo_level == "state" & !(geography %in% c("United States", paste0("Region ", 1:10), substate_areas))) |
          (geo_level == "substate" & !(geography %in% c("United States", paste0("Region ", 1:10), states_disagg)))
      )

    return(filtered_data)

  }

  return(final_data)
}
