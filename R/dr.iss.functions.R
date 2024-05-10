#' Read ISS/eSURV data
#'
#' @param iss_path path to the .csv file
#'
#' @return a tibble containing ISS/eSURV data
#' @export
load_iss_data <-function(iss_path) {
  return(read_csv(iss_path))
}

clean_iss_data <- function(iss.data, start_date, end_date) {
  cli::cli_process_start("Standardizing priority levels")
  issy2 <- iss.data %>%
    mutate(priority_level = case_when(
      get(priority_col)=="high" ~ "High",
      get(priority_col) == "highest" ~ "High",
      get(priority_col)=="medium" ~ "Medium",
      get(priority_col)=="low" ~ "Low",
      get(priority_col)=="n/a" ~ "Not Focal Site",
      get(priority_col)=="none" ~ "Not Focal Site",
      T ~ get(priority_col)
    )) %>%
    mutate(priority_level = factor(priority_level, levels = c(
      "High", "Medium", "Low", "Not Focal Site"
    )))
  cli::cli_process_done()

  cli::cli_process_start("Adding date columns")
  # add month
  issy2$monyear <- as.yearmon(as.Date(issy2$starttime))
  issy2$month <- month(as.Date(issy2$starttime))
  issy2$year <- year(as.Date(issy2$starttime))
  cli::cli_process_done()

  # Unreported AFP
  issy2$unrep_afp <- as.numeric(issy2$num_unreportedcases)
  # Province
  issy2$prov <- toupper(issy2$states_province)
  # District
  issy2$dists <- iconv(issy2$district,to="ASCII//TRANSLIT")
  issy2$dists <- toupper(issy2$dists)

  # Convert "n/a" characters to actual null values
  cli::cli_process_start("Converting n/a characters to actual null values")
  issy2 <- issy2 |>
    mutate(dists = if_else(dists == "N/A", NA, dists),
           prov = if_else(prov == "N/A", NA, prov)
    )
  cli::cli_process_done()

  # Remove accents
  cli::cli_process_start("Performing for names")
  issy2 <- issy2 %>%
    mutate(facility_name2 = iconv(issy2$hf,
                                  to="ASCII//TRANSLIT"))

  # Make all capital letters and remove extra whitespace
  issy2 <- issy2 %>%
    mutate(facility_name2 = toupper(facility_name2)) %>%
    mutate(facility_name2 = str_squish(facility_name2))
  cli::cli_process_done()

  # ISS graphs ------
  wrongsy = count(issy2, today_date>=end_date)

  wrongsy

  issy2.1 = issy2 %>%
    filter(today_date<=Sys.Date() &
             today_date>= start_date)
  issy3 = issy2.1 %>%
    #filter(is_priority_afp=="Yes") %>%
    group_by(month, year, priority_level) %>%
    summarize(freq = n()) %>%
    filter(year<=year(Sys.Date()) &
             year>= year(start_date))

  count(issy3, month, year) # One is 2033

  issy3$labs = month.abb[issy3$month] %>%
    factor(., levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                         "Aug", "Sep", "Oct", "Nov", "Dec"))
  issy3 <- issy3 |>
    filter(between(year, year(start_date), year(end_date)))

}
