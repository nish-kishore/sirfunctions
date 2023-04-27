#' Calculate EV detection rate function
#'
#' @name f.ev.rate.01
#' @description Function to calcualte the EV detection rate in sites from POLIS
#' @import dplyr
#' @import lubridate
#' @import tibble
#' @param es.data tibble: ES data which includes site name (site.name),
#' country (ADM0_NAME),
#' date of collection (collect.date), and a binary ev detection variable (ev.detect)
#' that indicates absence/presence (0, 1) of enterovius in an ES sample
#' @param start.date chr: "YYYY-MM-DD"
#' @param end.date chr: "YYYY-MM-DD"
#' @returns tibble
#' @export
f.ev.rate.01 <- function(
  es.data,
  start.date,
  end.date
){

  # Analysis start and end date as defined by user (as a character)
  start.date <- as_date(start.date)
  end.date <- as_date(end.date)

  #Warning message about non-overlapping dates
  if(start.date < as_date(es.data$collect.date |> min(na.rm = T))){
    print(paste0(
      "Your specified start date is ",
      start.date,
      ", however the first date of ES data available is ",
      es.data$collect.date |> min(na.rm = T),
      ". Please use caution interpreting any values calculated before ",
      es.data$collect.date |> min(na.rm = T)
    ))
  }

  #check to make sure that data has necessary variables
  necessary.es.vars <- c("collect.date", "ADM0_NAME",  "site.name", "ev.detect")
  useful.es.vars <- c("ADM1_NAME", "ADM2_NAME","lat", "lng")

  if(sum(!necessary.es.vars %in% names(es.data)) > 0){
    missing.vars <- necessary.es.vars[!necessary.es.vars %in% names(es.data)]
    stop(paste0("The following necessary variables were not found in `es.data`: ",
                paste0(missing.vars, collapse = ", ")))
  }

  if(sum(!useful.es.vars %in% names(es.data)) > 0){
    missing.vars <- useful.es.vars[!useful.es.vars %in% names(es.data)]
    readline(paste0("The following useful variables were not found in `es.data`, please consider including them: ",
                paste0(missing.vars, collapse = ", "), "\n Press [ENTER] to continue"))
  }


  # Limit ES data to the range described by the analysis start and end dates
  es.data <- es.data |>
    filter(collect.date >= start.date & collect.date <= end.date)


  # Create site-level summary
  es.sum <- es.data |>
    group_by(ADM0_NAME, site.name) |>
    summarize(
      num.samples = n(),
      num.ev.pos = sum(ev.detect == 1, na.rm = T)
    ) |>
    mutate(
      ev.rate = num.ev.pos/num.samples,
      ev.percent = percent(ev.rate, accuracy=1),
      ev.rate.cat = case_when(
        ev.rate < .50 ~ "<50%",
        ev.rate >= .50 & ev.rate < .80 ~ "50 to <80%",
        ev.rate >= .80 ~ "80-100%")
    ) |>
    ungroup() |>
    as_tibble() |>
    mutate(
      start.date = start.date,
      end.date = end.date,
      interval = end.date - start.date
    )

  es.sum <- left_join(
    es.sum,
    es.data |>
      select(any_of(c("site.name", "ADM1_NAME", "ADM2_NAME","lat", "lng"))) |>
      distinct()
  )

  return(es.sum) # Data frame to return from function

}

