
#' Function to calculate timeliness of detection
#'
#' @name f.timely.detection.01
#' @description Function to calculate the overall timeliness of detection in AFP & ES POLIS data
#' @import dplyr
#' @import lubridate
#' @import tibble
#' @param afp.data tibble: AFP data which includes classification of AFP cases with onset date and date of notification to HQ,
#' @param es.data tibble: ES data which includes classification of samples with collection date and date of notification to HQ,
#' @param ctryseq.data tibble: countries with sequencing capacity within or outside of the country,
#' country (ADM0_NAME),
#' classification of AFP cases & ES samples
#' onset date of AFP cases and collection date of ES samples
#' date of notification to HQ (date.notification.to.hq),
#' @param start.date chr: "YYYY-MM-DD" - start date for evaluation
#' @param end.date chr: "YYYY-MM-DD" - end date for evaluation
#' @param rolling boolean: default FALSE
#' @returns list of two tibbles with global and sub-global AFP / ES detection timeliness evaluation
#' @export

f.timely.detection.01 <- function(
    afp.data,
    es.data,
    ctryseq.data,
    start.date,
    end.date,
    rolling = F
){

  # Analysis start and end date as defined by user (as a character)

  # Check if dates entered are valid
  tryCatch({
    start.date <- as_date(start.date)
    end.date <- as_date(end.date)
  },
  error = function(cond) {
    cond$message = "Please enter dates in 'YYYY-MM-DD' format."
    stop(e)
  })

  years <- year(start.date):year(end.date)

  # Limit AFP data to the range described by the analysis start and end dates
  afp.data <- afp.data |>
    filter(date >= start.date & date <= end.date)

  # Warning message about non-overlapping dates
  if(start.date < as_date(afp.data$date |> min(na.rm = T))){
    print(paste0(
      "Your specified start date is ",
      start.date,
      ", however the first date of AFP data available is ",
      afp.data$date |> min(na.rm = T),
      ". Please use caution interpreting any values calculated before ",
      afp.data$date |> min(na.rm = T)
    ))
  }


  # Limit ES data to the range described by the analysis start and end dates
  es.data <- es.data |>
    filter(collect.date >= start.date & collect.date <= end.date)

  # Warning message about non-overlapping dates
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

  # ES Indicators Analysis
  es.samp <- es.data %>%
    as_tibble() |>
    mutate(
      # Replaces all Missing as "BAD"
      date.notification.to.hq = dmy(date.notification.to.hq),
      collecttonothq = as.numeric(date.notification.to.hq - collect.date)
    ) %>%
    mutate(
      date.final.combined.result = dmy(date.final.combined.result),
      date.final.results.reported = dmy(date.final.results.reported),
      date.final.culture.result = dmy(date.final.culture.result),
      date.f6.ref.itd = dmy(date.f6.ref.itd),
      pv = ifelse(wpv==1 | vdpv==1, 1, 0),
      end.date =case_when(
        pv==1 ~coalesce(date.notification.to.hq, date.final.results.reported, date.final.combined.result),
        pv==0 ~coalesce(date.notification.to.hq, date.final.combined.result, date.final.culture.result, date.final.results.reported, date.f6.ref.itd)
        ),
      collect.to.enddate = as.numeric(end.date - collect.date)
      )%>%
    filter(!is.na(ADM0_NAME))

  #2.  ES detection
  es.pos.detect.01 <- es.samp %>%
    filter(pv==1) %>%
    select(epid = env.sample.id, ctry = ADM0_NAME, year = collect.yr, ontonothq = collect.to.enddate)

  #1. AFP detection
  afp.pos.detect.01 <- afp.data %>%
    ungroup() %>%
    filter(!cdc.classification.all.2.2 %in% c("COMPATIBLE" ,"NPAFP", "PENDING", "UNKNOWN", "VAPP", "LAB PENDING")) %>%
    select(epid, ctry, year, cdc.classification.all.2.2, date, datenotificationtohq) %>%
    mutate(
      datenotifytohq = dmy(datenotificationtohq),
      ontonothq = as.numeric(datenotifytohq - date)
    ) %>%
    select(epid, ctry, year, ontonothq)

  if(rolling){

    afpes.detect.01 <- rbind(afp.pos.detect.01, es.pos.detect.01) %>%
      pivot_longer(!c(ctry, epid, year), names_to = "interval", values_to = "value") %>%
      group_by(ctry, interval) %>%
      summarize(median_days = median(value, na.rm = T),
                afpes.pos.spec = n()) %>%
      ungroup()

    afpes.detect.02 <- left_join(afpes.detect.01, ctryseq.data,  by=c('ctry'='ctry')) %>%
      select(who.region, ctry, afpes.pos.spec, seq.capacity, median_days)

    afpes.detect.03 <- afpes.detect.02 %>%
      mutate(
        afpes.detection.35d =
          case_when(
            seq.capacity == 'yes' & median_days <= 35 ~ 1,
            seq.capacity == 'yes' & median_days > 35 ~ 0),
        afpes.detection.49d =
          case_when(
            seq.capacity == 'no' & median_days <= 49 ~ 1,
            seq.capacity == 'no' & median_days > 49 ~ 0)
      ) |>
      mutate(
        start.date = start.date,
        end.date = end.date
      )

    afpes.detect.04 <- afpes.detect.03 %>%
      summarise(
        num.ctry.inseq = sum(seq.capacity == 'yes'),
        num.ctry.outseq = sum(seq.capacity == 'no'),
        num.afpes.detect.35d = sum(afpes.detection.35d == 1, na.rm = T),
        num.afpes.detect.49d = sum(afpes.detection.49d == 1, na.rm = T)
      ) %>%
      mutate(
        pct.afpes.detect.35d = round(num.afpes.detect.35d/num.ctry.inseq *100, digits = 0),
        ctry.afpes.detect.35d = paste(paste(num.afpes.detect.35d, num.ctry.inseq, sep="/"), " ", "(", pct.afpes.detect.35d, "%", ")", sep=""),
        pct.afpes.detect.49d = round(num.afpes.detect.49d/num.ctry.outseq *100, digits = 0),
        ctry.afpes.detect.49d = paste(paste(num.afpes.detect.49d, num.ctry.outseq, sep="/"), " ", "(", pct.afpes.detect.49d, "%", ")", sep="")
      ) %>%
      select(-num.ctry.inseq, -num.ctry.outseq, -num.afpes.detect.35d, -num.afpes.detect.49d,
             -pct.afpes.detect.35d, -pct.afpes.detect.49d) |>
      mutate(
        start.date = start.date,
        end.date = end.date
      )

  }else{

    afpes.detect.01 <- rbind(afp.pos.detect.01, es.pos.detect.01) %>%
      pivot_longer(!c(ctry, epid, year), names_to = "interval", values_to = "value") %>%
      group_by(ctry, year, interval) %>%
      summarize(median_days = median(value, na.rm = T),
                afpes.pos.spec = n()) %>%
      ungroup()

    afpes.detect.02 <- left_join(afpes.detect.01, ctryseq.data,  by=c('ctry'='ctry')) %>%
      select(who.region, ctry, year, afpes.pos.spec, seq.capacity, median_days)

    afpes.detect.03 <- afpes.detect.02 %>%
      mutate(
        afpes.detection.35d =
          case_when(
            seq.capacity == 'yes' & median_days <= 35 ~ 1,
            seq.capacity == 'yes' & median_days > 35 ~ 0),
        afpes.detection.49d =
          case_when(
            seq.capacity == 'no' & median_days <= 49 ~ 1,
            seq.capacity == 'no' & median_days > 49 ~ 0)
      )

    afpes.detect.04 <- afpes.detect.03 %>%
      group_by(year) %>%
      summarise(
        num.ctry.inseq = sum(seq.capacity == 'yes'),
        num.ctry.outseq = sum(seq.capacity == 'no'),
        num.afpes.detect.35d = sum(afpes.detection.35d == 1, na.rm = T),
        num.afpes.detect.49d = sum(afpes.detection.49d == 1, na.rm = T)
      ) %>%
      mutate(
        pct.afpes.detect.35d = round(num.afpes.detect.35d/num.ctry.inseq *100, digits = 0),
        ctry.afpes.detect.35d = paste(paste(num.afpes.detect.35d, num.ctry.inseq, sep="/"), " ", "(", pct.afpes.detect.35d, "%", ")", sep=""),
        pct.afpes.detect.49d = round(num.afpes.detect.49d/num.ctry.outseq *100, digits = 0),
        ctry.afpes.detect.49d = paste(paste(num.afpes.detect.49d, num.ctry.outseq, sep="/"), " ", "(", pct.afpes.detect.49d, "%", ")", sep="")
      ) %>%
      select(-num.ctry.inseq, -num.ctry.outseq, -num.afpes.detect.35d, -num.afpes.detect.49d,
             -pct.afpes.detect.35d, -pct.afpes.detect.49d)

  }

  timly.detect <- list("ctry" = afpes.detect.03,
                       "global" = afpes.detect.04) # Store output in list
  return(timly.detect) # Return output
}
