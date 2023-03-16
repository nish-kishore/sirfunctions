#' Calculate Stool Adequacy Function
#'
#' @name f.stool.ad.01
#' @description creates an adequacy variable - 'missing' parameter defines how
#' missing data is treated - "Good" classifies missing data as good quality (POLIS method)
#' "Bad" classifies all missing as bad quality, "missing" excludes missing from
#' the calculations
#' @param ctry.data list: country specific data object
#' @param start.date chr:
#' @param end.date chr:
#' @param prov.dist chr: "prov" or "dist" or "ctry"
#' @param missing chr: "good" or "bad" or "missing"
#' @param bad.data chr: "remove" or "inadequate" or "adequate"
#' @returns tibble
#'

# RETURN WHAT PARAMETERS PEOPLE CHOSE AS ANOTHER VARIABLE

f.stool.ad.01 <- function(ctry.data, start.date, end.date, prov.dist, missing, bad.data) {
  start.date <- as_date(start.date)
  end.date <- as_date(end.date)
  
  #start.date <- as_date(date_first)
  #end.date <- as_date(date_last)
  
  afp.data <- ctry.data$afp.all.2 |>
    filter(date >= start.date & date <= end.date)
  pop.data <- dpop %>%
    filter(ADM0_NAME == country) %>%
    filter(year >= year(start.date) & year<=year(end.date))
  
  if (start.date < as_date(afp.data$date |> min(na.rm = T))) {
    print(paste0(
      "Your specified start date is ",
      start.date,
      ", however the first date of AFP data available is ",
      afp.data$date |> min(na.rm = T),
      ". Please use caution interpreting any values calculated before ",
      afp.data$date |> min(na.rm = T)
    ))
  }
  
  # Coding data to be adequate (1), inadequate (0), data missing (99), or data error (77)
  stool.data <- afp.data
  stool.data$adequacy.final <- NA
  
  stool.data <- afp.data |>
    as_tibble() |>
    filter(cdc.classification.all2 != "NOT-AFP") |> 
    mutate(adequacy.final = case_when(#Conditions for Bad Data
      bad.stool1 == "data entry error" | 
        bad.stool1 == "date before onset" | 
        bad.stool1 == "date onset missing" ~ 77
    )) %>%
    mutate(adequacy.final = case_when(#Conditions for Bad Data
      is.na(adequacy.final) & (bad.stool2 == "data entry error" | 
                                 bad.stool2 == "date before onset" | 
                                 bad.stool2 == "date onset missing") ~ 77,
      TRUE ~ adequacy.final
    )) %>%
    mutate(adequacy.final = case_when(#Conditions for Missing Adequacy
      is.na(adequacy.final) & (is.na(stool.1.condition) == T | 
                                 is.na(stool.2.condition) == T | 
                                 stool.1.condition == "Unknown" | stool.2.condition == "Unknown") ~ 99,
      TRUE ~ adequacy.final
    )) %>%
    mutate(adequacy.final = case_when(#Conditions for Poor Adequacy
      is.na(adequacy.final) & (ontostool1 > 13 | ontostool1 < 0 | 
                                 is.na(stool1tostool2) == T |
                                 ontostool2 > 14 | ontostool2 < 1 | stool1tostool2 < 1 |
                                 stool.1.condition == "Poor" | stool.2.condition == "Poor") ~ 0,
      TRUE ~ adequacy.final)) %>% 
    mutate(adequacy.final = case_when(#Conditions for Good Adequacy
      is.na(adequacy.final) & (ontostool1 <= 13 & ontostool1 >= 0 & 
                                 ontostool2 <= 14 & ontostool2 >= 1 & 
                                 stool1tostool2 >= 1 & stool.1.condition == "Good" & 
                                 stool.2.condition == "Good") ~ 1,
      TRUE ~ adequacy.final
    )) |>
    mutate(year = year(date)) |>
    select(epid, year, ctry,adm0guid, prov,adm1guid, dist, adm2guid, adequacy.final, cdc.classification.all2)
  
  # Preserving the original classifications
  stool.data1 <- stool.data
  
  
  if(bad.data == "remove"){
    stool.data = stool.data %>%
      filter(adequacy.final != 77)
  }
  if(bad.data == "inadequate"){
    stool.data$adequacy.final = replace(stool.data$adequacy.final, stool.data$adequacy.final==77,0)
  }
  if(bad.data == "adequate"){
    stool.data$adequacy.final = replace(stool.data$adequacy.final, stool.data$adequacy.final==77,1)
  }
  
  if(missing == "good"){
    stool.data$adequacy.final = replace(stool.data$adequacy.final, stool.data$adequacy.final==99,1)
  }
  if(missing == "bad"){
    stool.data$adequacy.final = replace(stool.data$adequacy.final, stool.data$adequacy.final==99,0)
  }
  if(missing == "missing"){
    stool.data = stool.data %>%
      filter(adequacy.final != 99)
  }
  
  
  if (prov.dist == "ctry") {
    
    baddy = stool.data1 %>%
      group_by(ctry, year, adequacy.final)%>%
      summarize(bad.data = n()) %>%
      ungroup()%>%
      filter(adequacy.final == 77) %>%
      select(-adequacy.final)
    
    
    missy = stool.data1 %>%
      group_by(ctry, year, adequacy.final)%>%
      summarize(missing.data = n()) %>%
      ungroup()%>%
      filter(adequacy.final == 99) %>%
      select(-adequacy.final)
    
    int.data <- stool.data |>
      group_by(year, ctry, adequacy.final) |>
      summarize(freq = n()) |>
      ungroup()
    
    tots <- stool.data |>
      group_by(ctry, year) |>
      summarize(toty = n())
    
    int.data <- full_join(int.data, tots)
    
    int.data.ad <- int.data |>
      filter(adequacy.final == 1) |>
      mutate(stoolad = 100 * freq / toty) %>%
      rename(num.ad = freq)
    
    int.data <- full_join(int.data.ad, missy,
                          by = c("year" = "year", "ctry" = "ctry")
    ) %>%
      full_join(baddy, by = c("year" = "year", "ctry" = "ctry"))
    
    int.data$bad.data = replace_na(int.data$bad.data, 0)
    int.data$ad.only = int.data$num.ad - int.data$missing.data
    
    
    int.data <- int.data %>%
      rename(
        "num.adequate" = "ad.only",
        #"tot.stool" = "toty2",
        "num.missing.data" = "missing.data",
        "num.data.error" = "bad.data",
        "num.adj.w.miss" = "num.ad",
        "tot.final" = "toty",
        "per.stool.ad" = "stoolad"
      ) %>%
      select(
        year, ctry, num.adequate, num.missing.data, num.data.error,
        num.adj.w.miss, tot.final, per.stool.ad
      ) 
  }
  
  
  if (prov.dist == "prov") {
    baddy = stool.data1 %>%
      group_by(ctry, year, adm1guid,adequacy.final)%>%
      summarize(bad.data = n()) %>%
      ungroup()%>%
      filter(adequacy.final == 77) %>%
      select(-adequacy.final)
    
    missy = stool.data1 %>%
      group_by(ctry, year, adm1guid, adequacy.final)%>%
      summarize(missing.data = n()) %>%
      ungroup()%>%
      filter(adequacy.final == 99) %>%
      select(-adequacy.final)
    
    
    int.data <- stool.data |>
      group_by(year, ctry, adm1guid, adequacy.final) |>
      summarize(freq = n()) |>
      ungroup()
    
    tots <- stool.data |>
      group_by(ctry, adm1guid, year) |>
      summarize(toty = n())
    
    int.data <- left_join(int.data, tots)
    
    int.data.ad <- int.data |>
      filter(adequacy.final == 1) |>
      mutate(stoolad = 100 * freq / toty) %>%
      rename(num.ad = freq)
    
    
    int.data <- full_join(int.data.ad, missy,
                          by = c("year" = "year", "ctry" = "ctry", "adm1guid" = "adm1guid")
    ) %>%
      full_join(baddy, by = c("year" = "year", "ctry" = "ctry", "adm1guid" = "adm1guid"))
    
    int.data$bad.data = replace_na(int.data$bad.data, 0)
    int.data$missing.data = replace_na(int.data$missing.data, 0)
    int.data$ad.only = int.data$num.ad - int.data$missing.data
    
    int.data <- int.data %>%
      rename(
        "num.adequate" = "ad.only",
        #"tot.stool" = "toty2",
        "num.missing.data" = "missing.data",
        "num.data.error" = "bad.data",
        "num.adj.w.miss" = "num.ad",
        "tot.final" = "toty",
        "per.stool.ad" = "stoolad"
      ) %>%
      full_join(unique(pop.data[,c("ADM0_NAME", "ADM1_NAME", "adm1guid", "year")]), 
                by = c("year" = "year",
                       "ctry" = "ADM0_NAME",
                       "adm1guid" = "adm1guid")) %>%
      rename("prov" = "ADM1_NAME") %>%
      select(
        year, ctry, prov, num.adequate, num.missing.data, num.data.error,
        num.adj.w.miss, tot.final, per.stool.ad
      )
  }
  
  
  if (prov.dist == "dist") {
    baddy = stool.data1 %>%
      group_by(ctry, year, adm1guid,adm2guid,adequacy.final)%>%
      summarize(bad.data = n()) %>%
      ungroup()%>%
      filter(adequacy.final == 77) %>%
      select(-adequacy.final)
    
    missy = stool.data1 %>%
      group_by(ctry, year, adm1guid,adm2guid, adequacy.final)%>%
      summarize(missing.data = n()) %>%
      ungroup()%>%
      filter(adequacy.final == 99) %>%
      select(-adequacy.final)
    
    
    int.data <- stool.data |>
      group_by(year, ctry, adm1guid,adm2guid, adequacy.final) |>
      summarize(freq = n()) |>
      ungroup()
    
    tots <- stool.data |>
      group_by(ctry, adm1guid,adm2guid, year) |>
      summarize(toty = n())
    
    int.data <- left_join(int.data, tots)
    
    int.data.ad <- int.data |>
      filter(adequacy.final == 1) |>
      mutate(stoolad = 100 * freq / toty) %>%
      rename(num.ad = freq)
    
    
    int.data <- full_join(int.data.ad, missy,
                          by = c(
                            "year" = "year", "ctry" = "ctry", "adm1guid" = "adm1guid",
                            "adm2guid" = "adm2guid"
                          )
    ) %>%
      full_join(baddy, by = c("year" = "year", "ctry" = "ctry", 
                              "adm1guid" = "adm1guid",
                              "adm2guid" = "adm2guid"
      ))
    int.data$bad.data = replace_na(int.data$bad.data, 0)
    int.data$missing.data = replace_na(int.data$missing.data, 0)
    int.data$ad.only = int.data$num.ad - int.data$missing.data
    
    int.data <- int.data %>%
      rename(
        "num.adequate" = "ad.only",
        #"tot.stool" = "toty2",
        "num.missing.data" = "missing.data",
        "num.data.error" = "bad.data",
        "num.adj.w.miss" = "num.ad",
        "tot.final" = "toty",
        "per.stool.ad" = "stoolad"
      ) %>%
      full_join(unique(pop.data[,c("ADM0_NAME", "ADM1_NAME", "adm1guid", 
                                   "ADM2_NAME", "adm2guid", "year")]), 
                by = c("year" = "year",
                       "ctry" = "ADM0_NAME",
                       "adm1guid" = "adm1guid",
                       "adm2guid" = "adm2guid")) %>%
      rename("prov" = "ADM1_NAME",
             "dist" = "ADM2_NAME") %>%
      select(
        year, ctry, prov, dist, num.adequate, num.missing.data, num.data.error,
        num.adj.w.miss, tot.final, per.stool.ad
      )
    
    # For discussion at retreat?
    
    int.data$num.adequate = replace_na(int.data$num.adequate, 0)
    int.data$num.missing.data = replace_na(int.data$num.missing.data, 0)
    int.data$num.data.error = replace_na(int.data$num.data.error, 0)
    int.data$num.adj.w.miss = replace_na(int.data$num.adj.w.miss, 0)
    int.data$tot.final = replace_na(int.data$tot.final, 0)
    int.data$per.stool.ad = replace_na(int.data$per.stool.ad, 0)
    
  }
  return(int.data)
}
