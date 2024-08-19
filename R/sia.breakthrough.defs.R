###            Defining breakthrough
###            Written by Stephanie Kovacs and Nishant Kishore
###            November 2021
###            Updates in March 2022
###            Updated April 2023 - Nick Heaghney
###            Updated 2024 - Nick Heaghney
###

raw.data <- get_all_polio_data()

#Read in case data
#specify desired virustypes in case.list
#remove wild 1 in afg + pak
case.list <- c("cVDPV 1", "cVDPV 2", "cVDPV 3", "WILD 1")

cvdpv.wild.cases <- clean_case_data(case.data = raw.data$pos,
                              .measurement = case.list,
                              start.date = as.Date("2000-01-01"),
                              end.date = Sys.Date(),
                              type = "reg") |>
  dplyr::mutate(place.admin.1=ifelse(place.admin.1=="KHYBER PAKHTOON", "KP", place.admin.1),
                place.admin.1=ifelse(place.admin.1=="KPTD", "KP", place.admin.1),
                place.admin.1=ifelse(place.admin.1=="KPAKHTUNKHWA", "KP", place.admin.1),
                place.admin.1=ifelse(place.admin.1=="GBALTISTAN", "GB", place.admin.1),
                place.admin.1=ifelse(place.admin.1=="GILGIT BALTISTAN", "GB", place.admin.1)) |>
  dplyr::filter(!(place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN") & measurement == "WILD 1"))


#Read in cleaned SIA data for last 4 years, filter out bopv campaigns in afg/pak
sia <- clean_sia_data(sia.data = raw.data$sia,
                      start.date = paste0(lubridate::year(Sys.Date())-4,"-01-01"),
                      method = "reg") |>
  dplyr::filter(!(place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN") & vaccine.type == "bOPV"))

#read in cleaned planned SIAs
sia.planned <- clean_sia_data(sia.data = raw.data$sia,
                                   start.date = paste0(lubridate::year(Sys.Date())-2,"-01-01"),
                                   method = "planned") |>
  dplyr::select(sia.sub.activity.code, place.admin.0, sub.activity.start.date,
                sub.activity.end.date, vaccine.type, yr.sia) |>
  dplyr::rename(Country = place.admin.0) |>
  dplyr::group_by(Country) |>
  dplyr::arrange(sub.activity.start.date) |>
  dplyr::slice(1L) |>
  unique()

#read in cleaned IPV SIAs
sia.ipv <- clean_sia_data(sia.data = raw.data$sia,
                               start.date = paste0(lubridate::year(Sys.Date())-4,"-01-01"),
                               method = "ipv")

