#################################################################################
###            Defining breakthrough
###            Written by Stephanie Kovacs and Nishant Kishore
###            November 2021
###            Updates in March 2022
###            Updated April 2023 - Nick Heaghney
###            Updated 2024 - Nick Heaghney
###
#################################################################################

#Read in case data
#specify desired virustypes in case.list
#remove wild 1 in afg + pak
case.list <- c("cVDPV 1", "cVDPV 2", "cVDPV 3", "WILD 1")

list.cases <- lapply(case.list, FUN = pull_clean_case_data, start.date = as.Date("2000-01-01"),
                     end.date = Sys.Date(), min.yronset = paste0(year(Sys.Date())-4),
                     type = "reg")

cvdpv.wild.cases <- do.call(rbind.data.frame, list.cases) %>%
  #adding a hard fix to SIA data to fix Pakistan names to match virus
  #place admin1 names now GB and KP to match EOC names
  mutate(place.admin.1=ifelse(place.admin.1=="KHYBER PAKHTOON", "KP", place.admin.1),
         place.admin.1=ifelse(place.admin.1=="KPTD", "KP", place.admin.1),
         place.admin.1=ifelse(place.admin.1=="KPAKHTUNKHWA", "KP", place.admin.1),
         place.admin.1=ifelse(place.admin.1=="GBALTISTAN", "GB", place.admin.1),
         place.admin.1=ifelse(place.admin.1=="GILGIT BALTISTAN", "GB", place.admin.1)) %>%
  filter(!(place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN") & measurement == "WILD 1"))
