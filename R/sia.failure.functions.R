##              SIA Failure Functions                                        ###
##              Writen by Stephanie Kovacs and Nishant Kishore               ###
##              updated by Nick Heaghney spring 2023                         ###
##              updated by Nick Heaghney Summer 2024                         ###


#### Set up SIA Impact Report  ####

#initiate sia impact reprot function
#this function initializes a folder structure for the purpose of running the sia impact report
#' @export
#' @import tidypolis
#' @param folder_loc str: location of folder to set up and run SIA impact report
#' @param edav boolean: should the system use EDAV as it's cache; default is FALSE
init_sia_impact <- function(folder_loc,
                            edav = F){

  if(edav == T){
    Sys.setenv(POLIS_EDAV_FLAG = T)
  }else{
    Sys.setenv(POLIS_EDAV_FLAG = F)
  }

  if(tidypolis:::tidypolis_io(io = "exists.dir", file_path = folder_loc)){

    Sys.setenv(SIA_FOLDER = folder_loc)

  }else{
    tidypolis:::tidypolis_io(io = "create", file_path = folder_loc)

    Sys.setenv(SIA_FOLDER = folder_loc)

    tidypolis:::tidypolis_io(io = "create", file_path = paste0(folder_loc, "/assets"))
    tidypolis:::tidypolis_io(io = "create", file_path = paste0(folder_loc, "/assets/cache"))
    tidypolis:::tidypolis_io(io = "create", file_path = paste0(folder_loc, "/assets/donut_maps"))
    tidypolis:::tidypolis_io(io = "create", file_path = paste0(folder_loc, "/outputs"))
    tidypolis:::tidypolis_io(io = "create", file_path = paste0(folder_loc, "/outputs/emergences"))
  }

}


#' @description
#' Store parameters in cache
#' @import lubridate tidypolis
#' @param breakthrough_min_date int minimum days after SIA to be considered breakthrough
#' @param start_date date start date of analysis
#' @param end_date date end date of analysis, default is present day
#' @param recent_sia_start_year int year of earliest SIA in analysis, default is 2 years prior to present year
#' @param breakthrough_middle_date int number of days to set cutoff between early and late breakthrough
#' @param breakthrough_max_date int maximum number of days a case could be considered breakthrough
#' @param detection_pre_sia_date int used to restrict "Recent SIA with breakthrough transmission" figures to 'recent' SIAs in f.geompoint.case()
set_parameters <- function(breakthrough_min_date = NULL,
                           start_date = NULL,
                           end_date = Sys.Date(),
                           recent_sia_start_year = lubridate::year(Sys.Date())-2,
                           breakthrough_middle_date = NULL,
                           breakthrough_max_date = NULL,
                           detection_pre_sia_date = NULL){
  parameter_list <- list(
    breakthrough_min_date = breakthrough_min_date,
    start_date = start_date,
    end_date = end_date,
    recent_sia_start_year = recent_sia_start_year,
    breakthrough_middle_date = breakthrough_middle_date,
    breakthrough_max_date = breakthrough_max_date,
    detection_pre_sia_date = detection_pre_sia_date)
  #If NULL, stop and prompt for entry
  for(i in 1:length(parameter_list)){
    value <- parameter_list[i][[1]]
    if(is.null(value)){
      stop(paste0(names(parameter_list[i]), " cannot be left blank. Please re-run set_parameters() with a value specified."))
    }
  }
  #move old parameters to cache
  if(tidypolis:::tidypolis_io(io = "exists.file", file_path = paste0(Sys.getenv("SIA_FOLDER"), "/assets/cache/report_parameters.rds"))){
    old_parameters <- load_parameters()
    tidypolis:::tidypolis_io(obj = old_parameters, io = "write", file_path = paste0(Sys.getenv("SIA_FOLDER"), "/assets/cache/previous_report_parameters.rds"))
  }
  tidypolis:::tidypolis_io(obj = parameter_list, io = "write", file_path = paste0(Sys.getenv("SIA_FOLDER"), "/assets/cache/report_parameters.rds"))

}

#' @description
#' a function to load parameters
#' @import tidypolis
load_parameters <- function(){
  if(!tidypolis:::tidypolis_io(io = "exists.file", file_path = paste0(Sys.getenv("SIA_FOLDER"), "/assets/cache/report_parameters.rds"))){
    stop("Parameters not yet specified. Please specify parameters using set_parameters().")
  }
  parameters <- tidypolis:::tidypolis_io(io = "read", file_path = paste0(Sys.getenv("SIA_FOLDER"), "/assets/cache/report_parameters.rds"))

  return(parameters)
}


                                      #### Data Cleaning/Prep ####

#' @description function to pull clean, de-duplicated SIA campaign data for different purposes within
#' the SIA impact report
#' @param sia.data df sia data to be cleaned
#' @param start.date date, start date of SIA campaigns
#' @param end.date date, system date = today, last date of SIA campaigns, should be day of analysis
#' @param method chr, signifies type of SIA being pulled, reg includes n/mopv2, topv and bopv, planned
#' is limited to upcoming SIAs, IPV is for countries/regions that will only do IPV response
clean_sia_data <- function(sia.data,
                           start.date=as.Date("2016-01-01"),
                           end.date = Sys.Date(),
                           method = 'reg'){
  if(method == "reg"){
    print("----BEGINNING SIA DATA CLEANING----")
    print("[0/3]-Starting cleaning steps")
    # read in SIA files

    sia.clean.01 <- sia.data |>
      dplyr::filter(activity.start.date >= start.date & activity.end.date <= end.date)

    sia.02 <- sia.clean.01 |>
      #select variables of interest to ease error checking
      dplyr::select(sia.sub.activity.code, status, phase, im.loaded, lqas.loaded,
                    vaccine.type, sub.activity.start.date, adm2guid, linked.obx,
                    yr.sia, place.admin.0, place.admin.1, place.admin.2,
                    sub.activity.end.date, vaccine.type, adm0guid, adm1guid, `admin.coverage.%`)


    print("[1/3]-Removing campaigns that did not occur")

    sia.02 <- sia.02 |>
      #first step is to get rid of campaigns that did not occur
      #Unconfirmed was used pre-2014 for some older campaigns
      #missing status for campaigns 2004-2017 will be assigned as unconfirmed\
      dplyr::mutate(
        complete.camp = dplyr::case_when(
          status == "Done" |
            phase == "Completed" |
            im.loaded == "Yes" |
            lqas.loaded == "Yes" ~ "Completed",
          status == "Cancelled" |
            status == "Delayed" |
            (status == "Planned" & phase != "Completed") ~ "Not completed",
          status == "Unconfirmed" ~"Unconfirmed"),
        type.2.opv = ifelse(vaccine.type == "mOPV2" |
                              vaccine.type=="nOPV2" |
                              vaccine.type=="tOPV", 1, 0),
        type.1.opv = ifelse(vaccine.type == "bOPV" |
                              vaccine.type=="mOPV1" |
                              vaccine.type=="tOPV"|
                              vaccine.type=="IPV + bOPV", 1, 0),
        type.3.opv = ifelse(vaccine.type=="mOPV3" |
                              vaccine.type=="bOPV" |
                              vaccine.type=="tOPV" |
                              vaccine.type=="IPV + bOPV", 1, 0),
        ipv.vac = ifelse(vaccine.type == "f-IPV" |
                           vaccine.type == "IPV" |
                           vaccine.type == "IPV + bOPV", 1, 0)) |>
      dplyr::filter(complete.camp == "Completed" |
               complete.camp == "Unconfirmed")

    print("[2/3]-Removing duplicates")

    sia.02 <- sia.02 |>
      #Second step is to remove duplicates:
      dplyr::mutate(sub.activity.start.date = lubridate::as_date(sub.activity.start.date)) |>
      dplyr::arrange(sub.activity.start.date) |>
      dplyr::group_by(adm2guid, vaccine.type) |>
      #this creates variable that is days from last campaign of that vaccine
      dplyr::mutate(camp.diff.days = as.numeric(sub.activity.start.date - dplyr::lag(sub.activity.start.date))) |>
      dplyr::ungroup() |>

      #identify SIAs that are duplicated because there are no difference in campaign days
      dplyr::mutate(dup = dplyr::case_when(
        camp.diff.days == 0 & !is.na(adm2guid) ~ 1,
        camp.diff.days > 0 | is.na(adm2guid) == T | is.na(camp.diff.days) == T ~ 0)) |>
      #remove duplicates
      dplyr::filter(dup != 1) |>
      #manually removing extra duplicates
      #same date, vaccine and age range as another campaign in same dist
      dplyr::filter(!sia.sub.activity.code %in% c("PAK-2021-006-1", "SOM-2000-002-2"))

    print("[3/3]-Final cleaning steps")

    sia.reg <- sia.02 |>
      dplyr::filter(vaccine.type=="mOPV2" | vaccine.type=="nOPV2" |
                      (vaccine.type=="tOPV" & sub.activity.start.date>"2017-01-01") |
                      vaccine.type == "bOPV") |>
      dplyr::select(sia.sub.activity.code, place.admin.0, place.admin.1, place.admin.2,
             sub.activity.start.date, sub.activity.end.date, vaccine.type, adm0guid,
             adm1guid, adm2guid, yr.sia, `admin.coverage.%`)

    return(sia.reg)
  }

  if(method == "planned"){
    print("----BEGINNING SIA DATA CLEANING----")
    print("[0/3]-Starting cleaning steps")
    # read in SIA files
    sia.clean.01 <- sia.data |>
      dplyr::filter(activity.start.date >= Sys.Date())

    sia.02 <- sia.clean.01 |>
      #select variables of interest to ease error checking
      dplyr::select(sia.sub.activity.code, status, phase, im.loaded, lqas.loaded, vaccine.type,
             sub.activity.start.date, adm2guid, linked.obx, yr.sia, place.admin.0,
             place.admin.1, place.admin.2,sub.activity.end.date,
             vaccine.type, adm0guid, adm1guid, `admin.coverage.%`)

    print("[1/3]- subsetting to planned campaigns")

    sia.02 <- sia.02 |>
      #first step is to get rid of campaigns that did not occur
      #Unconfirmed was used pre-2014 for some older campaigns
      #missing status for campaigns 2004-2017 will be assigned as unconfirmed\
      dplyr::mutate(
        planned.camp = ifelse(phase %in% c("Forecasted", "Planned") &
                                status != "Canceled", "Planned", NA),
        complete.camp = dplyr::case_when(
          status == "Done" |
            phase == "Completed" |
            im.loaded == "Yes" |
            lqas.loaded == "Yes" ~ "Completed",
          status == "Cancelled" |
            status == "Delayed" |
            (status == "Planned" & phase != "Completed") ~ "Not completed",
          status == "Unconfirmed" ~"Unconfirmed"),
        type.2.opv = ifelse(vaccine.type == "mOPV2" |
                              vaccine.type=="nOPV2" |
                              vaccine.type=="tOPV", 1, 0),
        type.1.opv = ifelse(vaccine.type == "bOPV" |
                              vaccine.type=="mOPV1" |
                              vaccine.type=="tOPV"|
                              vaccine.type=="IPV + bOPV", 1, 0),
        type.3.opv = ifelse(vaccine.type=="mOPV3" |
                              vaccine.type=="bOPV" |
                              vaccine.type=="tOPV" |
                              vaccine.type=="IPV + bOPV", 1, 0),
        ipv.vac = ifelse(vaccine.type == "f-IPV" |
                           vaccine.type == "IPV" |
                           vaccine.type == "IPV + bOPV", 1, 0)) |>
      dplyr::filter(planned.camp == "Planned" &
               lubridate::year(Sys.Date()) <= yr.sia)

    print("[2/3]-Removing duplicates")

    sia.02 <- sia.02 |>

      #Second step is to remove duplicates:

      dplyr::mutate(sub.activity.start.date = lubridate::as_date(sub.activity.start.date)) |>
      dplyr::arrange(sub.activity.start.date) |>
      dplyr::group_by(adm2guid, vaccine.type) |>
      #this creates variable that is days from last campaign of that vaccine
      dplyr::mutate(camp.diff.days = as.numeric(sub.activity.start.date - dplyr::lag(sub.activity.start.date))) |>
      dplyr::ungroup() |>

      #identify SIAs that are duplicated because there are no difference in campaign days
      dplyr::mutate(dup = dplyr::case_when(
        camp.diff.days == 0 & !is.na(adm2guid) ~ 1,
        camp.diff.days > 0 | is.na(adm2guid) == T | is.na(camp.diff.days) == T ~ 0)) |>
      #remove duplicates
      dplyr::filter(dup != 1) |>
      #manually removing extra duplicates
      #same date, vaccine and age range as another campaign in same dist
      #this one no IM
      dplyr::filter(!sia.sub.activity.code %in% c("PAK-2021-006-1", "SOM-2000-002-2"))

    print("[3/3]-Final cleaning steps")


    sia.planned <- sia.02 |>
      # filter(vaccine.type=="mOPV2" | vaccine.type=="nOPV2" | (vaccine.type=="tOPV" & sub.activity.start.date>"2017-01-01") | vaccine.type == "bOPV") %>%
      dplyr::select(sia.sub.activity.code, place.admin.0, place.admin.1, place.admin.2,
                    sub.activity.start.date, sub.activity.end.date, vaccine.type,
                    adm0guid, adm1guid, adm2guid, yr.sia, status, phase, `admin.coverage.%`)

    return(sia.planned)
  }

  if(method == "ipv"){
    print("----BEGINNING SIA DATA CLEANING----")
    print("[0/3]-Starting cleaning steps")
    # read in SIA files
    sia.clean.01 <- sia.data |>
      dplyr::filter(activity.start.date >= start.date)

    sia.02 <- sia.clean.01 |>
      #select variables of interest to ease error checking
      dplyr::select(sia.sub.activity.code, status, phase, im.loaded, lqas.loaded,
                    vaccine.type, sub.activity.start.date, adm2guid, linked.obx,
                    yr.sia, place.admin.0, place.admin.1, place.admin.2,
                    sub.activity.end.date, vaccine.type, adm0guid, adm1guid, `admin.coverage.%`)

    print("[1/3]-Removing campaigns that did not occur")

    sia.02 <- sia.02 |>
      #first step is to get rid of campaigns that did not occur
      #Unconfirmed was used pre-2014 for some older campaigns
      #missing status for campaigns 2004-2017 will be assigned as unconfirmed\
      dplyr::mutate(
        complete.camp = dplyr::case_when(
          status == "Done" |
            phase == "Completed" |
            im.loaded == "Yes" |
            lqas.loaded == "Yes" ~ "Completed",
          status == "Cancelled" |
            status == "Delayed" |
            (status == "Planned" & phase != "Completed") ~ "Not completed",
          status == "Unconfirmed" ~"Unconfirmed"),
        type.2.opv = ifelse(vaccine.type == "mOPV2" |
                              vaccine.type=="nOPV2" |
                              vaccine.type=="tOPV", 1, 0),
        type.1.opv = ifelse(vaccine.type == "bOPV" |
                              vaccine.type=="mOPV1" |
                              vaccine.type=="tOPV"|
                              vaccine.type=="IPV + bOPV", 1, 0),
        type.3.opv = ifelse(vaccine.type=="mOPV3" |
                              vaccine.type=="bOPV" |
                              vaccine.type=="tOPV" |
                              vaccine.type=="IPV + bOPV", 1, 0),
        ipv.vac = ifelse(vaccine.type == "f-IPV" |
                           vaccine.type == "IPV" |
                           vaccine.type == "IPV + bOPV", 1, 0)) |>
      dplyr::filter(complete.camp == "Completed" |
               complete.camp == "Unconfirmed")

    print("[2/3]-Removing duplicates")

    sia.02 <- sia.02 |>

      #Second step is to remove duplicates:

      dplyr::mutate(sub.activity.start.date = lubridate::as_date(sub.activity.start.date)) |>
      dplyr::arrange(sub.activity.start.date) |>
      dplyr::group_by(adm2guid, vaccine.type) |>
      #this creates variable that is days from last campaign of that vaccine
      dplyr::mutate(camp.diff.days = as.numeric(sub.activity.start.date - dplyr::lag(sub.activity.start.date))) |>
      dplyr::ungroup() |>

      #identify SIAs that are duplicated because there are no difference in campaign days
      dplyr::mutate(dup = dplyr::case_when(
        camp.diff.days == 0 & !is.na(adm2guid) ~ 1,
        camp.diff.days > 0 | is.na(adm2guid) == T | is.na(camp.diff.days) == T ~ 0)) |>
      #remove duplicates
      dplyr::filter(dup != 1) |>
      #manually removing extra duplicates
      #same date, vaccine and age range as another campaign in same dist
      #this one no IM
      dplyr::filter(!sia.sub.activity.code %in% c("PAK-2021-006-1", "SOM-2000-002-2"))

    print("[3/3]-Final cleaning steps")

    sia.ipv <- sia.02 |>
      dplyr::filter(vaccine.type=="IPV" | vaccine.type=="f-IPV" | vaccine.type=="IPV + bOPV") |>
      dplyr::select(sia.sub.activity.code, place.admin.0, place.admin.1, place.admin.2,
                    sub.activity.start.date, sub.activity.end.date, vaccine.type,
                    adm0guid, adm1guid, adm2guid, yr.sia, `admin.coverage.%`)

    return(sia.ipv)
  }
}

#' @description
#' a funciton to return only relevant polio cases for the sia impact report
#' @param case.data df dataframe of positive cases
#' @param start.date date start date for sia
#' @param end.date date end date for cases, default is today
#' @param .measurement str positve case types to include
#' @param min.yronset date earliest year of onset for cases to be included
#' @param type str options are "reg" or "donut"
clean_case_data <- function(case.data,
                            start.date = as.Date("2016-01-01"),
                            end.date = Sys.Date(),
                            .measurement = NULL,
                            min.yronset = NULL,
                            type = "reg"){
  print("----CLEANING CASE DATA----")

  if(type == "reg"){

    out <- case.data |>
      dplyr::filter(dateonset >= start.date & dateonset <= end.date,
                    measurement %in% .measurement) |>
      dplyr::mutate(place.admin.0 = ifelse(place.admin.0 == "CÔTE D’IVOIRE", "COTE D IVOIRE", place.admin.0)) %>%
      {
        if(is.null(.measurement)){.}else{
          filter(., measurement %in% .measurement)
        }
      } %>%
      {
        if(is.null(min.yronset)){.}else{
          filter(., yronset >= min.yronset)
        }
      } |>
      dplyr::select(epid, place.admin.0, place.admin.1, place.admin.2, adm0guid,
                    adm1guid, adm2guid = admin2guid, measurement, yronset, dateonset,
                    datasource, latitude, longitude, ntchanges, emergencegroup,
                    source, whoregion) |>
      unique()
  }

  if(type == "donut"){
    out <- case.data |>
      #subset to places of interest
      #filter(whoregion == "AFRO") %>%
      #only look at data from 2016 onwards
      dplyr::filter(yronset >= paste0(lubridate::year(Sys.Date())-4,"-01-01")) |>
      #look at cVDPV 1, cVDPV 2, cVDPV 3, WILD 1 data
      dplyr::filter(measurement %in% c("cVDPV 1", "cVDPV 2", "cVDPV 3", "WILD 1")) |>
      #keep variables of interest
      dplyr::select(epid, whoregion, dateonset, lat = latitude, lon = longitude,
                    datasource, measurement, ntchanges, emergencegroup, viruscluster,
                    admin2guid, source) |>
      #deduplicate
      unique()
  }

  print(paste0(nrow(out), " case records loaded!"))

  return(out)
}



                       #### Analysis functions ####

#Cluster Function
#this function identifies "cluster" or OBX response so we can identify rounds
#' @export
#' @import dplyr stats cluster
#' @param x df: data to be clustered
#' @param seed num
#' @param method str cluster method to use, can be "kmeans" or "mindate"
#' @param grouping_days int:
cluster_dates <- function(x,
                          seed = 1234,
                          method = "kmeans",
                          grouping_days = 365){

  if(method == "kmeans"){
    #prepare the data
    y <- x |>
      #select only dates
      dplyr::select(date = sub.activity.start.date) |>
      #calculate distance from minimum date
      dplyr::mutate(date = as.numeric(date - min(date))) |>
      #normalize values for clustering
      scale()

    set.seed(seed)
    #calculate the optimal number of clusters
    optim_k <- y %>%
      #calculate optimal number of clusters using the
      #gap statistic
      {cluster::clusGap(., FUN = stats::kmeans, nstart = 25, K.max = max(min(nrow(.)-1, nrow(.)/2), 2), B = 100)} %>%
      #extract gap statistic matrix
      {.$Tab[,"gap"]} %>%
      #calculate the max gap statistic, given the sparsity in the data
      #am not limiting to the first max SE method
      which.max()

    set.seed(seed)
    #calculate the clusters
    x$cluster <- stats::kmeans(y, optim_k)$cluster %>%
      #clusters don't always come out in the order we want them to
      #so here we convert them into factors, relevel and then extract
      #the numeric value to ensure that the cluster numbers are in order
      {factor(., levels = unique(.))} %>%
      as.numeric()

    #outputting the method used
    x$cluster_method <- method

    return(x)
  }else{
    if(method == "mindate"){
      x <- x |>
        dplyr::mutate(cluster = as.numeric(sub.activity.start.date - min(sub.activity.start.date)),
                      cluster = cut(cluster, seq(0, grouping_days*6, by = grouping_days), include.lowest = T),
                      cluster = as.numeric(cluster),
                      cluster_method = method)

      return(x)
    }
  }



}

#' @description
#' manager function to run the cluster_dates() function using helper function run_cluster_dates to cluster SIAs by type
#' @export
#' @import dplyr
#' @param df dataframe of SIAs to identify rounds by vaccine type
cluster_dates_for_sias <- function(sia.type2){


  tick <- Sys.time()
  #original vax types
  out_mopv2 <- sia.type2 |>
    run_cluster_dates(min_obs = 4, type = "mOPV2")

  out_nopv2 <- sia.type2 |>
    run_cluster_dates(min_obs = 4, type = "nOPV2")

  out_topv <- sia.type2 |>
    run_cluster_dates(min_obs = 4, type = "tOPV")

  #add bopv
  out_bopv <- sia.type2 |>
    run_cluster_dates(min_obs = 4, type = "bOPV")

  cluster <- dplyr::bind_rows(out_mopv2, out_nopv2, out_topv, out_bopv) |>
    dplyr::select(sia.sub.activity.code, adm2guid, cluster)

  #merge back with SIA data

  case.sia <- dplyr::left_join(sia.type2, cluster, by = c("sia.sub.activity.code"="sia.sub.activity.code", "adm2guid"="adm2guid")) |>
    dplyr::arrange(adm2guid, sub.activity.start.date) |>
    dplyr::group_by(adm2guid, vaccine.type, cluster) |>
    dplyr::mutate(round.num = row_number()) |>
    dplyr::ungroup() |>
    dplyr::group_by(adm2guid) |>
    dplyr::mutate(max.round = max(sub.activity.start.date)) |>
    dplyr::ungroup() |>
    dplyr::mutate(last.camp = ifelse(max.round == sub.activity.start.date, 1, 0))

  tock <- Sys.time()

  print(tock - tick)

  return(case.sia)

}

#' Wrapper around the cluster_dates function to do some error checking
#'
#' @export
#' @import dplyr readr
#' @param data df dataframe on which to run cluster dates function
#' @param min_obs int
#' @param type str vaccine type
run_cluster_dates <- function(data,
                              sia_folder = Sys.getenv("SIA_FOLDER"),
                              min_obs = 4,
                              type){

  #check which locations meet minimum obs requirements
  in_data <- data |>
    dplyr::filter(vaccine.type == type) |>
    dplyr::group_by(adm2guid) |>
    dplyr::summarize(count = n())

  #check if cache exists
  cache_exists <- tidypolis:::tidypolis_io(io = "exists.file", file_path = paste0(sia_folder, "/assets/cache/", type,"_cluster_cache.rds"))

  if(cache_exists){
    cache <- tidypolis:::tidypolis_io(io = "read", file_path = paste0(sia_folder, "/assets/cache/", type,"_cluster_cache.rds"))
    in_data <- setdiff(in_data, cache)

    print(paste0(nrow(in_data), " potentially new SIAs in [",type,"] found for clustering analysis"))

    #drop cache rows where the adm2guid is in in_data with a different count
    cache <- cache |>
      dplyr::filter(!(adm2guid %in% in_data$adm2guid))

    tidypolis:::tidypolis_io(obj = dplyr::bind_rows(in_data, cache), io = "write", file_path = paste0(sia_folder, "/assets/cache/", type,"_cluster_cache.rds"))
  }else{
    print(paste0("No cache found for [", type, "], creating cache and running clustering for ", nrow(in_data), " SIAs"))
    tidypolis:::tidypolis_io(obj = in_data, io = "write", file_path = paste0(sia_folder, "/assets/cache/", type,"_cluster_cache.rds"))
  }

  if(nrow(in_data) > 0){
    print("Clustering new SIA data")
    in_data <- in_data |>
      dplyr::filter(count >= min_obs)

    included <- data |>
      dplyr::filter(vaccine.type == type) |>
      dplyr::filter(adm2guid %in% in_data$adm2guid)

    #observations which didn't meet the minimum requirement
    dropped <- setdiff(dplyr::filter(data, vaccine.type == type), included)

    #for data with at least a minimum number of observations
    out <- dplyr::ungroup(included) |>
      dplyr::group_by(adm2guid) |>
      dplyr::group_split() |>
      #apply function to each subset
      lapply(cluster_dates) |>
      #bind output back together
      dplyr::bind_rows()

    #error checking for situations where no data < min_obs
    if(nrow(dropped) > 0){
      #for data with low obs
      out2 <- dplyr::ungroup(dropped) |>
        dplyr::group_by(adm2guid) |>
        dplyr::group_split() |>
        lapply(function(x) cluster_dates(x, method = "mindate")) |>
        dplyr::bind_rows()
    }


    #error catching the return
    if(nrow(dropped) > 0){

      out <- bind_rows(out, out2)
    }

    #data cache
    data_cache_exists <- tidypolis:::tidypolis_io(io = "exists.file", file_path = paste0(sia_folder, "/assets/cache/", type, "data_cluster_cache.rds"))

    if(data_cache_exists){
      data_cache <- tidypolis:::tidypolis_io(io = "read", file_path = paste0(sia_folder, "/assets/cache/", type, "data_cluster_cache.rds"))

      out <- filter(data_cache, !sia.sub.activity.code %in% unique(out$sia.sub.activity.code)) |>
        dplyr::bind_rows(out)
      # data_cache2 <- data_cache %>%
      #   anti_join(out, by=c("sia.sub.activity.code", "adm2guid"))
      # out <- data_cache2 %>%
      #   bind_rows(out)

      tidypolis:::tidypolis_io(obj = out, io = "write", file_path = paste0(sia_folder, "/assets/cache/", type, "data_cluster_cache.rds"))

    }else{
      print(paste0("No data cache found for [", type, "], creating data cache and saving clustering results for ", nrow(out), " SIAs"))
      tidypolis:::tidypolis_io(obj = out, io = "write", file_path = paste0(sia_folder, "/assets/cache/", type, "data_cluster_cache.rds"))
  }




  }else{
    print(paste0("No new SIA data found for [", type, "], loading cached data!"))
    out <- tidypolis:::tidypolis_io(io = "read", file_path = paste0(sia_folder, "/assets/cache/", type, "data_cluster_cache.rds"))
  }

  return(out)
}

#' @description
#' function to identify afp or env detections with no follow up SIA
#' @import dplyr
#' @param x tibble dataframe of positive cases to be passed into the function
#' @param case.sia tibble dataframe created from cases joined to SIAs and passed into clustering algorithm
#' @param breakthrough_max_date int the maximum days post-SIA to be considered breakthrough
detection_no_sia.v2 <- function(x,
                                case.sia,
                                breakthrough_max_date = load_parameters()$breakthrough_max_date){

  sias <- dplyr::filter(case.sia, adm1guid == x$adm1guid)

  max_diff <- NA

  if(nrow(sias) > 0){
    diffs <- sias |>
      dplyr::mutate(timediff = sub.activity.start.date - unique(x$dateonset)) |>
      dplyr::pull(timediff)

    max_diff <- max(diffs)

    flag <- (max_diff < -breakthrough_max_date)
  }else{flag <- T}

  return(
    tibble(
      "epid" = unique(x$epid),
      "virus" = x$measurement,
      "emergencegroup" = x$emergencegroup,
      "ctry" = unique(x$place.admin.0),
      "prov" = unique(x$place.admin.1),
      "flag" = flag,
      "max_diff" = max_diff,
      "last_sia" = min(x$dateonset) + max_diff,
      "date_first_detect" = min(x$dateonset),
      "date_last_detect" = max(x$dateonset),
      "days_since_last_detect" = Sys.Date() - min(x$dateonset)
    )
  )

}

#' @description
#' a funciton to create a df counting detections and defining breakthrough
#' in relation to SIAs at the district level
#' @import dplyr
#' @param case.sia.01 tibble a df of detections and SIAs at district level
#' @param breakthrough_min_date int minimum days after SIA to be considered breakthrough
#' @param breakthrough_middle_date int number of days to set cutoff between early and late breakthrough
#' @param breakthrough_max_date int maximum number of days a case could be considered breakthrough
create_case_sia_02 <- function(case.sia.01,
                               breakthrough_min_date=load_parameters()$breakthrough_min_date,
                               breakthrough_middle_date = load_parameters()$breakthrough_middle_date,
                               breakthrough_max_date = load_parameters()$breakthrough_max_date){

  print("----CREATING CASE.SIA.02----")

  case.sia.02 <- dplyr::full_join(case.sia.01, calc_sia_emerge(case.sia.01),
                           by = c("sia.sub.activity.code" = "sia.sub.activity.code")) |>

    dplyr::mutate(new.emerge = ifelse(emergencegroup == emerge1 | emergencegroup == emerge2 |
                                      emergencegroup == emerge3 | emergencegroup == emerge4 |
                                      emergencegroup == emerge5 | emergencegroup == emerge6 |
                                      emergencegroup == emerge7 | emergencegroup == emerge8, 0, 1)) |>

    dplyr::mutate(break.through1 = dplyr::case_when(timetocase < breakthrough_min_date ~"Early Case",
                                                    timetocase >= breakthrough_min_date & timetocase <= breakthrough_middle_date ~ "Break through",
                                                    timetocase > breakthrough_middle_date & timetocase <= breakthrough_max_date ~"Late Break through",
                                                    timetocase > breakthrough_max_date ~"Very late break through",
                                                    is.na(dateonset) == T ~"Never case"),
                  #new emergence in response zone likely not due to current OPV use
                  new.emergence.01 = ifelse(timetocase >= -30 & timetocase < breakthrough_max_date & new.emerge == 1, 1,0),
                  new.emergence.01 = ifelse(is.na(new.emergence.01) == T, 0, new.emergence.01),
                  #new emergence in response zone likely due to OPV2 use
                  new.emergence.02 = ifelse(timetocase >= breakthrough_max_date & new.emerge == 1, 1, 0),
                  new.emergence.02 = ifelse(is.na(new.emergence.02) == T, 0, new.emergence.02)) |>

    dplyr::group_by(adm2guid, sia.sub.activity.code) |>
    dplyr::mutate(num.case28.180days = sum(break.through1 == "Break through"),
                  num.case180.365 = sum(break.through1 == "Late Break through"),
                  num.case.after365 = sum(break.through1 == "Very late break through"),
                  num.new.emerge.01 = sum(new.emergence.01),
                  num.new.emerge.02 = sum(new.emergence.02)) |>

    dplyr::select(sia.sub.activity.code, place.admin.0, place.admin.1, place.admin.2,
                  sub.activity.start.date, sub.activity.end.date, vaccine.type, adm0guid,
                  adm1guid, adm2guid, yr.sia, sia.type,
                  # num.dist.incamp, round.num.01, vac.round.num.count, vac.total.rounds, opv2.round.count,
                  # total.opv2.rounds, linked.obx, obx1, obx2, obx3, obx4,
                  `admin.coverage.%`, emerge1, emerge2, emerge3, emerge4, emerge5,
                  emerge6, emerge7, emerge8, num.case28.180days, num.case180.365,
                  num.case.after365, num.new.emerge.01, num.new.emerge.02, cluster, round.num) |>

    #add cluster and round number
    dplyr::ungroup() |>
    dplyr::distinct() |>

    #any early break through of transmission
    dplyr::mutate(breakthrough.01 = ifelse(num.case28.180days > 0, 1, 0),
                  #transmission 180-365 days
                  breakthrough.02 = ifelse(num.case180.365 > 0, 1, 0)) |>
    dplyr::full_join(calc_first_break_case(case.sia.01, breakthrough_min_date),
                     by = c("adm2guid", "sia.sub.activity.code"))

  print("----CASE.SIA.02 CREATED----")
  return(case.sia.02)
}

#' @description
#' a function to identify all emergences within SIA area within 365 to 0 days BEFORE SIA round to note emergences for rounds
#' @import dplyr tidyr
#' @param case.sia.01 tibble a df of detections and SIAs at district level
#' @param breakthrough_middle_date int number of days to set cutoff between early and late breakthrough
calc_sia_emerge <- function(case.sia.01,
                            breakthrough_middle_date = load_parameters()$breakthrough_middle_date){

  #note for now we will focus only on breakthrough regardless of emergence but this can be used later

  sia.emerge <- case.sia.01 |>
    dplyr::ungroup() |>
    dplyr::filter(timetocase> -breakthrough_middle_date & timetocase< breakthrough_middle_date) |>
    dplyr::select(sia.sub.activity.code, timetocase, emergencegroup) |>
    dplyr::arrange(sia.sub.activity.code, timetocase) |>
    dplyr::select(-timetocase) |>
    dplyr::distinct() |>
    dplyr::group_by(sia.sub.activity.code) |>
    dplyr::mutate(num.emerge = dplyr::row_number()) |>
    dplyr::mutate(num.emerge=paste("emerge", num.emerge, sep="")) |>
    tidyr::pivot_wider(names_from=num.emerge, values_from=emergencegroup)

  return(sia.emerge)
}

#' @description
#' a function to identify first breakthrough case by district/SIA round
#' @import dplyr
#' @param case.sia.01 tibble a df of detections and SIAs at district level
#' @param breakthrough_min_date int minimum days after SIA to be considered breakthrough
calc_first_break_case <- function(case.sia.01,
                                  breakthrough_min_date=load_parameters()$breakthrough_min_date){
  first.break.case <- case.sia.01 |>
    dplyr::mutate(break.case = ifelse(timetocase >= breakthrough_min_date, 1, 0)) |>
    dplyr::filter(break.case == 1) |>
    dplyr::group_by(adm2guid, sia.sub.activity.code) |>
    dplyr::mutate(first.break.case = min(dateonset)) |>
    dplyr::filter(first.break.case == dateonset) |>
    dplyr::select(adm2guid, sia.sub.activity.code, first.break.case, timetocase) |>
    dplyr::distinct() |>
    dplyr::rename(timetofirstcase=timetocase)

  return(first.break.case)
}

#' @description
#' a function to identify SIAs with breakthrough detections for which there is no followup
#' rounds
#' @import dplyr tidyr
#' @param case.sia.02 tibble df from create_case_sia_02
#' @param case.sia tibble df of all SIAs with round numbers included from clustering
#' @param cases tibble df of all detections
#' @param breakthrough_min_date int minimum days after SIA to be considered breakthrough
#' @param breakthrough_middle_date int number of days to set cutoff between early and late breakthrough
#' @param breakthrough_max_date int maximum number of days a case could be considered breakthrough
create_recent_sia_fail <- function(case.sia.02,
                                   case.sia,
                                   cases,
                                   breakthrough_min_date=load_parameters()$breakthrough_min_date,
                                   breakthrough_middle_date = load_parameters()$breakthrough_middle_date,
                                   breakthrough_max_date = load_parameters()$breakthrough_max_date){

  recent.sia.failure <- case.sia.02 |>
    dplyr::filter(adm2guid %in% create_case_sia_figs(case.sia.02,
                                                     case.sia,
                                                     cases,
                                                     method = "latest.cases",
                                                     breakthrough_min_date = breakthrough_min_date,
                                                     breakthrough_middle_date = breakthrough_middle_date,
                                                     breakthrough_max_date = breakthrough_max_date)$adm2guid) |>
    dplyr::group_by(adm2guid) |>
    dplyr::mutate(max.sia = max(sub.activity.start.date)) |>
    dplyr::filter(max.sia == sub.activity.start.date) |>
    dplyr::ungroup() |>
    dplyr::select(sia.sub.activity.code, place.admin.0, sub.activity.start.date,
                  vaccine.type, timetofirstcase, num.case28.180days, num.case180.365) |>
    dplyr::group_by(sia.sub.activity.code) |>
    dplyr::mutate(min.timetocase = min(timetofirstcase, na.rm = T),
                  tot.cases28.180days = sum(num.case28.180days),
                  tot.cases180.365days = sum(num.case180.365)) |>
    dplyr::ungroup() |>
    dplyr::select(sia.sub.activity.code, place.admin.0, sub.activity.start.date,
                  vaccine.type, min.timetocase, tot.cases28.180days, tot.cases180.365days) |>
    dplyr::filter(tot.cases28.180days>0 | tot.cases180.365days>0) |>
    dplyr::distinct()

  #adding in round using sia.failure
  sia.round <- case.sia.02 |>
    dplyr::group_by(sia.sub.activity.code) |>
    dplyr::mutate(round.num.sia = Mode(round.num),
                  num.breakthrough.01 = sum(breakthrough.01),
                  num.breakthrough.02 = sum(breakthrough.02)) |>
    dplyr::select(sia.sub.activity.code, yr.sia, place.admin.0, vaccine.type,
                  round.num.sia, emerge1, emerge2, emerge3, emerge4, emerge5, emerge6,
                  emerge7, emerge8) |>
    dplyr::distinct() |>
    dplyr::select(sia.sub.activity.code, round.num.sia, emerge1, emerge2, emerge3,
                  emerge3, emerge4, emerge5, emerge6, emerge7, emerge8)


  recent.sia.failure.01 <- dplyr::left_join(recent.sia.failure, sia.round, by=c("sia.sub.activity.code")) |>
    tidyr::unite("Emergences", emerge1:emerge8, na.rm=T, sep=", ") |>
    dplyr::rename(SIA = sia.sub.activity.code,
                  Country = place.admin.0,
                  "Start Date" = sub.activity.start.date,
                  Vaccine = vaccine.type,
                  "Time to First Breakthrough" = min.timetocase,
                  !!(paste0("Detections ",breakthrough_min_date,"-",breakthrough_middle_date," days")) := tot.cases28.180days,
                  !!(paste0("Detections ",breakthrough_middle_date,"-",breakthrough_max_date," days")) := tot.cases180.365days,
                  Round = round.num.sia) |>
    dplyr::select(Country, SIA, Round, dplyr::everything())

  return(recent.sia.failure.01)

}


#' Mode function
#' @description
#' this function is used to identify the most common district level round response to assign
#' a campaign specific round
#' @export
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#' @description
#' a function to create figures of SIA rounds and detections
#' @import dplyr lubridate
#' @param case.sia.02 tibble df from create_case_sia_02 function
#' @param case.sia tibble df of all SIAs with round numbers included from clustering
#' @param cases tibble df of all detections
#' @param method str method by which to create figures, options: "fig1", "latest.cases", "fig2"
#' @param breakthrough_min_date int minimum days after SIA to be considered breakthrough
#' @param breakthrough_middle_date int number of days to set cutoff between early and late breakthrough
#' @param breakthrough_max_date int maximum number of days a case could be considered breakthrough
create_case_sia_figs <- function(case.sia.02,
                                 case.sia,
                                 cases,
                                 method,
                                 breakthrough_min_date=load_parameters()$breakthrough_min_date,
                                 breakthrough_middle_date = load_parameters()$breakthrough_middle_date,
                                 breakthrough_max_date = load_parameters()$breakthrough_max_date){

  if(method == "fig1"){

    #case.by.break.01 is EVERY cVDPV2 detection with whether or not its is a breakthrough
    #note because of AFG and PAK using tOPV, for now they are excluded
    case.sia.fig.01 <- dplyr::bind_rows(create_cases_by_break(case.sia,
                                                              cases,
                                                              breakthrough_min_date,
                                                              breakthrough_middle_date,
                                                              breakthrough_max_date) |>
                                          dplyr::select(id = epid, adm2guid, date = dateonset,
                                                        breakthrough.01case, breakthrough.02case,
                                                        place.admin.0, place.admin.1, place.admin.2,
                                                        source, measurement, emergencegroup),
                                        case.sia.02 |>
                                          dplyr::select(id = sia.sub.activity.code, adm2guid,
                                                        date = sub.activity.start.date,
                                                        place.admin.0, place.admin.1, place.admin.2,
                                                        round.num, vaccine.type)) |>
      dplyr::mutate(typea = dplyr::case_when(round.num == 1 & vaccine.type == "mOPV2"~ "mOPV2 R1",
                                             round.num == 2 & vaccine.type == "mOPV2"~ "mOPV2 R2",
                                             round.num == 3 & vaccine.type == "mOPV2"~ "mOPV2 R3",
                                             round.num == 4 & vaccine.type == "mOPV2"~ "mOPV2 R4",
                                             round.num == 5 & vaccine.type == "mOPV2"~ "mOPV2 R5",
                                             round.num == 6 & vaccine.type == "mOPV2"~ "mOPV2 R6",
                                             round.num == 7 & vaccine.type == "mOPV2"~ "mOPV2 R7",
                                             round.num == 8 & vaccine.type == "mOPV2"~ "mOPV2 R8",
                                             round.num >= 9 & vaccine.type == "mOPV2"~ "mOPV2 R9+",

                                             round.num == 1 & vaccine.type == "nOPV2"~ "nOPV2 R1",
                                             round.num == 2 & vaccine.type == "nOPV2"~ "nOPV2 R2",
                                             round.num == 3 & vaccine.type == "nOPV2"~ "nOPV2 R3",
                                             round.num == 4 & vaccine.type == "nOPV2"~ "nOPV2 R4",
                                             round.num >= 5 & vaccine.type == "nOPV2"~ "nOPV2 R5+",

                                             round.num == 1 & vaccine.type == "tOPV"~ "tOPV R1",
                                             round.num == 2 & vaccine.type == "tOPV"~ "tOPV R2",
                                             round.num == 3 & vaccine.type == "tOPV"~ "tOPV R3",
                                             round.num == 4 & vaccine.type == "tOPV"~ "tOPV R4",
                                             round.num >= 5 & vaccine.type == "tOPV"~ "tOPV R5+",

                                             round.num == 1 & vaccine.type == "bOPV"~ "bOPV R1",
                                             round.num == 2 & vaccine.type == "bOPV"~ "bOPV R2",
                                             round.num == 3 & vaccine.type == "bOPV"~ "bOPV R3",
                                             round.num == 4 & vaccine.type == "bOPV"~ "bOPV R4",
                                             round.num >= 5 & vaccine.type == "bOPV"~ "bOPV R5+",

                                             breakthrough.01case == 1~ paste0("Breakthrough ",breakthrough_min_date,"-",breakthrough_middle_date," days"),
                                             breakthrough.02case == 1 ~ paste0("Breakthrough ",breakthrough_middle_date+1,"-",breakthrough_max_date," days"),
                                             breakthrough.02case == 0 & breakthrough.01case == 0 ~ "Not breakthrough"),
                                             typeb = ifelse(is.na(round.num) ==T, "Detection", "SIA")) |>
      dplyr::mutate(geo.list = paste(place.admin.1, place.admin.0, sep=", "),
             yr.sia = lubridate::year(date))

    return(case.sia.fig.01)
  }

  if(method == "latest.cases"){
    #Code to limit sia to only failures in past 365 days for figures

    latest.cases <- create_cases_by_break(case.sia,
                                          cases,
                                          breakthrough_min_date,
                                          breakthrough_middle_date,
                                          breakthrough_max_date) |>
      dplyr::mutate(time.from.detect = Sys.Date()-dateonset) |>
      dplyr::filter(time.from.detect <= breakthrough_max_date) |>
      dplyr::filter(breakthrough.01case == 1 | breakthrough.02case == 1) |>
      dplyr::select(adm2guid) |>
      dplyr::distinct()
    #above are only breakthroughs in past year

    return(latest.cases)
  }

  if(method == "fig2"){

    #case.by.break.01 is EVERY cVDPV2 detection with whether or not its is a breakthrough
    #note because of AFG and PAK using tOPV, for now they are excluded
    case.sia.fig.01 <- dplyr::bind_rows(create_cases_by_break(case.sia,
                                                              cases,
                                                              breakthrough_min_date,
                                                              breakthrough_middle_date,
                                                              breakthrough_max_date) |>
                                   dplyr::select(id = epid, adm2guid, date = dateonset,
                                                 breakthrough.01case, breakthrough.02case,
                                                 place.admin.0, place.admin.1, place.admin.2,
                                                 source, measurement, emergencegroup),
                                   case.sia.02 |>
                                   dplyr::select(id = sia.sub.activity.code, adm2guid,
                                                 date = sub.activity.start.date,
                                                 place.admin.0, place.admin.1, place.admin.2,
                                                 round.num, vaccine.type)) |>
      dplyr::mutate(typea = dplyr::case_when(round.num == 1 & vaccine.type == "mOPV2"~ "mOPV2 R1",
                                             round.num == 2 & vaccine.type == "mOPV2"~ "mOPV2 R2",
                                             round.num == 3 & vaccine.type == "mOPV2"~ "mOPV2 R3",
                                             round.num == 4 & vaccine.type == "mOPV2"~ "mOPV2 R4",
                                             round.num == 5 & vaccine.type == "mOPV2"~ "mOPV2 R5",
                                             round.num == 6 & vaccine.type == "mOPV2"~ "mOPV2 R6",
                                             round.num == 7 & vaccine.type == "mOPV2"~ "mOPV2 R7",
                                             round.num == 8 & vaccine.type == "mOPV2"~ "mOPV2 R8",
                                             round.num >= 9 & vaccine.type == "mOPV2"~ "mOPV2 R9+",

                                             round.num == 1 & vaccine.type == "nOPV2"~ "nOPV2 R1",
                                             round.num == 2 & vaccine.type == "nOPV2"~ "nOPV2 R2",
                                             round.num == 3 & vaccine.type == "nOPV2"~ "nOPV2 R3",
                                             round.num == 4 & vaccine.type == "nOPV2"~ "nOPV2 R4",
                                             round.num >= 5 & vaccine.type == "nOPV2"~ "nOPV2 R5+",

                                             round.num == 1 & vaccine.type == "tOPV"~ "tOPV R1",
                                             round.num == 2 & vaccine.type == "tOPV"~ "tOPV R2",
                                             round.num == 3 & vaccine.type == "tOPV"~ "tOPV R3",
                                             round.num == 4 & vaccine.type == "tOPV"~ "tOPV R4",
                                             round.num >= 5 & vaccine.type == "tOPV"~ "tOPV R5+",

                                             round.num == 1 & vaccine.type == "bOPV"~ "bOPV R1",
                                             round.num == 2 & vaccine.type == "bOPV"~ "bOPV R2",
                                             round.num == 3 & vaccine.type == "bOPV"~ "bOPV R3",
                                             round.num == 4 & vaccine.type == "bOPV"~ "bOPV R4",
                                             round.num >= 5 & vaccine.type == "bOPV"~ "bOPV R5+",

                                             breakthrough.01case == 1~ paste0("Breakthrough ",breakthrough_min_date,"-",breakthrough_middle_date," days"),
                                             breakthrough.02case == 1 ~ paste0("Breakthrough ",breakthrough_middle_date+1,"-",breakthrough_max_date," days"),
                                             breakthrough.02case == 0 & breakthrough.01case == 0 ~ "Not breakthrough"),
                                             typeb = ifelse(is.na(round.num) ==T, "Detection", "SIA")) |>
      dplyr::mutate(geo.list = paste(place.admin.1, place.admin.0, sep=", "),
                    yr.sia = lubridate::year(date))

    latest.cases <- create_cases_by_break(case.sia,
                                          cases,
                                          breakthrough_min_date,
                                          breakthrough_middle_date,
                                          breakthrough_max_date) |>
      dplyr::mutate(time.from.detect = Sys.Date()-dateonset) |>
      dplyr::filter(time.from.detect <= breakthrough_max_date) |>
      dplyr::filter(breakthrough.01case == 1 | breakthrough.02case == 1) |>
      dplyr::select(adm2guid) |>
      dplyr::distinct()

    case.sia.fig.02 <- case.sia.fig.01 |>
      dplyr::filter(adm2guid %in% latest.cases$adm2guid) |>
      dplyr::group_by(adm2guid) |>
      dplyr::mutate(max.date = max(date)) |>
      dplyr::filter(max.date == date) |>
      dplyr::filter(typeb == "Detection" &
                      (typea == paste0("Breakthrough ",breakthrough_min_date,"-",breakthrough_middle_date," days") |
                         typea == paste0("Breakthrough ", breakthrough_middle_date+1,"-",breakthrough_max_date," days"))) |>
      dplyr::ungroup() |>
      dplyr::select(place.admin.1, place.admin.0) |>
      dplyr::mutate(geo.list = paste(place.admin.1, place.admin.0, sep=", ")) |>
      dplyr::select(geo.list) |>
      dplyr::distinct()

    return(case.sia.fig.02)

  }
}

#' @description
#' a function to determine if individual cases are breakthrough
#' @import dplyr
#' @param case.sia tibble df of all SIAs with round numbers included from clustering
#' @param cases tibble df of detections
#' @param breakthrough_min_date int minimum days after SIA to be considered breakthrough
#' @param breakthrough_middle_date int number of days to set cutoff between early and late breakthrough
#' @param breakthrough_max_date int maximum number of days a case could be considered breakthrough
create_cases_by_break <- function(case.sia,
                                  cases,
                                  breakthrough_min_date=load_parameters()$breakthrough_min_date,
                                  breakthrough_middle_date = load_parameters()$breakthrough_middle_date,
                                  breakthrough_max_date = load_parameters()$breakthrough_max_date){

  cases.by.break <- dplyr::full_join(case.sia, cases, by=c("adm2guid", "place.admin.0",
                                                           "place.admin.1", "place.admin.2")) |>
    dplyr::select(sia.sub.activity.code, sub.activity.start.date, last.camp, place.admin.2,
                  place.admin.1, place.admin.0, adm2guid, cluster, round.num, epid, dateonset, source) |>
    dplyr::filter(!is.na(sia.sub.activity.code) & !is.na(epid)) |>
    dplyr::mutate(timetocase = dateonset-sub.activity.start.date) |>
    dplyr::filter(timetocase >= breakthrough_min_date & timetocase <= breakthrough_max_date & last.camp == 1) |>
    dplyr::mutate(breakthrough.01case = ifelse(timetocase >= breakthrough_min_date & timetocase <= breakthrough_middle_date, 1, 0),
                  breakthrough.02case = ifelse(timetocase > breakthrough_min_date & timetocase <= breakthrough_max_date, 1, 0)) |>
    dplyr::select(epid,timetocase, breakthrough.01case, breakthrough.02case) |>
    dplyr::distinct() |>
    dplyr::group_by(epid) |>
    dplyr::mutate(min.timetocase=min(timetocase)) |>
    dplyr::filter(min.timetocase==timetocase)


  cases.by.break.01 <- dplyr::left_join(cases, cases.by.break, by = c("epid")) |>
    dplyr::mutate(breakthrough.01case = ifelse(is.na(breakthrough.01case) == T, 0, breakthrough.01case),
                  breakthrough.02case = ifelse(is.na(breakthrough.02case) == T, 0, breakthrough.02case))

  return(cases.by.break.01)
}


#' @description
#' a function to create donut maps
#' @import dplyr tidyr tidypolis stringr
#' @param .folder str folder location to output donut maps
#' @param case.sia.02 tibble df from create_case_sia_02 function
#' @param .case.data tibble df of cases to be used
#' @param dist.shapes sf object of district shapes default is raw.data$global.dist
#' @param ctry.shapes sf object of country shapes default is raw.data$global.ctry
#' @param detection_pre_sia_date int used to restrict "Recent SIA with breakthrough transmission" figures to 'recent' SIAs
#' @param breakthrough_middle_date int number of days to set cutoff between early and late breakthrough
run_donut <- function(.folder = paste0(Sys.getenv("SIA_FOLDER"), "/outputs/100km"),
                      case.sia.02,
                      .case.data,
                      dist.shapes = raw.data$global.dist,
                      ctry.shapes = raw.data$global.ctry,
                      detection_pre_sia_date = load_parameters()$detection_pre_sia_date,
                      breakthrough_middle_date = load_parameters()$breakthrough_middle_date){
  #load data

  sia.04 <- case.sia.02 |>
    dplyr::ungroup() |>
    dplyr::filter(yr.sia >= lubridate::year(load_parameters()$start_date), vaccine.type %in% c("mOPV2", "nOPV2", "tOPV", "bOPV")) |>
    dplyr::select(sia.sub.activity.code, GUID = adm2guid, activity.start.date = sub.activity.start.date,
                  activity.end.date = sub.activity.end.date, vaccine.type, round.num) |>
    dplyr::mutate(activity.end.date = as_date(activity.end.date))

  pos <- clean_case_data(.case.data, type = "donut")

  global.dist <- dist.shapes

  global.ctry <- ctry.shapes

  #create dataset of country names for plotting
  sia.to.ctry <- dplyr::left_join(dplyr::select(sia.04, sia.sub.activity.code, GUID),
                                  as_tibble(global.dist) |>
                                    dplyr::select(GUID, ADM0_VIZ_NAME, WHO_REGION),
                                  by = "GUID") |>
    dplyr::select(sia.sub.activity.code, ctry = ADM0_VIZ_NAME, WHO_REGION) |>
    unique() |>
    dplyr::rowwise() |>
    dplyr::mutate(ctry.code = strsplit(sia.sub.activity.code, "-")[[1]][1]) |>
    dplyr::ungroup() |>
    dplyr::select(ctry.code, WHO_REGION, ctry) |>
    unique() |>
    tidyr::drop_na() |>
    dplyr::add_row("ctry.code" = "LBR", "WHO_REGION" = "AFRO", "ctry" = "Liberia")

  #run across all sia codes
  run_sia_spatial_fail(folder = .folder,
                       sia.04 = sia.04,
                       global.dist = global.dist,
                       pos = pos)

  sia.donut <- tidypolis:::tidypolis_io(io = "read", file_path = paste0(Sys.getenv("SIA_FOLDER"), "/outputs/100km/primary_output.csv")) |>
    dplyr::group_by(sia.sub.activity.code) |>
    dplyr::filter(updated == max(updated)) |>
    dplyr::ungroup() |>
    dplyr::filter(sia.sub.activity.code %in% sia.04$sia.sub.activity.code)

  donut.cases <- tidypolis:::tidypolis_io(io = "read", file_path = paste0(Sys.getenv("SIA_FOLDER"), "/outputs/100km/case_output.csv")) |>
    dplyr::group_by(epid, sia.sub.activity.code, emergencegroup) |>
    dplyr::mutate(n = n()) |>
    dplyr::ungroup() |>
    dplyr::group_by(sia.sub.activity.code, epid) |>
    dplyr::filter(updated == max(updated)) |>
    dplyr::ungroup()

  #merging sia.donut with larger sia data

  all.sia <- case.sia.02 |>
    dplyr::group_by(sia.sub.activity.code) |>
    dplyr::mutate(round.num.sia = Mode(round.num),
                  num.breakthrough.01 = sum(breakthrough.01),
                  num.breakthrough.02 = sum(breakthrough.02)) |>
    dplyr::select(sia.sub.activity.code, yr.sia, sub.activity.start.date, place.admin.0,
                  vaccine.type, round.num.sia, num.breakthrough.01, num.breakthrough.02) |>
    dplyr::distinct() |>
    dplyr::mutate(breakthrough.01 = ifelse(num.breakthrough.01>0, 1, 0),
                  breakthrough.02 = ifelse(num.breakthrough.02>0, 1, 0))

  sia.donut.02 <- dplyr::full_join(sia.donut, all.sia, by = c("sia.sub.activity.code")) |>
    dplyr::filter(!is.na(place.admin.0) & !is.na(cases_in_region))


  #merging donut cases to original case.sia.01 which links cases to SIAs

  donut.cases.01 <- donut.cases |>
    dplyr::rename(donut.sia.code = sia.sub.activity.code) |>
    dplyr::mutate(dateonset = as.Date(dateonset)) |>
    dplyr::full_join(case.sia.01, by = c("epid", "dateonset")) |>
    dplyr::filter(!is.na(donut.sia.code)) |>
    dplyr::mutate(covered.by.sia60 = ifelse(timetocase >= -60 & timetocase < 0, 1, 0)) |>
    dplyr::group_by(epid) |>
    dplyr::mutate(sia.after.case60 = sum(covered.by.sia60)) |>
    dplyr::ungroup() |>
    dplyr::select(donut.sia.code, sia.date, epid, dateonset, type,
                  emergencegroup.x, place.admin.0, place.admin.1,
                  place.admin.2, sia.after.case60) |>
    dplyr::distinct()

  donut.cases.02 <- donut.cases |>
    dplyr::mutate(dateonset = as.Date(dateonset)) |>
    dplyr::left_join(donut.cases.01, by=c("epid", "dateonset", "sia.sub.activity.code"="donut.sia.code",
                                          "sia.date", "type")) |>
    dplyr::distinct(sia.sub.activity.code, emergencegroup, epid, .keep_all=T) |>
    dplyr::mutate(sia.after.case60 = ifelse(sia.after.case60 >= 1, 1, 0),
                  type.v2=case_when(type == "In Spatial Buffer" & (sia.after.case60 == 0 | is.na(sia.after.case60)) ~ "In Spatial Buffer, no SIA w/in 60 days",
                                    type == "In Spatial Buffer" & sia.after.case60 == 1  ~ "In Spatial Buffer, SIA w/in 60 days",
                                    type == paste0("In Spatial Buffer -",detection_pre_sia_date," to 0 days")  &  (sia.after.case60 == 0 | is.na(sia.after.case60)) ~ paste0("In Spatial Buffer ",detection_pre_sia_date," to 0 days, no SIA w/in 60 days"),
                                    type == paste0("In Spatial Buffer -",detection_pre_sia_date," to 0 days")  &  sia.after.case60 == 1 ~paste0("In Spatial Buffer ",detection_pre_sia_date," to 0 days, SIA w/in 60 days"),
                                    type == "In SIA Region" ~ "In SIA Region"))


  aoi_list <- sia.donut$sia.sub.activity.code |>
    unique()

  if(tidypolis:::tidypolis_io(io = "exists.file", file_path = paste0(Sys.getenv("SIA_FOLDER"), "/outputs/plot_aoi_list.rds"))){

    print("Previous plot log identified!")

    # old_aoi_list <- read_rds(here("sia_impact_pipeline", "outputs", "plot_aoi_list.rds"))
    old_aoi_list <- tidypolis:::tidypolis_io(io = "list", file_path = paste0(Sys.getenv("SIA_FOLDER"), "/assets/donut_maps")) |>
      stringr::str_replace(".png", "")
    tidypolis:::tidypolis_io(obj = old_aoi_list, io = "write", file_path = paste0(Sys.getenv("SIA_FOLDER"), "/outputs/plot_aoi_list.rds"))
    aoi_list <- aoi_list[!aoi_list %in% old_aoi_list]

    if(length(aoi_list) == 0){print("All previous SIAs have been plotted, replotting SIAs from the last 180 days.")}

    recent_aoi_list <- sia.donut |>
      dplyr::filter(Sys.Date() - start_date <= breakthrough_middle_date) |>
      dplyr::pull(sia.sub.activity.code) |>
      #c(aoi_list) %>%
      unique()

    print(paste0("Plotting ", length(aoi_list)+length(recent_aoi_list), " figures."))

    tidypolis:::tidypolis_io(obj = unique(c(aoi_list, old_aoi_list)), io = "write", file_path = paste0(Sys.getenv("SIA_FOLDER"), "/outputs/plot_aoi_list.rds"))

    #delete now canceled sia
    aoi_list <- aoi_list[aoi_list %in% sia.04$sia.sub.activity.code] |>
      c(recent_aoi_list) |>
      unique() |>
      sort()

  }else{

    tidypolis:::tidypolis_io(obj = aoi_list, io = "write", file_path = paste0(Sys.getenv("SIA_FOLDER"), "/outputs/plot_aoi_list.rds"))

    #delete now canceled sia
    aoi_list <- aoi_list[aoi_list %in% sia.04$sia.sub.activity.code] |>
      unique() |>
      sort()

  }

  run_sia_spatial_fail_plots_v2(aoi_list = aoi_list,
                                sia.04 = sia.04,
                                global.dist = global.dist,
                                sk_output = donut.cases.02,
                                pos = pos,
                                sia.to.ctry = sia.to.ctry,
                                global.ctry = global.ctry,
                                plot_folder = paste0(Sys.getenv("SIA_FOLDER"), "/assets/donut_maps"))


  return(list(
    "sia.donut" = sia.donut,
    "all.sia" = all.sia,
    "donut.cases.02" = donut.cases.02
  ))

}


#' Run SIA spatial fail, error checking and logs
#' @description
#' a function to run sia spatial fail, inlcuding error checking and logs
#' @import dplyr
#' @param overwrite boolean delete existing folders (not yet implemented)
#' @param sia.04 tibble The latest sia datset created by SK.
#' @param global.dist sf The shapefile of global districts as cleaned in pre-processing but not deshaped .
#' @param pos tibble The list of positives with latitude changed to "lat" and longitude change to "lon".
#' @param day_min int The minimum number of days to wait before cases are counted
#' as an SIA failure within the super region, default is breakthrough_min_date days.
#' @param day_max int The maximum number of days to wait till cases are counted as
#' an SIA failure within the super region, default is 180 days.
#' @param dist_min int The minimum distance in meters that the buffer should be, default is 100km.
#' @param dist_max int The maximum distance in meters that the buffer should be, default is 500km.
#' @param hard_dist boolean describing if the buffer distance is hard coded
#' @param dist int If 'hard_dist' is true, this value sets the distance of the buffer zone.
#' @param plots boolean Determines if function should return a tibble or the plot
#' features. If plotting, highly recommend just doing one at a time. The
#' default is FALSE
#' @param folder String which describes the folder location
run_sia_spatial_fail <- function(overwrite = T,
                                 sia.04,
                                 global.dist,
                                 pos,
                                 day_min = load_parameters()$breakthrough_min_date,
                                 day_max = load_parameters()$breakthrough_middle_date,
                                 dist_min = 100000,
                                 dist_max = 500000,
                                 hard_dist = T,
                                 dist = 100000,
                                 plots = F,
                                 folder){

  #create folder and files of interest
  if(!tidypolis:::tidypolis_io(io = "exists.dir", file_path = folder)){
    tidypolis:::tidypolis_io(io = "create", file_path = folder)

    #create template files from original report
    prime_output_temp <- data.frame(matrix(ncol = 19, nrow = 0))
    colnames(prime_output_temp) <- c("sia.sub.activity.code", "cases_in_region", "cases_in_buffer_lte28", "x_min",
                                     "x_max", "x_prop_zero", "cases_in_buffer_gt28", "y_min", "y_max", "y_prop_zero",
                                     "cases_in_buffer_lt0", "z_min", "z_max", "z_prop_zero", "dist", "day_min",
                                     "day_max", "start_date", "updated")
    tidypolis:::tidypolis_io(obj = prime_output_temp, io = "write", file_path = paste0(folder, "/primary_output.csv"))

    case_output_temp <- data.frame(matrix(ncol = 7, nrow = 0))
    colnames(case_output_temp) <- c("sia.sub.activity.code", "sia.date", "epid", "dateonset",
                                    "type", "emergencegroup", "updated")
    tidypolis:::tidypolis_io(obj = case_output_temp, io = "write", file_path = paste0(folder, "/case_output.csv"))

    type <- c("country", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg",
              "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "country", "country",
              "country", "country", "country", "country", "country", "country",
              "country", "country", "country", "country", "country", "country",
              "country", "country", "country", "country", "country", "country",
              "country", "country", "country", "country", "country", "country",
              "country", "country", "country", "country", "country", "country",
              "country", "country", "country", "country", "country", "country",
              "country", "country", "country", "country", "country", "country",
              "country", "country", "country", "country", "country", "country",
              "country", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg",
              "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg",
              "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg",
              "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg",
              "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg",
              "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg", "cg",
              "cg", "cg", "cg", "cg", "cg")
    label <- c("Nigeria", "Lake Chad", "Lake Chad", "Lake Chad", "Lake Chad", "Lake Chad",
               "Lake Chad", "Lake Chad", "Lake Chad", "Lake Chad", "Lake Chad", "Lake Chad",
               "Lake Chad", "Lake Chad", "Lake Chad", "Lake Chad", "Lake Chad", "Lake Chad",
               "Lake Chad", "Pakistan", "Lao People's Democratic Republic", "Nigeria",
               "Afghanistan", "Angola", "Democratic Republic Of The Congo", "Ghana",
               "Syrian Arab Republic", "Philippines", "Niger", "Central African Republic",
               "Burkina Faso", "Malaysia", "Papua New Guinea", "Chad", "Ethiopia",
               "Somalia", "China", "Benin", "Togo", "Indonesia", "Yemen", "Myanmar",
               "Mozambique", "Zambia", "Cote D Ivoire", "Sudan", "South Sudan",
               "Madagascar", "Guinea", "Senegal", "Mali", "Cameroon", "Tajikistan",
               "Guinea-Bissau", "Eritrea", "Ukraine", "Sierra Leone", "Congo", "Liberia",
               "Algeria", "Malawi", "Israel", "Djibouti", "Occupied Palestinian Territory, Including East Jerusalem",
               "Egypt", "Uganda", "Gambia", "Mauritania", "Iran (Islamic Republic Of)", "Kenya",
               "Northern Nigeria", "Northern Nigeria", "Northern Nigeria", "Northern Nigeria",
               "Northern Nigeria", "Northern Nigeria", "Northern Nigeria", "Northern Nigeria",
               "Northern Nigeria", "Northern Nigeria", "Northern Nigeria", "Eastern DRC",
               "Eastern DRC", "Eastern DRC", "North Yemen", "North Yemen", "North Yemen",
               "North Yemen", "North Yemen", "North Yemen", "North Yemen", "North Yemen",
               "North Yemen", "North Yemen", "North Yemen", "North Yemen", "North Yemen",
               "North Yemen", "North Yemen", "North Yemen", "North Yemen", "North Yemen",
               "North Yemen", "South Central Somalia", "South Central Somalia",
               "South Central Somalia", "South Central Somalia", "South Central Somalia",
               "South Central Somalia", "South Central Somalia", "South Central Somalia",
               "South Central Somalia", "South Central Somalia", "High Risk Afghanistan",
               "High Risk Afghanistan", "High Risk Afghanistan", "High Risk Afghanistan",
               "High Risk Afghanistan", "High Risk Afghanistan", "High Risk Afghanistan",
               "High Risk Pakistan", "High Risk Pakistan", "High Risk Pakistan", "High Risk Pakistan",
               "High Risk Pakistan", "High Risk Pakistan", "High Risk Pakistan", "High Risk Pakistan",
               "High Risk Pakistan", "High Risk Pakistan", "High Risk Pakistan", "High Risk Pakistan",
               "High Risk Pakistan", "High Risk Pakistan", "High Risk Pakistan", "High Risk Pakistan",
               "High Risk Pakistan", "High Risk Pakistan", "High Risk Pakistan", "High Risk Pakistan",
               "High Risk Pakistan", "High Risk Pakistan", "High Risk Pakistan", "High Risk Pakistan",
               "High Risk Pakistan")
    ctry <- c("NIGERIA", "NIGER", "NIGER", "NIGERIA", "NIGERIA", "NIGERIA", "NIGERIA",
              "NIGERIA", "NIGERIA", "NIGERIA", "NIGERIA", "NIGERIA", "NIGERIA", "CHAD",
              "CHAD", "CHAD", "CHAD", "CHAD", "CHAD", "PAKISTAN", "LAO PEOPLE'S DEMOCRATIC REPUBLIC",
              "NIGERIA", "AFGHANISTAN", "ANGOLA", "DEMOCRATIC REPUBLIC OF THE CONGO", "GHANA",
              "SYRIAN ARAB REPUBLIC", "PHILIPPINES", "NIGER", "CENTRAL AFRICAN REPUBLIC",
              "BURKINA FASO", "MALAYSIA", "PAPUA NEW GUINEA", "CHAD", "ETHIOPIA", "SOMALIA",
              "CHINA", "BENIN", "TOGO", "INDONESIA", "YEMEN", "MYANMAR", "MOZAMBIQUE",
              "ZAMBIA", "COTE D IVOIRE", "SUDAN", "SOUTH SUDAN", "MADAGASCAR", "GUINEA",
              "SENEGAL", "MALI", "CAMEROON", "TAJIKISTAN", "GUINEA-BISSAU", "ERITREA",
              "UKRAINE", "SIERRA LEONE", "CONGO", "LIBERIA", "ALGERIA", "MALAWI", "ISRAEL",
              "DJIBOUTI", "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM", "EGYPT",
              "UGANDA", "GAMBIA", "MAURITANIA", "IRAN (ISLAMIC REPUBLIC OF)", "KENYA",
              "NIGERIA", "NIGERIA", "NIGERIA", "NIGERIA", "NIGERIA", "NIGERIA", "NIGERIA",
              "NIGERIA", "NIGERIA", "NIGERIA", "NIGERIA", "DEMOCRATIC REPUBLIC OF THE CONGO",
              "DEMOCRATIC REPUBLIC OF THE CONGO", "DEMOCRATIC REPUBLIC OF THE CONGO",
              "YEMEN", "YEMEN", "YEMEN", "YEMEN", "YEMEN", "YEMEN", "YEMEN", "YEMEN", "YEMEN",
              "YEMEN", "YEMEN", "YEMEN", "YEMEN", "YEMEN", "YEMEN", "YEMEN", "YEMEN", "YEMEN",
              "YEMEN", "SOMALIA", "SOMALIA", "SOMALIA", "SOMALIA", "SOMALIA", "SOMALIA",
              "SOMALIA", "SOMALIA", "SOMALIA", "SOMALIA", "AFGHANISTAN", "AFGHANISTAN",
              "AFGHANISTAN", "AFGHANISTAN", "AFGHANISTAN", "AFGHANISTAN", "AFGHANISTAN",
              "PAKISTAN", "PAKISTAN", "PAKISTAN", "PAKISTAN", "PAKISTAN", "PAKISTAN",
              "PAKISTAN", "PAKISTAN", "PAKISTAN", "PAKISTAN", "PAKISTAN", "PAKISTAN",
              "PAKISTAN", "PAKISTAN", "PAKISTAN", "PAKISTAN", "PAKISTAN", "PAKISTAN",
              "PAKISTAN", "PAKISTAN", "PAKISTAN", "PAKISTAN", "PAKISTAN", "PAKISTAN",
              "PAKISTAN")
    prov <- c("", "DIFFA", "ZINDER", "BAUCHI", "BORNO", "JIGAWA", "KADUNA", "KANO", "KATSINA",
              "KEBBI", "SOKOTO", "YOBE", "ZAMFARA", "CHARI BAGUIRMI", "HADJER LAMIS",
              "LAC", "MAYO KEBBI EST", "MAYO KEBBI OUEST", "N'DJAMENA", "", "", "", "",
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
              "", "", "", "", "", "", "", "", "", "", "", "BAUCHI", "BORNO", "JIGAWA",
              "KADUNA", "KANO", "KATSINA", "KEBBI", "SOKOTO", "YOBE", "GOMBE", "ZAMFARA",
              "MANIEMA", "TANGANYIKA", "HAUT LOMAMI", "SAADAH", "ALJAWF", "HAJJAH",
              "AMRAN", "ALHUDAIDAH", "ALMAHAWEET", "SANAA", "SANAA CITY", "MARIB",
              "RIMAH", "DHAMAR", "IBB", "ALBAIDAH", "DHAMAR", "TAIZ", "LAHAJ", "ADEN",
              "ALDHALE", "ABYAN", "GALGADUD", "HIRAN", "MIDDLE SHABELLE", "LOWER SHABELLE",
              "BANADIR", "BAKOL", "BAY", "GEDO", "MIDDLE JUBA", "LOWER JUBA", "KANDAHAR",
              "HILMAND", "URUZGAN", "NANGARHAR", "KUNAR", "PAKTIKA", "FARAH", "BALOCHISTAN",
              "BALOCHISTAN", "BALOCHISTAN", "BALOCHISTAN", "ISLAMABAD", "ISLAMABAD",
              "KPAKHTUNKHWA", "KPAKHTUNKHWA", "KPAKHTUNKHWA", "KPAKHTUNKHWA", "KPAKHTUNKHWA",
              "KPAKHTUNKHWA", "KPAKHTUNKHWA", "KPAKHTUNKHWA", "PUNJAB", "PUNJAB", "PUNJAB",
              "SINDH", "SINDH", "SINDH", "SINDH", "SINDH", "SINDH", "SINDH","SINDH")
    dist <- c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
              "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
              "", "", "", "", "", "", "", "", "", "", "", "", "KABDULAH", "MASTUNG",
              "PISHIN", "QUETTA", "CDA", "ICT", "BANNU", "DIKHAN", "KHYBER", "LAKKIMRWT",
              "PESHAWAR", "TANK", "WAZIR-N", "WAZIR-S", "FAISALABAD", "LAHORE", "RAWALPINDI",
              "HYDERABAD", "KHICENTRAL", "KHIEAST", "KHIKAMARI", "KHIKORANGI", "KHIMALIR",
              "KHISOUTH", "KHIWEST")
    adm_level <- c("", "adm1", "adm1", "adm1", "adm1", "adm1", "adm1", "adm1", "adm1",
                   "adm1", "adm1", "adm1", "adm1", "adm1", "adm1", "adm1", "adm1",
                   "adm1", "adm1", "", "", "", "", "", "", "", "", "", "", "", "",
                   "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                   "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                   "", "", "", "", "", "", "", "adm1", "adm1", "adm1", "adm1", "adm1",
                   "adm1", "adm1", "adm1", "adm1", "adm1", "adm1", "adm1", "adm1",
                   "adm1", "adm1", "adm1", "adm1", "adm1", "adm1", "adm1", "adm1",
                   "adm1", "adm1", "adm1", "adm1", "adm1", "adm1", "adm1", "adm1",
                   "adm1", "adm1", "adm1", "adm1", "adm1", "adm1", "adm1", "adm1",
                   "adm1", "adm1", "adm1", "adm1", "adm1", "adm1", "adm1", "adm1",
                   "adm1", "adm1", "adm1", "adm1", "adm1", "adm2", "adm2", "adm2",
                   "adm2", "adm2", "adm2", "adm2", "adm2", "adm2", "adm2", "adm2",
                   "adm2", "adm2", "adm2", "adm2", "adm2", "adm2", "adm2", "adm2",
                   "adm2", "adm2", "adm2", "adm2", "adm2", "adm2")
    geo_temp <- data.frame(type, label, ctry, prov, dist, adm_level)

    tidypolis:::tidypolis_io(obj = geo_temp, io = "write", file_path = paste0(folder, "/geographies.csv"))
  }


  #create cache objects

  aoi_list <- sort(unique(sia.04$sia.sub.activity.code))

  #check if cache exists
  if(file.exists(paste0(folder, "/done_aoi_list.rds"))){

    print("Previous run log found!")

    old_aoi_list <- read_rds(paste0(folder, "/done_aoi_list.rds"))
    aoi_list <- aoi_list[!aoi_list %in% old_aoi_list]

    print(paste0(length(aoi_list), " new records identified"))

    aoi_list <- sia.04 |>
      dplyr::filter(Sys.Date() - activity.start.date <= 180) |>
      dplyr::pull(sia.sub.activity.code) |>
      c(aoi_list) |>
      unique()

    print(paste0(length(aoi_list), " total new records being processed including those that occured in the last 180 days"))


    tidypolis:::tidypolis_io(obj = c(old_aoi_list, aoi_list) %>% unique(), io = "write", file_path = paste0(folder, "/done_aoi_list.rds"))

  }else{

    tidypolis:::tidypolis_io(obj = aoi_list, io = "write", file_path = paste0(folder, "/done_aoi_list.rds"))

  }

  lapply(aoi_list, function(x) sia_spatial_fail(aoi = x,
                                                sia.04 = sia.04,
                                                global.dist = global.dist,
                                                pos = pos,
                                                plots = F,
                                                folder = paste0(folder,"/")))

}



#' Generate spatial maps Version 2:
#' @description
#' A wrapper function to create spatial map outputs version 2 including
#' classification by outputs provided by SK
#' Run SIA spatial fail, error checking and logs
#' @import dplyr sf ggplot2
#' @param aoi_list character vector of sia sub activity codes
#' @param overwrite Boolean, delete existing folders (not yet implemented)
#' @param sia.04 The latest sia datset created by SK.
#' @param global.dist The shapefile of global districts as cleaned in pre-
#' processing but not deshaped .
#' @param pos The list of positives with latitude changed to "lat" and
#' longitude change to "lon".
#' @param sia.to.ctry Dataset of country names for plotting
#' @param sk_output Dataset with classified outputs
#' @param day_min The minimum number of days to wait before cases are counted
#' as an SIA failure within the super region, default is breakthrough_min_date days.
#' @param day_max The maximum number of days to wait till cases are counted as
#' an SIA failure within the super region, default is 180 days.
#' @param dist_min The minimum distance in meters that the buffer should be,
#' default is 100km.
#' @param dist_max The maximum distance in meters that the buffer should be,
#' default is 500km.
#' @param hard_dist A boolean describing if the buffer distanc is hard coded
#' @param dist If 'hard_dist' is true, this value sets the distance of the
#' buffer zone.
#' @param plots Determines if function should return a tibble or the plot
#' features. If plotting, highly recommend just doing one at a time. The
#' default is FALSE
#' @param plot_folder String which describes the folder location
run_sia_spatial_fail_plots_v2 <- function(aoi_list,
                                          sia.04,
                                          global.dist,
                                          pos,
                                          sk_output,
                                          global.ctry,
                                          sia.to.ctry,
                                          day_min = load_parameters()$breakthrough_min_date,
                                          day_max = load_parameters()$breakthrough_middle_date,
                                          detection_pre_sia_date = load_parameters()$detection_pre_sia_date,
                                          dist_min = 100000,
                                          dist_max = 500000,
                                          hard_dist = T,
                                          dist = 100000,
                                          plot_folder = paste0(Sys.getenv("SIA_FOLDER"), "/assets/donut_maps")){

  for(i in 1:length(aoi_list)){
    aoi <- aoi_list[i]
    print(paste0(i, " of ", length(aoi_list)))
    #generate spatial output
    x <- sia_spatial_fail(aoi = aoi,
                          sia.04 = sia.04,
                          global.dist = global.dist,
                          pos = pos,
                          plots = T)

    viz_bbox <- sf::st_bbox(x$buffer) |>
      expand_bbox(100000, 100000)

    ctry.name <- dplyr::filter(sia.to.ctry, ctry.code == x$sia.sub.activity.code |>
                                 str_split("-") %>%
                                 {.[[1]][1]}) |>
      dplyr::pull(ctry)

    round.num <- dplyr::filter(sia.04, sia.sub.activity.code == x$sia.sub.activity.code) |>
      dplyr::pull(round.num) |>
      max()

    ctry.geom <- global.ctry |>
      dplyr::filter(ADM0_VIZ_NAME == ctry.name)

    cases <- dplyr::bind_rows(x$cases_in_buffer, x$cases_in_region, x$cases_prev_in_buffer) |>
      dplyr::left_join(dplyr::filter(sk_output, sia.sub.activity.code == aoi) |>
                         dplyr::select(epid, emergencegroup, dateonset, type.v2),
                       by = c("epid", "emergencegroup", "dateonset")) |>
      dplyr::mutate(type.v2 = case_when(
        type.v2 == "In Spatial Buffer, no SIA w/in 60 days" ~ "In Spatial Buffer after SIA,\n not covered by other SIA",
        type.v2 == "In Spatial Buffer, SIA w/in 60 days" ~ "In Spatial Buffer,\n covered by other SIA",
        type.v2 == paste0("In Spatial Buffer ",detection_pre_sia_date," to 0 days, SIA w/in 60 days") ~ paste0("In Spatial Buffer,\n covered by other SIA"),
        type.v2 == "In SIA Region" ~ "In SIA Region",
        type.v2 == paste0("In Spatial Buffer ",detection_pre_sia_date," to 0 days, no SIA w/in 60 days") ~ paste0("In Spatial Buffer before SIA,\n not covered by other SIA")
      ),
      type.v2 = factor(type.v2,
                       levels = c("In SIA Region",
                                  "In Spatial Buffer,\n covered by other SIA",
                                  "In Spatial Buffer before SIA,\n not covered by other SIA",
                                  "In Spatial Buffer after SIA,\n not covered by other SIA"))) |>
      dplyr::filter(!is.na(type.v2))

    map <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = global.ctry, fill = "white", size = 1.01, color = "gray", alpha=0.4) +
      ggplot2::geom_sf(data = ctry.geom, fill = "#e5d8bd", size = 1.01, color = "black", alpha=0.6) +
      ggplot2::geom_sf(data = x$sr, color = "black", size = 1.5, fill = "gray", alpha=0.8) +
      ggplot2::geom_sf(data = x$buffer, color = "blue", size = 1.01, fill = NA, alpha=0.6) +
      ggplot2::geom_sf(data = cases, aes(color = type.v2, shape = type.v2, size = type.v2), alpha=0.8) +
      ggplot2::coord_sf(xlim = viz_bbox[c(1,3)], ylim = viz_bbox[c(2,4)]) +
      ggplot2::scale_color_manual(values = c("black", "black", "#a50f15", "#a50f15"), drop = F) +
      # scale_color_manual(values = c("orange", "red", "blue", "purple", "green"),
      #                    drop = F) +
      ggplot2::scale_shape_manual(values = c(16, 1, 18, 13), drop = F) +
      # scale_shape_manual(values = c(16, 18, 16, 1, 1), drop = F) +
      # scale_size_manual(values = c(2, 4, 2, 2, 2), drop = F) +
      ggplot2::scale_size_manual(values = c(2, 2, 3, 3), drop = F) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom",
                     axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::guides(color=guide_legend(nrow=2,byrow=TRUE)) +
      ggplot2::labs(color = "Type of Detection", shape = "Type of Detection", size = "Type of Detection",
                    title = paste0("\nRound ", round.num, " SIA in ", ctry.name, " (in brown), started on ", x$start_date),
                    subtitle = paste0(nrow(x$cases_in_region), " detection(s) in SIA zone and ",
                                      nrow(x$cases_in_buffer) + nrow(x$cases_prev_in_buffer), " in buffer, ",
                                      nrow(dplyr::filter(cases, type.v2 %in% c("In Spatial Buffer before SIA,\n not covered by other SIA",
                                                                               "In Spatial Buffer after SIA,\n not covered by other SIA"))), " not covered by an SIA"),
                    caption = paste0("SIA Code: ", x$sia.sub.activity.code,"; Buffer Size: ", round(x$dist/1000,0),
                                     "km;\nBefore SIA: Detection 0-",detection_pre_sia_date," days before SIA;\nAfter SIA: Detection ",day_min,"-",day_max," days after SIA;\nCovered by other SIA: Other SIA covered the district 0-60 days after detection." ))

    print(paste0("Writing map to: ", paste0(plot_folder, "/", x$sia.sub.activity.code, ".png")))
    f.save.plot(plot_folder,
                plot_name = x$sia.sub.activity.code,
                p = map,
                save_plot = F,
                width = 9,
                height = 9,
                dpi = 300)

    print(paste0(x$sia.sub.activity.code, " map completed and saved!"))

    gc()

  }

}


#' @description
#' Check if a spatial super region and buffer zone contains cases in the
#' time period of interest after activity
#' @import dplyr sf lubridate
#' @param aoi Activity of interest, the sia sub activity code used to
#' subset the sia data.
#' @param sia.04 The latest sia datset created by SK.
#' @param global.dist The shapefile of global districts as cleaned in pre-
#' processing but not deshaped .
#' @param pos The list of positives with latitude changed to "lat" and
#' longitude change to "lon".
#' @param day_min The minimum number of days to wait before cases are counted
#' as an SIA failure within the super region, default is 'breakthrough_min_date' days.
#' @param day_max The maximum number of days to wait till cases are counted as
#' an SIA failure within the super region, default is 180 days.
#' @param dist_min The minimum distance in meters that the buffer should be,
#' default is 100km.
#' @param dist_max The maximum distance in meters that the buffer should be,
#' default is 500km.
#' @param hard_dist A boolean describing if the buffer distanc is hard coded
#' @param dist If 'hard_dist' is true, this value sets the distance of the
#' buffer zone.
#' @param plots Determines if function should return a tibble or the plot
#' features. If plotting, highly recommend just doing one at a time. The
#' default is FALSE
#' @param folder String which describes the folder location
sia_spatial_fail <- function(aoi,
                             sia.04,
                             global.dist,
                             pos,
                             day_min = load_parameters()$breakthrough_min_date,
                             day_max = load_parameters()$breakthrough_middle_date,
                             detection_pre_sia_date = load_parameters()$detection_pre_sia_date,
                             dist_min = 100000,
                             dist_max = 500000,
                             hard_dist = T,
                             dist = 100000,
                             plots = F,
                             folder){

  tryCatch(
    {

      #set the use of S2 geometries to F to avoid overlapping vertices issues
      sf::sf_use_s2(F)
      print(aoi)
      #activity start date
      start_date <- dplyr::filter(sia.04, sia.sub.activity.code == aoi) |>
        dplyr::pull(activity.start.date) |>
        lubridate::as_date() |>
        min()

      #create a super region
      super_region <- dplyr::filter(sia.04, sia.sub.activity.code == aoi) |>
        dplyr::pull(GUID) %>%
        {subset(global.dist, GUID %in% .)} |>
        sf::st_union()

      #find size of super region
      bbox <- sf::st_bbox(super_region)

      length <- bbox |>
        #find points of the bounding box
        matrix() %>%
        {
          tibble::tibble(
            "lon" = c(.[1,1],.[1,1],.[3,1],.[3,1]),
            "lat" = c(.[2,1],.[4,1],.[2,1],.[4,1])
          )
        } |>
        #turn into geom points
        sf::st_as_sf(coords = c("lon", "lat"),
                     crs = 4326, agr = "constant") |>
        #calculate all distances
        sf::st_distance() |>
        max() %>%
        #distance is divided by 4 for buffering (just trial and error)
        {./4} |>
        as.numeric()

      #adjusts buffer distance to max / min
      if(hard_dist){
        length <- dist
      }else{
        length <- dplyr::case_when(
          length < dist_min ~ dist_min,
          length > dist_max ~ dist_max,
          T ~ length
        )
      }


      sf::sf_use_s2(T)

      super_region2 <- sf::st_make_valid(super_region)
      #super_region2 <- st_convex_hull(super_region)

      #create buffer zone
      buffer <- sf::st_buffer(super_region2, length)

      #subset positives by time period convert into points and spatially subset
      #poi = positivies of interest
      poi <- pos |>
        dplyr::filter(!is.na(lon), !is.na(lat),
                      #all cases that could be of interest
                      dateonset > start_date,
                      dateonset <= start_date + day_max) |>
        sf::st_as_sf(coords = c("lon", "lat"),
                     crs = 4326, agr = "constant")

      #all positive cases in super region and > day_min days after activity day
      pos_in_sr <- poi[sf::st_intersects(poi,super_region2, sparse = F),] |>
        dplyr::filter(dateonset >= start_date + day_min) |>
        dplyr::select(epid, dateonset, emergencegroup) |>
        dplyr::mutate(type = "In SIA Region") |>
        unique()

      #all positive cases inside the buffer zone
      pos_in_buffer <- poi[sf::st_intersects(poi,buffer, sparse = F),] |>
        dplyr::select(epid, dateonset, emergencegroup, admin2guid) |>
        dplyr::mutate(type = "In Spatial Buffer") |>
        dplyr::filter(!(epid %in% pos_in_sr$epid)) |>
        unique()

      if(nrow(pos_in_buffer) > 0){
        pos_in_buffer <- {pos_in_buffer[!sf::st_intersects(pos_in_buffer,super_region2,sparse = F),]}

        #if buffer values matched guid from SIA place in buffer

        for(i in pos_in_buffer$epid){
          if(dplyr::filter(pos, epid == i) |>
             dplyr::pull(admin2guid) |>
             unique() %>%
             {. %in% (dplyr::filter(sia.04, sia.sub.activity.code == aoi) |>
                      dplyr::pull(GUID) )}){
            pos_in_sr <- dplyr::bind_rows(pos_in_sr, dplyr::filter(pos_in_buffer, epid == i))
            pos_in_buffer <- dplyr::filter(pos_in_buffer, epid != i)
          }
        }

      }

      #all positives outside the SIA region leading up to activity
      pos_prev_in_buffer <- pos |>
        dplyr::filter(!is.na(lon), !is.na(lat),
                      #all cases that could be of interest
                      dateonset >= start_date - detection_pre_sia_date,
                      dateonset < start_date) |>
        sf::st_as_sf(coords = c("lon", "lat"),
                     crs = 4326, agr = "constant") %>%
        {.[sf::st_intersects(.,buffer, sparse = F),]} |>
        dplyr::select(epid, dateonset, emergencegroup) |>
        dplyr::mutate(type = paste0("In Spatial Buffer -",detection_pre_sia_date," to 0 days")) |>
        unique()

      if(nrow(pos_prev_in_buffer) > 0){
        pos_prev_in_buffer <- {pos_prev_in_buffer[!sf::st_intersects(pos_prev_in_buffer, super_region2, sparse = F),]}

        #if buffer values matched guid from SIA place in buffer

        for(i in pos_prev_in_buffer$epid){
          if(dplyr::filter(pos, epid == i) |>
             dplyr::pull(admin2guid) %>%
             {. %in% (dplyr::filter(sia.04, sia.sub.activity.code == aoi) |>
                      dplyr::pull(GUID) )}){
            pos_in_sr <- dplyr::bind_rows(pos_in_sr, dplyr::filter(pos_prev_in_buffer, epid == i))
            pos_prev_in_buffer <- dplyr::filter(pos_prev_in_buffer, epid != i)
          }
        }

      }



      #if(!plots){
      #bootstrapping the selection range for cases in buffer region
      #   sp_boot_sample <- function(guid, n, reps=100){
      #     filter(global.dist, GUID == guid) %>%
      #       st_sample(n*reps) %>%
      #       st_as_sf() %>%
      #       mutate(id = ceiling(row_number()/n))
      #   }
      #
      #   global.dist2 <- global.dist
      #
      #   sf_use_s2(F)
      #   global.dist2$sr_overlap <- st_intersects(global.dist, buffer, sparse = F)
      #   candidate_regions <- filter(global.dist2, sr_overlap) %>%
      #     filter(!GUID %in% (filter(sia.04, sia.sub.activity.code == aoi) %>%
      #                          pull(GUID)) )
      #
      #   #all cases in the candidate region within our time period of interest
      #   x <- filter(pos, admin2guid %in% unique(candidate_regions$GUID),
      #               !is.na(lon), !is.na(lat),
      #               #all cases that could be of interest
      #               dateonset >= start_date,
      #               dateonset <= start_date + day_min)
      #
      #   if(nrow(x) > 0){
      #     x <- x %>%
      #       group_by(admin2guid) %>%
      #       summarise(count = n()) %>%
      #       filter(count > 0) %>%
      #       {
      #         lapply(1:nrow(.), function(x){
      #           sp_boot_sample(guid = pull(.[x,"admin2guid"]),
      #                          n = pull(.[x,"count"]))
      #         })
      #       } %>%
      #       bind_rows()
      #
      #     x <- x[st_intersects(x,buffer, sparse = F),] %>%
      #       group_by(id) %>%
      #       summarise(count = n()) %>%
      #       pull(count) %>%
      #       {
      #         list(
      #           "min" = min(.),
      #           "max" = max(.),
      #           "prop_zero" = mean(. == 0)
      #         )
      #       }
      #   }else{
      #     x <- list(
      #       "min" = 0,
      #       "max" = 0,
      #       "prop_zero" = 0
      #     )
      #   }
      #
      #
      #   y <- filter(pos, admin2guid %in% unique(candidate_regions$GUID),
      #               !is.na(lon), !is.na(lat),
      #               #all cases that could be of interest
      #               dateonset > start_date + day_min,
      #               dateonset <= start_date + day_max)
      #
      #   if(nrow(y) > 0){
      #     y <- y %>%
      #       group_by(admin2guid) %>%
      #       summarise(count = n()) %>%
      #       filter(count > 0) %>%
      #       {
      #         lapply(1:nrow(.), function(x){
      #           sp_boot_sample(guid = pull(.[x,"admin2guid"]),
      #                          n = pull(.[x,"count"]))
      #         })
      #       } %>%
      #       bind_rows()
      #
      #     y <- y[st_intersects(y,buffer, sparse = F),] %>%
      #       group_by(id) %>%
      #       summarise(count = n()) %>%
      #       pull(count) %>%
      #       {
      #         list(
      #           "min" = min(.),
      #           "max" = max(.),
      #           "prop_zero" = mean(. == 0)
      #         )
      #       }
      #   }else{
      #     y <- list(
      #       "min" = 0,
      #       "max" = 0,
      #       "prop_zero" = 0
      #     )
      #   }
      #
      #
      #   z <- filter(pos, admin2guid %in% unique(candidate_regions$GUID),
      #               !is.na(lon), !is.na(lat),
      #               #all cases that could be of interest
      #               dateonset > start_date + day_min,
      #               dateonset <= start_date + day_max)
      #
      #   if(nrow(z) > 0){
      #     z <- z %>%
      #       group_by(admin2guid) %>%
      #       summarise(count = n()) %>%
      #       filter(count > 0) %>%
      #       {
      #         lapply(1:nrow(.), function(x){
      #           sp_boot_sample(guid = pull(.[x,"admin2guid"]),
      #                          n = pull(.[x,"count"]))
      #         })
      #       } %>%
      #       bind_rows()
      #
      #     z <- z[st_intersects(z,buffer, sparse = F),] %>%
      #       group_by(id) %>%
      #       summarise(count = n()) %>%
      #       pull(count) %>%
      #       {
      #         list(
      #           "min" = min(.),
      #           "max" = max(.),
      #           "prop_zero" = mean(. == 0)
      #         )
      #       }
      #   }else{
      #     z <- list(
      #       "min" = 0,
      #       "max" = 0,
      #       "prop_zero" = 0
      #     )
      #   }
      #
      # }
      #


      #list for plotting output
      plot_output <- list(
        "sia.sub.activity.code" = aoi,
        "cases_in_region" = pos_in_sr,
        "cases_in_buffer" = pos_in_buffer,
        "cases_prev_in_buffer" = pos_prev_in_buffer,
        "sr" = super_region,
        "buffer" = buffer,
        "dist" = length,
        "day_min" = day_min,
        "day_max" = day_max,
        "start_date" = start_date
      )

      if(!plots){
        #primary output of the run
        primary_output <- tibble::tibble(
          "sia.sub.activity.code" = aoi,
          "cases_in_region" = nrow(pos_in_sr),
          "cases_in_buffer_lte28" = dplyr::filter(pos_in_buffer, dateonset <= start_date + day_min) |> nrow(),
          "x_min" = NA, #x$min
          "x_max" = NA, #x$max,
          "x_prop_zero" = NA, #x$prop_zero,
          "cases_in_buffer_gt28" = dplyr::filter(pos_in_buffer, dateonset > start_date + day_min) |> nrow(),
          "y_min" = NA, # y$min,
          "y_max" = NA, # y$max,
          "y_prop_zero" = NA, # y$prop_zero,
          "cases_in_buffer_lt0" = dplyr::filter(pos_prev_in_buffer) |> nrow(),
          "z_min" = NA, # z$min,
          "z_max" = NA, # z$max,
          "z_prop_zero" = NA, # z$prop_zero,
          "dist" = length,
          "day_min" = day_min,
          "day_max" = day_max,
          "start_date" = start_date,
          "updated" = Sys.time()
        )

        #case output
        case_output <- dplyr::bind_rows(
          tibble::tibble(pos_in_sr) |>
            dplyr::select(epid, dateonset, emergencegroup, type) |>
            dplyr::mutate(sia.sub.activity.code = aoi,
                          sia.date = start_date),
          tibble::tibble(pos_in_buffer) |>
            dplyr::select(epid, dateonset, emergencegroup, type) |>
            dplyr::mutate(sia.sub.activity.code = aoi,
                          sia.date = start_date),
          tibble::tibble(pos_prev_in_buffer) |>
            dplyr::select(epid, dateonset, emergencegroup, type) |>
            dplyr::mutate(sia.sub.activity.code = aoi,
                          sia.date = start_date)) |>
          dplyr::select(sia.sub.activity.code, sia.date, epid, dateonset, type, emergencegroup) |>
          dplyr::mutate(updated = Sys.time())

      }

      #determine if plots of data are returned
      if(plots){
        return(plot_output)
      }else{
        #print output files
        tidypolis:::tidypolis_io(io = "write",
                                 obj = bind_rows(tidypolis:::tidypolis_io(io = "read", file_path = paste0(folder, "primary_output.csv")) |>
                                                   dplyr::mutate(dplyr::across(c("cases_in_region", "cases_in_buffer_lte28", "x_min", "x_max", "x_prop_zero",
                                                                                 "cases_in_buffer_gt28", "y_min", ,"y_max", "y_prop_zero", "cases_in_buffer_lt0",
                                                                                 "z_min", "z_max", "z_prop_zero", "dist", "day_min", "day_max"), as.integer),
                                                                 dplyr::across(c("start_date", "updated"), as.Date)), primary_output),
                                 file_path = paste0(folder,"primary_output.csv"))

        #print case files
        tidypolis:::tidypolis_io(io = "write",
                                 obj = bind_rows(tidypolis:::tidypolis_io(io = "read", file_path = paste0(folder, "case_output.csv")) |>
                                                   dplyr::mutate(dplyr::across(c("sia.date", "dateonset", "updated"), as.Date)), case_output),
                                 file_path = paste0(folder,"case_output.csv"))
      }
    },
    error = function(e){
      e$message <- paste0(e$message, " (in ", aoi, ")")
      message(e)
    },
    warning = function(w){
      w$message <- paste0(w$message, " (in ", aoi, ")")
    }
  )
}



#' Expand bbox
#' Function created by Chrisjb, source code at https://rdrr.io/github/Chrisjb/basemapR/man/expand_bbox.html
#' @description
#' A function to take a bounding box (generated using st_bbox) and expand it by x meters in the X direction and y meters in the Y direction.
#' @import sf
#' @param bbox a bounding box generated by sf::st_bbox() or named numeric vector with xmin, ymin, xmax, ymax.
#' @param X the distance in metres that we want to expand the bounding box by in the X direction
#' @param Y the distance in metres that we want to expand the bounding box by in the Y direction
#' @param X2 if specified, the meters in the Easterly direction and metresX becomes metres in the Westerly direction.
#' @param Y2 if specified, the metres to the South. metresY becomes metres to the North.
#' @param crs_out what epsg coordinate system to return the bbox in. Defaults to lat/lng (4326).
#'
#' @return a named numeric vector of bbox extents. This can be converted into an sf object using sf::st_as_sfc(). Coordinates are in lat/lng (4326).
#'
#' @examples
#' library(sf)
#' camden <- dplyr::filter(localauth_data, Name == "Camden") %>%
#'   st_transform(4326)
#' bbox <- expand_bbox(st_bbox(camden), 5000, 5000)
#'
#' library(ggplot2)
#' ggplot() +
#'   base_map(bbox, increase_zoom = 2) +
#'   geom_sf(data = camden, fill = NA) +
#'   coord_sf(
#'     xlim = c(bbox["xmin"], bbox["xmax"]),
#'     ylim = c(bbox["ymin"], bbox["ymax"]), crs = 4326
#'   )
#' @export

expand_bbox <- function(bbox,
                        X,
                        Y,
                        X2 = X,
                        Y2 = Y,
                        crs_out = 4326) {
  bbox <- bbox |>
    sf::st_as_sfc() |>
    sf::st_transform(crs = 4326) |>
    sf::st_bbox()


  bbox["xmin"] <- bbox["xmin"] - (X / 6370000) * (180 / pi) / cos(bbox["xmin"] * pi / 180)
  bbox["xmax"] <- bbox["xmax"] + (X2 / 6370000) * (180 / pi) / cos(bbox["xmax"] * pi / 180)
  bbox["ymin"] <- bbox["ymin"] - (Y / 6370000) * (180 / pi)
  bbox["ymax"] <- bbox["ymax"] + (Y2 / 6370000) * (180 / pi)

  bbox |>
    sf::st_as_sfc() |>
    sf::st_transform(crs = crs_out) |>
    sf::st_bbox()
}


#' @description  Function to generate plots and guarantee folder exists
#' @import ggplot2 cowplot
#' @param plot_folder str:
#' @param plot_name str:
#' @param p plot_object:
#' @param save_plot boolean: use `save_plot` instead of `ggsave`
#' @param ... Pass any plotting specific paramters
f.save.plot <- function(plot_folder,
                        plot_name,
                        p,
                        save_plot,
                        ...){

  .filename <- paste0(plot_folder, "/", plot_name, ".png")

  if(!tidypolis:::tidypolis_io(io = "exists.dir", file_path = plot_folder)){
    tidypolis:::tidypolis_io(io = "create", file_path = plot_folder)
  }

  if(save_plot){
    cowplot::save_plot(plot = p,
              filename = .filename,
              bg = "white",
              ...)
  }else{
    ggplot2::ggsave(
      filename = .filename,
      plot = p,
      bg = "white",
      ...
    )
  }
}


#' @description Generate data for within region analysis
#' @import dplyr
#' @param a variable WHO region in df
#' @param b variable year sia
#' @param c variable round number
#' @param d variable vaccine type
#' @param e variable country name
country_surv_plot <- function(a,
                              b,
                              c,
                              d,
                              e){
  dplyr::filter(dplyr::left_join(case.sia.02, ctry.region.2, by = c("place.admin.0" = "ADM0_NAME")),
         WHO_REGION == a,
         #yr.sia == b,
         round.num == c,
         vaccine.type == d,
         place.admin.0 == e) |>
    dplyr::select(breakthrough.01, breakthrough.02, timetofirstcase) |>
    dplyr::arrange(timetofirstcase) |>
    dplyr::mutate(num.camps = n(),
                  surv.01 = 1 - (cumsum(breakthrough.01)/num.camps),
                  surv.02 = 1 - (cumsum(breakthrough.02)/num.camps)) |>
    dplyr::group_by(timetofirstcase) |>
    dplyr::mutate(surv.01 = min(surv.01),
                  surv.02 = min(surv.02)) |>
    dplyr::ungroup() |>
    dplyr::distinct() |>
    dplyr::select(timetofirstcase, surv.01, surv.02) |>
    dplyr::mutate(timetofirstcase = as.numeric(timetofirstcase)) |>
    dplyr::add_row(timetofirstcase = 0, surv.01 = 1, surv.02 = 1, .before = 1) %>%
    {dplyr::bind_rows(.,
               dplyr::bind_cols(.[2:nrow(.),"timetofirstcase"],
                                .[1:nrow(.)-1,"surv.01"],
                                .[1:nrow(.)-1,"surv.02"]))} |>
    dplyr::arrange(timetofirstcase, -surv.01) |>
    dplyr::bind_cols("WHO_REGION" = a,
                     #"yr.sia" = b,
                     "round.num.sia" = c,
                     "vaccine.type" = d,
                     "ctry" = e)
}

#' @description
#' a function to create survival plots based on first breakthrough cases
#' @import dplyr
#' @param region str WHO region to create survival plots for
#' @param folder str folder location to save plots
#' @param plot_data tibble df to use to craete surv plots
#' @param breakthrough_min_date int minimum days after SIA to be considered breakthrough
#' @param breakthrough_middle_date int number of days to set cutoff between early and late breakthrough
#' @param breakthrough_max_date int maximum number of days a case could be considered breakthrough
surv_plot_func <- function(region,
                           folder = paste0(Sys.getenv("SIA_FOLDER"), "/outputs"),
                           plot_data,
                           breakthrough_min_date = load_parameters()$breakthrough_min_date,
                           breakthrough_middle_date = load_parameters()$breakthrough_middle_date,
                           breakthrough_max_date = load_parameters()$breakthrough_max_date){

  tmp <- plot_data %>%
    mutate(ctry = case_when(
      ctry == "Democratic Republic of the Congo" ~ "DRC",
      ctry == "Central African Republic" ~ "CAR",
      T ~ ctry
    )) %>%
    filter(timetofirstcase <= breakthrough_max_date,WHO_REGION == region) %>%
    mutate(ctry = factor(ctry))

  a <- filter(tmp, round.num.sia == 1) %>%
    ggplot(aes(x = timetofirstcase, y = surv.01, group = vaccine.type, color = vaccine.type)) +
    #geom_point(alpha = 0.5, fill = NA) +
    geom_point() +
    geom_line() +
    facet_wrap(~ctry, drop = F, ncol = 3) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = color.surv.plots) +
    labs(x = "Days to first case", y = "Percent of campaigns that succeeded",
         shape = paste0("Failed before ",breakthrough_middle_date," days"), color = "Vaccine\nType",
         title = "Round 1") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = c(0.8,1))

  b <- filter(tmp, round.num.sia == 2) %>%
    ggplot(aes(x = timetofirstcase, y = surv.01, group = vaccine.type, color = vaccine.type)) +
    #geom_point(alpha = 0.5, fill = NA) +
    geom_point() +
    geom_line() +
    facet_wrap(~ctry, drop = F, ncol = 3) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = color.surv.plots) +
    labs(x = "Days to first case", y = "Percent of campaigns that succeeded",
         shape = paste0("Failed before ",breakthrough_middle_date," days"), color = "Vaccine\nType",
         title = "Round 2") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = c(0.8,1))

  figure <- ggarrange(a,b,
                      common.legend = T,
                      #labels = NULL, ncol = 2,
                      legend = "bottom", align = "hv",
                      font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

  p <- annotate_figure(figure, top = text_grob(region, color = "black", face = "bold", size = 14) ,
                       bottom = text_grob(paste0("Failures defined as those ",breakthrough_min_date," - ",breakthrough_max_date," days from activity"), color = "black",
                                          hjust = 1, x = 1, face = "italic", size = 10))

  if(region == "AFRO"){
    ggsave(paste0(folder, "/", region, ".png"), width = 11, height = 20, plot = p, bg = "white")
  }else{
    ggsave(paste0(folder, "/", region, ".png"), width = 11, height = 8.5, plot = p, bg = "white")
  }

  print(paste0("Saved plot to ", paste0(folder, "/", region, ".png")))

}
