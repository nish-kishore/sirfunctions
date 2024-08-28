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
    tidypolis:::tidypolis_io(io = "create", file_path = paste0(folder_loc, "/assets/resources"))
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

      out <- filter(data_cache, !sia.sub.activity.code %in% unique(out$sia.sub.activity.code)) %>%
        bind_rows(out)
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
    dply::distinct() |>

    #any early break through of transmission
    dplyr::mutate(breakthrough.01 = ifelse(num.case28.180days > 0, 1, 0),
                  #transmission 180-365 days
                  breakthrough.02 = ifelse(num.case180.365 > 0, 1, 0)) |>
    dplyr::full_join(calc_first_break_case(case.sia.01, breakthrough_min_date),
                     by = c("adm2guid", "sia.sub.activity.code"))

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
