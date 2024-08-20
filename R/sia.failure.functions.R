##              SIA Failure Functions                                        ###
##              Writen by Stephanie Kovacs and Nishant Kishore               ###
##              updated by Nick Heaghney spring 2023                         ###
##              updated by Nick Heaghney Summer 2024                         ###


#### Set up SIA Impact Report  ####

#initiate sia impact reprot function
#this function initializes a folder structure for the purpose of running the sia impact report
#' @export
#' @import package
#' @param folder_loc str: location of folder to set up and run SIA impact report
init_sia_impact <- function(folder_loc){

  if(dir.exists(folder_loc)){

  }else{
    dir.create(folder_loc)
    dir.create(paste0(folder_loc, "/assets"))
    dir.create(paste0(folder_loc, "/assets/cache"))
    dir.create(paste0(folder_loc, "/assets/donut_maps"))
    dir.create(paste0(folder_loc, "/assets/resources"))
  }

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
                            type = "reg"){
  print("----CLEANING CASE DATA----")

  if(type == "reg"){

    out <- case.data |>
      dplyr::filter(dateonset >= start.date & dateonset <= end.date,
                    measurement %in% .measurement) |>
      dplyr::mutate(place.admin.0 = ifelse(place.admin.0 == "CÔTE D’IVOIRE", "COTE D IVOIRE", place.admin.0)) |>
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
#' @import dplyr stats
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
      {clusGap(., FUN = stats::kmeans, nstart = 25, K.max = max(min(nrow(.)-1, nrow(.)/2), 2), B = 100)} %>%
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
#' @import dplyr
#' @param data df dataframe on which to run cluster dates function
#' @param min_obs int
#' @param type str vaccine type
run_cluster_dates <- function(data,
                              min_obs = 4,
                              type){

  #check which locations meet minimum obs requirements
  in_data <- data |>
    dplyr::filter(vaccine.type == type) |>
    dplyr::group_by(adm2guid) |>
    dplyr::summarize(count = n())

  #check if cache exists
  cache_exists <- file.exists(here("sia_impact_pipeline","assets","cache",paste0(type,"_cluster_cache.rds")))

  if(cache_exists){
    cache <- read_rds(here("sia_impact_pipeline","assets","cache",paste0(type,"_cluster_cache.rds")))
    in_data <- setdiff(in_data, cache)

    print(paste0(nrow(in_data), " potentially new SIAs in [",type,"] found for clustering analysis"))

    #drop cache rows where the adm2guid is in in_data with a different count
    cache <- cache %>%
      filter(!(adm2guid %in% in_data$adm2guid))

    bind_rows(in_data, cache) %>%
      write_rds(here("sia_impact_pipeline","assets","cache",paste0(type,"_cluster_cache.rds")))
  }else{
    print(paste0("No cache found for [", type, "], creating cache and running clustering for ", nrow(in_data), " SIAs"))
    write_rds(in_data, here("sia_impact_pipeline","assets","cache",paste0(type,"_cluster_cache.rds")))
  }

  if(nrow(in_data) > 0){
    print("Clustering new SIA data")
    in_data <- in_data %>%
      filter(count >= min_obs)

    included <- data %>%
      filter(vaccine.type == type) %>%
      filter(adm2guid %in% in_data$adm2guid)

    #observations which didn't meet the minimum requirement
    dropped <- setdiff(filter(data, vaccine.type == type), included)

    #for data with at least a minimum number of observations
    out <- ungroup(included) %>%
      group_by(adm2guid) %>%
      group_split() %>%
      #apply function to each subset
      lapply(cluster_dates) %>%
      #bind output back together
      bind_rows()

    #error checking for situations where no data < min_obs
    if(nrow(dropped) > 0){
      #for data with low obs
      out2 <- ungroup(dropped) %>%
        group_by(adm2guid) %>%
        group_split() %>%
        lapply(function(x) cluster_dates(x, method = "mindate")) %>%
        bind_rows()
    }


    #error catching the return
    if(nrow(dropped) > 0){

      out <- bind_rows(out, out2)
    }

    #data cache
    data_cache_exists <- file.exists(here("sia_impact_pipeline","assets","cache",paste0(type,"data_cluster_cache.rds")))

    if(data_cache_exists){
      data_cache <- read_rds(here("sia_impact_pipeline","assets","cache",paste0(type,"data_cluster_cache.rds")))

      out <- filter(data_cache, !sia.sub.activity.code %in% unique(out$sia.sub.activity.code)) %>%
        bind_rows(out)
      # data_cache2 <- data_cache %>%
      #   anti_join(out, by=c("sia.sub.activity.code", "adm2guid"))
      # out <- data_cache2 %>%
      #   bind_rows(out)

      write_rds(out, here("sia_impact_pipeline","assets","cache",paste0(type,"data_cluster_cache.rds")))
    }else{
      print(paste0("No data cache found for [", type, "], creating data cache and saving clustering results for ", nrow(out), " SIAs"))
      write_rds(out, here("sia_impact_pipeline","assets","cache",paste0(type,"data_cluster_cache.rds")))
    }




  }else{
    print(paste0("No new SIA data found for [", type, "], loading cached data!"))
    out <- read_rds(here("sia_impact_pipeline","assets","cache",paste0(type,"data_cluster_cache.rds")))
  }

  return(out)


}
