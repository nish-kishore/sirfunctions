##              SIA Failure Functions                                        ###
##              Writen by Stephanie Kovacs and Nishant Kishore               ###
##              updated by Nick Heaghney spring 2023                         ###
##              updated by Nick Heaghney Summer 2024                         ###


#### Set up SIA Impact Report  ####

#initiate sia impact reprot function
#this function initializes a folder structure for the purpose of running the sia impact report
#' @export
#' @param folder_loc str: location of folder to set up and run SIA impact report
init_sia_impact <- function(folder_loc){

  if(dir.exists(folder_loc)){

  }else{
    dir.create(folder_loc)
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
pull_clean_sia_data <- function(sia.data,
                                start.date=as.Date("2016-01-01"),
                                end.date = Sys.Date(),
                                method = 'reg'){
  if(method == "reg"){
    print("----BEGINNING SIA DATA CLEANING----")
    print("[0/3]-Starting cleaning steps")
    tick <- Sys.time()
    # read in SIA files

    sia.clean.01 <- sia.data |>
      filter(activity.start.date >= start.date & activity.end.date<=end.date)

    sia.02 <- sia.clean.01 %>%
      #select variables of interest to ease error checking
      select(sia.sub.activity.code,
             status, phase, im.loaded, lqas.loaded, vaccine.type,
             sub.activity.start.date, adm2guid,
             linked.obx,
             # activity.comments,
             yr.sia, place.admin.0,
             place.admin.1, place.admin.2,sub.activity.end.date,
             vaccine.type, adm0guid, adm1guid, `admin.coverage.%`)


    print("[1/3]-Removing campaigns that did not occur")
    tick <- Sys.time()

    sia.02 <- sia.02 %>%
      #first step is to get rid of campaigns that did not occur
      #Unconfirmed was used pre-2014 for some older campaigns
      #missing status for campaigns 2004-2017 will be assigned as unconfirmed\
      mutate(
        complete.camp = case_when(
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
                           vaccine.type == "IPV + bOPV", 1, 0)) %>%
      filter(complete.camp == "Completed" |
               complete.camp == "Unconfirmed")

    tock <- Sys.time()

    print(tock - tick)

    print("[2/3]-Removing duplicates")

    tick <- Sys.time()

    sia.02 <- sia.02 %>%

      #Second step is to remove duplicates:

      mutate(sub.activity.start.date = as_date(sub.activity.start.date)) %>%
      arrange(sub.activity.start.date) %>%
      group_by(adm2guid, vaccine.type) %>%
      #this creates variable that is days from last campaign of that vaccine
      mutate(camp.diff.days = as.numeric(sub.activity.start.date - lag(sub.activity.start.date)))%>%
      ungroup() %>%

      #identify SIAs that are duplicated because there are no difference in campaign days
      mutate(dup = case_when(
        camp.diff.days == 0 & !is.na(adm2guid) ~ 1,
        camp.diff.days > 0 | is.na(adm2guid) == T | is.na(camp.diff.days) == T ~ 0)) %>%
      #remove duplicates
      filter(dup != 1) %>%
      #manually removing extra duplicates
      #same date, vaccine and age range as another campaign in same dist
      filter(sia.sub.activity.code!="PAK-2021-006-1") %>%
      #this one no IM
      filter(sia.sub.activity.code!="SOM-2000-002-2")

    tock <- Sys.time()

    print(tock - tick)


    print("[3/3]-Final cleaning steps")


    sia.reg <- sia.02%>%
      filter(vaccine.type=="mOPV2" | vaccine.type=="nOPV2" | (vaccine.type=="tOPV" & sub.activity.start.date>"2017-01-01") | vaccine.type == "bOPV") %>%
      select(sia.sub.activity.code, place.admin.0, place.admin.1, place.admin.2, sub.activity.start.date, sub.activity.end.date, vaccine.type, adm0guid, adm1guid, adm2guid,
             yr.sia,
             # num.dist.incamp,
             # vac.round.num.count,
             # vac.total.rounds,
             # opv2.round.count,
             # total.opv2.rounds,
             #linked.obx, obx1, obx2, obx3, obx4,
             `admin.coverage.%`)

    return(sia.reg)
  }

  if(method == "planned"){
    print("----BEGINNING SIA DATA PULL / CLEANING----")
    tick <- Sys.time()
    # read in SIA files
    sia.clean.01 <- sia.data |>
      filter(activity.start.date >= Sys.Date())

    tock <- Sys.time()

    print(paste0("Data loaded successfully!"))
    print(paste0(nrow(sia.clean.01), " records loaded in ", as.numeric(tock-tick),
                 " seconds."))

    sia.02 <- sia.clean.01 %>%
      #select variables of interest to ease error checking
      select(sia.sub.activity.code,
             status, phase, im.loaded, lqas.loaded, vaccine.type,
             sub.activity.start.date, adm2guid,
             linked.obx,
             # activity.comments,
             yr.sia, place.admin.0,
             place.admin.1, place.admin.2,sub.activity.end.date,
             vaccine.type, adm0guid, adm1guid, `admin.coverage.%`)

    print("[0/3]-Starting cleaning steps")

    print("[1/3]- subsetting to planned campaigns")
    tick <- Sys.time()

    sia.02 <- sia.02 %>%
      #first step is to get rid of campaigns that did not occur
      #Unconfirmed was used pre-2014 for some older campaigns
      #missing status for campaigns 2004-2017 will be assigned as unconfirmed\
      mutate(
        planned.camp = ifelse(phase %in% c("Forecasted", "Planned") &
                                status != "Canceled", "Planned", NA),
        complete.camp = case_when(
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
                           vaccine.type == "IPV + bOPV", 1, 0)) %>%
      filter(planned.camp == "Planned" &
               year(Sys.Date()) <= yr.sia)

    tock <- Sys.time()

    print(tock - tick)

    print("[2/3]-Removing duplicates")

    tick <- Sys.time()

    sia.02 <- sia.02 %>%

      #Second step is to remove duplicates:

      mutate(sub.activity.start.date = as_date(sub.activity.start.date)) %>%
      arrange(sub.activity.start.date) %>%
      group_by(adm2guid, vaccine.type) %>%
      #this creates variable that is days from last campaign of that vaccine
      mutate(camp.diff.days = as.numeric(sub.activity.start.date - lag(sub.activity.start.date)))%>%
      ungroup() %>%

      #identify SIAs that are duplicated because there are no difference in campaign days
      mutate(dup = case_when(
        camp.diff.days == 0 & !is.na(adm2guid) ~ 1,
        camp.diff.days > 0 | is.na(adm2guid) == T | is.na(camp.diff.days) == T ~ 0)) %>%
      #remove duplicates
      filter(dup != 1) %>%
      #manually removing extra duplicates
      #same date, vaccine and age range as another campaign in same dist
      filter(sia.sub.activity.code!="PAK-2021-006-1") %>%
      #this one no IM
      filter(sia.sub.activity.code!="SOM-2000-002-2")

    tock <- Sys.time()

    print(tock - tick)


    print("[3/3]-Final cleaning steps")


    sia.planned <- sia.02 %>%
      # filter(vaccine.type=="mOPV2" | vaccine.type=="nOPV2" | (vaccine.type=="tOPV" & sub.activity.start.date>"2017-01-01") | vaccine.type == "bOPV") %>%
      select(sia.sub.activity.code, place.admin.0, place.admin.1, place.admin.2, sub.activity.start.date, sub.activity.end.date, vaccine.type, adm0guid, adm1guid, adm2guid,
             yr.sia, status, phase,
             # num.dist.incamp,
             # vac.round.num.count,
             # vac.total.rounds,
             # opv2.round.count,
             # total.opv2.rounds,
             #linked.obx, obx1, obx2, obx3, obx4,
             `admin.coverage.%`)


    return(sia.planned)
  }

  if(method == "ipv"){
    print("----BEGINNING SIA DATA PULL / CLEANING----")
    tick <- Sys.time()
    print("Connecting to S drive to pull latest data...")
    # read in SIA files
    sia.clean.01 <- sia.data |>
      filter(activity.start.date >= start.date)

    tock <- Sys.time()

    print(paste0("Data loaded successfully!"))
    print(paste0(nrow(sia.clean.01), " records loaded in ", as.numeric(tock-tick),
                 " seconds."))

    sia.02 <- sia.clean.01 %>%
      #select variables of interest to ease error checking
      select(sia.sub.activity.code,
             status, phase, im.loaded, lqas.loaded, vaccine.type,
             sub.activity.start.date, adm2guid,
             linked.obx,
             # activity.comments,
             yr.sia, place.admin.0,
             place.admin.1, place.admin.2,sub.activity.end.date,
             vaccine.type, adm0guid, adm1guid, `admin.coverage.%`)

    print("[0/3]-Starting cleaning steps")

    print("[1/3]-Removing campaigns that did not occur")
    tick <- Sys.time()

    sia.02 <- sia.02 %>%
      #first step is to get rid of campaigns that did not occur
      #Unconfirmed was used pre-2014 for some older campaigns
      #missing status for campaigns 2004-2017 will be assigned as unconfirmed\
      mutate(
        complete.camp = case_when(
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
                           vaccine.type == "IPV + bOPV", 1, 0)) %>%
      filter(complete.camp == "Completed" |
               complete.camp == "Unconfirmed")

    tock <- Sys.time()

    print(tock - tick)

    print("[2/3]-Removing duplicates")

    tick <- Sys.time()

    sia.02 <- sia.02 %>%

      #Second step is to remove duplicates:

      mutate(sub.activity.start.date = as_date(sub.activity.start.date)) %>%
      arrange(sub.activity.start.date) %>%
      group_by(adm2guid, vaccine.type) %>%
      #this creates variable that is days from last campaign of that vaccine
      mutate(camp.diff.days = as.numeric(sub.activity.start.date - lag(sub.activity.start.date)))%>%
      ungroup() %>%

      #identify SIAs that are duplicated because there are no difference in campaign days
      mutate(dup = case_when(
        camp.diff.days == 0 & !is.na(adm2guid) ~ 1,
        camp.diff.days > 0 | is.na(adm2guid) == T | is.na(camp.diff.days) == T ~ 0)) %>%
      #remove duplicates
      filter(dup != 1) %>%
      #manually removing extra duplicates
      #same date, vaccine and age range as another campaign in same dist
      filter(sia.sub.activity.code!="PAK-2021-006-1") %>%
      #this one no IM
      filter(sia.sub.activity.code!="SOM-2000-002-2")

    tock <- Sys.time()

    print(tock - tick)


    print("[3/3]-Final cleaning steps")


    sia.ipv <- sia.02%>%
      filter(vaccine.type=="IPV" | vaccine.type=="f-IPV" | vaccine.type=="IPV + bOPV") %>%
      select(sia.sub.activity.code, place.admin.0, place.admin.1, place.admin.2, sub.activity.start.date, sub.activity.end.date, vaccine.type, adm0guid, adm1guid, adm2guid,
             yr.sia,
             # num.dist.incamp,
             # vac.round.num.count,
             # vac.total.rounds,
             # opv2.round.count,
             # total.opv2.rounds,
             #linked.obx, obx1, obx2, obx3, obx4,
             `admin.coverage.%`)

    return(sia.ipv)
  }
}


#Cluster Function
#this function identifies "cluster" or OBX response so we can identify rounds
#' @export
#' @param x df: data to be clustered
#' @param seed
#' @param method
#' @param grouping_days int:
cluster_dates <- function(x,
                          seed = 1234,
                          method = "kmeans",
                          grouping_days = 365){

  if(method == "kmeans"){
    #prepare the data
    y <- x %>%
      #select only dates
      select(date = sub.activity.start.date) %>%
      #calculate distance from minimum date
      mutate(date = as.numeric(date - min(date))) %>%
      #normalize values for clustering
      scale()

    set.seed(seed)
    #calculate the optimal number of clusters
    optim_k <- y %>%
      #calculate optimal number of clusters using the
      #gap statistic
      {clusGap(., FUN = kmeans, nstart = 25, K.max = max(min(nrow(.)-1, nrow(.)/2), 2), B = 100)} %>%
      #extract gap statistic matrix
      {.$Tab[,"gap"]} %>%
      #calculate the max gap statistic, given the sparsity in the data
      #am not limiting to the first max SE method
      which.max()

    set.seed(seed)
    #calculate the clusters
    x$cluster <- kmeans(y, optim_k)$cluster %>%
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
      x <- x %>%
        mutate(cluster = as.numeric(sub.activity.start.date - min(sub.activity.start.date)),
               cluster = cut(cluster, seq(0, grouping_days*6, by = grouping_days), include.lowest = T),
               cluster = as.numeric(cluster),
               cluster_method = method)

      return(x)
    }
  }



}
