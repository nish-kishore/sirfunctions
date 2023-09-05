
#' Pull Polio data from S Drive
#'
#' @name f.pull.polio.data
#' @description Calculate the NPAFP rate from POLIS data
#' @export
f.pull.polio.data <- function(

){


}


load_clean_dist_sp <- function(fp = file.path('', '', 'cdc.gov', 'project', 'CGH_GID_Active', 'PEB',
                                              'SIR', 'DATA', 'Core 2.0', 'datafiles_01', 'shapefiles_01',
                                              'WHO_POLIO_GLOBAL_GEODATABASE.gdb'),
                               dist_guid = NULL,
                               ctry_name = NULL,
                               end.year = year(Sys.Date()),
                               st.year = 2000,
                               data.only = F,
                               type = NULL){
  out <- fp %>%
    st_read(
      layer = "GLOBAL_ADM2",
      quiet = T
    ) %>%
    mutate(

      STARTDATE = as.Date(STARTDATE),
      # Typo in the dist start date (year) in shapefiles. Temporary correcting the start date for South Darfur in Sudan
      STARTDATE = if_else(ADM0_GUID =="{3050873E-F010-4C4F-82D1-541E3C4FD887}" & ADM1_GUID =="{0836D898-32B9-4912-AEA2-D07BD6E50ED8}"
                          & STARTDATE=='2018-01-01',
                          STARTDATE+365, STARTDATE),

      # Error in shapes of LAR district in FARS province of IRAN.
      # Received confirmation from WHO - Start date should be '2021-01-01'.
      # Manually making corrections until WHO fix it in the original geodatabase.
      STARTDATE = if_else(ADM0_GUID =="{2EEA3A5C-8A36-4A18-A7AB-1B927A092A60}" & ADM1_GUID =="{76F33E17-ADB9-4582-A533-4C96286864E3}" &
                            GUID == "{54464216-2BD3-4F30-BF2C-3846BEE6805D}" & STARTDATE=='2020-01-01',
                          STARTDATE+366, STARTDATE),

      yr.st = year(STARTDATE),
      yr.end = year(ENDDATE),
      ADM0_NAME = ifelse(str_detect(ADM0_NAME, "IVOIRE"),"COTE D IVOIRE", ADM0_NAME)
    ) %>%

    # remove the ouad eddahab in Morocco which started and ended the same year and causes overlap
    filter(!GUID=="{AE526BC0-8DC3-411C-B82E-75259AD3598C}") %>%
    # this filters based on dates set in RMD
    filter(yr.st <= end.year & (yr.end >= st.year | yr.end == 9999)) %>%
    {
      if(is.null(dist_guid)){.}else{
        filter(., GUID %in% dist_guid)
      }
    } %>%
    {
      if(is.null(ctry_name)){.}else{
        filter(., ADM0_NAME %in% ctry_name)
      }
    }

  if(data.only & is.null(type)){
    out <- tibble(out)

    return(out)
  }

  if(!data.only & is.null(type)){
    return(out)
  }

  if(ifelse(!is.null(type), type == "long", F)){

    df.list <- list()

    for (i in st.year:end.year) {
      df.list <- c(df.list, list(i = f.yrs.01(out, i)))
    }

    return(do.call(rbind, df.list))
  }

}

load_clean_prov_sp <- function(fp = file.path('', '', 'cdc.gov', 'project', 'CGH_GID_Active', 'PEB',
                                              'SIR', 'DATA', 'Core 2.0', 'datafiles_01', 'shapefiles_01',
                                              'WHO_POLIO_GLOBAL_GEODATABASE.gdb'),
                               prov_guid = NULL,
                               prov_name = NULL,
                               ctry_name = NULL,
                               end.year = year(Sys.Date()),
                               st.year = 2000,
                               data.only = F,
                               type = NULL){
  out <- fp %>%
    st_read(
      layer = "GLOBAL_ADM1",
      quiet = T
    ) %>%
    mutate(
      yr.st = year(STARTDATE),
      yr.end = year(ENDDATE),
      ADM0_NAME = ifelse(str_detect(ADM0_NAME, "IVOIRE"),"COTE D IVOIRE", ADM0_NAME)
    ) %>%
    # this filters based on dates set in RMD
    filter(yr.st <= end.year & (yr.end >= st.year | yr.end == 9999)) %>%
    {
      if(is.null(prov_guid)){.}else{
        filter(., GUID %in% prov_guid)
      }
    } %>%
    {
      if(is.null(ctry_name)){.}else{
        filter(., ADM0_NAME %in% ctry_name)
      }
    } %>%
    {
      if(is.null(prov_name)){.}else{
        filter(., ADM1_NAME %in% prov_name)
      }
    }

  if(data.only & is.null(type)){
    out <- tibble(out)

    return(out)
  }
  if(!data.only & is.null(type)){
    return(out)
  }
  if(ifelse(!is.null(type), type == "long", F)){
    df.list <- list()

    for (i in st.year:end.year) {
      df.list <- c(df.list, list(i = f.yrs.01(out, i)))
    }

    return(do.call(rbind, df.list))
  }

}

load_clean_ctry_sp <- function(fp = file.path('', '', 'cdc.gov', 'project', 'CGH_GID_Active', 'PEB',
                                              'SIR', 'DATA', 'Core 2.0', 'datafiles_01', 'shapefiles_01',
                                              'WHO_POLIO_GLOBAL_GEODATABASE.gdb'),
                               ctry_guid = NULL,
                               ctry_name = NULL,
                               end.year = year(Sys.Date()),
                               st.year = 2000,
                               data.only = F,
                               type = NULL){
  out <- fp %>%
    st_read(
      layer = "GLOBAL_ADM0",
      quiet = T
    ) %>%
    mutate(
      yr.st = year(STARTDATE),
      yr.end = year(ENDDATE),
      ADM0_NAME = ifelse(str_detect(ADM0_NAME, "IVOIRE"),"COTE D IVOIRE", ADM0_NAME)
    ) %>%
    # this filters based on dates set in RMD
    filter(yr.st <= end.year & (yr.end >= st.year | yr.end == 9999)) %>%
    {
      if(is.null(ctry_guid)){.}else{
        filter(., GUID %in% ctry_guid)
      }
    } %>%
    {
      if(is.null(ctry_name)){.}else{
        filter(., ADM0_NAME %in% ctry_name)
      }
    }

  if(data.only & is.null(type)){
    out <- as_tibble(out)

    return(out)
  }

  if(!data.only & is.null(type)){
    return(out)
  }

  if(ifelse(!is.null(type), type == "long", F)){
    df.list <- list()
    for (i in st.year:end.year) {
      df.list <- c(df.list, list(i = f.yrs.01(out, i)))
    }
    return(bind_rows(df.list))
  }

}
