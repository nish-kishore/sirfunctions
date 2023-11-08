#DAL - Data Access Layer

#### 1) Utility functions ####

#' Validate connection to EDAV
#'
#' @description Validate connection to CDC EDAV
#' @import AzureStor AzureAuth
#' @returns azure container verification
get_azure_storage_connection <- function(){
  mytoken <- AzureAuth::get_azure_token(
    resource = "https://storage.azure.com/",
    tenant = "9ce70869-60db-44fd-abe8-d2767077fc8f",
    app = "04b07795-8ddb-461a-bbee-02f9e1bf7b46",
    auth_type = "authorization_code"
  )

  token_hash <- AzureAuth::list_azure_tokens() |> names()
  token_hash <- token_hash[1]
  mytoken <- AzureAuth::load_azure_token(token_hash)


  endptoken <- AzureStor::storage_endpoint(endpoint = "https://davsynapseanalyticsdev.dfs.core.windows.net", token = mytoken)

  azcontainer <- AzureStor::storage_container(endptoken, "ddphsis-cgh")

  return(azcontainer)
}

#' Helper function to read and write key data
#'
#' @description Helper function read and write key data to EDAV
#' @import cli
#' @param io str: "read", "write" or "list"
#' @param file_loc str: location to "read", "write" or "list"
#' @param obj default NULL object to be saved
#' @param azcontainer azure container object
#' @returns tibble
edav_io <- function(
    io,
    file_loc,
    obj = NULL,
    azcontainer = get_azure_storage_connection()
){

  opts <- c("read", "write", "delete", "list")

  if(!io %in% opts){
    stop("io: must be 'read', 'write', 'delete' or 'list'")
  }

  if(io == "write" & is.null(obj)){
    stop("Need to supply an object to be written")
  }

  if(io == "list"){

    if(!AzureStor::storage_dir_exists(azcontainer, file_loc)){
      stop("Directory does not exist")
    }

    return(AzureStor::list_storage_files(azcontainer, file_loc) |>
      tibble::as_tibble())

  }

  if(io == "read"){

    if(!AzureStor::storage_file_exists(azcontainer, file_loc)){
      stop("File does not exist")
    }

    if(grepl(".rds", file_loc)){
      return(AzureStor::storage_load_rds(azcontainer, file_loc))
    }

    if(grepl(".csv", file_loc)){
      return(AzureStor::storage_read_csv(azcontainer, file_loc))
    }

  }

  if(io == "write"){

    if(grepl(".rds", file_loc)){
      AzureStor::storage_save_rds(object = obj, container = azcontainer, file = file_loc)
    }

    if(grepl(".csv", file_loc)){
      AzureStor::storage_write_csv(object = obj, container = azcontainer, file = file_loc)
    }

  }

  if(io == "delete"){

    if(!AzureStor::storage_file_exists(azcontainer, file_loc)){
      stop("File does not exist")
    }

    AzureStor::delete_storage_file(azcontainer, file_loc, confirm = F)

  }

}

#' Test network connection to the S drive
#'
#' @description Tests upload and download from S drive
#' @param azcontainer azure storage container
#' @param folder str: Location of download folder in S drive
#' @param test_size int: byte size of a theoretical folder
#' @param return_list boolean: return a list of download time estimates, default F
#' @import dplyr readr prettyunits tibble cli
#' @returns System message with download and update time, potentially list output
#' @export
test_EDAV_connection <- function(
    azcontainer = get_azure_storage_connection(),
    folder = "GID/PEB/SIR/Data",
    return_list = F,
    test_size = 10000000
){

  tmp_data <- replicate(100, iris, simplify = F) |>
    dplyr::bind_rows() |>
    tibble::as_tibble()

  tmp_file <- paste0(tempfile(),".rds")

  readr::write_csv(tmp_data, tmp_file)

  file_size <- file.info(tmp_file)$size

  tick <- Sys.time()

  edav_io(io = "write", file_loc = paste0(folder, "/tmp.rds"), obj = tmp_data)
  #readr::write_rds(tmp_data, paste0(folder, "/tmp.rds"))

  tock <- Sys.time()

  dt1 <- as.numeric(difftime(tock, tick, units = "secs"))

  tick <- Sys.time()

  x <- edav_io(io = "read", file_loc = paste0(folder, "/tmp.rds"))
  #x <- readr::read_rds(paste0(folder, "/tmp.rds"))

  tock <- Sys.time()

  dt2 <- as.numeric(difftime(tock, tick, units = "secs"))

  cli::cli_alert_success(c(
    "Uploaded {prettyunits::pretty_bytes(file_size)} in ",
    "{prettyunits::pretty_sec(dt1)}"
  ))
  cli::cli_alert_success(c(
    "Downloaded {prettyunits::pretty_bytes(file_size)} in ",
    "{prettyunits::pretty_sec(dt2)}"
  ))

  suppressMessages(edav_io(io = "delete", file_loc = paste0(folder, "/tmp.rds")), classes = c("message", "warning"))
  #x <- file.remove(paste0(folder, "/tmp.rds"))

  dt3 <- test_size/file_size*dt1
  dt4 <- test_size/file_size*dt2

  cli::cli_alert_info(c(
    "For a {prettyunits::pretty_bytes(test_size)} file we would expect the following times:"
  ))

  cli::cli_alert_info("Upload: {prettyunits::pretty_sec(dt3)}")
  cli::cli_alert_info("Download: {prettyunits::pretty_sec(dt4)}")

  if(return_list){
    return(list("size" = file_size, "u" = dt1, "d" = dt2))
  }

}

#### 2) Key data pull functions ####


#' Retrieve all pre-processed Polio Data
#'
#' @description Download all Polio data from CDC pre-processed endpoint
#' @import dplyr cli sf tibble
#' @param folder str: location of the CDC pre-processed endpoint
#' @returns named list containing polio data that is relevant to CDC
#' @export
get_all_polio_data <- function(
    folder = "GID/PEB/SIR/Data/"
){

  cli::cli_h1("Testing download times")


  download_metrics <- test_EDAV_connection(return_list = T)


  dl_table <- c(
    file.path(
      folder,
      "spatial",
      "global.ctry.rds"
    ),
    file.path(
      folder,
      "spatial",
      "global.prov.rds"
    ),
    file.path(
      folder,
      "spatial",
      "global.dist.rds"
    ),
    list.files(
      path = file.path(
        folder,
        "all_year_all_var_files_20220909"
      ),
      pattern = "^.*(afp).*(linelist).*(.rds)$",
      full.names = TRUE
    ),
    file.path(
      folder,
      "ctry.pop.2000_2023.rds"
    ),
    file.path(
      folder,
      "prov.pop.long2010_2023.rds"
    ),
    file.path(
      folder,
      "dist.pop.long2010_2023.rds"
    ),
    file.path(
      folder,
      "coverage",
      "dpt_district_summaries.rda"
    ),
    file.path(
      folder,
      "coverage",
      "mcv1_district_summaries.rda"
    ),
    file.path(
      folder,
      "all_year_all_var_files_20220909"
    ) |>
      list.files(pattern = "^(es).*(.rds)$", full.names = TRUE),
    file.path(
      folder,
      "all_year_all_var_files_20220909"
    ) |>
      list.files(pattern = "^.*(sia).*(.rds)$", full.names = TRUE),
    file.path(
      folder,
      "all_year_all_var_files_20220909"
    ) |>
      list.files(pattern = "^(positives).*(.rds)$", full.names = TRUE)
  ) |>
    lapply(function(x){
      tibble::tibble(
        "file" = strsplit(x, "/")[[1]] %>% {.[length(.)]},
        "size" = file.size(x),
        "dl_time_sec" = file.size(x)/download_metrics$size*download_metrics$d/16
      )
    }) |>
    bind_rows()

  file_size <- dl_table$size |> sum()
  download_time <- dl_table$dl_time_sec |> sum()

  cli::cli_alert_info(c(
    "Estimated MAX download time for {prettyunits::pretty_bytes(file_size)} is ",
    "{prettyunits::pretty_sec(download_time)}"
  ))

  x <- readline("Do you want to start download? [Y/N]: ")

  if(grepl("y", tolower(x), fixed = T)){

    cli::cli_h1("Downloading POLIS Data")

    raw.data <- list()

    cli::cli_process_start("1) Loading country shape files from S Drive")

    #loading spatial data
    raw.data$global.ctry <-
      file.path(
        folder,
        "shapefiles_01",
        "global.ctry.rds"
      ) |>
      readr::read_rds()


    cli::cli_process_done()



    cli::cli_process_start("2) Loading province shape files from S Drive")
    raw.data$global.prov <-
      file.path(
        folder,
        "shapefiles_01",
        "global.prov.rds"
      ) |>
      readr::read_rds()
    cli::cli_process_done()

    cli::cli_process_start("3) Loading district shape files from S Drive")
    raw.data$global.dist <-
      file.path(
        folder,
        "shapefiles_01",
        "global.dist.rds"
      ) |>
      read_rds()
    cli::cli_process_done()

    cli::cli_process_start("4) Loading AFP line list data from S Drive")
    raw.data$afp <-
      list.files(
        path = file.path(
          folder,
          "all_year_all_var_files_20220909"
        ),
        pattern = "^.*(afp).*(linelist).*(.rds)$",
        full.names = TRUE
      ) |>
      lapply(readr::read_rds) |>
      dplyr::bind_rows() |>
      dplyr::filter(surveillancetypename == "AFP") |>
      dplyr::filter(yronset >= 2016) |>
      dplyr::mutate(
        cdc.classification.all2 = ifelse(
          final.cell.culture.result == "Not received in lab" &
            cdc.classification.all == "PENDING",
          "LAB PENDING",
          cdc.classification.all
        ),
        hot.case = ifelse(
          paralysis.asymmetric == "Yes" &
            paralysis.onset.fever == "Yes" &
            paralysis.rapid.progress == "Yes",
          1,
          0
        ),
        hot.case = ifelse(is.na(hot.case), 99, hot.case)
      )
    cli::cli_process_done()

    cli::cli_process_start("Processing AFP data for analysis")

    raw.data$afp.epi <- raw.data$afp |>
      dplyr::filter(yronset >= 2016) |>
      dplyr::mutate(epi.week = epiweek(dateonset)) |>
      dplyr::group_by(place.admin.0, epi.week, yronset, cdc.classification.all2) |>
      dplyr::summarise(afp.cases = n()) |>
      dplyr::mutate(epiweek.year = paste(yronset, epi.week, sep = "-")) |>
      #manual fix of epi week
      dplyr::mutate(epi.week = ifelse(epi.week == 52 &
                                 yronset == 2022, 1, epi.week)) |>
      ungroup()

    #factoring cdc classification to have an order we like in stacked bar chart
    raw.data$afp.epi$cdc.classification.all2 <-
      factor(
        raw.data$afp.epi$cdc.classification.all2,
        levels = c(
          "WILD 1",
          "cVDPV 2",
          "VDPV 2",
          "cVDPV 1",
          "VDPV 1",
          "COMPATIBLE",
          "PENDING",
          "LAB PENDING",
          "NPAFP",
          "NOT-AFP"
        ),
        labels = c(
          "WILD 1",
          "cVDPV 2",
          "VDPV 2",
          "cVDPV 1",
          "VDPV 1",
          "COMPATIBLE",
          "PENDING",
          "LAB PENDING",
          "NPAFP",
          "NOT-AFP"
        )
      )

    raw.data$para.case <- raw.data$afp |>
      dplyr::filter(
        cdc.classification.all2 %in% c("cVDPV 2", "VDPV 1", "VDPV 2", "WILD 1", "cVDPV 1", "COMPATIBLE")
      ) |>
      dplyr::mutate(yronset = ifelse(is.na(yronset) == T, 2022, yronset)) #this fix was for the manually added MOZ case
    cli::cli_process_done()


    cli::cli_process_start("5) Loading population data from S Drive")
    raw.data$dist.pop <-
      file.path(
        folder,
        "dist.pop.long2010_2023.rds"
      ) |>
      readr::read_rds() |>
      dplyr::ungroup() |>
      dplyr::filter(year >= 2016)

    raw.data$prov.pop <-
      file.path(
        folder,
        "prov.pop.long2010_2023.rds"
      ) |>
      readr::read_rds() |>
      dplyr::ungroup() |>
      dplyr::filter(year >= 2016)

    raw.data$ctry.pop <-
      file.path(
        folder,
        "ctry.pop.2000_2023.rds"
      ) |>
      readr::read_rds() |>
      dplyr::ungroup() |>
      dplyr::filter(year >= 2016)

    cli::cli_process_done()

    cli::cli_process_start("6) Loading coverage data from S Drive")
    file.path(
      folder,
      "coverage",
      "dpt_district_summaries.rda"
    ) |>
      load()

    file.path(
      folder,
      "coverage",
      "mcv1_district_summaries.rda"
    ) |>
      load()

    raw.data$coverage <- dpt |>
      select(ctry = adm0_name,
             prov = adm1_name,
             dist = adm2_name,
             year,
             dpt1,
             dpt3) |>
      left_join(
        mcv1 |>
          select(
            ctry = adm0_name,
            prov = adm1_name,
            dist = adm2_name,
            year,
            mcv1,
            under5_pop
          ),
        by = c("ctry", "prov", "dist", "year")
      )

    rm(dpt)
    rm(mcv1)
    cli::cli_process_done()

    cli::cli_process_start("7) Loading ES data from S drive")
    raw.data$es <-
      file.path(
        folder,
        "all_year_all_var_files_20220909"
      ) |>
      list.files(pattern = "^(es).*(.rds)$", full.names = TRUE) |>
      lapply(readr::read_rds) |>
      dplyr::bind_rows()
    cli::cli_process_done()

    cli::cli_process_start("8) Loading SIA data from S drive")
    raw.data$sia <-
      file.path(
        folder,
        "all_year_all_var_files_20220909"
      ) |>
      list.files(pattern = "^.*(sia).*(.rds)$", full.names = TRUE) |>
      lapply(readr::read_rds) |>
      dplyr::bind_rows()
    cli::cli_process_done()

    cli::cli_process_start("9) Loading all positives from S drive")
    raw.data$pos <-
      file.path(
        folder,
        "all_year_all_var_files_20220909"
      ) |>
      list.files(pattern = "^(positives).*(.rds)$", full.names = TRUE) |>
      lapply(readr::read_rds) |>
      dplyr::bind_rows()
    cli::cli_process_done()

    cli::cli_process_start("10) Loading road network data")
    raw.data$roads <- sf::read_sf("//cdc.gov/project/CGH_GID_Active/PEB/SIR/DATA/Core 2.0/datafiles_01/shapefiles_01/ne_10m_roads")
    cli::cli_process_done()

    cli::cli_process_start("11) Loading city spatial data")
    raw.data$cities <- sf::read_sf("//cdc.gov/project/CGH_GID_Active/PEB/SIR/DATA/Core 2.0/datafiles_01/shapefiles_01/World_Cities")
    cli::cli_process_done()

    cli::cli_process_start("12) Clearing out unused memory")
    gc()
    cli::cli_process_done()

    return(raw.data)

  }else{
    cli::cli_alert_warning("Stopping download process, please try again with a better connection")
  }

}

#' Extract country specific information from raw polio data
#'
#' @description Extract country specific data from raw data
#' @import cli dplyr sf stringr
#' @param .raw.data List of raw data sources
#' @param .country String of a country name of interest
#' @export
extract_country_data <- function(
    .country,
    .raw.data = raw.data
){
  cli::cli_h1(paste0("--Processing country data for: ", .country, "--"))
  cli::cli_process_start("1) Subsetting country spatial data")
  ctry.data <- list()
  ctry.data$ctry <- .raw.data$global.ctry |>
    dplyr::filter(stringr::str_detect(ADM0_NAME, .country))

  #Error checking for overlapping ADM0 Names
  ctrys <- sort(unique(ctry.data$ctry$ADM0_SOVRN))

  if(length(ctrys) > 1){

    ctry.options <- paste0(paste0("\n",  paste0(1:length(ctrys), ") "), ctrys), collapse = "")

    message("Multiple countries match that name, please choose one by designating a number: ")
    message(ctry.options)

    chosen.country <- as.integer(readline("Enter only the number to designate a country: \n"))
    i <- 1

    while(!chosen.country %in% 1:length(ctrys) & i < 5){
      chosen.country <- as.integer(readline("Invalid choice, please only choose a number from the list and enter only that number as an integer: \n"))
      i <- i + 1
    }

    if(i == 5){
      stop("Invalid country choice, please verify input and try running the function again!")
    }

    chosen.country <- ctrys[chosen.country]

  }else{
    chosen.country <- ctry.data$ctry$ADM0_SOVRN
  }

  ctry.data$ctry <- dplyr::filter(ctry.data$ctry, ADM0_SOVRN == chosen.country)
  .country <- unique(ctry.data$ctry$ADM0_NAME)

  ctry.data$prov <- .raw.data$global.prov |>
    dplyr::filter(ADM0_NAME == .country)

  ctry.data$dist <- .raw.data$global.dist |>
    dplyr::filter(ADM0_NAME == .country)

  ctry.data$name <- ctry.data$ctry$ADM0_NAME
  ctry.data$vis.name <- ctry.data$ctry$ADM0_VIZ_NAME
  ctry.data$ctry.code <- ctry.data$ctry$WHO_CODE

  cli::cli_process_done()

  cli::cli_process_start("2) Extracting bordering geometries for reference")
  sf::sf_use_s2(F)
  a <- sf::st_touches(ctry.data$ctry, .raw.data$global.dist, sparse = F)
  sf::sf_use_s2(T)
  ctry.data$proximal.dist <- .raw.data$global.dist[a, ]

  sf::sf_use_s2(F)
  a <- sf::st_touches(ctry.data$ctry, .raw.data$global.ctry, sparse = F)
  sf::sf_use_s2(T)
  ctry.data$proximal.ctry <- .raw.data$global.ctry[a, ]
  cli::cli_process_done()

  cli::cli_process_start("3) Pulling data from OSM for Roads")

  ctry.data$roads <- raw.data$roads |>
    sf::st_intersection(ctry.data$ctry)

  cli::cli_process_done()

  cli::cli_process_start("4) Pulling data from OSM for Cities")

  ctry.data$cities <- raw.data$cities |>
    sf::st_intersection(ctry.data$ctry)

  cli::cli_process_done()

  cli::cli_process_start("5) Prepping AFP linelist data")

  ctry.data$afp.all <- raw.data$afp |>
    #filter(str_detect(place.admin.0, .country)) |>
    dplyr::filter(place.admin.0 == .country) |>
    dplyr::filter(!is.na(lon) & !is.na(lat)) |>
    sf::st_as_sf(coords = c(x = "lon", y = "lat"),
             crs = sf::st_crs(ctry.data$ctry)) |>
    dplyr::rename(
      ctry = place.admin.0,
      prov = place.admin.1,
      dist = place.admin.2,
      sex = person.sex,
      date = dateonset,
      year = yronset,
      date.notify = datenotify,
      date.invest = dateinvest,
      cdc.class = cdc.classification.all
    )

  ctry.data$afp.all.2 <- raw.data$afp |>
    #filter(str_detect(place.admin.0, .country)) |>
    dplyr::filter(place.admin.0 == .country) |>
    dplyr::rename(
      ctry = place.admin.0,
      prov = place.admin.1,
      dist = place.admin.2,
      sex = person.sex,
      date = dateonset,
      year = yronset,
      date.notify = datenotify,
      date.invest = dateinvest,
      cdc.class = cdc.classification.all
    )

  ctry.data$afp <- raw.data$afp |>
    #filter(str_detect(place.admin.0, .country)) |>
    dplyr::filter(place.admin.0 == .country) |>
    dplyr::filter(!is.na(lon) & !is.na(lat)) |>
    dplyr::filter(!(
      cdc.classification.all %in% c("PENDING", "NPAFP", "COMPATIBLE", "UNKNOWN", "NOT-AFP")
    )) |>
    sf::st_as_sf(coords = c(x = "lon", y = "lat"),
             crs = sf::st_crs(ctry.data$ctry)) |>
    dplyr::rename(
      ctry = place.admin.0,
      prov = place.admin.1,
      dist = place.admin.2,
      sex = person.sex,
      date = dateonset,
      year = yronset,
      date.notify = datenotify,
      date.invest = dateinvest,
      cdc.class = cdc.classification.all
    )

  ctry.data$afp.2 <- raw.data$afp |>
    #filter(str_detect(place.admin.0, .country)) |>
    dplyr::filter(place.admin.0 == .country) |>
    dplyr::filter(!(
      cdc.classification.all %in% c("PENDING", "NPAFP", "COMPATIBLE", "UNKNOWN", "NOT-AFP")
    )) |>
    dplyr::rename(
      ctry = place.admin.0,
      prov = place.admin.1,
      dist = place.admin.2,
      sex = person.sex,
      date = dateonset,
      year = yronset,
      date.notify = datenotify,
      date.invest = dateinvest,
      cdc.class = cdc.classification.all
    )

  ctry.data$afp.epi <- raw.data$afp.epi |>
    dplyr::filter(place.admin.0 == .country)
  #filter(str_detect(place.admin.0, .country))

  ctry.data$para.case <- ctry.data$afp.epi |>
    dplyr::filter(
      cdc.classification.all2 %in% c("cVDPV 2", "VDPV 1", "VDPV 2", "WILD 1", "cVDPV 1", "COMPATIBLE")
    ) |>
    dplyr::mutate(yronset = ifelse(is.na(yronset) == T, 2022, yronset)) #this fix was for the manually added MOZ case

  cli::cli_process_done()

  cli::cli_process_start("6) Prepping population data")
  ctry.data$dist.pop <- raw.data$dist.pop |>
    dplyr::filter(ADM0_NAME == .country) |>
    #filter(str_detect(ADM0_NAME, .country)) |>
    dplyr::mutate(ADM0_NAME = .country) |>
    dplyr::select(year,
           ctry = ADM0_NAME,
           prov = ADM1_NAME,
           dist = ADM2_NAME,
           u15pop,
           adm2guid)

  cli::cli_process_done()

  cli::cli_process_start("7) Prepping positives data")
  ctry.data$pos <- raw.data$pos |>
    dplyr::filter(place.admin.0 == .country)
  #filter(str_detect(place.admin.0, .country)) |>
  cli::cli_process_done()

  gc()

  return(ctry.data)

}

#### 3) Secondary SP Functions ####

#' Standard function to load District data
#'
#' @description Pulls district shapefiles directly from the geodatabase
#' @param fp str: Location of geodatabase
#' @import stringr AzureStor dplyr lubridate
#' @param azcontainer azure validated container object
#' @param dist_guid array/str: Array of all district GUIDS that you want to pull
#' @param ctry_name array/str: Array of all country names that you want to pull
#' @param end.year int: last year you want to pull information for - default is current year
#' @param st.year int: earlier year of spatial data you want to pull - default is 2000
#' @param data.only boolean: default F, if true, returns a rectangular tibble instead of a shape file
#' @param type str: "long" or NULL, default NULL, if "long" returns a spatial object for every year group
#' @returns tibble or sf dataframe
#' @export
load_clean_dist_sp <- function(azcontainer = get_azure_storage_connection(),
                               fp = "GID/PEB/SIR/Data/spatial/global.dist.rds",
                               dist_guid = NULL,
                               ctry_name = NULL,
                               end.year = year(Sys.Date()),
                               st.year = 2000,
                               data.only = F,
                               type = NULL){
  cli::cli_alert_info("Loading country spatial files")
  out <- suppressWarnings(AzureStor::storage_load_rds(azcontainer, fp)) |>
    dplyr::mutate(

      STARTDATE = lubridate::as_date(STARTDATE),
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

      yr.st = lubridate::year(STARTDATE),
      yr.end = lubridate::year(ENDDATE),
      ADM0_NAME = ifelse(stringr::str_detect(ADM0_NAME, "IVOIRE"),"COTE D IVOIRE", ADM0_NAME)
    ) %>%

    # remove the ouad eddahab in Morocco which started and ended the same year and causes overlap
    dplyr::filter(!GUID=="{AE526BC0-8DC3-411C-B82E-75259AD3598C}") %>%
    # this filters based on dates set in RMD
    dplyr::filter(yr.st <= end.year & (yr.end >= st.year | yr.end == 9999)) %>%
    {
      if(is.null(dist_guid)){.}else{
        dplyr::filter(., GUID %in% dist_guid)
      }
    } %>%
    {
      if(is.null(ctry_name)){.}else{
        filter(., ADM0_NAME %in% ctry_name)
      }
    }

  if(data.only & is.null(type)){
    out <- tibble::tibble(out)

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

#' Standard function to load Province data
#'
#' @description Pulls province shapefiles directly from the geodatabase
#' @import stringr AzureStor lubridate dplyr tibble
#' @param azcontainer azure validated container object
#' @param fp str: Location of geodatabase
#' @param prov_guid array/str: Array of all province GUIDS that you want to pull
#' @param prov_name array/str: Array of all province names that you want to pull
#' @param ctry_name array/str: Array of all country names that you want to pull
#' @param end.year int: last year you want to pull information for - default is current year
#' @param st.year int: earlier year of spatial data you want to pull - default is 2000
#' @param data.only boolean: default F, if true, returns a rectangular tibble instead of a shape file
#' @param type str: "long" or NULL, default NULL, if "long" returns a spatial object for every year group
#' @returns tibble or sf dataframe
#' @export
load_clean_prov_sp <- function(azcontainer = get_azure_storage_connection(),
                               fp = "GID/PEB/SIR/Data/spatial/global.prov.rds",
                               prov_guid = NULL,
                               prov_name = NULL,
                               ctry_name = NULL,
                               end.year = year(Sys.Date()),
                               st.year = 2000,
                               data.only = F,
                               type = NULL){
  cli::cli_alert_info("Loading province spatial files")
  out <- suppressWarnings(AzureStor::storage_load_rds(azcontainer, fp)) |>
    dplyr::mutate(
      yr.st = lubridate::year(STARTDATE),
      yr.end = lubridate::year(ENDDATE),
      ADM0_NAME = ifelse(stringr::str_detect(ADM0_NAME, "IVOIRE"),"COTE D IVOIRE", ADM0_NAME)
    ) %>%
    # this filters based on dates set in RMD
    dplyr::filter(yr.st <= end.year & (yr.end >= st.year | yr.end == 9999)) %>%
    {
      if(is.null(prov_guid)){.}else{
        dplyr::filter(., GUID %in% prov_guid)
      }
    } %>%
    {
      if(is.null(ctry_name)){.}else{
        dplyr::filter(., ADM0_NAME %in% ctry_name)
      }
    } %>%
    {
      if(is.null(prov_name)){.}else{
        dplyr::filter(., ADM1_NAME %in% prov_name)
      }
    }

  if(data.only & is.null(type)){
    out <- tibble::tibble(out)

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

#' Standard function to load Country data
#'
#' @description Pulls province shapefiles directly from the geodatabase
#' @import stringr AzureStor dplyr tibble lubridate
#' @param azcontainer azure validated container object
#' @param fp str: Location of geodatabase
#' @param ctry_guid array/str: Array of all country GUIDS that you want to pull
#' @param ctry_name array/str: Array of all country names that you want to pull
#' @param end.year int: last year you want to pull information for - default is current year
#' @param st.year int: earlier year of spatial data you want to pull - default is 2000
#' @param data.only boolean: default F, if true, returns a rectangular tibble instead of a shape file
#' @param type str: "long" or NULL, default NULL, if "long" returns a spatial object for every year group
#' @returns tibble or sf dataframe
#' @export
load_clean_ctry_sp <- function(azcontainer = get_azure_storage_connection(),
                               fp = "GID/PEB/SIR/Data/spatial/global.ctry.rds",
                               ctry_guid = NULL,
                               ctry_name = NULL,
                               end.year = year(Sys.Date()),
                               st.year = 2000,
                               data.only = F,
                               type = NULL){
  cli::cli_alert_info("Loading country spatial files")
  out <- suppressWarnings(AzureStor::storage_load_rds(azcontainer, fp)) |>
    dplyr::mutate(
      yr.st = lubridate::year(STARTDATE),
      yr.end = lubridate::year(ENDDATE),
      ADM0_NAME = ifelse(stringr::str_detect(ADM0_NAME, "IVOIRE"),"COTE D IVOIRE", ADM0_NAME)
    ) %>%
    # this filters based on dates set in RMD
    dplyr::filter(yr.st <= end.year & (yr.end >= st.year | yr.end == 9999)) %>%
    {
      if(is.null(ctry_guid)){.}else{
        dplyr::filter(., GUID %in% ctry_guid)
      }
    } %>%
    {
      if(is.null(ctry_name)){.}else{
        dplyr::filter(., ADM0_NAME %in% ctry_name)
      }
    }

  if(data.only & is.null(type)){
    out <- tibble::as_tibble(out)

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
    return(dplyr::bind_rows(df.list))
  }

}

#### 4) Misc ####

#' Utility function to fix years
#'
#' @param df tibble: dataframe to be used for fixing years
#' @param yrs numeric array: array of years to fix data
#' @returns long tibble
f.yrs.01 <- function(df, yrs) {
  quo.yrs <- enquo(yrs)

  shape01 <- df %>%
    filter((yr.st <= !!quo.yrs & yr.end >= !!quo.yrs) |
             yr.st == !!quo.yrs) %>%
    mutate(active.year.01 = !!quo.yrs)

  return(shape01)
}

