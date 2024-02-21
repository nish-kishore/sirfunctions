#DAL - Data Access Layer

#### 1) Utility functions ####

#' Validate connection to EDAV
#'
#' @description Validate connection to CDC EDAV
#' @import AzureStor AzureAuth utils dplyr
#' @param app_id str: Application ID defaults to "04b07795-8ddb-461a-bbee-02f9e1bf7b46"
#' @param auth str: authorization type defaults to "authorization_code"
#' @returns azure container verification
#' @export
get_azure_storage_connection <- function(
    app_id = "04b07795-8ddb-461a-bbee-02f9e1bf7b46",
    auth = "authorization_code"
  ){
  mytoken <- AzureAuth::get_azure_token(
    resource = "https://storage.azure.com/",
    tenant = "9ce70869-60db-44fd-abe8-d2767077fc8f",
    app = app_id,
    auth_type = auth
  )

  cached_tokens <- AzureAuth::list_azure_tokens()
  token_hash_names <- AzureAuth::list_azure_tokens() |> names()

  mytoken <- lapply(1:length(token_hash_names), function(x){
    dplyr::tibble(
      "token" = token_hash_names[x],
      "resource" = cached_tokens[[token_hash_names[x]]]$resource
    )
    }) |>
    dplyr::bind_rows() |>
    dplyr::filter(resource == "https://storage.azure.com/") |>
    utils::head(1) |>
    dplyr::pull(token) |>
    AzureAuth::load_azure_token()

  endptoken <- AzureStor::storage_endpoint(endpoint = "https://davsynapseanalyticsdev.dfs.core.windows.net", token = mytoken)

  azcontainer <- AzureStor::storage_container(endptoken, "ddphsis-cgh")

  return(azcontainer)
}

#' Helper function to read and write key data
#'
#' @description Helper function read and write key data to EDAV
#' @import cli AzureStor
#' @param io str: "read", "write", "delete", "exists.dir", "exists.file", "create" or "list"
#' @param default_dir str: "GID/PEB/SIR"
#' @param file_loc str: location to "read", "write", "exists.dir", "exists.file", "create" or "list"
#' @param obj default NULL object to be saved
#' @param azcontainer azure container object
#' @param force_delete boolean: use delete io without validation
#' @returns output dependent on "io"
#' @export
edav_io <- function(
    io,
    default_dir = "GID/PEB/SIR",
    file_loc = NULL,
    obj = NULL,
    azcontainer = suppressMessages(get_azure_storage_connection()),
    force_delete = F
){

  if(!is.null(file_loc)){
    if(is.null(default_dir)){
      file_loc <- file_loc
    }else{
      file_loc <- paste(default_dir, file_loc, sep = "/")
    }
  }else{
    file_loc <- default_dir
  }

  opts <- c("read", "write", "delete", "list", "exists.dir", "exists.file", "create")

  if(!io %in% opts){
    stop("io: must be 'read', 'write', 'exists.dir', 'exists.file','create', 'delete' or 'list'")
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

  if(io == "exists.dir"){
    return(AzureStor::storage_dir_exists(azcontainer, file_loc))
  }

  if(io == "exists.file"){
    return(AzureStor::storage_file_exists(azcontainer, file_loc))
  }

  if(io == "create"){
    tryCatch(
      {AzureStor::create_storage_dir(azcontainer, file_loc)
        print("Directory created!")},
      error = function(e) {stop("Directory creation failed")}
    )

  }

  if(io == "read"){

    if(!AzureStor::storage_file_exists(azcontainer, file_loc)){
      stop("File does not exist")
    }

    if(!grepl(".rds|.rda|.csv",file_loc)){
      stop("At the moment only 'rds' 'rda' and 'csv' are supported for reading.")
    }

    if(grepl(".rds", file_loc)){
      return(suppressWarnings(AzureStor::storage_load_rds(azcontainer, file_loc)))
    }

    if(grepl(".csv", file_loc)){
      return(suppressWarnings(AzureStor::storage_read_csv(azcontainer, file_loc)))
    }

    if(grepl(".rda", file_loc)){
      return(suppressWarnings(AzureStor::storage_load_rdata(azcontainer, file_loc)))
    }

  }

  if(io == "write"){

    if(!grepl(".rds|.csv",file_loc)){
      stop("At the moment only 'rds' 'rda' and 'csv' are supported for reading.")
    }

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

    if(force_delete){
      AzureStor::delete_storage_file(azcontainer, file_loc, confirm = F)
    }else{
      x <- readline(prompt = "Are you sure you want to delete this file? It can only be recovered by an administrator. [Y/N]")
      x <- tolower(x)


      if(grepl("y|n",x)){
        if(x == "y"){
          AzureStor::delete_storage_file(azcontainer, file_loc, confirm = F)
          cli::cli_alert_info("File deleted!")
        }else{
          cli::cli_alert_info("Deletion canceled.")
        }
      }else{
        stop("Response must be 'Y' or 'N'")
      }
    }

  }

}

#' Test network connection to the EDAV
#'
#' @description Tests upload and download from EDAV
#' @param azcontainer azure storage container
#' @param folder str: Location of download folder in EDAV
#' @param test_size int: byte size of a theoretical folder
#' @param return_list boolean: return a list of download time estimates, default F
#' @import dplyr readr prettyunits tibble cli
#' @returns System message with download and update time, potentially list output
#' @export
test_EDAV_connection <- function(
    azcontainer = suppressMessages(get_azure_storage_connection()),
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

  edav_io(io = "write", file_loc = paste0(folder, "/tmp.rds"), obj = tmp_data, default_dir = NULL)
  #readr::write_rds(tmp_data, paste0(folder, "/tmp.rds"))

  tock <- Sys.time()

  dt1 <- as.numeric(difftime(tock, tick, units = "secs"))

  tick <- Sys.time()

  x <- edav_io(io = "read", file_loc = paste0(folder, "/tmp.rds"), default_dir = NULL)
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

  suppressMessages(edav_io(io = "delete", file_loc = paste0(folder, "/tmp.rds"), default_dir = NULL, force_delete = T), classes = c("message", "warning"))
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
#' @description Download POLIS data from the CDC pre-processed endpoint. By default
#' this function will return a "small" or recent dataset. This is primarily for data
#' that is from 2019 onwards. You can specify a "medium" sized dataset for data
#' that is from 2016 onwards. Finally the "large" sized dataset will provide information
#' from 2001 onwards. Regular pulls form the data will recreate the "small" dataset
#' when new information is availble and the Data Management Team can force the
#' creation of the "medium" and "large" static datasets as necessary.
#' @import dplyr cli sf tibble
#' @param size str: "small", "medium", "large", defaults to "small"
#' @param folder str: location of the CDC pre-processed endpoint
#' @param force.new.run boolean: default F, if T will run recent data and cache
#' @param recreate.static.files boolean: default F, if T will run all data and cache
#' @returns named list containing polio data that is relevant to CDC
#' @export
get_all_polio_data <- function(
    size = "small",
    folder = "GID/PEB/SIR/Data/",
    force.new.run = F,
    recreate.static.files = F
){

  #check to see that size parameter is appropriate
  if(!size %in% c("small", "medium", "large")){
    stop("The parameter 'size' must be either 'small', 'medium', or 'large'")
  }

  #look to see if the recent raw data rds is in the analytic folder
  prev_table <- edav_io(io = "list", file_loc = file.path(folder, "/analytic"), default_dir = NULL) |>
    dplyr::filter(grepl("raw.data.recent.rds", name)) |>
    dplyr::select("file" = "name", "size", "ctime" = "lastModified")

  #if there is previous dataset then
  if(nrow(prev_table) > 0){
    #check to see if it was created in the last 7 days and set parameters
    if(difftime(Sys.time(), prev_table$ctime, units = "days") > 7){
      force.new.run <- T
      create.cache <- T
    }else{
      force.new.run <- F
      create.cache <- F
    }
  }

  if(recreate.static.files){
    force.new.run <- T
    create.cache <- T
  }

  if(!force.new.run){

    #determine all raw data files to be downloaded
    cli::cli_alert_info("Downloading most recent active polio data from 2019 onwards")
    raw.data.post.2019 <- edav_io(io = "read", file_loc = prev_table$file, default_dir = NULL)

    if(size == "small"){
      raw.data <- raw.data.post.2019
    }

    if(size == "medium"){

      prev_table <- edav_io(io = "list", file_loc = file.path(folder, "/analytic"), default_dir = NULL) |>
        dplyr::filter(grepl("raw.data.2016.2018.rds", name)) |>
        dplyr::select("file" = "name", "size", "ctime" = "lastModified")

      cli::cli_alert_info("Downloading static polio data from 2016-2019")
      raw.data.2016.2018 <- edav_io(io = "read", file_loc = prev_table$file, default_dir = NULL)

      raw.data <- split_concat_raw_data(
        action = "concat",
        raw.data.post.2019 = raw.data.post.2019,
        raw.data.2016.2019 = raw.data.2016.2018
        )
    }

    if(size == "large"){

      prev_table <- edav_io(io = "list", file_loc = file.path(folder, "/analytic"), default_dir = NULL) |>
        dplyr::filter(grepl("raw.data.2016.2018.rds", name)) |>
        dplyr::select("file" = "name", "size", "ctime" = "lastModified")

      cli::cli_alert_info("Downloading static polio data from 2016-2019")
      raw.data.2016.2019 <- edav_io(io = "read", file_loc = prev_table$file, default_dir = NULL)

      prev_table <- edav_io(io = "list", file_loc = file.path(folder, "/analytic"), default_dir = NULL) |>
        dplyr::filter(grepl("raw.data.2000", name)) |>
        dplyr::select("file" = "name", "size", "ctime" = "lastModified")

      cli::cli_alert_info("Downloading static polio data from 2001-2016")
      raw.data.2001.2016 <- edav_io(io = "read", file_loc = prev_table$file, default_dir = NULL)

      raw.data <- split_concat_raw_data(
        action = "concat",
        raw.data.post.2019 = raw.data.post.2019,
        raw.data.2016.2019 = raw.data.2016.2019,
        raw.data.2001.2016 = raw.data.2001.2016
      )
    }

    cli::cli_process_done()

    return(raw.data)

  }else{

    cli::cli_h1("Testing download times")

    download_metrics <- test_EDAV_connection(return_list = T)

    #use the truncated AFP file
    afp.trunc <- T

    if(recreate.static.files){
      afp.trunc <- F
    }

    dl_table <- dplyr::bind_rows(
      edav_io(io = "list", file_loc = file.path(folder, "polis"), default_dir = NULL),
      edav_io(io = "list", file_loc = file.path(folder, "spatial"), default_dir = NULL),
      edav_io(io = "list", file_loc = file.path(folder, "coverage"), default_dir = NULL),
      edav_io(io = "list", file_loc = file.path(folder, "pop"), default_dir = NULL),
      edav_io(io = "list", file_loc = file.path("GID/PEB/SIR/POLIS/"), default_dir = NULL) |>
        filter(grepl("cache", name))
    ) |>
      dplyr::filter(!is.na(size)) |>
      dplyr::select("file" = "name", "size") |>
      dplyr::mutate(
        "dl_time_sec" = size / download_metrics$size*download_metrics$d
      )

    if(afp.trunc){
      dl_table <- dl_table |>
        filter(!grepl("afp_linelist_2001", file))
    }else{
      dl_table <- dl_table |>
        filter(!grepl("afp_linelist_2019", file))
    }

    file_size <- dl_table$size |> sum()
    download_time <- dl_table$dl_time_sec |> sum()

    cli::cli_h1("Downloading POLIS Data")

    raw.data <- list()

    cli::cli_process_start("1) Loading country shape files from EDAV")
    raw.data$global.ctry <- load_clean_ctry_sp()
    cli::cli_process_done()


    cli::cli_process_start("2) Loading province shape files from EDAV")
    raw.data$global.prov <- load_clean_prov_sp()
    cli::cli_process_done()

    cli::cli_process_start("3) Loading district shape files from EDAV")
    raw.data$global.dist <- load_clean_dist_sp()
    cli::cli_process_done()

    cli::cli_process_start("4) Loading AFP line list data from EDAV (This file is almost 3GB and can take a while)")
    raw.data$afp <-
      edav_io(io = "read",
              file_loc = dplyr::filter(dl_table, grepl("afp", file)) |>
                dplyr::pull(file), default_dir = NULL) |>
      dplyr::filter(surveillancetypename == "AFP") |>
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


    cli::cli_process_start("5) Loading population data from EDAV")
    raw.data$dist.pop <-
      edav_io(io = "read",
              file_loc = dplyr::filter(dl_table, grepl("dist.pop", file)) |>
                dplyr::pull(file), default_dir = NULL) |>
      dplyr::ungroup()

    raw.data$prov.pop <-
      edav_io(io = "read",
              file_loc = dplyr::filter(dl_table, grepl("prov.pop", file)) |>
                dplyr::pull(file), default_dir = NULL) |>
      dplyr::ungroup()

    raw.data$ctry.pop <-
      edav_io(io = "read",
              file_loc = dplyr::filter(dl_table, grepl("ctry.pop", file)) |>
                dplyr::pull(file), default_dir = NULL) |>
      dplyr::ungroup()

    cli::cli_process_done()

    cli::cli_process_start("6) Loading coverage data from EDAV")
    raw.data$coverage <-
      edav_io(io = "read",
              file_loc = dplyr::filter(dl_table, grepl("dpt", file)) |>
                dplyr::pull(file), default_dir = NULL) |>
      select(ctry = adm0_name,
             prov = adm1_name,
             dist = adm2_name,
             year,
             dpt1,
             dpt3) |>
      left_join(
        edav_io(io = "read",
                file_loc = dplyr::filter(dl_table, grepl("mcv1", file)) |>
                  dplyr::pull(file), default_dir = NULL) |>
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

    cli::cli_process_done()

    cli::cli_process_start("7) Loading ES data from EDAV")

    raw.data$es <-
      edav_io(io = "read",
              file_loc = dplyr::filter(dl_table, grepl("/es_2001", file)) |>
                dplyr::pull(file), default_dir = NULL)
    cli::cli_process_done()

    cli::cli_process_start("8) Loading SIA data from EDAV")
    raw.data$sia <-
      edav_io(io = "read",
              file_loc = dplyr::filter(dl_table, grepl("sia", file)) |>
                dplyr::pull(file), default_dir = NULL)
    cli::cli_process_done()

    cli::cli_process_start("9) Loading all positives from EDAV")
    raw.data$pos <-
      edav_io(io = "read",
              file_loc = dplyr::filter(dl_table, grepl("/pos", file)) |>
                dplyr::pull(file), default_dir = NULL)
    cli::cli_process_done()

    cli::cli_process_start("10) Loading other surveillance linelist from EDAV")
    raw.data$other <-
      edav_io(io = "read",
              file_loc = dplyr::filter(dl_table, grepl("/other", file)) |>
                dplyr::pull(file), default_dir = NULL)
    cli::cli_process_done()

    cli::cli_process_start("11) Loading road network data")
    raw.data$roads <- edav_io(io = "read",
                              file_loc = dplyr::filter(dl_table, grepl("roads", file)) |>
                                dplyr::pull(file), default_dir = NULL)
    cli::cli_process_done()

    cli::cli_process_start("12) Loading city spatial data")
    raw.data$cities <- edav_io(io = "read",
                               file_loc = dplyr::filter(dl_table, grepl("cities", file)) |>
                                 dplyr::pull(file), default_dir = NULL)
    cli::cli_process_done()

    cli::cli_process_start("13) Creating Metadata object")

    polis.cache <- edav_io(io = "read",
                           file_loc = dplyr::filter(dl_table, grepl("cache", file)) |>
                             dplyr::pull(file), default_dir = NULL) |>
      dplyr::mutate(last_sync = as.Date(last_sync))

    raw.data$metadata$download_time <- max(polis.cache$last_sync, na.rm = TRUE)

    raw.data$metadata$processed_time <- edav_io(io = "list", file_loc = file.path(folder, "/polis"), default_dir = NULL) |>
      dplyr::filter(grepl("positives", name)) |>
      dplyr::select("ctime" = "lastModified") |>
      dplyr::mutate(ctime = as.Date(ctime)) |>
      dplyr::pull(ctime)

    raw.data$metadata$user <- polis.cache |>
      dplyr::filter(table == "virus") |>
      dplyr::pull(last_user)

    raw.data$metadata$most_recent_pos <- max(raw.data$pos$dateonset, na.rm = TRUE)
    raw.data$metadata$most_recent_pos_loc <- raw.data$pos |>
      dplyr::arrange(desc(dateonset)) |>
      dplyr::slice(1) |>
      dplyr::pull(place.admin.0)


    raw.data$metadata$most_recent_afp <- max(raw.data$afp$dateonset, na.rm = TRUE)
    raw.data$metadata$most_recent_afp_loc <- raw.data$afp |>
      dplyr::arrange(desc(dateonset)) |>
      dplyr::slice(1) |>
      dplyr::pull(place.admin.0)


    raw.data$metadata$most_recent_env <- max(raw.data$es$collect.date, na.rm = TRUE)
    raw.data$metadata$most_recent_env_loc <- raw.data$es |>
      dplyr::arrange(desc(collect.date)) |>
      dplyr::slice(1) |>
      dplyr::pull(ADM0_NAME)


    raw.data$metadata$most_recent_sia <- max(raw.data$sia$sub.activity.start.date)
    raw.data$metadata$most_recent_sia_code <- raw.data$sia |>
      dplyr::arrange(desc(sub.activity.start.date)) |>
      dplyr::slice(1) |>
      dplyr::pull(sia.code)
    raw.data$metadata$most_recent_sia_location <- raw.data$sia |>
      dplyr::arrange(desc(sub.activity.start.date)) |>
      dplyr::slice(1) |>
      dplyr::pull(place.admin.0)
    raw.data$metadata$most_recent_sia_vax <- raw.data$sia |>
      dplyr::arrange(desc(sub.activity.start.date)) |>
      dplyr::slice(1) |>
      dplyr::pull(vaccine.type)

    raw.data$metadata$most_recent_vdpv_class_change_date <- raw.data$pos$vdpvclassificationchangedate |>
      lubridate::as_date() |>
      max(na.rm = T)

    raw.data$metadata$default_subset_year <- default.subset.year

    rm(polis.cache)

    cli::cli_process_done()

    cli::cli_process_start("14) Clearing out unused memory")
    gc()
    cli::cli_process_done

  }

  if(create.cache){
    cli::cli_process_start("13) Caching processed data")

    out <- split_concat_raw_data(action = "split", split.years = c(2000,2016,2019), raw.data.all = raw.data)

    current.year <- lubridate::year(Sys.time())

    out_files <- out$split.years |>
      mutate(file_name = ifelse(grepl(current.year, tag),'recent', stringr::str_replace_all(tag, "-", ".")),
             file_name = paste0("raw.data.",file_name,".rds"))

    if(!recreate.static.files){
      out_files <- out_files |> filter(grepl("recent", file_name))
    }

    for(i in 1:nrow(out_files)){
      edav_io(
        io = "write",
        file_loc = file.path(folder, paste0("analytic/", pull(out_files[i, ], file_name))),
        obj = out[[pull(out_files[i, ], tag)]],
        default_dir = NULL
      )
    }

    cli::cli_process_done()
  }


  return(raw.data)

}

#' Extract country specific information from raw polio data
#'
#' @description Extract country specific data from raw data
#' @import cli dplyr sf stringr
#' @param .raw.data list: list of raw data sources
#' @param .country str: a country name of interest
#' @export
extract_country_data <- function(
    .country,
    .raw.data = raw.data
){

  .country <- stringr::str_to_upper(stringr::str_trim(.country))
  cli::cli_h1(paste0("--Processing country data for: ", stringr::str_to_title(.country), "--"))
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
load_clean_dist_sp <- function(azcontainer = suppressMessages(get_azure_storage_connection()),
                               fp = "GID/PEB/SIR/Data/spatial/global.dist.rds",
                               dist_guid = NULL,
                               ctry_name = NULL,
                               end.year = year(Sys.Date()),
                               st.year = 2000,
                               data.only = F,
                               type = NULL){
  cli::cli_alert_info("Loading district spatial files")
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
load_clean_prov_sp <- function(azcontainer = suppressMessages(get_azure_storage_connection()),
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
load_clean_ctry_sp <- function(azcontainer = suppressMessages(get_azure_storage_connection()),
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


#' Split or concatenate raw.data by year
#'
#' @param action Can either be to `concat` or `split`
#' @param split.years
#' @param raw.data.all
#' @param raw.data.post.2019
#' @param raw.data.2016.2019
#' @param raw.data.2001.2016
#' @returns list of lists or a single concatenated list
split_concat_raw_data <- function(
    action,
    split.years = NULL,
    raw.data.all = NULL,
    raw.data.post.2019 = NULL,
    raw.data.2016.2019 = NULL,
    raw.data.2001.2016 = NULL
  ){

  #temp
  split.years <- c(2000, 2016, 2019)
  #temp

  actions <- c("concat", "split")

  if(!action %in% actions){
    stop("`action` must be either `concat` or `split`")
  }

  current.year <- lubridate::year(Sys.time())

  key.tables <- c("afp", "afp.epi", "para.case", "dist.pop", "es", "sia", "pos", "other")

  static.tables <- c("global.ctry", "global.prov", "global.dist", "prov.pop", "ctry.pop", "coverage", "roads", "cities", "metadata")

  if(action == "split"){

    if(is.null(split.years) | class(split.years) != "numeric"){
      stop("You must provide a numeric array of years to split by and the years must be between 2000-current year")
    }

    if(is.null(raw.data.all)){
      stop("You must provide a complete raw.data dataset to be split")
    }

    if(!current.year %in% split.years){
      split.years <- c(split.years, current.year) |> sort()
    }

    split.years <- lapply(1:(length(split.years)-1), function(x){
      tibble(
        start.yr = split.years[x],
        end.yr = ifelse(
          split.years[x + 1] == current.year,
          current.year,
          split.years[x + 1] - 1)
        )
    }) |>
      bind_rows() |>
      mutate(tag = paste0(start.yr,"-",end.yr))

    key.table.vars <- tibble(
      "data" = key.tables,
      "year.var" = c(rep("yronset",3),"year", "collect.yr", "yr.sia", rep("yronset",2))
    )


    out <- list()

    for(i in 1:nrow(split.years)){

      for(j in 1:nrow(key.table.vars)){

        out[[pull(split.years[i, ], tag)]][[key.table.vars[j, ] |> pull(data)]] <-
          raw.data.all[[key.table.vars[j, ] |> pull(data)]] |>
          dplyr::filter(
            !!sym(key.table.vars[j, ] |> pull(year.var)) >= pull(split.years[i, ], start.yr) &
              !!sym(key.table.vars[j, ] |> pull(year.var)) <= pull(split.years[i, ], end.yr)
          )



      }


      for(k in 1:length(static.tables)){

        out[[pull(split.years[i, ], tag)]][[static.tables[k]]] <-
          raw.data.all[[static.tables[k]]]

      }

    }

    out$split.years <- split.years

    return(out)


  }

  if(action == "concat"){

    if(sum(is.null(raw.data.post.2019), is.null(raw.data.2016.2019), is.null(raw.data.2001.2016)) > 1){
      stop("You must include at least two subsets of raw.data files to concatenate")
    }

    input <- list()

    if(!is.null(raw.data.post.2019)){
      input[["raw.data.post.2019"]] <- raw.data.post.2019
    }

    if(!is.null(raw.data.post.2019)){
      input[["raw.data.2016.2019"]] <- raw.data.2016.2019
    }

    if(!is.null(raw.data.2001.2016)){
      input[["raw.data.2001.2016"]] <- raw.data.2001.2016
    }

    to.concat <- names(input)

    out <- list()

    for(i in key.table.vars$data){

      out[[i]] <- lapply(to.concat, function(x){
        input[[x]][[i]]
      }) |>
        bind_rows()

    }

    for(i in static.tables){

      out[[i]] <- raw.data.post.2019[[i]]

    }

    return(out)

  }


}



