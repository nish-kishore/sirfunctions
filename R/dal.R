# DAL - Data Access Layer

#### 1) Utility functions ####

#' Validate connection to EDAV
#'
#' Generate token which connects to CDC EDAV resources and
#' validates that the individual still has access. The current tenant ID
#' is hard coded for CDC resources.
#' @importFrom utils head
#' @param app_id `str` Application ID defaults to "04b07795-8ddb-461a-bbee-02f9e1bf7b46",
#' this can be changed if you have a service principal.
#' @param auth `str` Authorization type defaults to "authorization_code",
#' this can be changed if you have a service principal.
#' @returns Azure container verification
#' @examples
#' azcontainer <- get_azure_storage_connection()
#'
#' @export
get_azure_storage_connection <- function(
    app_id = "04b07795-8ddb-461a-bbee-02f9e1bf7b46",
    auth = "authorization_code") {
  mytoken <- AzureAuth::get_azure_token(
    resource = "https://storage.azure.com/",
    tenant = "9ce70869-60db-44fd-abe8-d2767077fc8f",
    app = app_id,
    auth_type = auth
  )

  cached_tokens <- AzureAuth::list_azure_tokens()
  token_hash_names <- AzureAuth::list_azure_tokens() |> names()

  mytoken <- lapply(1:length(token_hash_names), function(x) {
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

#' Helper function to read and write key data to the EDAV environment
#'
#' The function serves as the primary way to interact with the EDAV system from R. It can
#' read, write, create folders, check whether a file or a folder exists, upload files, and list
#' all files in a folder.
#' @import cli AzureStor ggplot2 flextable
#' @param io `str` The type of operation to perform in EDAV.
#' - `"read"` Read a file from EDAV, must be an rds, csv, or rda.
#' - `"write"` Write a file from EDAV, must be an rds, csv or rda.
#' - `"exists.dir"` Returns a boolean after checking to see if a folder exists.
#' - `"exists.file"`Returns a boolean after checking to see if a file exists.
#' - `"create"` Creates a folder and all preceding folders.
#' - `"list"` Returns a tibble with all objects in a folder.
#' - `"upload"` Moves a file of any type to EDAV.
#' @param default_dir `str` The default directory in EDAV. `"GID/PEB/SIR"` is the default directory
#' for all SIR data in EDAV. Can be set to `NULL` if you provide the full directory path in `file_loc`.
#' @param file_loc `str` Location to "read", "write", "exists.dir", "exists.file", "create" or "list", can include
#' the information in `default_dir` if you set that parameter to `NULL`.
#' @param obj `robj` Object to be saved, needed for `"write"`. Defaults to `NULL`.
#' @param azcontainer Azure container object returned from [get_azure_storage_connection()].
#' @param force_delete `bool` Use delete io without verification in the command line.
#' @param local_path `str` Local file pathway to upload a file to EDAV. Default is `NULL`.
#' This parameter is only required when passing `"upload"` in the `io` parameter.
#' @returns Output dependent on "io".
#' @examples
#' \dontrun{
#' df <- edav_io("read", file_loc = "df1.csv") # read file from EDAV
#' edav_io("write", file_loc = "Data/test", obj = df) # saves df to the test folder in EDAV
#' edav_io("exists.dir", "Data/nonexistentfolder") # returns FALSE
#' edav_io("exists.file", file_loc = "Data/test/df1.csv") # returns TRUE
#' edav_io("create", "Data/nonexistentfolder") # creates a folder called nonexistentfolder
#' edav_io("list") # list all files from the default directory
#' edav_io("upload", file_loc = "Data/test", local_path = "C:/Users/ABC1/Desktop/df2.csv")
#' }
#' @export
edav_io <- function(
    io,
    default_dir = "GID/PEB/SIR",
    file_loc = NULL,
    obj = NULL,
    azcontainer = suppressMessages(get_azure_storage_connection()),
    force_delete = F,
    local_path = NULL) {
  if (!is.null(file_loc)) {
    if (is.null(default_dir)) {
      file_loc <- file_loc
    } else {
      file_loc <- paste(default_dir, file_loc, sep = "/")
    }
  } else {
    file_loc <- default_dir
  }

  opts <- c("read", "write", "delete", "list", "exists.dir", "exists.file", "create", "upload")

  if (!io %in% opts) {
    stop("io: must be 'read', 'write', 'exists.dir', 'exists.file','create', 'delete' 'list' or 'upload'")
  }

  if (io == "write" & is.null(obj)) {
    stop("Need to supply an object to be written")
  }

  if (io == "upload" & is.null(local_path)) {
    stop("Need to supply file pathway of file to be uploaded")
  }

  if (io == "list") {
    if (!AzureStor::storage_dir_exists(azcontainer, file_loc)) {
      stop("Directory does not exist")
    }

    return(AzureStor::list_storage_files(azcontainer, file_loc) |>
             dplyr::as_tibble())
  }

  if (io == "exists.dir") {
    return(AzureStor::storage_dir_exists(azcontainer, file_loc))
  }

  if (io == "exists.file") {
    return(AzureStor::storage_file_exists(azcontainer, file_loc))
  }

  if (io == "create") {
    tryCatch(
      {
        AzureStor::create_storage_dir(azcontainer, file_loc)
        print("Directory created!")
      },
      error = function(e) {
        stop("Directory creation failed")
      }
    )
  }

  if (io == "read") {
    if (!AzureStor::storage_file_exists(azcontainer, file_loc)) {
      stop("File does not exist")
    }

    if (!grepl(".rds|.rda|.csv", file_loc)) {
      stop("At the moment only 'rds' 'rda' and 'csv' are supported for reading.")
    }

    if (grepl(".rds", file_loc)) {
      corrupted.rds <- NULL

      tryCatch(
        {
          return(suppressWarnings(AzureStor::storage_load_rds(azcontainer, file_loc)))
          corrupted.rds <<- FALSE
        },
        error = function(e) {
          cli::cli_alert_warning("RDS download from EDAV was corrupted, downloading directly...")
          corrupted.rds <<- TRUE
        }
      )

      if (corrupted.rds) {
        dest <- tempfile()
        AzureStor::storage_download(container = azcontainer, file_loc, dest)
        x <- readRDS(dest)
        unlink(dest)
        return(x)
      }
    }

    if (grepl(".csv", file_loc)) {
      return(suppressWarnings(AzureStor::storage_read_csv(azcontainer, file_loc)))
    }

    if (grepl(".rda", file_loc)) {
      return(suppressWarnings(AzureStor::storage_load_rdata(azcontainer, file_loc)))
    }
  }

  if (io == "write") {

    if (grepl(".rds", file_loc)) {
      AzureStor::storage_save_rds(object = obj, container = azcontainer, file = file_loc)
    }

    if (grepl(".csv", file_loc)) {
      AzureStor::storage_write_csv(object = obj, container = azcontainer, file = file_loc)
    }

    if ("gg" %in% class(obj)) {
      temp <- tempfile()
      ggplot2::ggsave(filename = paste0(temp, "/", sub(".*\\/", "", file_loc)), plot = obj)
      AzureStor::storage_upload(container = azcontainer, dest = file_loc,
                                src = paste0(temp, "/", sub(".*\\/", "", file_loc)))
      unlink(temp)
    }

    if ("flextable" %in% class(obj)) {
      temp <- tempfile()
      flextable::save_as_image(obj, path = paste0(temp, "/", sub(".*\\/", "", file_loc)))
      AzureStor::storage_upload(container = azcontainer, dest = file_loc,
                                src = paste0(temp, "/", sub(".*\\/", "", file_loc)))
      unlink(temp)
    }
  }

  if (io == "upload") {
    AzureStor::storage_upload(container = azcontainer, dest = file_loc, src = local_path)
  }

  if (io == "delete") {
    if (!AzureStor::storage_file_exists(azcontainer, file_loc)) {
      stop("File does not exist")
    }

    if (force_delete) {
      AzureStor::delete_storage_file(azcontainer, file_loc, confirm = F)
    } else {
      x <- readline(prompt = "Are you sure you want to delete this file? It can only be recovered by an administrator. [Y/N]")
      x <- tolower(x)


      if (grepl("y|n", x)) {
        if (x == "y") {
          AzureStor::delete_storage_file(azcontainer, file_loc, confirm = F)
          cli::cli_alert_info("File deleted!")
        } else {
          cli::cli_alert_info("Deletion canceled.")
        }
      } else {
        stop("Response must be 'Y' or 'N'")
      }
    }
  }
}

#' Test network connection to the EDAV
#'
#' Tests upload and download from EDAV by creating a temporary file
#' of a given size and testing the time it takes to upload and download the file.
#' @import dplyr readr cli
#' @param azcontainer Azure storage container provided by [get_azure_storage_connection()].
#' @param folder `str` Location of folder in the EDAV environment that you want to download
#' and upload data from.
#' @param test_size `int` byte size of a theoretical file to be uploaded or downloaded.
#' @param return_list `bool` return a list of download time estimates. Defaults to `FALSE`.
#' @returns System message with download and update time, potentially a list.
#' @examples
#' test_EDAV_connection()
#'
#' @export
test_EDAV_connection <- function(
    azcontainer = suppressMessages(get_azure_storage_connection()),
    folder = "GID/PEB/SIR/Data",
    return_list = F,
    test_size = 10000000) {

  if (!requireNamespace("prettyunits", quietly = TRUE)) {
    stop('Package "prettyunits" must be installed to use this function.',
         .call = FALSE)
  }

  tmp_data <- replicate(100, iris, simplify = F) |>
    dplyr::bind_rows() |>
    dplyr::as_tibble()

  tmp_file <- paste0(tempfile(), ".rds")

  readr::write_csv(tmp_data, tmp_file)

  file_size <- file.info(tmp_file)$size

  tick <- Sys.time()

  edav_io(io = "write", file_loc = paste0(folder, "/tmp.rds"), obj = tmp_data, default_dir = NULL)
  # readr::write_rds(tmp_data, paste0(folder, "/tmp.rds"))

  tock <- Sys.time()

  dt1 <- as.numeric(difftime(tock, tick, units = "secs"))

  tick <- Sys.time()

  x <- edav_io(io = "read", file_loc = paste0(folder, "/tmp.rds"), default_dir = NULL)
  # x <- readr::read_rds(paste0(folder, "/tmp.rds"))

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
  # x <- file.remove(paste0(folder, "/tmp.rds"))

  dt3 <- test_size / file_size * dt1
  dt4 <- test_size / file_size * dt2

  cli::cli_alert_info(c(
    "For a {prettyunits::pretty_bytes(test_size)} file we would expect the following times:"
  ))

  cli::cli_alert_info("Upload: {prettyunits::pretty_sec(dt3)}")
  cli::cli_alert_info("Download: {prettyunits::pretty_sec(dt4)}")

  if (return_list) {
    return(list("size" = file_size, "u" = dt1, "d" = dt2))
  }
}

#### 2) Key data pull functions ####


#' Retrieve all pre-processed polio data
#'
#' @description Download POLIS data from the CDC pre-processed endpoint. By default
#' this function will return a "small" or recent dataset. This is primarily for data
#' that is from 2019 onwards. You can specify a "medium" sized dataset for data
#' that is from 2016 onwards. Finally the "large" sized dataset will provide information
#' from 2001 onwards. Regular pulls form the data will recreate the "small" dataset
#' when new information is availble and the Data Management Team can force the
#' creation of the "medium" and "large" static datasets as necessary.
#' @import dplyr cli sf
#' @param size `str` Size of data to download. Defaults to `"small"`.
#' - `"small"`: data from 2019-present
#' - `"medium"`: data from 2016-present
#' - `"large"`: data from 2001-present
#' @param folder `str` Location of the CDC pre-processed endpoint, defaults to `"GID/PEB/SIR/Data"`.
#' @param force.new.run `bool` Default `FALSE`, if `TRUE` will run recent data and cache.
#' @param recreate.static.files `bool` Default `FALSE`, if `TRUE` will run all data and cache.
#' @param attach.spatial.data `bool` Default `TRUE`, adds spatial data to downloaded object.
#' @returns Named `list` containing polio data that is relevant to CDC.
#' @examples
#' \dontrun{
#' raw.data <- get_all_polio_data() # downloads data since 2019, including spatial files
#' }
#' raw.data <- get_all_polio_data(size = "small", attach.spatial.data = FALSE) # exclude spatial data
#'
#' @export
get_all_polio_data <- function(
    size = "small",
    folder = "GID/PEB/SIR/Data/",
    force.new.run = F,
    recreate.static.files = F,
    attach.spatial.data = T) {
  # check to see that size parameter is appropriate
  if (!size %in% c("small", "medium", "large")) {
    stop("The parameter 'size' must be either 'small', 'medium', or 'large'")
  }

  # look to see if the recent raw data rds is in the analytic folder
  prev_table <- edav_io(io = "list", file_loc = file.path(folder, "/analytic"), default_dir = NULL) |>
    dplyr::filter(grepl("raw.data.recent.rds", name)) |>
    dplyr::select("file" = "name", "size", "ctime" = "lastModified")

  # if there is previous dataset then
  if (nrow(prev_table) > 0) {
    # check to see if it was created in the last 7 days and set parameters
    if (difftime(Sys.time(), prev_table$ctime, units = "days") > 7) {
      force.new.run <- T
      create.cache <- T
    } else {
      force.new.run <- F
      create.cache <- F
    }
  } else {
    force.new.run <- T
    create.cache <- T
  }

  if (recreate.static.files) {
    force.new.run <- T
    create.cache <- T
  }

  if (!force.new.run) {
    # determine all raw data files to be downloaded
    cli::cli_alert_info("Downloading most recent active polio data from 2019 onwards")
    raw.data.post.2019 <- edav_io(io = "read", file_loc = prev_table$file, default_dir = NULL)

    if (size == "small") {
      raw.data <- raw.data.post.2019
    }

    if (size == "medium") {
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

    if (size == "large") {
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

    if (attach.spatial.data) {
      cli::cli_process_start("Downloading and attaching spatial data")
      spatial.data <- edav_io(io = "read", file_loc = file.path(folder, paste0("analytic/spatial.data.rds")), default_dir = NULL)

      raw.data$global.ctry <- spatial.data$global.ctry
      raw.data$global.prov <- spatial.data$global.prov
      raw.data$global.dist <- spatial.data$global.dist
      raw.data$roads <- spatial.data$roads
      raw.data$cities <- spatial.data$cities

      cli::cli_process_done()
    }

    cli::cli_process_start("Checking for duplicates in datasets.")
    raw.data <- duplicate_check(raw.data)
    cli::cli_process_done()

    return(raw.data)
  } else {
    cli::cli_h1("Testing download times")

    download_metrics <- test_EDAV_connection(return_list = T)

    # use the truncated AFP file
    afp.trunc <- T

    if (recreate.static.files) {
      afp.trunc <- F
    }

    dl_table <- dplyr::bind_rows(
      edav_io(io = "list", file_loc = file.path(folder, "polis"), default_dir = NULL),
      edav_io(io = "list", file_loc = file.path(folder, "spatial"), default_dir = NULL),
      edav_io(io = "list", file_loc = file.path(folder, "coverage"), default_dir = NULL),
      edav_io(io = "list", file_loc = file.path(folder, "pop"), default_dir = NULL),
      edav_io(io = "list", file_loc = file.path("GID/PEB/SIR/POLIS/"), default_dir = NULL) |>
        dplyr::filter(grepl("cache", name))
    ) |>
      dplyr::filter(!is.na(size)) |>
      dplyr::select("file" = "name", "size") |>
      dplyr::mutate(
        "dl_time_sec" = size / download_metrics$size * download_metrics$d
      )

    if (afp.trunc) {
      dl_table <- dl_table |>
        dplyr::filter(!grepl("afp_linelist_2001", file))
    } else {
      dl_table <- dl_table |>
        dplyr::filter(!grepl("afp_linelist_2019", file))
    }

    file_size <- dl_table$size |> sum()
    download_time <- dl_table$dl_time_sec |> sum()

    cli::cli_h1("Downloading POLIS Data")

    raw.data <- list()
    spatial.data <- list()

    cli::cli_process_start("1) Loading country shape files from EDAV")
    spatial.data$global.ctry <- load_clean_ctry_sp()
    cli::cli_process_done()


    cli::cli_process_start("2) Loading province shape files from EDAV")
    spatial.data$global.prov <- load_clean_prov_sp()
    cli::cli_process_done()

    cli::cli_process_start("3) Loading district shape files from EDAV")
    spatial.data$global.dist <- load_clean_dist_sp()
    cli::cli_process_done()

    cli::cli_process_start("4) Loading AFP line list data from EDAV (This file is almost 3GB and can take a while)")
    raw.data$afp <-
      edav_io(
        io = "read",
        file_loc = dplyr::filter(dl_table, grepl("afp", file)) |>
          dplyr::pull(file), default_dir = NULL
      ) |>
      dplyr::filter(surveillancetypename == "AFP") |>
      dplyr::mutate(
        cdc.classification.all2 = dplyr::case_when(final.cell.culture.result == "Not received in lab" &
                                                     cdc.classification.all == "PENDING" ~ "LAB PENDING",
                                                   TRUE ~ cdc.classification.all),
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
      dplyr::mutate(epi.week = lubridate::epiweek(dateonset)) |>
      dplyr::group_by(place.admin.0, epi.week, yronset, cdc.classification.all2) |>
      dplyr::summarize(afp.cases = dplyr::n()) |>
      dplyr::mutate(epiweek.year = paste(yronset, epi.week, sep = "-")) |>
      # manual fix of epi week
      dplyr::mutate(epi.week = ifelse(epi.week == 52 &
        yronset == 2022, 1, epi.week)) |>
      dplyr::ungroup()

    # factoring cdc classification to have an order we like in stacked bar chart
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
          "NOT-AFP",
          "UNKNOWN",
          "aVDPV 1",
          "aVDPV 3",
          "cVDPV1andcVDPV2",
          "CombinationWild1-cVDPV 2",
          "aVDPV 2",
          "VDPV 3",
          "iVDPV 2",
          "VDPV1andcVDPV2",
          "VAPP",
          "cVDPV 3",
          "iVDPV 3",
          "WILD 3",
          "WILD1andWILD3",
          "iVDPV 1",
          "cVDPV2andcVDPV3"
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
          "NOT-AFP",
          "UNKNOWN",
          "aVDPV 1",
          "aVDPV 3",
          "cVDPV1andcVDPV2",
          "CombinationWild1-cVDPV 2",
          "aVDPV 2",
          "VDPV 3",
          "iVDPV 2",
          "VDPV1andcVDPV2",
          "VAPP",
          "cVDPV 3",
          "iVDPV 3",
          "WILD 3",
          "WILD1andWILD3",
          "iVDPV 1",
          "cVDPV2andcVDPV3"
        )
      )

    raw.data$para.case <- raw.data$afp |>
      dplyr::filter(
        stringr::str_detect(cdc.classification.all2, "VDPV|WILD|COMPATIBLE")
      ) |>
      dplyr::mutate(yronset = ifelse(is.na(yronset) == T, 2022, yronset)) # this fix was for the manually added MOZ case
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
      edav_io(
        io = "read",
        file_loc = dplyr::filter(dl_table, grepl("dpt", file)) |>
          dplyr::pull(file), default_dir = NULL
      ) |>
      dplyr::select(
        ctry = adm0_name,
        prov = adm1_name,
        dist = adm2_name,
        year,
        dpt1,
        dpt3
      ) |>
      dplyr::left_join(
        edav_io(
          io = "read",
          file_loc = dplyr::filter(dl_table, grepl("mcv1", file)) |>
            dplyr::pull(file), default_dir = NULL
        ) |>
          dplyr::select(
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
      edav_io(
        io = "read",
        file_loc = dplyr::filter(dl_table, grepl("/es_2001", file)) |>
          dplyr::pull(file), default_dir = NULL
      )
    cli::cli_process_done()

    cli::cli_process_start("8) Loading SIA data from EDAV")
    raw.data$sia <-
      edav_io(
        io = "read",
        file_loc = dplyr::filter(dl_table, grepl("sia", file)) |>
          dplyr::pull(file), default_dir = NULL
      )

    cli::cli_process_done()

    cli::cli_process_start("9) Loading all positives from EDAV")
    raw.data$pos <-
      edav_io(
        io = "read",
        file_loc = dplyr::filter(dl_table, grepl("/pos", file)) |>
          dplyr::pull(file), default_dir = NULL
      )

    cli::cli_process_done()

    cli::cli_process_start("10) Loading other surveillance linelist from EDAV")
    raw.data$other <-
      edav_io(
        io = "read",
        file_loc = dplyr::filter(dl_table, grepl("/other", file)) |>
          dplyr::pull(file), default_dir = NULL
      )

    cli::cli_process_done()

    cli::cli_process_start("11) Loading road network data")
    spatial.data$roads <- edav_io(
      io = "read",
      file_loc = dplyr::filter(dl_table, grepl("roads", file)) |>
        dplyr::pull(file), default_dir = NULL
    )
    cli::cli_process_done()

    cli::cli_process_start("12) Loading city spatial data")
    spatial.data$cities <- edav_io(
      io = "read",
      file_loc = dplyr::filter(dl_table, grepl("cities", file)) |>
        dplyr::pull(file), default_dir = NULL
    )
    cli::cli_process_done()

    cli::cli_process_start("13) Loading SIA Clusters and Calculating Rounds")
    sia.clusters <- edav_io(io = "list", file_loc = paste0(folder, "sia_cluster_cache"),
                            default_dir = NULL) |>
      dplyr::filter(grepl("data_cluster_cache", name)) |>
      dplyr::pull(name)

    sia.cluster.data <- list()

    for(i in 1:length(sia.clusters)){
      sia.cluster.data[[length(sia.cluster.data) + 1]] <- edav_io(io = "read", file_loc = sia.clusters[i], default_dir = NULL)
    }

    raw.data$sia.rounds <- do.call(rbind.data.frame, sia.cluster.data) |>
      dplyr::arrange(adm2guid, sub.activity.start.date) |>
      dplyr::group_by(adm2guid, vaccine.type, cluster) |>
      dplyr::mutate(cdc.round.num = row_number()) |>
      dplyr::ungroup() |>
      dplyr::group_by(adm2guid) |>
      dplyr::mutate(cdc.max.round = max(sub.activity.start.date)) |>
      dplyr::ungroup() |>
      dplyr::mutate(cdc.last.camp = ifelse(cdc.max.round == sub.activity.start.date, 1, 0)) |>
      dplyr::select(sia.code, sia.sub.activity.code, sub.activity.start.date, vaccine.type,
                    place.admin.0, place.admin.1, place.admin.2, adm0guid, adm1guid, adm2guid,
                    cluster, cluster_method, cdc.round.num, cdc.max.round, cdc.last.camp)
    rm(sia.clusters, sia.cluster.data)

    cli::cli_process_done()

    cli::cli_process_start("14) Creating Metadata object")

    polis.cache <- edav_io(
      io = "read",
      file_loc = dplyr::filter(dl_table, grepl("cache", file)) |>
        dplyr::pull(file), default_dir = NULL
    ) |>
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
      dplyr::arrange(dplyr::desc(dateonset)) |>
      dplyr::slice(1) |>
      dplyr::pull(place.admin.0)


    raw.data$metadata$most_recent_afp <- max(raw.data$afp$dateonset, na.rm = TRUE)
    raw.data$metadata$most_recent_afp_loc <- raw.data$afp |>
      dplyr::arrange(dplyr::desc(dateonset)) |>
      dplyr::slice(1) |>
      dplyr::pull(place.admin.0)


    raw.data$metadata$most_recent_env <- max(raw.data$es$collect.date, na.rm = TRUE)
    raw.data$metadata$most_recent_env_loc <- raw.data$es |>
      dplyr::arrange(dplyr::desc(collect.date)) |>
      dplyr::slice(1) |>
      dplyr::pull(ADM0_NAME)


    raw.data$metadata$most_recent_sia <- max(raw.data$sia$sub.activity.start.date)
    raw.data$metadata$most_recent_sia_code <- raw.data$sia |>
      dplyr::arrange(dplyr::desc(sub.activity.start.date)) |>
      dplyr::slice(1) |>
      dplyr::pull(sia.code)
    raw.data$metadata$most_recent_sia_location <- raw.data$sia |>
      dplyr::arrange(dplyr::desc(sub.activity.start.date)) |>
      dplyr::slice(1) |>
      dplyr::pull(place.admin.0)
    raw.data$metadata$most_recent_sia_vax <- raw.data$sia |>
      dplyr::arrange(dplyr::desc(sub.activity.start.date)) |>
      dplyr::slice(1) |>
      dplyr::pull(vaccine.type)

    raw.data$metadata$most_recent_vdpv_class_change_date <- raw.data$pos$vdpvclassificationchangedate |>
      lubridate::as_date() |>
      max(na.rm = T)

    rm(polis.cache)

    cli::cli_process_done()

    cli::cli_process_start("15) Clearing out unused memory")
    gc()
    cli::cli_process_done()
  }

  if(create.cache){
    cli::cli_process_start("16) Caching processed data")

    out <- split_concat_raw_data(action = "split", split.years = c(2000, 2016, 2019), raw.data.all = raw.data)

    current.year <- lubridate::year(Sys.time())

    out_files <- out$split.years |>
      dplyr::mutate(
        file_name = ifelse(grepl(current.year, tag), "recent", stringr::str_replace_all(tag, "-", ".")),
        file_name = paste0("raw.data.", file_name, ".rds")
      )

    if (!recreate.static.files) {
      out_files <- out_files |> dplyr::filter(grepl("recent", file_name))
    }

    for (i in 1:nrow(out_files)) {
      edav_io(
        io = "write",
        file_loc = file.path(folder, paste0("analytic/", dplyr::pull(out_files[i, ], file_name))),
        obj = out[[dplyr::pull(out_files[i, ], tag)]],
        default_dir = NULL
      )
    }

    edav_io(
      io = "write",
      file_loc = file.path(folder, paste0("analytic/spatial.data.rds")),
      obj = spatial.data, default_dir = NULL
    )

    cli::cli_process_done()
  }

  cli::cli_process_start("Checking for duplicates in datasets.")
  raw.data <- duplicate_check(raw.data)
  cli::cli_process_done()

  return(raw.data)
}

#' Extract country specific information from raw polio data
#'
#' @description Filters country specific data from the CDC generated `raw.data` object from [get_all_polio_data()].
#' @import cli dplyr sf stringr
#' @param .raw.data `list` Output of [get_all_polio_data()].
#' @param .country `str` Country name of interest. Case insensitive.
#' @return Named `list` with country specific datasets.
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' ctry.data <- extract_country_data("nigeria", raw.data)
#'
#'
#' @export
extract_country_data <- function(
    .country,
    .raw.data = raw.data) {
  .country <- stringr::str_to_upper(stringr::str_trim(.country))
  if (!(.country %in% unique(.raw.data$ctry.pop$ADM0_NAME))) {
    stop("Invalid country name. Please try again.")
  }

  cli::cli_h1(paste0("--Processing country data for: ", stringr::str_to_title(.country), "--"))
  # Error checking for overlapping ADM0 Names
  ctry.matches <- .raw.data$ctry.pop |>
    dplyr::filter(stringr::str_detect(ADM0_NAME, .country))
  ctrys <- sort(unique(ctry.matches$ADM0_NAME)) |>
    stringr::str_to_title()
  if (length(ctrys) > 1) {
    message("Multiple countries match that name")

    response <- T
    attempts <- 5
    chosen.country <- 0
    while (response) {
      if (attempts == 0) {
        response <- F
        message("Exiting...")
        return()
      } else {
        ctry.options <- paste0(paste0("\n", paste0(1:length(ctrys), ") "), ctrys), collapse = "")
        message("Please choose one by designating a number or type 'q' to quit: ")
        message(ctry.options)

        chosen.country <- readline("Enter only the number to designate a country: \n")
      }

      if (chosen.country == "q") {
        response <- F
        message("Exiting...")
        return()
      }

      chosen.country <- suppressWarnings(as.integer(stringr::str_trim(chosen.country)))
      if (is.na(chosen.country) | !(chosen.country %in% 1:length(ctrys))) {
        message("Invalid choice, please try again.")
        attempts <- attempts - 1
        if (attempts == 1) {
          message(attempts, " attempt remaining\n")
        } else {
          message(attempts, " attempts remaining\n")
        }
        next
      } else {
        chosen.country <- ctrys[chosen.country]
        response <- F
      }
    }
  } else {
    chosen.country <- ctrys[1]
  }

  .country <- chosen.country |> stringr::str_to_upper()
  ctry.data <- list()
  steps <- 1

  if (!is.null(.raw.data$global.ctry)) {
    cli::cli_process_start(paste0(steps, ") Subsetting country spatial data\n"))
    ctry.data$ctry <- .raw.data$global.ctry |>
      dplyr::filter(stringr::str_detect(ADM0_NAME, .country))
    ctry.data$ctry <- dplyr::filter(
      ctry.data$ctry,
      stringr::str_to_upper(ADM0_SOVRN) == stringr::str_to_upper(chosen.country)
    )
    .country <- unique(ctry.data$ctry$ADM0_NAME)

    ctry.data$prov <- .raw.data$global.prov |>
      dplyr::filter(ADM0_NAME == .country)

    ctry.data$dist <- .raw.data$global.dist |>
      dplyr::filter(ADM0_NAME == .country)

    ctry.data$name <- ctry.data$ctry$ADM0_NAME
    ctry.data$vis.name <- ctry.data$ctry$ADM0_VIZ_NAME
    ctry.data$ctry.code <- ctry.data$ctry$WHO_CODE

    cli::cli_process_done()

    steps <- steps + 1
    cli::cli_process_start(paste0(steps, ") Extracting bordering geometries for reference"))

    error1 <- F
    tryCatch(
      {
        a <- sf::st_touches(ctry.data$ctry, .raw.data$global.dist, sparse = F)
        ctry.data$proximal.dist <- .raw.data$global.dist[a, ]
      },
      error = function(cond) {
        message("There was an error in extracting district borders.")
        error1 <<- T
      }
    )

    if (error1) {
      tryCatch(
        {
          message("Attempting fix by toggling sf::sf_use_s2(F).")
          sf::sf_use_s2(F)
          a <- sf::st_touches(ctry.data$ctry, .raw.data$global.dist, sparse = F)
          ctry.data$proximal.dist <- .raw.data$global.dist[a, ]
          sf::sf_use_s2(T)
        },
        error = function(cond) {
          message("Unable to fix spatial file errors for district borders.")
        }
      )
    }

    error2 <- F
    tryCatch(
      {
        a <- sf::st_touches(ctry.data$ctry, .raw.data$global.ctry, sparse = F)
        ctry.data$proximal.ctry <- .raw.data$global.ctry[a, ]
      },
      error = function(cond) {
        message("There was an error in extracting country borders.")
        error2 <<- T
      }
    )

    if (error2) {
      tryCatch(
        {
          message("Attempting fix by toggling sf::sf_use_s2(F).")
          sf::sf_use_s2(F)
          a <- sf::st_touches(ctry.data$ctry, .raw.data$global.ctry, sparse = F)
          ctry.data$proximal.ctry <- .raw.data$global.ctry[a, ]
          sf::sf_use_s2(T)
        },
        error = function(cond) {
          message("Unable to fix spatial file errors for district borders.")
        }
      )
    }

    cli::cli_process_done()
    steps <- steps + 1
    cli::cli_process_start(paste0(steps, ") Pulling data from OSM for Roads"))

    error3 <- F
    tryCatch(
      {
        ctry.data$roads <- .raw.data$roads |>
          sf::st_intersection(ctry.data$ctry)
      },
      error = function(cond) {
        message("Unable to pull data for Roads. Toggling sf::sf_use_s2(F).")
        error3 <<- T
      }
    )

    error4 <- F
    if (error3) {
      tryCatch(
        {
          sf::sf_use_s2(F)
          ctry.data$roads <- .raw.data$roads |>
            sf::st_intersection(ctry.data$ctry)
          sf::sf_use_s2(T)
        },
        error = function(cond) {
          message(paste0(
            "sf_use_s2(F) failed. Using st_make_valid() on ctry.data$ctry.\n",
            " This fix in some cases can cause inaccurate road maps.\n",
            " If so, it is recommended to fix the spatial files."
          ))
          error4 <<- T
        }
      )
    }

    if (error4) {
      tryCatch(
        {
          sf::sf_use_s2(F)
          ctry.data$roads <- raw.data$roads |>
            sf::st_intersection(sf::st_make_valid(ctry.data$ctry))
          sf::sf_use_s2(T)
        },
        error = function(cond) {
          message("Unable to fix spatial file errors in road maps.")
        }
      )
    }

    cli::cli_process_done()
    steps <- steps + 1
    cli::cli_process_start(paste0(steps, ") Pulling data from OSM for Cities"))

    error5 <- F
    tryCatch(
      {
        ctry.data$cities <- .raw.data$cities |>
          sf::st_intersection(ctry.data$ctry)
      },
      error = function(cond) {
        message("Unable to pull data for Cities. Toggling sf::sf_use_s2(F).")
        error5 <<- T
      }
    )

    error6 <- F
    if (error5) {
      tryCatch(
        {
          sf::sf_use_s2(F)
          ctry.data$cities <- .raw.data$cities |>
            sf::st_intersection(ctry.data$ctry)
          sf::sf_use_s2(T)
        },
        error = function(cond) {
          message(paste0(
            "sf_use_s2(F) failed. Using st_make_valid() on the ctry.data$ctry.\n",
            " This fix in some cases can cause inaccurate city maps.\n",
            " If so, it is recommended to fix the spatial files."
          ))
          error6 <<- T
        }
      )
    }

    if (error6) {
      tryCatch(
        {
          sf::sf_use_s2(F)
          ctry.data$cities <- raw.data$cities |>
            sf::st_intersection(sf::st_make_valid(ctry.data$ctry))
          sf::sf_use_s2(T)
        },
        error = function(cond) {
          message("Unable to fix spatial file errors in city maps.")
        }
      )
    }

    cli::cli_process_done()
    steps <- steps + 1
  }
  cli::cli_process_start(paste0(steps, ") Prepping AFP linelist data"))

  ctry.data$afp.all <- .raw.data$afp |>
    # filter(stringr::str_detect(place.admin.0, .country)) |>
    dplyr::filter(place.admin.0 == .country) |>
    dplyr::filter(!is.na(lon) & !is.na(lat)) |>
    sf::st_as_sf(
      coords = c(x = "lon", y = "lat"),
      crs = sf::st_crs(ctry.data$ctry)
    ) |>
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

  ctry.data$afp.all.2 <- .raw.data$afp |>
    # filter(stringr::str_detect(place.admin.0, .country)) |>
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

  ctry.data$afp <- .raw.data$afp |>
    # filter(stringr::str_detect(place.admin.0, .country)) |>
    dplyr::filter(place.admin.0 == .country) |>
    dplyr::filter(!is.na(lon) & !is.na(lat)) |>
    dplyr::filter(!(
      cdc.classification.all %in% c("PENDING", "NPAFP", "COMPATIBLE", "UNKNOWN", "NOT-AFP")
    )) |>
    sf::st_as_sf(
      coords = c(x = "lon", y = "lat"),
      crs = sf::st_crs(ctry.data$ctry)
    ) |>
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

  ctry.data$afp.2 <- .raw.data$afp |>
    # filter(stringr::str_detect(place.admin.0, .country)) |>
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

  ctry.data$afp.epi <- .raw.data$afp.epi |>
    dplyr::filter(place.admin.0 == .country)
  # filter(stringr::str_detect(place.admin.0, .country))

  ctry.data$para.case <- ctry.data$afp.epi |>
    dplyr::filter(
      cdc.classification.all2 %in% c("cVDPV 2", "VDPV 1", "VDPV 2", "WILD 1", "cVDPV 1", "COMPATIBLE")
    ) |>
    dplyr::mutate(yronset = ifelse(is.na(yronset) == T, 2022, yronset)) # this fix was for the manually added MOZ case

  cli::cli_process_done()
  steps <- steps + 1
  cli::cli_process_start(paste0(steps, ") Prepping population data"))

  ctry.data$ctry.pop <- .raw.data$ctry.pop |>
    dplyr::filter(ADM0_NAME == .country) |>
    dplyr::select(year,
      ctry = ADM0_NAME,
      u15pop,
      adm0guid,
      datasource
    )

  ctry.data$prov.pop <- .raw.data$prov.pop |>
    dplyr::filter(ADM0_NAME == .country) |>
    dplyr::mutate(ADM0_NAME = .country) |>
    dplyr::select(year,
      ctry = ADM0_NAME,
      prov = ADM1_NAME,
      u15pop = u15pop.prov,
      adm0guid = ADM0_GUID,
      adm1guid,
      datasource
    )

  ctry.data$dist.pop <- .raw.data$dist.pop |>
    dplyr::filter(ADM0_NAME == .country) |>
    # filter(stringr::str_detect(ADM0_NAME, .country)) |>
    dplyr::mutate(ADM0_NAME = .country) |>
    dplyr::select(year,
      ctry = ADM0_NAME,
      prov = ADM1_NAME,
      dist = ADM2_NAME,
      u15pop,
      adm0guid = ADM0_GUID,
      adm1guid,
      adm2guid,
      datasource
    )

  cli::cli_process_done()
  steps <- steps + 1
  cli::cli_process_start(paste0(steps, ") Prepping positives data"))
  ctry.data$pos <- .raw.data$pos |>
    dplyr::filter(place.admin.0 == .country)
  # filter(stringr::str_detect(place.admin.0, .country)) |>
  cli::cli_process_done()
  steps <- steps + 1
  cli::cli_process_start(paste0(steps, ") Attaching ES data"))
  ctry.data$es <- .raw.data$es |>
    dplyr::filter(ADM0_NAME == .country)
  cli::cli_process_done()
  steps <- steps + 1
  cli::cli_process_start(paste0(steps, ") Attaching SIA data"))
  ctry.data$sia <- .raw.data$sia |>
    dplyr::filter(place.admin.0 == .country)
  cli::cli_process_done()

  cli::cli_process_start("Attaching metadata from get_all_polio_data()")
  ctry.data$metadata <- .raw.data$metadata
  cli::cli_process_done()

  gc()

  return(ctry.data)
}


#' Assess duplicates in the get_all_polio_data() output
#'
#' @description
#' Checks for duplicate records in AFP, other, SIA, and Virus datasets.
#' @import dplyr cli
#' @param .raw.data Named `list` output of [get_all_polio_data()]
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' raw.data <- duplicate_check(raw.data)
#' @export
#'
duplicate_check <- function(.raw.data = raw.data) {
  if (nrow(.raw.data$afp[duplicated(.raw.data$afp[, c("epid", "place.admin.0", "dateonset")]), ]) > 0) {
    cli::cli_alert_warning("There are potential duplicates in the AFP linelist, please check afp.dupe")
    .raw.data$afp.dupe <- .raw.data$afp |>
      dplyr::group_by(.data$epid, .data$place.admin.0, .data$dateonset) |>
      dplyr::mutate(count = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::filter(count > 1) |>
      dplyr::arrange(.data$epid)
  }

  if (nrow(.raw.data$other[duplicated(.raw.data$other[, c("epid", "place.admin.0", "dateonset")]), ]) > 0) {
    cli::cli_alert_warning("There are potential duplicates in the Other Surveillance linelist, please check other.dupe")
    .raw.data$other.dupe <- .raw.data$other |>
      dplyr::group_by(.data$epid, .data$place.admin.0, .data$dateonset) |>
      dplyr::mutate(count = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::filter(count > 1) |>
      dplyr::arrange(.data$epid)
  }

  if (nrow(.raw.data$sia[duplicated(.raw.data$sia[, c(
    "adm2guid", "sub.activity.start.date", "vaccine.type", "age.group",
    "status", "lqas.loaded", "im.loaded"
  )]), ]) > 0) {
    cli::cli_alert_warning("There are potential duplicates in the SIA data, please check sia.dupe")
    .raw.data$sia.dupe <- .raw.data$sia |>
      dplyr::group_by(.data$adm2guid, .data$sub.activity.start.date,
                      .data$vaccine.type, .data$age.group, .data$status,
                      .data$lqas.loaded, .data$im.loaded) |>
      dplyr::mutate(count = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::filter(count > 1) |>
      dplyr::arrange(.data$sia.sub.activity.code)
  }

  if (nrow(.raw.data$es[duplicated(.raw.data$es[, c(
    "env.sample.id", "virus.type", "emergence.group", "nt.changes",
    "site.id", "collection.date", "collect.yr"
  )]), ]) > 0) {
    cli::cli_alert_warning("There are potential duplicates in the ES data, please check es.dupe")
    .raw.data$es.dupe <- .raw.data$es |>
      dplyr::group_by(.data$env.sample.id, .data$virus.type, .data$emergence.group,
                      .data$nt.changes, .data$site.id, .data$collection.date,
                      .data$collect.yr) |>
      dplyr::mutate(es.dups = dplyr::n()) |>
      dplyr::filter(.data$es.dups > 1) |>
      dplyr::select(dplyr::all_of(c("env.sample.manual.edit.id", "env.sample.id",
                                  "sample.id", "site.id", "site.code", "site.name",
                                  "sample.condition", "collection.date", "virus.type",
                                  "nt.changes", "emergence.group", "collect.date", "collect.yr", "es.dups")
                                  )
      )
  }

  if (nrow(.raw.data$pos[duplicated(.raw.data$pos[, c(
    "epid", "epid.in.polis", "pons.epid", "polis.case.id",
    "env.sample.id", "place.admin.0", "source", "datasource",
    "virustype", "dateonset", "yronset", "ntchanges", "emergencegroup"
  )]), ]) > 0) {
    cli::cli_alert_warning("There are potential duplicates in the Positives data, please check pos.dupe")
    .raw.data$pos.dupe <- .raw.data$pos |>
      dplyr::group_by(
        .data$epid, .data$epid.in.polis, .data$pons.epid,
        .data$polis.case.id, .data$env.sample.id, .data$place.admin.0,
        .data$source, .data$datasource, .data$virustype, .data$dateonset,
        .data$yronset, .data$ntchanges, .data$emergencegroup
      ) |>
      dplyr::mutate(count = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::filter(count > 1) |>
      dplyr::arrange(.data$epid)
  }

  return(.raw.data)
}

#### 3) Secondary SP Functions ####

#' Download district geographic data
#
#' @description Pulls district shapefiles directly from the geodatabase.
#' @import stringr AzureStor dplyr lubridate
#' @param fp `str` Location of geodatabase.
#' @param azcontainer Azure validated container object.
#' @param dist_guid `str array` Array of all district GUIDS that you want to pull.
#' @param ctry_name `str array` Array of all country names that you want to pull.
#' @param end.year `int` Last year you want to pull information for. Default is current year.
#' @param st.year `int` Earlier year of spatial data you want to pull. Default is 2000.
#' @param data.only `bool` Whether to return a tibble with shapefiles or not. Defaults to `FALSE`.
#' @param type `str` Whether to return a spatial object for every year group. Defaults to `NULL`.
#' - `"long"` Return a dataset for every year group.
#' - `NULL` Return a dataset only with unique GUIDs and when they were active.
#' @param version `str` Specify whether to return standard shapefiles or new shapefiles
#' still under evaluation/development. Default is `"standard"`.
#' - `"standard"` Standard shapefiles.
#' - `"dev"`  New shapefiles still under evaluation/development.
#' @returns A `tibble` or `sf` dataframe containing spatial data.
#' @examples
#' dist <- load_clean_dist_sp(ctry_name = c("ALGERIA", "NIGERIA"), st.year = 2019)
#' dist.long <- load_clean_dist_sp(ctry_name = "ALGERIA", st.year = 2019, type = "long")
#'
#' @export
load_clean_dist_sp <- function(azcontainer = suppressMessages(get_azure_storage_connection()),
                               fp = "GID/PEB/SIR/Data/spatial/global.dist.rds",
                               dist_guid = NULL,
                               ctry_name = NULL,
                               end.year = lubridate::year(Sys.Date()),
                               st.year = 2000,
                               data.only = F,
                               type = NULL,
                               version = "standard") {

  if(version == "dev"){
    fp <- "GID/PEB/SIR/Data/spatial_dev/global.dist.rds"
    cli::cli_alert_info("Loading under development district spatial files")
  }else{
    cli::cli_alert_info("Loading district spatial files")
  }

  out <- suppressWarnings(AzureStor::storage_load_rds(azcontainer, fp)) |>
    dplyr::mutate(
      STARTDATE = lubridate::as_date(STARTDATE),
      # Typo in the dist start date (year) in shapefiles. Temporary correcting the start date for South Darfur in Sudan
      STARTDATE = dplyr::if_else(ADM0_GUID == "{3050873E-F010-4C4F-82D1-541E3C4FD887}" & ADM1_GUID == "{0836D898-32B9-4912-AEA2-D07BD6E50ED8}" &
        STARTDATE == "2018-01-01",
      STARTDATE + 365, STARTDATE
      ),

      # Error in shapes of LAR district in FARS province of IRAN.
      # Received confirmation from WHO - Start date should be '2021-01-01'.
      # Manually making corrections until WHO fix it in the original geodatabase.
      STARTDATE = dplyr::if_else(ADM0_GUID == "{2EEA3A5C-8A36-4A18-A7AB-1B927A092A60}" & ADM1_GUID == "{76F33E17-ADB9-4582-A533-4C96286864E3}" &
        GUID == "{54464216-2BD3-4F30-BF2C-3846BEE6805D}" & STARTDATE == "2020-01-01",
      STARTDATE + 366, STARTDATE
      ),
      yr.st = lubridate::year(STARTDATE),
      yr.end = lubridate::year(ENDDATE),
      ADM0_NAME = ifelse(stringr::str_detect(ADM0_NAME, "IVOIRE"), "COTE D IVOIRE", ADM0_NAME)
    ) %>%
    # remove the ouad eddahab in Morocco which started and ended the same year and causes overlap
    dplyr::filter(!GUID == "{AE526BC0-8DC3-411C-B82E-75259AD3598C}") %>%
    # this filters based on dates set in RMD
    dplyr::filter(yr.st <= end.year & (yr.end >= st.year | yr.end == 9999)) %>%
    {
      if (is.null(dist_guid)) {
        .
      } else {
        dplyr::filter(., GUID %in% dist_guid)
      }
    } %>%
    {
      if (is.null(ctry_name)) {
        .
      } else {
        dplyr::filter(., ADM0_NAME %in% ctry_name)
      }
    }

  if (data.only) {
    out <- dplyr::as_tibble(out)
    return(out)
  }

  if (!is.null(type) && type == "long") {
    df.list <- lapply(st.year:end.year, function(i) f.yrs.01(out, i))
    out <- do.call(rbind, df.list)
  }

  return(out)
}

#' Download province geographic data
#'
#' @description Pulls province shapefiles directly from the geodatabase
#' @import stringr AzureStor lubridate dplyr
#' @param azcontainer Azure validated container object
#' @param fp `str` Location of geodatabase.
#' @param prov_guid `str array` Array of all province GUIDS that you want to pull.
#' @param prov_name `str array` Array of all province names that you want to pull.
#' @param ctry_name `str array` Array of all country names that you want to pull.
#' @param end.year `int` Last year you want to pull information for. Default is current year.
#' @param st.year `int` Earlier year of spatial data you want to pull. Default is 2000.
#' @param data.only `bool` Whether to return a tibble with shapefiles or not. Defaults to `FALSE`.
#' @param type `str` Whether to return a spatial object for every year group. Defaults to `NULL`.
#' - `"long"` Return a dataset for every year group.
#' - `NULL` Return a dataset only with unique GUIDs and when they were active.
#' @param version `str` Specify whether to return standard shapefiles or new shapefiles
#' still under evaluation/development. Default is `"standard"`.
#' - `"standard"` Standard shapefiles.
#' - `"dev"`  New shapefiles still under evaluation/development.
#' @returns A `tibble` or `sf` dataframe containing spatial data.
#' @examples
#' \dontrun{
#' prov <- load_clean_prov_sp(ctry_name = c("ALGERIA", "NIGERIA"), st.year = 2019)
#' prov.long <- load_clean_prov_sp(ctry_name = "ALGERIA", st.year = 2019, type = "long")
#' }
#' @export
load_clean_prov_sp <- function(azcontainer = suppressMessages(get_azure_storage_connection()),
                               fp = "GID/PEB/SIR/Data/spatial/global.prov.rds",
                               prov_guid = NULL,
                               prov_name = NULL,
                               ctry_name = NULL,
                               end.year = lubridate::year(Sys.Date()),
                               st.year = 2000,
                               data.only = F,
                               type = NULL,
                               version = "standard") {

  if(version == "dev"){
    fp <- "GID/PEB/SIR/Data/spatial_dev/global.prov.rds"
    cli::cli_alert_info("Loading under development province spatial files")
  }else{
    cli::cli_alert_info("Loading province spatial files")
  }

  out <- suppressWarnings(AzureStor::storage_load_rds(azcontainer, fp)) |>
    dplyr::mutate(
      yr.st = lubridate::year(STARTDATE),
      yr.end = lubridate::year(ENDDATE),
      ADM0_NAME = ifelse(stringr::str_detect(ADM0_NAME, "IVOIRE"), "COTE D IVOIRE", ADM0_NAME)
    ) %>%
    # this filters based on dates set in RMD
    dplyr::filter(yr.st <= end.year & (yr.end >= st.year | yr.end == 9999)) %>%
    {
      if (is.null(prov_guid)) {
        .
      } else {
        dplyr::filter(., GUID %in% prov_guid)
      }
    } %>%
    {
      if (is.null(ctry_name)) {
        .
      } else {
        dplyr::filter(., ADM0_NAME %in% ctry_name)
      }
    } %>%
    {
      if (is.null(prov_name)) {
        .
      } else {
        dplyr::filter(., ADM1_NAME %in% prov_name)
      }
    }

  if (data.only) {
    out <- dplyr::as_tibble(out)
    return(out)
  }

  if (!is.null(type) && type == "long") {
    df.list <- lapply(st.year:end.year, function(i) f.yrs.01(out, i))
    out <- do.call(rbind, df.list)
  }

  return(out)
}

#' Download country georgraphic data
#'
#' @description Pulls country shapefiles directly from the geodatabase.
#' @import stringr AzureStor dplyr lubridate
#' @param azcontainer Azure validated container object
#' @param fp `str` Location of geodatabase.
#' @param ctry_guid `str array` Array of all country GUIDS that you want to pull.
#' @param ctry_name `str array` Array of all country names that you want to pull.
#' @param end.year `int` Last year you want to pull information for. Default is current year.
#' @param st.year `int` Earlier year of spatial data you want to pull. Default is 2000.
#' @param data.only `bool` Whether to return a tibble with shapefiles or not. Defaults to `FALSE`.
#' @param type `str` Whether to return a spatial object for every year group. Defaults to `NULL`.
#' - `"long"` Return a dataset for every year group.
#' - `NULL` Return a dataset only with unique GUIDs and when they were active.
#' @param version `str` Specify whether to return standard shapefiles or new shapefiles
#' still under evaluation/development. Default is `"standard"`.
#' - `"standard"` Standard shapefiles.
#' - `"dev"`  New shapefiles still under evaluation/development.
#' @returns A `tibble` or `sf` dataframe containing spatial data.
#' @examples
#' ctry <- load_clean_ctry_sp(ctry_name = "ALGERIA")
#' ctry.long <- load_clean_ctry_sp(ctry_name = "ALGERIA", type = "long")
#' @export
load_clean_ctry_sp <- function(azcontainer = suppressMessages(get_azure_storage_connection()),
                               fp = "GID/PEB/SIR/Data/spatial/global.ctry.rds",
                               ctry_guid = NULL,
                               ctry_name = NULL,
                               end.year = lubridate::year(Sys.Date()),
                               st.year = 2000,
                               data.only = F,
                               type = NULL,
                               version = "standard") {

  if(version == "dev"){
    fp <- "GID/PEB/SIR/Data/spatial_dev/global.ctry.rds"
    cli::cli_alert_info("Loading under development country spatial files")
  }else{
    cli::cli_alert_info("Loading country spatial files")
  }

  out <- suppressWarnings(AzureStor::storage_load_rds(azcontainer, fp)) |>
    dplyr::mutate(
      yr.st = lubridate::year(STARTDATE),
      yr.end = lubridate::year(ENDDATE),
      ADM0_NAME = ifelse(stringr::str_detect(ADM0_NAME, "IVOIRE"), "COTE D IVOIRE", ADM0_NAME)
    ) %>%
    # this filters based on dates set in RMD
    dplyr::filter(yr.st <= end.year & (yr.end >= st.year | yr.end == 9999)) %>%
    {
      if (is.null(ctry_guid)) {
        .
      } else {
        dplyr::filter(., GUID %in% ctry_guid)
      }
    } %>%
    {
      if (is.null(ctry_name)) {
        .
      } else {
        dplyr::filter(., ADM0_NAME %in% ctry_name)
      }
    }

  if (data.only) {
    out <- dplyr::as_tibble(out)
    return(out)
  }

  if (!is.null(type) && type == "long") {
    df.list <- lapply(st.year:end.year, function(i) f.yrs.01(out, i))
    out <- do.call(rbind, df.list)
  }

  return(out)
}

#### 4) Misc ####

#' Utility function to fix years
#'
#' @param df tibble: dataframe to be used for fixing years
#' @param yrs numeric array: array of years to fix data
#' @returns long tibble
f.yrs.01 <- function(df, yrs) {
  quo.yrs <- dplyr::enquo(yrs)

  shape01 <- df %>%
    dplyr::filter((yr.st <= !!quo.yrs & yr.end >= !!quo.yrs) |
      yr.st == !!quo.yrs) %>%
    dplyr::mutate(active.year.01 = !!quo.yrs)

  return(shape01)
}


#' Split or concatenate raw.data by year
#'
#' @param action Can either be to `concat` or `split`
#' @param split.years an array of years by which data should be split
#' @param raw.data.all a list of data objects to be split
#' @param raw.data.post.2019 a list of data objects to be concatenated
#' @param raw.data.2016.2019 a list of data objects to be concatenated
#' @param raw.data.2001.2016 a list of data objects to be concatenated
#' @returns list of lists or a single concatenated list
split_concat_raw_data <- function(
    action,
    split.years = NULL,
    raw.data.all = NULL,
    raw.data.post.2019 = NULL,
    raw.data.2016.2019 = NULL,
    raw.data.2001.2016 = NULL) {
  actions <- c("concat", "split")

  if (!action %in% actions) {
    stop("`action` must be either `concat` or `split`")
  }

  current.year <- lubridate::year(Sys.time())

  key.tables <- c("afp", "afp.epi", "para.case", "es", "sia", "pos", "other", "dist.pop", "prov.pop", "ctry.pop", "coverage")

  static.tables <- c("metadata")

  if (action == "split") {
    if (is.null(split.years) | !is.numeric(split.years)) {
      stop("You must provide a numeric array of years to split by and the years must be between 2000-current year")
    }

    if (is.null(raw.data.all)) {
      stop("You must provide a complete raw.data dataset to be split")
    }

    if (!current.year %in% split.years) {
      split.years <- c(split.years, current.year) |> sort()
    }

    split.years <- lapply(1:(length(split.years) - 1), function(x) {
      dplyr::tibble(
        start.yr = split.years[x],
        end.yr = ifelse(
          split.years[x + 1] == current.year,
          current.year,
          split.years[x + 1] - 1
        )
      )
    }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(tag = paste0(start.yr, "-", end.yr))

    key.table.vars <- dplyr::tibble(
      "data" = key.tables,
      "year.var" = c(rep("yronset", 3), "collect.yr", "yr.sia", rep("yronset", 2), rep("year", 4))
    )


    out <- list()

    for (i in 1:nrow(split.years)) {
      for (j in 1:nrow(key.table.vars)) {
        out[[dplyr::pull(split.years[i, ], tag)]][[key.table.vars[j, ] |> dplyr::pull("data")]] <-
          raw.data.all[[key.table.vars[j, ] |> dplyr::pull("data")]] |>
          dplyr::filter(
            !!sym(key.table.vars[j, ] |> dplyr::pull("year.var")) >= dplyr::pull(split.years[i, ], start.yr) &
              !!sym(key.table.vars[j, ] |> dplyr::pull("year.var")) <= dplyr::pull(split.years[i, ], end.yr)
          )
      }


      for (k in 1:length(static.tables)) {
        out[[dplyr::pull(split.years[i, ], tag)]][[static.tables[k]]] <-
          raw.data.all[[static.tables[k]]]
      }
    }

    out$split.years <- split.years

    return(out)
  }

  if (action == "concat") {
    if (sum(is.null(raw.data.post.2019), is.null(raw.data.2016.2019), is.null(raw.data.2001.2016)) > 1) {
      stop("You must include at least two subsets of raw.data files to concatenate")
    }

    input <- list()

    if (!is.null(raw.data.post.2019)) {
      input[["raw.data.post.2019"]] <- raw.data.post.2019
    }

    if (!is.null(raw.data.post.2019)) {
      input[["raw.data.2016.2019"]] <- raw.data.2016.2019
    }

    if (!is.null(raw.data.2001.2016)) {
      input[["raw.data.2001.2016"]] <- raw.data.2001.2016
    }

    to.concat <- names(input)

    out <- list()

    key.table.vars <- dplyr::tibble(
      "data" = key.tables,
      "year.var" = c(rep("yronset", 3), "collect.yr", "yr.sia", rep("yronset", 2), rep("year", 4))
    )

    for (i in key.table.vars$data) {
      out[[i]] <- lapply(to.concat, function(x) {
        input[[x]][[i]]
      }) |>
        dplyr::bind_rows()
    }

    for (i in static.tables) {
      out[[i]] <- raw.data.post.2019[[i]]
    }

    return(out)
  }
}

#' Check GUIDs present in the AFP linelist but not in the pop files
#'
#' The function will run a check in the AFP linelist for GUIDs that are not part
#' of the spatial files. In these instances, typically, unknown GUIDs are part of the
#' new geodatabase from WHO that get released in the next updated geodatabase. Therefore,
#' this function should be used only if necessary. For example, in instances where mapping an AFP case into a
#' a district is critical and the shapefile from `extract_country_data()` is not yet updated.
#' @param ctry.data Named `list` output of `extract_country_data()`, with spatial data attached.
#'
#' @return A `list` containing errors in province and district GUIDs.
#' @examples
#' raw.data <- get_all_polio_data() # must contain spatial data to run the function
#' ctry.data <- extract_country_data("algeria", raw.data)
#' error.list <- check_afp_guid_ctry_data(ctry.data)
#'
#' @export
check_afp_guid_ctry_data <- function(ctry.data) {
  error_list <- list()

  cli::cli_process_start("Checking province GUIDs")

  prov_mismatches_pop <- setdiff(ctry.data$afp.all.2$adm1guid, ctry.data$prov.pop$adm1guid)
  if (length(prov_mismatches_pop) > 0) {
    cli::cli_alert_warning(paste0(
      "There are ", length(prov_mismatches_pop),
      " GUIDs from afp.all.2 that are not in prov.pop"
    ))
  }
  error_list$prov_mismatches_pop <- ctry.data$afp.all.2 |>
    dplyr::filter(.data$adm1guid %in% prov_mismatches_pop) |>
    dplyr::select("prov", "year", "adm1guid") |>
    unique()

  if ("prov" %in% names(ctry.data)) {
    prov_mismatches_shape <- setdiff(ctry.data$afp.all.2$adm1guid, ctry.data$prov$GUID)
    if (length(prov_mismatches_shape) > 0) {
      cli::cli_alert_warning(paste0(
        "There are ", length(prov_mismatches_shape),
        " GUIDs from afp.all.2 that are not in prov"
      ))
    }
    error_list$prov_mismatches_shape <- ctry.data$afp.all.2 |>
      dplyr::filter(.data$adm1guid %in% prov_mismatches_shape) |>
      dplyr::select("prov", "year", "adm1guid") |>
      unique()
  }


  cli::cli_process_done()

  cli::cli_process_start("Checking district GUIDs")
  dist_mismatches_pop <- setdiff(ctry.data$afp.all.2$adm2guid, ctry.data$dist.pop$adm2guid)

  if (length(dist_mismatches_pop) > 0) {
    cli::cli_alert_warning(paste0(
      "There are ", length(dist_mismatches_pop),
      " GUIDs from afp.all.2 that are not in dist.pop"
    ))
  }
  error_list$dist_mismatches_pop <- ctry.data$afp.all.2 |>
    dplyr::filter(.data$adm2guid %in% dist_mismatches_pop) |>
    dplyr::select("prov", "dist", "year", "adm2guid") |>
    unique()

  if (!"dist" %in% names(ctry.data$dist)) {
    dist_mismatches_shape <- setdiff(ctry.data$afp.all.2$adm2guid, ctry.data$dist$GUID)
    if (length(dist_mismatches_shape) > 0) {
      cli::cli_alert_warning(paste0(
        "There are ", length(dist_mismatches_shape),
        " GUIDs from afp.all.2 that are not in dist"
      ))
    }
    error_list$dist_mismatches_shape <- ctry.data$afp.all.2 |>
      dplyr::filter(.data$adm2guid %in% dist_mismatches_shape) |>
      dplyr::select("prov", "dist", "year", "adm2guid") |>
      unique()
  }

  cli::cli_process_done()

  return(error_list)
}

#' Fix unknown GUIDs in the AFP linelist
#'
#' Fix unknown GUIDs in the AFP linelist by obtaining GUIDs found in the pop files. It attempts to replace
#' the unknown GUIDs from the AFP linelist by using geographic info for a specific year that coincides with the case date
#' and uses the GUIDs contained in the current spatial data instead.
#' @param afp.data `tibble` AFP linelist (afp.all.2).
#' @param pop.data `tibble` Population file (prov.pop or dist.pop).
#' @param guid_list `str list` Unknown GUIDs from the AFP linelist.
#' This is the output of `check_afp_guid_ctry_data()`.
#' @param spatial_scale `str` The spatial scale to impute data. Either `"prov"` or `"dist"`.
#'
#' @return `tibble` AFP data with corrected GUIDs based on the population files.
#' @examples
#' raw.data <- get_all_polio_data()
#' ctry.data <- extract_country_data("algeria")
#' error.list <- check_afp_guid_ctry_data(ctry.data)
#' ctry.data$afp.all.2 <- fix_ctry_data_missing_guids(ctry.data$afp.all.2,
#'                                                    ctry.data$dist.pop,
#'                                                    error.list$dist_mismatches_pop,
#'                                                    "dist")
#'
#' @export
fix_ctry_data_missing_guids <- function(afp.data, pop.data, guid_list, spatial_scale) {
  afp.data <- switch(spatial_scale,
                     "prov" = {
                       afp.data |>
                         dplyr::mutate(adm1guid = dplyr::if_else(.data$adm1guid %in% guid_list, NA, .data$adm1guid)) |>
                         dplyr::left_join(pop.data, by = c("ctry", "prov", "year", "adm0guid")) |>
                         dplyr::mutate(adm1guid = dplyr::coalesce(.data$adm1guid.x, .data$adm1guid.y)) |>
                         dplyr::select(-dplyr::any_of(c("adm1guid.x", "adm1guid.y")))
                     },
                     "dist" = {
                       afp.data |>
                         dplyr::mutate(adm2guid = dplyr::if_else(.data$adm2guid %in% guid_list, NA, .data$adm2guid)) |>
                         dplyr::left_join(pop.data, by = c("ctry", "prov", "dist", "year",
                                                    "adm0guid", "adm1guid")) |>
                         dplyr::mutate(adm2guid = dplyr::coalesce(.data$adm2guid.x, .data$adm2guid.y)) |>
                         dplyr::select(-dplyr::any_of(c("adm2guid.x", "adm2guid.y")))
                     }
                     )

  return(afp.data)
}
#' Compress PNG files using pngquant
#'
#' Compress PNG files. The software pngquant is required to use this function.
#' It attempts to reduce the file size of images without major loss in image quality. Files sizes can be reduced
#' from 30-60% using this function. The compressed file will be outputted to the same folder as the original image.
#' @param img `str` File path to the png file.
#' @param pngquant_path `str` File path to pngquant executable file (pngquant.exe).
#' @param suffix `str` Optional parameter to add a suffix to the compressed image.
#' @examples
#' \dontrun{
#' img_path <- "C:/Users/ABC1/Desktop/pic1.png"
#' pngquant_path <- "C:/Users/ABC1/Downloads/pngquant.exe"
#' compress_png(img_path, pngquant_path, "_compressed")
#' }
#'
#' @export
compress_png <- function(img, pngquant_path = NULL, suffix = "") {

  # Check
  if (!stringr::str_detect(img, ".png$")) {
    stop("Only .png files can be compressed.")

  }
  if (is.null(pngquant_path)) {
    stop("Please pass an argument to the pngquant_path parameter.")
  }

  if (!file.exists(pngquant_path)) {
    stop("Path pngquant.exe not found.")
    }

  # Compress the image file
  system2(pngquant_path, args = c("--ext", paste0(suffix, ".png"), "--force", img))
}

#' Get the columns where records differ in a group
#'
#' Get the columns where duplicates differ after performing a [dplyr::distinct()] operation.
#' In some instances, two records might exist with the same unique identifier. In datasets with lots of columns,
#' it is difficult to figure out which columns these potential duplicates differ. The function outputs the columns
#' where records with the same unique identifier differ.
#'
#' @importFrom dplyr syms mutate everything across group_by summarise filter
#' @importFrom tidyr pivot_longer
#' @param df `df` or `tibble` Dataframe with at least one column containing unique identifiers and other columns.
#' @param id_col `str` Column used as a unique identifier for records.
#'
#' @return `tibble` A tibble showing the columns where duplicates differ.
#' @examples
#' df1 <- tibble::tibble(col1 = c(1,1,2), col2 = c("a", "b", "c"), col3 = c(1,1,3))
#' diff_cols <- get_diff_cols(df1, "col1")
#'
#' @export
get_diff_cols <- function(df, id_col) {
  col_with_differences <- df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) as.character(x))) |>
    dplyr::group_by(!!!syms(id_col)) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), \(x) length(unique(x)) == 1)) |>
    tidyr::pivot_longer(cols = -c(id_col), names_to = "column_name", values_to = "logical") |>
    dplyr::filter(logical == FALSE) |>
    dplyr::group_by(!!!dplyr::syms(id_col)) |>
    dplyr::summarise(col_with_diff = paste(unique(.data$column_name), collapse = ", "))

  return(col_with_differences)
}


