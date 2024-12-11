# DAL - Data Access Layer

#### 1) Utility functions ####

#' Validate connection to EDAV
#'
#' Generate token which connects to CDC EDAV resources and
#' validates that the individual still has access. The current tenant ID
#' is hard coded for CDC resources.
#'
#' @param app_id `str` Application ID defaults to `"04b07795-8ddb-461a-bbee-02f9e1bf7b46"`,
#' this can be changed if you have a service principal.
#' @param auth `str` Authorization type defaults to `"authorization_code"`,
#' this can be changed if you have a service principal.
#'
#' Valid values are:`"authorization_code"`, `"device_code"`,
#' `"client_credentials"`, `"resource_owner"`, `"on_behalf_of"`.
#'
#' See **Details** of [AzureAuth::get_azure_token()] for further details.
#' @param posit_yaml_path `str` Path to the YAML file in Posit workbench.
#' If `NULL`, the path will be in `"~/credentials/posit_workbench_creds.yaml"`.
#' @param ... additional parameters passed to [AzureAuth::get_azure_token()].
#' @returns Azure container verification
#' @examples
#' azcontainer <- get_azure_storage_connection()
#'
#' @export
get_azure_storage_connection <- function(
    app_id = "04b07795-8ddb-461a-bbee-02f9e1bf7b46",
    auth = "authorization_code",
    posit_yaml_path = NULL,
    ...) {

  if (stringr::str_starts(Sys.getenv("SF_PARTNER"), "posit_workbench")) {
    if (is.null(posit_yaml_path)) {
      creds_path <- "~/credentials/posit_workbench_creds.yaml"
    } else {
      creds_path <- posit_yaml_path
    }
    creds <- yaml::read_yaml(creds_path)
    mytoken <- AzureAuth::get_azure_token(
      resource = "https://storage.azure.com/",
      tenant = "9ce70869-60db-44fd-abe8-d2767077fc8f",
      app = creds$app_id,
      auth_type = NULL,
      password = creds$pw,
      ...
    )
  } else {
    mytoken <- AzureAuth::get_azure_token(
      resource = "https://storage.azure.com/",
      tenant = "9ce70869-60db-44fd-abe8-d2767077fc8f",
      app = app_id,
      auth_type = auth,
      ...
    )
  }

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

#' sirfunctions i/o handler
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Manages read/write/list/create/delete functions for sirfunctions. This function
#' is adapted from [tidypolis_io](https://github.com/nish-kishore/tidypolis/blob/4e2f75e5ee3205b84c5b78f4b1776e2270e1f9ec/R/dal.R#L15).
#'
#' @param obj `str` Object to be loaded into EDAV
#' @param io `str` The type of operation to use. Valid values include:
#' - `"read"`: reads data from the specified `file_path`.
#' - `"write"`: writes data to the specified `file_path`.
#' - `"list"`: lists the files in the specified `file_path`.
#' - `"exists.dir"`: determines whether a directory is present.
#' - `"exists.file"`: determines whether a file is present.
#' - `"create.dir"`: creates a directory to the specified `file_path`.
#' - `"delete"`: deletes a file or folder in the specified `file_path`.
#' @param file_loc `str` Path of file relative to the `default_folder`.
#' @param default_folder `str` The default folder to use. Defaults to `"GID/PEB/SIR`.
#' @param edav `bool` Whether the function should interact with the EDAV environment.
#' Defaults to `TRUE`, otherwise, interacts with files locally.
#' @param azcontainer `Azure container` A container object returned by
#' [get_azure_storage_connection()].
#' @param full_names `bool` If `io="list"`, include the full reference path. Default `TRUE`.
#' @param ... Optional parameters that work with [readr::read_delim()] or [readxl::read_excel()].
#' @returns Conditional on `io`. If `io` is `"read"`, then it will return a tibble. If `io` is `"list"`, it will return a
#' list of file names. Otherwise, the function will return `NULL`. `exists.dir` and `exists.file` will return a `bool`.
#'
#' @examples
#' \dontrun{
#' df <- sirfunctions_io("read", file_loc = "df1.csv") # read file from EDAV
#' # Passing parameters that work with read_csv or read_excel, like sheet or skip.
#' df2 <- sirfunctions_io("read", file_loc = "df2.xlsx", sheet = 1, skip = 2)
#' list_of_df <- list(df_1 = df, df_2 = df)
#' # Saves df to the test folder in EDAV
#' sirfunctions_io("write", file_loc = "Data/test/df.csv", obj = df)
#' # Saves list_of_df as an Excel file with multiple sheets.
#' sirfunctions_io("write", file_loc = "Data/test/df.xlsx", obj = list_of_df)
#' sirfunctions_io("exists.dir", "Data/nonexistentfolder") # returns FALSE
#' sirfunctions_io("exists.file", file_loc = "Data/test/df1.csv") # returns TRUE
#' sirfunctions_io("create", "Data/nonexistentfolder") # creates a folder called nonexistentfolder
#' sirfunctions_io("list") # list all files from the default directory
#' }
#'
#' @export
sirfunctions_io <- function(
    io,
    default_folder = "GID/PEB/SIR",
    file_loc,
    obj = NULL,
    edav = TRUE,
    azcontainer = suppressMessages(sirfunctions::get_azure_storage_connection()),
    full_names = T,
    ...) {
  if (!is.null(file_loc)) {
    if (is.null(default_folder)) {
      file_loc <- file_loc
    } else {
      file_loc <- file.path(default_folder, file_loc)
    }
  } else {
    file_loc <- default_folder
  }

  opts <- c("read", "write", "delete", "list", "exists.dir", "exists.file", "create.dir")

  if (!io %in% opts) {
    stop("io: must be 'read', 'write', 'delete', 'create.dir', 'exists.dir', 'exists.file' or 'list'")
  }

  if (io == "write" & is.null(obj)) {
    stop("Need to supply an object to be written")
  }

  if (io == "list") {
    if (edav) {
      out <- sirfunctions::edav_io(
        io = "list",
        NULL,
        file_loc,
        azcontainer = azcontainer
      )

      if (full_names) {

        return(out)

      } else {
        return(out |> dplyr::mutate(name = basename(name)))
      }
    } else {
      files <- list.files(file_loc, full.names = TRUE)

      get_file_info <- function(file) {
        info <- file.info(file) |>
          dplyr::as_tibble() |>
          dplyr::select(size, isdir, lastModified = mtime) |>
          dplyr::mutate(
            name = file,
            lastModified = lubridate::as_datetime(lastModified)
          ) |>
          dplyr::select(name, size, isdir, lastModified)

        return(info)

      }

      files <- lapply(files, \(x) get_file_info(x)) |>
        dplyr::bind_rows()

      if (full_names) {

        return(files)

      } else {
        files <- files |>
          dplyr::mutate(name = basename(name))

        return(files)

      }
    }
  }

  if (io == "exists.dir") {
    if (edav) {
      return(edav_io("exists.dir",
        NULL,
        file_loc,
        azcontainer = azcontainer
      ))
    } else {
      return(dir.exists(file_loc))
    }
  }

  if (io == "exists.file") {
    if (edav) {
      return(edav_io(
        io = "exists.file",
        NULL,
        file_loc,
        azcontainer = azcontainer
      ))
    } else {
      return(file.exists(file_loc))
    }
  }

  if (io == "read") {
    if (edav) {
      return(edav_io(
        io = "read", NULL,
        file_loc, azcontainer = azcontainer
      ))
    } else {
      if (!grepl(".rds$|.rda$|.csv$|.xlsx$|.xls$", file_loc)) {
        stop("At the moment only 'rds' 'rda' 'csv' 'xlsx' and 'xls' are supported for reading.")
      }

      if (endsWith(file_loc, ".rds")) {
        return(readr::read_rds(file_loc))
      } else if (endsWith(file_loc, ".rda")) {
        return(load(file_loc))
      } else if (endsWith(file_loc, ".csv")) {
        return(readr::read_csv(file_loc))
      } else if (endsWith(file_loc, ".xlsx") | endsWith(file_loc, ".xls")) {
        return(read_excel_from_edav(src = file_loc, ...))
      }
    }
  }

  if (io == "write") {
    if (is.null(obj)) {
      stop("You need to include an object to be written.")
    }

    if (edav) {
      edav_io(io = "write", NULL, file_loc = file_loc, obj = obj, azcontainer = azcontainer)
    } else {
      if (!grepl(".rds$|.rda$|.csv$", file_loc)) {
        stop("At the moment only 'rds' 'rda' 'csv' 'xlsx' and 'xls' are supported for reading.")
      } else if (endsWith(file_loc, ".rds")) {
        readr::write_rds(x = obj, file = file_loc)
      } else if (endsWith(file_loc, ".rda")) {
        save(list = obj, file = file_loc)
      } else if (endsWith(file_loc, ".csv")) {
        readr::write_csv(x = obj, file = file_loc)
      } else if (endsWith(file_loc, ".xlsx") | endsWith(file_loc, ".xls")) {
        writexl::write_xlsx(obj, path = file_loc)
      }
    }
  }

  if (io == "delete") {
    if (edav) {
      edav_io("delete", NULL, file_loc = file_loc, force_delete = T, azcontainer = azcontainer)
    } else {
      file.remove(file_loc)
    }
  }

  if (io == "create.dir") {
    if (edav) {
      edav_io(io = "create", NULL, file_loc = file_loc)
    } else {
      if (dir.exists(file_loc)) {
        file.remove(list.files(file_loc, full.names = TRUE))
      } else {
        dir.create(file_loc)
      }
    }
  }

  return(NULL)
}

#' Helper function to read and write key data to the EDAV environment
#'
#' The function serves as the primary way to interact with the EDAV system from R. It can
#' read, write, create folders, check whether a file or a folder exists, upload files, and list
#' all files in a folder.
#'
#' @param io `str` The type of operation to perform in EDAV.
#' - `"read"` Read a file from EDAV, must be an rds, csv, rda, or xls/xlsx file.
#' - `"write"` Write a file to EDAV, must be an rds, csv, rda, or xls/xlsx file. To
#' write an Excel file with multiple sheets, pass a named list containing the tibbles
#' of interest. See examples.
#' - `"exists.dir"` Returns a boolean after checking to see if a folder exists.
#' - `"exists.file"`Returns a boolean after checking to see if a file exists.
#' - `"create"` Creates a folder and all preceding folders.
#' - `"list"` Returns a tibble with all objects in a folder.
#' - `"upload"` Moves a file of any type to EDAV.
#' - `"delete"` Deletes a file.
#' @param default_dir `str` The default directory in EDAV. `"GID/PEB/SIR"` is the default directory
#' for all SIR data in EDAV. Can be set to `NULL` if you provide the full directory path in `file_loc`.
#' @param file_loc `str` Location to "read", "write", "exists.dir", "exists.file", "create" or "list", can include
#' the information in `default_dir` if you set that parameter to `NULL`.
#' @param obj `robj` Object to be saved, needed for `"write"`. Defaults to `NULL`.
#' @param azcontainer Azure container object returned from [get_azure_storage_connection()].
#' @param force_delete `bool` Use delete io without verification in the command line.
#' @param local_path `str` Local file pathway to upload a file to EDAV. Default is `NULL`.
#' This parameter is only required when passing `"upload"` in the `io` parameter.
#' @param ... Optional parameters that work with [readr::read_delim()] or [readxl::read_excel()].
#' @returns Output dependent on argument passed in the `io` parameter.
#' @examples
#' \dontrun{
#' df <- edav_io("read", file_loc = "df1.csv") # read file from EDAV
#' # Passing parameters that work with read_csv or read_excel, like sheet or skip.
#' df2 <- edav_io("read", file_loc = "df2.xlsx", sheet = 1, skip = 2)
#' list_of_df <- list(df_1 = df, df_2 = df)
#' # Saves df to the test folder in EDAV
#' edav_io("write", file_loc = "Data/test/df.csv", obj = df)
#' # Saves list_of_df as an Excel file with multiple sheets.
#' edav_io("write", file_loc = "Data/test/df.xlsx", obj = list_of_df)
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
    local_path = NULL,
    ...) {
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

    if (!grepl(".rds$|.rda$|.csv$|.xlsx$|.xls$", file_loc)) {
      stop("At the moment only 'rds' 'rda', 'csv', 'xls', and 'xlsx' are supported for reading.")
    }

    if (endsWith(file_loc, ".rds")) {
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

    if (endsWith(file_loc, ".csv")) {
      return(suppressWarnings(AzureStor::storage_read_csv(azcontainer, file_loc, ...)))
    } else if (endsWith(file_loc, ".rda")) {
      return(suppressWarnings(AzureStor::storage_load_rdata(azcontainer, file_loc)))
    } else if (endsWith(file_loc, ".xlsx") | endsWith(file_loc, ".xls")) {
      output <- NULL
      withr::with_tempdir(
        {
          AzureStor::storage_download(azcontainer,
                                      file_loc,
                                      file.path(tempdir(), basename(file_loc)),
                                      overwrite = TRUE
          )
          output <- read_excel_from_edav(src = file.path(tempdir(),
                                                         basename(file_loc)),
                                         ...)
        }
        )
      return(output)
    }
  }

  if (io == "write") {
    if (!grepl(".rds$|.csv$|.xlsx$|.xls$|.png$|.jpg$|.jpeg$", file_loc)) {
      cli::cli_abort(paste0("Please pass a path including the file name in file_loc.",
                            " (i.e., folder/data.csv)"))
    }

    if (endsWith(file_loc, ".rds")) {
      AzureStor::storage_save_rds(object = obj, container = azcontainer, file = file_loc)
    }

    if (endsWith(file_loc, ".csv")) {
      AzureStor::storage_write_csv(object = obj, container = azcontainer, file = file_loc)
    }

    if (endsWith(file_loc, ".xlsx") | endsWith(file_loc, ".xls")) {
      withr::with_tempdir(
        {
          writexl::write_xlsx(obj,
                              path = file.path(tempdir(), basename(file_loc)))

          AzureStor::storage_upload(
            container = azcontainer, dest = file_loc,
            src = file.path(tempdir(), basename(file_loc))
          )
        }
        )
    }

    if ("gg" %in% class(obj)) {
      temp <- tempfile()
      ggplot2::ggsave(filename = paste0(temp, "/", sub(".*\\/", "", file_loc)), plot = obj)
      AzureStor::storage_upload(
        container = azcontainer, dest = file_loc,
        src = paste0(temp, "/", sub(".*\\/", "", file_loc))
      )
      unlink(temp)
    }

    if ("flextable" %in% class(obj)) {
      temp <- tempfile()
      flextable::save_as_image(obj, path = paste0(temp, "/", sub(".*\\/", "", file_loc)))
      AzureStor::storage_upload(
        container = azcontainer, dest = file_loc,
        src = paste0(temp, "/", sub(".*\\/", "", file_loc))
      )
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
#'
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
      .call = FALSE
    )
  }

  tmp_data <- replicate(100, iris, simplify = F) |>
    dplyr::bind_rows() |>
    dplyr::as_tibble()

  tmp_file <- paste0(tempfile(), ".rds")

  readr::write_csv(tmp_data, tmp_file)

  file_size <- file.info(tmp_file)$size

  tick <- Sys.time()

  edav_io(io = "write", file_loc = paste0(folder, "/tmp.rds"), obj = tmp_data,
          default_dir = NULL,
          azcontainer = azcontainer)
  # readr::write_rds(tmp_data, paste0(folder, "/tmp.rds"))

  tock <- Sys.time()

  dt1 <- as.numeric(difftime(tock, tick, units = "secs"))

  tick <- Sys.time()

  x <- edav_io(io = "read", file_loc = paste0(folder, "/tmp.rds"),
               default_dir = NULL,
               azcontainer = azcontainer)
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

  suppressMessages(edav_io(io = "delete", file_loc = paste0(folder, "/tmp.rds"),
                           default_dir = NULL, force_delete = T,
                           azcontainer = azcontainer),
                   classes = c("message", "warning"))
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

#' Obtain a constant variable used in sirfunctions
#'
#' Some links used in certain functions are hardcoded, but may change in the future.
#' For ease of link maintenance, constants will be added to this function.
#'
#' @param constant_name `str` Name of the constant. Valid values include:
#' - `"DEFAULT_EDAV_FOLDER"`
#' - `"CTRY_RISK_CAT"`
#' - `"LAB_LOCATIONS"`
#' - `"DR_TEMPLATE"`
#' - `"SIRFUNCTIONS_GITHUB_TREE"`
#' - `"AFRO_LAB_DATA"`
#' - `"EMRO_LAB_DATA"`
#'
#' @returns `str` A string, typically a file path or a URL.
#' @export
#'
#' @examples
#' get_constant("DEFAULT_EDAV_FOLDER")
get_constant <- function(constant_name) {
  switch(constant_name,
    "DEFAULT_EDAV_FOLDER" = "GID/PEB/SIR",
    "CTRY_RISK_CAT" = "Data/misc/country_prioritization/SG_country_prioritization_GPSAP2025-2026_04Dec2024.csv",
    "LAB_LOCATIONS" = "Data/lab/Routine_lab_testing_locations.csv",
    "DR_TEMPLATE" = "https://raw.githubusercontent.com/nish-kishore/sg-desk-reviews/main/resources/desk_review_template.Rmd",
    "SIRFUNCTIONS_GITHUB_TREE" = "https://api.github.com/repos/nish-kishore/sirfunctions/git/trees",
    "AFRO_LAB_DATA" = "Data/lab/2024-09-20 AFRO Lab Extract (AFP only since 2022).csv",
    "EMRO_LAB_DATA" = "Data/lab/2024-09-20 EMRO Lab Extract (AFP only since 2022).csv"
  )
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
#'
#' @param size `str` Size of data to download. Defaults to `"small"`.
#' - `"small"`: Data from 2019-present.
#' - `"medium"`: Data from 2016-present.
#' - `"large"`: Data from 2001-present.
#' @param data_folder `str` Location of the data folder containing pre-processed POLIS data,
#' spatial files, coverage data, and population data. Defaults to `"GID/PEB/SIR/Data"`.
#' @param polis_folder `str` Location of the POLIS folder. Defaults to `"GID/PEB/SIR/POLIS"`.
#' @param force.new.run `bool` Default `FALSE`, if `TRUE` will run recent data and cache.
#' @param recreate.static.files `bool` Default `FALSE`, if `TRUE` will run all data and cache.
#' @param attach.spatial.data `bool` Default `TRUE`, adds spatial data to downloaded object.
#' @param use_edav `bool` Build raw data list using EDAV files. Defaults to `TRUE`.
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
    data_folder = "GID/PEB/SIR/Data",
    polis_folder = "GID/PEB/SIR/POLIS",
    force.new.run = F,
    recreate.static.files = F,
    attach.spatial.data = T,
    use_edav = TRUE) {# check to see that size parameter is appropriate
if (!size %in% c("small", "medium", "large")) {
  stop("The parameter 'size' must be either 'small', 'medium', or 'large'")
}

# Constant variables
# Each file comes out of these folders
analytic_folder <- file.path(data_folder, "analytic")
polis_data_folder <- file.path(data_folder, "polis")
spatial_folder <- file.path(data_folder, "spatial")
coverage_folder <- file.path(data_folder, "coverage")
pop_folder <- file.path(data_folder, "pop")

# Required files
raw_data_recent_name <- "raw.data.recent.rds"
raw_data_2016_2018_name <- "raw.data.2016.2018.rds"
raw_data_2000_name <- "raw.data.2000.2015.rds"
spatial_data_name <- "spatial.data.rds"
global_ctry_sf_name <- "global.ctry.rds"
global_prov_sf_name <- "global.prov.rds"
global_dist_sf_name <- "global.dist.rds"

# look to see if the recent raw data rds is in the analytic folder
prev_table <- sirfunctions_io("list", NULL, analytic_folder,
  edav = use_edav
)

if (nrow(prev_table) > 0) {
  prev_table <- prev_table |>
    dplyr::filter(grepl(raw_data_recent_name, name)) |>
    dplyr::select("file" = "name", "size", "ctime" = "lastModified")
} else {
  # if empty, make sure to recreate tibble to the right format
  prev_table <- tibble(
    "file" = NA,
    "size" = NA,
    "ctime" = NA
  ) |>
    dplyr::mutate(file = as.character(file),
                  size = as.double(size),
                  ctime = as_datetime(ctime)) |>
    dplyr::filter(!is.na(file))
}

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
  if (use_edav) {
    cli::cli_alert_info("Downloading most recent active polio data from 2019 onwards")
  } else {
    cli::cli_alert_info("Loading most recent active polio data from 2019 onwards")
  }
  raw.data.post.2019 <- sirfunctions_io("read", NULL, prev_table$file, edav = use_edav)

  if (size == "small") {
    raw.data <- raw.data.post.2019
  }

  if (size == "medium") {
    prev_table <- sirfunctions_io("list", NULL, analytic_folder, edav = use_edav) |>
      dplyr::filter(grepl(raw_data_2016_2018_name, name)) |>
      dplyr::select("file" = "name", "size", "ctime" = "lastModified")

    if (use_edav) {
      cli::cli_alert_info("Downloading static polio data from 2016-2019")
    } else {
      cli::cli_alert_info("Loading static polio data from 2016-2019")
    }

    raw.data.2016.2018 <- sirfunctions_io("read", NULL, prev_table$file, edav = use_edav)

    raw.data <- split_concat_raw_data(
      action = "concat",
      raw.data.post.2019 = raw.data.post.2019,
      raw.data.2016.2019 = raw.data.2016.2018
    )
  }

  if (size == "large") {
    prev_table <- sirfunctions_io("list", NULL, analytic_folder,
      edav = use_edav, full_names = TRUE
    ) |>
      dplyr::filter(grepl(raw_data_2016_2018_name, name)) |>
      dplyr::select("file" = "name", "size", "ctime" = "lastModified")

    if (use_edav) {
      cli::cli_alert_info("Downloading static polio data from 2016-2019")
    } else {
      cli::cli_alert_info("Loading static polio data from 2016-2019")
    }
    raw.data.2016.2019 <- sirfunctions_io("read", NULL, prev_table$file, edav = use_edav)

    prev_table <- sirfunctions_io("list", NULL, analytic_folder, edav = use_edav) |>
      dplyr::filter(grepl(raw_data_2000_name, name)) |>
      dplyr::select("file" = "name", "size", "ctime" = "lastModified")

    if (use_edav) {
      cli::cli_alert_info("Downloading static polio data from 2001-2016")
    } else {
      cli::cli_alert_info("Loading static polio data from 2001-2016")
    }

    raw.data.2001.2016 <- sirfunctions_io("read", NULL, prev_table$file, edav = use_edav)

    raw.data <- split_concat_raw_data(
      action = "concat",
      raw.data.post.2019 = raw.data.post.2019,
      raw.data.2016.2019 = raw.data.2016.2019,
      raw.data.2001.2016 = raw.data.2001.2016
    )
  }

  cli::cli_process_done()

  if (attach.spatial.data) {
    if (use_edav) {
      cli::cli_process_start("Downloading and attaching spatial data")
    } else {
      cli::cli_process_start("Loading and attaching spatial data")
    }

    spatial.data <- sirfunctions_io("read", NULL,
      file.path(analytic_folder, spatial_data_name),
      edav = use_edav
    )

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

  # Check that the required folders have data
  for (folder in c(analytic_folder, polis_data_folder, spatial_folder,
                   coverage_folder, pop_folder)) {

    # get_all_polio_data will recreate the analytic folder if it's missing
    switch(basename(folder),
           "analytic" = {
             if (!sirfunctions_io("exists.dir", NULL, folder, edav = use_edav)) {
               cli::cli_alert_info("No analytics folder found. Will create a new one.")
               sirfunctions_io("create.dir", NULL, folder, edav = use_edav)
             }
           },
           "polis" = {
             if (!sirfunctions_io("exists.dir", NULL, folder, edav = use_edav)) {
               cli::cli_alert_info("Creating polis folder in the data folder")
               sirfunctions_io("create.dir", NULL, folder, edav = use_edav)
             } else {
               cli::cli_alert_info("Moving updated polis data to the data folder")
             }
             create_polis_data_folder(data_folder, polis_folder, use_edav)
           },
           "spatial" = {
             if (!sirfunctions_io("exists.dir", NULL, folder, edav = use_edav)) {
               cli::cli_abort(paste0("No spatial data found in the data folder.",
                                     " Ensure that the output folder when running ",
                                     " tidypolis::process_spatial() is ",
                                     spatial_folder),
               )
             }
           },
           "coverage" = {
             if (!sirfunctions_io("exists.dir", NULL, folder, edav = use_edav)) {
               cli::cli_abort(paste0("Coverage data not found.",
                                     "Please create and add coverage data in: ",
                                     folder))
             }
           },
           "pop" = {
             if (!sirfunctions_io("exists.dir", NULL, folder, edav = use_edav)) {
               cli::cli_abort(paste0("Population data not found. ",
                                     "Preprocessing of population files may be required. ",
                                     "Please create a pop data folder and add data in: ",
                                     folder))
             }
           }
    )
  }

  if (use_edav) {
    cli::cli_h1("Testing download times")
    download_metrics <- test_EDAV_connection(return_list = T)
  }

  # use the truncated AFP file
  afp.trunc <- T

  if (recreate.static.files) {
    afp.trunc <- F
  }

  dl_table <- dplyr::bind_rows(
    sirfunctions_io("list", NULL, polis_data_folder, edav = use_edav),
    sirfunctions_io("list", NULL, spatial_folder, edav = use_edav),
    sirfunctions_io("list", NULL, coverage_folder, edav = use_edav),
    sirfunctions_io("list", NULL, pop_folder, edav = use_edav),
    sirfunctions_io("list", NULL, polis_folder, edav = use_edav) |>
      dplyr::filter(grepl("cache", name))
  ) |>
    dplyr::filter(!is.na(size)) |>
    dplyr::select("file" = "name", "size")

  if (use_edav) {
    dl_table <- dl_table |>
      dplyr::mutate(
        "dl_time_sec" = size / download_metrics$size * download_metrics$d
      )
  }

  if (afp.trunc) {
    dl_table <- dl_table |>
      dplyr::filter(!grepl("afp_linelist_2001", file))
  } else {
    dl_table <- dl_table |>
      dplyr::filter(!grepl("afp_linelist_2019", file))
  }

  file_size <- dl_table$size |> sum()

  if (use_edav) {
    download_time <- dl_table$dl_time_sec |> sum()
  }

  if (use_edav) {
    cli::cli_h1("Downloading POLIS Data")
  } else {
    cli::cli_h1("Loading POLIS Data")
  }

  raw.data <- list()
  spatial.data <- list()

  cli::cli_process_start("1) Loading country shape files")
  spatial.data$global.ctry <- load_clean_ctry_sp(
    fp = file.path(spatial_folder, global_ctry_sf_name),
    edav = use_edav
  )
  cli::cli_process_done()

  cli::cli_process_start("2) Loading province shape files")
  spatial.data$global.prov <- load_clean_prov_sp(
    fp = file.path(spatial_folder, global_prov_sf_name),
    edav = use_edav
  )
  cli::cli_process_done()

  cli::cli_process_start("3) Loading district shape files")
  spatial.data$global.dist <- load_clean_dist_sp(
    fp = file.path(spatial_folder, global_dist_sf_name),
    edav = use_edav
  )
  cli::cli_process_done()

  cli::cli_process_start("4) Loading AFP line list data (This file is almost 3GB and can take a while)")
  raw.data$afp <-
    sirfunctions_io("read", NULL, file_loc = dplyr::filter(
      dl_table,
      grepl("afp", file)
    ) |>
      dplyr::pull(file), edav = use_edav) |>
    dplyr::filter(surveillancetypename == "AFP") |>
    dplyr::mutate(
      cdc.classification.all2 = dplyr::case_when(
        final.cell.culture.result == "Not received in lab" &
          cdc.classification.all == "PENDING" ~ "LAB PENDING",
        TRUE ~ cdc.classification.all
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


  cli::cli_process_start("5) Loading population data")
  raw.data$dist.pop <-
    sirfunctions_io("read", NULL,
      dplyr::filter(dl_table, grepl("dist.pop", file)) |>
        dplyr::pull(file),
      edav = use_edav
    ) |>
    dplyr::ungroup()

  raw.data$prov.pop <-
    sirfunctions_io("read", NULL,
      file_loc = dplyr::filter(dl_table, grepl("prov.pop", file)) |>
        dplyr::pull(file), edav = use_edav
    ) |>
    dplyr::ungroup()

  raw.data$ctry.pop <-
    sirfunctions_io("read", NULL,
      dplyr::filter(dl_table, grepl("ctry.pop", file)) |>
        dplyr::pull(file),
      edav = use_edav
    ) |>
    dplyr::ungroup()
  cli::cli_process_done()


  cli::cli_process_start("6) Loading coverage data")
  raw.data$coverage <-
    sirfunctions_io("read", NULL,
      file_loc = dplyr::filter(dl_table, grepl("dpt", file)) |>
        dplyr::pull(file), edav = use_edav
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
      sirfunctions_io("read", NULL,
        file_loc = dplyr::filter(dl_table, grepl("mcv1", file)) |>
          dplyr::pull(file), edav = use_edav
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

  cli::cli_process_start("7) Loading ES data")

  raw.data$es <-
    sirfunctions_io("read", NULL,
      file_loc = dplyr::filter(dl_table, grepl("/es_2001", file)) |>
        dplyr::pull(file), edav = use_edav
    )
  cli::cli_process_done()

  cli::cli_process_start("8) Loading SIA data")
  raw.data$sia <-
    sirfunctions_io("read", NULL,
      file_loc = dplyr::filter(dl_table, grepl("sia", file)) |>
        dplyr::pull(file), edav = use_edav
    )

  cli::cli_process_done()

  cli::cli_process_start("9) Loading all positives")
  raw.data$pos <-
    sirfunctions_io("read", NULL,
      file_loc = dplyr::filter(dl_table, grepl("/pos", file)) |>
        dplyr::pull(file), edav = use_edav
    )

  cli::cli_process_done()

  cli::cli_process_start("10) Loading other surveillance linelist")
  raw.data$other <-
    sirfunctions_io("read", NULL,
      file_loc = dplyr::filter(dl_table, grepl("/other", file)) |>
        dplyr::pull(file), edav = use_edav
    )

  cli::cli_process_done()

  cli::cli_process_start("11) Loading road network data")
  spatial.data$roads <- sirfunctions_io("read", NULL,
    file_loc = dplyr::filter(dl_table, grepl("roads.rds", file)) |>
      dplyr::pull(file), edav = use_edav
  )
  cli::cli_process_done()

  cli::cli_process_start("12) Loading city spatial data")
  spatial.data$cities <- sirfunctions_io("read", NULL,
    file_loc = dplyr::filter(dl_table, grepl("cities.rds", file)) |>
      dplyr::pull(file), edav = use_edav
  )
  cli::cli_process_done()

  cli::cli_process_start("13) Creating Metadata object")

  polis.cache <- sirfunctions_io("read", NULL,
    file_loc = dplyr::filter(dl_table, grepl("cache.rds", file)) |>
      dplyr::pull(file), edav = use_edav
  ) |>
    dplyr::mutate(last_sync = as.Date(last_sync))

  raw.data$metadata$download_time <- max(polis.cache$last_sync, na.rm = TRUE)

  raw.data$metadata$processed_time <- sirfunctions_io("list", NULL,
    polis_folder,
    edav = use_edav
  ) |>
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

  cli::cli_process_start("14) Clearing out unused memory")
  gc()
  cli::cli_process_done()
}

if (create.cache) {
  cli::cli_process_start("15) Caching processed data")

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
    sirfunctions_io("write", NULL,
      file_loc = file.path(analytic_folder, dplyr::pull(out_files[i, ], file_name)),
      obj = out[[dplyr::pull(out_files[i, ], tag)]],
      edav = use_edav
    )
  }

  sirfunctions_io("write", NULL,
    file_loc = file.path(analytic_folder, "spatial.data.rds"),
    obj = spatial.data, edav = use_edav
  )

  cli::cli_process_done()
}

if (attach.spatial.data) {
  raw.data$global.ctry <- spatial.data$global.ctry
  raw.data$global.prov <- spatial.data$global.prov
  raw.data$global.dist <- spatial.data$global.dist
  raw.data$roads <- spatial.data$roads
  raw.data$cities <- spatial.data$cities
}

cli::cli_process_start("Checking for duplicates in datasets.")
raw.data <- duplicate_check(raw.data)
cli::cli_process_done()

return(raw.data)
}

#' Extract country specific information from raw polio data
#'
#' @description Filters country specific data from the CDC generated `raw.data` object from [get_all_polio_data()].
#'
#' @param .raw.data `list` Output of [get_all_polio_data()].
#' @param .country `str` Country name of interest. Case insensitive.
#' @returns Named `list` with country specific datasets.
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' ctry.data <- extract_country_data("nigeria", raw.data)
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
#'
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
      dplyr::group_by(epid, place.admin.0, dateonset) |>
      dplyr::mutate(count = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::filter(count > 1) |>
      dplyr::arrange(epid)
  }

  if (nrow(.raw.data$other[duplicated(.raw.data$other[, c("epid", "place.admin.0", "dateonset")]), ]) > 0) {
    cli::cli_alert_warning("There are potential duplicates in the Other Surveillance linelist, please check other.dupe")
    .raw.data$other.dupe <- .raw.data$other |>
      dplyr::group_by(epid, place.admin.0, dateonset) |>
      dplyr::mutate(count = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::filter(count > 1) |>
      dplyr::arrange(epid)
  }

  if (nrow(.raw.data$sia[duplicated(.raw.data$sia[, c(
    "adm2guid", "sub.activity.start.date", "vaccine.type", "age.group",
    "status", "lqas.loaded", "im.loaded"
  )]), ]) > 0) {
    cli::cli_alert_warning("There are potential duplicates in the SIA data, please check sia.dupe")
    .raw.data$sia.dupe <- .raw.data$sia |>
      dplyr::group_by(
        adm2guid, sub.activity.start.date,
        vaccine.type, age.group, status,
        lqas.loaded, im.loaded
      ) |>
      dplyr::mutate(count = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::filter(count > 1) |>
      dplyr::arrange(sia.sub.activity.code)
  }

  if (nrow(.raw.data$es[duplicated(.raw.data$es[, c(
    "env.sample.id", "virus.type", "emergence.group", "nt.changes",
    "site.id", "collection.date", "collect.yr"
  )]), ]) > 0) {
    cli::cli_alert_warning("There are potential duplicates in the ES data, please check es.dupe")
    .raw.data$es.dupe <- .raw.data$es |>
      dplyr::group_by(
        env.sample.id, virus.type, emergence.group,
        nt.changes, site.id, collection.date,
        collect.yr
      ) |>
      dplyr::mutate(es.dups = dplyr::n()) |>
      dplyr::filter(es.dups > 1) |>
      dplyr::select(dplyr::all_of(c(
        "env.sample.manual.edit.id", "env.sample.id",
        "sample.id", "site.id", "site.code", "site.name",
        "sample.condition", "collection.date", "virus.type",
        "nt.changes", "emergence.group", "collect.date", "collect.yr", "es.dups"
      )))
  }

  if (nrow(.raw.data$pos[duplicated(.raw.data$pos[, c(
    "epid", "epid.in.polis", "pons.epid", "polis.case.id",
    "env.sample.id", "place.admin.0", "source", "datasource",
    "virustype", "dateonset", "yronset", "ntchanges", "emergencegroup"
  )]), ]) > 0) {
    cli::cli_alert_warning("There are potential duplicates in the Positives data, please check pos.dupe")
    .raw.data$pos.dupe <- .raw.data$pos |>
      dplyr::group_by(
        epid, epid.in.polis, pons.epid,
        polis.case.id, env.sample.id, place.admin.0,
        source, datasource, virustype, dateonset,
        yronset, ntchanges, emergencegroup
      ) |>
      dplyr::mutate(count = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::filter(count > 1) |>
      dplyr::arrange(epid)
  }

  return(.raw.data)
}

#' Update a local dataset with new data
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Update a local global polio data (raw.data) with new data.
#'
#' @param local_dataset `str` File path to the global polio data RDS file.
#' @param overwrite `bool` Should the file be overwritten? Default `TRUE`.
#' @returns None.
#' @export
#' @examples
#' \dontrun{
#' local_raw_data <- "C:/Users/ABC1/Desktop/raw.data.rds"
#' update_polio_data(local_raw_data, overwrite = FALSE)
#' }
#'
update_polio_data <- function(local_dataset, overwrite = T) {
  cli::cli_process_start("Reading local dataset")
  old_data <- readr::read_rds(local_dataset)
  old_data_names <- names(old_data)
  dataset_name <- basename(local_dataset)
  cli::cli_process_done()

  spatial_data <- F
  if (length(intersect(c("global.ctry", "global.prov", "global.dist"), old_data_names)) > 0) {
    spatial_data <- T
  }

  new_data <- get_all_polio_data(attach.spatial.data = spatial_data)


  updated_data <- list()
  afp_dedup_col <- c("epid", "place.admin.0", "dateonset")
  pos_dedup_col <- c("epid", "polis.case.id", "env.sample.id", "place.admin.0",
                     "virustype", "ntchanges", "emergencegroup", "dateonset",
                     "source")
  sia_dedup_col <- c("adm2guid", "sub.activity.start.date", "vaccine.type",
                     "age.group","status", "lqas.loaded", "im.loaded")
  es_dedup_col <- c("env.sample.id", "virus.type", "emergence.group",
                    "nt.changes", "site.id", "collection.date", "collect.yr")

  for (i in old_data_names) {
    cli::cli_alert_info(paste0("Updating ", i))
    if (i %in% c("metadata",
                 "ctry.pop", "prov.pop", "dist.pop",
                 "global.ctry", "global.prov", "global.dist",
                 "roads", "cities", "afp.epi", "coverage")) {
      cli::cli_process_start(paste0("Replacing ", i, " with the most recent data"))
      updated_data[i] <- list(new_data[[i]])
      cli::cli_process_done()
    } else if (stringr::str_ends(i, ".dupe")) {
      cli::cli_alert_info("Skipping...")
      next # Ignore updating of dupes datasets
    } else {
      updated_data[i] <- list(suppressMessages(dplyr::full_join(old_data[[i]], new_data[[i]])))

      dedup_col <- switch(i,
                          "afp"  = afp_dedup_col,
                          "para.case" = afp_dedup_col,
                          "other" = afp_dedup_col,
                          "pos" = pos_dedup_col,
                          "sia" = sia_dedup_col,
                          "es" = es_dedup_col)
      yr_col <- switch(i,
                       "afp" = "yronset",
                       "para.case" = "yronset",
                       "other" = "yronset",
                       "pos" = "yronset",
                       "sia" = "yr.sia",
                       "es" = "collect.yr"
                       )

      if (i %in% c("afp", "other", "pos", "sia", "es", "para.case")) {
        cli::cli_process_start("Deduplicating records")
        updated_data[[i]] <- updated_data[[i]] |>
          dplyr::group_by(!!!dplyr::syms(dedup_col)) |>
          dplyr::slice_tail() |>
          dplyr::ungroup() |>
          # Create a UID for comparing old data with new
          tidyr::unite("uid", dplyr::any_of(dedup_col), remove = FALSE)
        cli::cli_process_done()

        cli::cli_process_start("Removing dropped records from the updated dataset")
        # Get updated 2019 UIDs
        new_ids <- tidyr::unite(new_data[[i]], "uid", dplyr::any_of(dedup_col)) |>
          dplyr::pull(uid)
        # Create UIDs in the old data
        old_data[[i]] <- tidyr::unite(old_data[[i]], "uid", dplyr::any_of(dedup_col),
                                 remove = FALSE)
        # Check UIDs that are in old data that are dropped in the new data
        removed_ids <- old_data[[i]] |>
          dplyr::filter(!!dplyr::sym(yr_col) >= 2019,
                        !uid %in% new_ids) |>
          dplyr::pull(uid)
        # Remove the dropped records
        updated_data[[i]] <- updated_data[[i]] |>
          dplyr::filter(!uid %in% removed_ids)

        if (length(removed_ids) == 0) {
          cli::cli_alert_info("No records removed since the last update.")
        } else {
          cli::cli_alert_info(paste0("There were ", length(removed_ids),
                                     " records removed in the latest update."))
        }
        cli::cli_process_done()
      }
    }
  }

  cli::cli_alert_success("Local dataset updated.")

  cli::cli_process_start("Final duplicate checking in the updated dataset.")
  updated_data <- duplicate_check(updated_data)
  cli::cli_process_done()

  if (overwrite) {
    readr::write_rds(updated_data, local_dataset)
    cli::cli_alert_success("File overwritten successfully.")
  } else {
    new_dataset <- file.path(dirname(local_dataset),
                             paste0(tools::file_path_sans_ext(dataset_name), "_saved_", lubridate::today(), ".rds"))
    readr::write_rds(updated_data, new_dataset)
    cli::cli_alert_success("Updated file written successfully.")
  }

  # Cleaning up environment
  rm(old_data, new_data, updated_data)
  invisible(gc())

}

#### 3) Secondary SP Functions ####

#' Download district geographic data
#
#' @description Pulls district shapefiles directly from the geodatabase.
#'
#' @param fp `str` Location of geodatabase.
#' @param azcontainer Azure validated container object.
#' @param dist_guid `str array` Array of all district GUIDS that you want to pull.
#' @param ctry_name `str array` Array of all country names that you want to pull.
#' @param prov_name `str array` Array of all province names that you want to pull.
#' @param dist_name `str array` Array of all dist names that you want to pull.
#' @param end_year `int` Last year you want to pull information for. Default is current year.
#' @param st_year `int` Earlier year of spatial data you want to pull. Default is 2000.
#' @param data_only `bool` Whether to return a tibble with shapefiles or not. Defaults to `FALSE`.
#' @param type `str` Whether to return a spatial object for every year group. Defaults to `NULL`.
#' - `"long"` Return a dataset for every year group.
#' - `NULL` Return a dataset only with unique GUIDs and when they were active.
#' @param version `str` Specify whether to return standard shapefiles or new shapefiles
#' still under evaluation/development. Default is `"standard"`.
#' - `"standard"` Standard shapefiles.
#' - `"dev"`  New shapefiles still under evaluation/development.
#' @param edav `bool` Load from EDAV? Defaults to `TRUE`.
#' @param end.year `int` `r lifecycle::badge("deprecated")` Renamed in favor of
#' `end_year`.
#' @param st.year `int` `r lifecycle::badge("deprecated")` Renamed in favor of
#' `st_year`.
#' @param data.only `bool` `r lifecycle::badge("deprecated")` Renamed in favor of
#' `data_only`.
#'
#' @returns `tibble` or `sf` Dataframe containing spatial data.
#' @examples
#' dist <- load_clean_dist_sp(ctry_name = c("ALGERIA", "NIGERIA"), st.year = 2019)
#' dist.long <- load_clean_dist_sp(ctry_name = "ALGERIA", st.year = 2019, type = "long")
#'
#' @export
load_clean_dist_sp <- function(azcontainer = suppressMessages(get_azure_storage_connection()),
                               fp = "GID/PEB/SIR/Data/spatial/global.dist.rds",
                               dist_guid = NULL,
                               dist_name = NULL,
                               prov_name = NULL,
                               ctry_name = NULL,
                               end_year = lubridate::year(Sys.Date()),
                               st_year = 2000,
                               data_only = FALSE,
                               type = NULL,
                               version = "standard",
                               edav = TRUE,
                               end.year = lifecycle::deprecated(),
                               st.year = lifecycle::deprecated(),
                               data.only = lifecycle::deprecated()) {
  if (lifecycle::is_present(end.year)) {
    lifecycle::deprecate_warn(
      "1.3.0", "load_clean_dist_sp(end.year)",
      "load_clean_dist_sp(end_year)"
    )
    end_year <- end.year
  }

  if (lifecycle::is_present(st.year)) {
    lifecycle::deprecate_warn(
      "1.3.0", "load_clean_dist_sp(st.year)",
      "load_clean_dist_sp(st_year)"
    )
    st_year <- st.year
  }

  if (lifecycle::is_present(data.only)) {
    lifecycle::deprecate_warn(
      "1.3.0", "load_clean_dist_sp(data.only)",
      "load_clean_dist_sp(data_only)"
    )
    data_only <- data.only
  }

  if (version == "dev") {
    fp <- "GID/PEB/SIR/Data/spatial_dev/global.dist.rds"
    cli::cli_alert_info("Loading under development district spatial files")
  } else {
    cli::cli_alert_info("Loading district spatial files")
  }

  if (edav) {
    out <- suppressWarnings(AzureStor::storage_load_rds(azcontainer, fp))
  } else {
    out <- readRDS(fp)
  }

  out <- out |>
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
    dplyr::filter(yr.st <= end_year & (yr.end >= st_year | yr.end == 9999)) %>%
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
    } %>%
    {
      if (is.null(prov_name)) {
        .
      } else {
        dplyr::filter(., ADM1_NAME %in% prov_name)
      }
    } %>%
    {
      if (is.null(dist_name)) {
        .
      } else {
        dplyr::filter(., ADM2_NAME %in% dist_name)
      }
    }

  if (data_only) {
    out <- dplyr::as_tibble(out)
    return(out)
  }

  if (!is.null(type) && type == "long") {
    df.list <- lapply(st_year:end_year, function(i) f.yrs.01(out, i))
    out <- do.call(rbind, df.list)
  }

  return(out)
}

#' Download province geographic data
#'
#' @description Pulls province shapefiles directly from the geodatabase
#'
#' @param azcontainer Azure validated container object
#' @param fp `str` Location of geodatabase.
#' @param prov_guid `str array` Array of all province GUIDS that you want to pull.
#' @param prov_name `str array` Array of all province names that you want to pull.
#' @param ctry_name `str array` Array of all country names that you want to pull.
#' @param end_year `int` Last year you want to pull information for. Default is current year.
#' @param st_year `int` Earlier year of spatial data you want to pull. Default is 2000.
#' @param data_only `bool` Whether to return a tibble with shapefiles or not. Defaults to `FALSE`.
#' @param type `str` Whether to return a spatial object for every year group. Defaults to `NULL`.
#' - `"long"` Return a dataset for every year group.
#' - `NULL` Return a dataset only with unique GUIDs and when they were active.
#' @param version `str` Specify whether to return standard shapefiles or new shapefiles
#' still under evaluation/development. Default is `"standard"`.
#' - `"standard"` Standard shapefiles.
#' - `"dev"`  New shapefiles still under evaluation/development.
#' @param edav `bool` Load from EDAV? Defaults to `TRUE`.
#' @param end.year `int` `r lifecycle::badge("deprecated")` Renamed in favor of
#' `end_year`.
#' @param st.year `int` `r lifecycle::badge("deprecated")` Renamed in favor of
#' `st_year`.
#' @param data.only `bool` `r lifecycle::badge("deprecated")` Renamed in favor of
#' `data_only`.
#'
#' @returns `tibble` or `sf` Dataframe containing spatial data.
#' @examples
#' \dontrun{
#' prov <- load_clean_prov_sp(ctry_name = c("ALGERIA", "NIGERIA"), st_year = 2019)
#' prov.long <- load_clean_prov_sp(ctry_name = "ALGERIA", st_year = 2019, type = "long")
#' }
#' @export
load_clean_prov_sp <- function(azcontainer = suppressMessages(get_azure_storage_connection()),
                               fp = "GID/PEB/SIR/Data/spatial/global.prov.rds",
                               prov_guid = NULL,
                               prov_name = NULL,
                               ctry_name = NULL,
                               end_year = lubridate::year(Sys.Date()),
                               st_year = 2000,
                               data_only = FALSE,
                               type = NULL,
                               version = "standard",
                               edav = TRUE,
                               end.year = lifecycle::deprecated(),
                               st.year = lifecycle::deprecated(),
                               data.only = lifecycle::deprecated()) {
  if (lifecycle::is_present(end.year)) {
    lifecycle::deprecate_warn(
      "1.3.0", "load_clean_prov_sp(end.year)",
      "load_clean_prov_sp(end_year)"
    )
    end_year <- end.year
  }

  if (lifecycle::is_present(st.year)) {
    lifecycle::deprecate_warn(
      "1.3.0", "load_clean_prov_sp(st.year)",
      "load_clean_prov_sp(st_year)"
    )
    st_year <- st.year
  }

  if (lifecycle::is_present(data.only)) {
    lifecycle::deprecate_warn(
      "1.3.0", "load_clean_prov_sp(data.only)",
      "load_clean_prov_sp(data_only)"
    )
    data_only <- data.only
  }

  if (version == "dev") {
    fp <- "GID/PEB/SIR/Data/spatial_dev/global.prov.rds"
    cli::cli_alert_info("Loading under development province spatial files")
  } else {
    cli::cli_alert_info("Loading province spatial files")
  }

  if (edav) {
    out <- suppressWarnings(AzureStor::storage_load_rds(azcontainer, fp))
  } else {
    out <- readRDS(fp)
  }

  out <- out |>
    dplyr::mutate(
      yr.st = lubridate::year(STARTDATE),
      yr.end = lubridate::year(ENDDATE),
      ADM0_NAME = ifelse(stringr::str_detect(ADM0_NAME, "IVOIRE"), "COTE D IVOIRE", ADM0_NAME)
    ) %>%
    # this filters based on dates set in RMD
    dplyr::filter(yr.st <= end_year & (yr.end >= st_year | yr.end == 9999)) %>%
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

  if (data_only) {
    out <- dplyr::as_tibble(out)
    return(out)
  }

  if (!is.null(type) && type == "long") {
    df.list <- lapply(st_year:end_year, function(i) f.yrs.01(out, i))
    out <- do.call(rbind, df.list)
  }

  return(out)
}

#' Download country georgraphic data
#'
#' @description Pulls country shapefiles directly from the geodatabase.
#'
#' @param azcontainer Azure validated container object
#' @param fp `str` Location of geodatabase.
#' @param ctry_guid `str array` Array of all country GUIDS that you want to pull.
#' @param ctry_name `str array` Array of all country names that you want to pull.
#' @param end_year `int` Last year you want to pull information for. Default is current year.
#' @param st_year `int` Earlier year of spatial data you want to pull. Default is 2000.
#' @param data_only `bool` Whether to return a tibble with shapefiles or not. Defaults to `FALSE`.
#' @param type `str` Whether to return a spatial object for every year group. Defaults to `NULL`.
#' - `"long"` Return a dataset for every year group.
#' - `NULL` Return a dataset only with unique GUIDs and when they were active.
#' @param version `str` Specify whether to return standard shapefiles or new shapefiles
#' still under evaluation/development. Default is `"standard"`.
#' - `"standard"` Standard shapefiles.
#' - `"dev"`  New shapefiles still under evaluation/development.
#' @param edav `bool` Load data from EDAV? Defaults to TRUE.
#' @param end.year `int` `r lifecycle::badge("deprecated")` Renamed in favor of
#' `end_year`.
#' @param st.year `int` `r lifecycle::badge("deprecated")` Renamed in favor of
#' `st_year`.
#' @param data.only `bool` `r lifecycle::badge("deprecated")` Renamed in favor of
#' `data_only`.
#'
#' @returns `tibble` or `sf` Dataframe containing spatial data.
#' @examples
#' ctry <- load_clean_ctry_sp(ctry_name = "ALGERIA")
#' ctry.long <- load_clean_ctry_sp(ctry_name = "ALGERIA", type = "long")
#' @export
load_clean_ctry_sp <- function(azcontainer = suppressMessages(get_azure_storage_connection()),
                               fp = "GID/PEB/SIR/Data/spatial/global.ctry.rds",
                               ctry_guid = NULL,
                               ctry_name = NULL,
                               end_year = lubridate::year(Sys.Date()),
                               st_year = 2000,
                               data_only = FALSE,
                               type = NULL,
                               version = "standard",
                               edav = TRUE,
                               end.year = lifecycle::deprecated(),
                               st.year = lifecycle::deprecated(),
                               data.only = lifecycle::deprecated()) {
  if (lifecycle::is_present(end.year)) {
    lifecycle::deprecate_warn(
      "1.3.0", "load_clean_ctry_sp(end.year)",
      "load_clean_ctry_sp(end_year)"
    )
    end_year <- end.year
  }

  if (lifecycle::is_present(st.year)) {
    lifecycle::deprecate_warn(
      "1.3.0", "load_clean_ctry_sp(st.year)",
      "load_clean_ctry_sp(st_year)"
    )
    st_year <- st.year
  }

  if (lifecycle::is_present(data.only)) {
    lifecycle::deprecate_warn(
      "1.3.0", "load_clean_ctry_sp(data.only)",
      "load_clean_ctry_sp(data_only)"
    )
    data_only <- data.only
  }

  if (version == "dev") {
    fp <- "GID/PEB/SIR/Data/spatial_dev/global.ctry.rds"
    cli::cli_alert_info("Loading under development country spatial files")
  } else {
    cli::cli_alert_info("Loading country spatial files")
  }

  if (edav) {
    out <- suppressWarnings(AzureStor::storage_load_rds(azcontainer, fp))
  } else {
    out <- readRDS(fp)
  }

  out <- out |>
    dplyr::mutate(
      yr.st = lubridate::year(STARTDATE),
      yr.end = lubridate::year(ENDDATE),
      ADM0_NAME = ifelse(stringr::str_detect(ADM0_NAME, "IVOIRE"), "COTE D IVOIRE", ADM0_NAME)
    ) %>%
    # this filters based on dates set in RMD
    dplyr::filter(yr.st <= end_year & (yr.end >= st_year | yr.end == 9999)) %>%
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

  if (data_only) {
    out <- dplyr::as_tibble(out)
    return(out)
  }

  if (!is.null(type) && type == "long") {
    df.list <- lapply(st_year:end_year, function(i) f.yrs.01(out, i))
    out <- do.call(rbind, df.list)
  }

  return(out)
}

#### 4) Misc ####

#' Utility function to fix years
#'
#' @param df `tibble` Dataframe to be used for fixing years.
#' @param yrs `array` Numeric array of years to fix data.
#' @returns `tibble` Long format table to fix years.
#' @keywords internal
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
#' @param action `str` Can either be to `"concat"` or `"split"`.
#' @param split.years `array` A numeric array of years by which data should be split.
#' @param raw.data.all `list` A list of data objects to be split.
#' @param raw.data.post.2019 `list` A list of data objects to be concatenated.
#' @param raw.data.2016.2019 `list` A list of data objects to be concatenated.
#' @param raw.data.2001.2016 `list` A list of data objects to be concatenated.
#' @returns `list` A list of lists or a single concatenated list.
#' @keywords internal
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
#' a district is critical and the shapefile from [extract_country_data()] is not yet updated.
#' @param ctry.data `list` Country polio data, with spatial data attached. Output of [extract_country_data()] or
#' [init_dr()].
#'
#' @returns `list` A list containing errors in province and district GUIDs.
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
    dplyr::filter(adm1guid %in% prov_mismatches_pop) |>
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
      dplyr::filter(adm1guid %in% prov_mismatches_shape) |>
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
    dplyr::filter(adm2guid %in% dist_mismatches_pop) |>
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
      dplyr::filter(adm2guid %in% dist_mismatches_shape) |>
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
#' This is the output of [check_afp_guid_ctry_data()].
#' @param spatial_scale `str` The spatial scale to impute data. Either `"prov"` or `"dist"`.
#'
#' @returns `tibble` AFP data with corrected GUIDs based on the population files.
#' @examples
#' raw.data <- get_all_polio_data()
#' ctry.data <- extract_country_data("algeria", raw.data)
#' error.list <- check_afp_guid_ctry_data(ctry.data)
#' ctry.data$afp.all.2 <- fix_ctry_data_missing_guids(
#'   ctry.data$afp.all.2,
#'   ctry.data$dist.pop,
#'   error.list$dist_mismatches_pop,
#'   "dist"
#' )
#'
#' @export
fix_ctry_data_missing_guids <- function(afp.data, pop.data, guid_list, spatial_scale) {
  afp.data <- switch(spatial_scale,
    "prov" = {
      afp.data |>
        dplyr::mutate(adm1guid = dplyr::if_else(adm1guid %in% guid_list, NA, adm1guid)) |>
        dplyr::left_join(pop.data, by = c("ctry", "prov", "year", "adm0guid")) |>
        dplyr::mutate(adm1guid = dplyr::coalesce(adm1guid.x, adm1guid.y)) |>
        dplyr::select(-dplyr::any_of(c("adm1guid.x", "adm1guid.y")))
    },
    "dist" = {
      afp.data |>
        dplyr::mutate(adm2guid = dplyr::if_else(adm2guid %in% guid_list, NA, adm2guid)) |>
        dplyr::left_join(pop.data, by = c(
          "ctry", "prov", "dist", "year",
          "adm0guid", "adm1guid"
        )) |>
        dplyr::mutate(adm2guid = dplyr::coalesce(adm2guid.x, adm2guid.y)) |>
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
#' @returns None. Will output compressed image to the local folder.
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
#' @param df `df` or `tibble` Dataframe with at least one column containing unique identifiers and other columns.
#' @param id_col `str` Column used as a unique identifier for records.
#'
#' @returns `tibble` A tibble showing the columns where duplicates differ.
#' @examples
#' df1 <- dplyr::tibble(col1 = c(1, 1, 2), col2 = c("a", "b", "c"), col3 = c(1, 1, 3))
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
    dplyr::summarise(col_with_diff = paste(unique(column_name), collapse = ", "))

  return(col_with_differences)
}

#' Check for rows with NA values
#'
#' A general function that checks the number of `NA` rows for a particular
#' column.
#' @param df `tibble` Dataset to check.
#' @param .col_name `str` Name of the target column.
#' @param .group_by `str` or `list` A string or a list of strings to group the
#' check by.
#'
#' @returns `tibble` A summary of the number of rows missing for the target
#' variable.
#' @export
#'
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' missing <- check_missing_rows(raw.data$afp, "age.months", c("place.admin.0", "yronset"))
check_missing_rows <- function(df,
                               .col_name,
                               .group_by) {
  # Input checks
  if (!.col_name %in% names(df)) {
    cli::cli_abort(paste0(.col_name, "column is missing in the dataset."))
  }

  if (length(setdiff(.group_by, names(df))) != 0) {
    invalid_cols <- setdiff(.group_by, names(df))
    cli::cli_alert_warning(paste0(
      "These columns are missing in the dataset: ",
      paste(invalid_cols, collapse = ", ")
    ))
    cli::cli_abort("Please use different grouping columns and try again.")
  }

  missing <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.group_by))) |>
    dplyr::summarise(
      n = sum(is.na(.data[[.col_name]])),
      total_rows = dplyr::n(),
      prop = round(n / total_rows, 2)
    )

  return(missing)
}

#' Interactive loading of EDAV data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function is a way to interactively work with files in the EDAV
#' environment, which is convenient as we don't have to search for files within
#' Azure Storage Explorer.
#' @details
#' There are Excel files that may need additional formatting before it can be
#' read properly into an R object. For example, skipping columns or rows.
#' For complicated Excel files, it would be best to directly call [edav_io()]
#' in "read" mode, and pass additional parameters via `...`. See [edav_io()]
#' examples for details.
#'
#'
#' @param path `str` Path to start at initially.
#' @param azcontainer Azure storage container provided by [get_azure_storage_connection()].
#'
#' @returns `tibble` Data from the EDAV environment.
#' @export
#'
#' @examples
#' \dontrun{
#' test <- explore_edav()
#' }
explore_edav <- function(path = get_constant("DEFAULT_EDAV_FOLDER"),
                         azcontainer = suppressMessages(get_azure_storage_connection())) {
  cli::cli_alert_info(paste0(
    "Interactive file selection activated.",
    " Use esc to exit."
  ))
  pointer <- path
  while (TRUE) {
    tryCatch(
      expr = {
        output <- edav_io(io = "list", default_dir = "",
                          file_loc = file.path(pointer),
                          azcontainer = azcontainer
                          )
        print(
          output |>
            dplyr::mutate(name = stringr::str_extract(name, "[^/]+$")),
          n = nrow(output)
        )
      },
      error = function(e) {
        cli::cli_alert_warning("\nAccess denied. Please choose a valid option.")
        pointer <<- gsub("[^/]+$", "", pointer)
        pointer <<- sub("/$", "", pointer)

        output <<- edav_io(io = "list", default_dir = "",
                           file_loc = file.path(pointer),
                           azcontainer = azcontainer)
        print(
          output |>
            dplyr::mutate(name = stringr::str_extract(name, "[^/]+$")),
          n = nrow(output)
        )
      }
    )

    cli::cli_alert_info(paste0(
      "\nPlease choose an option (1-4):\n",
      "1) Previous directory\n",
      "2) Move up one directory\n",
      "3) Download as an R object (.Rds, .Rda, .csv, .xlsx supported) \n",
      "4) Download file locally\n",
      "5) Copy absolute file path"
    ))
    response <- stringr::str_trim(readline("Response: "))
    if (!response %in% c("1", "2", "3", "4", "5")) {
      cli::cli_alert_warning("Invalid response. Please try again.\n")
    } else if (response == "1") {
      # Special case of navigating back to root
      if (stringr::str_count(pointer, "/") == 0) {
        pointer <- ""
      }
      pointer <- gsub("[^/]+$", "", pointer)
      pointer <- sub("/$", "", pointer)
      cli::cli_alert_success(paste0("Navigating to: ", pointer))
    } else if (response == "2") {
      while (TRUE) {
        if (nrow(output) == 1) {
          pointer <- output[1, ]$name
          pointer <- sub("/$", "", pointer)
          cli::cli_alert_success(paste0("Navigating to: ", pointer))
          break
        } else if (nrow(output) == 0) {
          cli::cli_alert_warning("Directory is empty.")
          break
        }
        cli::cli_alert_info("Please input the line number:")
        response <- stringr::str_trim(readline("Response: "))
        response <- tryCatch(suppressWarnings(as.numeric(response)),
          error = function(e) {
            NA
          }
        )
        if (is.na(response) | response > nrow(output) | response == 0) {
          cli::cli_alert_warning("Invalid response. Please try again:\n")

          print(
            output |>
              dplyr::mutate(name = stringr::str_extract(name, "[^/]+$")),
            n = nrow(output)
          )
        } else if (output[response, ]$isdir == FALSE) {
          cli::cli_alert_warning("Not a directory. Please try again:\n")
          break
        } else {
          pointer <- output[response, ]$name
          pointer <- sub("/$", "", pointer)
          cli::cli_alert_success(paste0("Navigating to: ", pointer))
          break
        }
      }
    } else if (response == "3") {
      while (TRUE) {
        cli::cli_alert_info("Please input the line number:")
        response <- stringr::str_trim(readline("Response: "))
        response <- tryCatch(suppressWarnings(as.numeric(response)),
          error = function(e) {
            NA
          }
        )
        if (is.na(response) | response > nrow(output) | response == 0) {
          cli::cli_alert_info("Invalid response. Please try again:\n")

          print(
            output |>
              dplyr::mutate(name = stringr::str_extract(name, "[^/]+$")),
            n = nrow(output)
          )
        } else if (output[response, ]$isdir == TRUE) {
          cli::cli_alert_info(paste0(
            output[response, ]$name,
            " is a directory. Navigating to it."
          ))
          pointer <- output[response, ]$name
          break
        } else {
          pointer <- file.path(output[response, ]$name)
          ext <- tools::file_ext(pointer)
          if (ext %in% c("xlsx", "xls")) {
            withr::with_tempdir(
              {
                AzureStor::storage_download(azcontainer,
                                            pointer,
                                            file.path(tempdir(), basename(pointer)),
                                            overwrite = TRUE
                )
                output <- read_excel_from_edav(file.path(tempdir(),
                                                         basename(pointer)))
                return(output)
              }
            )
          } else {
            output <- edav_io("read", default_dir = "", pointer,
                              azcontainer = azcontainer)
            return(output)
          }
        }
      }
    } else if (response == "4") {
      while (TRUE) {
        cli::cli_alert_info("Please input the line number:")
        response <- stringr::str_trim(readline("Response: "))
        response <- tryCatch(suppressWarnings(as.numeric(response)),
                             error = function(e) {
                               NA
                             }
        )
        if (is.na(response) | response > nrow(output) | response == 0) {
          cli::cli_alert_info("Invalid response. Please try again:\n")

          print(
            output |>
              dplyr::mutate(name = stringr::str_extract(.data$name, "[^/]+$")),
            n = nrow(output)
          )
        } else if (output[response, ]$isdir == TRUE) {
          cli::cli_alert_info(paste0(
            output[response, ]$name,
            " is a directory. Navigating to it."
          ))
          pointer <- output[response, ]$name
          break
        } else {
          pointer <- file.path(output[response, ]$name)
          while (TRUE) {
            cli::cli_alert_info("Enter folder path (no quotes):")
            dest <- file.path(stringr::str_trim(readline("Response: ")))
            if (!dir.exists(dest)) {
              cli::cli_alert_info("Not a valid file path. Please try again.")
            } else {
              AzureStor::storage_download(azcontainer,
                                          pointer,
                                          file.path(dest, basename(pointer)),
                                          overwrite = TRUE
              )

              cli::cli_alert_success(paste0("File downloaded at: ",
                                            file.path(dest, basename(pointer))))
              return(NULL)
            }
          }
        }
      }
    } else if (response == "5") {
      while (TRUE) {
        cli::cli_alert_info("Please input the line number:")
        response <- stringr::str_trim(readline("Response: "))
        response <- tryCatch(suppressWarnings(as.numeric(response)),
          error = function(e) {
            NA
          }
        )
        if (is.na(response) | response > nrow(output) | response == 0) {
          cli::cli_alert_info("Invalid response. Please try again:\n")
          print(
            output |>
              dplyr::mutate(name = stringr::str_extract(
                name,
                "[^/]+$"
              )),
            n = nrow(output)
          )
        } else {
          path_name <- file.path(output[response, ]$name)
          return(path_name)
        }
      }
    }
  }
}

# Private functions ----

#' Reads an Excel file from EDAV to the R environment
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function is an extension of the readxl() that adapts to files with
#' multiple tabs. If there are multiple tabs, each sheet are downloaded into a
#' named list with the corresponding tab name.
#'
#' @details
#' Actually, this function doesn't need to be used on EDAV files. It can work
#' with local files as well.
#'
#'
#' @param src `str` Path to the Excel file.
#' @param ... Additional parameters of [readxl::read_excel()].
#'
#' @return `tibble` or `list` A tibble or a list of tibbles containing data from
#' the Excel file.
#' @keywords internal
#'
read_excel_from_edav <- function(src, ...) {
  sheets <- readxl::excel_sheets(src)
  if (length(sheets) > 1) {
    output <- purrr::map(sheets, \(x) readxl::read_xlsx(path = src, sheet = x,
                                                        ...))
    names(output) <- sheets
  } else {
    output <- readxl::read_excel(src)
  }

  return(output)
}

#' Creates polis folder to the data folder
#'
#' @description
#' Copies the updated POLIS data into the data folder. This function is used
#' inside get_all_polio_data().
#'
#' @param data_folder `str` Path to the data folder.
#' @param polis_folder `str` Path to the core ready folder
#' @param use_edav `bool` Are file paths on EDAV?
#'
#' @return NULL
#' @keywords internal
#'
create_polis_data_folder <- function(data_folder, polis_folder, use_edav) {

  files <- c("afp_linelist_2001-01-01",
             "afp_linelist_2019-01-01",
             "es_2001-01-08",
             "other_surveillance_type_linelist_2016_",
             "positives_2001-01-01",
             "sia_2000")

  source.table <- sirfunctions_io("list", NULL,
                                  file.path(polis_folder, "data", "Core_Ready_Files"),
                                  edav = use_edav) |>
    dplyr::select(src_path = name) |>
    dplyr::mutate(
      name = basename(src_path),
      dest_path = file.path(data_folder, "polis", name)
    ) |>
    dplyr::filter(stringr::str_starts(name, paste0(files, collapse = "|"))) |>
    dplyr::select(name, src_path, dest_path)

  # Archive previous versions in the core polis data folder
  cli::cli_process_start("Archiving previous data in the polis folder")
  if (!sirfunctions_io("exists.dir", NULL, file.path(data_folder, "polis", "archive"), edav = use_edav)) {
    sirfunctions_io("create.dir", NULL, file.path(data_folder, "polis", "archive"), edav = use_edav)
  }
  # Move previous files to archive in polis data folder

  # Explicitly create since Sys.Date() could potentially vary when ran overnight
  archive_folder_name <- Sys.Date()
  sirfunctions_io("create.dir", NULL,
                  file.path(data_folder, "polis", "archive", archive_folder_name),
                  edav = use_edav)

  current.table <- sirfunctions_io(io = "list", NULL,
                                 file.path(data_folder, "polis"),
                                 edav = use_edav) |>
    dplyr::filter(isdir != TRUE)

  if (nrow(current.table) == 0) {
    cli::cli_alert_success("No data to archive")
    cli::cli_process_done()
  } else {
    current.table <- current.table |>
      dplyr::select(src_path = name) |>
      dplyr::mutate(
        name = basename(src_path)
      ) |>
      dplyr::filter(stringr::str_starts(name, paste0(files, collapse = "|"))) |>
      dplyr::select(name, src_path)

    current.table.size <- sirfunctions_io(io = "list", NULL,
                                          file.path(data_folder, "polis"),
                                          edav = use_edav) |>
      dplyr::mutate(size = size / 1000000000) |>
      dplyr::pull(size) |>
      sum(na.rm = TRUE)
    cli::cli_alert_warning(paste0("NOTE: This archive is ",
                                  round(current.table.size, 2),
                                  " GB. ",
                                  "If you're archiving locally, ",
                                  "we recommend reviewing old archives periodically and",
                                  " decide whether to put them in a backup storage device or",
                                  " in cloud storage."))

    lapply(1:nrow(current.table), \(i) {
      local <- sirfunctions_io("read", NULL, current.table$src_path[i], edav = use_edav)
      sirfunctions_io("write", NULL,
                      file.path(data_folder, "polis", "archive", archive_folder_name,
                                basename(current.table$src_path[i])),
                      obj = local, edav = use_edav)
    })
    cli::cli_process_done()
  }

  cli::cli_process_start("Adding updated data to polis data folder")

  # Delete previous files
  if (nrow(current.table) != 0) {
    lapply(1:nrow(current.table), \(i) {
      sirfunctions_io("delete", NULL,
                      current.table$src_path[i], edav = use_edav)
    })
  }

  # Move all files to core polis data folder
  lapply(1:nrow(source.table), function(i){
    local <- sirfunctions_io("read", NULL, dplyr::pull(source.table[i,], src_path), edav = use_edav)
    sirfunctions_io("write", NULL, dplyr::pull(source.table[i,], dest_path),
                    obj = local, edav = use_edav)
  })

  cli::cli_process_done()

  return(NULL)
}
