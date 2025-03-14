#' Convert raw data into a parquet hierarchal folder
#'
#' The function takes a `raw_data` object (output of [get_all_polio_data()]) and
#' saves it into a parquet directory
#' @param raw_data `list` A `raw_data` object.
#' @param path `str` Path to export the parquet folder to.
#'
#' @returns None.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' create_raw_data_parquet(raw_data, "C:/Users/ABC1/Desktop/raw_data_parquet")
#' }
create_raw_data_parquet <- function(raw_data, path){
  start <- Sys.time()
  df_names <- names(raw_data)

  if (!dir.exists(path)) {
    cli::cli_abort("Directory path does not exist.")
  }

  cli::cli_process_start("Creating raw_data parquet folder")
  iter <- 1
  for (i in df_names) {
    cli::cli_alert_info(paste0("Now processing: ", i))

    if (i %in% c("global.prov", "global.dist")) {
      raw_data[[i]] |>
        dplyr::mutate(SHAPE = sf::st_as_text(SHAPE)) |>
        arrow::write_dataset(path = file.path(path, i),
                             partitioning = get_partition_cols(i))

    } else if (i == "global.ctry") {
      raw_data[[i]] |>
        dplyr::mutate(Shape = sf::st_as_text(Shape)) |>
        arrow::write_dataset(path = file.path(path, i),
                             partitioning = get_partition_cols(i))
    } else if (i %in% c("cities", "roads")) {
      raw_data[[i]] |>
        dplyr::mutate(geometry = sf::st_as_text(geometry)) |>
        arrow::write_dataset(path = file.path(path, i),
                             partitioning = get_partition_cols(i))

    } else if (i == "metadata") {
      raw_data[[i]] |>
        dplyr::as_tibble() |>
        arrow::write_dataset(path = file.path(path, i),
                             partitioning = get_partition_cols(i))
    } else {
      raw_data[[i]] |> arrow::write_dataset(path = file.path(path, i),
                                            partitioning = get_partition_cols(i))
    }

    cli::cli_alert_info(paste0(iter, "/", length(df_names), " processed."))
    iter <- iter + 1
  }

  cli::cli_process_done()
  cli::cli_alert_success("raw_data parquet folder created!")
  cli::cli_alert_info(paste0("Data processed in: ",
                             round(difftime(Sys.time(), start, "mins"), 2),
                             " mins."))
}

#' Recreate raw data from local parquet folder
#'
#' Recreates an output of [get_all_polio_data()] from a folder housing
#' data in parquet format.
#'
#' @param path `str` Local path to the parquet folder
#' @param from_edav `bool` Build using local files or files in EDAV?
#' @param container `azcontainer` An instance of an Azure container to connect
#' to. Pass [get_azure_storage_connection()] using defaults if not accessing
#' using a service principal.
#'
#' @returns `list` A list containing connections to the folders associated with
#' individual datasets in the original output of [get_all_polio_data()].
#' @export
#'
#' @examples
#' \dontrun{
#' # Building raw_data locally
#' parquet_path <- "C:/Users/ABC1/Desktop/parquet_folder"
#' raw_data <- build_parquet_raw_data(parquet_path)
#'
#' # Build raw_data from EDAV
#' raw_data <- build_parquet_raw_data()
#' }
build_parquet_raw_data <- function(path = NULL, from_edav = F, container = NULL) {

  if (from_edav) {
    # Default values
    if (!is.null(path)) {
      path <- "GID/PEB/SIR/Sandbox/parquet_sandbox"
    }
    if (!is.null(container)) {
      container <- get_azure_storage_connection()
    }

    raw_data <- build_parquet_raw_data_edav(path, container)
  } else {
    raw_data <- build_parquet_raw_data_local(path)
  }

  return(raw_data)
}

# Private functions ----

#' Gets the column used to partition a column
#'
#' @param name `str` Name of the column
#'
#' @returns `chr` A character vector of columns to partition with.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_partition_cols("afp")
#' }
get_partition_cols <- function(name) {
  switch(name,
         "afp" = c("place.admin.0", "yronset"),
         "afp.dupe" = c("place.admin.0", "yronset"),
         "afp.epi" = c("place.admin.0", "yronset"),
         "para.case" = c("place.admin.0", "yronset"),
         "es" = c("ADM0_NAME", "collect.yr"),
         "es.dupe" = c("ADM0_NAME", "collect.yr"),
         "sia" = c("place.admin.0", "yr.sia"),
         "sia.dupe" = c("place.admin.0", "yr.sia"),
         "pos" = c("place.admin.0", "yronset"),
         "pos.dupe" = c("place.admin.0", "yronset"),
         "other" = c("place.admin.0", "yronset"),
         "other.dupe" = c("place.admin.0", "yronset"),
         "dist.pop" = c("ADM0_NAME"),
         "prov.pop" = c("ADM0_NAME"),
         "ctry.pop" = c("ADM0_NAME"),
         "global.ctry" = c("ADM0_NAME"),
         "global.prov" = c("ADM0_NAME"),
         "global.dist" = c("ADM0_NAME"),
         "roads" = c("continent"),
         "cities" = c("CNTRY_NAME"),
         "metadata" = "download_time"
         )
}

#' Build raw_data using local parquet files
#'
#' @param path `str` A path to the parquet directory
#'
#' @returns `list` A list containing connections to the folders associated with
#' individual datasets in the original output of [get_all_polio_data()].
#' @keywords internal
#'
build_parquet_raw_data_local <- function(path = NULL) {

  if (!dir.exists(path)) {
    cli::cli_abort("Not a valid directory.")
  }

  valid_values <- c("afp", "afp.dupe", "afp.epi", "para.case", "es", "es.dupe",
                    "sia", "sia.dupe", "pos", "pos.dupe", "other", "other.dupe",
                    "dist.pop", "prov.pop", "ctry.pop", "global.ctry",
                    "global.prov", "global.dist", "roads" , "cities", "metadata"
                    )
  data <- list.files(path)
  data <- intersect(data, valid_values)

  raw_data <- list()
  for (i in data) {
    raw_data[[i]] <- arrow::open_dataset(file.path(path, i))
  }

  return(raw_data)

}

#' Build raw_data using EDAV files
#'
#' @param path `str` Path to EDAV folder containing parquet files. This must
#' be the absolute file path from the Blob endpoint of the container.
#' @param container `azcontainer` An instance of an Azure container to connect
#' to. Pass [get_azure_storage_connection()] using defaults if not accessing
#' using a service principal.
#'
#' @returns `list` A list containing connections to the folders associated with
#' individual datasets in the original output of [get_all_polio_data()].
#' @keywords internal
#'
build_parquet_raw_data_edav <- function(path = NULL, container = NULL, ...) {

  if (is.null(container)) {
    container <- get_azure_storage_connection()
  }

  exist <- edav_io("exists.dir", default_dir = "",
                   file_loc = path, azcontainer = container)
  if (!exist) {
    cli::cli_abort("The directory does not exist on EDAV.")
  } else {
    rm(exist)
  }

  cli::cli_process_start("Building raw_data from EDAV parquet files")
  start <- Sys.time()

  raw_data <- NULL
  # Download files locally in the temp directory first
  dest <- "C:/Users/XRG9/Desktop/test"
  local_pq <- file.path(dest, basename(path))
  AzureStor::multidownload_adls_file(container,
                                     src = "GID/PEB/SIR/Sandbox/parquet_sandbox/*",
                                     dest = local_pq,
                                     recursive = TRUE,
                                     overwrite = TRUE
                                     )

  raw_data <- build_parquet_raw_data_local(local_pq)
  cli::cli_process_done()
  cli::cli_process_start(paste0("Built in ", difftime(start, Sys.time(), "mins"), " mins."))

  return(raw_data)

}
