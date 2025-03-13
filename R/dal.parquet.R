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
  df_names <- names(raw_data)

  for (i in df_names) {
    switch()
  }

}

# Private functions ----

#' Gets the column used to partition a column
#'
#' @param name `str` Name of the column
#'
#' @return `chr` A character vector of columns to partition with.
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
         "dist.pop" = c("ADM0_NAME", "year"),
         "prov.pop" = c("ADM0_NAME", "year"),
         "ctry.pop" = c("ADM0_NAME", "year"),
         "global.ctry" = c("ADM0_NAME"),
         "global.prov" = c("ADM0_NAME"),
         "global.dist" = c("ADM0_NAME"),
         "roads" = c("continent"),
         "cities" = c("CTRY_NAME"),
         "metadata" = "download_time"
         )
}
