#' Function to add metadata tags to figures and tables
#'
#' Add metadata tags to figures and tables. These include the download date of the dataset.
#' The function will return an error if both `raw_data` and `time_tag` parameters are `NULL`.
#'
#' @importFrom flextable add_footer_lines
#'
#' @param object `ggplot` or `flextable` The figure or table to add metadata to.
#' @param time_tag `str` A date and time string. Defaults to `raw.data$metadata$download_time`.
#' @param raw_data `list` outputs of [get_all_polio_data()] or [extract_country_data()].
#' @returns A `ggplot` or `flextable` object with metadata added.
#' @examples
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' df <- datasets::iris
#' p1 <- ggplot2::ggplot() + ggplot2::geom_col(data = df, ggplot2::aes(x = Sepal.Length, y = Sepal.Width))
#' p2 <- f.metadata.tag(p1, raw.data) # use raw.data download time
#' p3 <- f.metadata.tag(p1, time_tag = "2021-01-01") # use custom time tag
#'
#'
#' @export
f.metadata.tag <- function(object,
                           raw_data = NULL,
                           time_tag = NULL) {
  # flag to stop if time_tag not provided
  # use raw.data time_tag by default unless tag is explicitly stated
  if (is.null(raw_data) & is.null(time_tag)) {
    cli::cli_alert_warning("Please provide raw.data, ctry.data, or a value for 'time_tag'")
    return(object)
  } else if (!is.null(raw_data) & is.null(time_tag)) {
    tryCatch(
      {time_tag = raw_data$metadata$download_time
      },
      error = function(cond) {
        cond$message <- "raw_data does not contain metadata$download_time."
        stop(cond)
        }
      )
  }

  #
  # if (is.na(time_tag)) {
  #   stop("There is no raw.data file in the environment or it's named something else. Please either load the raw.data file or provide a values for 'time_tag'")
  # }

  # check to see if the object is either a ggplot or flextable
  if (!"ggplot" %in% class(object) &
    !"flextable" %in% class(object)) {
    stop("Object must be a ggplot or flextable object")
  }

  # if ggplot
  if ("ggplot" %in% class(object)) {
    # checks if caption already exists and either uses it or creates a new one
    if (is.null(object$labels$caption)) {
      object$labels$caption <- paste0(
        "Data downloaded from POLIS: ",
        time_tag
      )
    } else {
      object$labels$caption <- paste0(
        object$labels$caption,
        "\n",
        "Data downloaded from POLIS: ",
        time_tag
      )
    }
  }

  # if flextable
  if ("flextable" %in% class(object)) {
    object <-
      flextable::add_footer_lines(
        object,
        paste0(
          "Data Downloaded from POLIS: ",
          time_tag
        )
      )
  }

  return(object)
}
