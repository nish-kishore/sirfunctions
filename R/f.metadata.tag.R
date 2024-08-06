#' Function to add metadata tags to figures and tables
#'
#'
#' @description add metadata tags to figures and tables
#' @importFrom flextable add_footer_lines
#'
#' @param object ggplot or flextable object: the figure or table to add metadata to
#' @param time_tag str: a date and time string, defaults to raw.data$metadata$download_time
#' @param raw_data Rds object: polio raw data or country data
#' @returns a `ggplot` or `flextable` object with metadata added
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
