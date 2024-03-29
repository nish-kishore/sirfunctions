#' Function to add metadata tags to figures and tables
#'
#'
#' @description add metadata tags to figures and tables
#' @import flextable
#' @param object ggplot or flex table object: the figure or table to add metadata to
#' @param time_tag str: a date and time string, defaults to raw.data$metadata$download_time
#' and will throw an error if "raw.data" doesn't exist in your environment or is called something else
#' @export
f.metadata.tag <- function(object,
                           time_tag = ifelse(exists("raw.data"), raw.data$metadata$download_time, NA)) {

  #flag to stop if time_tag not provided
  if (is.na(time_tag)){
    stop("There is no raw.data file in the environment or it's named something else. Please either load the raw.data file or provide a values for 'time_tag'")
  }

  #check to see if the object is either a ggplot or flextable
  if (!"ggplot" %in% class(object) &
      !"flextable" %in% class(object)) {
    stop("Object must be a ggplot or flextable object")
  }

  #if ggplot
  if ("ggplot" %in% class(object)) {

    #checks if caption already exists and either uses it or creates a new one
    if (is.null(object$labels$caption)) {
      object$labels$caption <- paste0("Data downloaded from POLIS: ",
                                      time_tag)
    } else{
      object$labels$caption <- paste0(object$labels$caption,
                                      "\n",
                                      "Data downloaded from POLIS: ",
                                      time_tag)
    }
  }

  #if flextable
  if ("flextable" %in% class(object)) {
    object <-
      flextable::add_footer_lines(object,
                                  paste0(
                                    "Data Downloaded from POLIS: ",
                                    time_tag
                                  ))
  }

  return(object)
}

