#' @description add metadata tags to figures and tables
#' @param object_type str: specify whether a "figure" or a "table"
#' @param object obj: the figure or table to add metadata to
#' @param metadata raw.data$metadata$download_time
f.metadata.tag <- function(object_type,
                           object,
                           metadata = raw.data$metadata$download_time){

  if(object_type == "figure"){
    if(is.null(object$labels$caption)){
      object$labels$caption <- paste0("Data downloaded from POLIS: ",
                                      metadata)
    }else{
      object$labels$caption <- paste0(
        object$labels$caption,
        "\n",
        "Data downloaded from POLIS: ",
        metadata
      )
    }
  }
  if(object_type == "table"){
    object <- flextable::add_footer_lines(object, paste0("Data Downloaded from POLIS: ", raw.data$metadata$download_time))
  }
  return(object)
}

