#' @description add metadata tags to figures and tables
#' @import flextable
#' @param object obj: the figure or table to add metadata to
#' @param metadata raw.data$metadata$download_time
#' @export
f.metadata.tag <- function(object,
                           metadata = raw.data$metadata$download_time){

  object.class <- data.frame(class(object))

  if(exists("raw.data")){
    if(grepl("ggplot", object.class$class.object.)){
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
    if(class(object) == "flextable"){
      object <- flextable::add_footer_lines(object, paste0("Data Downloaded from POLIS: ", raw.data$metadata$download_time))
    }
  }else{
    stop()
  }
  return(object)
}

