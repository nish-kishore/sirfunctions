#' Function to return specific plot looks
#' @description Function to collate and return plot looks
#' @import ggplot2
#' @param type String describing a value to be returned. Accepted values include:
#' '02', 'epicurve', 'geomtile', 'gpln_type1', 'gpln_type2'
#' @return ggplot theme for addition into plot
#' @export
f.plot.looks <- function(type) {
  types <- c("02", "epicurve", "geomtile", "gpln_type1", "gpln_type2")

  if (!type %in% types) {
    stop(paste0(
      "Your selection is not one of the standard SIR plot looks, please choose one of the following: ",
      paste0(types, collapse = ", ")
    ))
  }


  if (type == "02") {
    return(list(
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white"),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.background = ggplot2::element_rect(colour = "black", fill = "white", linetype = "solid") # add a border to the legend
      )
    ))
  }

  if (type == "epicurve") {
    return(list(
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white"),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.position = "bottom",
        axis.text.x = ggplot2::element_text(angle = 90),
        legend.background = ggplot2::element_rect(colour = "black", fill = "white", linetype = "solid") # add a border to the legend
      )
    ))
  }


  if (type == "geomtile") {
    return(list(
      ggplot2::theme(
        # panel.grid.minor = ggplot2::element_blank(),
        # panel.background = ggplot2::element_rect(fill = "white")
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.background = ggplot2::element_rect(colour = "black", fill = "white", linetype = "solid") # add a border to the legend
      )
    ))
  }

  if (type == "gpln_type1") {
    return(
      ggplot2::theme(
        panel.spacing = ggplot2::unit(0, "pt"),
        panel.border = ggplot2::element_rect(colour = "grey50", fill = NA),
        legend.position = "bottom",
        axis.text.x = ggplot2::element_text(face = "bold"),
        axis.text.y = ggplot2::element_text(face = "bold"),
        axis.title.x = ggplot2::element_text(face = "bold"),
        axis.title.y = ggplot2::element_text(face = "bold"),
        legend.text = ggplot2::element_text(face = "bold"),
        legend.title = ggplot2::element_text(face = "bold"),
        plot.title = ggplot2::element_text(hjust = 0.5),
        strip.text.x = ggplot2::element_text(
          face = "bold"
        )
      )
    )
  }

  if (type == "gpln_type2") {
    return(
      ggplot2::theme(
        panel.grid = ggplot2::element_line(colour = "transparent"),
        panel.background = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5),
        legend.position = "bottom"
      )
    )
  }
}
