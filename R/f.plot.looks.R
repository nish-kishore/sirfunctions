
#' Function to return specific plot looks
#' @description Function to collate and return plot looks
#' @param type String describing a value to be returned
#' @return ggplot theme for addition into plot
f.plot.look <- function(type) {

  types <- c("02", "epicurve", "geomtile", "gpln_type1" ,"gpln_type2")

  if (!type %in% types){

    stop(paste0("Your selection is not one of the standard SIR plot looks, please choose one of the following: ",
                paste0(types, collapse = ", ")))

  }


  if (type == "02") {
    return(list(
      theme(
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(colour = "black", fill = "white", linetype = "solid") # add a border to the legend
      )
    ))
  }

  if (type == "epicurve") {
    return(list(
      theme(
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90),
        legend.background = element_rect(colour = "black", fill = "white", linetype = "solid") # add a border to the legend
      )
    ))
  }


  if (type == "geomtile") {
    return(list(
      theme(
        # panel.grid.minor = element_blank(),
        # panel.background = element_rect(fill = "white")
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(colour = "black", fill = "white", linetype = "solid") # add a border to the legend
      )
    ))
  }

  if (type == "gpln_type1") {
    return(
      theme(
        panel.spacing = unit(0, "pt"),
        panel.border = element_rect(colour = "grey50", fill = NA),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(
          face = "bold"
        )
      )
    )
  }

  if (type == "gpln_type2") {
    return(
      theme(
        panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
    )
  }
}
