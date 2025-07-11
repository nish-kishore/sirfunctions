#' Set plot looks
#'
#' The function serves to collate and return plot looks. Depending on the parameter,
#' specific values in a `ggplot2` theme object will be returned.
#'
#' @param type `str` Type of graph format. Accepted values include:
#' - `"02"`
#' - `"epicurve"`
#' - `"geomtile"`
#' - `"gpln_type1"`
#' - `"gpln_type2"`
#' @returns `ggplot2 theme obj` A theme object that can be added into an existing plot.
#' @examples
#' \dontrun{
#' epicurve_looks <- f.plot.looks("epicurve")
#' df <- datasets::iris
#' p1 <- ggplot2::ggplot() +
#'   ggplot2::geom_col(data = df, ggplot2::aes(x = Sepal.Length, y = Sepal.Width))
#' p2 <- p1 + epicurve_looks
#' }
#'
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
