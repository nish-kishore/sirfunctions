#' SG Prioritization Map
#' @description
#' `r lifecycle::badge("experimental")`
#' Creates a map showing the SG prioritization map.
#'
#' @param ctry_risk_cat `tibble` Risk category for each country.
#' Defaults to `NULL`, which downloads the SG risk category data set from EDAV.
#' @param year `tibble` Active year for the country shape files.
#' @param output_path `str` Where to output the figure to. Defaults to the
#' path to the figures folder set when running [init_kpi()].
#'
#' @return `ggplot` A map.
#' @export
#'
#' @examples
#' \dontrun{
#' sg_priority_map(output_path = getwd())
#' }
sg_priority_map <- function(ctry_risk_cat = NULL,
                            year = lubridate::year(Sys.Date()),
                            output_path = Sys.getenv("KPI_FIGURES")) {

  if (is.null(ctry_risk_cat)) {
    ctry_risk_cat = edav_io("read", file_loc = get_constant("CTRY_RISK_CAT"))
  }

  color.risk.cat <- c(
    "HIGH" = "#d73027",
    "MEDIUM" = "orange",
    "LOW (WATCHLIST)" = "#9ecae1"
  )

  plotlooks02 <- list(
    ggplot2::theme(
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "bottom",
      legend.background = element_rect(colour = "black",
                                       fill = "white",
                                       linetype = "solid")
    )
  )

  ctry_sf <- load_clean_ctry_sp(type = "long") |>
    dplyr::filter(.data$active.year.01 == year)
  ctry_sf <- ctry_sf |>
    dplyr::left_join(ctry_risk_cat, by = c("ADM0_NAME" = "Country"))

  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = NULL), ctry_sf, fill = "white", color = "grey") +
    ggplot2::geom_sf(aes(fill = .data$`SG Priority Level`),
                     ctry_sf |>
                       dplyr::filter(.data$ADM0_NAME %in%
                                       ctry_risk_cat$Country,
                                     .data$`SG Priority Level` != "LOW"),
                     color = "black") +
    ggplot2::scale_fill_manual(values = color.risk.cat,
                                name = "Priority Level", na.value = "white") +
    plotlooks02

  ggplot2::ggsave(file.path(output_path, "sg_priority_map.png"))

  return(map)

}

u15_dist_prop_bar <- function(c1) {

}
