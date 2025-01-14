# Maps ----
#' SG Prioritization Map
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
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
generate_sg_priority_map <- function(ctry_risk_cat = NULL,
                            year = lubridate::year(Sys.Date()),
                            output_path = Sys.getenv("KPI_FIGURES")) {

  if (is.null(ctry_risk_cat)) {
    ctry_risk_cat <- edav_io("read", file_loc = get_constant("CTRY_RISK_CAT"))
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

# Bar charts ----
## High priority countries ----
#' Generate KPI indicator bar charts
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A generalized function to create bar charts using the KPI tables.
#' @param df `tibble` A KPI table, namely C1-C4.
#' @param indicator `quoted var` Name of the indicator within the KPI table.
#' @param target `num` A numeric target.
#' @param label `quoted var` Name of the column containing labels.
#' @param faceting `ggplot2` A ggplot2 faceting object.
#' Either using [ggplot2::facet_grid()] or [ggplot2::facet_wrap()].
#' @param y.axis.title `Str` Title of the y axis.
#'
#' @return `ggplot2` A bar chart.
#' @keywords internal
generate_kpi_barchart <- function(df, indicator, target, label, faceting,
                                  y.axis.title) {
  plotlooks.bar <- list(
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white"),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90),
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(
        colour = "black",
        fill = "white",
        linetype = "solid"
      ), # add a border to the legend
      axis.title = ggplot2::element_text(size = 10, face = "bold"),
      strip.text = ggplot2::element_text(size = 10)
    )
  )

  bar_plot <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = df,
      ggplot2::aes(
        x = stats::reorder(ctry.short, {{ indicator }}),
        y = {{ indicator }}
      ), fill = "#fd8d3c",
      stat = "identity"
    ) +
    ggplot2::scale_fill_manual(name = "Risk Category") +
    ggplot2::scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
    ggplot2::geom_hline(yintercept = target, linetype = "dashed", color = "black") +
    ggplot2::geom_text(
      data = df,
      ggplot2::aes(
        x = stats::reorder(ctry.short, {{ indicator }}),
        y = {{ indicator }} / 2 + 5,
        label = {{ label }}
      ),
      size = 2.6
    ) +
    ggplot2::labs(y = y.axis.title) +
    faceting +
    ggplot2::theme_bw() +
    plotlooks.bar +
    ggplot2::theme(
      strip.text.y = ggplot2::element_text(angle = 0),
      strip.text.y.left = ggplot2::element_text(angle = 0),
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

#' Generate KPI NPAFP bar chart summary
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Generates a bar chart highlighting percentage of priority countries with 80%
#' of 100,000+ under 15y pop districts achieving npAFP rate of  ≥2/100,000.
#'
#' @param c1 `tibble` Output of [generate_c1_table()].
#' @param afp_data `tibble` AFP dataset. List item of the output of
#' [get_all_polio_data()].
#'
#' @return `ggplot2` A barplot.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' c1 <- generate_c1_table(raw_data, "2021-01-01", "2023-12-31", rolling = TRUE)
#' plot <- generate_kpi_npafp_bar(c1, raw_data$afp)
#' }
generate_kpi_npafp_bar <- function(c1, afp_data,
                                   output_path = Sys.getenv("KPI_FIGURES")) {

  ctry_abbrev <- get_ctry_abbrev(afp_data)
  priority_ctry <- c1 |>
    dplyr::left_join(ctry_abbrev,
                     by = c("ctry" = "place.admin.0", "whoregion")) |>
    dplyr::filter(.data$`SG Priority Level` == "HIGH") |>
    dplyr::mutate(prop_met_npafp = .data$prop_met_npafp * 100)

  bar_plot <- generate_kpi_barchart(priority_ctry,
                        .data$prop_met_npafp,
                        80,
                        .data$npafp_label,
                        faceting = ggplot2::facet_grid(.data$rolling_period ~ .data$whoregion,
                                              scales = "free", space = "free",
                                              switch = "y",
                                              labeller = label_wrap_gen(13)),
                        y.axis.title = "% Districts w/ U15Pop>=100k, NPAFP >=2 (AFRO, EMRO, SEARO), NPAFP >= 1 (WPRO)")

  ggplot2::ggsave(file.path(output_path, "kpi_bar_npafp.png"),
                  dpi = 400, height = 4, width = 12, bg="white")
  return(bar_plot)

}

#' Generate KPI EV rate bar chart summary
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Generates a bar chart highlighting percentage of priority countries
#' achieving 80% of ES sites meeting sensitivity threshold of at least 50%
#' samples positive for enterovirus. Sites included are sites with at least
#' 10 collections in the last 12 months.
#'
#' @inheritParams generate_kpi_npafp_bar
#'
#' @return `ggplot2` A barplot.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' c1 <- generate_c1_table(raw_data, "2021-01-01", "2023-12-31", rolling = TRUE)
#' plot <- generate_kpi_evdetect_bar(c1, raw_data$afp)
#' }
generate_kpi_evdetect_bar <- function(c1, afp_data,
                                   output_path = Sys.getenv("KPI_FIGURES")) {

  ctry_abbrev <- get_ctry_abbrev(afp_data)
  priority_ctry <- c1 |>
    dplyr::left_join(ctry_abbrev,
                     by = c("ctry" = "place.admin.0", "whoregion")) |>
    dplyr::filter(.data$`SG Priority Level` == "HIGH") |>
    dplyr::mutate(prop_met_ev = .data$prop_met_ev * 100)

  bar_plot <- generate_kpi_barchart(priority_ctry,
                                    .data$prop_met_ev,
                                    80,
                                    .data$ev_label,
                                    faceting = ggplot2::facet_grid(.data$rolling_period ~ .data$whoregion,
                                                                   scales = "free", space = "free",
                                                                   switch = "y",
                                                                   labeller = label_wrap_gen(13)),
                                    y.axis.title = "% sites meeting EV detection rate of 50%")

  ggplot2::ggsave(file.path(output_path, "kpi_bar_es.png"),
                  dpi = 400, height = 3.5, width = 12, bg="white")

  return(bar_plot)

}

#' Generate KPI Stool adequacy bar chart summary
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Generates a bar chart highlighting percentage of priority countries
#' achieving stool adequacy targets.
#'
#' @inheritParams generate_kpi_npafp_bar
#'
#' @return `ggplot2` A barplot.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' c1 <- generate_c1_table(raw_data, "2021-01-01", "2023-12-31", rolling = TRUE)
#' plot <- generate_kpi_stoolad_bar(c1, raw_data$afp)
#' }
generate_kpi_stoolad_bar <- function(c1, afp_data,
                                   output_path = Sys.getenv("KPI_FIGURES")) {

  ctry_abbrev <- get_ctry_abbrev(afp_data)
  priority_ctry <- c1 |>
    dplyr::left_join(ctry_abbrev,
                     by = c("ctry" = "place.admin.0", "whoregion")) |>
    dplyr::filter(.data$`SG Priority Level` == "HIGH") |>
    dplyr::mutate(prop_met_stool = .data$prop_met_stool * 100)

  bar_plot <- generate_kpi_barchart(priority_ctry,
                                    .data$prop_met_stool,
                                    80,
                                    .data$stool_label,
                                    faceting = ggplot2::facet_grid(.data$rolling_period ~ .data$whoregion,
                                                                   scales = "free", space = "free",
                                                                   switch = "y",
                                                                   labeller = label_wrap_gen(13)),
                                    y.axis.title = "% Stool Adequacy")

  ggplot2::ggsave(file.path(output_path, "kpi_bar_stoolad.png"),
                  dpi = 400, height = 4, width = 12, bg="white")
  return(bar_plot)
}


