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

#' Generate KPI maps
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Generalized function to build KPI maps.
#'
#' @inheritParams generate_kpi_npafp_map
#' @param indicator `quoted var` Indicator variable.
#' @param color_scheme `list` Named list with color mappings.
#' @param legend_title `str` Title of the legend.
#'
#' @return `ggplot` A ggplot object.
#' @keywords internal
generate_kpi_map <- function(c2, ctry_sf, who_region, indicator, .year,
                             color_scheme, legend_title) {

  plotlooks02 <- list(
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
  )

  ctry_sf <- ctry_sf |>
    dplyr::filter(.data$WHO_REGION %in% who_region)

  c2 <- c2 |>
    add_risk_category() |>
    dplyr::filter(.data$year == .year,
                  .data$Region %in% who_region)

  priority_ctry <- c2 |>
    dplyr::filter(.data$`SG Priority Level` == "HIGH")

  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = {{ indicator }}), c2, color = NA,
                     na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = color_scheme, name = legend_title,
                               na.value = "grey") +
    ggplot2::geom_sf(data = ctry_sf, fill = NA, linewidth = 0.5, size = 0.5,
                     color  ="grey") +
    ggplot2::geom_sf(data = ctry_sf |>
                       filter(.data$ADM0_NAME %in% unique(priority_ctry$ctry)),
                     fill = NA, linewidth = 0.5, size = 0.5,
                     color = "black") +
    ggplot2::labs(caption = "*Countries outlined in black are designated High risk by SG") +
    plotlooks02

  return(map)

}

#' Generate district level NPAFP maps by region
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Generates a map of district NPAFP rates.
#'
#'
#' @param c2 `tibble` Output of [generate_c2_table()].
#' @param ctry_sf `sf` Country shapefile in long format.
#' @param dist_sf `sf` District shapefile in long format.
#' @param who_region `str` WHO region. Valid values are:
#' - `"AFRO"`: African Region
#' - `"AMRO"`: Region of the Americas
#' - `"EMRO"`: Eastern Mediterranean Region
#' - `"EURO"`: European Region
#' - `"SEARO"`:South-East Asia Region
#' - `"WPRO"`:Western Pacific Region
#' @param .year `num` Numeric year of interest.
#' @param output_path `str` Where to output the figure to. Defaults to the path
#' initialized when [init_kpi()] was ran.
#'
#' @return `ggplot` A district NPAFP map.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' c2 <- generate_c2_table(raw_data$afp, raw_data$dist, "2021-01-01",
#' "2023-12-31", c("ctry", "dist", "adm2guid", "year"))
#' ctry_sf <- load_clean_ctry_sp(type="long")
#' dist_sf <- load_clean_dist_sp(type="long")
#' map <- generate_kpi_npafp_map(c2, ctry_sf, dist_sf, "AFRO", 2022, getwd())
#' }
generate_kpi_npafp_map <- function(c2, ctry_sf, dist_sf, who_region, .year,
                                   output_path = Sys.getenv("KPI_FIGURES")) {

  if (!who_region %in% c("AFRO", "EMRO", "SEARO", "WPRO", "AMRO", "EURO")) {
    cli::cli_abort("Please enter a valid who_region value.")
  }
  color.npafp <- c(
    "Silent (u15pop >= 100K)"="#762a83",
    "No cases (u15pop < 100K)" = "#999999",
    "< 1" = "#d7191c",
    ">= 1 & <2" = "#fdae61",
    ">= 2 & <3" = "#74c476",
    ">=3" = "#006d2c",
    "Missing Pop" = "#3182bd")

  c2 <- c2 |>
    dplyr::left_join(dist_sf, by = c("adm2guid" = "GUID",
                                     "year" = "active.year.01")) |>
    sf::st_as_sf()

  map <- generate_kpi_map(c2, ctry_sf, who_region, npafp_cat, .year,
                          color.npafp,
                          "NPAFP Rate")

  if (who_region == "WPRO") {
    wpro.aoi <- c("PAPUA NEW GUINEA", "PHILLIPINES", "AUSTRAILIA", "VIETNAM",
                  "LAO PEOPLE'S DEMOCRATIC REPUBLIC")
    aoi <- sf::st_bbox(ctry_sf |> dplyr::filter(ADM0_NAME %in% wpro.aoi))
    map <- map +
      ggplot2::coord_sf(xlim = aoi[c("xmin", "xmax")], ylim = aoi[c("ymin", "ymax")])
  }

  file_name <- paste0("npafp_maps_", who_region, "_", .year, ".png")

  ggplot2::ggsave(file.path(output_path, file_name),
                  dpi = 300, height = 12, width = 10, bg="white")
  return(map)
}

#' Generate district level stool adequacy maps
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Generates a map of district level stool adequacy maps
#'
#' @inheritParams generate_kpi_npafp_map
#'
#' @return `ggplot` A stool adequacy map.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' c2 <- generate_c2_table(raw_data$afp, raw_data$dist, "2021-01-01",
#' "2023-12-31", c("ctry", "dist", "adm2guid", "year"))
#' ctry_sf <- load_clean_ctry_sp(type="long")
#' dist_sf <- load_clean_dist_sp(type="long")
#' map <- generate_kpi_stool_map(c2, ctry_sf, dist_sf, "AFRO", 2022, getwd())
#' }
generate_kpi_stool_map <- function(c2, ctry_sf, dist_sf, who_region, .year,
                                   output_path = Sys.getenv("KPI_FIGURES")) {
  stool_color <- c(
    "Zero AFP cases" = "#999999",
    "Unable to Assess" = "white",
    "<50%" = "#d7191c",
    "50%-79%" = "#fdae61",
    "80%+" = "#2c7bb6"
  )

  c2 <- c2 |>
    dplyr::left_join(dist_sf, by = c("adm2guid" = "GUID",
                                     "year" = "active.year.01")) |>
    sf::st_as_sf()

  map <- generate_kpi_map(c2, ctry_sf, who_region, stool_cat, .year,
                          stool_color,
                          "Stool Adequacy")

  if (who_region == "WPRO") {
    wpro.aoi <- c("PAPUA NEW GUINEA", "PHILLIPINES", "AUSTRAILIA", "VIETNAM",
                  "LAO PEOPLE'S DEMOCRATIC REPUBLIC")
    aoi <- sf::st_bbox(ctry_sf |> dplyr::filter(ADM0_NAME %in% wpro.aoi))
    map <- map +
      ggplot2::coord_sf(xlim = aoi[c("xmin", "xmax")], ylim = aoi[c("ymin", "ymax")])
  }

  file_name <- paste0("stool_ad_maps_", who_region, "_", .year, ".png")

  ggplot2::ggsave(file.path(output_path, file_name),
                  dpi = 300, height = 12, width = 10, bg="white")
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

# Violin plots ----

