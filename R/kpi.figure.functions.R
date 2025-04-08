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

  ctry_risk_cat <- ctry_risk_cat |>
    dplyr::mutate(
      sg_priority_level = factor(.data$`SG Priority Level`, levels = c(
        "LOW", "LOW (WATCHLIST)", "MEDIUM", "HIGH"))
    )

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
    ggplot2::geom_sf(aes(fill = .data$sg_priority_level),
                     ctry_sf |>
                       dplyr::filter(.data$ADM0_NAME %in%
                                       ctry_risk_cat$Country,
                                     .data$sg_priority_level != "LOW"),
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
#' @param .year_label `str` Year of the rollup.
#' @param .ctry_sf `sf` Country shapefile.
#' @param .dist_sf `sf` District shapefile.
#'
#' @return `ggplot` A ggplot object.
#' @export
generate_kpi_map <- function(c2, who_region, indicator, .year_label,
                             risk_category,
                             color_scheme, legend_title,
                             .ctry_sf, .dist_sf) {

  plotlooks02 <- f.plot.looks("02")

  # Calculate the end date year
  year <- c2 |>
    dplyr::filter(year_label == .year_label) |>
    dplyr::pull(analysis_year_end) |>
    unique() |>
    lubridate::year()

  if (length(year) == 0) {
    cli::cli_abort(paste0(.year_label, " is not a valid year_label. Try again."))
  }

  if (is.null(.ctry_sf)) {
    .ctry_sf <- suppressMessages(load_clean_ctry_sp(
      st_year = year,
      end_year = year,
      type = "long"
    ))
  }

  if (is.null(.dist_sf)) {
    .dist_sf <- suppressMessages(load_clean_dist_sp(
      st_year = year,
      end_year = year,
      type = "long"
    ))
  }

  c2 <- c2 |>
    dplyr::mutate(year = lubridate::year(analysis_year_end)) |>
    dplyr::filter(
      .data$year_label == .year_label,
      .data$Region %in% who_region,
      !is.na(!!rlang::sym(indicator))
    ) |>
    dplyr::left_join(.dist_sf, by = c(
      "adm2guid" = "GUID",
      "year" = "active.year.01"
    )) |>
    sf::st_as_sf()

  if (!is.null(risk_category)) {
    c2 <- c2 |>
      dplyr::filter(.data$`SG Priority Level` %in% risk_category)
  }

  .ctry_sf <- .ctry_sf |>
    dplyr::filter(.data$WHO_REGION %in% who_region)
  high_risk <- c2 |>
    dplyr::filter(.data$`SG Priority Level` == "HIGH") |>
    dplyr::pull(.data$ctry) |>
    unique()

  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = !!rlang::sym(indicator)), c2,
      color = NA,
      na.rm = TRUE
    ) +
    ggplot2::scale_fill_manual(
      values = color_scheme, name = legend_title
    ) +
    ggplot2::geom_sf(
      data = .ctry_sf, fill = NA, linewidth = 0.5, size = 0.5,
      color = "grey"
    ) +
    ggplot2::geom_sf(
      data = .ctry_sf |>
        filter(.data$ADM0_NAME %in% high_risk),
      fill = NA, linewidth = 0.5, size = 0.5,
      color = "black"
    ) +
    ggplot2::labs(caption = "*Countries outlined in black are designated High risk by SG") +
    plotlooks02

  if ("WPRO" %in% who_region & length(who_region) == 1) {
    wpro.aoi <- c(
      "PAPUA NEW GUINEA", "PHILLIPINES", "AUSTRAILIA", "VIETNAM",
      "LAO PEOPLE'S DEMOCRATIC REPUBLIC"
    )
    aoi <- sf::st_bbox(.ctry_sf |> dplyr::filter(.data$ADM0_NAME %in% wpro.aoi))
    map <- map +
      ggplot2::coord_sf(xlim = aoi[c("xmin", "xmax")], ylim = aoi[c("ymin", "ymax")])
  }

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
#' @param year_label `str` Roll up year (i.e., `"Year 1", "Year 2", ...`).
#' @param who_region `str` A WHO region or a list of regions. Valid values are:
#' - `"AFRO"`: African Region
#' - `"AMRO"`: Region of the Americas
#' - `"EMRO"`: Eastern Mediterranean Region
#' - `"EURO"`: European Region
#' - `"SEARO"`:South-East Asia Region
#' - `"WPRO"`:Western Pacific Region
#' @param output_path `str` Where to output the figure to. Defaults to the path
#' initialized when [init_kpi()] was ran.
#' @param ctry_sf `sf` Country shapefile in long format. Output of
#' [load_clean_ctry_sp(type = "long")]. Defaults to `NULL`, which will download
#' the required country shapefile when the function is ran.
#' @param dist_sf `sf` District shapefile in long format. Output of
#' [load_clean_dist_sp(type = "long")]. Defaults to `NULL`, which will download
#' the required district shapefile when the function is ran.
#'
#' @return `ggplot` A district NPAFP map.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' c2 <- generate_c2_table(raw_data$afp, raw_data$dist, "2021-01-01",
#' "2023-12-31", c("ctry", "dist", "adm2guid", "year"))
#' map <- generate_kpi_npafp_map(c2, "Year 1", "AFRO", output_path = getwd())
#' }
generate_kpi_npafp_map <- function(c2, year_label, who_region = NULL,
                                   risk_category = NULL,
                                   output_path = Sys.getenv("KPI_FIGURES"),
                                   ctry_sf = NULL, dist_sf = NULL) {

  npafp_col <- c(
    "Area did not exist during the entire rolling period" = "lightgrey",
    "Silent (u15pop >= 100K)" = "#762a83",
    "No cases (u15pop < 100K)" = "#999999",
    "< 1" = "#d7191c",
    ">= 1 & <2" = "#fdae61",
    ">= 2 & <3" = "#74c476",
    ">=3" = "#006d2c",
    "Missing Pop" = "#3182bd"
  )

  c2 <- c2 |> dplyr::filter(!is.na(.data$npafp_cat))

  map <- generate_kpi_map(
    c2, who_region, "npafp_cat", year_label, risk_category, npafp_col, "NPAFP Rate",
    ctry_sf, dist_sf
  )

  if (is.null(who_region)) {
    file_name <- paste0("npafp_maps_global_", year_label,".png")
  } else {
    file_name <- paste0("npafp_maps_", paste0(who_region, collapse = "_")
                        ,"_", year_label,".png")
  }

  ggplot2::ggsave(file.path(output_path, file_name),
    dpi = 300, height = 12, width = 10, bg = "white"
  )
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
#' map <- generate_kpi_stool_map(c2, "Year 1", "AFRO", output_path = getwd())
#' }
generate_kpi_stool_map <- function(c2, year_label, who_region = NULL,
                                   risk_category = NULL,
                                   output_path = Sys.getenv("KPI_FIGURES"),
                                   ctry_sf = NULL, dist_sf = NULL) {

  stool_color <- c(
    "Area did not exist during the entire rolling period" = "lightgrey",
    "Zero AFP cases" = "#999999",
    "Unable to Assess" = "white",
    "<50%" = "#d7191c",
    "50%-79%" = "#fdae61",
    "80%+" = "#2c7bb6"
  )

  c2 <- c2 |> dplyr::filter(!is.na("stool_cat"))

  map <- generate_kpi_map(
    c2, who_region, "stool_cat", year_label, risk_category, stool_color,
    "Stool Adequacy", ctry_sf, dist_sf
  )

  if (is.null(who_region)) {
    file_name <- paste0("stool_ad_maps_global_", year_label, ".png")
  } else {
    file_name <- paste0(
      "stool_ad_maps_", paste0(who_region, collapse = "_"),
      "_", year_label, ".png"
    )
  }

  ggplot2::ggsave(file.path(output_path, file_name),
    dpi = 300, height = 12, width = 10, bg = "white"
  )
  return(map)
}


#' EV detection rate map
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Generates a map of EV detection rates for each environmental surveillance site.
#'
#' @param c3 `tibble` Output of [generate_c3_table()]. This must be summarized
#' at the site level (i.e., use the default `.group_by`) param of the
#' [generate_c3_table()].
#' @param .year_label `str` Roll up year (i.e., `"Year 1", "Year 2", ...`).
#' @param who_region `str` Name of the region or a list of regions.
#' @param output_path `str` Where to output the figure to. Defaults to the
#' figure path assigned after running [init_kpi()].
#' @param dot_size `num` Point size.
#'
#' @return `ggplot` A map showing EV detection rate by site.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' c3 <- generate_c3_table(raw_data$es, "2021-01-01", "2023-12-31")
#' map <- generate_kpi_ev_map(c3, "Year 1", "AFRO", getwd())
#' }
generate_kpi_ev_map <- function(c3, .year_label, who_region = NULL,
                                output_path = Sys.getenv("KPI_FIGURES"),
                                dot_size = 2.3,
                                ctry_sf = NULL) {

  ev_cols <- c("<50%" = "red", "50% to <80%" = "#f16913",
               "80-100%" = "#0070c0", "<5 samples collected" = "black"
               )
  plotlooks02 <- list(
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
      legend.background = ggplot2::element_rect(colour = "black",
                                                fill = "white",
                                                linetype = "solid")
    )
  )

  year <- c3 |>
    dplyr::filter(year_label == .year_label) |>
    dplyr::pull(analysis_year_end) |>
    unique() |>
    lubridate::year()

  if (length(year) == 0) {
    cli::cli_abort(paste0(.year_label, " is not a valid year_label. Try again."))
  }

  if (is.null(ctry_sf)) {
    ctry_sf <- suppressMessages(load_clean_ctry_sp(st.year = year,
                                                   end.year = year,
                                                   type = "long"))
  }

  c3 <- c3 |> dplyr::filter(year_label == .year_label)

  if (!is.null(who_region)) {
    c3 <- c3 |>
      dplyr::mutate(region = get_region(.data$ADM0_NAME)) |>
      dplyr::filter(.data$region %in% who_region)
    ctry_sf <- ctry_sf |>
      dplyr::filter(
        .data$WHO_REGION %in% who_region
      )
  }

  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = ctry_sf, color = "grey", fill = NA, linewidth = 0.5) +
    ggplot2::geom_point(
      ggplot2::aes(as.numeric(.data$lng), as.numeric(.data$lat),
                   color = .data$ev_det_cat),
      c3, size = dot_size, alpha = 0.7,  stroke = 1) +
    plotlooks02 +
    ggplot2::scale_color_manual(values = ev_cols, name = "EV detection rate") +
    ggplot2::theme(legend.position = "right")

  if ("WPRO" %in% who_region & length(who_region) == 1) {
    wpro_aoi <- c("PAPUA NEW GUINEA", "PHILLIPINES", "AUSTRAILIA", "VIETNAM",
                  "LAO PEOPLE'S DEMOCRATIC REPUBLIC")
    aoi <- sf::st_bbox(ctry_sf |> dplyr::filter(ADM0_NAME %in% wpro_aoi))
    map <- map +
      ggplot2::coord_sf(xlim = aoi[c("xmin", "xmax")], ylim = aoi[c("ymin", "ymax")])
  }

  if (is.null(who_region)) {
    file_name <- paste0("ev_rate_map_global_", .year_label,".png")
  } else {
    file_name <- paste0("ev_rate_map_", paste0(who_region, collapse = "_")
                        ,"_", year,".png")
  }
  ggplot2::ggsave(filename = paste0(file.path(output_path, file_name)),
         dpi = 300, height = 12, width = 10, bg = "white")

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
#' @param indicator `str` Name of the indicator within the KPI table.
#' @param target `num` A numeric target.
#' @param label `str` Name of the column containing labels.
#' @param faceting `ggplot2` A ggplot2 faceting object.
#' Either using [ggplot2::facet_grid()] or [ggplot2::facet_wrap()].
#' @param y.axis.title `Str` Title of the y axis.
#'
#' @return `ggplot2` A bar chart.
#' @export
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
        x = stats::reorder(ctry.short, !!rlang::sym(indicator)),
        y = !!rlang::sym(indicator)
      ), fill = "#fd8d3c",
      stat = "identity"
    ) +
    ggplot2::scale_fill_manual(name = "Risk Category") +
    ggplot2::scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
    ggplot2::geom_hline(yintercept = target, linetype = "dashed", color = "black") +
    ggplot2::geom_text(
      data = df,
      ggplot2::aes(
        x = stats::reorder(ctry.short, !!rlang::sym(indicator)),
        y = !!rlang::sym(indicator) / 2 + 5,
        label = !!rlang::sym(label)
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

  return(bar_plot)
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
                     by = c("ctry" = "place.admin.0", "Region" = "whoregion")) |>
    dplyr::filter(.data$`SG Priority Level` == "HIGH") |>
    dplyr::mutate(prop_met_npafp = .data$prop_met_npafp)

  bar_plot <- generate_kpi_barchart(priority_ctry,
                        "prop_met_npafp",
                        80,
                        "npafp_label",
                        faceting = ggplot2::facet_grid(.data$rolling_period ~ .data$Region,
                                              scales = "free", space = "free",
                                              switch = "y",
                                              labeller = label_wrap_gen(13)),
                        y.axis.title = "% Districts w/ U15Pop>=100k, NPAFP >= 3 (ENDEMICS),\n NPAFP >=2 (AFRO, EMRO, SEARO), NPAFP >= 1 (WPRO)") +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(size = 8)
    )

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
                     by = c("ctry" = "place.admin.0", "Region" = "whoregion")) |>
    dplyr::filter(.data$`SG Priority Level` == "HIGH") |>
    dplyr::mutate(prop_met_ev = .data$prop_met_ev)

  bar_plot <- generate_kpi_barchart(priority_ctry,
                                    "prop_met_ev",
                                    80,
                                    "ev_label",
                                    faceting = ggplot2::facet_grid(.data$rolling_period ~ .data$Region,
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
                     by = c("ctry" = "place.admin.0", "Region" = "whoregion")) |>
    dplyr::filter(.data$`SG Priority Level` == "HIGH") |>
    dplyr::mutate(prop_met_stool = .data$prop_met_stool)

  bar_plot <- generate_kpi_barchart(priority_ctry,
                                    "prop_met_stool",
                                    80,
                                    "stool_label",
                                    faceting = ggplot2::facet_grid(.data$rolling_period ~ .data$Region,
                                                                   scales = "free", space = "free",
                                                                   switch = "y",
                                                                   labeller = label_wrap_gen(13)),
                                    y.axis.title = "% Stool Adequacy")

  ggplot2::ggsave(file.path(output_path, "kpi_bar_stoolad.png"),
                  dpi = 400, height = 4, width = 12, bg="white")
  return(bar_plot)
}

# Violin plots ----

#' Generates KPI violin plot
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function is generalized to produce violin plots used in the KPI code.
#'
#' @param df `tibble` Data to be used.
#' @param country.label `quoted var` Country label.
#' @param interval `quoted var` Interval to use.
#' @param priority_level `quoted var` Priority level column.
#' @param faceting `ggplot::facet` A faceting object.
#' @param target `num` Numeric target.
#' @param y.min `num` Minimum used in the y-axis.
#' @param y.max `num` Maximum used in the y-axis.
#'
#' @return `ggplot` A plot object.
#' @export
generate_kpi_violin <- function(
    df,
    country.label,
    interval,
    priority_level,
    faceting,
    target,
    y.min = 0,
    y.max) {
  map <-   ggplot(
    df |> filter(!!rlang::sym(interval) <= y.max, !!rlang::sym(interval) >= 0),
    aes(x = !!rlang::sym(country.label), y = !!rlang::sym(interval), fill = !!rlang::sym(priority_level))
  ) +
    geom_violin(na.rm = T, scale = "width", linewidth = 0) +
    stat_summary(
      fun.y = median, geom = "text",
      size = 2, color = "black", aes(label = round(after_stat(y)))
    ) +
    faceting +
    { if (!is.null(target)) {
      geom_hline(yintercept = target, linetype = "dashed", color = "#525252")
    }
    } +
    scale_y_continuous(
      breaks = seq(0, y.max, by = 50),
      limits = c(y.min, y.max + 5),
      expand = c(0, 0)
    ) +
    geom_text(aes(label = paste0("n=", after_stat(count))),
              y = y.max - 5,
              stat = "count",
              size = 2
    ) +
    labs(y = "Days", x = NULL) +
    theme_bw() +
    theme(
      legend.position = "top",
      strip.text = element_text(size = 5),
      text = element_text(size = 7),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      axis.text.x = ggplot2::element_text(angle = 90)
    )

  return(map)
}

#' Timely detection of AFP/ES samples
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Generates a violin plot highlighting the median detection time of samples.
#'
#' @param raw_data `list` Global polio data. Output of [get_all_polio_data()]
#' @param start_date `str` Analysis start date formatted as "YYYY-MM-DD".
#' @param end_date `str` Analysis end date formatted as "YYYY-MM-DD".
#' @param output_path `str` Where to output the figure to.
#' @param y_max `num` The maximum y-axis value.
#'
#' @return `ggplot` A violin plot showing timeliness of detection.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' generate_timely_det_violin(raw_data, "2021-01-01", "2023-12-31", getwd())
#' }
generate_timely_det_violin <- function(raw_data,
                                       start_date, end_date,
                                       output_path = Sys.getenv("KPI_FIGURES"),
                                       y_max = 250) {

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  pos <- generate_pos_timeliness(raw_data, start_date, end_date)
  ctry_abbrev <- get_ctry_abbrev(raw_data$afp)
  color.risk.cat <- c(
    "HIGH" = "#d73027",
    "MEDIUM" = "orange",
    "LOW (WATCHLIST)" = "#9ecae1"
  )

  exclude_low <- pos |>
    dplyr::filter(.data$`SG Priority Level` != "LOW") |>
    dplyr::left_join(ctry_abbrev,
                     by = c("place.admin.0", "whoregion")) |>
    dplyr::mutate(seq_lab = case_when(
      .data$seq.capacity == "no" ~ "No sequencing capacity",
      .data$seq.capacity == "yes" ~ "Sequencing capacity"
    )) |>
    dplyr::filter(!is.na(.data$seq_lab)) |>
    dplyr::mutate(
      sg_priority_level = factor(.data$`SG Priority Level`, levels = c(
        "LOW", "LOW (WATCHLIST)", "MEDIUM", "HIGH"))
    )


  plot <- generate_kpi_violin(exclude_low, "ctry.short", "ontonothq",
                              "sg_priority_level",
                              ggh4x::facet_nested(rolling_period~seq_lab+who.region,
                                                  scales = "free", space = "free",
                                           labeller = label_wrap_gen(13),
                                           switch = "y"),
                              80, y.max = y_max)
  plot <- plot +
    ggplot2::scale_fill_manual(values = color.risk.cat,
                               name = "Priority Level", na.value = "white")

  ggplot2::ggsave(file.path(output_path, "kpi_wild_vdpv_timely_det.png"),
                  dpi = 400, height = 5, width = 12, bg = "white")

  return(plot)

}

#' Timeliness of lab samples from collection to virus isolation results
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Shows the timeliness of lab culture results.
#'
#' @param lab_data `tibble` Global lab dataset.
#' @param afp_data `tibble` AFP dataset.
#' @param start_date `str` Start date of the analysis formatted as "YYYY-MM-DD".
#' @param end_date `str` End date of the analysis formatted as "YYYY-MM-DD".
#' @param output_path `str` Where to output the figure to.
#' @param y_max `num` Maximum value in the y-axis.
#'
#' @return `ggplot` A violin plot showing timeliness of lab culture.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' lab_data <- edav_io("read", file_loc = get_constant("CLEANED_LAB_DATA"))
#' generate_lab_culture_violin(lab_data, raw_data$afp,
#'                             "2021-01-01", "2023-12-31", getwd())
#' }
generate_lab_culture_violin <- function(lab_data, afp_data,
                                       start_date, end_date,
                                       output_path = Sys.getenv("KPI_FIGURES"),
                                       y_max = 60) {

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  ctry_abbrev <- get_ctry_abbrev(afp_data)
  color.risk.cat <- c(
    "HIGH" = "#d73027",
    "MEDIUM" = "orange",
    "LOW (WATCHLIST)" = "#9ecae1"
  )

  lab_data <- generate_kpi_lab_timeliness(lab_data, start_date, end_date,
                                          afp_data)
  lab_filtered <- lab_data |> dplyr::left_join(ctry_abbrev,
                                           by = c("country" = "place.admin.0",
                                                  "whoregion")) |>
    add_risk_category(ctry_col = "country") |>
    dplyr::mutate(
      year = lubridate::year(.data$DateStoolCollected)
    ) |>
    dplyr::filter(.data$`SG Priority Level` != "LOW" |
                    !is.na(.data$`SG Priority Level`),
                  !is.na(.data$culture.itd.cat),
                  dplyr::between(DateStoolCollected, start_date, end_date)) |>
    dplyr::mutate(
    sg_priority_level = factor(.data$`SG Priority Level`, levels = c(
      "LOW", "LOW (WATCHLIST)", "MEDIUM", "HIGH"))
    )

  plot <- generate_kpi_violin(lab_filtered, "ctry.short", "days.lab.culture",
                              "sg_priority_level",
                              ggh4x::facet_nested(year~whoregion + culture.itd.cat,
                                                  scales = "free", space = "free",
                                                  labeller = label_wrap_gen(13),
                                                  switch = "y"), 14,
                              y.max = y_max)
  plot <- plot +
    ggplot2::scale_fill_manual(values = color.risk.cat,
                               name = "Priority Level", na.value = "white")
  ggplot2::ggsave(file.path(output_path, "kpi_lab_days_lab_culture.png"),
                  dpi = 400, height = 5, width = 12, bg = "white")

  return(plot)
}

#' Timeliness of virus isolation to ITD results
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Shows the timeliness of ITD results of specimens that require ITD. The target
#' is 7 days or less.
#'
#' @inheritParams generate_lab_culture_violin
#'
#' @return `ggplot` A violin plot showing timeliness of ITD results from lab
#' culture.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' lab_data <- edav_io("read", file_loc = get_constant("CLEANED_LAB_DATA"))
#' generate_lab_itd_violin(lab_data, raw_data$afp,
#'                         "2021-01-01", "2023-12-31", getwd())
#' }
generate_lab_itd_violin <- function(lab_data, afp_data,
                                        start_date, end_date,
                                        output_path = Sys.getenv("KPI_FIGURES"),
                                        y_max = 60) {

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  ctry_abbrev <- get_ctry_abbrev(afp_data)
  color.risk.cat <- c(
    "HIGH" = "#d73027",
    "MEDIUM" = "orange",
    "LOW (WATCHLIST)" = "#9ecae1"
  )

  lab_data <- generate_kpi_lab_timeliness(lab_data, start_date, end_date,
                                          afp_data)
  lab_filtered <- lab_data |> dplyr::left_join(ctry_abbrev,
                                               by = c("country" = "place.admin.0",
                                                      "whoregion")) |>
    add_risk_category(ctry_col = "country") |>
    dplyr::mutate(
      year = lubridate::year(.data$DateStoolCollected)
    ) |>
    dplyr::filter(.data$`SG Priority Level` != "LOW" |
                    !is.na(.data$`SG Priority Level`),
                  !is.na(.data$culture.itd.cat),
                  dplyr::between(DateStoolCollected, start_date, end_date)) |>
    dplyr::mutate(
      sg_priority_level = factor(.data$`SG Priority Level`, levels = c(
        "LOW", "LOW (WATCHLIST)", "MEDIUM", "HIGH"))
    )

  plot <- generate_kpi_violin(lab_filtered, "ctry.short", "days.culture.itd",
                              "sg_priority_level",
                              ggplot2::facet_wrap(~ .data$culture.itd.lab,
                                                  scales = "free", ncol = 7,
                                         labeller = label_wrap_gen(20)), 7,
                              y.max = y_max)
  plot <- plot +
    ggplot2::scale_fill_manual(values = color.risk.cat,
                               name = "Priority Level", na.value = "white")
  ggplot2::ggsave(file.path(output_path, "kpi_lab_days_culture_itd.png"),
                  dpi = 400, height = 5, width = 12, bg = "white")

  return(plot)
}

#' Timeliness of shipment to sequencing results
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Shows the timeliness of shipment to sequencing results for specimens that
#' require sequencing. The target is 7 days or less.
#'
#' @inheritParams generate_lab_culture_violin
#'
#' @return `ggplot` A violin plot showing the timeliness of shipment to
#' sequencing results.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' lab_data <- edav_io("read", file_loc = get_constant("CLEANED_LAB_DATA"))
#' generate_lab_seqship_violin(lab_data, raw_data$afp,
#'                             "2021-01-01", "2023-12-31", getwd())
#' }
generate_lab_seqship_violin <- function(lab_data, afp_data,
                                        start_date, end_date,
                                        output_path = Sys.getenv("KPI_FIGURES"),
                                        y_max = 50) {

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  ctry_abbrev <- get_ctry_abbrev(afp_data)
  color.risk.cat <- c(
    "HIGH" = "#d73027",
    "MEDIUM" = "orange",
    "LOW (WATCHLIST)" = "#9ecae1"
  )

  lab_data <- generate_kpi_lab_timeliness(lab_data, start_date, end_date,
                                          afp_data)
  lab_filtered <- lab_data |> dplyr::left_join(ctry_abbrev,
                                               by = c("country" = "place.admin.0",
                                                      "whoregion")) |>
    add_risk_category(ctry_col = "country") |>
    dplyr::mutate(
      year = lubridate::year(.data$DateStoolCollected)
    ) |>
    dplyr::filter(.data$`SG Priority Level` != "LOW" |
                    !is.na(.data$`SG Priority Level`),
                  !is.na(.data$culture.itd.cat),
                  dplyr::between(DateStoolCollected, start_date, end_date)) |>
    dplyr::mutate(
      sg_priority_level = factor(.data$`SG Priority Level`, levels = c(
        "LOW", "LOW (WATCHLIST)", "MEDIUM", "HIGH"))
    )

  plot <- generate_kpi_violin(lab_filtered, "ctry.short", "days.seq.ship",
                              "sg_priority_level",
                              ggh4x::facet_nested(year ~ seq.cat + seq.lab,
                                                  scales = "free", space = "free",
                                                  labeller = label_wrap_gen(13),
                                                  switch = "y"), 7,
                              y.max = y_max)
  plot <- plot +
    ggplot2::scale_fill_manual(values = color.risk.cat,
                               name = "Priority Level", na.value = "white")
  ggplot2::ggsave(file.path(output_path, "kpi_lab_days_seq_ship.png"),
                  dpi = 400, height = 5, width = 12, bg = "white")

  return(plot)
}

#' Timeliness of arrival in sequencing lab to sequencing results
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Shows the timeliness of arrival in the sequencing lab to sequencing results.
#' The target is 7 days or less for AFP cases and 14 days for ES samples.
#'
#' @inheritParams generate_lab_culture_violin
#'
#' @return `ggplot` A violin plot showing the timeliness of sequencing results.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' lab_data <- edav_io("read", file_loc = get_constant("CLEANED_LAB_DATA"))
#' generate_lab_seqres_violin(lab_data, raw_data$afp,
#'                            "2021-01-01", "2023-12-31", getwd())
#' }
generate_lab_seqres_violin <- function(lab_data, afp_data,
                                        start_date, end_date,
                                        output_path = Sys.getenv("KPI_FIGURES"),
                                        y_max = 60) {

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  ctry_abbrev <- get_ctry_abbrev(afp_data)
  color.risk.cat <- c(
    "HIGH" = "#d73027",
    "MEDIUM" = "orange",
    "LOW (WATCHLIST)" = "#9ecae1"
  )

  lab_data <- generate_kpi_lab_timeliness(lab_data, start_date, end_date,
                                          afp_data)
  lab_filtered <- lab_data |> dplyr::left_join(ctry_abbrev,
                                               by = c("country" = "place.admin.0",
                                                      "whoregion")) |>
    add_risk_category(ctry_col = "country") |>
    dplyr::mutate(
      year = lubridate::year(.data$DateStoolCollected)
    ) |>
    dplyr::filter(.data$`SG Priority Level` != "LOW" |
                    !is.na(.data$`SG Priority Level`),
                  !is.na(.data$culture.itd.cat),
                  dplyr::between(DateStoolCollected, start_date, end_date)) |>
    dplyr::mutate(
      sg_priority_level = factor(.data$`SG Priority Level`, levels = c(
        "LOW", "LOW (WATCHLIST)", "MEDIUM", "HIGH"))
    )

  plot <- generate_kpi_violin(lab_filtered, "ctry.short", "days.itd.seqres",
                              "sg_priority_level",
                              ggh4x::facet_nested(year ~ seq.cat + seq.lab,
                                                  scales = "free", space = "free",
                                                  labeller = label_wrap_gen(13),
                                                  switch = "y"), 7,
                              y.max = y_max)
  plot <- plot +
    ggplot2::scale_fill_manual(values = color.risk.cat,
                               name = "Priority Level", na.value = "white")
  ggplot2::ggsave(file.path(output_path, "kpi_lab_days_itd_seqres.png"),
                  dpi = 400, height = 5, width = 12, bg = "white")

  return(plot)
}

# Line plots ----

#' Generate tile plots for indicators
#' @description
#' Generates tile plots for indicators present in c1-c4, showing changes over
#' multiple rolling periods.
#'
#' @details
#' The function automatically detects the geographic scale to present in each
#' plot. If passing a table that is grouped lower than the country level, we recommend
#' that only one country is present or a subset of districts are presented so that
#' the output plot is legible.
#'
#'
#' @param c_table `tibble` Either C1, C2, C3, C4
#'
#' @return `ggplot2` A tile plot for each indicator for each geography
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' c1 <- generate_c1_table(raw_data, "2021-01-01", "2023-12-31")
#' generate_kpi_tile(c1)
#' }
generate_kpi_tile <- function(c_table, priority_category = "HIGH",
                              output_path = Sys.getenv("KPI_FIGURES")) {
  col_names <- names(c_table)
  geo <- dplyr::case_when("site.name" %in% col_names == TRUE ~ "site.name",
    "culture.itd.lab" %in% col_names == TRUE ~ "culture.itd.lab",
    "seq.lab" %in% col_names == TRUE ~ "seq.lab",
    "dist" %in% col_names == TRUE ~ "dist",
    "prov" %in% col_names == TRUE ~ "prov",
    "ctry" %in% col_names == TRUE ~ "ctry",
    .default = "rolling_period"
  )

  table_name <- dplyr::case_when(
    "npafp_denom" %in% col_names == TRUE ~ "c1_rollup",
    "prop_timely_wild_vdpv" %in% col_names == TRUE ~ "c1",
    "prop_complete_60_day" %in% col_names == TRUE ~ "c2",
    "site.name" %in% col_names == TRUE ~ "c3",
    "prop_met_ev_5_samples" %in% col_names == TRUE ~ "c3_rollup",
    "prop_timely_isolation" %in% col_names == TRUE ~ "c4_itd_results",
    "prop_timely_seqres" %in% col_names == TRUE ~ "c4_seq_results"
  )

  if ("ctry" %in% col_names) {
    # get rolling periods
    rolling_periods <- c_table |>
      dplyr::select(rolling_period) |>
      dplyr::distinct()

    # combine rolling periods if not already present
    c_ctry <- dplyr::cross_join(
      c_table |>
        dplyr::select(
          rolling_period, year_label,
          analysis_year_start, analysis_year_end
        ) |>
        dplyr::distinct(),
      c_table |>
        dplyr::select(dplyr::any_of(c(
          "ctry", "Region",
          "SG Priority Level",
          "sg_priority_level"
        ))) |>
        dplyr::distinct()
    )
    c_table <- dplyr::left_join(c_ctry, c_table)
  }

  c_sub <- c_table %>%
    {
      if ("SG Priority Level" %in% col_names) {
        dplyr::filter(., `SG Priority Level` == priority_category)
      } else if ("sg_priority_level" %in% col_names) {
        dplyr::filter(., sg_priority_level == priority_category)
      } else {
        .
      }
    } |>
    dplyr::select(
      dplyr::any_of(c(
        "rolling_period", geo,
        "npafp_rate", "per.stool.ad", "ev_rate"
      )),
      dplyr::starts_with("prop"),
      dplyr::starts_with("timely"),
      # dplyr::starts_with("median"), need to figure out the best way
      -dplyr::ends_with("label")
    ) |>
    dplyr::rename_with(recode,
      # c1
      prop_met_npafp = "Non-polio AFP rate – subnational, %",
      prop_met_stool = "Stool adequacy – subnational, %",
      prop_met_ev = "ES EV detection rate – national, %",
      prop_timely_wild_vdpv = "Timeliness of detection for WPV/VDPV, %",
      prop_met_timely_wild_vdpv = "Timeliness of detection for WPV/VDPV, %",
      # c2
      npafp_rate = "Non-polio AFP rate",
      per.stool.ad = "Stool adequacy, %",
      prop_good_condition = "Stool condition, %",
      prop_complete_60_day = "Completeness of 60-day follow-ups, %",
      timely_not = "Timeliness of notification, %",
      median_timely_not = "Median timeliness of notification",
      timely_inv = "Timeliness of investigation, %",
      median_timely_inv = "Median timeliness of investigation",
      timely_field = "Timeliness of field activities, %",
      median_timely_field = "Median timeliness of field activities",
      timely_stool_shipment = "Timeliness of stool specimen shipment, %",
      median_stool_shipment = "Median timeliness of stool shipment",
      timely_opt_field_shipment = "Timeliness of optimized field and shipment, %",
      median_onto_lab = "Median timeliness of optimized field and shipment",
      timely_wpv_vdpv = "Timeliness of detection for WPV/VDPV – AFP, %",
      median_ontonothq = "Median timeliness of detection for WPV/VDPV",
      # c3
      prop_met_ev = "ES EV detection rate, % ",
      prop_met_ev_5_samples = "ES EV detection rate, % (>= 5 samples)",
      prop_met_good_samples = "Condition of ES sample, %",
      median_timely_shipment_per_site = "Median Timeliness of ES sample, %",
      prop_met_timely_wpv_vdpv_det = "Timeliness of detection for WPV/VDPV – ES, %",
      # c4 - itd
      prop_timely_isolation = "Timeliness of virus isolation results",
      prop_timely_itd = "Timeliness of ITD results",
      prop_timely_seqship = "Timeliness of shipment for sequencing",
      # c4 - seq
      prop_timely_seqres = "Timeliness of shipment for sequencing"
    ) |>
    dplyr::select(-dplyr::any_of(c("timely_isolation", "timely_seqship",
                                   "timely_itd", "timely_seqres"))) |>
    tidyr::pivot_longer(
      names_to = "indicator", values_to = "value",
      cols = -dplyr::any_of(c("rolling_period", geo))
    ) |>
    dplyr::mutate(
      category = dplyr::case_when(
        value >= 80 ~ "\u226580%",
        value >= 50 ~ "\u226550%",
        value < 50 ~ "<50%",
        .default = "No Data"
      ),
      geo = stringr::str_to_title(!!rlang::sym(geo)),
      indicator = stringr::str_wrap(indicator, width = 20)
    )

  if (geo == "rolling_period") {
    tile_plot <-
      ggplot2::ggplot(c_sub, aes(x = factor(rolling_period), y = 1, fill = category)) +
      ggplot2::geom_tile(lwd = 1, linetype = 1, color = "white") +
      ggplot2::theme_minimal() +
      ggh4x::facet_grid2(
        cols = ggplot2::vars(indicator),
        scales = "free_y", independent = "y", switch = "y"
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "\u226580%" = "#0070c0",
          "\u226550%" = "#F99244",
          "<50%" = "#FF4041",
          "No Data" = "lightgrey"
        ),
        name = "", na.value = "lightgrey"
      ) +
      ggplot2::theme(
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 4.5, face = "bold", color = "grey"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.y.left = element_text(angle = 0, hjust = 1)
      )
  } else {
    tile_plot <-
      ggplot2::ggplot(c_sub, aes(x = factor(rolling_period), y = geo, fill = category)) +
      ggplot2::geom_tile(lwd = 1, linetype = 1, color = "white") +
      ggplot2::theme_minimal() +
      ggh4x::facet_grid2(
        rows = vars(geo), cols = ggplot2::vars(indicator),
        scales = "free_y", independent = "y", switch = "y"
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "\u226580%" = "#0070c0",
          "\u226550%" = "#F99244",
          "<50%" = "#FF4041",
          "No Data" = "lightgrey"
        ),
        name = "", na.value = "lightgrey"
      ) +
      ggplot2::theme(
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 4.5, face = "bold", color = "grey"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.y.left = element_text(angle = 0, hjust = 1)
      )
  }

  if (is.null(output_path)) {
    return(tile_plot)
  } else if (!is.null(output_path) & table_name != "c1_rollup") {
    ggplot2::ggsave(file.path(output_path, paste0("kpi_tileplot_", table_name, ".jpg")),
      dpi = 400, height = 7, width = 12, bg = "white"
    )
  } else {
    ggplot2::ggsave(file.path(output_path, paste0("kpi_tileplot_", table_name, ".jpg")),
      dpi = 400, height = 2, width = 12, bg = "white"
    )
  }

  return(tile_plot)
}
