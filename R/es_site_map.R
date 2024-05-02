
library(tidyverse)
library(sf)
library(osmdata)
library(readxl)
library(cli)
library(ggpubr)

data <- sirfunctions::get_all_polio_data()

es_data <- readxl::read_excel("data/ES_Sites_Masterlist_Final_08March2024.xlsx") |>
  dplyr::select(COUNTRY, PROVINCE, DISTRICT = DISTRICT_ADM02_NAME, SITE_NAME, SITE_CODE,
         lat = Lat_Y, lon = Long_X)

dist <- data$global.dist
es <- data$es


#' Create ES Site Map
#'
#' @description
#' Automate the creation of an ES site map using OSM data
#' @param site_code str: string of site code
#' @param es_data tibble: es site data with geo coordinates for a given site
#' @param bbox_m int: size in meters
#' @param dist spatial sf file of all global districts
#' @param es ES data from POLIS
#' @param out_file str: location where image should be written
#' @returns ggplot map object with visualization
create_es_site_map <- function(
    site_code,
    es_data,
    bbox_m = 3000,
    dist,
    es,
    out_file = NULL
){

  cli::cli_process_start("Starting site processing")
  site_data <- es_data |>
    dplyr::filter(
      SITE_CODE == site_code
    )

  sp.local_sites <- es_data |>
    dplyr::filter(COUNTRY == site_data$COUNTRY,
           PROVINCE == site_data$PROVINCE,
           DISTRICT == site_data$DISTRICT) |>
    sf::st_as_sf(coords = c(x = "lon", y = "lat"), crs = 4326) |>
    dplyr::mutate(type = ifelse(SITE_CODE == site_data$SITE_CODE, "Primary", "Nearby"),
           SITE_NAME = stringr::str_to_title(SITE_NAME))

  site_name <- site_data |> dplyr::pull(SITE_NAME)

  cli::cli_process_done()

  cli::cli_process_start("Processing ES Data")

  multi.site.data <- es |>
    dplyr::filter(site.code == site_data$SITE_CODE) |>
    dplyr::filter(year(collect.date) >= (lubridate::year(Sys.Date())-2)) |>
    dplyr::pull(admin.2.id) |>
    unique() %>%
    {data$es |> dplyr::filter(admin.2.id == .)} |>
    dplyr::select(site, collect.date, ev.detect, sabin, vdpv, wpv, nvaccine) |>
    tidyr::pivot_longer(ev.detect:nvaccine) |>
    dplyr::mutate(name = factor(name, levels = c("ev.detect", "sabin", "vdpv", "wpv", "nvaccine"),
                         labels = c("Sample", "Sabin", "VDPV", "WPV", "Novel"))) |>
    tidyr::drop_na()

  table1 <- es |>
    dplyr::filter(site.code == site_data$SITE_CODE) |>
    dplyr::filter(collect.yr >= (year(Sys.Date())-2)) |>
    dplyr::pull(admin.2.id) |>
    unique() %>%
    {data$es |> dplyr::filter(admin.2.id == .)} |>
    dplyr::select(site, collect.date, ev.detect, sabin, vdpv, wpv, nvaccine) |>
    dplyr::mutate(year = lubridate::year(collect.date)) |>
    dplyr::filter(year >= (year(Sys.Date())-2)) |>
    dplyr::group_by(site, year) |>
    dplyr::summarise(count = dplyr::n(),
              ev.det.rate = paste0(round(mean(ev.detect)*100,0), "%"),
              sabin = sum(sabin, na.rm = T),
              vdpv = sum(vdpv, na.rm = T),
              wpv = sum(wpv, na.rm = T),
              nvaccine = sum(nvaccine, na.rm = T), .groups = "drop") |>
    dplyr::rowwise() |>
    dplyr::mutate(site = paste0(strwrap(site, width = 30), collapse = "\n")) |>
    dplyr::select("Site" = site, "Year" = year, "# Samples" = count, "EV Det. Rate" = ev.det.rate,
           "Sabin" = sabin, "VDPV" = vdpv, "WPV" = wpv, "Novel" = nvaccine) |>
    #ggtexttable(theme = ttheme("classic"), rows = NULL) |>
    flextable::flextable() |>
    flextable::merge_v(j = "Site") |>
    flextable::colformat_num(j = "Year", big.mark = "") |>
    flextable::hline(j = "Site", i = 1:8) |>
    flextable::gen_grob()


  fig1 <- ggplot2::ggplot() +
    ggplot2::geom_point(data = multi.site.data |> dplyr::filter(name == "Sample"), ggplot2::aes(x = collect.date, y = site, color = name), size = 0.8, alpha = 0.4, show.legend = T) +
    ggplot2::geom_point(data = multi.site.data |> dplyr::filter(name != "Sample"), ggplot2::aes(x = collect.date, y = site, color = name), size = 1.5, show.legend = T) +
    ggplot2::scale_color_manual(values = c("gray", RColorBrewer::brewer.pal(4, "Dark2") ), drop = F) +
    ggplot2::theme_bw() +
    ggplot2::labs(color = "Type", x = "Date", y = "Site")

  cli::cli_process_done()

  sp.site_data <- site_data |>
    sf::st_as_sf(coords = c(x = "lon", y = "lat"), crs = 4326)

  cli::cli_process_start("Creating BBOX")
  x <- dplyr::pull(site_data, lon)
  y <- dplyr::pull(site_data, lat)

  bbox <- c()

  bbox["xmin"] <- x - (bbox_m / 6370000) * (180 / pi) / cos(x * pi / 180)
  bbox["xmax"] <- x + (bbox_m / 6370000) * (180 / pi) / cos(x * pi / 180)
  bbox["ymin"] <- y - (bbox_m / 6370000) * (180 / pi)
  bbox["ymax"] <- y + (bbox_m / 6370000) * (180 / pi)

  bbox <- matrix(bbox, nrow = 2, byrow = T)

  colnames(bbox) <- c("min", "max")
  rownames(bbox) <- c("x", "y")

  cli::cli_process_done()

  cli::cli_process_start("Pulling base shapefile")
  area.shape <- dist |>
    dplyr::filter(ADM0_NAME == stringr::str_replace_all(site_data$COUNTRY, "'", " "))
  cli::cli_process_done()

  cli::cli_process_start("Pulling major roads from OSM")
  area.major <- osmdata::opq(bbox, timeout = 50) |>
    osmdata::add_osm_feature(key = "highway",
                    value = c("motorway", "primary", "secondary")) |>
    osmdata::osmdata_sf()
  cli::cli_process_done()


  cli::cli_process_start("Pulling minor roads from OSM")
  area.minor <- osmdata::opq(bbox, timeout = 50) |>
    osmdata::add_osm_feature(key = "highway", value = c("tertiary", "residential")) |>
    osmdata::osmdata_sf()
  cli::cli_process_done()

  cli::cli_process_start("Pulling blue lines from OSM")
  area.blue <- osmdata::opq(bbox, timeout = 50) |>
    osmdata::add_osm_feature(key = "water") |>
    osmdata::osmdata_sf()
  cli::cli_process_done()

  cli::cli_process_start("Pulling ocean layer")
  area.ocean <- osmdata::opq(bbox, timeout = 50) |>
    osmdata::add_osm_feature(key = "water") |>
    osmdata::osmdata_sf()
  cli::cli_process_done()

  cli::cli_process_start("Pulling cities from OSM")
  area.cities <- osmdata::opq(bbox, timeout = 50) |>
    osmdata::add_osm_feature(key = "place", value = c("city", "city_block", "locality",
                                             "municipality", "neighborhood", "town",
                                             "village", "suburb")) |>
    osmdata::osmdata_sf()


  cities <- area.cities$osm_points |>
    dplyr::mutate(location = factor(place, levels = c("city", "city_block", "town", "locality",
                                               "municipality", "neighborhood",
                                               "village", "suburb"))) #|>
    #filter(as.numeric(location) == min(as.numeric(location)))
  cli::cli_process_done()

  base.map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = area.shape, fill = "#fff7bc") +
    ggplot2::geom_sf(data = area.blue$osm_polygons,
            inherit.aes = TRUE,
            color = "royalblue4",
            fill = "royalblue4",
            size = 0.1) +
    ggplot2::geom_sf(data = area.blue$osm_multipolygons,
            inherit.aes = TRUE,
            color = "royalblue4",
            fill = "royalblue4",
            size = 0.1) +
    ggplot2::geom_sf(data = area.ocean$osm_polygons,
            inherit.aes = TRUE,
            color = "royalblue4",
            fill = "royalblue4",
            size = 0.1) +
    ggplot2::geom_sf(data = area.major$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = 0.2) +
    ggplot2::geom_sf(data = area.minor$osm_lines,
            inherit.aes = FALSE,
            color = "#666666",
            size = 0.1) +
    ggplot2::geom_sf(data = sp.local_sites, mapping = ggplot2::aes(fill = SITE_NAME), size = 3, shape = 23) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::coord_sf(xlim = bbox["x",], ylim = bbox["y",]) +
    ggplot2::theme_void() +
    ggplot2::labs(fill = "Site Name") +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "royalblue4"),
          legend.position = "bottom"
          )

  if(nrow(area.cities$osm_points) > 0){
    base.map <- base.map +
      ggplot2::geom_sf_label(data = cities, aes(label = name), size = 3)
  }

  cli::cli_process_start("Writing image")
  top <- ggpubr::ggarrange(base.map, table1, ncol = 2, widths = c(1, 0.8))

  out_fig <- ggpubr::ggarrange(top, fig1, ncol = 1, heights = c(1,0.5), align = "v")

  out_fig <- ggpubr::annotate_figure(out_fig, top = paste0("ES Site - ", site_name, " - (", site_data$COUNTRY,"/",site_data$PROVINCE,"/",site_data$DISTRICT, ")"))

  ggsave(filename = out_file, plot = out_fig, dpi = 300, height = 12, width = 18, bg = "white")

  cli::cli_process_done()
}

create_es_site_map(
  site_code = "ENV-SIL-WEA-WAU-CTB",
  es_data = es_data,
  bbox_m = 3000,
  dist = dist,
  es = es,
  out_file = "C:/Users/ynm2/Desktop/es_fig.png"
)

create_es_site_map(
  site_code = "ENV-SIL-WEA-WAU-KCS",
  es_data = es_data,
  bbox_m = 3000,
  dist = dist,
  es = es,
  out_file = "C:/Users/ynm2/Desktop/es_fig2.png"
)

create_es_site_map(
  site_code = "ENV-CIV-AB2-APA-IND",
  es_data = es_data,
  bbox_m = 3000,
  dist = dist,
  es = es,
  out_file = "C:/Users/ynm2/Desktop/es_fig3.png"
)

