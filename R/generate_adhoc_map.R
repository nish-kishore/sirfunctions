# Private Functions ----
# NOTE: These functions are not meant to be used outside of generate_adhoc_map()

#' Adjust the zoom level of the map
#'
#' This is an internal function changes the zoom level of the map depending on the
#' user specification.
#'
#' @param g1 `ggplot` Original ggplot map.
#' @param map_ref `ggplot` Reference map.
#' @param m_base_ctry `sf` Shapefile containing base map of country.
#' @param country `str` Country or countries to zoom into.
#' @keywords internal
#'
#' @returns `ggplot` Map with adjusted zoom level, if specified.
#'
set_zoom_level <- function(g1, map_ref, country, m_base_ctry) {
  # Set up map zoom level
  map_lvl <- map_ref |> dplyr::filter(place.admin.0 %in% country)
  z_lvl <- map_lvl$ref_lvl

  cli::cli_alert_info(paste0("Suggested zoom level: ", round(mean(z_lvl), 1)))

  # Find best center coordinate in a country or countries
  zoom_to <- c(mean(m_base_ctry$CENTER_LON), mean(m_base_ctry$CENTER_LAT))
  zoom_level <- mean(z_lvl) # 1-19
  lon_span <- mean(360 / 2^zoom_level)
  lat_span <- mean(180 / 2^zoom_level)
  lon_bounds <- c(zoom_to[1] - lon_span / 2, zoom_to[1] + lon_span / 2)
  lat_bounds <- c(zoom_to[2] - lat_span / 2, zoom_to[2] + lat_span / 2)

  g1a <- g1 + ggplot2::coord_sf(xlim = lon_bounds, ylim = lat_bounds, expand = FALSE)

  print(g1a)

  cli::cli_alert_info("Map may need to be adjusted")

  wait <- T
  while (wait) {
    map_c <- readline(prompt = "Update zoom level of map? (yes/no) ")
    map_c <- stringr::str_trim(stringr::str_to_lower(map_c))

    if (!map_c %in% c("yes", "no")) {
      cli::cli_alert_warning("Invalid response. please try again.")
      next
    }

    if (map_c == "no") {
      wait <- F
      next
    } else {
      z_lvl <- readline(prompt = "Zoom Level: 1-19: ")
      z_lvl <- eval(parse(text = z_lvl))

      zoom_to <- c(mean(m_base_ctry$CENTER_LON), mean(m_base_ctry$CENTER_LAT)) # Center of Country
      zoom_level <- mean(z_lvl) # 1-19
      lon_span <- mean(360 / 2^zoom_level)
      lat_span <- mean(180 / 2^zoom_level)
      lon_bounds <- c(zoom_to[1] - lon_span / 2, zoom_to[1] + lon_span / 2)
      lat_bounds <- c(zoom_to[2] - lat_span / 2, zoom_to[2] + lat_span / 2)

      g1a <- g1 +
        ggplot2::coord_sf(xlim = lon_bounds, ylim = lat_bounds, expand = FALSE)

      print(g1a)
    }
  }

  return(g1a)
}


#' Build detection map
#'
#' This is the key function that builds the detection map. This is internally
#' referred to as `g1` within [generate_adhoc_map()].
#'
#' @param m_base_region `sf` Map base for region.
#' @param m_base_prov  `sf` Map base for provinces.
#' @param data_p `tibble` Data for map creation.
#' @param m_data_prov `tibble` Map data for provinces.
#' @param new_detect `bool` Whether to include new detections.
#' @param virus_type `str` or `list` Virus type to display.
#' @param surv_options `str` or `list` Surveillance options.
#' @param start_date `date` Start date.
#' @param date_3a `date` ???
#' @param download_date `date` Date global polio data was downloaded.
#' @param emg_cols `list` Emergence colors.
#' @param country `str` Country or countries of interest.
#' @param labels `bool` Whether to label provinces.
#' @param clean_maps `ggplot` Clean maps.
#' @param data_r `tibble` Reported detections.
#' @param .owner `str` Entity that produced the map.
#'
#' @keywords internal
#' @returns `ggplot` Map of recent detections.

build_detection_map <- function(m_base_region, m_base_prov, data_p, m_data_prov,
                                new_detect, virus_type, surv_options, start_date, date_3a,
                                download_date, emg_cols, country, labels, clean_maps, data_r,
                                .owner) {

    if (!requireNamespace("ggspatial", quietly = TRUE)) {
    stop(
      'Package "ggspatial" must be installed to use this function.',
      call. = FALSE
    )
  }

  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop(
      'Package "ggrepel" must be installed to use this function.',
      call. = FALSE
    )
  }

  g1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = m_base_region, fill = "grey80", color = "black", lwd = 0.6, show.legend = FALSE) +
    ggplot2::geom_sf(data = m_base_prov, aes(fill = detect_status2), color = "grey", lwd = 0.5, show.legend = TRUE) +
    ggplot2::geom_sf(data = m_base_region, alpha = 0, color = "black", lwd = 0.6, show.legend = FALSE) +
    ggplot2::scale_fill_manual(
      name = "Last detection \nin region:",
      values = c(
        ">13months" = "cornsilk",
        "6-13months" = "wheat2",
        "<6months" = "burlywood3"
      ),
      labels = c(
        ">13 months",
        "6-13 months",
        "<6 months"
      ), drop = F
    ) +
    ggplot2::geom_point(
      data = data_p, ggplot2::aes(
        x = as.numeric(longitude),
        y = as.numeric(latitude),
        color = emg_grp2,
        shape = detect_status
      ),
      size = 3
    ) +
    {
      if (labels %in% c("ALL", "YES")) {
        ggrepel::geom_label_repel(
          data = m_data_prov,
          ggplot2::aes(
            label = ADM1_NAME,
            geometry = SHAPE
          ),
          stat = "sf_coordinates",
          size = 2,
          label.padding = ggplot2::unit(0.1, "lines"),
          fontface = "bold",
          fill = ggplot2::alpha(c("white"), 0.6),
          nudge_x = -0.2, nudge_y = -.5
        )
      }
    } +
    ggplot2::geom_sf_text(
      data = m_base_region |>
        dplyr::filter(!(ADM0_NAME %in% country | WHO_CODE %in% clean_maps)),
      ggplot2::aes(label = WHO_CODE),
      size = 2, fontface = "bold"
    ) +
    {
      if (new_detect) {
        ggplot2::geom_point(
          data = data_r, ggplot2::aes(
            x = as.numeric(longitude),
            y = as.numeric(latitude),
            shape = measurement
          ),
          size = 4,
          stroke = 1.5
        )
      }
    } +
    ggplot2::scale_shape_manual(
      name = "Time since \n detection:",
      values = c(
        "<6months" = 16,
        "6-13months" = 1,
        "Reported detection" = 5
      ),
      labels = c(
        "6-13 months",
        "<6 months",
        "Reported detection"
      ), drop = F
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "lightblue"),
      legend.key = ggplot2::element_blank(),
      legend.title.position = "top",
      plot.caption = ggplot2::element_text(size = 8)
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(order = 3, nrow = 3),
      fill = ggplot2::guide_legend(order = 1, ncol = 1, reverse = TRUE),
      shape = ggplot2::guide_legend(
        order = 2, ncol = 1, reverse = TRUE,
        override.aes = list(size = 2), stroke = 1
      )
    ) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.15) +
    {
      if (new_detect) {
        ggplot2::labs(
          caption = paste(paste(virus_type, collapse = ", "), " poliovirus outbreak include: ", surv_options, " dectections since ", start_date, ".\n ",
            "Recent detections highlighted include all reported by WHO HQ since ", date_3a, ".\n ",
            "Notes: Dates are based on paralysis onset (cases) or sample collections (environmental or other ) dates.\n AFP cases are randomly assigned in each district.\n",
            "Produced by ", .owner, ". Source: POLIS. Data as of ", download_date, ".",
            sep = ""
          )
        )
      }
    } +
    {
      if (!new_detect) {
        ggplot2::labs(
          caption = paste(paste(virus_type, collapse = ", "), " poliovirus outbreak include: ", surv_options, " dectections since ", start_date, ".\n ",
            "Notes: Dates are based on paralysis onset (cases) or sample collections (environmental or other ) dates.\n AFP cases are randomly assigned in each district.\n",
            "Produced by ", .owner, ". Source: POLIS. Data as of ", download_date, ".",
            sep = ""
          )
        )
      }
    } +
    if (any(country %in% c("AFGHANISTAN", "PAKISTAN"))) {
      ggplot2::scale_color_manual(
        name = "Cluster:",
        values = emg_cols
      )
    } else {
      ggplot2::scale_color_manual(
        name = "Emergence group:",
        values = emg_cols
      )
    }
}

#' Identifying new detections
#'
#' The function adds new detections as specified by the user when they use
#' [generate_adhoc_map()]. This function inherits arguments from the parent function.
#'
#' @param data_p `tibble` Map data.
#' @param country `str` or `list` Country or a list of countries.
#' @param date_3a_a `str` ???
#' @param date_3a `str` ???
#' @param date_3b `str` ???
#' @keywords internal
#'
#' @returns a dataset containing new detections
add_new_detection <- function(data_p, country, date_3a, date_3a_a, date_3b) {
  # initialize variable
  data_r <- NULL

  # create a boolean mask
  condition <- data_p$place.admin.0 %in% c("AFGHANISTAN", "PAKISTAN")

  # filter and mutate at once
  data_r <- data_p |>
    dplyr::filter(
      place.admin.0 %in% country,
      dplyr::between(report_date, dplyr::if_else(condition, date_3a_a, date_3a), date_3b)
    ) |>
    dplyr::mutate(measurement = "Reported detection")

  return(data_r)
}


#' Identify last detections at the province level
#'
#' @param data_p dataset containing epids with cases
#' @param .start_date `date` Start date.
#' @param .end_date `date` End date.
#' @keywords internal
#'
#' @returns `tibble` Summary table identifying last detections at the province level
last_detections_prov <- function(data_p, .start_date, .end_date) {
  data_prov <- data_p |>
    dplyr::arrange(place.admin.1, dateonset) |>
    dplyr::group_by(place.admin.1) |>
    dplyr::summarise(
      last_prov_detect = dplyr::last(dateonset)
    ) |>
    dplyr::mutate(
      detect_status = dplyr::case_when(
        last_prov_detect > (.end_date %m-% months(6)) ~ "<6months",
        (last_prov_detect <= (.end_date %m-% months(6)) &
          last_prov_detect >= .start_date) ~ "6-13months"
      ),
      detect_overall = ifelse(!is.na(detect_status), "Y", "N"),
      detect_status = factor(detect_status, levels = c("6-13months", "<6months"))
    )

  return(data_prov)
}


#' Pull the main map data to build the map
#'
#' This function primarily pulls the relevant information to build the map. Inherits
#' areguments from [generate_adhoc_map()].
#'
#' @param raw.data `list` global polio data output from [get_all_polio_data()]
#' @param .vdpv `bool` Whether to include VDPV in maps.
#' @param country `str` or `list` Name of the country or countries.
#' @param virus_type `str` or `list` Virus type to map.
#' @param .start_date `date` Start date.
#' @param .end_date `date` End date.
#' @param surv `str` or `list` Surveillance type.
#' @keywords internal
#'
#' @returns `tibble` Filtered dataset used to build the map.
pull_map_data <- function(raw.data, .vdpv, country, surv, virus_type, .start_date, .end_date) {
  if (.vdpv == "YES") {
    virus_type_modified <- dplyr::case_when(virus_type == "cVDPV 1" ~ "vtype 1",
      virus_type == "cVDPV 2" ~ "vtype 2",
      virus_type == "cVDPV 3" ~ "vtype 3",
      virus_type == "WILD 1" ~ "wtype 1",
      .default = as.character(virus_type)
    )

    data_p <- raw.data$pos |>
      dplyr::mutate(
        vtype_mod = dplyr::case_when(
          measurement %in% c("cVDPV 1", "VDPV 1") ~ "vtype 1",
          measurement %in% c("cVDPV 2", "VDPV 2") ~ "vtype 2",
          measurement %in% c("cVDPV 3", "VDPV 3") ~ "vtype 3",
          measurement %in% c("WILD 1") ~ "wtype 1"
        ),
        emg_grp2 = dplyr::case_when(
          measurement %in% c("cVDPV 1", "cVDPV 2", "cVDPV 3") ~ emergencegroup,
          measurement == "WILD 1" ~ viruscluster,
          measurement == "VDPV 1" ~ "VDPV 1",
          measurement == "VDPV 2" ~ "VDPV 2",
          measurement == "VDPV 3" ~ "VDPV 3"
        )
      ) |>
      dplyr::filter(
        place.admin.0 %in% country,
        dplyr::between(dateonset, .start_date, .end_date),
        vtype_mod %in% virus_type_modified
      )
  } else {
    data_p <- raw.data$pos |>
      dplyr::filter(
        place.admin.0 %in% country &
          dplyr::between(dateonset, .start_date, .end_date),
        measurement %in% virus_type
      ) |>
      dplyr::mutate(
        emg_grp2 = dplyr::case_when(
          measurement == "WILD 1" ~ viruscluster,
          TRUE ~ emergencegroup
        )
      )
  }

  data_p <- data_p |>
    dplyr::arrange(measurement, place.admin.0, place.admin.1, dateonset) |>
    dplyr::mutate(
      place.admin.1 = factor(place.admin.1,
        levels = rev(sort(unique(place.admin.1))),
        ordered = TRUE
      ),
      source_mod = dplyr::case_when(
        source == "AFP" ~ "AFP",
        source == "ENV" ~ "ES",
        source %in% c(
          "Community", "Contact",
          "Healthy", "Other"
        ) ~ "OTHER"
      ),
      recent_detect = factor(case_when(
        dateonset >= (today() %m-% months(3)) ~ "Y"
      )),
      source_mod = factor(source_mod, levels = c("AFP", "OTHER", "ES")),
      detect_status = dplyr::case_when(
        dateonset > (.end_date %m-% months(6)) ~ "<6months",
        (dateonset <= (.end_date %m-% months(6)) &
          dateonset >= .start_date) ~ "6-13months"
      ),
      detect_status = factor(detect_status, levels = c("6-13months", "<6months"))
    ) |>
    dplyr::filter(source_mod %in% surv) |>
    dplyr::distinct(epid, .keep_all = T)

  return(data_p)
}

#' Load the sharepoint environment
#'
#' @param raw.data global polio data output by sirfunctions::get_all_polio_data()
#' @keywords internal
#'
#' @returns sharepoint path
load_sharepoint_env <- function(raw.data) {

  if (!requireNamespace("Microsoft365R", quietly = TRUE)) {
    stop(
      'Package "Microsoft365R" must be installed to use this function.',
      call. = FALSE
    )
  }

  Microsoft365R::get_sharepoint_site(
    site_url = "https://cdc.sharepoint.com/teams/CGH-GID-PEB",
    tenant = Sys.getenv("CLIMICROSOFT365_TENANT", "common"),
    app = Sys.getenv("CLIMICROSOFT365_AADAPPID"),
    scopes = c(
      "Group.ReadWrite.All", "Directory.Read.All", "Sites.ReadWrite.All",
      "Sites.Manage.All"
    ),
    token = NULL
  )

  sp_path_a <- paste("./OBX/Outputs/adhoc_maps")
  sp_newfolder <- paste("adhoc_maps", as.character(raw.data$metadata$download_time), sep = "_")
  sp_newpath <- file.path(sp_path_a, sp_newfolder)

  cli::cli_alert("Sharepoint Validation - Good to go")
  return(sp_newpath)
}

# Public Functions ----
#' Set the emergence colors
#'
#' Used in conjunction to [generate_adhoc_map()]. The function returns a named
#' list with emergence names mapped to a color.
#'
#' @param raw.data `list` Global polio data output of [get_all_polio_data()].
#' @param country `str` or `list` Countries of interest.
#' @param start_date `str` Start date of the time span to look for emergences. Defaults to 13 months from the end date.
#' @param end_date  `str` End date of the time span to look for emergences Defaults to download date of `raw.data`.
#'
#' @returns `list` A named list containing the mapping of emergence and corresponding colors.
#' @examples
#' \dontrun{
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' emg.cols <- set_emergence_colors(raw.data, "algeria")
#' }
#'
#' @export
set_emergence_colors <- function(raw.data, country, start_date = NULL, end_date = NULL) {
  # Default start and end dates
  if (is.null(end_date)) {
    end_date <- lubridate::as_date(raw.data$metadata$download_time)
  } else {
    end_date <- lubridate::as_date(end_date)
  }

  if (is.null(start_date)) {
    start_date <- lubridate::floor_date((end_date %m-% months(13, F)), "month")
  } else {
    start_date <- lubridate::as_date(start_date)
  }

  country <- stringr::str_to_upper(country)

  # Check input validity
  if (length(setdiff(country, unique(raw.data$global.ctry$ADM0_NAME))) > 0) {
    diff <- setdiff(country, unique(raw.data$global.ctry$ADM0_NAME))
    cli::cli_alert_warning("Invalid country names:")
    for (i in diff) {
      print(i)
    }
    cli::cli_alert_warning("Please check and try again")
    return(NULL)
  }

  emg <- raw.data$pos |>
    dplyr::filter(place.admin.0 %in% country) |>
    dplyr::mutate(emg_grp2 = dplyr::case_when(
      measurement == "WILD 1" ~ viruscluster,
      TRUE ~ emergencegroup
    )) |>
    dplyr::filter(
      dateonset >= start_date,
      source %in% c("AFP", "ENV"),
      measurement %in% c("cVDPV 1", "cVDPV 2", "cVDPV 3", "WILD 1")
    ) |>
    dplyr::distinct(emg_grp2) |>
    dplyr::arrange(emg_grp2) |>
    tidyr::drop_na()

  emg_cols <- f.color.schemes("emergence.groups")

  unassigned_emergence <- setdiff(emg$emg_grp2, names(emg_cols))
  if (length(unassigned_emergence) == 0) {
    print("All active emergences accounted for")
  } else {
    cli::cli_alert_warning("The following emergence group has no colors: ")
    for (i in unassigned_emergence) {
      cli::cli_alert_info(i)
    }

    cli::cli_alert_info(paste0(
      "You can add additional mapping by appending to emg_cols directly.\n",
      "For example: emg_cols['", i, "']='#hexcolor'"
    ))
  }

  emg_cols <- subset(emg_cols, (names(emg_cols) %in% emg$emg_grp2))

  return(emg_cols)
}


#' Create adhoc maps for emergences
#'
#' Creates a map of recent emergences. The default will display outbreaks from the past 13 months.
#'
#' @param raw.data `list` Global polio data. The output of [get_all_polio_data()].
#' Make sure the spatial data is attached, otherwise, it will not work.
#' @param country `str` or `list` Country name or a list of country names.
#' @param start_date `str` Start date. If not specified, defaults to 13 months prior to the download date of raw.data.
#' @param end_date `str` End date. If not specified, defaults to the download date of raw.data.
#' @param vdpv `bool` Whether to include VPDV in maps. Default `TRUE`.
#' @param new_detect `bool` Whether to highlight new detections based on WHO HQ report date. Default `TRUE`.
#' @param output `str` Either a path to a local folder to save the map to, `"sharepoint"`, or `NULL`. Defaults to `NULL`.
#' @param virus_type `str` or `list`. Virus type to include. Valid values are:
#'
#' `"cVDPV 1", "cVDPV 2", "cVDPV 3", "WILD 1".`
#'
#' Can pass as a list.
#' @param surv `str` or `list` Surveillance options. Valid values are:
#'
#' `"AFP", "ES", "OTHER"`
#'
#' `"OTHER"` includes Case Contact, Community, Healthy Children Sampling. Can pass as a list.
#' @param labels `str` Include labels for regions with virus detections.
#' Options:
#' - `"ALL"`: All regions
#' - `"YES"`: Recent Detections - <13 months
#' @param owner `str` Who produced the map. Defaults to `"CDC-GID-PEB"`.
#' @param new_detect_expand `bool` Whether to expand the reporting window. Defaults to `FALSE`.
#' @param image_size `str` Standard sizes of the map outputs. Options are:
#' - `"full_slide"`
#' - `"soco_slide"`
#' - `"half_slide"`
#'
#' Defaults to `NULL`.
#' @param height `numeric` Height of the map. Defaults to `6.2`.
#' @param width `numeric` Width of the map. Defaults to `4.5`.
#' @param scale `numeric` Scale of the map. Defaults to `1.25`.
#' @param dpi `numeric` DPI of the map. Defaults to `300`.
#' @returns `ggplot` A map of outbreaks.
#' @examples
#' \dontrun{
#' raw.data <- get_all_polio_data()
#' p1 <- generate_adhoc_map(raw.data, c("nigeria", "chad"))
#' # Put colors in emergences that don't have a mapped color
#' emg_cols <- set_emergence_colors(raw.data, c("nigeria", "chad"))
#' emg_cols["NIE-BOS-1"] <- "yellow"
#' emg_cols["NIE-YBS-1"] <- "green"
#' p2 <- p1 + ggplot2::scale_color_manual(name = "Emergence Group", values = emg_cols)
#' }
#' @export
generate_adhoc_map <- function(raw.data, country, virus_type = "cVDPV 2",
                               vdpv = T, new_detect = T,
                               surv = c("AFP", "ES", "OTHER"), labels = "YES",
                               owner = "CDC-GID-PEB",
                               new_detect_expand = F,
                               start_date = NULL,
                               end_date = NULL,
                               output = NULL,
                               image_size = NULL,
                               height = 6.2,
                               width = 4.5,
                               scale = 1.25,
                               dpi = 300) {

  if (!requireNamespace("ggspatial", quietly = TRUE)) {
    stop('Package "ggspatial" must be installed to use this function.',
      .call = FALSE
    )
  }

  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop('Package "ggrepel" must be installed to use this function.',
      .call = FALSE
    )
  }

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop('Package "tibble" must be installed to use this function.',
      .call = FALSE
    )
  }

  # Standardize inputs
  country <- stringr::str_trim(stringr::str_to_upper(country))
  surv <- stringr::str_trim(stringr::str_to_upper(surv))
  if (is.null(output)) {
    output <- "none"
  } else {
    output <- stringr::str_trim(output)
  }
  labels <- stringr::str_to_upper(labels)

  # Check input validity
  if (length(setdiff(country, unique(raw.data$global.ctry$ADM0_NAME))) > 0) {
    diff <- setdiff(country, unique(raw.data$global.ctry$ADM0_NAME))
    cli::cli_alert_warning("Invalid country names:")
    for (i in diff) {
      print(i)
    }
    cli::cli_alert_warning("Please check and try again")
    return(NULL)
  }

  # if (length(virus_type) != 1) {
  #   cli::cli_alert_warning("Only the mapping of one virus type is supported at this time.")
  #   return(NULL)
  # }

  if (length(setdiff(virus_type, c("cVDPV 1", "cVDPV 2", "cVDPV 3", "WILD 1"))) > 0) {
    cli::cli_alert_warning("Not a valid argument for virus_type. Valid arguments are: cVDPV 1, cVDPV 2, cVDPV 3, WILD 1")
    return(NULL)
  }

  if (output != "none" & !stringr::str_detect(output, stringr::regex("^sharepoint$", ignore_case = TRUE))) {
    if (!dir.exists(output)) {
      cli::cli_alert_warning("The local folder does not exist. Please check the output argument and try again.")
      return(NULL)
    }
  }

  if (!is.null(image_size)) {
    if (length(setdiff(image_size, c("full_slide", "soco_slide", "half_slide"))) > 0) {
      cli::cli_alert_warning("Not a valid argument for image_size. Valid arguments are: full_slide, soco_slide, half_slide.")
      return(NULL)
    }
  }



  # Static variables
  download_date <- lubridate::as_date(raw.data$metadata$download_time)
  run_date <- lubridate::today()
  map_ref <- sirfunctions::edav_io(io = "read", file_loc = "Data/orpg/mapref.table.rds")
  surv_options <- paste(surv, collapse = ", ")
  clean_maps <- c("COM", "STP", "CAV", "SEY")

  # Initialize required variables
  date_3a <- NULL
  date_3a_a <- NULL
  date_3b <- NULL
  data_r <- NULL
  m_base_region <- NULL
  m_data_prov <- NULL
  sp_newpath <- NULL

  # Change images params if image_size is not null
  if (!is.null(image_size)) {
    dpi <- 300
    if (image_size == "full_slide") {
      height <- 7.5
      width <- 13.33
    } else if (image_size == "soco_slide") {
      height <- 4.76
      width <- 11.5
    } else if (image_size == "half_slide") {
      height <- 4.6
      width <- 6.75
    }
  }

  # Default start and end dates
  if (is.null(end_date)) {
    end_date <- download_date
  } else {
    end_date <- lubridate::as_date(end_date)
  }

  if (is.null(start_date)) {
    start_date <- lubridate::floor_date((end_date %m-% months(13, F)), "month")
  } else {
    start_date <- lubridate::as_date(start_date)
  }

  # Getting emergence colors
  emg_cols <- set_emergence_colors(raw.data, country, start_date, end_date)

  # Load Sharepoint environment
  if (output == "sharepoint") {
    sp_newpath <- load_sharepoint_env(raw.data)
  }

  data_p <- pull_map_data(raw.data, vdpv, country, surv, virus_type, start_date, end_date)

  if (!nrow(data_p) > 0) {
    print("No serotype-specific virus reported in country/countries - moving on")
    return(NULL)
  }

  # Add in reporting period for report_date
  if (new_detect) {
    doa <- floor_date(download_date, "week", week_start = 1)

    cli::cli_alert_info(paste("Data was pulled on ", download_date, " and will highlight all reported detections from the last week."))
    d_add <- 0
    if (new_detect_expand) {
      cli::cli_alert_info(paste0(
        "If you would like to expand the window, please specify below:\n",
        "For example, if you would like to show all detections from the last month use 21."
      ))
      d_add <- readline(prompt = "Add number of additional weeks in days to extend the reporting virus window by (7-2wk, 21-4wks): ")
      d_add <- eval(parse(text = d_add))

      date_3a <- doa - days(5 + d_add)
      date_3a_a <- doa - days(6 + d_add)
      date_3b <- doa + days(1)
    } else {
      date_3a <- doa - days(5)
      date_3a_a <- doa - days(6)
      date_3b <- doa + days(1)
    }
  }

  if (!labels %in% c("YES", "ALL")) {
    cli::cli_alert_warning("Only 'YES' and 'ALL' are accepted for the labels param at this time.")
    return(NULL)
  }

  # Getting first detections
  data_prov <- last_detections_prov(data_p, start_date, end_date)

  # Add in new detections
  if (new_detect) {
    data_r <- add_new_detection(data_p, country, date_3a, date_3a_a, date_3b)
  }

  # Pull region data
  impact_region <- unique(data_prov$place.admin.1)

  # Pull WHO Region
  m_base_ctry <- raw.data$global.ctry |>
    dplyr::filter(ADM0_NAME %in% country, ENDDATE == "9999-12-31")
  who_region <- unique(m_base_ctry$WHO_REGION)

  # Define a mapping of WHO regions to base regions
  region_mapping <- list(
    "AFRO" = c("AFRO", "EMRO"),
    "EMRO" = c("AFRO", "EMRO", "SEARO", "EURO", "WPRO"),
    "EURO" = c("AFRO", "EMRO", "SEARO", "EURO", "WPRO"),
    "SEARO" = c("SEARO", "WPRO"),
    "WPRO" = c("SEARO", "WPRO", "EURO")
  )

  # Get the corresponding base regions based on WHO region
  if (any(c("EMRO", "EURO") %in% who_region)) {
    m_base_region <- raw.data$global.ctry |>
      dplyr::filter(WHO_REGION %in% region_mapping[["EMRO"]], ENDDATE == "9999-12-31")
  } else if ("WPRO" %in% who_region) {
    m_base_region <- raw.data$global.ctry |>
      dplyr::filter(WHO_REGION %in% region_mapping[["WPRO"]], ENDDATE == "9999-12-31")
  } else if ("SEARO" %in% who_region) {
    m_base_region <- raw.data$global.ctry |>
      dplyr::filter(WHO_REGION %in% region_mapping[["SEARO"]], ENDDATE == "9999-12-31")
  } else if ("AFRO" %in% who_region) {
    m_base_region <- raw.data$global.ctry |>
      dplyr::filter(WHO_REGION %in% region_mapping[["AFRO"]], ENDDATE == "9999-12-31")
  } else {
    m_base_region <- raw.data$global.ctry |>
      dplyr::filter(WHO_REGION %in% who_region, ENDDATE == "9999-12-31")
  }

  # Create labels by country
  m_base_prov <- raw.data$global.prov |>
    dplyr::filter(ADM0_NAME %in% country, ENDDATE == "9999-12-31") |>
    dplyr::left_join(data_prov, by = c("ADM1_NAME" = "place.admin.1")) |>
    dplyr::arrange(ADM1_NAME) |>
    tibble::rownames_to_column(var = "num_label") |>
    dplyr::mutate(
      detect_status2 = dplyr::case_when(
        is.na(detect_status) ~ ">13months",
        TRUE ~ detect_status
      ),
      detect_status2 = factor(detect_status2, levels = c(">13months", "6-13months", "<6months"))
    )

  # Filters to impacted regions only
  if (labels == "ALL") {
    m_data_prov <- m_base_prov
  } else if (labels == "YES") {
    m_data_prov <- m_base_prov |> dplyr::filter(ADM1_NAME %in% impact_region)
  }

  # Create the map
  g1 <- build_detection_map(
    m_base_region, m_base_prov, data_p, m_data_prov,
    new_detect, virus_type, surv_options, start_date, date_3a,
    download_date, emg_cols, country, labels, clean_maps, data_r,
    owner
  )

  # Adjust zoom level of map
  g1a <- set_zoom_level(g1, map_ref, country, m_base_ctry)

  # Save the map
  if (output == "none") {
    cli::cli_alert_info("Output not saved")
  } else if (stringr::str_detect(output, stringr::regex("^sharepoint$", ignore_case = T))) {
    cli::cli_alert_info("Saving to Sharepoint")

    fn1 <- paste0(
      paste(virus_type, collapse = "_"), "_", paste(who_region, collapse = "_"), "_",
      paste(country, collapse = "_"), "_", run_date, ".png"
    )
    temp_path <- paste0(tempdir(), "/", fn1)

    # Save Locally
    ggplot2::ggsave(temp_path, height = height, width = width, scale = scale, dpi = dpi)
    sp_newpath_full <- file.path(sp_newpath, fn1)

    # Upload to Sharepoint
    upload_to_sharepoint(
      file_to_upload = temp_path,
      sharepoint_file_loc = sp_newpath_full,
      site = "https://cdc.sharepoint.com/teams/CGH-GID-PEB",
      drive = "Documents"
    )
  } else {
    fn10 <- paste0(
      "map_", paste(virus_type, collapse = "_"), "_", paste(who_region, collapse = "_"), "_",
      paste(country, collapse = "_"), "_", run_date, ".png"
    )
    temp_path1 <- file.path(output, fn10)
    ggplot2::ggsave(temp_path1, height = height, width = width, scale = scale, dpi = dpi)
    cli::cli_alert_info("Output written to local hard drive")
  }

  cli::cli_alert_success("Beep Beep - maps are ready")
  return(g1a)
}

# Example ----
# raw.data <- sirfunctions::get_all_polio_data()
# country <- c("central african republic", "south sudan", "sudan")
# adhoc_map(raw.data, country)
