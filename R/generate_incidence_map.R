
#' Generate incidence map timeline
#'
#' @description
#' Generates an incidence map and an epicurve showing positive detections for
#' emergence groups.
#'
#' @param pos_data `tibble` Positives dataset.
#' @param ctry_sf `ctry_sf` Global country shapefile.
#' @param emergence_group `str` An emergence group or a vector of emergence group names.
#' @param emergence_colors `list` A named list where each emergence group is mapped its own
#' color. Names correspond to the emergence group, while values are the colors. If no colors
#' are passed, then colors are randomly selected for each emergence group.
#' @param sources `str` Source of detection or a vector of source names. Valid values are:
#' "AFP", "Community", "Contact", "ENV", "Healthy", "iVDPV".
#' @param start_date `str` Start date of the map. By default, it will be the earliest date of
#' detection.
#' @param end_date `str` End date of the map. By default, it will be the latest detection.
#' @param monthly_rolling_window `int` Monthly rolling window to show. Defaults to 12.
#' @param output_dir `str` Local path to the directory to output the figure. Defaults to NULL,
#' which does not export the figure.
#' @param fps `int` Frames per second. To increase the speed of the GIF, increase the fps.
#' By default, it is set to 2.
#' @param drop_legend `logical` Drop legends of the figure or not.
#' @param drop_ctry_labels `logical` Drop country labels for the map.
#' @param drop_description `logical` Drop the case counts of the map caption.
#' @param pt_size `int` Size of the points in the map. Defaults to 5.
#'
#' @returns `gif` A GIF showing the emergence over time.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data
#' generate_incidence_map(raw_data$pos, raw_data$global.ctry, emergence_group = c("YEM-TAI-1", "SOM-BAN-1", "ETH-TIG-1"))
#' }
generate_incidence_map <- function(pos_data,
                                   ctry_sf,
                                   emergence_group = NULL,
                                   emergence_colors = NULL,
                                   sources = c("AFP", "ENV"),
                                   start_date = NULL,
                                   end_date = NULL,
                                   monthly_rolling_window = 12,
                                   output_dir = NULL,
                                   fps = 2,
                                   drop_legend = FALSE,
                                   drop_ctry_labels = FALSE,
                                   drop_description = FALSE,
                                   pt_size = 0.7
                                   ) {

  # Checking for required packages ----
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop('Package "magick" must be installed to use this function.',
         .call = FALSE
    )
  }

  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    stop(
      'Package "ggpubr" must be installed to use this function.',
      call. = FALSE
    )
  }

  cli::cli_process_start("Setting up data structures")

  emergence_group_pos <- pos_data |>
    dplyr::select(ctry = place.admin.0, adm0guid,
                  dateonset, epid, source, emergencegroup, latitude, longitude) |>
    dplyr::mutate(month_date = lubridate::floor_date(dateonset, unit = "month"))

  # Filter ----
  if (!is.null(emergence_group)) {
    emergence_group_pos <- emergence_group_pos |>
      dplyr::filter(emergencegroup %in% emergence_group)
  }

  if (!is.null(start_date)) {
    emergence_group_pos <- emergence_group_pos |>
      dplyr::filter(dateonset >= lubridate::as_date(start_date))
  }

  if (!is.null(end_date)) {
    emergence_group_pos <- emergence_group_pos |>
      dplyr::filter(dateonset <= lubridate::as_date(end_date))
  }

  if (!is.null(sources)) {
    emergence_group_pos <- emergence_group_pos |>
      dplyr::filter(source %in% sources)
  }

  # Turn Country and Emergence Groups to Factors
  emergence_group_pos <- emergence_group_pos |>
    dplyr::mutate(emergencegroup = as.factor(emergencegroup),
                  ctry = as.factor(ctry),
                  source = as.factor(source))

  # Calculate date window ----

  min_date <- emergence_group_pos |>
    dplyr::pull(month_date) |>
    min()

  max_date <- emergence_group_pos |>
    dplyr::pull(month_date) |>
    max()

  date_seq <- seq(min_date, max_date, by = "month")

  # Create monthly summaries ----

  # (a) By month only
  monthly_pos <- tidyr::expand_grid(month_date = date_seq) |>
    left_join(emergence_group_pos, by = "month_date") |>
    group_by(month_date) |>
    summarise(n_det = sum(!is.na(epid)), .groups = "drop")

  # (b) By month and source
  monthly_pos_source <- tidyr::expand_grid(
    month_date = date_seq,
    source = unique(emergence_group_pos$source)
  ) |>
    left_join(emergence_group_pos, by = c("month_date", "source")) |>
    group_by(month_date, source) |>
    summarise(n_det = sum(!is.na(epid)), .groups = "drop")

  # (c) By month and emergencegroup
  monthly_pos_emergence <- tidyr::expand_grid(
    month_date = date_seq,
    emergencegroup = unique(emergence_group_pos$emergencegroup)
  ) |>
    left_join(emergence_group_pos, by = c("month_date", "emergencegroup")) |>
    group_by(month_date, emergencegroup) |>
    summarise(n_det = sum(!is.na(epid)), .groups = "drop")

  # (d) By month, adm0guid, and ctry
  monthly_pos_ctry <- tidyr::expand_grid(
    month_date = date_seq,
    adm0guid = unique(emergence_group_pos$adm0guid)
  ) |>
    left_join(emergence_group_pos, by = c("month_date", "adm0guid")) |>
    group_by(month_date, adm0guid) |>
    summarise(n_det = sum(!is.na(epid)), .groups = "drop") |>
    dplyr::left_join(dplyr::distinct(emergence_group_pos |>
                                       dplyr::select(adm0guid, ctry)))

  cli::cli_process_done()

  # Still in draft, disabling cumulative argument for now
  cumulative <- FALSE
  if (cumulative) {
    cli::cli_alert_info("Producing cumulative results")
    monthly_pos <- monthly_pos |>
      dplyr::mutate(n_det = if_else(is.na(n_det), 0, n_det)) |>
      dplyr::mutate(n_det = cumsum(n_det))

    monthly_pos_source <- monthly_pos_source |>
      dplyr::mutate(n_det = if_else(is.na(n_det), 0, n_det)) |>
      dplyr::group_by(source) |>
      dplyr::mutate(n_det = cumsum(n_det)) |>
      dplyr::ungroup()

    monthly_pos_emergence <- monthly_pos_emergence |>
      dplyr::mutate(n_det = if_else(is.na(n_det), 0, n_det)) |>
      dplyr::group_by(emergencegroup) |>
      dplyr::mutate(n_det = cumsum(n_det)) |>
      dplyr::ungroup()

    monthly_pos_ctry <- monthly_pos_ctry |>
      dplyr::mutate(n_det = if_else(is.na(n_det), 0, n_det)) |>
      dplyr::group_by(adm0guid, ctry) |>
      dplyr::mutate(n_det = cumsum(n_det)) |>
      dplyr::ungroup()
  }


  cli::cli_process_start("Parsing spatial information")
  ctry_sf <- ctry_sf |>
    dplyr::mutate(ADM0_NAME = dplyr::if_else(ADM0_NAME == "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM",
                                             "PALESTINE", ADM0_NAME))
  countries_to_include <- ctry_sf |>
    dplyr::filter(GUID %in% (emergence_group_pos |>
                               dplyr::pull(adm0guid) |>
                               unique()))

  country_bbox <- sf::st_bbox(countries_to_include) |>
    sirfunctions::f.expand.bbox(X = 100000, Y = 100000)

  cli::cli_process_done()

  # Set up colors and shapes ----
  if (is.null(emergence_colors)) {
    emergence_colors <- emergence_group_pos$emergencegroup |> unique()
    n_colors <- length(emergence_colors)
    random_colors <- rgb(runif(n_colors), runif(n_colors), runif(n_colors))
    names(random_colors) <- emergence_colors
    emergence_colors <- random_colors
  }

  cli::cli_h1(paste0("Generating individual images - Start Date: ", min_date, " - End Date: ", max_date))

  # Remove images from previous runs ----
  imgs <- list.files(tempdir(), pattern = "-01.png", full.names = T)
  if (length(imgs) > 0) {
    lapply(imgs, \(x) file.remove(x))
  }

  # Generate snapshots ----

  purrr::map(date_seq, \(date_of_eval) {
    make_incidence_map(date_of_eval,
                       emergence_group,
                       emergence_colors,
                       emergence_group_pos,
                       monthly_pos,
                       monthly_pos_ctry, monthly_pos_source,
                       monthly_pos_emergence,
                       countries_to_include,
                       ctry_sf, country_bbox,
                       monthly_rolling_window,
                       max_date,
                       drop_legend,
                       drop_ctry_labels,
                       drop_description,
                       pt_size,
                       cumulative=FALSE)
  }, .progress = TRUE)

  # Creating the GIF ----
  cli::cli_h1("Generating Gif")

  cli::cli_process_start("Reading images")
  ## list file names and read in
  imgs <- list.files(tempdir(), pattern = "-01.png", full.names = T)
  img_list <- lapply(imgs, magick::image_read)

  cli::cli_process_done()

  cli::cli_process_start("Joining images")
  ## join the images together
  img_joined <- magick::image_join(img_list)
  cli::cli_process_done()

  cli::cli_process_start("Generating gif")
  ## animate at 2 frames per second
  img_animated <- magick::image_animate(img_joined, fps = fps)

  ## save to disk
  if (!is.null(output_dir)) {
    magick::image_write(
      image = img_animated,
      path = file.path(output_dir,
                       paste0(paste(emergence_group, collapse = ", "),
                              " emergences from ", min_date, " to ", max_date, ".gif"
                       ))
    )
  }
  cli::cli_process_done()
  return(invisible(img_animated))
}

# Private functions ----
make_incidence_map <- function(date_of_eval,
                               emergence_group,
                               emergence_colors,
                               emergence_group_pos,
                               monthly_pos,
                               monthly_pos_ctry, monthly_pos_source,
                               monthly_pos_emergence,
                               countries_to_include,
                               ctry_sf, country_bbox,
                               monthly_rolling_window, max_date,
                               drop_legend,
                               drop_ctry_labels,
                               drop_description,
                               pt_size,
                               cumulative=FALSE) {
  date_of_eval <- lubridate::as_date(date_of_eval)
  rolling_month_end <- date_of_eval %m+% months(monthly_rolling_window)

  if (rolling_month_end > max_date) {
    rolling_month_end <- max_date
  }

  # create temp directory
  out_file <- paste0(tempdir(), "/", date_of_eval, ".png")

  if (cumulative) {
    plot_data <- emergence_group_pos |>
      dplyr::filter(dplyr::between(month_date, min_date, date_of_eval))
  } else {
    plot_data <- emergence_group_pos |>
      dplyr::filter(dplyr::between(month_date, date_of_eval,
                                   rolling_month_end))
  }

  ctry_counts <- monthly_pos_ctry |>
    dplyr::filter(between(month_date, date_of_eval,
                          rolling_month_end)) |>
    dplyr::mutate(ctry = stringr::str_to_title(ctry)) |>
    dplyr::select(ctry, n_det) |>
    dplyr::group_by(ctry) |>
    dplyr::summarize(n_det = sum(n_det, na.rm = TRUE)) |>
    dplyr::arrange(ctry)

  ctry_counts_label <- ""
  for (i in 1:nrow(ctry_counts)) {
    ctry_counts_label <- paste0(ctry_counts_label,
                                paste0(ctry_counts[[i, "ctry"]],": ",
                                       ctry_counts[[i, "n_det"]]," ")
    )
  }

  emergence_counts <- monthly_pos_emergence |>
    dplyr::filter(between(month_date, date_of_eval,
                          rolling_month_end)) |>
    dplyr::arrange(emergencegroup) |>
    dplyr::select(emergencegroup, n_det) |>
    dplyr::group_by(emergencegroup) |>
    dplyr::summarize(n_det = sum(n_det, na.rm = TRUE)) |>
    dplyr::arrange(emergencegroup)

  emg_counts_label <- ""
  for (i in 1:nrow(emergence_counts)) {
    emg_counts_label <- paste0(emg_counts_label,
                               paste0(emergence_counts[[i, "emergencegroup"]],": ",
                                      emergence_counts[[i, "n_det"]]," ")
    )
  }

  source_counts <- monthly_pos_source |>
    dplyr::filter(between(month_date, date_of_eval,
                          rolling_month_end)) |>
    dplyr::arrange(source) |>
    dplyr::select(source, n_det) |>
    dplyr::group_by(source) |>
    dplyr::summarize(n_det = sum(n_det, na.rm = TRUE))

  source_counts_label <- ""
  for (i in 1:nrow(source_counts)) {
    source_counts_label <- paste0(source_counts_label,
                                  paste0(source_counts[[i, "source"]],": ",
                                         source_counts[[i, "n_det"]]," ")
    )
  }


  if (drop_legend) {
    emergence_colors[] <- "orange"
  }

  map <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::geom_sf(data = ctry_sf, fill = "lightgrey") +
    ggplot2::geom_sf(data = countries_to_include, fill = "white", color = "black", lwd = 0.5)

  if (!drop_ctry_labels) {
    map <- map +
      ggplot2::geom_sf_text(data = ctry_sf |> filter(yr.end == 9999),
                                           aes(label = stringr::str_to_title(ADM0_NAME)), color = "black",
                                           size = 2)
  }

  map <- map +
    ggplot2::geom_point(data = plot_data,
                        ggplot2::aes(x = as.numeric(longitude),
                                     y = as.numeric(latitude),
                                     color = emergencegroup,
                                     shape = source
                        ), size = pt_size, show.legend = T)

  if (!drop_description) {
    map <- map +
      ggplot2::labs(caption = paste0(ctry_counts_label, "\n",
                                     emg_counts_label, "\n",
                                     source_counts_label))
  }

  map <- map +
    ggplot2::ylab("") +
    ggplot2::xlab("") +
    ggplot2::scale_color_manual(name = "Emergence Group", values = emergence_colors, drop = FALSE, na.value = "orange") +
    ggplot2::scale_shape_manual(name = "Detection Type", values = list("ENV" = 15, "AFP" = 16), drop = FALSE) +
    ggplot2::coord_sf(
      xlim = country_bbox[c("xmin", "xmax")],
      ylim = country_bbox[c("ymin", "ymax")]
    ) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#E1EEF9"),
                   legend.key = ggplot2::element_rect(fill = NA),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank()
                   )

  if (drop_legend) {
    map <- map +
      ggplot2::guides(color = "none") +
      ggplot2::theme_bw()
  }

  epi_curve <- monthly_pos_emergence |>
    ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::annotate("rect",
                      xmin = date_of_eval, xmax = rolling_month_end,
                      ymin = 0, ymax = Inf,
                      color = NA, fill = "grey", alpha = 0.3) +
    ggplot2::geom_bar(ggplot2::aes(x = month_date, y = n_det, fill = emergencegroup), stat = "identity") +
    ggplot2::scale_fill_manual(values = emergence_colors, name = "Emergence Group", na.value = "orange") +
    ggplot2::labs(x = "Date", y = paste("#", "Incident", "detections per month"))

  if (drop_legend) {
    epi_curve <- epi_curve +
      ggplot2::guides(fill = "none") +
      ggplot2::theme_bw()
  }

  if (!is.null(emergence_group)) {
    emg_group_description <- paste0("of ",  paste(emergence_group, collapse = ", "))
  } else {
    emg_group_description <- ""
  }

  out_plot <- ggpubr::ggarrange(map, epi_curve, ncol = 1, heights = c(2, 1)) |>
    ggpubr::annotate_figure(top = paste0(
      ifelse(cumulative, "Cumulative", "Incident"),
      " detections",
      emg_group_description,
      ":\n",
      lubridate::month(date_of_eval, label = T),
      " ",
      lubridate::year(date_of_eval),
      " - ",
      lubridate::month(rolling_month_end, label = T),
      " ",
      lubridate::year(rolling_month_end)
    ))

  ggplot2::ggsave(out_file, plot = out_plot, dpi = 300, height = 8, width = 6, bg = "white")
  invisible()

  }

