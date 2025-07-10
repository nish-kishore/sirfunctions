
generate_incidence_map <- function(pos_data,
                                   ctry_sf,
                                   prov_sf,
                                   emergence_group = NULL,
                                   emergence_colors = NULL,
                                   sources = c("AFP", "ENV"),
                                   start_date = NULL,
                                   end_date = NULL,
                                   output_dir = getwd()) {
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

  # set up data structures
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

  min_date <- emergence_group_pos |>
    dplyr::pull(month_date) |>
    min()

  max_date <- emergence_group_pos |>
    dplyr::pull(month_date) |>
    max()

  date_seq <- seq(min_date, max_date, by = "month")

  # (a) By month only
  monthly_pos <- tidyr::expand_grid(month_date = date_seq) %>%
    left_join(emergence_group_pos, by = "month_date") %>%
    group_by(month_date) %>%
    summarise(n_det = sum(!is.na(epid)), .groups = "drop")

  # (b) By month and source
  monthly_pos_source <- tidyr::expand_grid(
    month_date = date_seq,
    source = unique(emergence_group_pos$source)
  ) %>%
    left_join(emergence_group_pos, by = c("month_date", "source")) %>%
    group_by(month_date, source) %>%
    summarise(n_det = sum(!is.na(epid)), .groups = "drop")

  # (c) By month and emergencegroup
  monthly_pos_emergence <- tidyr::expand_grid(
    month_date = date_seq,
    emergencegroup = unique(emergence_group_pos$emergencegroup)
  ) %>%
    left_join(emergence_group_pos, by = c("month_date", "emergencegroup")) %>%
    group_by(month_date, emergencegroup) %>%
    summarise(n_det = sum(!is.na(epid)), .groups = "drop")

  # (d) By month, adm0guid, and ctry
  monthly_pos_ctry <- tidyr::expand_grid(
    month_date = date_seq,
    adm0guid = unique(emergence_group_pos$adm0guid)
  ) %>%
    left_join(emergence_group_pos, by = c("month_date", "adm0guid")) %>%
    group_by(month_date, adm0guid) %>%
    summarise(n_det = sum(!is.na(epid)), .groups = "drop") |>
    dplyr::left_join(dplyr::distinct(emergence_group_pos |>
                                       dplyr::select(adm0guid, ctry)))

  cli::cli_process_done()

  cli::cli_process_start("Parsing spatial information")
  countries_to_include <- ctry_sf |>
    dplyr::filter(GUID %in% (emergence_group_pos |>
                               dplyr::pull(adm0guid) |>
                               unique()))
  prov_to_include <- prov_sf |>
    dplyr::filter(ADM0_GUID %in% countries_to_include$GUID)

  country_bbox <- sf::st_bbox(countries_to_include) |>
    sirfunctions::f.expand.bbox(X = 100000, Y = 100000)

  cli::cli_process_done()

  # Set up colors and shapes
  if (is.null(emergence_colors)) {
    emergence_colors <- emergence_group_pos$emergencegroup |> unique()
    n_colors <- length(emergence_colors)
    random_colors <- rgb(runif(n_colors), runif(n_colors), runif(n_colors))
    names(random_colors) <- emergence_colors
    emergence_colors <- random_colors
  }

  cli::cli_h1(paste0("Generating individual images - Start Date: ", min_date, " - End Date: ", max_date))
  # Remove images from previous runs
  imgs <- list.files(tempdir(), pattern = "-01.png", full.names = T)
  if (length(imgs) > 0) {
    lapply(imgs, \(x) file.remove(x))
  }

  for (date_of_eval in date_seq) {
    date_of_eval <- lubridate::as_date(date_of_eval)

    cli::cli_process_start(date_of_eval)

    # create temp directory
    out_file <- paste0(tempdir(), "/", date_of_eval, ".png")

    plot_data <- emergence_group_pos |>
      dplyr::filter(between(month_date, min_date, date_of_eval))

    ctry_counts <- monthly_pos_ctry |>
      dplyr::filter(month_date == date_of_eval) |>
      dplyr::mutate(ctry = stringr::str_to_title(ctry)) |>
      dplyr::select(ctry, n_det) |>
      dplyr::arrange(ctry) |>
      tidyr::pivot_wider(names_from = ctry, values_from = n_det)

    emergence_counts <- monthly_pos_emergence |>
      dplyr::filter(month_date == date_of_eval) |>
      dplyr::arrange(emergencegroup) |>
      dplyr::select(emergencegroup, n_det) |>
      tidyr::pivot_wider(names_from = emergencegroup, values_from = n_det)

    source_counts <- monthly_pos_source |>
      dplyr::filter(month_date == date_of_eval) |>
      dplyr::arrange(source) |>
      dplyr::select(source, n_det) |>
      tidyr::pivot_wider(names_from = source, values_from = n_det) |>
      flextable::flextable()

    map <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = ctry_sf, fill = "white") +
      ggplot2::geom_sf(data = countries_to_include, fill = "lightgrey", color = "black", lwd = 1) +
      ggplot2::geom_sf(data = prov_to_include, fill = NA, color = "darkgrey") +
      # ggplot2::geom_sf_text(data = countries_to_include,
      #                       ggplot2::aes(label = stringr::str_to_title(ADM0_NAME))) +
      ggplot2::geom_point(data = plot_data,
                          ggplot2::aes(x = as.numeric(longitude),
                                       y = as.numeric(latitude),
                                       color = emergencegroup,
                                       shape = source
                                       ), show.legend = T) +
      ggplot2::ylab("") +
      ggplot2::xlab("") +
      ggplot2::scale_color_manual(name = "Emergence Group", values = emergence_colors) +
      ggplot2::scale_shape_manual(name = "Detection Type", values = list("ENV" = 15, "AFP" = 16)) +
      ggplot2::coord_sf(
        xlim = country_bbox[c("xmin", "xmax")],
        ylim = country_bbox[c("ymin", "ymax")]
      ) +
      ggplot2::theme_bw()

    epi_curve <- monthly_pos_emergence |>
      ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(x = month_date, y = n_det, fill = emergencegroup), stat = "identity") +
      ggplot2::scale_fill_manual(values = emergence_colors, name = "Emergence Group") +
      ggplot2::geom_vline(xintercept = date_of_eval, color = "red", linetype = 2) +
      ggplot2::labs(x = "Date", y = paste("#", ifelse(cumulative, "Cumulative", "Incident"), "detections per month")) +
      ggplot2::theme_bw()

    out_plot <- ggpubr::ggarrange(map, epi_curve, ncol = 1, heights = c(2, 1)) |>
      ggpubr::annotate_figure(top = paste0(
        ifelse(cumulative, "Cumulative", "Incident"),
        " detections of ",
        paste(emergence_group, collapse = ", "),
        ": ",
        lubridate::month(date_of_eval, label = T),
        ", ",
        lubridate::year(date_of_eval)
      ))

    ggplot2::ggsave(out_file, plot = out_plot, dpi = 300, height = 8, width = 6, bg = "white")

    cli::cli_process_done()
  }

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
