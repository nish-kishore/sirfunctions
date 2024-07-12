#' Generate Emergence Group Movement Gifs
#' @description
#' Generate the figures and stitch together a GIF to evaluate emergence group
#' movement over time, generally aggregated as cumulative per month
#' @importFrom cli cli_alert cli_alert_info cli_h1 cli_process_done cli_process_start
#' @importFrom dplyr arrange filter group_by inner_join lag left_join mutate n pull select slice summarise
#' @importFrom ggplot2 aes coord_sf geom_bar geom_sf geom_vline ggplot ggsave labs scale_fill_brewer theme_bw
#' @importFrom ggpubr annotate_figure ggarrange
#' @importFrom lubridate as_date floor_date month year
#' @importFrom sf st_bbox
#' @importFrom tidyr expand_grid
#' @param emergence_group str: designation of the emergence group to review
#' @param pos tibble: positives data set
#' @param dist sf: shapefile of all districts
#' @param ctry sf: shapefile of all countries
#' @param include_env boolean: To include environmental detections in analysis
#' @param cumulative boolean: To display cases as cumulative
#' @param out_gif str: location where gif should be saved
#'
#' @returns GIF written out to a location
create_emergence_group_gif <- function(
    emergence_group,
    pos,
    dist,
    ctry,
    include_env = T,
    cumulative = T,
    out_gif
){

  # data <- sirfunctions::get_all_polio_data(size = "medium")
  # pos <- data$pos
  # emergence_group <- "NIE-JIS-1"
  # dist <- data$global.dist
  # ctry <- data$global.ctry
  # include_env <- T
  # cumulative <- F
  # out_gif <- "C:/Users/ynm2/OneDrive - CDC/NIE_JIS_1.gif"
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop('Package "magick" must be installed to use this function.',
         .call = FALSE)
  }

  cli::cli_process_start("Setting up data structures")

  #set up data structures
  emergence_group_pos <- pos |>
    dplyr::filter(emergencegroup == emergence_group) |>
    dplyr::select(adm0guid, admin2guid, dateonset, epid, source)

  if(!include_env){
    cli::cli_alert("Subsetting to only AFP detections")
    emergence_group_pos <- emergence_group_pos |>
      dplyr::filter(source == "AFP")
  }else{
    cli::cli_alert("Subsetting to only AFP/ENV detections")
    emergence_group_pos <- emergence_group_pos |>
      dplyr::filter(source %in% c("AFP", "ENV"))
  }

  #group data by month country and district
  monthly_pos <- emergence_group_pos |>
    dplyr::mutate(month_date = lubridate::floor_date(dateonset, unit = "month")) |>
    dplyr::group_by(adm0guid, admin2guid, month_date) |>
    dplyr::summarise(n_det = dplyr::n(), .groups = "drop")

  cli::cli_process_done()

  min_date <- monthly_pos |> dplyr::pull(month_date) |> min()

  max_date <- monthly_pos |> dplyr::pull(month_date) |> max()

  date_seq <- seq(min_date, max_date, by = "month")

  if(cumulative){
    cli::cli_alert_info("Producing cumulative results")
    monthly_pos <- tidyr::expand_grid(
      adm0guid = unique(monthly_pos$adm0guid),
      admin2guid = unique(monthly_pos$admin2guid),
      month_date = date_seq
    ) |>
      left_join(monthly_pos, by = c("adm0guid", "admin2guid", "month_date")) |>
      dplyr::group_by(adm0guid, admin2guid) |>
      dplyr::arrange(month_date) |>
      dplyr::mutate(n_det = sum(n_det, dplyr::lag(n_det, default = 0), na.rm = T))
  }

  cli::cli_process_start("Parsing spatial information")
  countries_to_include <- ctry |>
    dplyr::filter(GUID %in% (monthly_pos |>
                               dplyr::pull(adm0guid) |>
                               unique()))

  country_bbox <- sf::st_bbox(countries_to_include) |>
    sirfunctions::f.expand.bbox(X = 100000, Y = 100000)

  districts_to_include <- dist |>
    dplyr::filter(GUID %in% (monthly_pos |>
                               dplyr::pull(admin2guid) |>
                               unique()))

  cli::cli_process_done()

  monthly_pos |> dplyr::pull(n_det) |>
    table() |> print()

  cli::cli_alert_info("The distribution of detections (incidence or cumulative) is presented above. Please provide a numeric vector of values to bin the # of detections.")

  x <- readline(prompt = "Vector: ")

  x <- eval(parse(text = x))

  monthly_pos <- monthly_pos |>
    dplyr::mutate(n_det_cat = cut(n_det, breaks = x, include.lowest = T))

  cli::cli_h1(paste0("Generating individual images - Start Date: ", min_date, " - End Date: ", max_date))
  for(date_of_eval in date_seq){

    date_of_eval <- lubridate::as_date(date_of_eval)

    cli::cli_process_start(date_of_eval)

    #create temp directory
    out_file <- paste0(tempdir(), "/", date_of_eval, ".png")

    plot_data <- dplyr::inner_join(
      dist,
      dplyr::filter(monthly_pos, month_date == date_of_eval),
      by = c("GUID" = "admin2guid")
    )

    if(nrow(plot_data) == 0){
      plot_data <- dist |>
        dplyr::mutate(n_det = -1) |>
        dplyr::mutate(n_det_cat = cut(n_det, breaks = x, include.lowest = T)) |>
        dplyr::slice(1)
    }

    map <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = ctry, fill = NA) +
      ggplot2::geom_sf(data = countries_to_include, fill = "grey") +
      ggplot2::geom_sf(data = plot_data, ggplot2::aes(fill = n_det_cat), show.legend = T) +
      ggplot2::scale_fill_brewer(palette = "YlOrRd", drop = F, na.translate = F) +
      ggplot2::coord_sf(xlim = country_bbox[c("xmin","xmax")],
                        ylim = country_bbox[c("ymin","ymax")]) +
      ggplot2::theme_bw() +
      ggplot2::labs(fill = "# detections")

    epi_curve <- monthly_pos |>
      dplyr::group_by(month_date) |>
      dplyr::summarise(n_det = sum(n_det), .groups = "drop") |>
      ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(x = month_date, y = n_det), stat = "identity") +
      ggplot2::geom_vline(xintercept = date_of_eval, color = "red", linetype = 2) +
      ggplot2::labs(x = "Date", y = paste("#",ifelse(cumulative, "Cumulative", "Incident"),"detections per month")) +
      ggplot2::theme_bw()

    out_plot <- ggpubr::ggarrange(map, epi_curve, ncol = 1, heights = c(2, 1)) |>
      ggpubr::annotate_figure(top = paste0(
        ifelse(cumulative, "Cumulative", "Incident"),
        " detections of ",
        emergence_group,
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
  img_animated <- magick::image_animate(img_joined, fps = 2)

  ## save to disk
  magick::image_write(image = img_animated,
                      path = out_gif)
  cli::cli_process_done()

}
