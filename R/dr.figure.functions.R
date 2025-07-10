# Figures ----
## Plots ----
#' Timeliness intervals of samples at the country level
#'
#' A stacked horizontal bar graph for timeliness intervals of samples at the country level.
#' To get the full intervals from field to lab, the lab data needs to be attached. Otherwise,
#' only the timeliness intervals from the field up to when it was sent to lab will be displayed.
#'
#' @param int.data `tibble` Summary table with timeliness intervals at the country level.
#' @param output_path `str` Path where to output the figure.
#' @param afp.year.lab `tibble` `r lifecycle::badge("deprecated")`
#' Deprecated since it is not used anymore.
#' @returns `ggplot` Plot of timeliness intervals at the country level.
#'
#' @examples
#' \dontrun{
#' # Attaching lab data
#' lab_path <- "C:/Users/ABC1/Desktop/algeria_lab_data.csv"
#' ctry.data <- init_dr("algeria", lab_data_path = lab_path)
#' lab.timeliness.ctry <- generate_lab_timeliness(ctry.data$lab.data, "ctry", start_date, end_date)
#' int.data.ctry <- generate_int_data(ctry.data, start_date, end_date,
#'   spatial.scale = "ctry",
#'   lab.timeliness.ctry
#' )
#' generate_ctry_timeliness_graph(int.data.ctry)
#' }
#' @seealso [generate_int_data()]
#' @export
generate_ctry_timeliness_graph <- function(int.data,
                                           output_path = Sys.getenv("DR_FIGURE_PATH"),
                                           afp.year.lab = lifecycle::deprecated()) {
  if (!requireNamespace("forcats", quietly = TRUE)) {
    stop(
      'Package "forcats" must be installed to use this function.',
      call. = FALSE
    )
  }

  if (lifecycle::is_present(afp.year.lab)) {
    lifecycle::deprecate_warn(
      when = "1.3.0",
      what = "generate_ctry_timeliness_graph(afp.prov.year.lab)",
      details = "afp.year.lab is not necessary to run this function."
    )
  }

  int.data <- int.data |>
    dplyr::filter(medi >= 0 | !is.na(medi))

  if (nrow(int.data) == 0) {
    return(output_empty_image(output_path, "timely_nation.png"))
  }

  timely_nation <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = int.data,
      ggplot2::aes(
        x = labs,
        y = medi,
        fill = forcats::fct_rev(type)
      ),
      position = "stack",
      stat = "identity"
    ) +
    ggplot2::geom_text(
      data = int.data,
      ggplot2::aes(
        x = labs,
        y = medi,
        label = medi,
        group = forcats::fct_rev(type)
      ),
      position = ggplot2::position_stack(vjust = 0.5)
    ) +
    ggplot2::coord_flip() +
    ggplot2::ylab("Median Days") +
    ggplot2::xlab("Year of Paralysis Onset") +
    ggplot2::scale_x_discrete(labels = labs) +
    ggplot2::scale_fill_manual(
      name = "Interval",
      drop = T,
      values = f.color.schemes("timeliness.col.vars"),
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::theme(
      legend.position = "right",
      legend.background = ggplot2::element_blank()
    )

  ggplot2::ggsave(
    "timely_nation.png",
    plot = timely_nation,
    path = output_path,
    width = 14,
    height = 4
  )

  return(timely_nation)
}

#' Timeliness intervals of samples at the province level
#'
#' A stacked horizontal bar graph for timeliness intervals of samples at the province level.
#' To get the full intervals from field to lab, the lab data needs to be attached. Otherwise,
#' only the timeliness intervals from the field up to when it was sent to lab will be displayed.
#'
#' @param int.data `tibble` Summary table with timeliness intervals at the province level.
#' @param output_path `str` Path where to output the figure.
#' @param afp.prov.year.lab `tibble` `r lifecycle::badge("deprecated")`
#' Deprecated since it is not used anymore.
#'
#' @returns `ggplot` Plot of timeliness intervals at the country level.
#' @examples
#' \dontrun{
#' # Attaching lab data
#' lab_path <- "C:/Users/ABC1/Desktop/algeria_lab_data.csv"
#' ctry.data <- init_dr("algeria", lab_data_path = lab_path)
#' lab.timeliness.prov <- generate_lab_timeliness(ctry.data$lab.data, "prov", start_date, end_date)
#' int.data.prov <- generate_int_data(ctry.data, start_date, end_date,
#'   spatial.scale = "prov",
#'   lab.timeliness.prov
#' )
#' generate_ctry_timeliness_graph(int.data.prov)
#' }
#' @export
generate_prov_timeliness_graph <- function(int.data,
                                           output_path = Sys.getenv("DR_FIGURE_PATH"),
                                           afp.prov.year.lab = lifecycle::deprecated()) {
  if (!requireNamespace("forcats", quietly = TRUE)) {
    stop(
      'Package "forcats" must be installed to use this function.',
      call. = FALSE
    )
  }

  if (lifecycle::is_present(afp.prov.year.lab)) {
    lifecycle::deprecate_warn(
      when = "1.3.0",
      what = "generate_prov_timeliness_graph(afp.prov.year.lab)",
      details = "afp.prov.year.lab is not necessary to run this function."
    )
  }

  int.data <- int.data |>
    dplyr::filter(medi >= 0 | !is.na(medi))

  if (nrow(int.data) == 0) {
    return(output_empty_image(output_path, "timely_prov.png"))
  }

  timely_prov <- ggplot2::ggplot(int.data |>
    dplyr::filter(is.na(medi) == F &
      is.na(prov) == F)) +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = as.character(labs),
        y = medi,
        fill = forcats::fct_rev(type)
      ),
      position = "stack",
      stat = "identity"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = labs,
        y = medi,
        label = medi,
        group = forcats::fct_rev(type)
      ),
      size = 3,
      position = ggplot2::position_stack(vjust = 0.5)
    ) +
    ggplot2::coord_flip() +
    ggplot2::ylab("Median Days") +
    ggplot2::xlab("Year of Paralysis Onset") +
    ggplot2::scale_x_discrete() +
    ggplot2::ylab("Days") +
    ggplot2::xlab("Year") +
    ggplot2::scale_fill_manual(
      name = "Interval",
      values = f.color.schemes("timeliness.col.vars"),
      guide = ggplot2::guide_legend(reverse = TRUE),
      drop = T
    ) +
    ggplot2::facet_grid(prov ~ ., scales = "free_y", space = "free", switch = "y", labeller = label_wrap_gen(8)) +
    ggplot2::theme(
      legend.position = "right",
      legend.background = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_text(size = 7, angle = 0)
    )

  ggplot2::ggsave(
    "timely_prov.png",
    plot = timely_prov,
    path = output_path,
    width = 14,
    height = 10
  )

  return(timely_prov)
}


#' Epicurve of AFP cases by year
#'
#' Generates an epicurve line graph of AFP cases by year.
#'
#' @param ctry.data `list` Large list containing country polio data. This is the output of either
#' [extract_country_data()] or [init_dr()].
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis. By default, it is up to the current date.
#' @param output_path `str` Local path location to save the figure.
#'
#' @returns `ggplot` A line graph of AFP cases faceted by year.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' generate_afp_epicurve(ctry.data, start_date)
#' }
#'
#' @export
generate_afp_epicurve <- function(ctry.data,
                                  start_date,
                                  end_date = lubridate::today(),
                                  output_path = Sys.getenv("DR_FIGURE_PATH")) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  afp.epi.date.filter <- ctry.data$afp.epi %>%
    dplyr::filter(
      dplyr::between(yronset, as.numeric(lubridate::year(start_date)), as.numeric(lubridate::year(end_date))),
      cdc.classification.all2 != "NOT-AFP"
    )

  case.num.labs <- dplyr::reframe(
    dplyr::group_by(afp.epi.date.filter, yronset),
    labs = paste0(yronset, " (N = ", sum(afp.cases), ")")
  ) %>%
    dplyr::distinct(.)

  afp.epi.date.filter1 <- dplyr::left_join(afp.epi.date.filter,
    case.num.labs,
    by = c("yronset" = "yronset")
  )

  if (nrow(afp.epi.date.filter1) == 0) {
    return(output_empty_image(output_path, "afp.epi.curve.png"))
  }

  afp.epi.curve <- ggplot2::ggplot(
    afp.epi.date.filter1,
    ggplot2::aes(fill = cdc.classification.all2, y = afp.cases, x = epi.week)
  ) +
    ggplot2::geom_bar(position = "stack", stat = "identity") +
    ggplot2::scale_fill_manual(
      values = sirfunctions::f.color.schemes(type = "epicurve"),
      name = "Classification",
      drop = T
    ) +
    sirfunctions::f.plot.looks(type = "epicurve") +
    ggplot2::facet_wrap(~labs, ncol = 3, drop = F)

  ggplot2::ggsave(
    "afp.epi.curve.png",
    plot = afp.epi.curve,
    path = output_path,
    width = 14,
    height = 5
  )

  return(afp.epi.curve)
}



#' AFP cases by province and year
#'
#' Generates a tile plot for the number of AFP cases per month by province.
#'
#' @param afp.by.month.prov `tibble` Table summarizing AFP cases by month and province. This is the output of
#' [generate_afp_by_month_summary()].
#' @param start_date `str` Start date of the analysis.
#' @param end_date `str` End date of the analysis. By default, it displays the most recent date.
#' @param output_path `str` Local path to output the figure.
#'
#' @returns `ggplot` A tile plot displaying the number of AFP cases by month and province.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' afp.by.month <- generate_afp_by_month(ctry.data$afp.all.2, start_date, end_date)
#' afp.by.month.prov <- generate_afp_by_month_summary(
#'   afp.by.month, ctry.data,
#'   start_date, end_date, "prov"
#' )
#' generate_afp_prov_year(afp.by.month.prov, start_date, end_date)
#' }
#'
#' @export
generate_afp_prov_year <- function(afp.by.month.prov,
                                   start_date,
                                   end_date = lubridate::today(),
                                   output_path = Sys.getenv("DR_FIGURE_PATH")) {
  if (!requireNamespace("forcats", quietly = TRUE)) {
    stop(
      'Package "forcats" must be installed to use this function.',
      call. = FALSE
    )
  }

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  afp.month.prov.g <- afp.by.month.prov |>
    dplyr::filter(dplyr::between(year, lubridate::year(start_date), lubridate::year(end_date)), !is.na(prov))

  afp.month.prov.g$case.cat <- factor(afp.month.prov.g$case.cat, levels = c(c("0", "1", "2-5", "6-9", "10+")))

  if (nrow(afp.month.prov.g) == 0) {
    return(output_empty_image(output_path, "afp.dets.prov.year.png"))
  }

  # add a point to indicate cVDPV2 detections

  afp.dets.prov.year <- ggplot2::ggplot(
    afp.month.prov.g |>
      dplyr::arrange(u15pop),
    ggplot2::aes(
      x = mon.year,
      y = forcats::fct_inorder(prov),
      fill = case.cat
    )
  ) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::ggtitle("Number of AFP Cases by Province") +
    sirfunctions::f.plot.looks("geomtile") +
    ggplot2::scale_fill_manual(
      values = sirfunctions::f.color.schemes("afp.prov"),
      name = "AFP Cases",
      drop = T
    ) +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0)) +
    ggplot2::labs(caption = "Provinces are ordered by under 15 population, with highest on top")

  ggplot2::ggsave(
    "afp.dets.prov.year.png",
    plot = afp.dets.prov.year,
    path = output_path,
    width = 14,
    height = 5
  )

  return(afp.dets.prov.year)
}



#' Virus detection in ES sites
#'
#' Generates a dot plot for viral detections across ES sites, with SIA dates
#' overlaid.
#'
#' @param sia.data `tibble` SIA surveillance data.
#' @param es.data Environmental surveillance data, cleaned using [clean_es_data()] or a cleaned `ctry.data$es`.
#' @param es_start_date `str` Start date of analysis. By default, it is one year from the end date.
#' @param es_end_date `str` End date of analysis.
#' @param vaccine_types `list` A named list with colors assigned names corresponding to vaccine types. By
#' default, it will use a prefilled list inside the function. However, the function will alert for missing
#' vaccine types and the user must pass another list appended by that vaccine type.
#' @param detection_types `list` A named list with colors assigned names corresponding to viral detection type.
#' By default, it will use a prefilled list inside the function. However, the function will alert for missing
#' detection types and the user must pass another list appended by that vaccine type.
#' @param output_path `str` Local path to output the figure to.
#' @param ctry.data
#' `r lifecycle::badge("deprecated")`
#' Please pass the SIA data directly to sia.data instead of a list containing it.
#' @param es.data.long
#' `r lifecycle::badge("deprecated")`
#' Please pass cleaned ES data instead.
#'
#' @returns `ggplot` A dot plot of viral detections per ES sites and SIA campaigns.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' es.data <- clean_es_data(ctry.data$es)
#' generate_es_site_det(ctry.data, es.data)
#' }
#' @export
generate_es_site_det <- function(sia.data,
                                 es.data,
                                 es_start_date = (lubridate::as_date(es_end_date) - lubridate::years(1)),
                                 es_end_date = end_date,
                                 output_path = Sys.getenv("DR_FIGURE_PATH"),
                                 vaccine_types = NULL,
                                 detection_types = NULL,
                                 ctry.data = lifecycle::deprecated(),
                                 es.data.long = lifecycle::deprecated()) {
  if (lifecycle::is_present(es.data.long)) {
    lifecycle::deprecate_warn(
      "1.3.0",
      "generate_es_site_det(es.data.long)",
      "generate_es_site_det(es.data)"
    )
    es.data <- es.data.long
  }

  if (lifecycle::is_present(ctry.data)) {
    lifecycle::deprecate_warn(
      "1.3.0",
      "generate_es_site_det(ctry.data)",
      "generate_es_site_det(sia.data)"
    )
    sia.data <- ctry.data$sia
  }


  es_start_date <- lubridate::as_date(es_start_date)
  es_end_date <- lubridate::as_date(es_end_date)

  es.data <- es.data |>
    dplyr::filter(dplyr::between(collect.date, es_start_date, es_end_date))

  if (nrow(es.data) == 0) {
    return(output_empty_image(output_path, "es.site.det.png"))
  }

  sias <- sia.data %>%
    dplyr::filter(status == "Done") %>%
    dplyr::filter(yr.sia >= lubridate::year(es_start_date) &
      yr.sia <= lubridate::year(es_end_date)) %>%
    dplyr::filter(province %in% es.data$ADM1_NAME)

  if (nrow(sia.data) == 0) {
    cli::cli_alert_info("No SIAs in the specified date range")
  }

  sias$activity.start.date <- as.Date(sias$activity.start.date)
  sias$activity.end.date <- as.Date(sias$activity.end.date)

  minsy <- dplyr::count(
    sias,
    yr.sia,
    province,
    activity.start.date,
    activity.end.date,
    vaccine.type
  )

  colnames(minsy)[colnames(minsy) == "province"] <- "ADM1_NAME"

  default_vaccine_type <- f.color.schemes("es.vaccine.types")
  default_detections <- f.color.schemes("es.detections")

  if (is.null(vaccine_types)) {
    vaccine_types <- default_vaccine_type
  }

  if (is.null(detection_types)) {
    detection_types <- default_detections
  }

  # Check whether the vaccine types and detection are present in the ggplot
  minsy_vaccine_types <- minsy |>
    dplyr::select("vaccine.type") |>
    unique() |>
    dplyr::pull()

  es.data_all_dets <- es.data |>
    dplyr::select("all_dets") |>
    unique() |>
    dplyr::pull()

  miss_vaccine <- minsy_vaccine_types[!(minsy_vaccine_types %in% names(default_vaccine_type))]
  miss_dets <- es.data_all_dets[!(es.data_all_dets %in% names(default_detections))]


  if (length(miss_vaccine > 0)) {
    warning_message <- paste0(
      "There are values of vaccine.types missing from default. ",
      "Please pass a named list with the vaccine types to the vaccine_types parameter and try again.",
      "Missing types are listed below:\n"
    )

    cli::cli_alert_warning(warning_message)
    print(miss_vaccine)
  }

  if (length(miss_dets) > 0) {
    warning_message <- paste0(
      "There are values of all_dets missing from default. ",
      "Please pass a named list with the detection types to the detection_types parameter and try again.",
      "Missing types are listed below:\n"
    )

    cli::cli_alert_warning(warning_message)
    print(miss_vaccine)
  }



  ## 22.1 ES sites & detection (es.site.det)

  new.site <- es.data %>%
    dplyr::filter(early.dat >= min(collect.date) &
      early.dat <= max(collect.date)) %>%
    dplyr::distinct(site.name, early.dat)


  minny <- min(es.data$collect.date) - 7
  maxy <- max(es.data$collect.date) + 7

  es.site.det <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = es.data |>
        dplyr::arrange(ADM1_NAME),
      ggplot2::aes(x = collect.date, y = site.name, col = desc(all_dets)),
      pch = 19,
      size = 3
    ) +
    ggplot2::geom_rect(
      data = minsy,
      ggplot2::aes(
        xmin = activity.start.date,
        xmax = activity.end.date,
        ymin = 0,
        ymax = Inf,
        fill = vaccine.type
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_point(
      data = es.data |>
        dplyr::arrange(ADM1_NAME),
      ggplot2::aes(x = collect.date, y = site.name, col = all_dets),
      pch = 19,
      size = 3
    ) +
    ggplot2::geom_point(
      data = es.data |>
        dplyr::arrange(ADM1_NAME),
      ggplot2::aes(x = collect.date, y = site.name),
      fill = NA,
      pch = 21,
      size = 3
    ) +
    ggplot2::xlab(label = "") +
    ggplot2::ylab(label = "Detection Sites") +
    ggplot2::scale_x_date(limits = c(es_start_date, es_end_date)) +
    ggplot2::scale_fill_manual(
      name = "SIAs",
      values = vaccine_types
    ) +
    ggplot2::scale_color_manual(
      name = "ES detections",
      values = detection_types
    ) +
    ggplot2::facet_grid(ADM1_NAME ~ .,
      scales = "free_y",
      space = "free",
      switch = "y",
      labeller = ggplot2::label_wrap_gen(8)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text.y.left = ggplot2::element_text(angle = 0))

  ggplot2::ggsave(
    "es.site.det.png",
    plot = es.site.det,
    path = output_path,
    width = 14,
    height = 8
  )

  return(es.site.det)
}

#' ES timeliness scatterplot
#'
#' Generates a scatterplot of the time it takes for each environmental samples to arrive in lab.
#'
#' @param es.data `tibble` ES data.
#' @param es_start_date `str` Start date of analysis. By default, this is one year from the end date.
#' @param es_end_date `str` End date of analysis.
#' @param output_path `str` Local path for where to save the figure to.
#'
#' @returns `ggplot` A scatterplot for timeliness of ES samples.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' generate_es_timely(ctry.data$es)
#' }
#'
#' @export
generate_es_timely <- function(es.data,
                               es_start_date = (lubridate::as_date(es_end_date) - lubridate::years(1)),
                               es_end_date = end_date,
                               output_path = Sys.getenv("DR_FIGURE_PATH")) {
  es_start_date <- lubridate::as_date(es_start_date)
  es_end_date <- lubridate::as_date(es_end_date)

  es.data <- es.data |>
    dplyr::filter(dplyr::between(collect.date, es_start_date, es_end_date))

  if (nrow(es.data) == 0) {
    return(output_empty_image(output_path, "es.timely.png"))
  }

  es.data$timely <-
    difftime(
      as.Date(es.data$date.received.in.lab, format = "%d/%m/%Y"),
      es.data$collect.date,
      units = "days"
    )

  per.time <- es.data %>%
    dplyr::count(timely > 3) %>%
    dplyr::rename(c("timely" = "timely > 3", "n" = "n"))
  # The number that are false are the percentage timely

  per.timely.title <- paste0(
    round(
      100 * dplyr::filter(per.time, timely == FALSE)["n"] / sum(per.time$n),
      0
    ),
    "% of samples arrived in lab within 3 days of collection - \n",
    format(es_start_date, "%B %Y"),
    " - ",
    format(es_end_date, "%B %Y")
  )

  miss.samp <- dplyr::filter(per.time, is.na(timely))

  num.miss.capt <- paste0(
    ifelse(dim(miss.samp)[1] == 0, 0, (miss.samp["n"])),
    " (",
    round(100 * as.numeric(ifelse(
      dim(miss.samp)[1] == 0, 0, (miss.samp["n"])
    )) / sum(per.time$n), 0),
    "%) samples were missing date information"
  )

  # Timeliness of ES
  # Excludes those with bad data (e.g. negative timeliness)
  es.timely <- ggplot2::ggplot() +
    ggplot2::geom_hline(
      yintercept = 3,
      color = "dark gray",
      linetype = "dashed",
      lwd = 1
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(es.data, timely >= 0),
      ggplot2::aes(x = collect.date, y = timely, color = site.name),
      alpha = 0.7,
      position = ggplot2::position_jitter(height = .2, width = 0.5),
      size = 3
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::number_format(accuracy = 1),
      breaks = c(seq(0, max(
        pretty(es.data$timely)
      ), 6))
    ) +
    ggplot2::labs(x = "Date of collection", y = "Transport time to lab (days)", color = "Site Name") +
    ggplot2::labs(title = per.timely.title, caption = num.miss.capt) +
    # ggplot2::scale_x_date(date_breaks = "2 months", date_labels = "%b-%y", limits = c(start.date.12m, end.date + months(1))) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 16),
      axis.text = ggplot2::element_text(size = 14),
      plot.caption = ggplot2::element_text(hjust = 0)
    )

  ggplot2::ggsave("es.timely.png",
    plot = es.timely,
    path = output_path,
    width = 14,
    height = 8
  )
  return(es.timely)
}

#' Immunization rates per year
#'
#' Generates a stacked percent bar plot displaying immunization rates per year for the country.
#' Note that this function only graphs immunization rates for children aged 6-59 months that have
#' the classification of NPAFP.
#'
#' @param ctry.data `list` A large list containing polio data of country.
#' This is the output of [extract_country_data()] or [init_dr()]. Note that `ctry_data` needs to be cleaned
#' via [clean_ctry_data()] prior to running the function.
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis.
#' @param output_path `str` Local path of where to save the figure to.
#'
#' @returns `ggplot` A percent bar plot displaying immunization rates per year by immunization status.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' ctry.data <- clean_ctry_data(ctry.data)
#' generate_case_num_dose_g(ctry.data, "2021-01-01", "2023-12-31")
#' }
#'
#' @export
generate_case_num_dose_g <- function(ctry.data,
                                     start_date,
                                     end_date,
                                     output_path = Sys.getenv("DR_FIGURE_PATH")) {
  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    stop(
      'Package "ggpubr" must be installed to use this function.',
      call. = FALSE
    )
  }

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  dose.num.cols <- c(
    "0" = "#C00000",
    "1-2" = "#FFC000",
    "3" = "#92D050",
    "4+" = "#548235",
    "Missing" = "#A5A5A5"
  )

  ### Create zero dose graphs
  # Cats - 0, 1-2, 3, 4+
  dcat.yr.prov <- ctry.data$afp.all.2 |>
    dplyr::filter(
      dplyr::between(date, start_date, end_date),
      cdc.classification.all2 == "NPAFP",
      dplyr::between(age.months, 6, 59)
    ) |>
    dplyr::group_by(dose.cat, year, prov) |>
    dplyr::summarise(freq = dplyr::n())

  if (nrow(dcat.yr.prov) == 0) {
    return(output_empty_image(output_path, "case.num.dose.g.png"))
  }

  # case num by year and province by vaccination status
  case.num.dose.g <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = dcat.yr.prov,
      ggplot2::aes(x = year, y = freq, fill = dose.cat),
      stat = "identity",
      position = "fill"
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("Percent of Cases") +
    ggplot2::scale_fill_manual("Number of doses - IPV/OPV",
      values = dose.num.cols,
      drop = F
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(title = "OPV/IPV Status of NP AFP cases, 6-59 months") +
    ggplot2::geom_text(
      data = dcat.yr.prov |> dplyr::group_by(year) |> dplyr::summarize(freq = sum(freq)),
      ggplot2::aes(
        label = paste0("n = ", freq),
        x = year,
        y = 1.02
      ),
      check_overlap = TRUE
    ) +
    ggpubr::theme_pubr()

  ggplot2::ggsave(
    "case.num.dose.g.png",
    plot = case.num.dose.g,
    path = output_path,
    width = 9,
    height = 8
  )

  return(case.num.dose.g)
}


#' Visits to health clinics per year
#'
#' Generates a bar plot showing the number of visits to health clinics per year
#' using the ISS/eSURV data.
#'
#' @param iss.data `tibble` ISS/eSURV data that has been cleaned via [clean_iss_data()].
#' @param start_date `str` Start date of the analysis.
#' @param end_date `str` End date of the analysis.
#' @param output_path `str` Local path where the figure is saved to.
#'
#' @returns `ggplot` Bar plot of health clinic visits.
#' @examples
#' \dontrun{
#' iss_path <- "C:/Users/ABC1/Desktop/iss_data.csv"
#' ctry.data <- init_dr("algeria", iss_data_path = iss_path)
#' ctry.data$iss.data <- clean_iss_data(ctry.data)
#' generate_iss_barplot(ctry.data$iss.data)
#' }
#'
#' @export
generate_iss_barplot <- function(iss.data = NULL,
                                 start_date,
                                 end_date,
                                 output_path = Sys.getenv("DR_FIGURE_PATH")) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  if (is.null(iss.data)) {
    return(message("No ISS data attached."))
  }

  if (!"today_date" %in% names(iss.data)) {
    iss.data <- iss.data |> dplyr::mutate(today_date = today)
  }


  iss.data2.1 <- iss.data %>%
    dplyr::filter(dplyr::between(today_date, start_date, end_date))

  iss.data3 <- iss.data2.1 %>%
    dplyr::group_by(month, year, priority_level) %>%
    dplyr::summarize(freq = dplyr::n()) %>%
    dplyr::filter(dplyr::between(year, lubridate::year(start_date), lubridate::year(end_date)))

  if (nrow(iss.data3) == 0) {
    return(output_empty_image(output_path, "iss.barplot.png"))
  }

  iss.data3$labs <- month.abb[iss.data3$month] %>%
    factor(
      .,
      levels = c(
        "Jan",
        "Feb",
        "Mar",
        "Apr",
        "May",
        "Jun",
        "Jul",
        "Aug",
        "Sep",
        "Oct",
        "Nov",
        "Dec"
      )
    )
  iss.data3 <- iss.data3 |>
    dplyr::filter(dplyr::between(year, lubridate::year(start_date), lubridate::year(end_date)))

  totty <- iss.data3 %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarize(totty = sum(freq))

  mtot <- max(totty$totty)

  iss.data.vis <- ggplot2::ggplot(data = iss.data3) +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = factor(labs),
        y = freq,
        fill = priority_level
      ),
      stat = "identity",
      position = "stack",
      col = "black"
    ) +
    ggplot2::scale_y_continuous(
      name = "Visit Number",
      limits = c(0, max(pretty(mtot))),
      breaks = seq(0, max(pretty(mtot)), max(pretty(mtot)) /
        5),
      labels = seq(0, max(pretty(mtot)), max(pretty(mtot)) /
        5)
    ) +
    ggplot2::scale_x_discrete(name = "Time") +
    ggplot2::scale_fill_manual(
      name = "Priority",
      values = c(
        "High" = "#d73027",
        "Medium" = "#fdae61",
        "Low" = "#4575b4",
        "Not Focal Site" = "#878787"
      )
    ) +
    ggplot2::facet_wrap(~year) +
    ggplot2::theme_bw()


  ggplot2::ggsave(
    "iss.barplot.png",
    plot = iss.data.vis,
    path = output_path,
    width = 14,
    height = 8
  )

  return(iss.data.vis)
}

## Maps ----
#' Country map with province populations
#'
#' The map displays the U15 population for each province for a country.
#'
#' @param ctry.data `list` Large list containing country polio data. This is the
#' output of [extract_country_data()] or [init_dr()].
#' @param ctry.shape `sf` Country shape file in long format.
#' @param prov.shape `sf` Province shape file in long format.
#' @param end_date `str` End date of the analysis.
#' @param output_path `str` Local path where to save the figure.
#' @param caption_size `numeric` Size of the caption. Default is `11`.
#'
#' @returns `ggplot` A map of U15 province populations and population centers.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' ctry.shape <- load_clean_ctry_sp(ctry_name = "ALGERIA", type = "long")
#' prov.shape <- load_clean_prov_sp(ctry_name = "ALGERIA", type = "long")
#' generate_pop_map(ctry.data, ctry.shape, prov.shape, "2023-12-31")
#' }
#' @seealso [load_clean_ctry_sp()], [load_clean_prov_sp()]
#' @export
generate_pop_map <- function(ctry.data,
                             ctry.shape,
                             prov.shape,
                             end_date,
                             output_path = Sys.getenv("DR_FIGURE_PATH"),
                             caption_size = 11) {
  end_date <- lubridate::as_date(end_date)

  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop(
      'Package "ggrepel" must be installed to use this function.',
      call. = FALSE
    )
  }

  if (!"active.year.01" %in% names(ctry.shape)) {
    error_message <- paste0(
      "ctry.shape is not in long format. ",
      "Use: ctry.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the country shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  if (!"active.year.01" %in% names(prov.shape)) {
    error_message <- paste0(
      "prov.shape is not in long format. ",
      "Use: prov.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the province shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  # Only the most recent shape file
  ctry.shape <- ctry.shape |>
    dplyr::filter(active.year.01 == lubridate::year(end_date)) |>
    dplyr::mutate(year = active.year.01)
  prov.shape <- prov.shape |>
    dplyr::filter(active.year.01 == lubridate::year(end_date)) |>
    dplyr::mutate(year = active.year.01)

  # Merge with province
  shape.prov.pop <-
    dplyr::left_join(prov.shape, ctry.data$prov.pop, by = c("GUID" = "adm1guid", "year")) |>
    dplyr::filter(year == lubridate::year(end_date))

  if (nrow(shape.prov.pop) == 0) {
    return(output_empty_image(output_path, "pop.map.png"))
  }

  pop.map <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ctry.data$ctry,
      color = "black",
      fill = NA,
      size = 1
    ) +
    ggplot2::geom_sf(data = shape.prov.pop, ggplot2::aes(fill = u15pop)) +
    ggplot2::geom_sf(data = sf::st_crop(ctry.data$roads, ctry.data$ctry)) +
    ggplot2::geom_sf(
      data = dplyr::filter(ctry.data$cities, toupper(CNTRY_NAME) == ctry.data$name),
      size = 3,
      color = "blue"
    ) +
    ggrepel::geom_label_repel(
      data = dplyr::filter(ctry.data$cities, toupper(CNTRY_NAME) == ctry.data$name),
      ggplot2::aes(label = CITY_NAME, geometry = geometry),
      stat = "sf_coordinates"
    ) +
    ggplot2::scale_fill_distiller(
      palette = "YlOrRd",
      direction = 1,
      labels = scales::comma
    ) +
    ggplot2::ggtitle(paste0(
      "Major Cities and Roads - Province Level Population - ",
      lubridate::year(end_date)
    )) +
    ggplot2::labs(
      fill = "Under-15 pop",
      caption = "- Under 15 population is shown at the province level\n- Major roads are shown in black\n- Population centers are shown in blue"
    ) +
    sirfunctions::f.plot.looks("epicurve") +
    ggplot2::scale_size_identity() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 15,
        face = "bold",
        hjust = 0.5
      ),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "right",
      plot.caption = ggplot2::element_text(hjust = 0, size = caption_size),
      legend.background = ggplot2::element_blank(),
    )

  ggplot2::ggsave("pop.map.png",
    plot = pop.map,
    path = output_path,
    width = 8,
    height = 8
  )
  return(pop.map)
}

#' Map district U15 populations
#'
#' Generates a map of U15 district populations, with population centers and roads.
#'
#' @param ctry.data `list` Large list of polio country data. This is the output of
#' either [extract_country_data()] or [init_dr()].
#' @param ctry.shape `sf` Shapefile of country in long format.
#' @param prov.shape `sf` Shapefile of province in long format.
#' @param dist.shape `sf` Shapefile of district in long format.
#' @param end_date `str` End date of the analysis.
#' @param output_path `str` Local path of where to save the figure.
#' @param caption_size `numeric` Size of the caption. Default is `11`.
#'
#' @returns `ggplot` A map of district level populations and population centers.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' ctry.shape <- load_clean_ctry_sp(ctry_name = "ALGERIA", type = "long")
#' prov.shape <- load_clean_prov_sp(ctry_name = "ALGERIA", type = "long")
#' dist.shape <- load_clean_dist_sp(ctry_name = "ALGERIA", type = "long")
#' generate_pop_map(ctry.data, ctry.shape, prov.shape, dist.shape, "2023-12-31")
#' }
#' @seealso [load_clean_ctry_sp()], [load_clean_prov_sp()], [load_clean_dist_sp()]
#'
#' @export
generate_dist_pop_map <- function(ctry.data,
                                  ctry.shape,
                                  prov.shape,
                                  dist.shape,
                                  end_date,
                                  output_path = Sys.getenv("DR_FIGURE_PATH"),
                                  caption_size = 11) {
  end_date <- lubridate::as_date(end_date)

  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop(
      'Package "ggrepel" must be installed to use this function.',
      call. = FALSE
    )
  }

  if (!"active.year.01" %in% names(ctry.shape)) {
    error_message <- paste0(
      "ctry.shape is not in long format. ",
      "Use: ctry.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the country shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  if (!"active.year.01" %in% names(prov.shape)) {
    error_message <- paste0(
      "prov.shape is not in long format. ",
      "Use: prov.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the province shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  if (!"active.year.01" %in% names(dist.shape)) {
    error_message <- paste0(
      "dist.shape is not in long format. ",
      "Use: dist.shape <- load_clean_dist_sp(ctry_name='<ctry name>', type='long')",
      " to download the province shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  ctry.shape <- ctry.shape |>
    dplyr::filter(active.year.01 == lubridate::year(end_date)) |>
    dplyr::mutate(year = active.year.01)
  prov.shape <- prov.shape |>
    dplyr::filter(active.year.01 == lubridate::year(end_date)) |>
    dplyr::mutate(year = active.year.01)
  dist.shape <- dist.shape |>
    dplyr::filter(active.year.01 == lubridate::year(end_date)) |>
    dplyr::mutate(year = active.year.01)

  shape.prov.pop <-
    dplyr::left_join(prov.shape, ctry.data$prov.pop, by = c("GUID" = "adm1guid", "year")) |>
    dplyr::filter(year == lubridate::year(end_date))

  shape.dist.pop <-
    dplyr::left_join(dist.shape, ctry.data$dist.pop, by = c("GUID" = "adm2guid", "year")) |>
    dplyr::filter(year == lubridate::year(end_date))

  if (nrow(shape.dist.pop) == 0) {
    return(output_empty_image(output_path, "pop.map.prov.png"))
  }

  pop.map.provn <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    ggplot2::geom_sf(data = shape.dist.pop, ggplot2::aes(fill = u15pop), color = NA) +
    ggplot2::geom_sf(
      data = prov.shape,
      color = "black",
      fill = NA
    ) +
    ggrepel::geom_label_repel(
      data = shape.prov.pop,
      ggplot2::aes(label = ADM1_NAME, geometry = Shape),
      stat = "sf_coordinates",
      force = 80
    ) +
    ggplot2::scale_fill_distiller(
      palette = "YlOrRd",
      direction = "both",
      labels = scales::comma
    ) +
    ggplot2::ggtitle(paste0("Province Names - District Level Population - ", lubridate::year(end_date))) +
    ggplot2::labs(fill = "Under-15 pop") +
    sirfunctions::f.plot.looks("epicurve") +
    ggplot2::scale_size_identity() +
    ggplot2::labs(caption = "- Under 15 population is shown at the district level\n- Labels are province names\n- Black lines are province borders") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 15,
        face = "bold",
        hjust = 0.5
      ),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "right",
      plot.caption = ggplot2::element_text(hjust = 0, size = caption_size),
      legend.background = ggplot2::element_blank()
    )

  ggplot2::ggsave(
    "pop.map.prov.png",
    plot = pop.map.provn,
    path = output_path,
    width = 8,
    height = 8
  )
  return(pop.map.provn)
}

#' AFP case map
#'
#' Generates a map of AFP cases, excluding any with pending classification.
#'
#' @param afp.all `sf` AFP linelist containing point geometry. This is
#' `ctry.data$afp.all`, which is an output of either [extract_country_data()] and
#' [init_dr()].
#' @param ctry.shape `sf` Country shapefile in long format.
#' @param prov.shape `sf` Province shapefile in long format.
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis. Default is today's date.
#' @param output_path `str` Local path where to save the figure to.
#'
#' @returns `ggplot` Map of AFP cases.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' ctry.shape <- load_clean_ctry_sp(ctry_name = "ALGERIA", type = "long")
#' prov.shape <- load_clean_prov_sp(ctry_name = "ALGERIA", type = "long")
#' generate_afp_case_map(ctry.data, ctry.shape, prov.shape, "2023-12-31")
#' }
#' @seealso [load_clean_ctry_sp()], [load_clean_prov_sp()]
#' @export
generate_afp_case_map <- function(afp.all,
                                  ctry.shape,
                                  prov.shape,
                                  start_date,
                                  end_date = lubridate::today(),
                                  output_path = Sys.getenv("DR_FIGURE_PATH")) {

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  para_colors <- sirfunctions::f.color.schemes("para.case")

  if (!"active.year.01" %in% names(ctry.shape)) {
    error_message <- paste0(
      "ctry.shape is not in long format. ",
      "Use: ctry.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the country shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  if (!"active.year.01" %in% names(prov.shape)) {
    error_message <- paste0(
      "prov.shape is not in long format. ",
      "Use: prov.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the province shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  afp.case.map.filter <- afp.all |>
    dplyr::filter(dplyr::between(as.Date(date.onset), start_date, end_date)) |>
    dplyr::filter(cdc.classification.all2 %in% names(f.color.schemes("para.case")) |
                    stringr::str_count(cdc.classification.all2,
                                       paste("WILD 1", "cVDPV 2", "VDPV 2",
                                             "cVDPV 1", "VDPV 1", "cVDPV1",
                                             "cVDPV2", "VDPV1", "VDPV2",
                                             "Wild1", sep = "|")) >= 2)

  # Add color schemes in instances of coinfections
  no_col_maps <- setdiff(unique(afp.case.map.filter$cdc.classification.all2),
                         names(para_colors))

  if (length(no_col_maps) > 0) {
    for (i in no_col_maps) {
      para_colors[i] = "purple"
    }
  }

  if (nrow(afp.case.map.filter) == 0) {
    return(output_empty_image(output_path, "afp.case.map.png"))
  }

  if (min(afp.case.map.filter$year) > year(start_date)) {
    cli::cli_alert_warning(paste0(
      "Start year of ", year(start_date), " specified. However, filtered cases only ",
      "start from ", min(afp.case.map.filter$year), ". Maps will not display ",
      "the specified start year."
    ))
  }

  if (min(prov.shape$active.year.01) > year(start_date)) {
    cli::cli_alert_warning(paste0(
      "Start year of ", year(start_date), " specified. However, the province shapefile ",
      "starts at ", min(prov.shape$active.year.01), ". Maps will not display ",
      "the specified start year."
    ))
  }

  if (max(afp.case.map.filter$year) < year(end_date)) {
    cli::cli_alert_warning(paste0(
      "End year of ", year(end_date), " specified. However, filtered cases only ",
      "goes up to ", max(afp.case.map.filter$year), ". Maps will not display ",
      "the specified end year."
    ))
  }

  if (max(prov.shape$active.year.01) < year(end_date)) {
    cli::cli_alert_warning(paste0(
      "End year of ", year(end_date), " specified. However, the province shapefile",
      "goes up to ", max(prov.shape$active.year.01), ". Maps will not display ",
      "the specified end year."
    ))
  }

  ctry.shape <- ctry.shape |>
    dplyr::filter(dplyr::between(
      active.year.01,
      lubridate::year(start_date),
      lubridate::year(end_date)
    )) |>
    dplyr::mutate(year = active.year.01) |>
    dplyr::filter(year <= max(afp.case.map.filter$year))

  prov.shape <- prov.shape |>
    dplyr::filter(dplyr::between(
      active.year.01,
      lubridate::year(start_date),
      lubridate::year(end_date)
    )) |>
    dplyr::mutate(year = active.year.01) |>
    dplyr::filter(year <= max(afp.case.map.filter$year))

  afp.case.map.filter <- afp.case.map.filter |>
    dplyr::mutate(year = as.factor(year))

  afp.case.map <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ctry.shape,
      ggplot2::aes(),
      color = "black",
      fill = NA,
      size = 1
    ) +
    ggplot2::geom_sf(
      data = prov.shape,
      ggplot2::aes(),
      color = "black",
      fill = NA,
      size = .5
    ) +
    ggplot2::geom_sf(
      data = afp.case.map.filter,
      ggplot2::aes(color = cdc.classification.all2),
      size = 1
    ) +
    ggplot2::facet_wrap(ggplot2::vars(year),
                        ncol = length(unique(afp.case.map.filter$year))
                        ) +
    ggplot2::scale_color_manual(
      values = para_colors,
      name = "Case type",
      drop = F
    ) +
    ggplot2::ggtitle(paste(
      "Paralytic Polio and Compatible Cases",
      min(as.numeric(as.character(afp.case.map.filter$year)), na.rm = TRUE),
      "-",
      max(as.numeric(as.character(afp.case.map.filter$year)), na.rm = TRUE)
    )) +
    # NOTE: IF THERE ARE NONE IT NEEDS TO THROW AN ERROR
    sirfunctions::f.plot.looks("epicurve") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  ggplot2::ggsave(
    "afp.case.map.png",
    plot = afp.case.map,
    path = output_path,
    width = 14,
    height = 4
  )

  return(afp.case.map)
}

#' Maps of NPAFP rates by province and year
#'
#' Generates a map of NPAFP rates for each province per year.
#'
#' @param prov.extract `tibble` Province NPAFP rate table. This is the output of
#' [f.npafp.rate.01()] calculated at the province level.
#' @param ctry.shape `sf` Country shape in long format.
#' @param prov.shape `sf` Province shape in long format.
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis.
#' @param output_path `str` Local path where the figure is saved to.
#' @param caption_size `numeric` Size of the caption. Default is `2`.
#'
#' @returns `ggplot` Map of NPAFP rates by province.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' prov.extract <- f.npafp.rate.01(
#'   afp.data = ctry.data$afp.all.2,
#'   pop.data = ctry.data$prov.pop,
#'   start.date = start_date,
#'   end.date = end_date,
#'   spatial.scale = "prov",
#'   pending = T,
#'   rolling = F,
#'   sp_continuity_validation = F
#' )
#' ctry.shape <- load_clean_ctry_sp(ctry_name = "ALGERIA", type = "long")
#' prov.shape <- load_clean_prov_sp(ctry_name = "ALGERIA", type = "long")
#' generate_npafp_maps(prov.extract, ctry.shape, prov.shape, "2021-01-01", "2023-12-31")
#' }
#' @seealso [load_clean_ctry_sp()], [load_clean_prov_sp()]
#' @export
generate_npafp_maps <- function(prov.extract,
                                ctry.shape,
                                prov.shape,
                                start_date,
                                end_date,
                                output_path = Sys.getenv("DR_FIGURE_PATH"),
                                caption_size = 2) {
  if (!"active.year.01" %in% names(ctry.shape)) {
    error_message <- paste0(
      "ctry.shape is not in long format. ",
      "Use: ctry.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the country shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  if (!"active.year.01" %in% names(prov.shape)) {
    error_message <- paste0(
      "prov.shape is not in long format. ",
      "Use: prov.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the province shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  ctry.shape <- ctry.shape |>
    dplyr::filter(dplyr::between(
      active.year.01,
      lubridate::year(start_date),
      lubridate::year(end_date)
    )) |>
    dplyr::mutate(year = active.year.01)
  prov.shape <- prov.shape |>
    dplyr::filter(dplyr::between(
      active.year.01,
      lubridate::year(start_date),
      lubridate::year(end_date)
    )) |>
    dplyr::mutate(year = active.year.01)

  provnpafp <- prov.extract |>
    filter(!is.na(prov))

  if (nrow(provnpafp) == 0) {
    return(output_empty_image(output_path, "npafp.map.png"))
  }

  provnpafp$cats <- cut(
    provnpafp$npafp_rate,
    breaks = c(-1, 0, 1, 2, 3, Inf),
    right = F,
    labels = c("Zero NPAFP cases", "<1", "1-<2", "2-<3", "3+")
  )

  prov.cut <- provnpafp %>%
    dplyr::mutate(cats = as.character(cats)) %>%
    dplyr::mutate(
      cats = dplyr::case_when(
        npafp_rate == 0 & u15pop >= 100000 ~ "Silent (u15pop >= 100K)",
        npafp_rate == 0 &
          u15pop < 100000 & u15pop > 0 ~ "No cases (u15pop < 100K)",
        npafp_rate == 0 & u15pop == 0 ~ "Missing Pop",
        T ~ cats
      )
    ) %>%
    dplyr::filter(year >= lubridate::year(start_date) & year <= lubridate::year(end_date))

  prov.cut$cats <- factor(
    prov.cut$cats,
    levels = c(
      "<1" = "<1",
      "1-<2" = "1-<2",
      "2-<3" = "2-<3",
      "3+" = "3+",
      "Missing Pop" = "Missing Pop",
      "No cases (u15pop < 100K)" = "No cases (u15pop < 100K)",
      "Silent (u15pop >= 100K)" = "Silent (u15pop >= 100K)"
    )
  )

  prov.cut <- dplyr::ungroup(prov.cut)

  # For those with no cases --> separate out districts with u15pop >100K and <100K

  prov.pop.case.npafp <-
    dplyr::left_join(prov.shape, prov.cut,
      by = c("GUID" = "adm1guid", "year" = "year")
    ) |>
    dplyr::filter(dplyr::between(
      year,
      lubridate::year(start_date),
      lubridate::year(end_date)
    ))

  # Labels for provinces meeting NPAFP rate
  # How many provinces meet >2 NPAFP?
  prov.2npafp <- provnpafp %>%
    dplyr::group_by(year, adm1guid, prov) %>%
    dplyr::summarize(meet2 = sum(npafp_rate >= 2, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
      num.meet2 = sum(meet2, na.rm = T),
      len.year = length(year)
    ) %>%
    dplyr::mutate(
      labs = paste0(
        num.meet2,
        "/",
        len.year,
        " (",
        round(100 * num.meet2 / len.year, 0),
        "%)",
        " provinces with >= 2 cases of NPAFP \nper 100,000 population"
      )
    )


  # Get coordinates for maps that are plotted
  ctcoord <- as.data.frame(sf::st_coordinates(ctry.shape))
  # Put text at 10% below the minimum X and Y coords for each map
  adjy <- (range(ctcoord$Y)[1] - range(ctcoord$Y)[2]) * .1


  npafp.maps <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    ggplot2::geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    ggplot2::geom_sf(
      data = prov.pop.case.npafp,
      color = "black",
      ggplot2::aes(fill = cats),
      show.legend = T
    ) +
    ggplot2::geom_text(
      data = prov.2npafp,
      ggplot2::aes(
        x = min(ctcoord$X),
        y = min(ctcoord$Y) + adjy,
        label = labs
      ),
      size = caption_size,
      check_overlap = TRUE,
      hjust = 0,
    ) +
    ggplot2::scale_fill_manual(
      name = "NPAFP rate",
      values = c(
        "No cases (u15pop < 100K)" = "lightgrey",
        "<1" = "#dc582a",
        "1-<2" = "#fdae61",
        "2-<3" = "#a6d96a",
        "3+" = "#1a9641",
        "Missing Pop" = "#2C83C7",
        "Silent (u15pop >= 100K)" = "#d7191c"
      ),
      drop = F
    ) +
    ggplot2::ggtitle("NPAFP Rate Annualized - Province") +
    sirfunctions::f.plot.looks("epicurve") +
    ggplot2::facet_wrap(~year, ncol = 4) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position.inside = c(0.5, -0.23),
      legend.direction = "horizontal"
    )

  ggplot2::ggsave("npafp.map.png",
    plot = npafp.maps,
    path = output_path,
    width = 14,
    height = 8
  )
  return(npafp.maps)
}

#' Maps of NPAFP rates by district and year
#'
#' Generates maps of the NPAFP rates for each district per year.
#'
#' @param dist.extract `tibble` Province NPAFP rate table. This is the output of
#' [f.npafp.rate.01()] calculated at the province level.
#' @param ctry.shape `sf` Country shapefile in long format.
#' @param prov.shape `sf` Province shapefile in long format.
#' @param dist.shape `sf` District shapefile in long format.
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis.
#' @param output_path `str` Local path Where the figure is saved to.
#' @param caption_size `numeric` Size of the caption. Default is `2`.
#'
#' @returns `ggplot` A map of districts with their NPAFP rates.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' dist.extract <- f.npafp.rate.01(
#'   afp.data = ctry.data$afp.all.2,
#'   pop.data = ctry.data$prov.pop,
#'   start.date = start_date,
#'   end.date = end_date,
#'   spatial.scale = "dist",
#'   pending = T,
#'   rolling = F,
#'   sp_continuity_validation = F
#' )
#' ctry.shape <- load_clean_ctry_sp(ctry_name = "ALGERIA", type = "long")
#' prov.shape <- load_clean_prov_sp(ctry_name = "ALGERIA", type = "long")
#' dist.shape <- load_clean_dist_sp(ctry_name = "ALGERIA", type = "long")
#' generate_npafp_maps_dist(
#'   dist.extract, ctry.shape, prov.shape, dist.shape,
#'   "2021-01-01", "2023-12-31"
#' )
#' }
#'
#' @export
generate_npafp_maps_dist <- function(dist.extract,
                                     ctry.shape,
                                     prov.shape,
                                     dist.shape,
                                     start_date,
                                     end_date,
                                     output_path = Sys.getenv("DR_FIGURE_PATH"),
                                     caption_size = 2) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  if (!"active.year.01" %in% names(ctry.shape)) {
    error_message <- paste0(
      "ctry.shape is not in long format. ",
      "Use: ctry.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the country shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  if (!"active.year.01" %in% names(prov.shape)) {
    error_message <- paste0(
      "prov.shape is not in long format. ",
      "Use: prov.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the province shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  if (!"active.year.01" %in% names(dist.shape)) {
    error_message <- paste0(
      "dist.shape is not in long format. ",
      "Use: dist.shape <- load_clean_dist_sp(ctry_name='<ctry name>', type='long')",
      " to download the province shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  ctry.shape <- ctry.shape |>
    dplyr::filter(dplyr::between(
      active.year.01,
      lubridate::year(start_date),
      lubridate::year(end_date)
    )) |>
    dplyr::mutate(year = active.year.01)
  prov.shape <- prov.shape |>
    dplyr::filter(dplyr::between(
      active.year.01,
      lubridate::year(start_date),
      lubridate::year(end_date)
    )) |>
    dplyr::mutate(year = active.year.01)
  dist.shape <- dist.shape |>
    dplyr::filter(dplyr::between(
      active.year.01,
      lubridate::year(start_date),
      lubridate::year(end_date)
    )) |>
    dplyr::mutate(year = active.year.01)

  distnpafp <- dist.extract |>
    filter(!is.na(dist))

  if (nrow(distnpafp) == 0) {
    return(output_empty_image(output_path, "npafp.map.dist.png"))
  }

  distnpafp$cats <- cut(
    distnpafp$npafp_rate,
    breaks = c(-1, 0, 1, 2, 3, Inf),
    right = F,
    labels = c("Zero NPAFP cases", "<1", "1-<2", "2-<3", "3+")
  )

  # For those with no cases --> separate out districts with u15pop >100K and <100K

  dist.cut <- distnpafp %>%
    dplyr::mutate(cats = as.character(cats)) %>%
    dplyr::mutate(
      cats = dplyr::case_when(
        npafp_rate == 0 & u15pop >= 100000 ~ "Silent (u15pop >= 100K)",
        npafp_rate == 0 &
          u15pop < 100000 & u15pop > 0 ~ "No cases (u15pop < 100K)",
        npafp_rate == 0 & u15pop == 0 ~ "Missing Pop",
        T ~ cats
      )
    ) |>
    dplyr::filter(dplyr::between(
      year,
      lubridate::year(start_date),
      lubridate::year(end_date)
    ))


  dist.cut$cats <- factor(
    dist.cut$cats,
    levels = c(
      "<1" = "<1",
      "1-<2" = "1-<2",
      "2-<3" = "2-<3",
      "3+" = "3+",
      "Missing Pop" = "Missing Pop",
      "No cases (u15pop < 100K)" = "No cases (u15pop < 100K)",
      "Silent (u15pop >= 100K)" = "Silent (u15pop >= 100K)"
    )
  )

  dist.cut <- dplyr::ungroup(dist.cut)

  dist.pop.case.npafp <-
    dplyr::left_join(dist.shape, dist.cut, by = c("GUID" = "adm2guid", "year" = "year"))

  # How many districts meet >2 NPAFP?
  dist.2npafp <- distnpafp %>%
    dplyr::group_by(year, adm2guid, dist) %>%
    dplyr::summarize(meet2 = sum(npafp_rate >= 2, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
      num.meet2 = sum(meet2, na.rm = T),
      len.year = length(year)
    ) %>%
    dplyr::mutate(
      labs = paste0(
        num.meet2,
        "/",
        len.year,
        " (",
        round(100 * num.meet2 / len.year, 0),
        "%)",
        " districts with >= 2 cases of NPAFP \nper 100,000 population"
      )
    )


  # Get coordinates for maps that are plotted
  ctcoord <- as.data.frame(sf::st_coordinates(ctry.shape))
  # Put text at 10% below the minimum X and Y coords for each map
  adjy <- (range(ctcoord$Y)[1] - range(ctcoord$Y)[2]) * .1

  npafp.maps.dist <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    ggplot2::geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    ggplot2::geom_sf(
      data = dist.pop.case.npafp,
      color = "black",
      ggplot2::aes(fill = cats),
      show.legend = T
    ) +
    ggplot2::geom_text(
      data = dist.2npafp,
      ggplot2::aes(
        x = min(ctcoord$X),
        y = min(ctcoord$Y) + adjy,
        label = labs
      ),
      size = caption_size,
      check_overlap = TRUE,
      hjust = 0
    ) +
    ggplot2::scale_fill_manual(
      name = "NPAFP rate",
      values = c(
        "No cases (u15pop < 100K)" = "lightgrey",
        "<1" = "#dc582a",
        "1-<2" = "#fdae61",
        "2-<3" = "#a6d96a",
        "3+" = "#1a9641",
        "Missing Pop" = "#2C83C7",
        "Silent (u15pop >= 100K)" = "#d7191c"
      ),
      drop = F
    ) +
    # ggplot2::scale_color_manual(values = sirfunctions::f.color.schemes("para.case"), name = "Case type",
    #                  drop = F) +
    ggplot2::ggtitle("NPAFP Rate Annualized - District") +
    sirfunctions::f.plot.looks("epicurve") +
    ggplot2::facet_wrap(~year, ncol = 4) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  ggplot2::ggsave(
    "npafp.map.dist.png",
    plot = npafp.maps.dist,
    path = output_path,
    width = 14,
    height = 8
  )

  return(npafp.maps.dist)
}


#' Stool adequacy maps by province
#'
#' Generates maps that contain the stool adequacy rate for each province per year.
#'
#' @param ctry.data `list` Large list containing polio data of a country. This is the output
#' of either [extract_country_data()] or [init_dr()].
#' @param pstool `tibble` Stool adequacy table at province level. This is the output of
#' [f.stool.ad.01()] calculated at the province level.
#' @param ctry.shape `sf` Country shapefile in long format.
#' @param prov.shape `sf` Province shapefile in long format.
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis.
#' @param output_path `str` Where to save the figure to.
#' @param caption_size `numeric` Size of the caption. Defaults to 3.
#'
#' @returns `ggplot` A map of stool adequacy rates for each province by year.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' pstool <- f.stool.ad.01(
#'   afp.data = ctry.data$afp.all.2,
#'   admin.data = ctry.data$prov.pop,
#'   start.date = start_date,
#'   end.date = end_date,
#'   spatial.scale = "prov",
#'   missing = "good",
#'   bad.data = "inadequate",
#'   rolling = F,
#'   sp_continuity_validation = F
#' )
#' ctry.shape <- load_clean_ctry_sp(ctry_name = "ALGERIA", type = "long")
#' prov.shape <- load_clean_prov_sp(ctry_name = "ALGERIA", type = "long")
#' generate_stool_ad_maps(ctry.data, pstool, ctry.shape, prov.shape, "2021-01-01", "2023-12-31")
#' }
#'
#' @export
generate_stool_ad_maps <- function(ctry.data,
                                   pstool,
                                   ctry.shape,
                                   prov.shape,
                                   start_date,
                                   end_date,
                                   output_path = Sys.getenv("DR_FIGURE_PATH"),
                                   caption_size = 3) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  if (!"active.year.01" %in% names(ctry.shape)) {
    error_message <- paste0(
      "ctry.shape is not in long format. ",
      "Use: ctry.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the country shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  if (!"active.year.01" %in% names(prov.shape)) {
    error_message <- paste0(
      "prov.shape is not in long format. ",
      "Use: prov.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the province shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  ctry.shape <- ctry.shape |>
    dplyr::filter(dplyr::between(
      active.year.01,
      lubridate::year(start_date),
      lubridate::year(end_date)
    )) |>
    dplyr::mutate(year = active.year.01)
  prov.shape <- prov.shape |>
    dplyr::filter(dplyr::between(
      active.year.01,
      lubridate::year(start_date),
      lubridate::year(end_date)
    )) |>
    dplyr::mutate(year = active.year.01)

  allafp <- ctry.data$afp.all.2 |>
    dplyr::filter(dplyr::between(date, start_date, end_date)) |>
    dplyr::reframe(
      dplyr::group_by(
        ctry.data$afp.all.2,
        cdc.classification.all2,
        adm1guid,
        year
      ),
      freq = dplyr::n()
    ) |>
    dplyr::filter(cdc.classification.all2 != "NOT-AFP")

  allprov <- ctry.data$prov.pop[, c("adm1guid", "year", "prov")] %>%
    dplyr::filter(dplyr::between(year, lubridate::year(start_date), lubridate::year(end_date)))

  all.prov.afp <- dplyr::left_join(allprov, allafp) %>%
    dplyr::group_by(year, adm1guid, prov) %>%
    dplyr::summarize(allafp = sum(freq, na.rm = T))

  stoolad.p <- dplyr::left_join(all.prov.afp,
    pstool,
    by = c(
      "prov" = "prov",
      "year" = "year",
      "adm1guid" = "adm1guid"
    )
  )

  if (nrow(stoolad.p) == 0) {
    return(output_empty_image(output_path, "stool.ad.maps.png"))
  }

  stoolad.p <- stoolad.p %>%
    filter(!is.na(prov)) %>%
    dplyr::tibble() %>%
    dplyr::mutate(
      prop.cat = dplyr::case_when(
        afp.cases == 0 ~ "Zero AFP cases",
        afp.cases != 0 & per.stool.ad < 40 ~ "<40%",
        afp.cases != 0 &
          per.stool.ad >= 40 & per.stool.ad < 60 ~ "40-59%",
        afp.cases != 0 &
          per.stool.ad >= 60 & per.stool.ad < 80 ~ "60-79%",
        afp.cases != 0 & per.stool.ad >= 80 ~ "80%+"
      )
    ) %>%
    dplyr::mutate(prop.cat = factor(
      prop.cat,
      levels = c("Zero AFP cases", "<40%", "40-59%", "60-79%", "80%+")
    ))

  stoolad.nums.p <- stoolad.p %>%
    dplyr::group_by(year, adm1guid, prov) %>%
    dplyr::summarize(meet.stool = sum(per.stool.ad >= 80, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
      num.meet.stool = sum(meet.stool, na.rm = T),
      len.year = length(year)
    ) %>%
    dplyr::mutate(
      labs = paste0(
        num.meet.stool,
        "/",
        len.year,
        " (",
        round(100 * num.meet.stool / len.year, 0),
        "%)",
        " provinces with >= 80% stool adequacy"
      )
    )

  stool.map.p <-
    dplyr::left_join(prov.shape, stoolad.p, by = c("GUID" = "adm1guid", "year" = "year"))

  # Get coordinates for maps that are plotted
  ctcoord <- as.data.frame(sf::st_coordinates(ctry.shape))
  # Put text at 10% below the minimum X and Y coords for each map
  adjy <- (range(ctcoord$Y)[1] - range(ctcoord$Y)[2]) * .1


  stool.ad.maps <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    ggplot2::geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    ggplot2::geom_sf(
      data = stool.map.p,
      color = "black",
      ggplot2::aes(fill = prop.cat),
      show.legend = T
    ) +
    ggplot2::geom_text(
      data = stoolad.nums.p,
      ggplot2::aes(
        x = min(ctcoord$X),
        y = min(ctcoord$Y) + adjy,
        label = labs
      ),
      size = caption_size,
      check_overlap = TRUE,
      hjust = 0
    ) +
    ggplot2::scale_fill_manual(
      name = "Stool Adequacy",
      values = c(
        "Zero AFP cases" = "lightgrey",
        "<40%" = "#dc582a",
        "40-59%" = "#fdae61",
        "60-79%" = "#ffffbf",
        "80%+" = "#2c7bb6",
        "Unable to Assess" = "white"
      ),
      drop = F
    ) +
    ggplot2::ggtitle("Stool Adequacy - Province") +
    sirfunctions::f.plot.looks("epicurve") +
    ggplot2::facet_wrap(~year, ncol = 4) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  ggplot2::ggsave(
    "stool.ad.maps.png",
    plot = stool.ad.maps,
    path = output_path,
    width = 14,
    height = 8
  )

  return(stool.ad.maps)
}

#' Maps of stool adequacy by district and year
#'
#' Generates maps of stool adequacy map by district and year.
#'
#' @param ctry.data `list` Large list containing polio data for a country. This is
#' the output of [extract_country_data()] or [init_dr()].
#' @param dstool `tibble` District stool adequacy table. This is the output of
#' [f.stool.ad.01()] calculated at the district level.
#' @param ctry.shape `sf` Country shapefile in long format.
#' @param prov.shape `sf` Province shapefile in long format.
#' @param dist.shape `sf` District shapefile in long format.
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis.
#' @param output_path `str` Local path where to save the figure to.
#' @param caption_size `numeric` Size of the caption. Defaults to 3.
#'
#' @returns `ggplot` Maps of stool adequacy rates for each district by year.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' dstool <- f.stool.ad.01(
#'   afp.data = ctry.data$afp.all.2,
#'   admin.data = ctry.data$dist.pop,
#'   start.date = start_date,
#'   end.date = end_date,
#'   spatial.scale = "dist",
#'   missing = "good",
#'   bad.data = "inadequate",
#'   rolling = F,
#'   sp_continuity_validation = F
#' )
#' ctry.shape <- load_clean_ctry_sp(ctry_name = "ALGERIA", type = "long")
#' prov.shape <- load_clean_prov_sp(ctry_name = "ALGERIA", type = "long")
#' dist.shape <- load_clean_dist_sp(ctry_name = "ALGERIA", type = "long")
#' generate_stool_ad_maps_dist(
#'   ctry.data, dstool,
#'   ctry.shape, prov.shape, dist.shape,
#'   "2021-01-01", "2023-12-31"
#' )
#' }
#'
#' @export
generate_stool_ad_maps_dist <- function(ctry.data,
                                        dstool,
                                        ctry.shape,
                                        prov.shape,
                                        dist.shape,
                                        start_date,
                                        end_date,
                                        output_path = Sys.getenv("DR_FIGURE_PATH"),
                                        caption_size = 3) {
  if (!"active.year.01" %in% names(ctry.shape)) {
    error_message <- paste0(
      "ctry.shape is not in long format. ",
      "Use: ctry.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the country shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  if (!"active.year.01" %in% names(prov.shape)) {
    error_message <- paste0(
      "prov.shape is not in long format. ",
      "Use: prov.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the province shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  if (!"active.year.01" %in% names(dist.shape)) {
    error_message <- paste0(
      "dist.shape is not in long format. ",
      "Use: dist.shape <- load_clean_dist_sp(ctry_name='<ctry name>', type='long')",
      " to download the province shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  ctry.shape <- ctry.shape |>
    dplyr::filter(dplyr::between(
      active.year.01,
      lubridate::year(start_date),
      lubridate::year(end_date)
    )) |>
    dplyr::mutate(year = active.year.01)
  prov.shape <- prov.shape |>
    dplyr::filter(dplyr::between(
      active.year.01,
      lubridate::year(start_date),
      lubridate::year(end_date)
    )) |>
    dplyr::mutate(year = active.year.01)
  dist.shape <- dist.shape |>
    dplyr::filter(dplyr::between(
      active.year.01,
      lubridate::year(start_date),
      lubridate::year(end_date)
    )) |>
    dplyr::mutate(year = active.year.01)



  # Get coordinates for maps that are plotted
  ctcoord <- as.data.frame(sf::st_coordinates(ctry.shape))
  # Put text at 10% below the minimum X and Y coordinates for each map
  adjy <- (range(ctcoord$Y)[1] - range(ctcoord$Y)[2]) * .1

  allafp.d <- ctry.data$afp.all.2 %>%
    dplyr::filter(date >= start_date & date <= end_date) %>%
    dplyr::reframe(
      dplyr::group_by(
        ctry.data$afp.all.2,
        cdc.classification.all2,
        adm2guid,
        year
      ),
      freq = dplyr::n()
    ) %>%
    dplyr::filter(cdc.classification.all2 != "NOT-AFP")

  alldist <- ctry.data$dist.pop[, c("adm2guid", "year", "prov", "dist")] %>%
    dplyr::filter(year >= lubridate::year(start_date) & year <= lubridate::year(end_date))

  all.dist.afp <- dplyr::left_join(alldist, allafp.d) %>%
    dplyr::group_by(year, adm2guid, prov, dist) %>%
    dplyr::summarize(allafp = sum(freq, na.rm = T))

  stoolad.d <- dplyr::left_join(
    all.dist.afp,
    dstool,
    by = c(
      "prov" = "prov",
      "year" = "year",
      "adm2guid" = "adm2guid",
      "dist" = "dist"
    )
  )

  stoolad.d <- stoolad.d %>%
    filter(!is.na(dist)) |>
    dplyr::tibble() %>%
    dplyr::mutate(
      prop.cat = dplyr::case_when(
        afp.cases == 0 ~ "Zero AFP cases",
        afp.cases != 0 & per.stool.ad < 40 ~ "<40%",
        afp.cases != 0 &
          per.stool.ad >= 40 & per.stool.ad < 60 ~ "40-59%",
        afp.cases != 0 &
          per.stool.ad >= 60 & per.stool.ad < 80 ~ "60-79%",
        afp.cases != 0 & per.stool.ad >= 80 ~ "80%+"
      )
    ) %>%
    dplyr::mutate(prop.cat = factor(
      prop.cat,
      levels = c("Zero AFP cases", "<40%", "40-59%", "60-79%", "80%+")
    ))

  if (nrow(stoolad.d) == 0) {
    return(output_empty_image(output_path, "stool.ad.maps.dist.png"))
  }

  stoolad.nums.d <- stoolad.d %>%
    dplyr::group_by(year, adm2guid, dist) %>%
    dplyr::summarize(meet.stool = sum(per.stool.ad >= 80, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
      num.meet.stool = sum(meet.stool, na.rm = T),
      len.year = length(year)
    ) %>%
    dplyr::mutate(
      labs = paste0(
        num.meet.stool,
        "/",
        len.year,
        " (",
        round(100 * num.meet.stool / len.year, 0),
        "%)",
        " districts with >= 80% stool adequacy"
      )
    )

  stool.map.d <-
    dplyr::left_join(dist.shape, stoolad.d, by = c("GUID" = "adm2guid", "year" = "year"))

  stool.ad.maps.dist <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    ggplot2::geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    ggplot2::geom_sf(
      data = dist.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    ggplot2::geom_sf(
      data = stool.map.d,
      color = "black",
      ggplot2::aes(fill = prop.cat),
      show.legend = T
    ) +
    ggplot2::geom_text(
      data = stoolad.nums.d,
      ggplot2::aes(
        x = min(ctcoord$X),
        y = min(ctcoord$Y) + adjy,
        label = labs
      ),
      size = caption_size,
      check_overlap = TRUE,
      hjust = 0
    ) +
    ggplot2::scale_fill_manual(
      name = "Stool Adequacy",
      values = c(
        "Zero AFP cases" = "lightgrey",
        "<40%" = "#dc582a",
        "40-59%" = "#fdae61",
        "60-79%" = "#ffffbf",
        "80%+" = "#2c7bb6",
        "Unable to Assess" = "white"
      ),
      drop = F
    ) +
    ggplot2::ggtitle("Stool Adequacy - District") +
    sirfunctions::f.plot.looks("epicurve") +
    ggplot2::facet_wrap(~year, ncol = 4) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  ggplot2::ggsave(
    "stool.ad.maps.dist.png",
    plot = stool.ad.maps.dist,
    path = output_path,
    width = 14,
    height = 8
  )

  return(stool.ad.maps.dist)
}



#' Maps evaluating timeliness of samples against timeliness targets.
#'
#' Generates a map at the provincial level summarizing the timeliness of samples
#' across different timeliness targets. The figure is faceted by the type of
#' timeliness target, with each facet containing the percentage of samples
#' from each province that met the targets over the years.
#'
#' @param ctry.data `list` Large list containing polio data for a country. This is the output
#' of [extract_country_data()] or [init_dr()].
#' @param ctry.shape `sf` Country shapefile in long format.
#' @param prov.shape `sf` Province shapefile in long format.
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis.
#' @param mark_x `logical` Mark where there are less than 5 AFP cases? Defaults to `TRUE`.
#' @param pt_size `numeric` Size of the marks.
#' @param output_path `str` Local path where to save the figure to.
#'
#' @returns `ggplot` Faceted map of each province evaluated against timeliness targets
#' across years.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' ctry.shape <- load_clean_ctry_sp(ctry_name = "ALGERIA", type = "long")
#' prov.shape <- load_clean_prov_sp(ctry_name = "ALGERIA", type = "long")
#' generate_timeliness_maps(ctry.data, ctry.shape, prov.shape, "2021-01-01", "2023-12-31")
#' }
#'
#' @export
generate_timeliness_maps <- function(ctry.data,
                                     ctry.shape,
                                     prov.shape,
                                     start_date,
                                     end_date,
                                     mark_x = T,
                                     pt_size = 4,
                                     output_path = Sys.getenv("DR_FIGURE_PATH")) {
  if (!requireNamespace("forcats", quietly = TRUE)) {
    stop(
      'Package "forcats" must be installed to use this function.',
      call. = FALSE
    )
  }

  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    stop(
      'Package "ggpubr" must be installed to use this function.',
      call. = FALSE
    )
  }

  if (!"active.year.01" %in% names(ctry.shape)) {
    error_message <- paste0(
      "ctry.shape is not in long format. ",
      "Use: ctry.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the country shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  if (!"active.year.01" %in% names(prov.shape)) {
    error_message <- paste0(
      "prov.shape is not in long format. ",
      "Use: prov.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the province shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  ctry.shape <- ctry.shape |>
    dplyr::filter(dplyr::between(
      active.year.01,
      lubridate::year(start_date),
      lubridate::year(end_date)
    )) |>
    dplyr::mutate(year = active.year.01)
  prov.shape <- prov.shape |>
    dplyr::filter(dplyr::between(
      active.year.01,
      lubridate::year(start_date),
      lubridate::year(end_date)
    )) |>
    dplyr::mutate(year = active.year.01)

  long.timely <- ctry.data$afp.all.2 %>%
    dplyr::select(
      "epid",
      "noti.7d.on",
      "inv.2d.noti",
      "coll.3d.inv",
      "ship.3d.coll",
      "year",
      "prov",
      "adm1guid"
    ) %>%
    tidyr::pivot_longer(
      !c("epid", "year", "prov", "adm1guid"),
      names_to = "type",
      values_to = "value"
    ) %>%
    dplyr::group_by(year, type, prov, adm1guid) %>%
    dplyr::summarize(prop = sum(value, na.rm = T) / dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year >= lubridate::year(start_date) &
      year <= lubridate::year(end_date)) %>%
    tidyr::complete(year, prov, type)

  if (nrow(long.timely) == 0) {
    return(output_empty_image(output_path, "mapt_all.png"))
  }

  for (i in 1:nrow(long.timely)) {
    if (is.na(long.timely$adm1guid[i])) {
      long.timely$adm1guid[i] <- long.timely$adm1guid[which(long.timely$prov ==
        long.timely$prov[i])][1]
    }
  }


  all.case <- dplyr::summarize(dplyr::group_by(ctry.data$afp.all.2, prov, year, adm1guid),
    case.num = dplyr::n()
  ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year >= lubridate::year(start_date) &
      year <= lubridate::year(end_date)) %>%
    tidyr::complete(year, prov, fill = list(case.num = 0))

  for (i in 1:nrow(all.case)) {
    if (is.na(all.case$adm1guid[i])) {
      all.case$adm1guid[i] <- all.case$adm1guid[which(all.case$prov == all.case$prov[i])][1]
    }
  }

  long.timely$prop <- cut(
    long.timely$prop,
    breaks = c(-1, 0.2, 0.5, 0.8, 0.9, 1.1, 1.2),
    right = F,
    labels = c("<20%", "20-49%", "50-79%", "80-89%", "90-100%", NA)
  )
  long.timely$prop <- forcats::fct_na_value_to_level(long.timely$prop, "Missing") # missing date data

  time.map <-
    dplyr::left_join(prov.shape, long.timely, by = c("GUID" = "adm1guid", "active.year.01" = "year"))
  time.map <-
    dplyr::full_join(time.map, all.case, by = c("GUID" = "adm1guid", "active.year.01" = "year"))

  time.map <- time.map %>%
    dplyr::mutate(prop = as.character(prop)) %>%
    dplyr::mutate(prop = ifelse(case.num == 0, "No AFP cases", prop)) %>%
    dplyr::mutate(prop = factor(
      prop,
      levels = c(
        "<20%",
        "20-49%",
        "50-79%",
        "80-89%",
        "90-100%",
        "No AFP cases",
        "Missing"
      )
    ))


  # Flag provinces with less than 5 AFP cases reported
  low.case.prov <- time.map %>%
    dplyr::group_by(year, ADM1_NAME) %>%
    dplyr::filter(case.num <= 5)

  # noti.7d.on
  mapt1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    ggplot2::geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    ggplot2::geom_sf(
      data = dplyr::filter(time.map, type == "noti.7d.on"),
      color = "black",
      ggplot2::aes(fill = prop)
    ) +
    ggplot2::scale_fill_manual(
      name = "Proportion",
      values = f.color.schemes("mapval"),
      drop = T
    ) +
    ggplot2::ggtitle("Proportion of cases with notification within 7 days of onset") +
    sirfunctions::f.plot.looks("epicurve")

  if (mark_x) {
    tryCatch(
      {
        mapt1 <- mapt1 +
          ggplot2::geom_sf(
            data = sf::st_centroid(dplyr::filter(low.case.prov, type == "noti.7d.on")),
            pch = 4,
            size = pt_size
          )
      },
      error = function(error) {
        cli::cli_alert_warning("Duplicate vertex. Setting sf_use_s2(F).")

        sf::sf_use_s2(F)
        mapt1 <- mapt1 +
          ggplot2::geom_sf(
            data = sf::st_centroid(dplyr::filter(low.case.prov, type == "noti.7d.on")),
            pch = 4,
            size = pt_size
          )
        sf::sf_use_s2(T)

        cli::cli_alert_success("Setting sf_use_s2(T)")
      }
    )
  }
  mapt1 <- mapt1 +
    ggplot2::facet_wrap(~year, ncol = 4) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0)
    )

  # inv.2d.noti
  mapt2 <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    ggplot2::geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    ggplot2::geom_sf(
      data = dplyr::filter(time.map, type == "inv.2d.noti"),
      color = "black",
      ggplot2::aes(fill = prop)
    )

  if (mark_x) {
    tryCatch(
      {
        mapt2 <- mapt2 +
          ggplot2::geom_sf(
            data = sf::st_centroid(dplyr::filter(
              low.case.prov, type == "inv.2d.noti"
            )),
            pch = 4,
            size = pt_size
          )
      },
      error = function(error) {
        cli::cli_alert_warning("Duplicate vertex. Setting sf_use_s2(F).")

        sf::sf_use_s2(F)
        mapt2 <- mapt2 +
          ggplot2::geom_sf(
            data = sf::st_centroid(dplyr::filter(
              low.case.prov, type == "inv.2d.noti"
            )),
            pch = 4,
            size = pt_size
          )
        sf::sf_use_s2(T)

        cli::cli_alert_success("Setting sf_use_s2(T)")
      }
    )
  }
  mapt2 <- mapt2 +
    ggplot2::scale_fill_manual(
      name = "Proportion",
      values = f.color.schemes("mapval"),
      drop = T
    ) +
    ggplot2::ggtitle("Proportion of cases with investigation within 2 days of notification") +
    sirfunctions::f.plot.looks("epicurve") +
    ggplot2::facet_wrap(~year, ncol = 4) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0)
    )

  # coll.3d.inv
  mapt3 <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    ggplot2::geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    ggplot2::geom_sf(
      data = dplyr::filter(time.map, type == "coll.3d.inv"),
      color = "black",
      ggplot2::aes(fill = prop)
    ) +
    ggplot2::scale_fill_manual(
      name = "Proportion",
      values = f.color.schemes("mapval"),
      drop = T
    ) +
    ggplot2::ggtitle("Proportion of cases with collection within 3 days of investigation") +
    sirfunctions::f.plot.looks("epicurve")

  if (mark_x) {
    tryCatch(
      {
        mapt3 <- mapt3 +
          ggplot2::geom_sf(
            data = sf::st_centroid(dplyr::filter(
              low.case.prov, type == "coll.3d.inv"
            )),
            pch = 4,
            size = pt_size
          )
      },
      error = function(error) {
        cli::cli_alert_warning("Duplicate vertex. Setting sf_use_s2(F).")

        sf::sf_use_s2(F)
        mapt3 <- mapt3 +
          ggplot2::geom_sf(
            data = sf::st_centroid(dplyr::filter(
              low.case.prov, type == "coll.3d.inv"
            )),
            pch = 4,
            size = pt_size
          )
        sf::sf_use_s2(T)

        cli::cli_alert_success("Setting sf_use_s2(T)")
      }
    )
  }
  mapt3 <- mapt3 +
    ggplot2::facet_wrap(~year, ncol = 4) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0)
    )

  # ship.3d.coll
  mapt4 <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    ggplot2::geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    ggplot2::geom_sf(
      data = dplyr::filter(time.map, type == "ship.3d.coll"),
      color = "black",
      ggplot2::aes(fill = prop)
    ) +
    ggplot2::scale_fill_manual(
      name = "Proportion",
      values = f.color.schemes("mapval"),
      drop = T
    ) +
    ggplot2::ggtitle("Proportion of stool shipped to lab within 3 days of collection") +
    sirfunctions::f.plot.looks("epicurve")



  if (mark_x) {
    tryCatch(
      {
        mapt4 <- mapt4 +
          ggplot2::geom_sf(
            data = sf::st_centroid(dplyr::filter(
              low.case.prov, type == "ship.3d.coll"
            )),
            pch = 4,
            size = pt_size
          )
      },
      error = function(error) {
        cli::cli_alert_warning("Duplicate vertex. Setting sf_use_s2(F).")

        sf::sf_use_s2(F)
        mapt4 <- mapt4 +
          ggplot2::geom_sf(
            data = sf::st_centroid(dplyr::filter(
              low.case.prov, type == "ship.3d.coll"
            )),
            pch = 4,
            size = pt_size
          )
        sf::sf_use_s2(T)

        cli::cli_alert_success("Setting sf_use_s2(T)")
      }
    )
  }

  mapt4 <- mapt4 +
    ggplot2::facet_wrap(~year, ncol = 4) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0)
    )

  mapt_dummy <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    ggplot2::geom_sf(
      data = prov.shape,
      color = "black",
      fill = "lightgrey",
      size = .5
    ) +
    ggplot2::geom_sf(
      data = time.map |> dplyr::filter(!is.na(year), !is.na(type)),
      color = "black",
      ggplot2::aes(fill = prop)
    ) +
    ggplot2::scale_fill_manual(
      name = "Proportion",
      values = f.color.schemes("mapval"),
      drop = T
    ) +
    ggplot2::facet_wrap(~year, ncol = 4) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0)
    ) +
    sirfunctions::f.plot.looks("epicurve")


  mapt_all <-
    ggpubr::ggarrange(
      mapt1,
      mapt2,
      mapt3,
      mapt4,
      ncol = 2,
      nrow = 2,
      common.legend = T,
      legend.grob = ggpubr::get_legend(mapt_dummy),
      legend = "bottom"
    )

  if (mark_x) {
    mapt_all <- ggpubr::annotate_figure(
      mapt_all,
      bottom = ggpubr::text_grob(
        "Provinces marked by an X have reported 5 or less AFP cases",
        hjust = 1.5
      )
    )
  }

  ggplot2::ggsave(
    "mapt_all.png",
    plot = mapt_all,
    path = output_path,
    width = 14,
    height = 8,
    bg = "white"
  )

  return(mapt_all)
}


#' ES detection map
#'
#' Generates a map showing the detection rate of each ES sites on a rolling period
#' as defined by the start and end dates of the analysis.
#'
#' @param es.data `tibble` ES data for a country. This is `ctry.data$es`, which is
#' part of the outputs of [extract_country_data()] and [init_dr()].
#' @param ctry.shape `sf` Country shapefile in long format.
#' @param prov.shape `sf` Province shapefile in long format.
#' @param es_start_date `str` Start date of analysis. Default is one year from the end date.
#' @param es_end_date `str` End date of analysis.
#' @param output_path `str` Local path where to save the figure to.
#' @param es.data.long
#' `r lifecycle::badge("deprecated")`
#' `tibble` Please pass the output of [clean_es_data()] into es.data instead. This paramater
#' is not being used in the function.
#'
#' @returns `ggplot` Map of EV detection rates for the environmental surveillance sites.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' es.data.long <- generate_es_data_long(ctry.data$es)
#' ctry.shape <- load_clean_ctry_sp(ctry_name = "ALGERIA", type = "long")
#' prov.shape <- load_clean_prov_sp(ctry_name = "ALGERIA", type = "long")
#' generate_es_det_map(ctry.data$es, ctry.shape, prov.shape,
#'   es_end_date = "2023-01-01"
#' )
#' }
#'
#' @export
generate_es_det_map <- function(es.data,
                                ctry.shape,
                                prov.shape,
                                es_start_date = (lubridate::as_date(es_end_date) - lubridate::years(1)),
                                es_end_date = end_date,
                                output_path = Sys.getenv("DR_FIGURE_PATH"),
                                es.data.long = lifecycle::badge("deprecated")) {
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop(
      'Package "ggrepel" must be installed to use this function.',
      call. = FALSE
    )
  }

  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    stop(
      'Package "ggpubr" must be installed to use this function.',
      call. = FALSE
    )
  }

  if (lifecycle::is_present(es.data.long)) {
    lifecycle::deprecate_warn(
      when = "1.3.0",
      what = "generate_es_det_map(es.data.long)",
      details = "es.data.long is no longer required as its function has been folded into clean_es_data()"
    )
  }

  es_start_date <- lubridate::as_date(es_start_date)
  es_end_date <- lubridate::as_date(es_end_date)

  if (!"active.year.01" %in% names(ctry.shape)) {
    error_message <- paste0(
      "ctry.shape is not in long format. ",
      "Use: ctry.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the country shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  if (!"active.year.01" %in% names(prov.shape)) {
    error_message <- paste0(
      "prov.shape is not in long format. ",
      "Use: prov.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the province shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  # For this map, use the most recent shapefile
  ctry.shape <- ctry.shape |>
    dplyr::filter(active.year.01 == lubridate::year(es_end_date)) |>
    dplyr::mutate(year = active.year.01)
  prov.shape <- prov.shape |>
    dplyr::filter(active.year.01 == lubridate::year(es_end_date)) |>
    dplyr::mutate(year = active.year.01)

  es.data <- es.data |>
    dplyr::filter(dplyr::between(collect.date, es_start_date, es_end_date))

  if (nrow(es.data) == 0) {
    return(output_empty_image(output_path, "es.det.map.png"))
  }

  det.rate <- dplyr::summarise(
    dplyr::group_by(es.data, site.name),
    det.rate = 100 * sum(as.numeric(ev.detect), na.rm = TRUE) / dplyr::n(),
    samp.num = dplyr::n()
  )

  det.rate$cats <- cut(
    det.rate$det.rate,
    breaks = c(0, 50, 80, 101),
    right = F,
    labels = c("<50%", "50-79%", "80-100%")
  )

  det.rate$cats <- as.character(det.rate$cats)

  det.rate <- det.rate %>%
    dplyr::mutate(cats = dplyr::case_when(samp.num < 5 ~ "<5 samples", TRUE ~ cats))

  site.coord <-
    dplyr::reframe(dplyr::group_by(es.data, site.name),
      lat = lat,
      lng = lng
    )
  site.coord <- unique(site.coord)

  det.rate$cats <- factor(det.rate$cats,
    levels = c("<50%", "50-79%", "80-100%", "<5 samples")
  )

  es.data <-
    dplyr::left_join(es.data, det.rate, by = c("site.name" = "site.name"))

  det.rate <- dplyr::left_join(det.rate, site.coord)


  # ES Map of sites
  # randomly put points in their districts

  es.det.map <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = 1
    ) +
    ggplot2::geom_sf(
      data = prov.shape,
      color = "black",
      fill = NA,
      size = .5
    ) +
    ggplot2::geom_point(
      data = det.rate,
      ggplot2::aes(
        x = as.numeric(lng),
        y = as.numeric(lat),
        color = cats
      ),
      show.legend = T
    ) +
    ggrepel::geom_label_repel(
      data = subset(det.rate),
      ggplot2::aes(
        x = as.numeric(lng),
        y = as.numeric(lat),
        label = site.name,
        color = cats,
      ),
      show.legend = FALSE,
      force = 100
    ) +
    ggplot2::ggtitle(paste0(
      "ES detection rate by site: ",
      format(es_start_date, "%B %Y"),
      " - ",
      format(es_end_date, "%B %Y")
    )) +
    ggplot2::scale_color_manual(
      values = c(
        "<50%" = "#FF0000",
        "50-79%" = "#feb24c",
        "80-100%" = "#0070c0",
        "<5 samples" = "black"
      ),
      name = "EV detection rate",
      drop = F
    ) +
    sirfunctions::f.plot.looks("02") +
    ggplot2::theme(legend.position = "right")

  if ("imputed_coord" %in% names(es.data)) {
    imputed_sites <- es.data |>
      dplyr::filter(imputed_coord == TRUE,
                    !is.na(lat), !is.na(lng)) |>
      dplyr::pull(site.name) |>
      unique()

    if (length(imputed_sites) != 0) {
      cli::cli_alert_info("Some sites have imputed coordinates due to missing site coordinates")
      es.det.map <- ggpubr::annotate_figure(
        es.det.map,
        bottom = ggpubr::text_grob(
          paste0("Sites missing coordinates randomly assigned within their district:\n"),
          hjust = 0.75,
          vjust = 0.5,
          size = 10,
          color = "darkgrey"
        )
      )
      es.det.map <- ggpubr::annotate_figure(es.det.map,
          bottom =  ggpubr::text_grob(
            paste0(imputed_sites, collapse = ", "),
            hjust = 0.97,
            vjust = -1.7,
            size = 8,
            color = "grey"
          )
        )
    }
  }


  ggplot2::ggsave(
    "es.det.map.png",
    plot = es.det.map,
    path = output_path,
    width = 9,
    height = 8
  )

  return(es.det.map)
}

#' Map of high priority health facilities
#'
#' Generates a map of high priority health facilities across years based on ISS/eSURV data.
#'
#' @param iss.data `tibble` ISS/eSurv data. Ensure that the `iss.data` is part of `ctry.data` and
#' has been cleaned by [clean_iss_data()].
#' @param ctry.shape `sf` Country shapefile in long format.
#' @param prov.shape `sf` Province shapefile in long format.
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis.
#' @param output_path `str` Local path where to save the figure to.
#'
#' @returns `ggplot` Map of where the high priority health facilities are across years.
#' @examples
#' \dontrun{
#' iss_path <- "C:/Users/ABC1/Desktop/iss_data.csv"
#' ctry.data <- init_dr("algeria", iss_data_path = iss_path)
#' ctry.data$iss.data <- clean_iss_data(ctry.data)
#' ctry.shape <- load_clean_ctry_sp(ctry_name = "ALGERIA", type = "long")
#' prov.shape <- load_clean_prov_sp(ctry_name = "ALGERIA", type = "long")
#' generate_iss_map(
#'   ctry.data$iss.data, ctry.shape, prov.shape,
#'   "2021-01-01", "2023-12-31"
#' )
#' }
#'
#' @export
generate_iss_map <- function(iss.data,
                             ctry.shape,
                             prov.shape,
                             start_date,
                             end_date,
                             output_path = Sys.getenv("DR_FIGURE_PATH")) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  if (is.null(iss.data)) {
    return(message("No ISS data attached."))
  }

  if (!"active.year.01" %in% names(ctry.shape)) {
    error_message <- paste0(
      "ctry.shape is not in long format. ",
      "Use: ctry.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the country shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  if (!"active.year.01" %in% names(prov.shape)) {
    error_message <- paste0(
      "prov.shape is not in long format. ",
      "Use: prov.shape <- load_clean_prov_sp(ctry_name='<ctry name>', type='long')",
      " to download the province shapefile in long format."
    )
    cli::cli_abort(error_message)
  }

  ctry.shape <- ctry.shape |>
    dplyr::filter(dplyr::between(
      active.year.01,
      lubridate::year(start_date),
      lubridate::year(end_date)
    )) |>
    dplyr::mutate(year = active.year.01)
  prov.shape <- prov.shape |>
    dplyr::filter(dplyr::between(
      active.year.01,
      lubridate::year(start_date),
      lubridate::year(end_date)
    )) |>
    dplyr::mutate(year = active.year.01)



  if (!"_gps_ending_longitude" %in% names(iss.data)) {
    iss.data <- iss.data |>
      dplyr::mutate(`_gps_ending_longitude` = .data[["_gps_longitude"]])
  }

  if (!"_gps_ending_latitude" %in% names(iss.data)) {
    iss.data <- iss.data |>
      dplyr::mutate(`_gps_ending_latitude` = .data[["_gps_latitude"]])
  }

  bbox <- sf::st_bbox(prov.shape)

  iss.data <- iss.data |>
    dplyr::filter(
      dplyr::between(`_gps_ending_longitude`, bbox$xmin, bbox$xmax),
      dplyr::between(`_gps_ending_latitude`, bbox$ymin, bbox$ymax)
    )

  if (nrow(iss.data) == 0) {
    return(output_empty_image(output_path, "iss.map.png"))
  }

  pryr <- dplyr::count(iss.data, priority_level, year) |>
    dplyr::filter(priority_level == "High")

  ctry.shape <- dplyr::left_join(ctry.shape, pryr) |>
    dplyr::mutate(labs = paste0(year, "\n(n = ", n, ")"))
  prov.shape <- dplyr::left_join(prov.shape, pryr) |>
    dplyr::mutate(labs = paste0(year, "\n(n = ", n, ")"))

  iss.data2 <- dplyr::full_join(iss.data, pryr)

  iss.data2$labs <- paste0(iss.data2$year, "\n(n = ", iss.data2$n, ")")

  iss.data.map <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = ctry.shape,
      color = "black",
      fill = NA,
      size = .5
    ) +
    ggplot2::geom_sf(
      data = prov.shape,
      color = "black",
      fill = NA,
      size = .5
    ) +
    ggplot2::geom_point(
      data = iss.data2 |>
        dplyr::filter(
          dplyr::between(
            year,
            lubridate::year(start_date),
            lubridate::year(end_date)
          ),
          priority_level == "High"
        ),
      ggplot2::aes(
        x = as.numeric(`_gps_ending_longitude`),
        y = as.numeric(`_gps_ending_latitude`),
        col = priority_level
      )
    ) +
    sirfunctions::f.plot.looks("epicurve") +
    ggplot2::scale_color_manual("Priority level", values = c("High" = "#d73027")) +
    ggplot2::facet_wrap(~labs, ncol = 4) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  ggplot2::ggsave("iss.map.png",
    plot = iss.data.map,
    path = output_path,
    width = 14,
    height = 8
  )

  return(iss.data.map)
}
# Tables ----
#' Surveillance indicator table
#'
#' Generates the surveillance indicator table for each year. Outputs the number of
#' AFP cases, national NPAFP rate and stool adequacy,percentage of population living in
#' districts with greater than or equal to 100,000 U15 meeting both indicators.
#'
#' @param ctry.data `list` Large list containing polio data of a country.
#' @param ctry.extract `tibble` Country NPAFP rate. Output of [f.npafp.rate.01()] calculated at the country level.
#' @param dist.extract `tibble` District NPAFP rate. Output of [f.npafp.rate.01()] calculated at the district level.
#' @param cstool `tibble` Country stool adequacy. Output of [f.stool.ad.01()] calculated at the country level.
#' @param dstool `tibble` District stool adequacy. Output of [f.stool.ad.01()] calculated at the district level.
#' @param afp.case `tibble` AFP case counts. Output of [generate_afp_by_month_summary()] with `by="year"`.
#' @param country_name `str` Name of the country.
#'
#' @returns `flextable` Table summarizing yearly trends in NPAFP and stool adequacy at the national level.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' ctry.extract <- f.npafp.rate.01(
#'   afp.data = ctry.data$afp.all.2,
#'   pop.data = ctry.data$ctry.pop,
#'   start.date = start_date,
#'   end.date = end_date,
#'   spatial.scale = "ctry",
#'   pending = T,
#'   rolling = F,
#'   sp_continuity_validation = F
#' )
#' dist.extract <- f.npafp.rate.01(
#'   afp.data = ctry.data$afp.all.2,
#'   pop.data = ctry.data$ctry.pop,
#'   start.date = start_date,
#'   end.date = end_date,
#'   spatial.scale = "dist",
#'   pending = T,
#'   rolling = F,
#'   sp_continuity_validation = F
#' )
#' cstool <- f.stool.ad.01(
#'   afp.data = ctry.data$afp.all.2,
#'   admin.data = ctry.data$ctry.pop,
#'   start.date = start_date,
#'   end.date = end_date,
#'   spatial.scale = "ctry",
#'   missing = "good",
#'   bad.data = "inadequate",
#'   rolling = F,
#'   sp_continuity_validation = F
#' )
#' dstool <- f.stool.ad.01(
#'   afp.data = ctry.data$afp.all.2,
#'   admin.data = ctry.data$dist.pop,
#'   start.date = start_date,
#'   end.date = end_date,
#'   spatial.scale = "dist",
#'   missing = "good",
#'   bad.data = "inadequate",
#'   rolling = F,
#'   sp_continuity_validation = F
#' )
#' afp.by.month <- generate_afp_by_month(ctry.data$afp.all.2, "2021-01-01", "2023-12-31")
#' afp.case <- generate_afp_by_month_summary(afp.by.month, ctry.data, start_date, end_date, "year")
#' generate_surv_ind_tab(ctry.data, ctry.extract, dist.extract, cstool, dstool, afp.case)
#' }
#' @export
generate_surv_ind_tab <- function(ctry.data,
                                  ctry.extract,
                                  dist.extract,
                                  cstool,
                                  dstool,
                                  afp.case,
                                  country_name = Sys.getenv("DR_COUNTRY")) {
  if (!requireNamespace("janitor", quietly = TRUE)) {
    stop('Package "janitor" must be installed to use this function.',
      .call = FALSE
    )
  }

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop('Package "tibble" must be installed to use this function.',
      .call = FALSE
    )
  }

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop('Package "flextable" must be installed to use this function.',
         .call = FALSE
    )
  }

  dist.ind.afp <- dplyr::left_join(
    dist.extract,
    dstool
  )

  # population meeting both >=2 NPAFP rate and >=80% stool adequacy
  tot.dist.pop <- dist.ind.afp %>%
    dplyr::filter(u15pop >= 100000) |>
    dplyr::group_by(year) |>
    dplyr::summarize(tot.dist.pop = sum(u15pop, na.rm = T))

  dist.adeq.ind <- dist.ind.afp |>
    dplyr::filter(npafp_rate >= 2 & per.stool.ad >= 80, u15pop >= 100000) |>
    dplyr::group_by(year) %>%
    dplyr::summarize(tot.dist.adeq = sum(u15pop, na.rm = T))

  meet.ind <- dplyr::left_join(tot.dist.pop, dist.adeq.ind, by = c("year" = "year")) %>%
    dplyr::mutate(dplyr::across(tot.dist.adeq, ~ tidyr::replace_na(.x, 0)))

  meet.ind <- meet.ind |>
    dplyr::mutate(prop.dist.adeq = tot.dist.adeq / tot.dist.pop * 100)

  ctry.ind.afp <- dplyr::left_join(ctry.extract, cstool, by = c("year", "adm0guid"))
  ctry.ind.afp <- dplyr::left_join(ctry.ind.afp, afp.case, by = c("year"))
  temp.ind.tab <- dplyr::left_join(ctry.ind.afp, meet.ind, by = c("year"))

  ## Districts with population over 100K
  dist.100k <- dist.extract %>%
    dplyr::filter(u15pop >= 100000)

  unique.dist.100k <- ctry.data$dist.pop %>%
    dplyr::filter(ctry == stringr::str_to_upper(country_name) &
      u15pop >= 100000) %>%
    unique() %>%
    dplyr::group_by(year, u15pop, adm2guid) %>%
    dplyr::filter(u15pop >= 100000) %>%
    dplyr::filter(year >= lubridate::year(start_date) &
      year <= lubridate::year(end_date))

  num.dists.100k <- unique.dist.100k %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(dist.100k.num = dplyr::n()) # total number of districts with pop >100K u15


  ad.dists.100k <- dplyr::left_join(dstool,
    dist.100k,
    by = c("year" = "year", "adm2guid" = "adm2guid")
  ) %>%
    dplyr::filter(npafp_rate >= 2 & per.stool.ad >= 80) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(ad.dist.100k.num = dplyr::n())
  # number of dists meeting both requirements with pop >100K u15

  adeq.dists <-
    dplyr::left_join(num.dists.100k, ad.dists.100k, by = c("year")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::mutate(prop = paste0(ad.dist.100k.num, "/", dist.100k.num))

  temp.ind.tab1 <-
    dplyr::left_join(temp.ind.tab, adeq.dists, by = c("year" = "year"))

  ## Making a flextable for this
  temp.ind.tab2 <- temp.ind.tab1 %>%
    dplyr::mutate(dplyr::across(
      c(afp.cases, npafp_rate, per.stool.ad, prop.dist.adeq),
      ~ as.numeric(.)
    )) %>%
    dplyr::mutate(dplyr::across(
      c(afp.cases, npafp_rate, per.stool.ad, prop.dist.adeq),
      ~ round(., 1)
    ))

  temp.ind.tab.flex <- as.data.frame(t(temp.ind.tab2)) %>%
    janitor::row_to_names(row_number = 1) %>%
    tibble::rownames_to_column("type") %>%
    dplyr::filter(type %in% c(
      "afp.cases",
      "npafp_rate",
      "per.stool.ad",
      "prop.dist.adeq",
      "prop"
    )) %>%
    dplyr::mutate(
      type = dplyr::case_when(
        type == "npafp_rate" ~ "NPAFP rate*",
        type == "afp.cases" ~ "AFP cases",
        type == "per.stool.ad" ~ "Stool adequacy**",
        type == "prop.dist.adeq" ~ "% Population living in districts >= 100,000 U15 that met both indicators",
        type == "prop" ~ "Districts >= 100,000 U15 that met both indicators",
        FALSE ~ type
      )
    )

  temp.ind.tab.flex <-
    temp.ind.tab.flex[c(2, 1, 3, 4, 5), ] # Reorder the table to be in the correct order

  ## Flextable of surveillance indicators (surv.ind.tab)
  surv.ind.tab <- flextable::flextable(temp.ind.tab.flex) %>%
    flextable::theme_booktabs() %>%
    flextable::bold(bold = TRUE, part = "header") %>%
    flextable::colformat_double(
      j = 2:ncol(temp.ind.tab.flex),
      digits = 1,
      na_str = "---"
    ) %>%
    flextable::set_header_labels(type = "") %>%
    flextable::add_footer_row(
      top = F,
      "*Pendings included\n**Stool adequacy defined as per Certification Indicator, i.e., 2 stools collected at least 24h apart AND <=14d of onset AND received in good condition at a WHO-accredited laboratory (missing condition assumed good)",
      colwidths = ncol(temp.ind.tab.flex)
    ) %>%
    flextable::autofit()

  return(surv.ind.tab)
}

#' Summary table of indicators at the province level
#'
#' Generates a table summarizing both NPAFP and stool adequacy rates at the province level and by year.
#'
#' @param pnpafp  `tibble` NPAFP table. Output of [f.npafp.rate.01()] at the province level.
#' @param pstool `tibble` Stool adequacy at province level. Output of [f.stool.ad.01()] at the province level.
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis.
#' @param prov.case.ind  `tibble` `r lifecycle::badge("deprecated")`
#' Deprecated in favor of the more informative pnpafp param name.
#'
#' @returns `flextable` Summary table of province NPAFP and stool adequacy rates per year.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' start_date <- "2021-01-01"
#' end_date <- "2023-12-31"
#' prov.extract <- f.npafp.rate.01(
#'   afp.data = ctry.data$afp.all.2,
#'   pop.data = ctry.data$prov.pop,
#'   start.date = start_date,
#'   end.date = end_date,
#'   spatial.scale = "prov",
#'   pending = T,
#'   rolling = F,
#'   sp_continuity_validation = F
#' )
#' pstool <- f.stool.ad.01(
#'   afp.data = ctry.data$afp.all.2,
#'   admin.data = ctry.data$prov.pop,
#'   start.date = start_date,
#'   end.date = end_date,
#'   spatial.scale = "prov",
#'   missing = "good",
#'   bad.data = "inadequate",
#'   rolling = F,
#'   sp_continuity_validation = F
#' )
#' generate_pop_tab(prov.extract, pstool, start_date, end_date)
#' }
#'
#' @export
generate_pop_tab <- function(pnpafp,
                             pstool,
                             start_date,
                             end_date,
                             prov.case.ind = lifecycle::deprecated()) {

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop('Package "flextable" must be installed to use this function.',
         .call = FALSE
    )
  }

  if (lifecycle::is_present(prov.case.ind)) {
    lifecycle::deprecate_warn(
      "1.3.0",
      "sirfunctions::generate_pop_tab(prov.case.ind = )",
      "sirfunctions::generate_pop_tab(pnpafp = )"
    )

    pnpafp <- prov.case.ind
  }

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  sub.prov.case.ind <- pnpafp %>%
    dplyr::select(
      "year",
      "n_npafp",
      "u15pop",
      "adm1guid",
      "prov",
      "npafp_rate"
    )

  sub.pstool <- pstool %>%
    dplyr::select("year", "per.stool.ad", "adm1guid", "prov") |>
    dplyr::filter(!is.na(prov))

  sub.prov.join <- dplyr::full_join(sub.prov.case.ind, sub.pstool) %>%
    dplyr::arrange(prov, year) |>
    ungroup()

  sub.prov.join <- sub.prov.join %>%
    dplyr::group_by(prov) %>%
    dplyr::mutate(diff = dplyr::lag(n_npafp)) %>%
    dplyr::mutate(diff_per = round(100 * (n_npafp - dplyr::lag(n_npafp)) /
      dplyr::lag(n_npafp), 1)) %>%
    dplyr::mutate(dplyr::across(c(per.stool.ad, diff, diff_per, n_npafp), \(x) round(x, 0))) %>%
    dplyr::mutate(dplyr::across(c(npafp_rate), \(x) round(x, 1))) |>
    dplyr::mutate(u15pop = round(u15pop, 0)) |>
    dplyr::filter(!is.na(prov))

  date.analysis <- seq(lubridate::year(start_date), lubridate::year(end_date), 1)
  pop.date.analysis <- paste0("u15pop_", date.analysis[1:length(date.analysis) - 1])

  sub.prov.join.wide <- tidyr::pivot_wider(
    sub.prov.join,
    names_from = "year",
    values_from = c(
      "per.stool.ad",
      "diff",
      "diff_per",
      "n_npafp",
      "npafp_rate",
      "u15pop"
    )
  ) %>%
    dplyr::select(-dplyr::all_of(pop.date.analysis))

  var.ord <- c(
    "prov",
    paste0("u15pop_", date.analysis[length(date.analysis)]),
    paste0("n_npafp_", date.analysis),
    paste0("diff_per_", date.analysis[2:length(date.analysis)]),
    paste0("npafp_rate_", date.analysis),
    paste0("per.stool.ad_", date.analysis)
  )

  sub.prov.join.wide <- sub.prov.join.wide[, c(var.ord)] %>%
    replace(is.na(.), 0)

  var.ord.case <- c(
    "prov",
    paste0("u15pop_", date.analysis[length(date.analysis)]),
    paste0("n_npafp_", date.analysis),
    paste0("diff_per_", date.analysis[2:length(date.analysis)])
  )

  # NPAFP table
  col_palette <- c("#FF9999", "white")
  col.npafp.rate <- sub.prov.join.wide[, c(paste0("npafp_rate_", date.analysis))] %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) tidyr::replace_na(x, 0))) %>%
    dplyr::mutate(dplyr::across(c(dplyr::everything()), \(x) cut(
      x,
      breaks = c(0, 2),
      right = F,
      label = FALSE
    )))

  npafp.rate.colors <- col_palette[as.matrix(col.npafp.rate)]

  # Stool adequacy
  col_palette <- c("#FF9999", "white")
  col.stool.ad <- sub.prov.join.wide[, c(paste0("per.stool.ad_", date.analysis))] %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) tidyr::replace_na(x, 0))) %>%
    dplyr::mutate(dplyr::across(c(dplyr::everything()), \(x) cut(
      x,
      breaks = c(0, 80),
      right = F,
      label = FALSE
    )))

  stool.ad.colors <- col_palette[as.matrix(col.stool.ad)]

  # case vars only
  sub.prov.join.wide.case <- sub.prov.join.wide %>%
    dplyr::select(dplyr::all_of(var.ord.case))
  # Cases and differences

  null.col <- rep(c(NA),
    times = ncol(sub.prov.join.wide.case) * nrow(sub.prov.join.wide.case)
  )

  col.mat <- c(null.col, npafp.rate.colors, stool.ad.colors)

  # Make provinces not meeting indicators red
  # If stool ad or NPAFP below threshold - color = "#CC0000"
  # Subset of prov not meeting indicators any year
  inad.prov <- sub.prov.join %>%
    dplyr::filter(npafp_rate < 2 | per.stool.ad < 80)

  uni.inad.prov <- match(unique(inad.prov$prov), sub.prov.join.wide$prov)

  # Color matrix
  col.mat.txt <- col.mat %>%
    stringr::str_replace(., "#FF9999", "#CC0000")
  col.mat.txt[uni.inad.prov] <- "#CC0000"

  # Flextable column formatting calculations
  # # NPAFP cases length
  npafp.case.length <- length(subset(var.ord, grepl("n_n", var.ord) == T |
    grepl("diff", var.ord) == T))
  # NPAFP rate length
  npafp.rate.length <- length(subset(var.ord, grepl("rate", var.ord) == T))
  # stool adequacy length
  stool.ad.length <- length(subset(var.ord, grepl("stool", var.ord) == T))

  # Labels for % difference
  diff.yr <- length(which(grepl("diff", names(
    sub.prov.join.wide
  )) == T))

  diff.lab <- NULL
  for (i in 1:(diff.yr)) {
    diff.lab[i] <- paste(
      "% difference ",
      min(date.analysis) + i - 1,
      "-",
      min(date.analysis) + i
    )
  }

  # Names for flextable columns
  names1 <- names(sub.prov.join.wide)
  names2 <- c(
    "Province",
    paste0("U15 Population - ", max(date.analysis)),
    date.analysis,
    diff.lab,
    date.analysis,
    date.analysis
  )


  small_border <- flextable::fp_border_default(color = "black", width = 1)
  # pop.tab flextable
  pop.tab <- flextable::flextable(sub.prov.join.wide) %>%
    flextable::theme_booktabs() %>%
    flextable::bg(j = colnames(sub.prov.join.wide), bg = col.mat) %>%
    flextable::color(j = colnames(sub.prov.join.wide), color = col.mat.txt) %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::set_header_df(
      mapping = data.frame(
        keys = names1,
        values = names2,
        stringsAsFactors = FALSE
      ),
      key = "keys"
    ) %>%
    flextable::add_header_row(
      values = c("", "# NP AFP Cases", "NP AFP rate", "% Stool Adequacy"),
      colwidths = c(2, npafp.case.length, stool.ad.length, stool.ad.length),
      top = TRUE
    ) %>%
    flextable::vline(
      j = c(
        2,
        2 + npafp.case.length,
        2 + npafp.case.length + stool.ad.length
      ),
      border = small_border
    ) %>%
    flextable::hline(part = "header") %>%
    flextable::bold(bold = TRUE, part = "header") %>%
    flextable::align(align = "center", part = "all")

  return(pop.tab)
}

#' Issues with stool adequacy at the country level
#'
#' Generates a summary table at the country level highlighting issues around stool adequacy.
#'
#' @param ctry.data `list` large list containing polio data for a country. This is the output of
#' [extract_country_data()] or [init_dr()].
#' @param cstool `tibble` Stool adequacy at the country level. This is the output of [f.stool.ad.01()].
#' @param start_date `str` Start date of analysis.
#' @param end_date `str` End date of analysis.
#'
#' @returns `flextable` Summary table containing stool adequacy issues at the country level.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' start_date <- "2021-01-01"
#' end_date <- "2023-12-31"
#' cstool <- f.stool.ad.01(
#'   afp.data = ctry.data$afp.all.2,
#'   admin.data = ctry.data$ctry.pop,
#'   start.date = start_date,
#'   end.date = end_date,
#'   spatial.scale = "ctry",
#'   missing = "good",
#'   bad.data = "inadequate",
#'   rolling = F,
#'   sp_continuity_validation = F
#' )
#' generate_inad_tab(ctry.data, cstool, start_date, end_date)
#' }
#'
#' @export
generate_inad_tab <- function(ctry.data,
                              cstool,
                              start_date,
                              end_date) {
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  if (!requireNamespace("janitor", quietly = TRUE)) {
    stop('Package "janitor" must be installed to use this function.',
      .call = FALSE
    )
  }

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop('Package "tibble" must be installed to use this function.',
      .call = FALSE
    )
  }

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop('Package "flextable" must be installed to use this function.',
         .call = FALSE
    )
  }

  # All AFP cases
  afps.all <- ctry.data$afp.all.2 %>%
    dplyr::filter(
      dplyr::between(date, start_date, end_date),
      cdc.classification.all2 != "NOT-AFP"
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      good.cond.1 = sum(
        (ontostool1 <= 14 & (stool.1.condition == "Good" | is.na(stool.1.condition))) |
          (ontostool2 <= 14 & (stool.2.condition == "Good" | is.na(stool.2.condition))),
        na.rm = T
      ),
      good.cond.2 = sum(stool1missing == 0 &
        stool2missing == 0 &
        ontostool1 <= 21 &
        ontostool2 <= 21 &
        (stool.1.condition == "Good" | is.na(stool.1.condition)) &
        (stool.2.condition == "Good" | is.na(stool.2.condition)), na.rm = T),
      afp.cases = n()
    ) |>
    dplyr::mutate(
      good.cond.1per = round(good.cond.1 / afp.cases * 100, 0),
      good.cond.2per = round(good.cond.2 / afp.cases * 100, 0)
    ) |>
    dplyr::mutate(
      good.cond.1per = paste0(
        good.cond.1per,
        "% (", good.cond.1, "/", afp.cases, " cases)"
      ),
      good.cond.2per = paste0(
        good.cond.2per,
        "% (", good.cond.2, "/", afp.cases, " cases)"
      )
    )

  # Join with cstool
  allinadstool <- dplyr::left_join(cstool, afps.all) |>
    mutate(per.stool.ad = round(per.stool.ad, 0))

  # Create additional columns
  allinadstool <- allinadstool |>
    dplyr::mutate(
      timelyper = round(late.collection / num.inadequate * 100, 0),
      missingper = round(one.or.no.stool / num.inadequate * 100, 0),
      poorper = round(bad.condition / num.inadequate * 100, 0),
      badper = round(bad.data / num.inadequate * 100, 0)
    ) |>
    dplyr::mutate(
      timelyper = paste0(late.collection, " (", timelyper, "%)"),
      missingper = paste0(one.or.no.stool, " (", missingper, "%)"),
      poorper = paste0(bad.condition, " (", poorper, "%)"),
      badper = paste0(bad.data, " (", badper, "%)")
    )

  inad.tab <- allinadstool |>
    dplyr::select(-dplyr::any_of(c("weight",
      "days_in_year", "days.at.risk",
      "adm0guid", "earliest_date", "latest_date",
      "datasource", "ctry", "u15pop"
    ))) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) as.character(x))) |>
    tidyr::pivot_longer(
      cols = c("afp.cases":"badper"),
      names_to = "type"
    ) |>
    tidyr::pivot_wider(names_from = "year", values_from = "value") |>
    dplyr::filter(
      type %in% c(
        "num.adequate",
        "num.inadequate",
        "per.stool.ad",
        "timelyper",
        "missingper",
        "poorper",
        "badper",
        "good.cond.1per",
        "good.cond.2per"
      )
    ) %>%
    dplyr::mutate(
      type = dplyr::case_when(
        type == "num.adequate" ~ "Cases with adequate stools",
        type == "num.inadequate" ~ "Cases with inadequate stools",
        type == "per.stool.ad" ~ "Stool adequacy*",
        type == "afp.cases" ~ "",
        type == "timelyper" ~ "Late collection (%)",
        type == "missingper" ~ "No Stool/one stool",
        type == "poorper" ~ "Poor condition",
        type == "badper" ~ "Bad dates",
        type == "good.cond.1per" ~ "1 stool within 14 days of onset (+ condition)",
        type == "good.cond.2per" ~ "2 stools within 21 days of onset (+ condition)",
        FALSE ~ type
      )
    )

  inad.tab <- inad.tab[c(3, 1, 2, 6, 7, 8, 9, 4, 5), ] # Reorder the table to be in the correct order

  inad.tab$sub <- c(
    "",
    "",
    "",
    "Among Inadequate Cases",
    "Among Inadequate Cases",
    "Among Inadequate Cases",
    "Among Inadequate Cases",
    "Among All Cases",
    "Among All Cases"
  )

  inad.tab.flex.a <- flextable::as_grouped_data(inad.tab, groups = c("sub"))
  inad.tab.flex <- flextable::flextable(inad.tab.flex.a) |>
    flextable::theme_booktabs() |>
    flextable::bold(bold = TRUE, part = "header") |>
    flextable::set_header_labels(type = "", sub = "") |>
    flextable::add_footer_row(
      top = F,
      "*Stool adequacy defined as per Certification Indicator, i.e., 2 stools collected at least 24h apart AND <=14d of onset AND received in good condition at a WHO-accredited laboratory (missing condition assumed good)",
      colwidths = ncol(inad.tab)
    ) %>%
    flextable::autofit()

  return(inad.tab.flex)
}


#' 60-day follow up table
#'
#' Generates a table summarizing the number of inadequate cases that need follow up.
#'
#' @param cases.need60day `tibble` Summary table containing those that need 60 day follow-up.
#'  Output of [generate_60_day_table_data()].
#'
#' @returns `flextable` A summary of cases requiring 60-day followups per year.
#' @examples
#' \dontrun{
#' raw.data <- get_all_polio_data(attach.spatial.data = FALSE)
#' ctry.data <- extract_country_data("algeria", raw.data)
#' stool.data <- generate_stool_data(
#'   ctry.data$afp.all.2, "good", "inadequate",
#'   "2021-01-01", "2023-12-31"
#' )
#' cases.need60day <- generate_60_day_table_data(
#'   stool.data,
#'   "2021-01-01", "2023-12-31"
#' )
#' generate_60_day_tab(cases.need60day)
#' }
#'
#' @export
generate_60_day_tab <- function(cases.need60day) {

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop('Package "flextable" must be installed to use this function.',
         .call = FALSE
    )
  }

  comp.by.year <- cases.need60day |>
    dplyr::group_by(year) |>
    dplyr::summarize(
      inadequate = sum(adequacy.final2 == "Inadequate"),
      inadequate.need.60day = sum(need60day.v2 == 1 & adequacy.final2 == "Inadequate"),
      got60day = sum(got60day == 1, na.rm = T),
      ontime60day = sum(ontime.60day == 1, na.rm = T),
      compatible = sum(cdc.classification.all2 == "COMPATIBLE"),
      pot.compatible = sum(pot.compatible == 1, na.rm = T),
      missing.fu.date = sum(missing.fu.date == 1, na.rm = T)
    ) |>
    dplyr::mutate(
      per.got60 = round(got60day / inadequate.need.60day * 100),
      per.ontime60day = round(ontime60day / inadequate.need.60day * 100),
      per.comp = round(compatible / inadequate * 100),
      per.pot.comp = round(pot.compatible / inadequate * 100),
      per.got60.2 = paste0(got60day, "/", inadequate.need.60day, " (", per.got60, "%)"),
      per.ontime60day.2 = paste0(ontime60day, "/", inadequate.need.60day, " (", per.ontime60day, "%)"),
      per.comp.2 = compatible,
      per.pot.comp.2 = pot.compatible,
      per.missing.fu.date = paste0(
        missing.fu.date, "/", inadequate.need.60day, " (",
        round(missing.fu.date / inadequate.need.60day * 100),
        "%)"
      )
    ) |>
    dplyr::select(
      "year",
      "inadequate",
      "inadequate.need.60day",
      "per.got60.2",
      "per.ontime60day.2",
      "per.missing.fu.date",
      "per.comp.2",
      "per.pot.comp.2"
    ) |>
    dplyr::mutate(year = as.character(year)) |>
    dplyr::arrange(dplyr::desc(year))


  # flex table

  tab.60d <- comp.by.year |>
    flextable::flextable() |>
    flextable::theme_booktabs() |>
    flextable::bold(bold = TRUE, part = "header") |>
    flextable::set_header_labels(
      year = "Year",
      inadequate = "No. inadequate cases",
      inadequate.need.60day = "No. inadequate cases due for follow-up*",
      per.got60.2 = "Recorded 60-day follow-up",
      per.ontime60day.2 = "Recorded 60-day follow-up between 60-90 days of onset",
      per.missing.fu.date = "No. missing follow up date with findings",
      per.comp.2 = "Compatible cases",
      per.pot.comp.2 = "Potentially compatible cases**"
    ) |>
    flextable::align(
      j = 2:7,
      align = "center",
      part = "all"
    ) |>
    flextable::align(
      j = 1:1,
      align = "left",
      part = "all"
    ) |>
    flextable::fontsize(size = 11, part = "all") |>
    flextable::width(j = 1:7, width = 2) |>
    flextable::vline(j = 6)

  tab.60d <- tab.60d |>
    flextable::add_footer_row(
      top = F,
      paste0(
        "*120 days post-paralysis onset (90 days to complete follow-up + 30 days to account for data lag)\n",
        "** Defined as inadequate cases with: ",
        "1) FU finding (Residual weakness, Lost to FU, Died) or no FU visit; ",
        "2) Discarded or Pending Lab / Pending Classification; ",
        '3) Filtered for "OPV/IPV status: <3 doses, or Unknown"'
      ),
      colwidths = ncol(comp.by.year)
    )

  return(tab.60d)
}


#' ES surveillance sites summary table
#'
#' Generates a summary table on the performance of surveillance sites over a rolling basis as
#' indicated by the start and end dates. Includes information on the EV detection rate, number of
#' samples collected, percentage of samples with good condition, and percentage of samples meeting the
#' timeliness target of arriving to lab within 3 days.
#'
#' @param es.data `tibble` ES data. This is `ctry.data$es`, which is part of the output of either
#' [extract_country_data()] or [init_dr()]. Ensure that the `ctry.data` object has been cleaned with
#' [clean_ctry_data()] first. Otherwise, there will be an error.
#' @param es_start_date `str` Start date of analysis. Defaults to a year before the end date.
#' @param es_end_date `str` End date of analysis.
#'
#' @returns `flextable` Summary table of ES surveillance site performance.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' ctry.data <- clean_ctry_data(ctry.data)
#' generate_es_tab(ctry.data$es, es_end_date = "2023-12-31")
#' }
#'
#' @export
generate_es_tab <- function(es.data,
                            es_start_date = (lubridate::as_date(es_end_date) - lubridate::years(1)),
                            es_end_date = end_date) {

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop('Package "flextable" must be installed to use this function.',
         .call = FALSE
    )
  }

  es_start_date <- lubridate::as_date(es_start_date)
  es_end_date <- lubridate::as_date(es_end_date)

  es.data <- es.data |>
    dplyr::filter(dplyr::between(collect.date, es_start_date, es_end_date))

  # Big table that needs calculating
  # Cols = province, district, site name, earliest sample collected in POLIS,
  # n samples collected (earliest to analysis date), % EV detected, % good condition
  # % arrived within 3 days, days from collection to lab arrival (median + range),
  # WPV/VDPV

  # ev.pct = ev percent
  # condition percent
  # transport percent
  # median transport days
  # DATES MAY NEED ADJUSTMENT DEPENDING ON ANALYSIS PERIOD


  # NOTE THAT RIGHT NOW THIS REMOVES SAMPLES MISSING A CONDITION FROM THE CALCULATIONS
  es.data$timely <-
    difftime(
      as.Date(es.data$date.received.in.lab, format = "%d/%m/%Y"),
      es.data$collect.date,
      units = "days"
    )

  # get values with negative intervals
  neg_interval_data <- es.data |>
    dplyr::filter(timely < 0 | is.na(timely)) |>
    dplyr::group_by(ADM1_NAME, ADM2_NAME, site.name) |>
    dplyr::summarize(neg_intervals = dplyr::n())
  # sample summary
  sample_summary <- es.data |>
    dplyr::group_by(ADM1_NAME, ADM2_NAME, site.name) |>
    dplyr::summarize(samples = dplyr::n())

  neg_interval_data <- neg_interval_data |>
    dplyr::left_join(sample_summary)

  neg_interval_data <- neg_interval_data |>
    dplyr::mutate(bad_samples = round(neg_intervals / samples * 100, 2))

  # turn negative timely intervals to NA to exclude from calculations
  es.data <- es.data |>
    dplyr::mutate(timely = dplyr::if_else(timely < 0, NA, timely))

  es.tab1 <- es.data %>%
    # dplyr::filter(lubridate::year(collect.date) == lubridate::year(end_date)) %>%
    dplyr::group_by(site.name, ADM1_NAME, ADM2_NAME) %>%
    dplyr::reframe(
      early.dat = format(early.dat, format = "%B %d, %Y"),
      # earliest report to POLIS
      ev.pct = 100 * sum(as.numeric(ev.detect), na.rm = TRUE) / dplyr::n(),
      # percent EV detected
      num.spec = dplyr::n(),
      # number of specimens
      num.spec.bad = sum(is.na(timely)),
      # no of bad specimens excluded
      condition.pct = 100 * sum(sample.condition == "Good", na.rm = T) / dplyr::n(),
      # specimens in good condition
      trans.pct = round(100 * sum(as.numeric(timely) <= 3, na.rm = TRUE) / dplyr::n(), 0),
      # % timely
      med.trans = paste0(
        median(as.numeric(timely), na.rm = T),
        " (",
        min(as.numeric(timely), na.rm = T),
        ", ",
        max(as.numeric(timely), na.rm = T),
        ")"
      ),
      # med (range)
      num.wpv.or.vdpv = sum(wpv, na.rm = T) + sum(vdpv, na.rm = T)
    ) %>% # WPV/VDPV
    dplyr::distinct()

  es.tab1 <- es.tab1 %>%
    dplyr::arrange(ADM1_NAME, ADM2_NAME, site.name)

  es.table <- es.tab1 %>%
    flextable::flextable(
      col_keys = c(
        "ADM1_NAME",
        "ADM2_NAME",
        "site.name",
        "early.dat",
        "num.spec",
        "num.spec.bad",
        "ev.pct",
        "condition.pct",
        "trans.pct",
        "med.trans",
        "num.wpv.or.vdpv"
      )
    ) %>%
    flextable::theme_booktabs() %>%
    flextable::add_header_lines(values = paste0(
      format(es_start_date, "%B %Y"),
      " - ",
      format(es_end_date, "%B %Y")
    )) %>%
    # flextable::hline(part="all", border = gray.border ) %>%
    flextable::bold(bold = TRUE, part = "header") %>%
    # flextable::hline(part = "header", border = std.border) %>%
    flextable::align(
      j = 4:9,
      align = "center",
      part = "all"
    ) %>%
    flextable::align(
      j = 1:3,
      align = "left",
      part = "all"
    ) %>%
    # flextable::hline(part = "header", border = std.border) %>%
    # hline_bottom(part = "body", border = std.border ) %>%
    flextable::colformat_double(
      j = 5:8,
      digits = 0,
      na_str = "NA"
    ) %>%
    flextable::width(width = 1) %>%
    flextable::width(j = 3, width = 2.5) %>%
    flextable::width(j = 1:2, width = 1.5) %>%
    # flextable::width(j=10, width = .1) %>%
    # add_footer(province = "Red = indicator not met, * and gray = missing data for >25% of samples, NA = data unavailable; Indicator targets: >=50% for EV (NPEV, vaccine, VDPV, or WPV) detection, =>80% for sample condition and transport time. Sites with <6 months of sample collection are labeled as 'new'.") %>%
    # merge_at(j = 1:9, part = "footer") %>%
    flextable::fontsize(size = 11, part = "all") %>%
    flextable::set_header_labels(
      ADM1_NAME = "Province",
      ADM2_NAME = "District",
      early.dat = "Earliest date reporting to POLIS",
      site.name = "Site name",
      num.spec = "No. samples collected",
      num.spec.bad = "Excluded samples with bad data (negative or N/A time intervals)",
      ev.pct = "% detected EV",
      condition.pct = "% good condition",
      trans.pct = "% arriving within 3 days",
      med.trans = "Median lab transport time (d)",
      num.wpv.or.vdpv = "No. VDPV or WPV"
    ) |>
    flextable::align(
      j = 10:11,
      align = "center",
      part = "all"
    )

  if ((neg_interval_data |> nrow()) > 0) {
    cli::cli_alert_info(
      paste0(
        "The following sample sites had bad data. ",
        "Bad data points were excluded from analysis:\n"
      )
    )
    for (i in 1:nrow(neg_interval_data)) {
      row <- neg_interval_data[i, ]
      message <- paste0(
        "\nProvince: ",
        row$ADM1_NAME,
        "\n",
        "District: ",
        row$ADM2_NAME,
        "\n",
        "Site Name: ",
        row$site.name,
        "\n",
        "Bad samples: ",
        row$neg_intervals,
        "\nTotal samples: ",
        row$samples,
        "\n",
        "Bad Data (%): ",
        row$bad_samples
      )
      cli::cli_alert(message)
    }
    cli::cli_alert_info("Additional info in es.table.errors.csv at the errors folder.")
    readr::write_csv(neg_interval_data,
      file = file.path(Sys.getenv("DR_ERROR_PATH"), "es.table.errors.csv")
    )
  }

  return(es.table)
}


# Helper functions ----

#' Output an empty image
#'
#' @param output_path `str` Output path.
#' @param img_title `str` Title of the image.
#'
#' @returns `NULL` silently upon success.
#' @keywords internal
#'
output_empty_image <- function(output_path, img_title) {
  cli::cli_alert_info(paste0("No data to create the figure for: ", img_title))
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::annotate("text", x = 1, y = 1, label = "No data to produce the figure.")
  ggplot2::ggsave(img_title, path = output_path, bg = "white", width = 4, height = 4)

  invisible()
}
