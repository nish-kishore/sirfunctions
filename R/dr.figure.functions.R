# Figures ----
## Plots ----
#' Timeliness bar graph at the country level
#' @import ggplot2 forcats
#' @param int.data summary table with timeliness intervals at the country level
#' @param output_path path where to output the figure
#'
#' @returns ggplot object
#' @export
generate_ctry_timeliness_graph <- function(int.data,
                                           output_path = Sys.getenv("DR_FIGURE_PATH")) {
  timely_nation <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = int.data,
      ggplot2::aes(
        x = factor(year),
        y = medi,
        fill = forcats::fct_rev(type)
      ),
      position = "stack",
      stat = "identity"
    ) +
    ggplot2::geom_text(
      data = int.data,
      ggplot2::aes(
        x = factor(year),
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

#' Timeliness interval bar graph at a province level
#' @import ggplot2
#' @param int.data summary table of median timeliness intervals at a province level
#' @param afp.prov.year.lab labels for AFP dataset summarized by year
#' @param output_path where to save the figure
#'
#' @returns ggplot object
#' @export
generate_prov_timeliness_graph <- function(int.data,
                                           afp.prov.year.lab,
                                           output_path = Sys.getenv("DR_FIGURE_PATH")) {
  prov.time.2 <- dplyr::left_join(
    int.data,
    afp.prov.year.lab,
    by = c(
      "year" = "year",
      "adm1guid" = "adm1guid",
      "prov" = "prov"
    )
  ) |>
    dplyr::filter(medi >= 0)

  timely_prov <- ggplot2::ggplot(prov.time.2 |>
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


#' Generate epicurve of AFP cases by year
#' @import dplyr ggplot2
#' @param ctry.data RDS data countaining polio data for a country
#' @param start_date start date of the desk review
#' @param end_date end date of the desk review
#' @param output_path where to save the figure
#'
#' @returns ggplot object
#' @export
generate_afp_epicurve <- function(ctry.data,
                                  start_date,
                                  end_date = lubridate::today(),
                                  output_path = Sys.getenv("DR_FIGURE_PATH")) {
  afp.epi.date.filter <- ctry.data$afp.epi %>%
    dplyr::filter(dplyr::between(yronset, as.numeric(lubridate::year(start_date)), as.numeric(lubridate::year(end_date))))

  case.num.labs <- dplyr::reframe(
    dplyr::group_by(afp.epi.date.filter, .data$yronset),
    labs = paste0(.data$yronset, " (N = ", sum(.data$afp.cases), ")")
  ) %>%
    dplyr::distinct(.)

  afp.epi.date.filter1 <- dplyr::left_join(afp.epi.date.filter,
    case.num.labs,
    by = c("yronset" = "yronset")
  )

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
#' @import dplyr lubridate ggplot2 forcats
#' @param afp.by.month.prov table summarizing afp cases by month and province
#' @param start_date start date of the desk review
#' @param end_date cutoff date of calculating the number of cases
#' @param output_path where to save the figure
#'
#' @returns ggplot object
#' @export
generate_afp_prov_year <- function(afp.by.month.prov,
                                   start_date,
                                   end_date = lubridate::today(),
                                   output_path = Sys.getenv("DR_FIGURE_PATH")) {
  afp.month.prov.g <- afp.by.month.prov |>
    dplyr::filter(dplyr::between(year, lubridate::year(start_date), lubridate::year(end_date)), !is.na(prov))

  afp.month.prov.g$case.cat <- factor(afp.month.prov.g$case.cat, levels = c(c("0", "1", "2-5", "6-9", "10+")))

  afp.dets.prov.year <- ggplot2::ggplot(
    afp.month.prov.g |>
      dplyr::arrange(.data$u15pop),
    ggplot2::aes(
      x = mon.year2,
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







#' Dot plot of virus detections in ES sites
#' @importFrom cli cli_alert_warning
#' @importFrom dplyr arrange between count distinct filter pull select
#' @importFrom ggplot2 aes facet_grid geom_point geom_rect ggplot ggsave label_wrap_gen scale_color_manual scale_fill_manual scale_x_date theme_bw xlab ylab
#' @importFrom lubridate year years
#' @importFrom scales brewer_pal
#' @param ctry.data RDS file of polio data for a country
#' @param es.data.long AFP data with viral detection columns
#' @param es_start_date start date of ES data
#' @param es_end_date end date of ES data
#' @param vaccine_types named list with vaccine types with color associations
#' @param detection_types named list with detection types with color associations
#' @param output_path where to save the figure
#'
#' @return ggplot object dot plot
#' @export
generate_es_site_det <- function(ctry.data,
                                 es.data.long,
                                 es_start_date = (end_date - lubridate::years(1)),
                                 es_end_date = end_date,
                                 output_path = Sys.getenv("DR_FIGURE_PATH"),
                                 vaccine_types = NULL,
                                 detection_types = NULL) {
  es.data.long <- es.data.long |>
    dplyr::filter(dplyr::between(collect.date, es_start_date, es_end_date))

  sias <- ctry.data$sia %>%
    dplyr::filter(status == "Done") %>%
    dplyr::filter(yr.sia >= lubridate::year(es_start_date) &
      yr.sia <= lubridate::year(es_end_date)) %>%
    dplyr::filter(province %in% es.data.long$ADM1_NAME)

  sias$activity.start.date <- as.Date(sias$activity.start.date)
  sias$activity.end.date <- as.Date(sias$activity.end.date)

  minsy <- dplyr::count(
    sias,
    .data$yr.sia,
    .data$province,
    .data$activity.start.date,
    .data$activity.end.date,
    .data$vaccine.type
  )

  colnames(minsy)[colnames(minsy) == "province"] <- "ADM1_NAME"

  default_vaccine_type <- c(
    "nOPV2" = "blue",
    "bOPV" = "coral1",
    "mOPV2" = "purple"
  )

  default_detections <- c(
    "No EV isolated" = "#f2f2f2",
    "NPEV only" = "darkgrey",
    "VDPV2" = "darkred",
    "Sabin 1" = scales::brewer_pal(palette = "Set1")(9)[1],
    "Sabin 2" = scales::brewer_pal(palette = "Set1")(9)[8],
    "Sabin 1/Sabin 3" = scales::brewer_pal(palette = "Set1")(9)[2],
    "Sabin 3" = scales::brewer_pal(palette = "Set1")(9)[3],
    "Sabin 1/Sabin 3/VDPV2" = scales::brewer_pal(palette = "Set1")(9)[4],
    "Sabin 1/VDPV2" = scales::brewer_pal(palette = "Set1")(9)[5],
    "Sabin 3/VDPV2" = scales::brewer_pal(palette = "Set1")(9)[6],
    "Sabin 1 or Sabin 3" = scales::brewer_pal(palette = "Set1")(9)[6],
    "Sabin 1/3" = scales::brewer_pal(palette = "Set1")(9)[2],
    "Sabin 1/3 and VDPV2" = scales::brewer_pal(palette = "Set1")(9)[5]
  )

  if (is.null(vaccine_types)) {
    vaccine_types <- default_vaccine_type
  }

  if (is.null(detection_types)) {
    detection_types <- default_detections
  }

  # Check whether the vaccine types and detection are present in the ggplot
  minsy_vaccine_types <- minsy |>
    dplyr::select(.data$vaccine.type) |>
    unique() |>
    dplyr::pull()

  es.data.long_all_dets <- es.data.long |>
    dplyr::select(.data$all_dets) |>
    unique() |>
    dplyr::pull()

  miss_vaccine <- minsy_vaccine_types[!(minsy_vaccine_types %in% names(default_vaccine_type))]
  miss_dets <- es.data.long_all_dets[!(es.data.long_all_dets %in% names(default_detections))]


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

  new.site <- es.data.long %>%
    dplyr::filter(early.dat >= min(collect.date) &
      early.dat <= max(collect.date)) %>%
    dplyr::distinct(.data$site.name, .data$early.dat)


  minny <- min(es.data.long$collect.date) - 7
  maxy <- max(es.data.long$collect.date) + 7

  es.site.det <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = es.data.long |>
        dplyr::arrange(ADM1_NAME),
      ggplot2::aes(x = collect.date, y = site.name, col = all_dets),
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
      data = es.data.long |>
        dplyr::arrange(ADM1_NAME),
      ggplot2::aes(x = collect.date, y = site.name, col = all_dets),
      pch = 19,
      size = 3
    ) +
    ggplot2::geom_point(
      data = es.data.long |>
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

#' Generate ES timeliness scatterplot
#' @importFrom dplyr between count filter rename
#' @importFrom ggplot2 aes element_text geom_hline geom_point ggplot ggsave labs position_jitter scale_y_continuous theme theme_classic
#' @importFrom lubridate years
#' @importFrom scales number_format
#' @param es.data ES data
#' @param es_start_date start date of ES
#' @param output_path where to save the figure
#' @param es_end_date end date of ES
#'
#' @return ggplot scatterplot for timeliness
#' @export
generate_es_timely <- function(es.data,
                               es_start_date = (end_date - lubridate::years(1)),
                               es_end_date = end_date,
                               output_path = Sys.getenv("DR_FIGURE_PATH")) {
  es.data <- es.data |>
    dplyr::filter(dplyr::between(collect.date, es_start_date, es_end_date))

  es.data$timely <-
    difftime(
      as.Date(es.data$date.received.in.lab, format = "%d/%m/%Y"),
      es.data$collect.date,
      units = "days"
    )

  per.time <- es.data %>%
    dplyr::count(timely > 3) %>%
    dplyr::rename(c("timely" = `timely > 3`, "n" = "n"))
  # The number that are false are the percentage timely

  per.timely.title <- paste0(
    round(
      100 * dplyr::filter(per.time, timely == FALSE)["n"] / sum(per.time$n),
      0
    ),
    "% of samples were shipped to lab within 3 days of collection - \n",
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

#' Generate zero-dose children barcharts
#' @importFrom dplyr group_by filter between n summarize
#' @importFrom ggplot2 aes geom_bar geom_text ggplot ggsave labs scale_fill_manual scale_y_continuous xlab ylab
#' @importFrom ggpubr theme_pubr
#' @importFrom scales percent
#' @param ctry.data RDS file containing polio data of country
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#' @param output_path where to save the figure
#'
#' @return ggplot barplot
#' @export
generate_case_num_dose_g <- function(ctry.data,
                                     start_date,
                                     end_date,
                                     output_path = Sys.getenv("DR_FIGURE_PATH")) {
  dose.num.cols <- c(
    "0" = "#C00000",
    "1-2" = "#FFC000",
    "3" = "#92D050",
    "4+" = "#548235",
    "Missing" = "#A5A5A5"
  )

  ### Create zero dose graphs
  # Cats - 0, 1-2, 3, 4+
  dcat.yr.prov <- dplyr::summarize(
    dplyr::group_by(
      ctry.data$afp.all.2 |>
        dplyr::filter(
          date >= start_date &
            date <= end_date,
          cdc.classification.all2 == "NPAFP",
          dplyr::between(age.months, 6, 59)
        ),
      dose.cat,
      year,
      prov
    ),
    freq = dplyr::n()
  )

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


#' Generate the ISS/eSURV barplot
#' @import dplyr ggplot2
#' @param iss.data tibble of ISS data
#' @param start_date start date of the desk review
#' @param end_date end date of the desk review
#' @param output_path where to save the figure
#'
#' @return ggplot object of a barplot
#' @export
generate_iss_barplot <- function(iss.data = NULL,
                                 start_date,
                                 end_date,
                                 output_path = Sys.getenv("DR_FIGURE_PATH")) {
  if (is.null(iss.data)) {
    return(message("No ISS data attached."))
  }

  if (!"today_date" %in% names(iss.data)) {
    iss.data <- iss.data |> dplyr::mutate(today_date = today)
  }


  iss.data2.1 <- iss.data %>%
    dplyr::filter(dplyr::between(today_date, start_date, end_date))

  iss.data3 <- iss.data2.1 %>%
    dplyr::group_by(.data$month, .data$year, .data$priority_level) %>%
    dplyr::summarize(freq = dplyr::n()) %>%
    dplyr::filter(dplyr::between(.data$year, lubridate::year(.data$start_date), lubridate::year(.data$end_date)))

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
#' Generate a map of population data
#' @importFrom dplyr filter left_join
#' @importFrom ggplot2 aes element_blank element_text geom_sf ggplot ggsave ggtitle labs scale_fill_distiller scale_size_identity theme
#' @importFrom ggrepel geom_label_repel
#' @importFrom lubridate year
#' @importFrom sf st_crop
#' @importFrom scales comma
#' @param ctry.data RDS object
#' @param prov.shape province of most recent shape file
#' @param end_date end date of the desk review
#' @param output_path where to save the figure
#'
#' @returns ggplot object
#' @export
generate_pop_map <- function(ctry.data,
                             prov.shape,
                             end_date,
                             output_path = Sys.getenv("DR_FIGURE_PATH")) {
  prov.pop <- ctry.data$prov.pop %>%
    dplyr::filter(year == lubridate::year(end_date))

  # Merge with province
  shape.prov.pop <-
    dplyr::left_join(prov.shape, prov.pop, by = c("GUID" = "adm1guid"))

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
      ggplot2::aes(label = CITY_NAME, geometry = .data$geometry),
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
    ggplot2::labs(fill = "Under-15 pop", caption = "- Under 15 population is shown at the province level\n- Major roads are shown in black\n- Population centers are shown in blue") +
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
      plot.caption = ggplot2::element_text(hjust = 0, size = 11),
      legend.background = ggplot2::element_blank()
    )

  ggplot2::ggsave("pop.map.png",
    plot = pop.map,
    path = output_path,
    width = 8,
    height = 8
  )
  return(pop.map)
}

#' Generate a map of population by district
#' @importFrom dplyr filter left_join
#' @importFrom ggplot2 aes element_blank element_text geom_sf ggplot ggsave ggtitle labs scale_fill_distiller scale_size_identity theme
#' @importFrom ggrepel geom_label_repel
#' @importFrom lubridate year
#' @importFrom scales comma
#' @param ctry.data RDS file of polio country data
#' @param ctry.shape recent shape file of country
#' @param prov.shape recent shape file of province
#' @param dist.shape recent shape file of district
#' @param end_date end date of the desk review
#' @param output_path where to save the figure
#'
#' @returns ggplot object
#' @export
generate_dist_pop_map <- function(ctry.data,
                                  ctry.shape,
                                  prov.shape,
                                  dist.shape,
                                  end_date,
                                  output_path = Sys.getenv("DR_FIGURE_PATH")) {
  prov.pop <- ctry.data$prov.pop %>%
    dplyr::filter(year == lubridate::year(end_date))

  dist.pop <- ctry.data$dist.pop %>%
    dplyr::filter(year == lubridate::year(end_date))

  shape.prov.pop <-
    dplyr::left_join(prov.shape, prov.pop, by = c("GUID" = "adm1guid"))

  shape.dist.pop <-
    dplyr::left_join(dist.shape, dist.pop, by = c("GUID" = "adm2guid"))


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
      ggplot2::aes(label = ADM1_NAME, geometry = .data$SHAPE),
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
      plot.caption = ggplot2::element_text(hjust = 0, size = 11),
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

#' Generate a map of AFP cases
#' @import dplyr ggplot2 lubridate
#' @param ctry.data RDS object containing polio country data
#' @param ctry.shape recent country shapefile
#' @param prov.shape  recent province shapefile
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#' @param output_path where to save the figure
#'
#' @returns ggplot object
#' @export
generate_afp_case_map <- function(ctry.data,
                                  ctry.shape,
                                  prov.shape,
                                  start_date,
                                  end_date = lubridate::today(),
                                  output_path = Sys.getenv("DR_FIGURE_PATH")) {
  afp.case.map.filter <- ctry.data$afp.all %>%
    dplyr::filter(dplyr::between(as.Date(date.onset), start_date, end_date)) |>
    dplyr::mutate(year = as.factor(year))

  if (nrow(afp.case.map.filter) == 0) {
    stop("No data available for the specified date range.")
  }

  afp.case.map.filter <- afp.case.map.filter |>
    dplyr::filter(!(
      cdc.class %in% c("PENDING", "NPAFP", "UNKNOWN", "NOT-AFP", "LAB PENDING")
    ))

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
    ggplot2::scale_color_manual(
      values = sirfunctions::f.color.schemes("para.case"),
      name = "Case type",
      drop = F
    ) +
    ggplot2::ggtitle(paste(
      "Paralytic Polio and Compatible Cases",
      lubridate::year(start_date),
      "-",
      lubridate::year(end_date)
    )) +
    # NOTE: IF THERE ARE NONE IT NEEDS TO THROW AN ERROR
    sirfunctions::f.plot.looks("epicurve") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  if (nrow(afp.case.map.filter) != 0) {
    afp.case.map <- afp.case.map +
      ggplot2::facet_wrap(~year, ncol = 4)
  }

  ggplot2::ggsave(
    "afp.case.map.png",
    plot = afp.case.map,
    path = output_path,
    width = 14,
    height = 4
  )

  return(afp.case.map)
}

#' Map of NPAFP rate by province
#' @import dplyr ggplot2 lubridate sf
#' @param prov.extract province NPAFP rate
#' @param ctry.shape recent country shape
#' @param prov.shape recent province shape
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#' @param output_path where to save the figure
#'
#' @returns ggplot object
#' @export
generate_npafp_maps <- function(prov.extract,
                                ctry.shape,
                                prov.shape,
                                start_date,
                                end_date,
                                output_path = Sys.getenv("DR_FIGURE_PATH")) {
  provnpafp <- prov.extract

  provnpafp$cats <- cut(
    provnpafp$npafp_rate,
    breaks = c(-1, 0, 1, 2, 3, Inf),
    right = F,
    labels = c("Zero NPAFP cases", "<1", "1-<2", "2-<3", "3+")
  )

  prov.cut <- provnpafp %>%
    dplyr::mutate(cats = as.character(.data$cats)) %>%
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
    dplyr::full_join(prov.shape, prov.cut, by = c("GUID" = "adm1guid")) %>%
    dplyr::filter(year <= lubridate::year(end_date) & year >= lubridate::year(start_date))

  # Labels for provinces meeting NPAFP rate
  # How many provinces meet >2 NPAFP?
  prov.2npafp <- provnpafp %>%
    dplyr::group_by(.data$year, .data$adm1guid, .data$prov) %>%
    dplyr::summarize(meet2 = sum(npafp_rate >= 2, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
      num.meet2 = sum(meet2, na.rm = T),
      len.year = length(year)
    ) %>%
    dplyr::mutate(
      labs = paste0(
        .data$num.meet2,
        "/",
        .data$len.year,
        " (",
        round(100 * .data$num.meet2 / .data$len.year, 0),
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
      size = 2,
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

#' Generate map of NPAFP rates by district
#' @import dplyr ggplot2 sf lubridate
#' @param dist.extract NPAFP rates by district
#' @param ctry.shape recent country shapefile
#' @param prov.shape recent province shapefile
#' @param dist.shape recent district shapefile
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#' @param output_path where to save the figure
#'
#' @returns ggplot object
#' @export
generate_npafp_maps_dist <- function(dist.extract,
                                     ctry.shape,
                                     prov.shape,
                                     dist.shape,
                                     start_date,
                                     end_date,
                                     output_path = Sys.getenv("DR_FIGURE_PATH")) {
  distnpafp <- dist.extract

  distnpafp$cats <- cut(
    distnpafp$npafp_rate,
    breaks = c(-1, 0, 1, 2, 3, Inf),
    right = F,
    labels = c("Zero NPAFP cases", "<1", "1-<2", "2-<3", "3+")
  )

  # For those with no cases --> separate out districts with u15pop >100K and <100K

  dist.cut <- distnpafp %>%
    dplyr::mutate(cats = as.character(.data$cats)) %>%
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
    dplyr::left_join(dist.shape, dist.cut, by = c("GUID" = "adm2guid"))

  # How many districts meet >2 NPAFP?
  dist.2npafp <- distnpafp %>%
    dplyr::group_by(.data$year, .data$adm2guid, .data$dist) %>%
    dplyr::summarize(meet2 = sum(npafp_rate >= 2, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
      num.meet2 = sum(meet2, na.rm = T),
      len.year = length(year)
    ) %>%
    dplyr::mutate(
      labs = paste0(
        .data$num.meet2,
        "/",
        .data$len.year,
        " (",
        round(100 * .data$num.meet2 / .data$len.year, 0),
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
    # ggplot2::geom_sf(data = ctry.data$dist, color = "black", fill = "lightgrey", size = .5) +
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
      size = 2,
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
#' @import dplyr ggplot2 sf lubridate
#' @param ctry.data RDS file for polio data of a country
#' @param pstool stool adequacy at province level
#' @param ctry.shape recent country shapefile
#' @param prov.shape recent province shapefile
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#' @param output_path where to save the figure
#'
#' @returns ggplot object
#' @export
generate_stool_ad_maps <- function(ctry.data,
                                   pstool,
                                   ctry.shape,
                                   prov.shape,
                                   start_date,
                                   end_date,
                                   output_path = Sys.getenv("DR_FIGURE_PATH")) {
  allafp <- ctry.data$afp.all.2 %>%
    dplyr::filter(date >= start_date & date <= end_date) %>%
    dplyr::reframe(
      dplyr::group_by(
        ctry.data$afp.all.2,
        .data$cdc.classification.all2,
        .data$adm1guid,
        .data$year
      ),
      freq = dplyr::n()
    ) %>%
    dplyr::filter(.data$cdc.classification.all2 != "NOT-AFP")

  allprov <- ctry.data$prov.pop[, c("adm1guid", "year", "prov")] %>%
    dplyr::filter(year >= lubridate::year(start_date) & year <= lubridate::year(end_date))

  all.prov.afp <- dplyr::left_join(allprov, allafp) %>%
    dplyr::group_by(.data$year, .data$adm1guid, .data$prov) %>%
    dplyr::summarize(allafp = sum(freq, na.rm = T))

  stoolad.p <- dplyr::left_join(all.prov.afp,
    pstool,
    by = c(
      "prov" = "prov",
      "year" = "year",
      "adm1guid" = "adm1guid"
    )
  )

  stoolad.p <- stoolad.p %>%
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
      .data$prop.cat,
      levels = c("Zero AFP cases", "<40%", "40-59%", "60-79%", "80%+")
    ))

  stoolad.nums.p <- stoolad.p %>%
    dplyr::group_by(.data$year, .data$adm1guid, .data$prov) %>%
    dplyr::summarize(meet.stool = sum(per.stool.ad >= 80, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
      num.meet.stool = sum(meet.stool, na.rm = T),
      len.year = length(year)
    ) %>%
    dplyr::mutate(
      labs = paste0(
        .data$num.meet.stool,
        "/",
        .data$len.year,
        " (",
        round(100 * .data$num.meet.stool / .data$len.year, 0),
        "%)",
        " provinces with >= 80% stool adequacy"
      )
    )

  stool.map.p <-
    dplyr::left_join(prov.shape, stoolad.p, by = c("GUID" = "adm1guid"))

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
      size = 3,
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

#' Stool adequacy map by district
#' @import dplyr ggplot2 sf lubridate
#' @param ctry.data RDS file of polio data for a country
#' @param dstool district stool adequacy
#' @param ctry.shape recent country shapefile
#' @param dist.shape recent district shapefile
#' @param prov.shape recent province shapefile
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#' @param output_path where to save the figure
#'
#' @returns ggplot object
#' @export
generate_stool_ad_maps_dist <- function(ctry.data,
                                        dstool,
                                        ctry.shape,
                                        prov.shape,
                                        dist.shape,
                                        start_date,
                                        end_date,
                                        output_path = Sys.getenv("DR_FIGURE_PATH")) {
  # Get coordinates for maps that are plotted
  ctcoord <- as.data.frame(sf::st_coordinates(ctry.shape))
  # Put text at 10% below the minimum X and Y coordinates for each map
  adjy <- (range(ctcoord$Y)[1] - range(ctcoord$Y)[2]) * .1

  allafp.d <- ctry.data$afp.all.2 %>%
    dplyr::filter(date >= start_date & date <= end_date) %>%
    dplyr::reframe(
      dplyr::group_by(
        ctry.data$afp.all.2,
        .data$cdc.classification.all2,
        .data$adm2guid,
        .data$year
      ),
      freq = dplyr::n()
    ) %>%
    dplyr::filter(cdc.classification.all2 != "NOT-AFP")

  alldist <- ctry.data$dist.pop[, c("adm2guid", "year", "prov", "dist")] %>%
    dplyr::filter(year >= lubridate::year(start_date) & year <= lubridate::year(end_date))

  all.dist.afp <- dplyr::left_join(alldist, allafp.d) %>%
    dplyr::group_by(.data$year, .data$adm2guid, .data$prov, .data$dist) %>%
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
      .data$prop.cat,
      levels = c("Zero AFP cases", "<40%", "40-59%", "60-79%", "80%+")
    ))

  stoolad.nums.d <- stoolad.d %>%
    dplyr::group_by(.data$year, .data$adm2guid, .data$dist) %>%
    dplyr::summarize(meet.stool = sum(per.stool.ad >= 80, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
      num.meet.stool = sum(meet.stool, na.rm = T),
      len.year = length(year)
    ) %>%
    dplyr::mutate(
      labs = paste0(
        .data$num.meet.stool,
        "/",
        .data$len.year,
        " (",
        round(100 * .data$num.meet.stool / .data$len.year, 0),
        "%)",
        " districts with >= 80% stool adequacy"
      )
    )

  stool.map.d <-
    dplyr::left_join(dist.shape, stoolad.d, by = c("GUID" = "adm2guid"))

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
      size = 3,
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



#' Map containing timeliness of samples
#' @importFrom cli cli_alert_success cli_alert_warning
#' @importFrom forcats fct_na_value_to_level
#' @importFrom ggplot2 aes element_blank element_text facet_wrap geom_sf ggplot ggsave ggtitle scale_fill_manual theme
#' @importFrom ggpubr annotate_figure get_legend ggarrange text_grob
#' @importFrom lubridate year
#' @importFrom sf sf_use_s2 st_centroid
#' @importFrom tidyr complete pivot_longer
#' @param ctry.data RDS file containing polio data for a country
#' @param ctry.shape recent country shapefile
#' @param prov.shape recent province shapefile
#' @param start_date start date of desk review
#' @param end_date end dtae of desk review
#' @param mark_x whether to put a mark on where AFP cases are less than 5
#' @param pt_size size of the marks
#' @param output_path where to save the figure
#'
#' @returns ggplot object
#' @export
generate_timeliness_maps <- function(ctry.data,
                                     ctry.shape,
                                     prov.shape,
                                     start_date,
                                     end_date,
                                     mark_x = T,
                                     pt_size = 4,
                                     output_path = Sys.getenv("DR_FIGURE_PATH")) {
  long.timely <- ctry.data$afp.all.2 %>%
    dplyr::select(
      .data$epid,
      .data$noti.7d.on,
      .data$inv.2d.noti,
      .data$coll.3d.inv,
      .data$ship.3d.coll,
      .data$year,
      .data$prov,
      .data$adm1guid
    ) %>%
    tidyr::pivot_longer(
      !c(.data$epid, .data$year, .data$prov, .data$adm1guid),
      names_to = "type",
      values_to = "value"
    ) %>%
    dplyr::group_by(.data$year, .data$type, .data$prov, .data$adm1guid) %>%
    dplyr::summarize(prop = sum(value, na.rm = T) / dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year >= lubridate::year(start_date) &
      year <= lubridate::year(end_date)) %>%
    tidyr::complete(.data$year, .data$prov, .data$type)


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
    dplyr::filter(.data$year >= lubridate::year(start_date) &
      year <= lubridate::year(end_date)) %>%
    tidyr::complete(.data$year, .data$prov, fill = list(case.num = 0))

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
    dplyr::left_join(prov.shape, long.timely, by = c("GUID" = "adm1guid"))
  time.map <-
    dplyr::full_join(time.map, all.case, by = c("GUID" = "adm1guid", "year" = "year"))

  time.map <- time.map %>%
    dplyr::mutate(prop = as.character(.data$prop)) %>%
    dplyr::mutate(prop = ifelse(.data$case.num == 0, "No AFP cases", .data$prop)) %>%
    dplyr::mutate(prop = factor(
      .data$prop,
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
    dplyr::group_by(.data$year, .data$ADM1_NAME) %>%
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


#' Generate ES detection map
#' @import dplyr ggplot2 ggrepel
#' @param es.data ES data for a country
#' @param es.data.long ES data summary and in long format
#' @param ctry.shape recent country shapefile
#' @param prov.shape recent province shapefile
#' @param es_start_date ES start date
#' @param es_end_date ES end date
#' @param output_path where to save the figure
#'
#' @return ggplot map of ES detections
#' @export
generate_es_det_map <- function(es.data,
                                es.data.long,
                                ctry.shape,
                                prov.shape,
                                es_start_date = (end_date - lubridate::years(1)),
                                es_end_date = end_date,
                                output_path = Sys.getenv("DR_FIGURE_PATH")) {
  es.data <- es.data |>
    dplyr::filter(dplyr::between(collect.date, es_start_date, es_end_date))

  es.data.long <- es.data.long |>
    dplyr::filter(dplyr::between(collect.date, es_start_date, es_end_date))

  det.rate <- dplyr::summarize(
    dplyr::group_by(es.data.long, site.name),
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
    dplyr::reframe(dplyr::group_by(es.data, .data$site.name),
      lat = .data$lat,
      lng = .data$lng
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

  ggplot2::ggsave(
    "es.det.map.png",
    plot = es.det.map,
    path = output_path,
    width = 9,
    height = 8
  )

  return(es.det.map)
}

#' Generate ISS map
#' @import dplyr sf ggplot2 lubridate
#' @param iss.data tibble of ISS/eSurv data
#' @param start_date start date of desk review
#' @param prov.shape shapefile containing province shapes
#' @param output_path where to save the figure
#' @param end_date end date of desk review
#'
#' @return a ggplot map
#' @export
generate_iss_map <- function(iss.data,
                             prov.shape,
                             start_date,
                             end_date,
                             output_path = Sys.getenv("DR_FIGURE_PATH")) {
  if (is.null(iss.data)) {
    return(message("No ISS data attached."))
  }

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

  pryr <- dplyr::count(iss.data, .data$priority_level, .data$year) %>%
    dplyr::filter(priority_level == "High")

  iss.data2 <- dplyr::full_join(iss.data, pryr)

  iss.data2$labs <- paste0(iss.data$year, "\n(n = ", iss.data2$n, ")")

  iss.data.map <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = prov.shape,
      color = "black",
      fill = NA,
      size = .5
    ) +
    ggplot2::geom_point(
      data = iss.data2 |>
        dplyr::filter(
          year <= lubridate::year(end_date) &
            year >= lubridate::year(start_date) &
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
#' Generate surveillance indicator table
#' @importFrom flextable add_footer_row autofit bold colformat_double flextable set_header_labels theme_booktabs
#' @importFrom dplyr across case_when group_by left_join mutate n summarize
#' @importFrom lubridate year
#' @importFrom stringr str_to_upper
#' @importFrom tidyr replace_na
#' @param ctry.data RDS file containing polio data of a country
#' @param ctry.extract country NPAFP rate
#' @param dis.extract district NPAFP rate
#' @param cstool stool adequacy at country level
#' @param dstool stool adequacy at a district level
#' @param afp.case AFP cases
#' @param country_name name of the country
#'
#' @return a flex table object for timeliness indicators
#' @export
generate_surv_ind_tab <- function(ctry.data,
                                  ctry.extract,
                                  dis.extract,
                                  cstool,
                                  dstool,
                                  afp.case,
                                  country_name = Sys.getenv("DR_COUNTRY")) {


  if (!requireNamespace("janitor", quietly = TRUE)) {
    stop('Package "janitor" must be installed to use this function.',
         .call = FALSE)
  }

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop('Package "tibble" must be installed to use this function.',
         .call = FALSE)
  }

  dist.ind.afp <- dplyr::left_join(dis.extract,
    dstool,
    by = c(
      "prov" = "prov",
      "dist" = "dist",
      "year" = "year"
    )
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
    dplyr::mutate(prop.dist.adeq = .data$tot.dist.adeq / .data$tot.dist.pop * 100)

  ctry.ind.afp <- dplyr::left_join(ctry.extract, cstool, by = c("year", "adm0guid"))
  ctry.ind.afp <- dplyr::left_join(ctry.ind.afp, afp.case, by = c("year"))
  temp.ind.tab <- dplyr::left_join(ctry.ind.afp, meet.ind, by = c("year"))

  ## Districts with population over 100K
  dist.100k <- dis.extract %>%
    dplyr::filter(u15pop >= 100000)

  unique.dist.100k <- ctry.data$dist.pop %>%
    dplyr::filter(ctry == stringr::str_to_upper(country_name) &
      u15pop >= 100000) %>%
    unique() %>%
    dplyr::group_by(.data$year, .data$u15pop, .data$adm2guid) %>%
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
    dplyr::mutate(prop = paste0(.data$ad.dist.100k.num, "/", .data$dist.100k.num))

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

#' Generate table for population
#' @importFrom dplyr across arrange full_join group_by mutate select
#' @importFrom flextable add_header_row align bg bold color flextable fp_border_default hline set_header_df theme_booktabs vline
#' @importFrom lubridate year
#' @importFrom stringr str_replace
#' @importFrom tidyr pivot_wider replace_na
#' @param prov.case.ind  case indicator at province level
#' @param pstool stool adequacy at porvince level
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#'
#' @return a flextable containing population indicators
#' @export
generate_pop_tab <- function(prov.case.ind,
                             pstool,
                             start_date,
                             end_date) {
  sub.prov.case.ind <- prov.case.ind %>%
    dplyr::select(
      .data$year,
      .data$n_npafp,
      .data$u15pop,
      .data$prov,
      .data$npafp_rate
    )

  sub.pstool <- pstool %>%
    dplyr::select(.data$year, .data$per.stool.ad, .data$prov) |>
    dplyr::filter(!is.na(prov))

  sub.prov.join <- dplyr::full_join(sub.prov.case.ind, sub.pstool, by = c("year", "prov")) %>%
    dplyr::arrange(.data$prov, .data$year)

  sub.prov.join <- sub.prov.join %>%
    dplyr::group_by(.data$prov) %>%
    dplyr::mutate(diff = dplyr::lag(.data$n_npafp)) %>%
    dplyr::mutate(diff_per = round(100 * (.data$n_npafp - dplyr::lag(.data$n_npafp)) /
      dplyr::lag(.data$n_npafp), 1)) %>%
    dplyr::mutate(dplyr::across(c(per.stool.ad, diff, diff_per, n_npafp), \(x) round(x, 0))) %>%
    dplyr::mutate(dplyr::across(c(npafp_rate), \(x) round(x, 1))) |>
    dplyr::mutate(u15pop = round(.data$u15pop, 0)) |>
    dplyr::filter(!is.na(prov))

  date.analysis <- seq(lubridate::year(start_date), lubridate::year(end_date), 1)
  pop.date.analysis <- paste0("u15pop_", date.analysis[1:length(date.analysis) - 1])

  sub.prov.join.wide <- tidyr::pivot_wider(
    sub.prov.join,
    names_from = year,
    values_from = c(
      .data$per.stool.ad,
      .data$diff,
      .data$diff_per,
      .data$n_npafp,
      .data$npafp_rate,
      .data$u15pop
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

#' Table containing stool adequacy at the country level
#' @importFrom dplyr across between case_when count group_by left_join mutate rename select summarize
#' @importFrom flextable add_footer_row as_grouped_data autofit bold flextable set_header_labels theme_booktabs
#' @importFrom lubridate year
#' @importFrom tidyr replace_na
#' @param ctry.data RDS file containing polio data for a country
#' @param stool.data AFP data with stool adequacy columns
#' @param cstool stool adequacy at the country level
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#'
#' @return a flextable containing stool adequacy at the country level
#' @export
generate_inad_tab <- function(ctry.data,
                              stool.data,
                              cstool,
                              start_date,
                              end_date) {

  if (!requireNamespace("janitor", quietly = TRUE)) {
    stop('Package "janitor" must be installed to use this function.',
         .call = FALSE)
  }

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop('Package "tibble" must be installed to use this function.',
         .call = FALSE)
  }

  stool.sub <- cstool[, c(
    "year",
    "num.adj.w.miss",
    "num.inadequate",
    "per.stool.ad",
    "afp.cases"
  )]

  # Late collection (%) among inadequate
  inads <- stool.data %>%
    dplyr::filter(dplyr::between(date, start_date, end_date)) %>%
    dplyr::filter(adequacy.final == "Inadequate")

  # Timeliness
  late.inads <- inads %>%
    dplyr::mutate(
      timelystool = dplyr::case_when(
        ontostool1 > 13 |
          ontostool1 < 0 |
          is.na(stool1tostool2) == T |
          ontostool2 > 14 |
          ontostool2 < 1 |
          stool1tostool2 < 1 ~ "Not Timely",
        T ~ "Timely"
      )
    )


  late.stool <- dplyr::count(late.inads, .data$year, .data$timelystool) %>%
    dplyr::filter(timelystool == "Not Timely")
  # variables exclude bad dates as 77 or Unable to Assess
  # Missing dates treated as absence of collection of stool as there is no variable
  # that specifies stool was not collected

  # No stool/one stool/one stool among inadequate
  dplyr::count(
    inads,
    .data$stoolmissing,
    .data$stool1missing,
    .data$stool2missing
  )

  stool.miss.any <- dplyr::summarize(
    dplyr::group_by(inads, year),
    stoolmiss = sum(stoolmissing, stool1missing, stool2missing, na.rm = T)
  )


  # Poor condition among inadequate
  dplyr::count(inads, .data$stool.1.condition, .data$stool.2.condition)

  cond.poor <- dplyr::filter(
    inads,
    stool.1.condition == "Poor" |
      stool.2.condition == "Poor"
  )

  yrs <- as.data.frame(seq(lubridate::year(start_date), lubridate::year(end_date), 1))
  names(yrs) <- "year"

  cond.poor.num <- dplyr::count(cond.poor, year)
  cond.poor.num <- dplyr::left_join(yrs, cond.poor.num, by = c("year" = "year")) %>%
    dplyr::mutate(dplyr::across(c(n), \(x) tidyr::replace_na(x, 0)))

  # ALL AFP
  afps.all <- ctry.data$afp.all.2 %>%
    dplyr::filter(dplyr::between(date, start_date, end_date))

  # 1 stool within 14 days of onset (+condition)
  good.cond.1 <- dplyr::count(
    afps.all,
    (.data$ontostool1 <= 14 | .data$ontostool2 <= 14) &
      (
        .data$stool.1.condition == "Good" |
          is.na(.data$stool.1.condition)
      ) &
      (
        .data$stool.2.condition == "Good" |
          is.na(.data$stool.2.condition)
      ),
    year
  )

  colnames(good.cond.1)[1] <- "conds"
  good.cond.1 <- good.cond.1 %>%
    dplyr::filter(conds == TRUE)
  # 2 stools within 21 days of onset (+condition)
  good.cond.2 <- dplyr::count(
    afps.all,
    .data$stool1missing == 0 &
      .data$stool2missing == 0 &
      .data$ontostool2 <= 21 &
      (
        .data$stool.1.condition == "Good" |
          is.na(.data$stool.1.condition)
      ) &
      (
        .data$stool.2.condition == "Good" |
          is.na(.data$stool.2.condition)
      ),
    year
  )
  colnames(good.cond.2)[1] <- "conds"
  good.cond.2 <- good.cond.2 %>%
    dplyr::filter(conds == TRUE)

  # Time to lab
  # !!! daysstooltolab is not a variable in afp.all.2, had to recreate it
  medi_lab <- dplyr::summarize(
    dplyr::group_by(ctry.data$afp.all.2, year),
    medi = median(ctry.data$afp.all.2$daysstooltolab, na.rm = T)
  )

  # Bind together tables
  allinadstool <- dplyr::left_join(stool.sub, late.stool, by = "year") %>%
    dplyr::select(-.data$timelystool) %>%
    dplyr::rename("timelystool" = "n") %>%
    dplyr::left_join(stool.miss.any, by = "year") %>%
    dplyr::left_join(cond.poor.num, by = "year") %>%
    dplyr::rename("cond.poor.num" = "n") %>%
    dplyr::left_join(good.cond.1, by = "year") %>%
    dplyr::select(-.data$conds) %>%
    dplyr::rename("good.cond.1" = "n") %>%
    dplyr::left_join(good.cond.2, by = "year") %>%
    dplyr::select(-.data$conds) %>%
    dplyr::rename("good.cond.2" = "n")

  allinadstool$timelyper <- paste0(
    allinadstool$timelystool,
    " (",
    round(
      100 * allinadstool$timelystool / allinadstool$num.inadequate,
      0
    ),
    "%)"
  )
  allinadstool$poorper <- paste0(
    allinadstool$cond.poor.num,
    " (",
    round(
      100 * allinadstool$cond.poor.num / allinadstool$num.inadequate,
      0
    ),
    "%)"
  )
  allinadstool$missingper <- paste0(
    allinadstool$stoolmiss,
    " (",
    round(100 * allinadstool$stoolmiss / allinadstool$num.inadequate, 0),
    "%)"
  )
  allinadstool$good.cond.1per <- paste0(
    round(100 * allinadstool$good.cond.1 / allinadstool$afp.cases, 0),
    "% (",
    allinadstool$good.cond.1,
    "/",
    allinadstool$afp.cases,
    " cases)"
  )
  allinadstool$good.cond.2per <- paste0(
    round(100 * allinadstool$good.cond.2 / allinadstool$afp.cases, 0),
    "% (",
    allinadstool$good.cond.2,
    "/",
    allinadstool$afp.cases,
    " cases)"
  )

  allinadstool$per.stool.ad <- round(allinadstool$per.stool.ad, 1)


  # Among inadequate cases
  # Among all AFP cases
  # Transport to the lab (median days)
  # NPENT (%)


  inad.tab <- as.data.frame(t(allinadstool)) %>%
    janitor::row_to_names(row_number = 1) %>%
    tibble::rownames_to_column("type") %>%
    dplyr::filter(
      type %in% c(
        "num.adj.w.miss",
        "num.inadequate",
        "per.stool.ad",
        "timelyper",
        "missingper",
        "poorper",
        "good.cond.1per",
        "good.cond.2per"
      )
    ) %>%
    dplyr::mutate(
      type = dplyr::case_when(
        type == "num.adj.w.miss" ~ "Cases with adequate stools",
        type == "num.inadequate" ~ "Cases with inadequate stools",
        type == "per.stool.ad" ~ "Stool adequacy*",
        type == "afp.cases" ~ "",
        type == "timelyper" ~ "Late collection (%)",
        type == "missingper" ~ "No Stool/one stool",
        type == "poorper" ~ "Poor condition",
        type == "good.cond.1per" ~ "1 stool within 14 days of onset (+ condition)",
        type == "good.cond.2per" ~ "2 stools within 21 days of onset (+ condition)",
        FALSE ~ type
      )
    )

  inad.tab <-
    inad.tab[c(3, 1, 2, 4, 6, 5, 7, 8), ] # Reorder the table to be in the correct order

  inad.tab$sub <- c(
    "",
    "",
    "",
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

#' Table used for those requiring 60 day follow up
#' @importFrom dplyr arrange desc group_by mutate n select summarize
#' @importFrom flextable align bold flextable fontsize set_header_labels theme_booktabs width
#' @param cases.need60day summary table containing those that need 60 day follow-up
#'
#' @return a flextable with 60 day follow up data
#' @export
generate_60_day_tab <- function(cases.need60day) {
  comp.by.year <- cases.need60day |>
    dplyr::group_by(year) |>
    dplyr::summarize(
      inadequate = dplyr::n(),
      got60day = sum(got60day == 1, na.rm = T),
      ontime60day = sum(ontime.60day == 1, na.rm = T),
      compatible = sum(cdc.classification.all2 == "COMPATIBLE"),
      pot.compatible = sum(pot.compatible == 1, na.rm = T),
      missing.fu.date = sum(missing.fu.date == 1, na.rm = T)
    ) |>
    dplyr::mutate(
      per.got60 = round(.data$got60day / .data$inadequate * 100),
      per.ontime60day = round(.data$ontime60day / .data$got60day * 100),
      per.comp = round(.data$compatible / .data$inadequate * 100),
      per.pot.comp = round(.data$pot.compatible / .data$inadequate * 100),
      per.got60.2 = paste(.data$got60day, " ", "(", .data$per.got60, "%", ")", sep = ""),
      per.ontime60day.2 = paste(
        .data$ontime60day,
        " ",
        "(",
        .data$per.ontime60day,
        "%",
        ")",
        sep = ""
      ),
      per.comp.2 = .data$compatible,
      per.pot.comp.2 = .data$pot.compatible,
      per.missing.fu.date = paste(
        .data$missing.fu.date,
        " ",
        "(",
        round(.data$missing.fu.date / .data$inadequate * 100),
        "%",
        ")",
        sep = ""
      )
    ) |>
    dplyr::select(
      .data$year,
      .data$inadequate,
      .data$per.got60.2,
      .data$per.ontime60day.2,
      .data$per.missing.fu.date,
      .data$per.comp.2,
      .data$per.pot.comp.2
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
      per.got60.2 = "Recorded 60-day follow-up",
      per.ontime60day.2 = "Recorded 60-day ontime (out of those visited)",
      per.missing.fu.date = "No. Missing follow up date with findings",
      per.comp.2 = "Compatible cases",
      per.pot.comp.2 = "Potentially compatible cases*"
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
    flextable::width(j = 1:7, width = 2) #|>

  return(tab.60d)
}


#' ES table with indicators
#' @importFrom cli cli_alert cli_alert_info
#' @importFrom dplyr filter arrange between distinct group_by if_else left_join mutate n reframe summarize
#' @importFrom flextable add_header_lines align bold colformat_double flextable fontsize set_header_labels theme_booktabs width
#' @importFrom lubridate years
#' @importFrom readr write_csv
#' @param es.data ES data
#' @param es_start_date ES start date
#' @param es_end_date ES end date
#'
#' @return flextable with ES indicators
#' @export
generate_es_tab <- function(es.data,
                            es_start_date = (end_date - lubridate::years(1)),
                            es_end_date = end_date) {
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
    dplyr::group_by(.data$ADM1_NAME, .data$ADM2_NAME, .data$site.name) |>
    dplyr::summarize(neg_intervals = dplyr::n())
  # sample summary
  sample_summary <- es.data |>
    dplyr::group_by(.data$ADM1_NAME, .data$ADM2_NAME, .data$site.name) |>
    dplyr::summarize(samples = dplyr::n())

  neg_interval_data <- neg_interval_data |>
    dplyr::left_join(sample_summary)

  neg_interval_data <- neg_interval_data |>
    dplyr::mutate(bad_samples = round(.data$neg_intervals / .data$samples * 100, 2))

  # turn negative timely intervals to NA to exclude from calculations
  es.data <- es.data |>
    dplyr::mutate(timely = dplyr::if_else(.data$timely < 0, NA, .data$timely))

  es.tab1 <- es.data %>%
    # dplyr::filter(lubridate::year(collect.date) == lubridate::year(end_date)) %>%
    dplyr::group_by(.data$site.name, .data$ADM1_NAME, .data$ADM2_NAME) %>%
    dplyr::reframe(
      early.dat = format(.data$early.dat, format = "%B %d, %Y"),
      # earliest report to POLIS
      ev.pct = 100 * sum(as.numeric(.data$ev.detect), na.rm = TRUE) / dplyr::n(),
      # percent EV detected
      num.spec = dplyr::n(),
      # number of specimens
      num.spec.bad = sum(is.na(.data$timely)),
      # no of bad specimens excluded
      condition.pct = 100 * sum(.data$sample.condition == "Good", na.rm = T) / dplyr::n(),
      # specimens in good condition
      trans.pct = round(100 * sum(as.numeric(.data$timely) <= 3, na.rm = TRUE) / dplyr::n(), 0),
      # % timely
      med.trans = paste0(
        median(as.numeric(.data$timely), na.rm = T),
        " (",
        min(as.numeric(.data$timely), na.rm = T),
        ", ",
        max(as.numeric(.data$timely), na.rm = T),
        ")"
      ),
      # med (range)
      num.wpv.or.vdpv = sum(.data$wpv, na.rm = T) + sum(.data$vdpv, na.rm = T)
    ) %>% # WPV/VDPV
    dplyr::distinct()

  es.tab1 <- es.tab1 %>%
    dplyr::arrange(.data$ADM1_NAME, .data$ADM2_NAME, .data$site.name)

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
