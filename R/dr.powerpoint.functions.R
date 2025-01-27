# Private functions ----
#' Generate PowerPoint assumptions
#'
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#' @keywords internal
#'
#' @returns list of strings
generate_pptx_assumptions <- function(start_date, end_date) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop(
      'Package "officer" must be installed to use this function.',
      call. = FALSE
    )
  }

  pptx.assumptions <- c(
    "Data sources:",
    paste0("POLIS (data as of ", format(end_date, "%d-%b-%Y"), ")", " unless specified otherwise"),
    "Missing population estimated from the UNDP growth factor from previous year's population.",
    paste0(
      "Timeframe for analysis: ", format(start_date, "%d-%b-%Y"),
      " to ", format(end_date, "%d-%b-%Y")
    ),
    paste0(
      "Some selected figures include additional data (",
      format(start_date, "%b %Y"),
      "-", format(Sys.Date(), "%b %Y"), ")"
    ), # UPDATE AS NEEDED
    "NPAFP Assumptions",
    "NPAFP cases with all pending cases included (Pending Lab and Pending Classification)",
    "Stool Adequacy Assumptions",
    "All AFP cases",
    "Samples with missing stool condition were considered good quality",
    "Samples with bad date data (e.g. collection before onset) were considered inadequate"
  )

  assump <- officer::unordered_list(
    level_list = c(1, 2, 2, 1, 2, 1, 2, 1, 2, 2, 2), # Indentation level for each bullet
    str_list = pptx.assumptions,
    style = officer::fp_text(color = "black", font.size = 17)
  )

  return(assump)
}

# Public functions ----
#' Get path of the PowerPoint template
#'
#' The desk review PowerPoint template is used to build the desk review slide deck.
#' The function will either download the template from the sg-desk-reviews GitHub page
#' or get it locally.
#' @param path `str` Path to the PowerPoint template. If `NULL`,
#' will prompt user to download from the sg-desk-review GitHub repository
#'
#' @returns `str` Local path of the PowerPoint template.
#' @examples
#' \dontrun{
#' get_ppt_template()
#'
#' # If present locally
#' template_path <- "C:/Users/ABC1/Desktop/deskreview_template.pptx"
#' ppt_template <- get_ppt_template(template_path)
#' }
#'
#' @export
get_ppt_template <- function(path = NULL) {
  if (is.null(path)) {
    url <- "https://github.com/nish-kishore/sg-desk-reviews/tree/main/resources"

    stop(paste0("\nPlease download the PPT template file here:\n"), url)
  } else {
    return(file.path(path))
  }
}


#' Generate the desk review slide deck
#'
#' The original function to build the desk review PowerPoint. This function has been
#' superseded by [generate_dr_ppt2()]. The function outputs images to the PowerPoint
#' directly from objects, unlike `generate_dr_ppt2()` which uses images saved in a folder.
#'
#' @param ppt_template_path `str` Sath to the PowerPoint template.
#' @param ctry.data `list` List containing polio data for a country. Either the output of
#' [extract_country_data()] or [init_dr()].
#' @param start_date `str` Start date of desk review.
#' @param end_date `str` End date of desk review.
#' @param pop.map `ggplot` Country pop map.
#' @param pop.map.prov `ggplot` Prov pop map.
#' @param afp.case.map `ggplot` Map of afp cases.
#' @param afp.epi.curve `ggplot` AFP epicurve.
#' @param surv.ind.tab `flextable` Surveillance indicator table.
#' @param afp.dets.prov.year `ggplot` AFP detections for province.
#' @param pop.tab `flextable` Table of population.
#' @param npafp.map `ggplot` NPAFP map for country level.
#' @param npafp.map.dist `ggplot` NPAFP map for district level.
#' @param stool.ad.maps `ggplot` Stool adequacy maps at province.
#' @param stool.ad.maps.dist `ggplot` Stool adequacy maps at district.
#' @param inad.tab.flex `flextable` Inadequate table.
#' @param tab.60d `flextable` 60-day follow-up table.
#' @param case.num.dose.g `ggplot` Immunization rates per year.
#' @param timely_nation `ggplot` Timeliness at country level.
#' @param timely_prov `ggplot` Timeliness at province level.
#' @param mapt_all `ggplot` Map with all indicators.
#' @param es.site.det `ggplot` ES site viral detection.
#' @param es.det.map `ggplot` ES site detection maps.
#' @param es.timely `ggplot` ES timeliness.
#' @param es.table `flextable` ES table.
#' @param country `str` Name of the country.
#' @param ppt_output_path `str` Path where the PowerPoint should be outputted.
#' @returns None.
#' @examples
#' \dontrun{
#' # Assume all figures and tables are assigned to the appropriate variable.
#' template_path <- "C:/Users/ABC1/Desktop/deskreview_template.pptx"
#' generate_dr_ppt(
#'   template_path, ctry.data, start_date, end_date, pop.map,
#'   pop.map, pop.map.prov, afp.case.map, afp.epi.curve,
#'   surv.ind.tab, afp.dets.prov.year, pop.tab, npafp.map,
#'   npafp.map.dist, stool.ad.maps, stool.ad.maps.dist,
#'   inad.tab.flex, tab.60d, case.num.dose.g,
#'   timely_nation, timely_prov,
#'   mapt_all, es.site.det, es.det.map, es.timely,
#'   es.table
#' )
#' }
#'
#' @export
generate_dr_ppt <- function(ppt_template_path, ctry.data, start_date, end_date,
                            pop.map, pop.map.prov, afp.case.map, afp.epi.curve,
                            surv.ind.tab, afp.dets.prov.year, pop.tab, npafp.map,
                            npafp.map.dist, stool.ad.maps, stool.ad.maps.dist,
                            inad.tab.flex, tab.60d, case.num.dose.g,
                            timely_nation, timely_prov,
                            mapt_all, es.site.det, es.det.map, es.timely,
                            es.table,
                            country = Sys.getenv("DR_COUNTRY"),
                            ppt_output_path = Sys.getenv("DR_POWERPOINT_PATH")) {
  if (!requireNamespace("rvg", quietly = TRUE)) {
    stop('Package "rvg" must be installed to use this function.',
      .call = FALSE
    )
  }

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop(
      'Package "officer" must be installed to use this function.',
      call. = FALSE
    )
  }

  assump <- generate_pptx_assumptions(start_date, end_date)

  if (!dir.exists(ppt_output_path)) {
    stop("Output path does not exist. Please try again.")
  }

  if (!file.exists(ppt_template_path)) {
    stop("Ppt template path does not exist. Please try again.")
  }

  ppt_template_path <- get_ppt_template(ppt_template_path)
  tempi <- officer::read_pptx(ppt_template_path)

  officer::layout_summary(tempi)
  officer::layout_properties(x = tempi, layout = "Two Content", master = "1_Office Theme")
  officer::layout_properties(x = tempi, layout = "Title and Content", master = "1_Office Theme")
  officer::layout_properties(x = tempi, layout = "4_tile", master = "1_Office Theme")

  draft_output <- tempi %>%
    # Title slide ----
    officer::add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    officer::ph_with(
      value = paste(ctry.data$name, "DESK REVIEW"),
      location = officer::ph_location_type("ctrTitle")
    ) %>%
    # Table of Contents  ----
    officer::add_slide(layout = "Two Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Table of Contents",
      location = officer::ph_location_type("title")
    ) %>%
    # Analysis notes ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Analysis Notes",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = assump,
      location = officer::ph_location_type("body")
    ) %>%
    # Reference map of country  ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = paste("Reference Map 1", start_date, "-", end_date),
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(rvg::dml(ggobj = pop.map), location = officer::ph_location_type("body")) %>%
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = paste("Reference Map 2", start_date, "-", end_date),
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(rvg::dml(ggobj = pop.map.prov), location = officer::ph_location_type("body")) %>%
    # Paralytic polio and compatible cases map ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = paste("Paralytic Polio and Compatible cases", start_date, "-", end_date),
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(rvg::dml(ggobj = afp.case.map), location = officer::ph_location_type("body")) %>%
    # Phylogenetic tree ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Phylogenetic Tree",
      location = officer::ph_location_type("title")
    ) %>%
    # Overview of Sample transport and processing ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Overview of Sample Transport & Processing",
      location = officer::ph_location_type("title")
    ) %>%
    # Section header: AFP surveillance ----
    officer::add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    officer::ph_with(value = "AFP Surveillance", location = officer::ph_location_type("ctrTitle")) %>%
    # Epi-curve ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = paste("Epicurve", start_date, "-", end_date),
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(rvg::dml(ggobj = afp.epi.curve), location = officer::ph_location_type("body")) %>%
    # POLIS screen shot ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Key message: AFP Surveillance Tracking",
      location = officer::ph_location_type("title")
    ) %>%
    # National surveillance indicators table (MISSING 100K NEEDS CODE REWORK) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "National Surveillance Indicators",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(surv.ind.tab, location = officer::ph_location_type("body")) %>%
    # AFP detections by province ----
    # Tile Chart and Table
    officer::add_slide(layout = "Two Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "AFP Detections by Province",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(rvg::dml(ggobj = afp.dets.prov.year),
      location = officer::ph_location_label("Content Placeholder 2")
    ) %>%
    officer::add_slide(layout = "Two Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "AFP Detections by Province and Year",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(pop.tab, location = officer::ph_location_label("Content Placeholder 3")) %>%
    # NPAFP rate/AFP detection by province by year (maps) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "NPAFP Rate by Province",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(rvg::dml(ggobj = npafp.map), location = officer::ph_location_type("body")) %>%
    # NPAFP rate/AFP detection by district by year (maps) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "NPAFP Rate by District",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(rvg::dml(ggobj = npafp.map.dist), location = officer::ph_location_type("body")) %>%
    # Stool adequacy by province by year (maps) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Stool Adequacy by Province",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(rvg::dml(ggobj = stool.ad.maps), location = officer::ph_location_type("body")) %>%
    # Stool adequacy by dist by year (maps) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Stool Adequacy by District",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(rvg::dml(ggobj = stool.ad.maps.dist), location = officer::ph_location_type("body")) %>%
    # Main issues with stool adequacy table (NOT DONE) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Main issues with stool adequacy",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(inad.tab.flex, location = officer::ph_location_type("body")) %>%
    # Virus isolation in/around SIA rounds (table) (NOT DONE) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Virus isolation in/around SIA rounds",
      location = officer::ph_location_type("title")
    ) %>%
    # 60-day follow-up table by year (table) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "60-day follow-up",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(tab.60d, location = officer::ph_location_type("body")) %>%
    # Possible clusters of potential compatibles (table) (NOT DONE) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Possible clusters of potential compatibles",
      location = officer::ph_location_type("title")
    ) %>%
    # Contact sampling (table) (NOT DONE) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Contact sampling",
      location = officer::ph_location_type("title")
    ) %>%
    # Possible clusters of under-immunized NPAFP cases (table) (NOT DONE) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Immunization Status Among NPAFP cases",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(rvg::dml(ggobj = case.num.dose.g), location = officer::ph_location_type("body")) |>
    # Key Points on AFP surveillance ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Key Points on AFP Surveillance",
      location = officer::ph_location_type("title")
    ) %>%
    # Section header: Active surveillance ----
    officer::add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    officer::ph_with(value = "Active Surveillance", location = officer::ph_location_type("ctrTitle")) %>%
    # Annual active surveillance visits by priority level (bar graph) (NOT DONE)----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Annual active surveillance visits by priority level",
      location = officer::ph_location_type("title")
    ) %>%
    # Active surveillance visits (table) (NOT DONE)----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Active surveillance visits",
      location = officer::ph_location_type("title")
    ) %>%
    # Key points on active surveillance ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Key Points on active Surveillance",
      location = officer::ph_location_type("title")
    ) %>%
    # Section header: AFP Surveillance Timeliness ----
    officer::add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    officer::ph_with(value = "AFP Surveillance Timeliness", location = officer::ph_location_type("ctrTitle")) %>%
    # National trend in timeliness of AFP case detection (bar graph) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "National timeliness",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(rvg::dml(ggobj = timely_nation), location = officer::ph_location_type("body")) %>%
    # Timeliness at provincial level (bar graph) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Timeliness by province and year",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(rvg::dml(ggobj = timely_prov), location = officer::ph_location_type("body")) %>%
    # Timeliness across provinces (multi map) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Timeliness by province and year",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(rvg::dml(ggobj = mapt_all), location = officer::ph_location_type("body")) %>%
    # Key points on AFP Surveillance Timeliness ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Key Points on AFP Surveillance Timeliness",
      location = officer::ph_location_type("title")
    ) %>%
    # Section header: Environmental Surveillance ----
    officer::add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    officer::ph_with(value = "Environmental Surveillance", location = officer::ph_location_type("ctrTitle")) %>%
    # ES sites and detection grid ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "ES Sites and Detection 1",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(rvg::dml(ggobj = es.site.det), location = officer::ph_location_type("body")) %>%
    # ph_with(rvg::dml(ggobj = es.site.det.facet), location =officer::ph_location_type("body")) %>%
    # ES sites and detection map ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "ES Site Performance",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(rvg::dml(ggobj = es.det.map), location = officer::ph_location_type("body")) %>%
    # ES timeliness sample collection to lab ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Timeliness of ES sample transport",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(rvg::dml(ggobj = es.timely), location = officer::ph_location_type("body")) %>%
    # ES site details (table)----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "ES Site Details",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(es.table, location = officer::ph_location_type("body")) %>%
    # ES summary ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Key Points on ES Surveillance",
      location = officer::ph_location_type("title")
    ) %>%
    # Section header: Overall observations ----
    officer::add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    officer::ph_with(value = "Overall Observations", location = officer::ph_location_type("ctrTitle")) %>%
    # Summary ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Summary",
      location = officer::ph_location_type("title")
    ) %>%
    # Summary by Province ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Summary by Province",
      location = officer::ph_location_type("title")
    )

  gc()

  # Print output  ----
  print(draft_output, file.path(
    ppt_output_path,
    paste0("Deskreview_", country, "_", format(Sys.Date(), "%d%m%Y"), ".pptx")
  ))
}

#' Generate the desk review slide deck from the figures folder
#'
#' Generating the PowerPoint from the figures folder is generally faster and allows
#' figures to remain consistent. Tables remain as PowerPoint tables.
#'
#' @param ctry.data `list` Country polio data. Either the output of [extract_country_data()] or
#' [init_dr()].
#' @param start_date `str` Start date of desk review.
#' @param end_date `str` End date of desk review.
#' @param surv.ind.tab `flextable` Surveillance indicator table
#' @param inad.tab.flex `flextable` Inadequates table.
#' @param tab.60d `flextable` 60-day follow-up table.
#' @param pop.tab `flextable` Population table.
#' @param es.table `flextable` ES table.
#' @param ppt_template_path `str` Path to the PowerPoint template.
#' @param fig.path `str` File path to the figures folder.
#' @param country `str` Name of the country.
#' @param ppt_output_path `str` Path where the PowerPoint should be outputted.
#'
#' @returns None.
#' @examples
#' \dontrun{
#' # Assume all figures and tables are assigned to the appropriate variable.
#' ppt_template <- "C:/Users/ABC1/Desktop/deskreview_template.pptx"
#' generate_dr_ppt2(ctry.data, start_date, end_date,
#'   surv.ind.tab, inad.tab.flex, tab.60d, es.table,
#'   ppt_template_path = ppt_template
#' )
#' }
#'
#' @export
generate_dr_ppt2 <- function(ctry.data,
                             start_date, end_date,
                             surv.ind.tab,
                             inad.tab.flex,
                             tab.60d,
                             pop.tab,
                             es.table,
                             ppt_template_path = NULL,
                             fig.path = Sys.getenv("DR_FIGURE_PATH"),
                             country = Sys.getenv("DR_COUNTRY"),
                             ppt_output_path = Sys.getenv("DR_POWERPOINT_PATH")) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop(
      'Package "officer" must be installed to use this function.',
      call. = FALSE
    )
  }

  ppt_template_path <- get_ppt_template(ppt_template_path)
  assump <- generate_pptx_assumptions(start_date, end_date)
  incomplete.adm.dist <- spatial_validation(ctry.data$dist.pop, "dist")

  if (!dir.exists(ppt_output_path)) {
    stop("Output path does not exist. Please try again.")
  }

  if (!file.exists(ppt_template_path)) {
    stop("Ppt template path does not exist. Please try again.")
  }

  tempi <- officer::read_pptx(ppt_template_path)

  officer::layout_summary(tempi)
  officer::layout_properties(x = tempi, layout = "Two Content", master = "1_Office Theme")
  officer::layout_properties(x = tempi, layout = "Title and Content", master = "1_Office Theme")
  officer::layout_properties(x = tempi, layout = "4_tile", master = "1_Office Theme")

  draft_output <- tempi %>%
    # Title slide ----
    officer::add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    officer::ph_with(
      value = paste(ctry.data$name, "DESK REVIEW"),
      location = officer::ph_location_type("ctrTitle")
    ) %>%
    # Table of Contents  ----
    officer::add_slide(layout = "Two Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Table of Contents",
      location = officer::ph_location_type("title")
    ) %>%
    # Analysis notes ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Analysis Notes",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = assump,
      location = officer::ph_location_type("body")
    ) %>%
    # Reference map of country  ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = paste("Reference Maps", start_date, "-", end_date),
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(officer::external_img(file.path(fig.path, "pop.map.png")), location = officer::ph_location(top = 2, width = 4, height = 4)) %>%
    officer::ph_with(officer::external_img(file.path(fig.path, "pop.map.prov.png")), location = officer::ph_location(top = 2, left = 7, width = 4, height = 4)) %>%
    # Paralytic polio and compatible cases map ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = paste("Paralytic Polio and Compatible cases", start_date, "-", lubridate::today()),
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(officer::external_img(file.path(fig.path, "afp.case.map.png")), location = officer::ph_location(top = 2, left = 5, width = 5.5, height = 5.5)) %>%
    # Phylogenetic tree ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Phylogenetic Tree",
      location = officer::ph_location_type("title")
    ) %>%
    # Overview of Sample transport and processing ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Overview of Sample Transport & Processing",
      location = officer::ph_location_type("title")
    ) %>%
    # Section header: AFP surveillance ----
    officer::add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    officer::ph_with(value = "AFP Surveillance", location = officer::ph_location_type("ctrTitle")) %>%
    # Epi-curve ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = paste("Epicurve", start_date, "-", lubridate::today()),
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(officer::external_img(file.path(fig.path, "afp.epi.curve.png")), location = officer::ph_location(top = 2, left = 1, width = 14, height = 5)) %>%
    # POLIS screen shot ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Key message: AFP Surveillance Tracking",
      location = officer::ph_location_type("title")
    ) %>%
    # National surveillance indicators table (MISSING 100K NEEDS CODE REWORK) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "National Surveillance Indicators",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(surv.ind.tab, location = officer::ph_location_type("body")) %>%
    # AFP detections by province ----
    # Tile Chart and Table
    officer::add_slide(layout = "Two Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "AFP Detections by Province",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(officer::external_img(file.path(fig.path, "afp.dets.prov.year.png")),
      location = officer::ph_location(top = 2, left = 1, height = 5, width = 14)
    ) %>%
    officer::add_slide(layout = "Two Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "AFP Detections by Province and Year",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(pop.tab, location = officer::ph_location_label("Content Placeholder 3")) %>%
    # NPAFP rate/AFP detection by province by year (maps) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "NPAFP Rate by Province",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(officer::external_img(file.path(fig.path, "npafp.map.png")), location = officer::ph_location(top = 2, left = 1, height = 8, width = 14)) %>%
    # NPAFP rate/AFP detection by district by year (maps) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "NPAFP Rate by District",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(officer::external_img(file.path(fig.path, "npafp.map.dist.png")), location = officer::ph_location(top = 2, left = 1, height = 8, width = 14)) %>%
    # Stool adequacy by province by year (maps) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Stool Adequacy by Province",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(officer::external_img(file.path(fig.path, "stool.ad.maps.png")), location = officer::ph_location(top = 2, left = 1, height = 8, width = 14)) %>%
    # Stool adequacy by dist by year (maps) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Stool Adequacy by District",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(officer::external_img(file.path(fig.path, "stool.ad.maps.dist.png")), location = officer::ph_location(top = 2, left = 1, height = 8, width = 14)) %>%
    # Main issues with stool adequacy table (NOT DONE) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Main issues with stool adequacy",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(inad.tab.flex, location = officer::ph_location_type("body")) %>%
    # Virus isolation in/around SIA rounds (table) (NOT DONE) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Virus isolation in/around SIA rounds",
      location = officer::ph_location_type("title")
    ) %>%
    # 60-day follow-up table by year (table) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "60-day follow-up",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(tab.60d, location = officer::ph_location_type("body")) %>%
    officer::ph_with(
      value = paste0(
        "* Defined as inadequate cases with: ",
        "1) FU finding (Residual weakness, Lost to FU, Died) or no FU visit; ",
        "2) Discarded or Pending Lab / Pending Classification; ",
        '3) Filtered for "OPV/IPV status: <3 doses, or Unknown"'
      ),
      location = officer::ph_location_type(type = "ftr")
    ) %>%
    # Possible clusters of potential compatibles (table) (NOT DONE) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Possible clusters of potential compatibles",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = paste0(
        "* Defined as inadequate cases with: ",
        "1) FU finding (Residual weakness, Lost to FU, Died) or no FU visit; ",
        "2) Discarded or Pending Lab / Pending Classification; ",
        '3) Filtered for "OPV/IPV status: <3 doses, or Unknown"'
      ),
      location = officer::ph_location_type(type = "ftr")
    ) %>%
    # Contact sampling (table) (NOT DONE) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Contact sampling",
      location = officer::ph_location_type("title")
    ) %>%
    # Possible clusters of under-immunized NPAFP cases (table) (NOT DONE) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Immunization Status Among NPAFP cases",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(officer::external_img(file.path(fig.path, "case.num.dose.g.png")), location = officer::ph_location(top = 2, left = 1, height = 8, width = 9)) %>%
    # Key Points on AFP surveillance ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Key Points on AFP Surveillance",
      location = officer::ph_location_type("title")
    ) %>%
    # Section header: Active surveillance ----
    officer::add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    officer::ph_with(value = "Active Surveillance", location = officer::ph_location_type("ctrTitle")) %>%
    # Annual active surveillance visits by priority level (bar graph) (NOT DONE)----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Annual active surveillance visits by priority level",
      location = officer::ph_location_type("title")
    ) %>%
    # Active surveillance visits (table) (NOT DONE)----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Active surveillance visits",
      location = officer::ph_location_type("title")
    ) %>%
    # Key points on active surveillance ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Key Points on active Surveillance",
      location = officer::ph_location_type("title")
    ) %>%
    # Section header: AFP Surveillance Timeliness ----
    officer::add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    officer::ph_with(value = "AFP Surveillance Timeliness", location = officer::ph_location_type("ctrTitle")) %>%
    # National trend in timeliness of AFP case detection (bar graph) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "National timeliness",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(officer::external_img(file.path(fig.path, "timely_nation.png")), location = officer::ph_location(top = 2, left = 1, height = 4, width = 14)) %>%
    # Timeliness at provincial level (bar graph) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Timeliness by province and year",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(officer::external_img(file.path(fig.path, "timely_prov.png")), location = officer::ph_location(top = 2, left = 1, height = 10, width = 14)) %>%
    # Timeliness across provinces (multi map) ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Timeliness by province and year",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(officer::external_img(file.path(fig.path, "mapt_all.png")), location = officer::ph_location_type(top = 2, left = 1, height = 8, width = 14)) %>%
    # Key points on AFP Surveillance Timeliness ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Key Points on AFP Surveillance Timeliness",
      location = officer::ph_location_type("title")
    ) %>%
    # Section header: Environmental Surveillance ----
    officer::add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    officer::ph_with(value = "Environmental Surveillance", location = officer::ph_location_type("ctrTitle")) %>%
    # ES sites and detection grid ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "ES Sites and Detection 1",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(officer::external_img(file.path(fig.path, "es.site.det.png")), location = officer::ph_location(top = 2, left = 1, height = 8, width = 14)) %>%
    # ES sites and detection map ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "ES Sites and Detection 2",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(officer::external_img(file.path(fig.path, "es.det.map.png")), location = officer::ph_location(top = 2, left = 1, height = 8, width = 9)) %>%
    officer::ph_with(
      value = "Sites with missing coordinates are randomly assigned points in their home district.",
      location = officer::ph_location_type("ftr")
    ) %>%
    # ES timeliness sample collection to lab ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Timeliness of ES sample transport",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(officer::external_img(file.path(fig.path, "es.timely.png")), location = officer::ph_location(top = 2, left = 1, height = 8, width = 14)) %>%
    # ES site details (table)----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "ES Site Details",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(es.table, location = officer::ph_location_type("body")) %>%
    # ES summary ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Key Points on ES Surveillance",
      location = officer::ph_location_type("title")
    ) %>%
    # Section header: Overall observations ----
    officer::add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    officer::ph_with(value = "Overall Observations", location = officer::ph_location_type("ctrTitle")) %>%
    # Summary ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Summary",
      location = officer::ph_location_type("title")
    ) %>%
    # Summary by Province ----
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Summary by Province",
      location = officer::ph_location_type("title")
    ) %>%
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Desk Review Assumptions and Calculations",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = officer::block_list(
        officer::fpar(officer::ftext(paste0("All data presented is from POLIS as of ", ctry.data$metadata$download_time), officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Data undergoes cleaning and standardization by CDC", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Substantive differences include:", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Classification of cases", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Classification is derived from laboratory classification first and then epi classification (see CDC data processing - Case classification for more details)", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Stool adequacy and timeliness", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext('Primary POLIS variable(s) are "Stool adequacy" and "Stool adequacy with condition"', officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext('Discrepancies observed between collection intervals and condition and what is reported with in "Stool adequacy with condition"', officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Recalculated using stool 1 condition, stool 2 condition, and calculated timeliness intervals (see slide CDC data processing - Timeliness intervals)", officer::fp_text(font.size = 17)))
      ),
      location = officer::ph_location_type("body"),
      level_list = c(1L, 1L, 1L, 2L, 3L, 2L, 3L, 3L, 3L)
    ) %>%
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "CDC Data Processing - Timeliness Intervals",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = officer::block_list(
        officer::fpar(officer::ftext("Calculated as difference in reported dates (epi data only) ", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Onset to notification - <= 7 days", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Notification to investigation - <= 2 days", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Investigation to stool 1 collection - <= 3 days*", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Stool 1 to stool 2 collection - Goal: >= 1 day and <= 11 days", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Stool collection to received in lab - <= 3 days (note: this is the indicator for collection to lab, NOT onset to lab) ", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Note: if there is no stool 2 collected, this is calculated from stool 1 collection", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Samples with conflicting/data errors are considered inadequate", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Date before onset", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Missing date", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Data entry error (>365 days between stool 1 and stool 2 collection) ", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Caveats", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("The displayed timeliness intervals are not adjusted for those countries who need to ship to an international lab for testing", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Timeliness intervals are not represented visually as listed in the GPSAP - i.e. 14 days from onset to received in lab - stacked bar shows the subcategories according to GPSAP intervals", officer::fp_text(font.size = 17)))
      ),
      line_spacing = 0,
      padding = 0,
      location = officer::ph_location_type("body"),
      level_list = c(1L, 2L, 2L, 2L, 2L, 2L, 3L, 1L, 2L, 2L, 2L, 1L, 2L, 2L)
    ) %>%
    officer::ph_with(value = "* This indicator is not explicitly defined in the GPSAP", location = officer::ph_location_type("ftr")) |>
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "CDC Data Processing - Case Classification",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = officer::block_list(
        officer::fpar(officer::ftext("CDC case classification uses lab classification first and then epi data - if epi and lab disagree, lab is considered correct", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Categories includes not AFP, NPAFP, compatible, pending, lab pending, and all virus types (eg VDPV, Wild 1) ", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Pending is when there is no virus type and classification is pending ", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext('Lab pending is no virus type, classification is pending, and final culture result is "not received in lab"', officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Laboratory classification is determined from Virus Type field", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Note that this does not take into account the vdpv1, vdpv2, vdpv3, and wild1 computed variables in POLIS ", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Extract virus from virus type and then use classification vdpv to determine ambiguous, immune deficient, or circulating for VDPVs", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Important caveats", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Case classification and lab results are updated in POLIS as lab results come in - this can and does result in varying time delays based on shipping and lab testing times", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Vdpv1, 2, and 3 variables in POLIS are also extracted from virus type ", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("There can be discrepancies between the two", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Can be difficult to identify dual infections", officer::fp_text(font.size = 17)))
      ),
      location = officer::ph_location_type("body"),
      level_list = c(1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L)
    ) %>%
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Epi Data Cleaning",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = officer::block_list(
        officer::fpar(officer::ftext("Filling missing geographic data", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Compare epid with previously reported case epids ", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("If EPID prov and dist code match with previously reported case in the same onset year, then backfill prov and dist", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("If no match, expand search to any year, then backfill prov and dist", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("If there are multiple guids identified with the same prov and dist for the match, then do not match and leave blank (cannot tell which GUID to use)", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext(paste0("There are ", (nrow(incomplete.adm.dist)), " missing district from raw data"), officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Able to extract district from EPID for (manually fill here) cases.", officer::fp_text(font.size = 17)))
      ),
      location = officer::ph_location_type("body"),
      level_list = c(1L, 2L, 2L, 3L, 3L, 1L, 2L)
    ) %>%
    officer::add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    officer::ph_with(
      value = "Lab Data Cleaning",
      location = officer::ph_location_type("title")
    ) %>%
    officer::ph_with(
      value = officer::block_list(
        officer::fpar(officer::ftext("Steps for lab cleaning", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Extract year from onset date if available", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("For non-cases/no onset date - extracted from EPID", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Deduplicated using lab master key (MasterKey)", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Data current up to (manually edit here)", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("If a master key was duplicated, used the more recent appearance (the later database)", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Matching province", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Match epids between lab and epi data - use province from epi data", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext("Note: a number of mismatches observed (hard to quantify since a subset of these are spelling differences)", officer::fp_text(font.size = 17, color = "red"))),
        officer::fpar(officer::ftext("Cross check epids with previous epids to identify same province code", officer::fp_text(font.size = 17))),
        officer::fpar(officer::ftext(paste0(
          "Created look up table for provinces and districts from previous epids",
          " matching geographic columns from other complete entries."
        ), officer::fp_text(font.size = 17)))
      ),
      location = officer::ph_location_type("body"),
      level_list = c(1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 3L, 1L, 1L)
    )



  gc()

  # Print output  ----
  print(draft_output, file.path(
    ppt_output_path,
    paste0("Deskreview_", country, "_", format(Sys.Date(), "%d%m%Y"), ".pptx")
  ))
}
