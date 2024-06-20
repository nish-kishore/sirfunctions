#' Generate PowerPoint assumptions
#'
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#'
#' @return list of strings
generate_pptx_assumptions <- function(start_date, end_date) {
  pptx.assumptions <- c('Data sources:',
                        paste0('POLIS (Data as of ', format(start_date, '%d-%b-%Y'),
                               ' to ',format(end_date, '%d-%b-%Y') ,')'),
                        'Missing population estimated from the UNDP growth factor from previous year’s population.',
                        paste0('Timeframe for analysis: ', format(start_date, '%d-%b-%Y'),
                               ' to ',format(end_date, '%d-%b-%Y')),
                        paste0('Some selected figures include additional data (',
                               format(start_date, "%b %Y"),
                               "-",format(Sys.Date(), "%b %Y"), ")"), # UPDATE AS NEEDED
                        'NPAFP Assumptions',
                        'NPAFP cases with all pending cases included (Pending Lab and Pending Classification)',
                        'Stool Adequacy Assumptions',
                        'All AFP cases',
                        'Samples with missing stool condition were considered good quality',
                        'Samples with bad date data (e.g. collection before onset) were considered inadequate')

  assump = officer::unordered_list(
    level_list = c(1, 2, 2, 1,2,1,2,1,2,2,2), #Indentation level for each bullet
    str_list = pptx.assumptions,
    style = fp_text(color = "black", font.size = 17))

  return(assump)

}

#' Get path of the PowerPoint template
#'
#' @param path path to the PowerPoint template. If NULL, will prompt user to download from the sg-desk-review GitHub repository
#' @param filename name of the PowerPoint template
#'
#' @return a string containing the path of the PowerPoint template
#' @export
get_ppt_template <- function(path=NULL, filename="desk_review_template.pptx") {

  if (is.null(path)) {
    url = "https://github.com/nish-kishore/sg-desk-reviews/tree/main/resources"

    stop(paste0("\nPlease download the PPT template file here:\n"), url)
  } else {
    return(file.path(path))
  }
}


#' Generate the desk review slide deck
#'
#' @param ppt_template_path path to the PowerPoint template
#' @param ctry.data RDS file containing polio data for a country
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#' @param pop.map country pop map
#' @param pop.map.prov prov pop map
#' @param afp.case.map map of afp cases
#' @param afp.epi.curve afp epicurv
#' @param surv.ind.tab surveillance indicator table
#' @param afp.dets.prov.year afp dets for province
#' @param npafp.map npafp map for country level
#' @param npafp.map.dist npafp map for district level
#' @param stool.ad.maps stool adequacy maps at province
#' @param stool.ad.maps.dist stool adequact maps at district
#' @param inad.tab.flex inadequacy table
#' @param tab.60d 60 day follow up table
#' @param timely_nation timeliness at country level
#' @param timely_prov timeliness at province level
#' @param mapt_all map with all indicators
#' @param es.site.det ES site viral detection
#' @param es.det.map ES site detection maps
#' @param es.timely ES timeliness
#' @param es.table ES table
#' @param country name of the country
#' @param ppt_output_path path where the powerpoint should be outputted
#'
#' @return does not return anything
#' @export
generate_dr_ppt <- function(ppt_template_path, ctry.data, start_date, end_date,
                            pop.map, pop.map.prov, afp.case.map, afp.epi.curve,
                            surv.ind.tab, afp.dets.prov.year, npafp.map,
                            npafp.map.dist, stool.ad.maps, stool.ad.maps.dist,
                            inad.tab.flex, tab.60d,timely_nation, timely_prov,
                            mapt_all, es.site.det, es.det.map, es.timely,
                            es.table, country, ppt_output_path
) {

  assump <- generate_pptx_assumptions(start_date, end_date)

  if (!dir.exists(ppt_output_path)) {
    stop("Output path does not exist. Please try again.")
  }

  if (!file.exists(ppt_template_path)) {
    stop("Ppt template path does not exist. Please try again.")
  }

  tempi <- read_pptx(ppt_template_path)

  layout_summary(tempi)
  layout_properties(x = tempi, layout = "Two Content", master = "1_Office Theme")
  layout_properties(x = tempi, layout = "Title and Content", master = "1_Office Theme")
  layout_properties(x = tempi, layout = "4_tile", master = "1_Office Theme")

  draft_output <- tempi %>%
    # Title slide ----
  add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    ph_with(value = paste(ctry.data$name, "DESK REVIEW"),
            location = ph_location_type("ctrTitle")) %>%
    # Table of Contents  ----
  add_slide(layout = "Two Content", master = "1_Office Theme") %>%
    ph_with(value = "Table of Contents",
            location = ph_location_type("title")) %>%
    # Analysis notes ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Analysis Notes",
            location = ph_location_type("title")) %>%
    ph_with(value = assump,
            location = ph_location_type("body")) %>%
    # Reference map of country  ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(
      value = paste("Reference Map 1", start_date, "-", end_date),
      location = ph_location_type("title")
    ) %>%
    ph_with(dml(ggobj = pop.map), location = ph_location_type("body")) %>%
    add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(
      value = paste("Reference Map 2", start_date, "-", end_date),
      location = ph_location_type("title")
    ) %>%
    ph_with(dml(ggobj = pop.map.prov), location = ph_location_type("body")) %>%
    # Paralytic polio and compatible cases map ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(
      value = paste("Paralytic Polio and Compatible cases", start_date, "-", end_date),
      location = ph_location_type("title")
    ) %>%
    ph_with(dml(ggobj = afp.case.map), location = ph_location_type("body")) %>%
    # Phylogenetic tree ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Phylogenetic Tree",
            location = ph_location_type("title")) %>%
    # Overview of Sample transport and processing ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Overview of Sample Transport & Processing",
            location = ph_location_type("title")) %>%
    # Section header: AFP surveillance ----
  add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    ph_with(value = "AFP Surveillance", location = ph_location_type("ctrTitle")) %>%
    # Epi-curve ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(
      value = paste("Epicurve", start_date, "-", end_date),
      location = ph_location_type("title")
    ) %>%
    ph_with(dml(ggobj = afp.epi.curve), location = ph_location_type("body")) %>%
    # POLIS screen shot ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Key message: AFP Surveillance Tracking",
            location = ph_location_type("title")) %>%
    # National surveillance indicators table (MISSING 100K NEEDS CODE REWORK) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "National Surveillance Indicators",
            location = ph_location_type("title")) %>%
    ph_with(surv.ind.tab, location = ph_location_type("body")) %>%
    # AFP detections by province ----
  # Tile Chart and Table
  add_slide(layout = "Two Content", master = "1_Office Theme") %>%
    ph_with(value = "AFP Detections by Province",
            location = ph_location_type("title")) %>%
    ph_with(dml(ggobj = afp.dets.prov.year),
            location = ph_location_label("Content Placeholder 2")) %>%
    add_slide(layout = "Two Content", master = "1_Office Theme") %>%
    ph_with(value = "AFP Detections by Province and Year",
            location = ph_location_type("title")) %>%
    ph_with(pop.tab, location = ph_location_label("Content Placeholder 3")) %>%
    # NPAFP rate/AFP detection by province by year (maps) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "NPAFP Rate by Province",
            location = ph_location_type("title")) %>%
    ph_with(dml(ggobj = npafp.map), location = ph_location_type("body")) %>%
    # NPAFP rate/AFP detection by district by year (maps) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "NPAFP Rate by District",
            location = ph_location_type("title")) %>%
    ph_with(dml(ggobj = npafp.map.dist), location = ph_location_type("body")) %>%
    # Stool adequacy by province by year (maps) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Stool Adequacy by Province",
            location = ph_location_type("title")) %>%
    ph_with(dml(ggobj = stool.ad.maps), location = ph_location_type("body")) %>%
    # Stool adequacy by dist by year (maps) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Stool Adequacy by District",
            location = ph_location_type("title")) %>%
    ph_with(dml(ggobj = stool.ad.maps.dist), location = ph_location_type("body")) %>%
    # Main issues with stool adequacy table (NOT DONE) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Main issues with stool adequacy",
            location = ph_location_type("title")) %>%
    ph_with(inad.tab.flex, location = ph_location_type("body")) %>%
    # Virus isolation in/around SIA rounds (table) (NOT DONE) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Virus isolation in/around SIA rounds",
            location = ph_location_type("title")) %>%
    # 60 day follow up table by year (table) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "60 day follow up",
            location = ph_location_type("title")) %>%
    ph_with(tab.60d, location = ph_location_type("body")) %>%
    # Possible clusters of potential compatibles (table) (NOT DONE) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Possible clusters of potential compatibles",
            location = ph_location_type("title")) %>%
    # Contact sampling (table) (NOT DONE) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Contact sampling",
            location = ph_location_type("title")) %>%
    # Possible clusters of under-immunized NPAFP cases (table) (NOT DONE) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Immunization Status Among NPAFP cases",
            location = ph_location_type("title")) %>%
    ph_with(dml(ggobj = case.num.dose.g), location = ph_location_type("body")) |>
    # Key Points on AFP surveillance ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Key Points on AFP Surveillance",
            location = ph_location_type("title")) %>%
    # Section header: Active surveillance ----
  add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    ph_with(value = "Active Surveillance", location = ph_location_type("ctrTitle")) %>%
    # Annual active surveillance visits by priority level (bar graph) (NOT DONE)----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Annual active surveillance visits by priority level",
            location = ph_location_type("title")) %>%
    # Active surveillance visits (table) (NOT DONE)----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Active surveillance visits",
            location = ph_location_type("title")) %>%
    # Key points on active surveillance ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Key Points on active Surveillance",
            location = ph_location_type("title")) %>%
    # Section header: AFP Surveillance Timeliness ----
  add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    ph_with(value = "AFP Surveillance Timeliness", location = ph_location_type("ctrTitle")) %>%
    # National trend in timeliness of AFP case detection (bar graph) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "National timeliness",
            location = ph_location_type("title")) %>%
    ph_with(dml(ggobj = timely_nation), location = ph_location_type("body")) %>%
    # Timeliness at provincial level (bar graph) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Timeliness by province and year",
            location = ph_location_type("title")) %>%
    ph_with(dml(ggobj = timely_prov), location = ph_location_type("body")) %>%
    # Timeliness across provinces (multi map) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Timeliness by province and year",
            location = ph_location_type("title")) %>%
    ph_with(dml(ggobj = mapt_all), location = ph_location_type("body")) %>%
    # Key points on AFP Surveillance Timeliness ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Key Points on AFP Surveillance Timeliness",
            location = ph_location_type("title")) %>%
    # Section header: Environmental Surveillance ----
  add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    ph_with(value = "Environmental Surveillance", location = ph_location_type("ctrTitle")) %>%
    # ES sites and detection grid ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "ES Sites and Detection 1",
            location = ph_location_type("title")) %>%
    ph_with(dml(ggobj = es.site.det), location = ph_location_type("body")) %>%
    #ph_with(dml(ggobj = es.site.det.facet), location = ph_location_type("body")) %>%
    # ES sites and detection map ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "ES Sites and Detection 2",
            location = ph_location_type("title")) %>%
    ph_with(dml(ggobj = es.det.map), location = ph_location_type("body")) %>%
    # ES timeliness sample collection to lab ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Timeliness of ES sample transport",
            location = ph_location_type("title")) %>%
    ph_with(dml(ggobj = es.timely), location = ph_location_type("body")) %>%
    # ES site details (table)----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "ES Site Details",
            location = ph_location_type("title")) %>%
    ph_with(es.table, location = ph_location_type("body")) %>%
    # ES summary ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Key Points on ES Surveillance",
            location = ph_location_type("title")) %>%
    # Section header: Overall observations ----
  add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    ph_with(value = "Overall Observations", location = ph_location_type("ctrTitle")) %>%
    # Summary ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Summary",
            location = ph_location_type("title")) %>%
    # Summary by Province ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Summary by Province",
            location = ph_location_type("title"))

  gc()

  # Print output  ----
  print(draft_output, file.path(
    ppt_output_path,
    paste0("draft_output_", Sys.Date(), "_", country, ".pptx")
  ))

}

#' Generate the desk review slide deck from the figures folder
#' Generating the PowerPoint from the figures folder is generally faster and allows
#' figures to remain consistent. Tables remain as PowerPoint tables.
#'
#' @param ctry.data RDS file containing polio data for a country
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#' @param surv.ind.tab surveillance indicator table
#' @param inad.tab.flex inadequacy table
#' @param tab.60d 60 day follow up table
#' @param es.table ES table
#' @param ppt_template_path path to the PowerPoint template
#' @param fig.path file path to the figures folder
#' @param country name of the country
#' @param ppt_output_path path where the powerpoint should be outputted
#'
#' @return does not return anything
#' @export
generate_dr_ppt2 <- function(ctry.data,
                             start_date, end_date,
                             surv.ind.tab,
                             inad.tab.flex,
                             tab.60d,
                             es.table,
                             ppt_template_path=NULL,
                             fig.path=Sys.getenv("DR_FIGURE_PATH"),
                             country=Sys.getenv("DR_COUNTRY"),
                             ppt_output_path=Sys.getenv("DR_POWERPOINT_PATH")) {

  library(officer)
  ppt_template_path <- get_ppt_template(ppt_template_path)
  assump <- generate_pptx_assumptions(start_date, end_date)
  incomplete.adm.dist <- spatial_validation(ctry.data$dist.pop, "dist")

  if (!dir.exists(ppt_output_path)) {
    stop("Output path does not exist. Please try again.")
  }

  if (!file.exists(ppt_template_path)) {
    stop("Ppt template path does not exist. Please try again.")
  }

  tempi <- read_pptx(ppt_template_path)

  layout_summary(tempi)
  layout_properties(x = tempi, layout = "Two Content", master = "1_Office Theme")
  layout_properties(x = tempi, layout = "Title and Content", master = "1_Office Theme")
  layout_properties(x = tempi, layout = "4_tile", master = "1_Office Theme")

  draft_output <- tempi %>%
    # Title slide ----
  add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    ph_with(value = paste(ctry.data$name, "DESK REVIEW"),
            location = ph_location_type("ctrTitle")) %>%
    # Table of Contents  ----
  add_slide(layout = "Two Content", master = "1_Office Theme") %>%
    ph_with(value = "Table of Contents",
            location = ph_location_type("title")) %>%
    # Analysis notes ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Analysis Notes",
            location = ph_location_type("title")) %>%
    ph_with(value = assump,
            location = ph_location_type("body")) %>%
    # Reference map of country  ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(
      value = paste("Reference Maps", start_date, "-", end_date),
      location = ph_location_type("title")
    ) %>%
    ph_with(external_img(file.path(fig.path, "pop.map.png")), location = ph_location(top = 2, width = 4, height = 4)) %>%
    ph_with(external_img(file.path(fig.path, "pop.map.prov.png")), location = ph_location(top = 2, left = 7, width = 4, height = 4)) %>%
    # Paralytic polio and compatible cases map ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(
      value = paste("Paralytic Polio and Compatible cases", start_date, "-", end_date),
      location = ph_location_type("title")
    ) %>%
    ph_with(external_img(file.path(fig.path, "afp.case.map.png")), location = ph_location(top = 2, left = 5, width = 5.5, height = 5.5)) %>%
    # Phylogenetic tree ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Phylogenetic Tree",
            location = ph_location_type("title")) %>%
    # Overview of Sample transport and processing ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Overview of Sample Transport & Processing",
            location = ph_location_type("title")) %>%
    # Section header: AFP surveillance ----
  add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    ph_with(value = "AFP Surveillance", location = ph_location_type("ctrTitle")) %>%
    # Epi-curve ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(
      value = paste("Epicurve", start_date, "-", end_date),
      location = ph_location_type("title")
    ) %>%
    ph_with(external_img(file.path(fig.path, "afp.epi.curve.png")), location = ph_location(top = 2, left = 1, width = 14, height = 5)) %>%
    # POLIS screen shot ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Key message: AFP Surveillance Tracking",
            location = ph_location_type("title")) %>%
    # National surveillance indicators table (MISSING 100K NEEDS CODE REWORK) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "National Surveillance Indicators",
            location = ph_location_type("title")) %>%
    ph_with(surv.ind.tab, location = ph_location_type("body")) %>%
    # AFP detections by province ----
  # Tile Chart and Table
  add_slide(layout = "Two Content", master = "1_Office Theme") %>%
    ph_with(value = "AFP Detections by Province",
            location = ph_location_type("title")) %>%
    ph_with(external_img(file.path(fig.path, "afp.dets.prov.year.png")),
            location = ph_location(top = 2, left = 1, height = 5, width = 14)) %>%
    add_slide(layout = "Two Content", master = "1_Office Theme") %>%
    ph_with(value = "AFP Detections by Province and Year",
            location = ph_location_type("title")) %>%
    ph_with(pop.tab, location = ph_location_label("Content Placeholder 3")) %>%
    # NPAFP rate/AFP detection by province by year (maps) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "NPAFP Rate by Province",
            location = ph_location_type("title")) %>%
    ph_with(external_img(file.path(fig.path, "npafp.map.png")), location = ph_location(top = 2, left = 1, height = 8, width = 14)) %>%
    # NPAFP rate/AFP detection by district by year (maps) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "NPAFP Rate by District",
            location = ph_location_type("title")) %>%
    ph_with(external_img(file.path(fig.path, "npafp.map.dist.png")), location = ph_location(top = 2, left = 1, height = 8, width = 14)) %>%
    # Stool adequacy by province by year (maps) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Stool Adequacy by Province",
            location = ph_location_type("title")) %>%
    ph_with(external_img(file.path(fig.path, "stool.ad.maps.png")), location = ph_location(top = 2, left = 1, height = 8, width = 14)) %>%
    # Stool adequacy by dist by year (maps) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Stool Adequacy by District",
            location = ph_location_type("title")) %>%
    ph_with(external_img(file.path(fig.path, "stool.ad.maps.dist.png")), location = ph_location(top = 2, left = 1, height = 8, width = 14)) %>%
    # Main issues with stool adequacy table (NOT DONE) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Main issues with stool adequacy",
            location = ph_location_type("title")) %>%
    ph_with(inad.tab.flex, location = ph_location_type("body")) %>%
    # Virus isolation in/around SIA rounds (table) (NOT DONE) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Virus isolation in/around SIA rounds",
            location = ph_location_type("title")) %>%
    # 60 day follow up table by year (table) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "60 day follow up",
            location = ph_location_type("title")) %>%
    ph_with(tab.60d, location = ph_location_type("body")) %>%
    ph_with(value = paste0("* Defined as inadequate cases with: ",
                           "1) FU finding (Residual weakness, Lost to FU, Died) or no FU visit; ",
                           "2) Discarded or Pending Lab / Pending Classification; ",
                           '3) Filtered for "OPV/IPV status: <3 doses, or Unknown"'),
            location = ph_location_type(type = "ftr")) %>%
    # Possible clusters of potential compatibles (table) (NOT DONE) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Possible clusters of potential compatibles",
            location = ph_location_type("title")) %>%
    ph_with(value = paste0("* Defined as inadequate cases with: ",
                           "1) FU finding (Residual weakness, Lost to FU, Died) or no FU visit; ",
                           "2) Discarded or Pending Lab / Pending Classification; ",
                           '3) Filtered for "OPV/IPV status: <3 doses, or Unknown"'),
            location = ph_location_type(type = "ftr")) %>%
    # Contact sampling (table) (NOT DONE) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Contact sampling",
            location = ph_location_type("title")) %>%
    # Possible clusters of under-immunized NPAFP cases (table) (NOT DONE) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Immunization Status Among NPAFP cases",
            location = ph_location_type("title")) %>%
    ph_with(external_img(file.path(fig.path, "case.num.dose.g.png")), location = ph_location(top = 2, left = 1, height = 8, width = 9)) %>%
    # Key Points on AFP surveillance ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Key Points on AFP Surveillance",
            location = ph_location_type("title")) %>%
    # Section header: Active surveillance ----
  add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    ph_with(value = "Active Surveillance", location = ph_location_type("ctrTitle")) %>%
    # Annual active surveillance visits by priority level (bar graph) (NOT DONE)----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Annual active surveillance visits by priority level",
            location = ph_location_type("title")) %>%
    # Active surveillance visits (table) (NOT DONE)----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Active surveillance visits",
            location = ph_location_type("title")) %>%
    # Key points on active surveillance ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Key Points on active Surveillance",
            location = ph_location_type("title")) %>%
    # Section header: AFP Surveillance Timeliness ----
  add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    ph_with(value = "AFP Surveillance Timeliness", location = ph_location_type("ctrTitle")) %>%
    # National trend in timeliness of AFP case detection (bar graph) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "National timeliness",
            location = ph_location_type("title")) %>%
    ph_with(external_img(file.path(fig.path, "timely_nation.png")), location = ph_location(top = 2, left = 1, height = 4, width = 14)) %>%
    # Timeliness at provincial level (bar graph) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Timeliness by province and year",
            location = ph_location_type("title")) %>%
    ph_with(external_img(file.path(fig.path, "timely_prov.png")), location = ph_location(top = 2, left = 1, height = 10, width = 14)) %>%
    # Timeliness across provinces (multi map) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Timeliness by province and year",
            location = ph_location_type("title")) %>%
    ph_with(external_img(file.path(fig.path, "mapt_all.png")), location = ph_location_type(top = 2, left = 1, height = 8, width = 14)) %>%
    # Key points on AFP Surveillance Timeliness ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Key Points on AFP Surveillance Timeliness",
            location = ph_location_type("title")) %>%
    # Section header: Environmental Surveillance ----
  add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    ph_with(value = "Environmental Surveillance", location = ph_location_type("ctrTitle")) %>%
    # ES sites and detection grid ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "ES Sites and Detection 1",
            location = ph_location_type("title")) %>%
    ph_with(external_img(file.path(fig.path, "es.site.det.png")), location = ph_location(top = 2, left = 1, height = 8, width = 14)) %>%
    #ph_with(dml(ggobj = es.site.det.facet), location = ph_location_type("body")) %>%
    # ES sites and detection map ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "ES Sites and Detection 2",
            location = ph_location_type("title")) %>%
    ph_with(external_img(file.path(fig.path, "es.det.map.png")), location = ph_location(top = 2, left = 1, height = 8, width = 9)) %>%
    ph_with(value = "Sites with missing coordinates are randomly assigned points in their home district.",
            location = ph_location_type("ftr")) %>%
    # ES timeliness sample collection to lab ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Timeliness of ES sample transport",
            location = ph_location_type("title")) %>%
    ph_with(external_img(file.path(fig.path, "es.timely.png")), location = ph_location(top = 2, left = 1, height = 8, width = 14)) %>%
    # ES site details (table)----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "ES Site Details",
            location = ph_location_type("title")) %>%
    ph_with(es.table, location = ph_location_type("body")) %>%
    # ES summary ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Key Points on ES Surveillance",
            location = ph_location_type("title")) %>%
    # Section header: Overall observations ----
  add_slide(layout = "Title Slide", master = "1_Office Theme") %>%
    ph_with(value = "Overall Observations", location = ph_location_type("ctrTitle")) %>%
    # Summary ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Summary",
            location = ph_location_type("title")) %>%
    # Summary by Province ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Summary by Province",
            location = ph_location_type("title"))  %>%
    add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Desk Review Assumptions and Calculations",
            location = ph_location_type("title")) %>%
    ph_with(value = block_list(
      fpar(ftext(paste0("All data presented is from POLIS as of ", ctry.data$metadata$download_time), fp_text(font.size = 17))),
      fpar(ftext("Data undergoes cleaning and standardization by CDC", fp_text(font.size = 17))),
      fpar(ftext("Substantive differences include:", fp_text(font.size = 17))),
      fpar(ftext("Classification of cases", fp_text(font.size = 17))),
      fpar(ftext("Classification is derived from laboratory classification first and then epi classification (see CDC data processing – Case classification for more details)", fp_text(font.size = 17))),
      fpar(ftext("Stool adequacy and timeliness", fp_text(font.size = 17))),
      fpar(ftext('Primary POLIS variable(s) are "Stool adequacy" and  "Stool adequacy with condition"', fp_text(font.size = 17))),
      fpar(ftext('Discrepancies observed between collection intervals and condition and what is reported with in "Stool adequacy with condition"', fp_text(font.size = 17))),
      fpar(ftext("Recalculated using stool 1 condition, stool 2 condition, and calculated timeliness intervals (see slide CDC data processing – Timeliness intervals)", fp_text(font.size = 17)))
    ),
    location = ph_location_type("body"),
    level_list = c(1L, 1L, 1L, 2L, 3L, 2L, 3L, 3L, 3L)) %>%
    add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "CDC Data Processing – Timeliness Intervals",
            location = ph_location_type("title")) %>%
    ph_with(value = block_list(
      fpar(ftext("Calculated as difference in reported dates (epi data only)​", fp_text(font.size = 17))),
      fpar(ftext("Onset to notification – ≤ 7 days", fp_text(font.size = 17))),
      fpar(ftext("Notification to investigation – ≤ 2 days", fp_text(font.size = 17))),
      fpar(ftext("Investigation to stool 1 collection – ≤ 3 days*", fp_text(font.size = 17))),
      fpar(ftext("Stool 1 to stool 2 collection – Goal: ≥1 day and ≤ 11 days", fp_text(font.size = 17))),
      fpar(ftext("Stool collection to received in lab – ≤ 3 days (note: this is the indicator for collection to lab, NOT onset to lab)​", fp_text(font.size = 17))),
      fpar(ftext("Note: if there is no stool 2 collected, this is calculated from stool 1 collection", fp_text(font.size = 17))),
      fpar(ftext("Samples with conflicting/data errors are considered inadequate", fp_text(font.size = 17))),
      fpar(ftext("Date before onset", fp_text(font.size = 17))),
      fpar(ftext("Missing date", fp_text(font.size = 17))),
      fpar(ftext("Data entry error (>365 days between stool 1 and stool 2 collection)​", fp_text(font.size = 17))),
      fpar(ftext("Caveats", fp_text(font.size = 17))),
      fpar(ftext("The displayed timeliness intervals are not adjusted for those countries who need to ship to an international lab for testing", fp_text(font.size = 17))),
      fpar(ftext("Timeliness intervals are not represented visually as listed in the GPSAP – i.e. 14 days from onset to received in lab – stacked bar shows the subcategories according to GPSAP intervals", fp_text(font.size = 17)))
    ),
    line_spacing = 0,
    padding = 0,
    location = ph_location_type("body"),
    level_list =c(1L, 2L, 2L, 2L, 2L, 2L, 3L, 1L, 2L, 2L, 2L, 1L, 2L, 2L)
    ) %>%
    ph_with(value = "* This indicator is not explicitly defined in the GPSAP", location = ph_location_type("ftr")) |>
    add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "CDC Data Processing – Case Classification",
            location = ph_location_type("title")) %>%
    ph_with(value = block_list(
      fpar(ftext("CDC case classification uses lab classification first and then epi data – if epi and lab disagree, lab is considered correct", fp_text(font.size = 17))),
      fpar(ftext("Categories includes not AFP, NPAFP, compatible, pending, lab pending, and all virus types (eg VDPV, Wild 1)​",fp_text(font.size = 17))),
      fpar(ftext("Pending is when there is no virus type and classification is pending​",fp_text(font.size = 17))),
      fpar(ftext('Lab pending is no virus type, classification is pending, and final culture result is "not received in lab"',fp_text(font.size = 17))),
      fpar(ftext("Laboratory classification is determined from Virus Type field",fp_text(font.size = 17))),
      fpar(ftext("Note that this does not take into account the vdpv1, vdpv2, vdpv3, and wild1 computed variables in POLIS​",fp_text(font.size = 17))),
      fpar(ftext("Extract virus from virus type and then use classification vdpv to determine ambiguous, immune deficient, or circulating for VDPVs",fp_text(font.size = 17))),
      fpar(ftext("Important caveats",fp_text(font.size = 17))),
      fpar(ftext("Case classification and lab results are updated in POLIS as lab results come in – this can and does result in varying time delays based on shipping and lab testing times",fp_text(font.size = 17))),
      fpar(ftext("Vdpv1, 2, and 3 variables in POLIS are also extracted from virus type​",fp_text(font.size = 17))),
      fpar(ftext("There can be discrepancies between the two",fp_text(font.size = 17))),
      fpar(ftext("Can be difficult to identify dual infections",fp_text(font.size = 17)))
    ),
    location = ph_location_type("body"),
    level_list = c(1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L)
    ) %>%
    add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Epi Data Cleaning",
            location = ph_location_type("title")) %>%
    ph_with(value = block_list(
      fpar(ftext("Filling missing geographic data",fp_text(font.size = 17))),
      fpar(ftext("Compare epid with previously reported case epids​",fp_text(font.size = 17))),
      fpar(ftext("If EPID prov and dist code match with previously reported case in the same onset year, then backfill prov and dist",fp_text(font.size = 17))),
      fpar(ftext("If no match, expand search to any year, then backfill prov and dist",fp_text(font.size = 17))),
      fpar(ftext("If there are multiple guids identified with the same prov and dist for the match, then do not match and leave blank (cannot tell which GUID to use)",fp_text(font.size = 17))),
      fpar(ftext(paste0("There are ", (incomplete.adm.dist |> length()), " missing district from raw data"), fp_text(font.size = 17))),
      fpar(ftext("Able to extract district from EPID for (manually fill here) cases.", fp_text(font.size = 17)))
    ),
    location = ph_location_type("body"),
    level_list = c(1L, 2L, 2L, 3L, 3L, 1L, 2L)
    ) %>%
    add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Lab Data Cleaning",
            location = ph_location_type("title")) %>%
    ph_with(value = block_list(
      fpar(ftext("Steps for lab cleaning",fp_text(font.size = 17))),
      fpar(ftext("Extract year from onset date if available",fp_text(font.size = 17))),
      fpar(ftext("For non-cases/no onset date – extracted from EPID",fp_text(font.size = 17))),
      fpar(ftext("Deduplicated using lab master key (MasterKey)",fp_text(font.size = 17))),
      fpar(ftext("Data current up to (manually edit here)", fp_text(font.size = 17))),
      fpar(ftext("If a master key was duplicated, used the more recent appearance (the later database)",fp_text(font.size = 17))),
      fpar(ftext("Matching province",fp_text(font.size = 17))),
      fpar(ftext("Match epids between lab and epi data – use province from epi data",fp_text(font.size = 17))),
      fpar(ftext("Note: a number of mismatches observed (hard to quantify since a subset of these are spelling differences)", fp_text(font.size = 17, color = "red"))),
      fpar(ftext("Cross check epids with previous epids to identify same province code",fp_text(font.size = 17))),
      fpar(ftext(paste0("Created look up table for provinces and districts from previous epids",
                        " matching geographic columns from other complete entries."),fp_text(font.size = 17)))

    ),
    location = ph_location_type("body"),
    level_list = c(1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 3L, 1L, 1L)
    )



  gc()

  # Print output  ----
  print(draft_output, file.path(
    ppt_output_path,
    paste0("draft_output_", Sys.Date(), "_", country, ".pptx")
  ))

}

