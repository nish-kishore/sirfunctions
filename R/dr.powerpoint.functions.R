#' Generate PowerPoint assumptions
#'
#' @param start_date start date of desk review
#' @param end_date end date of desk review
#'
#' @return list of strings
#' @export
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

  assump = unordered_list(
    level_list = c(1, 2, 2, 1,2,1,2,1,2,2,2), #Indentation level for each bullet
    str_list = pptx.assumptions,
    style = fp_text(color = "black", font.size = 18))

  return(assump)

}

#' Generate the desk review slide deck
#'
#' @param ppt_template_path path to the PowerPoint template
#' @param ctry.data RDS file containing polio data for a country
#' @param assump list of assumptions
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
generate_dr_ppt <- function(ppt_template_path, ctry.data, assump, start_date, end_date,
                            pop.map, pop.map.prov, afp.case.map, afp.epi.curve,
                            surv.ind.tab, afp.dets.prov.year, npafp.map,
                            npafp.map.dist, stool.ad.maps, stool.ad.maps.dist,
                            inad.tab.flex, tab.60d,timely_nation, timely_prov,
                            mapt_all, es.site.det, es.det.map, es.timely,
                            es.table, country, ppt_output_path
                            ) {

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
    ph_with(value = "AFP Detections by Province and Year",
            location = ph_location_type("title")) %>%
    ph_with(dml(ggobj = afp.dets.prov.year),
            location = ph_location_label("Content Placeholder 2")) %>%
    ph_with(pop.tab, location = ph_location_label("Content Placeholder 3")) %>%
    # NPAFP rate/AFP detection by province by year (maps) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "NPAFP rate",
            location = ph_location_type("title")) %>%
    ph_with(dml(ggobj = npafp.map), location = ph_location_type("body")) %>%
    # NPAFP rate/AFP detection by district by year (maps) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "NPAFP rate District",
            location = ph_location_type("title")) %>%
    ph_with(dml(ggobj = npafp.map.dist), location = ph_location_type("body")) %>%
    # Stool adequacy by province by year (maps) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Stool Adequacy",
            location = ph_location_type("title")) %>%
    ph_with(dml(ggobj = stool.ad.maps), location = ph_location_type("body")) %>%
    # Stool adequacy by dist by year (maps) ----
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with(value = "Stool Adequacy",
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
    ph_with(value = "Possible clusters of under-immunized NPAFP cases",
            location = ph_location_type("title")) %>%
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
