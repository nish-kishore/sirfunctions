#' Export the AFP linelist
#'
#' Export the AFP linelist with `adequacy.final2` column. The `adequacy.final2`
#' column describes the status of a stool sample, such as if a
#' stool sample is adequate or inadequate. Specifically, it is
#' created from [generate_stool_data()] which takes parameters on how to deal with missing or
#' inadequate stool samples.
#'
#' @import dplyr writexl
#' @param stool.data `tibble` AFP data with final adequacy columns.
#' This is the output of [generate_stool_data()].
#' @param country `str` Name of the country.
#' @param excel_output_path `str` Output path of the Excel file.
#' @returns None.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' stool.data <- generate_stool_data(
#'   ctry.data$afp.all.2, "good", "inadequate",
#'   "2021-01-01", "2023-12-31"
#' )
#' create_afp_export(stool.data)
#' }
#'
#' @export
create_afp_export <- function(stool.data, country = Sys.getenv("DR_COUNTRY"), excel_output_path = Sys.getenv("DR_TABLE_PATH")) {
  stool.data.export <- stool.data |>
    dplyr::mutate(nvaccine.2 = NA) |>
    dplyr::select(c(
      "polis.case.id", "epid", "followup.date", "followup.findings",
      "investigation.date", "notification.date", "stool.1.collection.date",
      "stool.2.collection.date", "stool.date.sent.to.ic.lab",
      "classification", "source.advanced.notification",
      "advanced.notification", "datenotificationtohq",
      "stool.1.condition", "stool.2.condition",
      "specimen.date", "paralysis.asymmetric", "paralysis.confirmed",
      "paralysis.hot.case", "paralysis.left.arm", "paralysis.left.leg",
      "paralysis.onset.fever", "paralysis.rapid.progress", "paralysis.right.arm",
      "paralysis.right.leg", "paralysis.site", "paralysis.sudden",
      "person.age.in.months", "person.age.in.years", "sex", "ctry", "prov",
      "dist", "admin0guid", "admin1guid", "admin2guid", "country.iso3", "whoregion",
      "ist", "doses.date.of.1st", "doses.date.of.2nd", "doses.date.of.3rd",
      "doses.date.of.4th", "doses.ipv.number", "doses.ipv.date.of.last",
      "doses.ipv.routine", "doses.ipv.sia", "doses.opv.date.of.last",
      "doses.opv.routine", "doses.opv.sia", "doses.total", "calcdosesrisi",
      "results.seq.date.to.program", "last.opv.sia.date", "last.ipv.sia.date",
      "created.date", "last.updated.date", "vaccine.1", "vaccine.2", "vaccine.3",
      "vdpv.1", "vdpv.2", "vdpv.3", "wild.1", "wild.3", "nvaccine.2", "calculated.age.(months)",
      "polis.longitude", "polis.latitude", "surveillancetypename",
      "case.date", "provisional.diagnosis", "adequate.stool", "stool.adequacy.with.condition",
      "final.cell.culture.result", "is.breakthrough", "poliovirustypes",
      "virus.cluster", "emergence.group", "nt.changes", "classificationvdpv",
      "vdpv.classification.id(s)", "anonymousepid", "epidateisolationresultsreceived",
      "epidateitdresultsreceived", "admin0id", "admin1id", "admin2id",
      "admin0officialname", "admin1officialname", "admin2officialname",
      "followupfindingscode", "stool1conditioncode", "stool2conditioncode",
      "publishdate", "surveillancetypecode", "virustypeids", "date",
      "year", "date.notify", "date.invest", "datestool1", "datestool2", "ontostool2",
      "ontostool1", "age.months", "ontonot", "ontoinvest", "nottoinvest",
      "investtostool1", "stool1tostool2", "stooltolabdate",
      "stooltoiclabdate", "clinicadmitdate", "vtype", "vtype.fixed", "cdc.class",
      "hot.case", "sabin1", "sabin2", "sabin3", "lon", "lat", "bad.onset", "bad.notify",
      "bad.invest", "bad.stool1", "bad.stool2", "bad.followup", "stool2missing",
      "stool1missing", "stoolmissing", "need60day", "got60day", "timeto60day",
      "ontime.60day", "adm0guid", "adm1guid", "adm2guid", "totalnumberofdosesipvopv",
      "cdc.classification.all2", "daysstooltolab", "noti.7d.on",
      "inv.2d.noti", "coll.3d.inv", "ship.3d.coll", "adequacy.final",
      "adequacy.final2"
    ))

  writexl::write_xlsx(
    stool.data.export,
    file.path(excel_output_path, paste0(
      Sys.Date(), "_",
      "AFP Linelist_", country, ".xlsx"
    ))
  )
}

#' Export stool adequacy data
#'
#' The function combines the stool adequacy summary tables from [f.stool.ad.01()]
#' and exports to an Excel file, with each geographic level on its own tab.
#'
#' @import writexl
#' @param cstool `tibble` Stool adequacy at country level.
#' @param pstool `tibble` Stool adequacy at province level.
#' @param dstool `tibble` Stool adequacy at district level.
#' @param excel_output_path `str` Output path.
#' @returns None.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' cstool <- f.stool.ad.01(
#'   ctry.data$afp.all.2, ctry.data$ctry.pop,
#'   "2021-01-01", "2023-01-01", "ctry"
#' )
#' pstool <- f.stool.ad.01(
#'   ctry.data$afp.all.2, ctry.data$prov.pop,
#'   "2021-01-01", "2023-01-01", "prov"
#' )
#' dstool <- f.stool.ad.01(
#'   ctry.data$afp.all.2, ctry.data$dist.pop,
#'   "2021-01-01", "2023-01-01", "dist"
#' )
#' create_stool_adequacy_export(cstool, pstool, dstool)
#' }
#'
#' @export
create_stool_adequacy_export <- function(cstool, pstool, dstool, excel_output_path = Sys.getenv("DR_TABLE_PATH")) {
  sheets <- list(
    "country_npafp" = cstool,
    "province_npafp" = pstool,
    "district_npafp" = dstool
  )
  writexl::write_xlsx(sheets, file.path(
    excel_output_path,
    paste0(Sys.Date(), "_", "stool_adequacy_indicators.xlsx")
  ))
}

#' Exports NPAFP indicator data summary tables
#'
#' The function combines the NPAFP rate summary tables from [f.npafp.rate.01()]
#' and exports to an Excel file, with each geographic level on its own tab.
#' @import writexl
#' @param ctry.case.ind `tibble` Country NPAFP indicator summary table.
#' @param prov.case.ind `tibble` Province NPAFP indicator summary table.
#' @param dist.case.ind `tibble` District NPAFP indicator summary table.
#' @param excel_output_path `str` Output path of the Excel file.
#' @returns None.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' ctry.case.ind <- f.npafp.rate.01(
#'   ctry.data$afp.all.2, ctry.data$ctry.pop,
#'   "2021-01-01", "2023-01-01", "ctry"
#' )
#' prov.case.ind <- f.npafp.rate.01(
#'   ctry.data$afp.all.2, ctry.data$prov.pop,
#'   "2021-01-01", "2023-01-01", "prov"
#' )
#' dist.case.ind <- f.npafp.rate.01(
#'   ctry.data$afp.all.2, ctry.data$dist.pop,
#'   "2021-01-01", "2023-01-01", "dist"
#' )
#' create_npafp_export(ctry.case.ind, prov.case.ind, dist.case.ind)
#' }
#'
#' @export
create_npafp_export <- function(ctry.case.ind, prov.case.ind, dist.case.ind, excel_output_path = Sys.getenv("DR_TABLE_PATH")) {
  sheets <- list(
    "country_npafp" = ctry.case.ind,
    "province_npafp" = prov.case.ind,
    "district_npafp" = dist.case.ind
  )
  # !!! need to be put into a condition where it would create the folder if it doesn't already exist
  writexl::write_xlsx(sheets, file.path(
    excel_output_path,
    paste0(Sys.Date(), "_", "npafp_indicators.xlsx")
  ))
}

#' Exports file for checking population roll-ups
#'
#' Export the population roll-ups and determine differences between each population counts.
#'
#' @import dplyr writexl tidyr
#' @param ctry.data `list` A large list containing polio data for a country. This is the output of either
#' [init_dr()] or [extract_country_data()].
#' @param country `str` Name of the country.
#' @param excel_output_path `str` Output path of the Excel file.
#' @returns None.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' create_pop_check_export(ctry.data)
#' }
#'
#' @export
create_pop_check_export <- function(ctry.data, country = Sys.getenv("DR_COUNTRY"),
                                    excel_output_path = Sys.getenv("DR_TABLE_PATH")) {
  pop.check <-
    dplyr::count(
      dplyr::ungroup(dplyr::filter(ctry.data$dist.pop, year >= min(year))), .data$prov, .data$dist,
      .data$adm1guid, .data$adm2guid, .data$year, .data$u15pop
    )

  pop.check1 <- as.data.frame(tidyr::pivot_wider(pop.check,
    names_from = "year",
    values_from = "u15pop"
  ))

  pop_rollup_diff <- suppressMessages(check_pop_rollout(ctry.data))

  sheets <- list(
    "districts_by_year" = pop.check1,
    "population_comparison" = pop_rollup_diff
  )

  writexl::write_xlsx(sheets, file.path(
    excel_output_path,
    paste0(
      Sys.Date(), "_",
      "population_check_",
      country, ".xlsx"
    )
  ))
}

#' Export 60-day follow up table
#'
#' Exports the output of [generate_60_day_table_data] into an Excel file.
#' @import dplyr readr
#' @param cases.need60day `tibble` Summary table for 60-day follow-up.
#' @param country `str` Name of the country.
#' @param excel_output_path `str` Output path of the Excel file.
#' @returns None.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' stool.data <- generate_stool_data(
#'   ctry.data$afp.all.2, "good", "inadequate",
#'   "2021-01-01", "2023-12-31"
#' )
#' cases.need60day <- generate_60_day_table_data(stool.data, start_date, end_date)
#' create_60_day_export(cases.need60day)
#' }
#'
#' @export
create_60_day_export <- function(cases.need60day, country = Sys.getenv("DR_COUNTRY"), excel_output_path = Sys.getenv("DR_TABLE_PATH")) {
  cases.need60day |>
    dplyr::rename(
      "Year" = "year",
      "age in months" = "age.months",
      "Hot Case" = "hot.case",
      "Hot Case, no review" = "hot.case.no.review",
      "Complete 60 day" = "got60day",
      "60-day ontime" = "ontime.60day",
      "Country" = "ctry",
      "Province" = "prov",
      "District" = "dist",
      "Adequate stool missing=good" = "adequacy.03",
      "total OPV doses" = "doses.total",
      "Potentially Compatible" = "pot.compatible",
      "Missing followup date but have findings" = "missing.fu.date"
    ) |>
    readr::write_csv(file.path(
      excel_output_path,
      paste0(Sys.Date(), "_", stringr::str_to_lower(country), "_60day_followup.csv")
    ))
}

#' Export potentially compatible and compatible summary table
#'
#' Exports the output of [generate_potentially_compatibles_cluster()] as an Excel file.
#' @import writexl
#' @param pot.c.clust `tibble` Potentially compatible cluster summary. The output of
#' [generate_potentially_compatibles_cluster()].
#' @param country `str` Name of the country.
#' @param excel_output_path `str` Output path of where to store the Excel file.
#' @returns None.
#' @examples
#' \dontrun{
#' ctry.data <- init_dr("algeria")
#' stool.data <- generate_stool_data(
#'   ctry.data$afp.all.2, "good", "inadequate",
#'   "2021-01-01", "2023-12-31"
#' )
#' cases.need60day <- generate_60_day_table_data(stool.data, start_date, end_date)
#' pot.c.clust <- generate_potentially_compatibles_cluster(cases.need60day)
#' create_pot_comp_clust_export(pot.c.clust)
#' }
#'
#' @export
create_pot_comp_clust_export <- function(pot.c.clust,
                                         country = Sys.getenv("DR_COUNTRY"),
                                         excel_output_path = Sys.getenv("DR_TABLE_PATH")) {
  writexl::write_xlsx(
    pot.c.clust,
    file.path(
      excel_output_path,
      paste0(
        "compatible_pot_compatible_cases_",
        country, ".xlsx"
      )
    )
  )
}
