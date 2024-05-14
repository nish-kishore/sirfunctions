#' Exports AFP linelist with adequacy.final2 column
#'
#' @param stool.data afp data with final adequacy columns
#' @param excel_output_path output path of the Excel file
#' @export
create_afp_export <- function(stool.data, country, excel_output_path) {
  stool.data.export=stool.data |>
    mutate(nvaccine.2 = NA) |>
    select(c("polis.case.id","epid","followup.date","followup.findings",
             "investigation.date","notification.date","stool.1.collection.date",
             "stool.2.collection.date","stool.date.sent.to.ic.lab",
             "classification","source.advanced.notification",
             "advanced.notification","datenotificationtohq",
             "stool.1.condition","stool.2.condition",
             "specimen.date","paralysis.asymmetric","paralysis.confirmed",
             "paralysis.hot.case","paralysis.left.arm","paralysis.left.leg",
             "paralysis.onset.fever","paralysis.rapid.progress","paralysis.right.arm",
             "paralysis.right.leg","paralysis.site","paralysis.sudden",
             "person.age.in.months","person.age.in.years","sex","ctry","prov",
             "dist","admin0guid","admin1guid","admin2guid","country.iso3","whoregion",
             "ist","doses.date.of.1st","doses.date.of.2nd","doses.date.of.3rd",
             "doses.date.of.4th","doses.ipv.number","doses.ipv.date.of.last",
             "doses.ipv.routine","doses.ipv.sia","doses.opv.date.of.last",
             "doses.opv.routine","doses.opv.sia","doses.total","calcdosesrisi",
             "results.seq.date.to.program","last.opv.sia.date","last.ipv.sia.date",
             "created.date","last.updated.date","vaccine.1","vaccine.2","vaccine.3",
             "vdpv.1","vdpv.2","vdpv.3","wild.1","wild.3","nvaccine.2","calculated.age.(months)",
             "polis.longitude","polis.latitude","surveillancetypename",
             "case.date","provisional.diagnosis","adequate.stool","stool.adequacy.with.condition",
             "final.cell.culture.result","is.breakthrough","poliovirustypes",
             "virus.cluster","emergence.group","nt.changes","classificationvdpv",
             "vdpv.classification.id(s)","anonymousepid","epidateisolationresultsreceived",
             "epidateitdresultsreceived","admin0id","admin1id","admin2id",
             "admin0officialname","admin1officialname","admin2officialname",
             "followupfindingscode","stool1conditioncode","stool2conditioncode",
             "publishdate","surveillancetypecode","virustypeids","date",
             "year","date.notify","date.invest","datestool1","datestool2","ontostool2",
             "ontostool1","age.months","ontonot","ontoinvest","nottoinvest",
             "investtostool1","stool1tostool2","stooltolabdate",
             "stooltoiclabdate","clinicadmitdate","vtype","vtype.fixed","cdc.class",
             "hot.case","sabin1","sabin2","sabin3","lon","lat","bad.onset","bad.notify",
             "bad.invest","bad.stool1","bad.stool2","bad.followup","stool2missing",
             "stool1missing","stoolmissing","need60day","got60day","timeto60day",
             "ontime.60day","adm0guid","adm1guid","adm2guid","totalnumberofdosesipvopv",
             "cdc.classification.all2","daysstooltolab","noti.7d.on",
             "inv.2d.noti","coll.3d.inv","ship.3d.coll","adequacy.final",
             "adequacy.final2"
    ))

  writexl::write_xlsx(stool.data.export,
                       file.path(excel_output_path, paste0(Sys.Date(), "_",
                                                           "AFP Linelist_",country, ".xlsx")))

}

#' Export stool adequacy data
#'
#' @param cstool stool adequacy at country level
#' @param pstool stool adequacy at province level
#' @param dstool stool adequacy at district level
#' @param excel_output_path output path
#'
#' @return does not return anything
#' @export
create_stool_adequacy_export <- function(cstool, pstool, dstool, excel_output_path) {
  sheets <- list(
    "country_npafp" = cstool,
    "province_npafp" = pstool,
    "district_npafp" = dstool
  )
  write_xlsx(sheets, file.path(
    excel_output_path,
    paste0(Sys.Date(), "_", "stool_adequacy_indicators.xlsx")
  ))
}

#' Export NPAFP data
#'
#' @param ctry.case.ind country NPAFP indicator
#' @param prov.case.ind province NPAFP indicator
#' @param dis.case.ind district NPAFP indicator
#' @param excel_output_path output path
#'
#' @return does not return anything
#' @export
create_npafp_export <- function(ctry.case.ind, prov.case.ind, dis.case.ind, excel_output_path) {

  sheets <- list(
    "country_npafp" = ctry.case.ind,
    "province_npafp" = prov.case.ind,
    "district_npafp" = dis.case.ind
  )
  # !!! need to be put into a condition where it would create the folder if it doesn't already exist
  write_xlsx(sheets, file.path(
    excel_output_path,
    paste0(Sys.Date(), "_", "npafp_indicators.xlsx")
  ))

}

#' File for checking population roll-ups
#'
#' @param ctry.data RDS file containing polio data for a country
#' @param ctry.data.error.log error log of a country
#' @param country name of the country
#' @param excel_output_path output path
#'
#' @return does not return anything
#' @export
create_pop_check_export <- function(ctry.data, ctry.data.error.log, country, excel_output_path) {
  pop.check <-
    count(ungroup(filter(ctry.data$dist.pop, year >= min(year))), prov, dist,
          adm1guid, adm2guid, year, u15pop)

  pop.check1 = as.data.frame(pivot_wider(pop.check,
                                         names_from = year,
                                         values_from = u15pop))

  sheets <- list("districts_by_year" = pop.check1,
                 "population_comparison" = ctry.data.error.log$pop_rollup_diff)

  write_xlsx(sheets,  file.path(
    excel_output_path,
    paste0(Sys.Date(), "_",
           "population_check_",
           country, ".xlsx")
  ))
}

#' Output for 60-day follow ups
#'
#' @param cases.need60day table for 60 day follow up
#' @param country name of the country
#' @param excel_output_path output path
#'
#' @return does not return anything
#' @export
create_60_day_export <- function(cases.need60day, country, excel_output_path) {
  cases.need60day |>
    rename(
      Year = year,
      "age in months" = age.months,
      "Hot Case" = hot.case,
      "Hot Case, no review" = hot.case.no.review,
      "Complete 60 day" = got60day,
      "60-day ontime" = ontime.60day,
      Country = ctry,
      Province = prov,
      District = dist,
      "Adequate stool missing=good" = adequacy.03,
      "total OPV doses" = doses.total,
      "Potentially Compatible" = pot.compatible,
      "Missing followup date but have findings" = missing.fu.date
    ) |>
    write_csv(file.path(
      excel_output_path,
      paste0(Sys.Date(), "_", str_to_lower(country), "_60day_followup.csv")
    ))
}

#' Import potentially compatible and compatible summaries
#'
#' @param pot.c.clust potentially compatible cluster summary
#' @param country name of the country
#' @param excel_output_path output path
#'
#' @return does not return anything
#' @export
create_pot_comp_clust_export <- function(pot.c.clust, country, excel_output_path) {
  write_xlsx(pot.c.clust,
             file.path(
               excel_output_path,
               str_to_lower(country),
               year(Sys.Date()),
               "data",
               paste0("compatible_pot_compatible_cases_",
                      country, ".xlsx")
             ))
}
