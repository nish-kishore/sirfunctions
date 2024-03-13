#' Generate Emergence Group Movement Gifs
#' @description
#' Generate the figures and stitch together a GIF to evaluate emergence group
#' movement over time, generally aggregated as cumulative per month
#' @import dplyr
#' @param emergence_group str: designation of the emergence group to review
#' @param pos tibble: positives data set
#' @param dist sf: shapefile of all districts
#' @param ctry sf: shapefile of all countries
#' @param out_file str: location where gif should be saved
#' @returns GIF written out to a location
create_emergence_group_gif <- function(
  emergence_group,
  pos,
  dist,
  ctry,
  out_file
  ){

  data <- sirfunctions::get_all_polio_data()
  pos <- data$pos
  emergence_group <- "NIE-JIS-1"
  dist <- data$global.dist
  ctry <- data$global.ctry
  out_file <- "C:/Users/ynm2/OneDrive - CDC/NIE_JIS_1.gif"

  #set up data structures
  emergence_group_pos <- pos |>
    dplyr::filter(emergencegroup == emergence_group) |>
    dplyr::select(adm0guid, admin2guid, dateonset, epid, source)

  #group data by month country and district
  emergence_group_pos |>
    mutate(date = as.yearmon(date))

}
