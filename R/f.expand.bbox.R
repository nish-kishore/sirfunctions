#' Expand bounding box
#'
#' _Sourced from https://rdrr.io/github/Chrisjb/basemapR/src/R/expand_bbox.R_.
#' A function to take a bounding box (generated using [sf::st_bbox()]) and expand it by x meters
#' in the X direction and y meters in the Y direction.
#'
#' @param bbox `bbox` A bounding box generated by [sf::st_bbox()].
#' @param X `numeric` The distance in meters that we want to expand the bounding box by in the X direction.
#' @param Y `numeric` The distance in meters that we want to expand the bounding box by in the Y direction.
#' @param X2 `numeric` If specified, the meters in the Easterly direction and meters X becomes meters in the Westerly direction.
#' @param Y2 `numeric` If specified, the meters to the South. meters Y becomes meters to the North.
#' @param crs_out `int` EPSG coordinate system to return the bounding box in. Defaults to 4326 (lat/lng).
#'
#' @returns A `bbox` object. This can be converted into an `sf` object using [sf::st_as_sfc()].
#' @examples
#' \dontrun{
#' ctry.shape <- load_clean_ctry_sp(ctry_name = "ALGERIA", st.year = 2019)
#' ctry.bbox <- sf::st_bbox(ctry.shape)
#' bbox_2 <- f.expand.bbox(ctry.bbox, 4, 4)
#' }
#'
#' @export

f.expand.bbox <- function(bbox, X, Y, X2 = X, Y2 = Y,
                          crs_out = 4326) {
  bbox <- bbox |>
    sf::st_as_sfc() |>
    sf::st_transform(crs = 4326) |>
    sf::st_bbox()


  bbox["xmin"] <- bbox["xmin"] - (X / 6370000) * (180 / pi) / cos(bbox["xmin"] * pi / 180)
  bbox["xmax"] <- bbox["xmax"] + (X2 / 6370000) * (180 / pi) / cos(bbox["xmax"] * pi / 180)
  bbox["ymin"] <- bbox["ymin"] - (Y / 6370000) * (180 / pi)
  bbox["ymax"] <- bbox["ymax"] + (Y2 / 6370000) * (180 / pi)

  bbox |>
    sf::st_as_sfc() |>
    sf::st_transform(crs = crs_out) |>
    sf::st_bbox()
}
