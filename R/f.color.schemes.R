#' Utility function for colors
#'
#' Utility function to return SIR color schemes used in various graphs and visualizations.
#'
#' @param type `str` Type of colors we can return. Accepted values include:
#' -`"epicurve"` Mapped to different `cdc.classification.all2` values.
#' - `"para.case"` A subset of `"epicurve"` representing paralytic cases.
#' - `"afp.prov"` Mapped for case counts at the province level.
#' - `"afp.dist"` Mapped for case counts at the province level.
#' - `"pot.comp"` Colors for categories of compatibles and potentially compatibles.
#' - `"silence"` Colors to use to map silent populations.
#' - `"silence.v2"` Colors to use to map silent populations.
#' - `"cases"` Values to map case type.
#' - `"es"` Values used in ES data.
#' - `"mapval"` Values used for creating maps with percentages.
#' - `"timeliness.col.vars"` Mapping intervals  used for lab timeliness intervals graphs.
#' @returns Named `list` with color sets.
#' @examples
#' color_list <- f.color.schemes("epicurve")
#'
#' @export

f.color.schemes <- function(type) {
  types <- c(
    "epicurve", "para.case", "afp.prov", "afp.dist",
    "pot.comp", "silence", "silence.v2", "cases", "es", "mapval",
    "timeliness.col.vars"
  )

  if (!type %in% types) {
    stop(paste0(
      "Your selection is not one of the standard SIR color schemes, please choose one of the following: ",
      paste0(types, collapse = ", ")
    ))
  }

  switch(type,
    "epicurve" =
      c(
        "WILD 1" = "#e41a1c",
        "cVDPV 2" = "#4daf4a",
        "VDPV 2" = "#a6d854",
        "cVDPV 1" = "#377eb8",
        "VDPV 1" = "#80b1d3",
        "COMPATIBLE" = "#ff7f00",
        "PENDING" = "#993404",
        "LAB PENDING" = "#fccde5",
        "NPAFP" = "#666666",
        "NOT-AFP" = "#beaed4"
      ),
    "para.case" =
      c(
        "WILD 1" = "#e41a1c",
        "cVDPV 2" = "#4daf4a",
        "VDPV 2" = "#a6d854",
        "cVDPV 1" = "#377eb8",
        "VDPV 1" = "#80b1d3",
        "COMPATIBLE" = "#ff7f00"
      ),
    "afp.prov" = {
      c(
        "0" = "grey",
        "1" = "#ffffcc",
        "2-5" = "#a1dab4",
        "6-9" = "#41b6c4",
        "10+" = "#225ea8"
      )
    },
    "afp.dist" =
      c(
        # "0"="grey",
        "1" = "#ffffcc",
        "2" = "#a1dab4",
        "3-5" = "#41b6c4",
        "6+" = "#225ea8"
      ),
    "pot.comp" = {
      c(
        "COMPATIBLE" = "#ff7f00",
        "POTENTIALLY COMPATIBLE" = "#984ea3"
      )
    },
    "silence" =
      c(
        "4" = "#d7191c",
        "3" = "#fdae61",
        "2" = "#ffffbf",
        "1" = "#abd9e9",
        "0" = "#2c7bb6"
      ),
    "silence.v2" =
      c(
        "4" = "#54278f",
        "3" = "#756bb1",
        "2" = "#9e9ac8",
        "1" = "#cbc9e2",
        "0" = "#f2f0f7"
      ),
    "cases" =
      c(
        "COMPATIBLE" = "black",
        "WILD 1" = "red",
        "cVDPV 2" = "dark green",
        "VDPV 2" = "light green",
        "VDPV 1" = "light blue",
        "cVDPV 1" = "dark blue"
      ),
    "es" =
      c(
        "<50%" = "red",
        "50-79%" = "#feb24c",
        "80-100%" = "#0070c0",
        "<5 samples" = "black"
      ),
    "mapval" =
      c(
        "<20%" = "#D7191C",
        "20-49%" = "#FDAE61",
        "50-79%" = "#FFFFBF",
        "80-89%" = "#A6D96A",
        "90-100%" = "#1A9641",
        "No AFP cases" = "lightgrey",
        "Missing" = "grey34"
      ),
    timeliness.col.vars = c(
      "Paralysis onset to notification" = "#a6cee3",
      "Case notification to investigation" = "#1f78b4",
      "Case investigation to stool 1 collection" = "#b2df8a",
      "Stool 1 collection to stool 2 collection" = "#33a02c",
      "Last stool collection to received in lab" = "#ff7f00",
      "Last stool collection sent to lab" = "#ff7f00",

      "Stool received lab to final culture results" = "#cab2d6",

      "Collection to sent from field" = "#fb9a99",
      "Sent from field to received nat level" = "#e31a1c",
      "Received nat level to sent to lab" = "#fdbf6f",
      "Sent to lab to received at lab" = "#ff7f00",
      "Received at lab to culture results" = "#cab2d6"
    )

  )
}
