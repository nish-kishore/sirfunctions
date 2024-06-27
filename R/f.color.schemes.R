#' Utility function for colors
#' @description Utility function to return SIR colors
#' @param type str: type of colors we can return. Accepted values include:
#' 'epicurve', 'para.case', 'afp.prov', 'afp.dist', 'pot.comp', 'silence',
#' 'silence.v2', 'cases', 'es', 'mapval', 'timeliness.col.vars'
#' @returns list: color sets
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
    "timeliness.col.vars" =
      c(
      "Paralysis onset to notification" = "#377eb8",
      "Case notification to investigation" = "#e41a1c",
      "Case investigation to stool 1 collection" = "#4daf4a",
      "Stool 1 collection to stool 2 collection" = "#984ea3",
      "Last stool collection to received in lab" = "#ff7f00",
      "Last stool collection sent to lab" = "#ff7f00",
      "Stool received lab to final culture results" = "#b55f04"
    )
  )
}
