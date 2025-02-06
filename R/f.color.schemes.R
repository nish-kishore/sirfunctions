#' Utility function for colors
#'
#' Utility function to return SIR color schemes used in various graphs and visualizations.
#'
#' @param type `str` Type of colors we can return. Accepted values include:
#' - `"epicurve"` Mapped to different `cdc.classification.all2` values.
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
#' - `"emergence.groups"` Standard emergence group colors. Used primarily
#' with [generate_adhoc_map()].
#' - `"es.vaccine.types"` Default vaccine types. Used primarily with
#' [generate_es_site_det()].
#' - `"es.detections"` Default detection types. Used primarily with
#' [generate_es_site_det()].
#' @returns Named `list` with color sets.
#' @examples
#' color_list <- f.color.schemes("epicurve")
#'
#' @export

f.color.schemes <- function(type) {
  types <- c(
    "epicurve", "para.case", "afp.prov", "afp.dist",
    "pot.comp", "silence", "silence.v2", "cases", "es", "mapval",
    "timeliness.col.vars", "emergence.groups", "es.vaccine.types",
    "es.detections"
  )

  if (!type %in% types) {
    cli::cli_alert_warning(c(
      paste0(
        "Your selection is not one of the standard SIR color schemes, ",
        "please choose one of the following: "
      )
    ))
    for (i in types) {
      cli::cli_alert_info(paste0(i))
    }
    cli::cli_abort("Please run the function again with a valid argument.")
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
         "timeliness.col.vars" = c(
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
         ),
         "emergence.groups" = c(
           "AFG-HLD-1" = "",
           "AFG-NGR-1" = "",
           "ANG-HUI-1" = "",
           "ANG-LNO-1" = "",
           "ANG-LNO-2" = "",
           "ANG-LNO-3" = "#68228b",
           "ANG-LUA-1" = "",
           "ANG-MOX-1" = "",
           "BOT-FRA-1" = "#458B74",
           "CAE-EST-1" = "",
           "CAE-EXT-1" = "#fd8d3c",
           "CAF-BAM-1" = "",
           "CAF-BAM-2" = "",
           "CAF-BER-1" = "",
           "CAF-BIM-1" = "",
           "CAF-BIM-2" = "",
           "CAF-BIM-3" = "",
           "CAF-BNG-1" = "",
           "CAF-BNG-2" = "#d21404",
           "CAF-BNG-3" = "#00008b",
           "CAF-KEM-1" = "",
           "CAF-MOZ-1" = "",
           "CHA-NDJ-1" = "",
           "CHN-SHA-1" = "",
           "CHN-SIC-1" = "",
           "cVDPV3" = "",
           "cVDPV3-ISR" = "",
           "EGY-NOR-1" = "#8B2323",
           "EGY-QEN-1" = "",
           "ETH-ORO-1" = "",
           "ETH-ORO-2" = "",
           "ETH-ORO-3" = "",
           "ETH-ORO-4" = "",
           "ETH-SOM-1" = "",
           "ETH-SOU-1" = "",
           "ETH-SOU-2" = "",
           "ETH-SOU-3" = "",
           "ETH-TIG-1" = "#CA5621",
           "GUF-SGO-1" = "",
           "GUI-KAN-1" = "",
           "INO-ACE-1" = "#ED7222",
           "INO-cVDPV2-n" = "#458B00",
           "INO-PAP-1" = "",
           "INO-PAP-2" = "",
           "IUUC-2022" = "",
           "MAD-ANO-1" = "",
           "MAD-ANO-2" = "#B254A5",
           "MAD-SUE-1" = "",
           "MAD-SUO-1" = "",
           "MMR-1" = "",
           "MOZ-MAN-1" = "#458B00",
           "MOZ-NPL-1" = "",
           "MOZ-NPL-2" = "#008B8b",
           "NIE-BOS-1" = "",
           "NIE-JIS-1" = "",
           "NIE-KBS-1" = "",
           "NIE-KGS-1" = "",
           "NIE-KGS-2" = "",
           "NIE-KTS-1" = "#68228b",
           "NIE-SOS-3" = "",
           "NIE-SOS-4" = "",
           "NIE-SOS-5" = "",
           "NIE-SOS-6" = "",
           "NIE-SOS-7" = "",
           "NIE-SOS-8" = "",
           "NIE-YBS-1" = "",
           "NIE-ZAS-1" = "#F5191C",
           "NIG-DOS-1" = "",
           "PAK-FSD-1" = "",
           "PAK-FSD-2" = "",
           "PAK-GB-1" = "",
           "PAK-GB-2" = "",
           "PAK-GB-3" = "",
           "PAK-KAM-1" = "",
           "PAK-KOH-1" = "",
           "PAK-LKW-1" = "",
           "PAK-PB-1" = "",
           "PAK-PWR-1" = "",
           "PAK-TOR-1" = "",
           "PAK-ZHB-1" = "",
           "PAK−KHI−2" = "",
           "PHL-NCR-1" = "",
           "PHL-NCR-2" = "",
           "RDC-BUE-1" = "#8B8B83",

           "RDC-HKA-2" = "#B14A34",
           "RDC-KOR-1" = "#104E8B",
           "RDC-MAN-3" = "#556B2F",
           "RDC-MAN-5" = "#B5651d",
           "RDC-cVDPV2" = "#000000",
           "RDC-SKV-1" = "#8B3A3A",
           "RDC-TAN-1" = "#CA5621",
           "RDC-TSH-1" = "#AC6A9F",
           "RDC-TSH-2" = "#F5191C",
           "RSS-JON-1" = "#fd8d3c",
           "RSS-UNL-1" = "#2D7E47",
           "RSS-WEQ-1" = "#8B8B83",
           "SOM-BAN-1" = "#891171",
           "SOM-BAY-1" = "#7C68B3",
           "SUD-RED-1" = "#F28265",
           "YEM-TAI-1" = "#4A708B",
           "ZIM-HRE-1" = "#A03E3F",
           "YB3A" = "#4A708B",
           "YB3C" = "#A03E3F",
           "YB3A4A" = "#AC6A9F",
           "YB3A4A & YB3A4B" = "#fd8d3c",
           "YB3A4B" = "#2D7E47"
         ),
         "es.vaccine.types" = c(
           "nOPV2" = "blue",
           "bOPV" = "coral1",
           "mOPV2" = "purple"
         ),
         "es.detections" = c(
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
  )
}
