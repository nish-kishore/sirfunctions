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
           "AFG-HLD-1" = "#f4c29d",
           "AFG-NGR-1" = "#b87560",
           "ANG-HUI-1" = "#a66262",
           "ANG-LNO-1" = "#f7dad1",
           "ANG-LNO-2" = "#eed6e2",
           "ANG-LNO-3" = "#68228b",
           "ANG-LUA-1" = "#ec9c9b",
           "ANG-MOX-1" = "#d7eed6",
           "BOT-FRA-1" = "#458B74",
           "CAE-EST-1" = "#d0b6ce",
           "CAE-EXT-1" = "#fd8d3c",
           "CAF-BAM-1" = "#ffabc0",
           "CAF-BAM-2" = "#ff6c91",
           "CAF-BER-1" = "#c0cfe0",
           "CAF-BIM-1" = "#7c4e1f",
           "CAF-BIM-2" = "#6c8b3c",
           "CAF-BIM-3" = "#93f3f1",
           "CAF-BNG-1" = "#54715d",
           "CAF-BNG-2" = "#d21404",
           "CAF-BNG-3" = "#00008b",
           "CAF-KEM-1" = "#fdff7a",
           "CAF-MOZ-1" = "#f4a900",
           "CHA-NDJ-1" = "#00a550",
           "CHN-SHA-1" = "#e2b714",
           "CHN-SIC-1" = "#2d9cce",
           "cVDPV3" = "#d85116",
           "cVDPV3-ISR" = "#3f6f2a",
           "EGY-NOR-1" = "#8B2323",
           "EGY-QEN-1" = "#ff6d5f",
           "ETH-ORO-1" = "#2d206e",
           "ETH-ORO-2" = "#f6ab7b",
           "ETH-ORO-3" = "#c04986",
           "ETH-ORO-4" = "#9066ff",
           "ETH-SOM-1" = "#b300b3",
           "ETH-SOU-1" = "#f7a000",
           "ETH-SOU-2" = "#ffdb99",
           "ETH-SOU-3" = "#c87941",
           "ETH-TIG-1" = "#CA5621",
           "GUF-SGO-1" = "#a5a58d",
           "GUI-KAN-1" = "#7a704e",
           "INO-ACE-1" = "#ED7222",
           "INO-cVDPV2-n" = "#6d3643",
           "INO-PAP-1" = "#78141e",
           "INO-PAP-2" = "#fcf3c8",
           "IUUC-2022" = "#2d435b",
           "MAD-ANO-1" = "#fac263",
           "MAD-ANO-2" = "#B254A5",
           "MAD-SUE-1" = "#e1f635",
           "MAD-SUO-1" = "#04b0ab",
           "MMR-1" = "#8b6c7c",
           "MOZ-MAN-1" = "#458B00",
           "MOZ-NPL-1" = "#556518",
           "MOZ-NPL-2" = "#008B8b",
           "NIE-BOS-1" = "#e56565",
           "NIE-JIS-1" = "#63461a",
           "NIE-KBS-1" = "#8afffc",
           "NIE-KGS-1" = "#39654e",
           "NIE-KGS-2" = "#41870a",
           "NIE-KTS-1" = "#68228b",
           "NIE-SOS-3" = "#1546c9",
           "NIE-SOS-4" = "#4d00fe",
           "NIE-SOS-5" = "#fdcf04",
           "NIE-SOS-6" = "#7f0000",
           "NIE-SOS-7" = "#edb509",
           "NIE-SOS-8" = "#0877da",
           "NIE-YBS-1" = "#b78c09",
           "NIE-ZAS-1" = "#F5191C",
           "NIG-DOS-1" = "#ff9999",
           "PAK-FSD-1" = "#a680b7",
           "PAK-FSD-2" = "#4e9c35",
           "PAK-GB-1" = "#d01212",
           "PAK-GB-2" = "#dae05b",
           "PAK-GB-3" = "#f4caa6",
           "PAK-KAM-1" = "#db5a5a",
           "PAK-KOH-1" = "#baeea0",
           "PAK-LKW-1" = "#008080",
           "PAK-PB-1" = "#004c4c",
           "PAK-PWR-1" = "#b2d8d8",
           "PAK-TOR-1" = "#be29ec",
           "PAK-ZHB-1" = "#fff9ae",
           "PAK−KHI−2" = "#993823",
           "PHL-NCR-1" = "#8585b7",
           "PHL-NCR-2" = "#800080",
           "RDC-BUE-1" = "#8B8B83",
           "RDC-cVDPV2" = "#000000",
           "RDC-EQT-1" = "#666600",
           "RDC-HKA-2" = "#B14A34",
           "RDC-HLO-2" = "#308c72",
           "RDC-HLO-3" = "#f2a950",
           "RDC-HLO-4" = "#efbbff",
           "RDC-KAS-1" = "#f27b35",
           "RDC-KAS-2" = "#aef458",
           "RDC-KAS-3" = "#f8ed62",
           "RDC-KOR-1" = "#104E8B",
           "RDC-MAN-2" = "#c74b38",
           "RDC-MAN-3" = "#556B2F",
           "RDC-MAN-4" = "#665811",
           "RDC-MAN-5" = "#B5651d",
           "RDC-SAN-1" = "#279944",
           "RDC-SKV-1" = "#8B3A3A",
           "RDC-TAN-1" = "#CA5621",
           "RDC-TAN-2" = "#0d6f2b",
           "RDC-TPA-1" = "#ff00a9",
           "RDC-TPA-2" = "#664d33",
           "RDC-TSH-1" = "#AC6A9F",
           "RDC-TSH-2" = "#F5191C",
           "RDC-TSH-3" = "#a65a2e",
           "RSS-CEQ-1" = "#e9d700",
           "RSS-JON-1" = "#fd8d3c",
           "RSS-UNL-1" = "#2D7E47",
           "RSS-WEQ-1" = "#8B8B83",
           "SOM-AWL-1" = "#8efeb3",
           "SOM-BAN-1" = "#891171",
           "SOM-BAY-1" = "#7C68B3",
           "SUD-RED-1" = "#F28265",
           "TOG-SAV-1" = "#f9cb9c",
           "YEM-SAD-1" = "#fb9f9f",
           "YEM-SAN-1" = "#dab600",
           "YEM-TAI-1" = "#4A708B",
           "ZAM-LUA-1" = "#b45f06",
           "ZIM-HRE-1" = "#A03E3F",
           "ZIM-HRE-2" = "#fb5858",
           "R4B5C4C" = "",
           "R4B5C4C+R4B5C5B2" = "",
           "R4B5C4C1" = "#a98600",
           "R4B5C4C2" = "#f24e29",
           "R4B5C4C2+R4B5C5B2B" = "",
           "R4B5C4D" = "",
           "R4B5C4D+R4B5C5B2" = "",
           "R4B5C4D2" = "",
           "R4B5C5B2" = "",
           "R4B5C5B2A" = "",
           "R4B5C5B2B" = "",
           "R4B5C5B2B + R4B5C5B2C" = "",
           "R4B5C5B2B+R4B5C4D" = "",
           "R4B5C5B2B2" = "",
           "R4B5C5B2B3" = "",
           "R4B5C5B2C" = "",
           "R4B5C5B2C1" = "",
           "R4B5C5B2C2" = "",
           "YB3A" = "#4A708B",
           "YB3A4A" = "#AC6A9F",
           "YB3A4A & YB3A4B" = "#fd8d3c",
           "YB3A4B" = "#2D7E47",
           "YB3B"  = "",
           "YB3C" = "#A03E3F",
           "YC2" = ""
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
