% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_adhoc_map.R
\name{generate_adhoc_map}
\alias{generate_adhoc_map}
\title{Create adhoc maps for emergences}
\usage{
generate_adhoc_map(
  raw.data,
  country,
  virus_type = "cVDPV 2",
  vdpv = T,
  new_detect = T,
  surv = c("AFP", "ES", "OTHER"),
  labels = "YES",
  owner = "CDC-GID-PEB",
  new_detect_expand = F,
  start_date = NULL,
  end_date = NULL,
  emg_cols = NULL,
  output = NULL,
  image_size = NULL,
  height = 6.2,
  width = 4.5,
  scale = 1.25,
  dpi = 300
)
}
\arguments{
\item{raw.data}{\code{list} Global polio data. The output of \code{\link[=get_all_polio_data]{get_all_polio_data()}}.
Make sure the spatial data is attached, otherwise, it will not work.}

\item{country}{\code{str} or \code{list} Country name or a list of country names.}

\item{virus_type}{\code{str} or \code{list}. Virus type to include. Valid values are:

\verb{"cVDPV 1", "cVDPV 2", "cVDPV 3", "WILD 1".}

Can pass as a list.}

\item{vdpv}{\code{logical} Whether to include VPDV in maps. Default \code{TRUE}.}

\item{new_detect}{\code{logical} Whether to highlight new detections based on WHO HQ report date. Default \code{TRUE}.}

\item{surv}{\code{str} or \code{list} Surveillance options. Valid values are:

\verb{"AFP", "ES", "OTHER"}

\code{"OTHER"} includes Case Contact, Community, Healthy Children Sampling. Can pass as a list.}

\item{labels}{\code{str} Include labels for regions with virus detections.
Options:
\itemize{
\item \code{"ALL"}: All regions
\item \code{"YES"}: Recent Detections - <13 months
}}

\item{owner}{\code{str} Who produced the map. Defaults to \code{"CDC-GID-PEB"}.}

\item{new_detect_expand}{\code{logical} Whether to expand the reporting window. Defaults to \code{FALSE}.}

\item{start_date}{\code{str} Start date. If not specified, defaults to 13 months prior to the download date of raw.data.}

\item{end_date}{\code{str} End date. If not specified, defaults to the download date of raw.data.}

\item{emg_cols}{\code{list} A named list with all of the emergence colors. Defaults to \code{NULL}, which will download
using \code{\link[=set_emergence_colors]{set_emergence_colors()}}.}

\item{output}{\code{str} Either a path to a local folder to save the map to, \code{"sharepoint"}, or \code{NULL}. Defaults to \code{NULL}.}

\item{image_size}{\code{str} Standard sizes of the map outputs. Options are:
\itemize{
\item \code{"full_slide"}
\item \code{"soco_slide"}
\item \code{"half_slide"}
}

Defaults to \code{NULL}.}

\item{height}{\code{numeric} Height of the map. Defaults to \code{6.2}.}

\item{width}{\code{numeric} Width of the map. Defaults to \code{4.5}.}

\item{scale}{\code{numeric} Scale of the map. Defaults to \code{1.25}.}

\item{dpi}{\code{numeric} DPI of the map. Defaults to \code{300}.}
}
\value{
\code{ggplot} A map of outbreaks.
}
\description{
Creates a map of recent emergences. The default will display outbreaks from the past 13 months.
}
\examples{
\dontrun{
raw.data <- get_all_polio_data()
p1 <- generate_adhoc_map(raw.data, "algeria")
# Put colors in emergences that don't have a mapped color
emg_cols <- set_emergence_colors(raw.data, c("nigeria", "chad"))
emg_cols["NIE-BOS-1"] <- "yellow"
emg_cols["NIE-YBS-1"] <- "green"
p2 <- generate_adhoc_map(raw.data, c("nigeria", "chad"), emg_cols = emg_cols)
}
}
