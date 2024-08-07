% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dr.figure.functions.R
\name{generate_prov_timeliness_graph}
\alias{generate_prov_timeliness_graph}
\title{Timeliness interval bar graph at a province level}
\usage{
generate_prov_timeliness_graph(
  int.data,
  afp.prov.year.lab,
  output_path = Sys.getenv("DR_FIGURE_PATH")
)
}
\arguments{
\item{int.data}{summary table of median timeliness intervals at a province level}

\item{afp.prov.year.lab}{labels for AFP dataset summarized by year}

\item{output_path}{where to save the figure}
}
\value{
ggplot object
}
\description{
Timeliness interval bar graph at a province level
}
