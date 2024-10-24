% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dr.figure.functions.R
\name{generate_ctry_timeliness_graph}
\alias{generate_ctry_timeliness_graph}
\title{Timeliness bar graph at the country level}
\usage{
generate_ctry_timeliness_graph(
  int.data,
  afp.year.lab,
  output_path = Sys.getenv("DR_FIGURE_PATH")
)
}
\arguments{
\item{int.data}{summary table with timeliness intervals at the country level}

\item{afp.year.lab}{summary table of samples sent by year}

\item{output_path}{path where to output the figure}
}
\value{
ggplot object
}
\description{
Timeliness bar graph at the country level
}
