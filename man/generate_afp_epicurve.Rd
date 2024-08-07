% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dr.figure.functions.R
\name{generate_afp_epicurve}
\alias{generate_afp_epicurve}
\title{Generate epicurve of AFP cases by year}
\usage{
generate_afp_epicurve(
  ctry.data,
  start_date,
  end_date = lubridate::today(),
  output_path = Sys.getenv("DR_FIGURE_PATH")
)
}
\arguments{
\item{ctry.data}{RDS data countaining polio data for a country}

\item{start_date}{start date of the desk review}

\item{end_date}{end date of the desk review}

\item{output_path}{where to save the figure}
}
\value{
ggplot object
}
\description{
Generate epicurve of AFP cases by year
}
