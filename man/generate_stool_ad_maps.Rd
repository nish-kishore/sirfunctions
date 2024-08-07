% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dr.figure.functions.R
\name{generate_stool_ad_maps}
\alias{generate_stool_ad_maps}
\title{Stool adequacy maps by province}
\usage{
generate_stool_ad_maps(
  ctry.data,
  pstool,
  ctry.shape,
  prov.shape,
  start_date,
  end_date,
  output_path = Sys.getenv("DR_FIGURE_PATH")
)
}
\arguments{
\item{ctry.data}{RDS file for polio data of a country}

\item{pstool}{stool adequacy at province level}

\item{ctry.shape}{recent country shapefile}

\item{prov.shape}{recent province shapefile}

\item{start_date}{start date of desk review}

\item{end_date}{end date of desk review}

\item{output_path}{where to save the figure}
}
\value{
ggplot object
}
\description{
Stool adequacy maps by province
}
