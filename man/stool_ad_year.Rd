% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f.stool.ad.01.R
\name{stool_ad_year}
\alias{stool_ad_year}
\title{Helper function to generate stool adequacy on a non-rolling basis}
\usage{
stool_ad_year(stool.data, pop.data, year.data, spatial_scale)
}
\arguments{
\item{stool.data}{AFP data with adequacy final column}

\item{pop.data}{population data}

\item{year.data}{table generated by the generate_year_data() function}

\item{spatial_scale}{"ctry", "prov", or "dist}
}
\value{
summary table of stool adequacy on a yearly basis
}
\description{
Helper function to generate stool adequacy on a non-rolling basis
}
\keyword{internal}
