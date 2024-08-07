% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dr.figure.functions.R
\name{generate_inad_tab}
\alias{generate_inad_tab}
\title{Table containing stool adequacy at the country level}
\usage{
generate_inad_tab(ctry.data, stool.data, cstool, start_date, end_date)
}
\arguments{
\item{ctry.data}{RDS file containing polio data for a country}

\item{stool.data}{AFP data with stool adequacy columns}

\item{cstool}{stool adequacy at the country level}

\item{start_date}{start date of desk review}

\item{end_date}{end date of desk review}
}
\value{
a flextable containing stool adequacy at the country level
}
\description{
Table containing stool adequacy at the country level
}
