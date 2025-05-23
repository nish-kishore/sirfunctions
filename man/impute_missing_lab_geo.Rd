% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dr.lab.functions.R
\name{impute_missing_lab_geo}
\alias{impute_missing_lab_geo}
\title{Impute missing geographic information from the AFP linelist}
\usage{
impute_missing_lab_geo(lab_data, afp_data = NULL)
}
\arguments{
\item{lab_data}{\code{tibble} Lab data to clean.}

\item{afp_data}{\code{tibble} AFP data.}
}
\value{
\code{tibble} Lab data set with imputed geographic columns based on the
AFP table.
}
\description{
Impute missing geographic information from the AFP linelist
}
\keyword{internal}
