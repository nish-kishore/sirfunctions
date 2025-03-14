% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dr.iss.functions.R
\name{load_iss_data}
\alias{load_iss_data}
\title{Read ISS/eSURV data}
\usage{
load_iss_data(iss_path, sheet_name = NULL)
}
\arguments{
\item{iss_path}{\code{str} Path to the excel or csv file.}

\item{sheet_name}{\code{str} Optional name of the ISS data. This is mainly used if
the path is to an Excel file and that Excel file has multiple tabs.}
}
\value{
\code{tibble} ISS/eSURV data loaded into a tibble.
}
\description{
The function is written to assist in load the ISS data from a path specified
by the user during \code{\link[=init_dr]{init_dr()}}. This function is not meant to be exported.
}
\examples{
\dontrun{
iss_path <- "C:/Users/ABC1/Desktop/iss_data.csv"
iss_data <- load_iss_data(iss_path)
}
}
