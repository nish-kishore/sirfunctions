% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dal.R
\name{duplicate_check}
\alias{duplicate_check}
\title{function to assess duplicates in output from get_all_polio_data}
\usage{
duplicate_check(.raw.data = raw.data)
}
\arguments{
\item{.raw.data}{\verb{named list} list of dataframes produced from \code{get_all_polio_data()}}
}
\description{
checks for duplicate records in afp, other surveillance, sia, and virus data
}
