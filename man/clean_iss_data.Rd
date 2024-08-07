% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dr.iss.functions.R
\name{clean_iss_data}
\alias{clean_iss_data}
\title{Perform common cleaning tasks for ISS/eSURV data}
\usage{
clean_iss_data(
  ctry.data,
  start_date,
  end_date,
  priority_col = "priority_level",
  start_time_col = "starttime",
  unreported_cases_col = "num_unreportedcases",
  prov_col = "states",
  dist_col = "districts",
  hf_col = "name_of_facility_visited",
  today_col = "today",
  date_of_visit_col = "date_of_visit"
)
}
\arguments{
\item{ctry.data}{ctry.data containing iss.data}

\item{start_date}{start date of desk review}

\item{end_date}{end date of desk review}

\item{priority_col}{column representing priority level as a string}

\item{start_time_col}{column representing start time as a string}

\item{unreported_cases_col}{column representing unreported cases as a string}

\item{prov_col}{column representing province as a string}

\item{dist_col}{column representing district as a string}

\item{hf_col}{column representing the health facility name as a string}

\item{today_col}{column representing when info was recorded}

\item{date_of_visit_col}{column representing date of visit}
}
\value{
a tibble of cleaned ISS data
}
\description{
Perform common cleaning tasks for ISS/eSURV data
}
