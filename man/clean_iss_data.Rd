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
\item{ctry.data}{\code{list} The output of \code{\link[=init_dr]{init_dr()}}, with ISS data attached.}

\item{start_date}{\code{str} Start date of desk review.}

\item{end_date}{\code{str} End date of desk review.}

\item{priority_col}{\code{str} Column representing priority level.}

\item{start_time_col}{\code{str} Column representing start time.}

\item{unreported_cases_col}{\code{str} Column representing unreported cases.}

\item{prov_col}{\code{str} Column representing province.}

\item{dist_col}{\code{str} Column representing district.}

\item{hf_col}{\code{str} Column representing the health facility name.}

\item{today_col}{\code{str} Column representing when info was recorded.}

\item{date_of_visit_col}{\code{str} Column representing date of visit.}
}
\value{
\code{tibble} Cleaned eSurv/ISS data.
}
\description{
ISS/eSURV data often needs to be cleaned and standardized before analysis. Because
these datasets may vary from country to country, reviewing the data first and its columns
is the first step. In general, there are eight required columns. These are the parameters
with a suffix \verb{_col}. Modify the passed arguments as necessary so the function can
successfully run. Priority levels are set to automatically detect high, medium, low, and not a focal site.
Ensure that priority level column categories have these specification:
\itemize{
\item \code{High}: begins with "h".
\item \code{Medium}: begins with "m".
\item \code{Low}: begins with "l".
\item \verb{Not Focal Site}: begins with "n" or "x".
}
}
\examples{
\dontrun{
iss_path <- "C:/Users/ABC1/Desktop/iss_data.csv"
ctry.data <- init_dr("somalia", iss_data_path = iss_path)
ctry.data$iss.data <- clean_iss_data(ctry.data)
}

}
