% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dr.figure.functions.R
\name{generate_es_timely}
\alias{generate_es_timely}
\title{ES timeliness scatterplot}
\usage{
generate_es_timely(
  es.data,
  es_start_date = (lubridate::as_date(es_end_date) - lubridate::years(1)),
  es_end_date = end_date,
  output_path = Sys.getenv("DR_FIGURE_PATH")
)
}
\arguments{
\item{es.data}{\code{tibble} ES data.}

\item{es_start_date}{\code{str} Start date of analysis. By default, this is one year from the end date.}

\item{es_end_date}{\code{str} End date of analysis.}

\item{output_path}{\code{str} Local path for where to save the figure to.}
}
\value{
\code{ggplot} A scatterplot for timeliness of ES samples.
}
\description{
Generates a scatterplot of the time it takes for each environmental samples to arrive in lab.
}
\examples{
\dontrun{
ctry.data <- init_dr("algeria")
generate_es_timely(ctry.data$es)
}

}
