% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kpi.table.functions.R
\name{generate_pos_timeliness}
\alias{generate_pos_timeliness}
\title{Generate timeliness columns}
\usage{
generate_pos_timeliness(
  raw_data,
  start_date,
  end_date,
  risk_table = NULL,
  lab_locs = NULL
)
}
\arguments{
\item{raw_data}{\code{list} Global polio surveillance dataset.
Output of \code{\link[=get_all_polio_data]{get_all_polio_data()}}.}

\item{start_date}{\code{str} Start date of the analysis in YYYY-MM-DD format.}

\item{end_date}{\code{str} End date of the analysis in YYYY-MM-DD format.}

\item{risk_table}{\code{tibble} Priority level of each country. Defaults to \code{NULL},
which will download the information directly from EDAV.}

\item{lab_locs}{\code{tibble} Summary of the sequencing capacities of labs.
Output of \code{\link[=get_lab_locs]{get_lab_locs()}}. Defaults to \code{NULL}, which will download the information
directly from EDAV.
.}
}
\value{
\code{tibble} Columns added
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

The function generates timeliness columns in the positives dataset.
}
\details{
This function is used in \code{\link[=generate_c1_table]{generate_c1_table()}} and \code{\link[=generate_timely_det_violin]{generate_timely_det_violin()}}.
}
\keyword{internal}
