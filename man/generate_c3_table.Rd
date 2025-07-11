% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kpi.table.functions.R
\name{generate_c3_table}
\alias{generate_c3_table}
\title{Environmental surveillance KPIs by site}
\usage{
generate_c3_table(
  es_data,
  start_date,
  end_date,
  risk_category = NULL,
  lab_locs = NULL,
  risk_table = NULL
)
}
\arguments{
\item{es_data}{\code{tibble} Environmental surveillance data.}

\item{start_date}{\code{str} Start date of the analysis in YYYY-MM-DD format.}

\item{end_date}{\code{str} End date of the analysis in YYYY-MM-DD format.}

\item{risk_category}{\code{str} Risk category or a list of categories.
Defaults to \code{NULL}. Valid values are: \verb{"LOW, LOW (WATCHLIST), MEDIUM, HIGH}.}

\item{lab_locs}{\code{tibble} Summary of the sequencing capacities of labs.
Output of \code{\link[=get_lab_locs]{get_lab_locs()}}. Defaults to \code{NULL}, which will download the information
directly from EDAV.
.}

\item{risk_table}{\code{tibble} Priority level of each country. Defaults to \code{NULL},
which will download the information directly from EDAV.}
}
\value{
\code{tibble} A summary table of environmental surveillance KPIs.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Environmental surveillance KPIs summarized by site.
}
\examples{
\dontrun{
raw_data <- get_all_polio_data(attach.spatial.data = FALSE)
c3 <- generate_c3_table(raw_data$es, "2021-01-01", "2023-12-31")
}
}
