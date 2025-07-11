% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kpi.table.functions.R
\name{add_risk_category}
\alias{add_risk_category}
\title{Add the risk category based on the country}
\usage{
add_risk_category(df, risk_table = NULL, ctry_col = "ctry")
}
\arguments{
\item{df}{\code{tibble} Dataframe with at least a column for country}

\item{risk_table}{\code{tibble} Risk category table for each country. Defaults to \code{NULL}.
When set to \code{NULL}, attempts to download the risk category table from EDAV.}
}
\value{
\code{tibble} A dataframe with risk category columns added.
}
\description{
Add the risk category based on the country
}
\keyword{internal}
