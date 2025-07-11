% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kpi.figure.functions.R
\name{generate_kpi_tile}
\alias{generate_kpi_tile}
\title{Generate tile plots for indicators}
\usage{
generate_kpi_tile(
  c_table,
  priority_category = "HIGH",
  output_path = Sys.getenv("KPI_FIGURES")
)
}
\arguments{
\item{c_table}{\code{tibble} Either C1, C2, C3, C4}

\item{priority_category}{\code{str} A string or a list of priority category. Valid values
are: "LOW", "LOW (WATCHLIST)", "MEDIUM", "HIGH".}

\item{output_path}{\code{str} Where to output the figure to.}
}
\value{
\code{ggplot2} A tile plot for each indicator for each geography
}
\description{
Generates tile plots for indicators present in c1-c4, showing changes over
multiple rolling periods.
}
\details{
The function automatically detects the geographic scale to present in each
plot. If passing a table that is grouped lower than the country level, we recommend
that only one country is present or a subset of districts are presented so that
the output plot is legible.
}
\examples{
\dontrun{
raw_data <- get_all_polio_data()
c1 <- generate_c1_table(raw_data, "2021-01-01", "2023-12-31")
generate_kpi_tile(c1)
}
}
