% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dr.figure.functions.R
\name{generate_ctry_timeliness_graph}
\alias{generate_ctry_timeliness_graph}
\title{Timeliness intervals of samples at the country level}
\usage{
generate_ctry_timeliness_graph(
  int.data,
  output_path = Sys.getenv("DR_FIGURE_PATH"),
  afp.year.lab = lifecycle::deprecated()
)
}
\arguments{
\item{int.data}{\code{tibble} Summary table with timeliness intervals at the country level.}

\item{output_path}{\code{str} Path where to output the figure.}

\item{afp.year.lab}{\code{tibble} \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
Deprecated since it is not used anymore.}
}
\value{
\code{ggplot} Plot of timeliness intervals at the country level.
}
\description{
A stacked horizontal bar graph for timeliness intervals of samples at the country level.
To get the full intervals from field to lab, the lab data needs to be attached. Otherwise,
only the timeliness intervals from the field up to when it was sent to lab will be displayed.
}
\examples{
\dontrun{
# Attaching lab data
lab_path <- "C:/Users/ABC1/Desktop/algeria_lab_data.csv"
ctry.data <- init_dr("algeria", lab_data_path = lab_path)
lab.timeliness.ctry <- generate_lab_timeliness(ctry.data$lab.data, "ctry", start_date, end_date)
int.data.ctry <- generate_int_data(ctry.data, start_date, end_date,
  spatial.scale = "ctry",
  lab.timeliness.ctry
)
generate_ctry_timeliness_graph(int.data.ctry)
}
}
\seealso{
\code{\link[=generate_int_data]{generate_int_data()}}
}
