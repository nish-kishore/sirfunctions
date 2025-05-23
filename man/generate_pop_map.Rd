% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dr.figure.functions.R
\name{generate_pop_map}
\alias{generate_pop_map}
\title{Country map with province populations}
\usage{
generate_pop_map(
  ctry.data,
  ctry.shape,
  prov.shape,
  end_date,
  output_path = Sys.getenv("DR_FIGURE_PATH"),
  caption_size = 11
)
}
\arguments{
\item{ctry.data}{\code{list} Large list containing country polio data. This is the
output of \code{\link[=extract_country_data]{extract_country_data()}} or \code{\link[=init_dr]{init_dr()}}.}

\item{ctry.shape}{\code{sf} Country shape file in long format.}

\item{prov.shape}{\code{sf} Province shape file in long format.}

\item{end_date}{\code{str} End date of the analysis.}

\item{output_path}{\code{str} Local path where to save the figure.}

\item{caption_size}{\code{numeric} Size of the caption. Default is \code{11}.}
}
\value{
\code{ggplot} A map of U15 province populations and population centers.
}
\description{
The map displays the U15 population for each province for a country.
}
\examples{
\dontrun{
ctry.data <- init_dr("algeria")
ctry.shape <- load_clean_ctry_sp(ctry_name = "ALGERIA", type = "long")
prov.shape <- load_clean_prov_sp(ctry_name = "ALGERIA", type = "long")
generate_pop_map(ctry.data, ctry.shape, prov.shape, "2023-12-31")
}
}
\seealso{
\code{\link[=load_clean_ctry_sp]{load_clean_ctry_sp()}}, \code{\link[=load_clean_prov_sp]{load_clean_prov_sp()}}
}
