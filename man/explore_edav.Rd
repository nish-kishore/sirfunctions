% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dal.R
\name{explore_edav}
\alias{explore_edav}
\title{Interactive loading of EDAV data}
\usage{
explore_edav(
  path = get_constant("DEFAULT_EDAV_FOLDER"),
  azcontainer = suppressMessages(get_azure_storage_connection())
)
}
\arguments{
\item{path}{\code{str} Path to start at initially.}

\item{azcontainer}{Azure storage container provided by \code{\link[=get_azure_storage_connection]{get_azure_storage_connection()}}.}
}
\value{
\code{tibble} Data from the EDAV environment.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This function is a way to interactively work with files in the EDAV
environment, which is convenient as we don't have to search for files within
Azure Storage Explorer.
}
\details{
There are Excel files that may need additional formatting before it can be
read properly into an R object. For example, skipping columns or rows.
For complicated Excel files, it would be best to directly call \code{\link[=edav_io]{edav_io()}}
in "read" mode, and pass additional parameters via \code{...}. See \code{\link[=edav_io]{edav_io()}}
examples for details.
}
\examples{
\dontrun{
test <- explore_edav()
}
}
