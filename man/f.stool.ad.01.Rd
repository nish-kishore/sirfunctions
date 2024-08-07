% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f.stool.ad.01.R
\name{f.stool.ad.01}
\alias{f.stool.ad.01}
\title{Calculate percent stool adequacy on a case basis}
\usage{
f.stool.ad.01(
  afp.data,
  admin.data,
  start.date,
  end.date,
  spatial.scale,
  missing = "good",
  bad.data = "inadequate",
  rolling = F,
  sp_continuity_validation = T
)
}
\arguments{
\item{afp.data}{tibble: AFP data which includes GUID at a given spatial scale
formatted as "adm(0,1,2)guid, onset date as "date" and cdc.classification.all2
which includes "NOT-AFP"}

\item{admin.data}{tibble: Full list of country administrative units by a given
spatial scale including "year", "adm(0,1,2)guid, and "(ctry/prov/dist)"
as appropriate}

\item{start.date}{chr: "YYYY-MM-DD" - starting date for analysis}

\item{end.date}{chr: "YYYY-MM-DD" - ending date for analysis}

\item{spatial.scale}{chr: "prov" or "dist" or "ctry"}

\item{missing}{chr: "good" or "bad" or "missing"; default "good"}

\item{bad.data}{chr: "remove" or "inadequate" or "adequate"; default "inadequate"}

\item{rolling}{boolean: Should data be annualized or calculated within the time period allotted}

\item{sp_continuity_validation}{boolean: Should data excluded temporaliry inconsistent GUIDs? Default T.}
}
\value{
tibble: long format stool adequacy evaluations
}
\description{
creates an adequacy variable - 'missing' parameter defines how
missing data is treated - "Good" classifies missing data as good quality
(POLIS method)
"Bad" classifies all missing as bad quality, "missing" excludes missing from
the calculations
}
