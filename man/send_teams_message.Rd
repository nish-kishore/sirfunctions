% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/com.R
\name{send_teams_message}
\alias{send_teams_message}
\title{Send a message on Microsoft Teams}
\usage{
send_teams_message(
  msg,
  team_id = "CGH-GID-PEB-SIR",
  channel = "CORE 2.0",
  attach = NULL,
  type = "text"
)
}
\arguments{
\item{msg}{str: message to be sent}

\item{team_id}{str: Team id, defaults to "CGH-GID-PEB-SIR"}

\item{channel}{str: channel where message should be sent}

\item{attach}{str: local path of files to be attached in message}

\item{type}{str: "text" or "html"}
}
\value{
Success or error message
}
\description{
Helper function to send message to validated MS Teams interface
}
