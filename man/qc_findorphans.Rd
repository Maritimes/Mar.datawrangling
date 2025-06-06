% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc_findorphans.r
\name{qc_findorphans}
\alias{qc_findorphans}
\title{qc_findorphans}
\usage{
qc_findorphans(
  cxn = NULL,
  db = NULL,
  extract_user = NULL,
  extract_computer = NULL,
  debug = F
)
}
\arguments{
\item{cxn}{A valid Oracle connection object. This parameter allows you to 
pass an existing connection, reducing the need to establish a new connection 
within the function. If provided, it takes precedence over the connection-
related parameters.}

\item{db}{default is \code{NULL}. This identifies the dataset you are working
with.}

\item{extract_user}{default is \code{NULL}.  This parameter can be used with
\code{extract_computer} to load encypted data files extracted by another user 
and/or computer}

\item{extract_computer}{default is \code{NULL}.  This parameter can be used with
\code{extract_user} to load encypted data files extracted by another user 
and/or computer}

\item{debug}{default = \code{FALSE}.  This is passed to self_filter() which 
causes each filter to be printed out.}
}
\description{
This function creates zzz_orph_* tables with records that don't
appear to be linked to the other tables (within Mar.datawrangling)
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
