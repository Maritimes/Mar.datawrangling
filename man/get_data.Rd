% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data.r
\name{get_data}
\alias{get_data}
\title{get_data}
\usage{
get_data(
  db = NULL,
  cxn = NULL,
  usepkg = "rodbc",
  force.extract = FALSE,
  fn.oracle.username = "_none_",
  fn.oracle.password = "_none_",
  fn.oracle.dsn = "_none_",
  env = .GlobalEnv,
  quiet = FALSE
)
}
\arguments{
\item{db}{default is \code{NULL}. This identifies the dataset you are working
with. Valid values include the following (assuming you have Oracle access)
\itemize{
\item \code{rv} - Bottom Trawl Surveys conducted in shore waters off south west Nova Scotia
\item \code{rvp70} - Bottom Trawl Surveys conducted in shore waters off south west Nova Scotia conducted prior to 1970
\item \code{isdb} - DFO at-sea fish catch observations from commercial fishing vessels operating in the North West Atlantic.
\item \code{chid} - Exploratory fishing surveys of the benthic fish fauna at 900-1800m.
\item \code{redfish} - A survey using stratified random design with day/night replication targeting deep sea redfish
\item \code{marfis} - A Policy and Economics Branch database that houses information on the fisheries of the Scotia-Fundy region, including data related to catch and effort.
\item \code{comland67} - Commercial Landings (1967-1985) - like MARFIS, but earlier.
\item \code{comland86} - Commercial Landings (1986-2001) - like MARFIS, but earlier. This is separated from the 1967 - 1985 COMLAND data due to differences in the code tables.
\item \code{stomach} -     Stomach Contents Database
\item \code{asef} - Tagged salmon records at fishways
\item \code{meso} - Mesopelagic Database
\item \code{meso_gully} - Mesopelagic Gully Database
\item \code{juvesh} - Juvenile Silver Hake Database
}}

\item{cxn}{A valid Oracle connection object. This parameter allows you to 
pass an existing connection, reducing the need to establish a new connection 
within the function. If provided, it takes precedence over the connection-
related parameters.}

\item{usepkg}{default is \code{'rodbc'}. This indicates whether the connection to Oracle should
use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is slightly easier to setup, but
roracle will extract data ~ 5x faster.Deprecated; use \code{cxn} instead.}

\item{force.extract}{The default value is FALSE.  By default, existing data will be loaded.  If
\code{force.extract ==TRUE}, than a full extraction will take place, overwriting any existing
data.}

\item{fn.oracle.username}{Default is \code{'_none_'}. This is your username 
for accessing Oracle objects. If you have a value for \code{oracle.username} 
stored in your environment (e.g., from an rprofile file), this can be left 
out and that value will be used. If a value for this is provided, it will 
take priority over your existing value. Deprecated; use \code{cxn} instead.}

\item{fn.oracle.password}{Default is \code{'_none_'}. This is your password 
for accessing Oracle objects. If you have a value for \code{oracle.password} 
stored in your environment (e.g., from an rprofile file), this can be left 
out and that value will be used. If a value for this is provided, it will 
take priority over your existing value. Deprecated; use \code{cxn} instead.}

\item{fn.oracle.dsn}{Default is \code{'_none_'}. This is your DSN/ODBC 
identifier for accessing Oracle objects. If you have a value 
for \code{oracle.dsn} stored in your environment (e.g., from an rprofile 
file), this can be left out and that value will be used. If a value for this 
is provided, it will take priority over your existing value. Deprecated; use 
\code{cxn} instead.}

\item{env}{This the the environment you want this function to work in.  The 
default value is \code{.GlobalEnv}.}

\item{quiet}{default is \code{FALSE}.  If True, no text describing progress
will be shown.}
}
\description{
This function contains the logic for deciding whether to extract or load data.  It
is aware of the required tables, and if all are present,it will load them into the
global environment.  If some are missing, it will offer to re-extract them.  If the user chooses
to re-extract, it will get their oracle credentials and verify that the user has access to all of
the required tables prior to attempting an extraction.
}
\seealso{
Other dfo_extractions: 
\code{\link{get_data_custom}()},
\code{\link{get_survey}()},
\code{\link{self_filter}()},
\code{\link{summarize_catches}()}
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
\concept{dfo_extractions}
