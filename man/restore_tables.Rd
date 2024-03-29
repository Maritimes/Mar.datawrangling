% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/restore_tables.R
\name{restore_tables}
\alias{restore_tables}
\title{restore_tables}
\usage{
restore_tables(db = NULL, clean = "TRUE")
}
\arguments{
\item{db}{default is \code{NULL}. This identifies the dataset you are working 
with.}

\item{clean}{default is \code{TRUE}. If this is true, the temporary environment that held the 
tables will be removed when the tables are restored.  If it is false, the environment will not be 
removed when the data is loaded.}
}
\value{
nothing
}
\description{
This function retrieves all of the tables stored by save_tables() and restores them
to the global environment.  Existing tables with the same names will be overwritten.
}
\seealso{
Other general_use: 
\code{\link{cleanup}()},
\code{\link{get_isdb_trip_set}()},
\code{\link{save_data}()},
\code{\link{save_tables}()}
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
\concept{general_use}
