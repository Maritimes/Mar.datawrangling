% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_catches.r
\name{summarize_catches}
\alias{summarize_catches}
\title{summarize_catches}
\usage{
summarize_catches(
  db = NULL,
  morph_dets = FALSE,
  gear_dets = FALSE,
  valid.coords = FALSE,
  quiet = FALSE,
  drop.na.cols = TRUE,
  debug = FALSE,
  env = .GlobalEnv
)
}
\arguments{
\item{db}{default is \code{NULL}. This identifies the dataset you are working 
with.}

\item{morph_dets}{default is FALSE.  For some databases, more detailed
records can be added for each catch (e.g. gsdet for 'rv' data).  This flag
indicates that the detailed records will be returned.  This means that every
catch location might have many records (e.g. length and weight data for all
measured fish in a tow). This generally only desired when individual fish
properties are needed (e.g. length, weight)}

\item{gear_dets}{default is FALSE.  For some databases (eg isdb), more
detailed records can be added for each gear.  This means that a single catch
will be duplicated multiple times - once for each new gear parameter (e.g.
mesh shape, door height, etc).  If there's a good reason to show this with
catch data, I can't think of it.}

\item{valid.coords}{default is FALSE.  This uses 
\code{Mar.utils::df.qc.spatial()} to remove coordinates that are not valid.  
If TRUE and no valid records remain, this function will abort.}

\item{quiet}{default is \code{FALSE}.  If coordinates are required
(\code{valid.coords=TRUE}), the script default is to report issues to the
user.  If \code{quiet=TRUE}, these notifications are not shown.}

\item{drop.na.cols}{default is \code{TRUE}.  If TRUE, any columns that are
do not have any values in them will be dropped.}

\item{debug}{default is \code{FALSE}.  If TRUE, messages describing the 
merging steps will be printed out}

\item{env}{This the the environment you want this function to work in.  The 
default value is \code{.GlobalEnv}.}
}
\value{
a data frame, and if \code{req.coords=TRUE}, a message indicating how
many positions were lost due to the constraint.
}
\description{
This function joins together all the rdata files into a single
data frame.
}
\seealso{
Other dfo_extractions: 
\code{\link{get_data}()},
\code{\link{get_survey}()},
\code{\link{self_filter}()}
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
\concept{dfo_extractions}
