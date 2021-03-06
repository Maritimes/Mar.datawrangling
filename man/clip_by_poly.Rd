% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clip_by_poly.r
\name{clip_by_poly}
\alias{clip_by_poly}
\title{clip_by_poly}
\usage{
clip_by_poly(
  db = NULL,
  df = NULL,
  lat.field = "LATITUDE",
  lon.field = "LONGITUDE",
  clip.poly = NULL,
  buffer.m = NULL,
  return.spatial = FALSE,
  env = .GlobalEnv
)
}
\arguments{
\item{db}{default is \code{NULL}. This identifies the dataset you are working 
with.}

\item{df}{default is \code{NULL}.  This is the dataframe to be clipped.}

\item{lat.field}{the default is \code{"LATITUDE"}. the name of the field holding latitude values 
(in decimal degrees)}

\item{lon.field}{the default is \code{"LONGITUDE"}.  the name of the field holding longitude 
values (in decimal degrees)}

\item{clip.poly}{default is \code{NULL}. This is the full path to a shapefile 
that the data will be clipped by (including the '.shp' extension).}

\item{buffer.m}{default is \code{NULL}. This is the distance in meters to buffer the border of 
\code{clip.poly}}

\item{return.spatial}{default is \code{FALSE}. If this is TRUE, a 
SpatialPointsDataFrame will be returned. Otherwise it will return a df.}

\item{env}{This the the environment you want this function to work in.  The 
default value is \code{.GlobalEnv}.}
}
\value{
spatialPointsDataFrame
}
\description{
This function takes either a db identifier or a dataframe and a polygon, and clips
the data to the extent of the polygon.  The polygon can be buffered as required to select nearby 
data as well.
}
\note{
If the input polygon has no projection assigned, it will be assumed to be in Geographic, 
WGS84. FYI, during buffering, the polygon is briefly converted to UTMZone20, and back again, 
since the use of distances requires projecting the data.
}
\seealso{
Other general_use: 
\code{\link{cleanup}()},
\code{\link{get_isdb_trip_set}()},
\code{\link{restore_tables}()},
\code{\link{save_data}()},
\code{\link{save_tables}()}
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
\concept{general_use}
