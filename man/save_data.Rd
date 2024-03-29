% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_data.r
\name{save_data}
\alias{save_data}
\title{save_data}
\usage{
save_data(
  db = NULL,
  df = NULL,
  filename = NULL,
  df.crs = "EPSG:4326",
  req.coords = TRUE,
  lat.field = "LATITUDE",
  lon.field = "LONGITUDE",
  formats = c("csv", "gpkg"),
  env = .GlobalEnv
)
}
\arguments{
\item{db}{default is \code{NULL}. This identifies the dataset you are working
with.}

\item{df}{This is a dataframe that you want to save in some other format.  If a spatial format is selected
(e.g. shapefile), it must have LATITUDE and LONGITUDE fields}

\item{filename}{default is \code{NULL}.  This will be the prefix of your filename}

\item{df.crs}{This is the CRS value for your dataframe.  This should be the reference system that your data is known to be in.
The default value \code{"EPSG:4326"} is WGS84 and is appropriate for most data collected using a GPS.}

\item{req.coords}{default is TRUE.  This filters out records without values for LATITUDE or
LONGITUDE.  The function aborts if req.coords=TRUE and no records remain.}

\item{lat.field}{the default is \code{"LATITUDE"}. the name of the field holding latitude values (in decimal degrees)}

\item{lon.field}{the default is \code{"LONGITUDE"}.  the name of the field holding longitude values (in decimal degrees)}

\item{formats}{This is a vector of the formats in which you would like to save the current data,
including "raw" for a (local) dataframe, "sf" for a (local) simple feature object,  
"gpkg" for an ESRI-compatible geopackage or "csv" (both written to the wd). The raw and sp objects will
just have the name specified by filename, while the csv also gets a timestamp, since it's written 
externally .}

\item{env}{This the the environment you want this function to work in.  The 
default value is \code{.GlobalEnv}.}
}
\description{
This function takes either a dataframe or all of the data you've filtered, and rolls it up into a csv and/
or a shapefile for continued analysis
}
\note{
Note that with the retirement of rgeos,  rgdal and maptools, shapefiles are no longer supported
by this package.  Instead, gpkg file are generated.  Additionally sp objects are also being phased 
out and are being replaced by sf objects.
}
\seealso{
Other general_use: 
\code{\link{cleanup}()},
\code{\link{get_isdb_trip_set}()},
\code{\link{restore_tables}()},
\code{\link{save_tables}()}
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
\concept{general_use}
