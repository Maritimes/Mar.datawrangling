#' @title save_catches
#' @description This function takes all of the data you've filtered, and rolls it up into a csv and/
#' or a shapefile for continued analysis
#' @param df  This is a dataframe that you want to save in some other format.  If a spatial format is selected
#' (e.g. shapefile), it must have LATITUDE and LONGITUDE fields
#' @param filename default is \code{NULL}.  This will be the prefix of your filename
#' @param df.crs This is the CRS value for your dataframe.  This should be the reference system that your data is known to be in.
#' The default value \code{"+init=epsg:4326"} is WGS84 and is appropriate for most data collected using a GPS.
#' @param db default is \code{NULL}. This identifies the dataset you are working
#' with.
#' @param req.coords default is TRUE.  This filters out records without values for LATITUDE or
#' LONGITUDE.  The function aborts if req.coords=TRUE and no records remain.
#' @param keep_nullsets default is FALSE.  In many cases, in addition to the
#' catch, it is also useful to know where fishing occurred that did not result
#' in catches.  this is especially true for most survey-type data.
#' Note that for industry fishing (i.e. ISDB and MARFIS), the null sets will
#' actually just consist of all sets that met your filter criteria.
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude values (in decimal degrees)
#' @param formats This is a vector of the formats in which you would like to save the current data
#' @importFrom rgdal writeOGR
#' @importFrom sp SpatialPoints
#' @importFrom utils write.csv
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
save_catches <- function(db = NULL, df= NULL, filename = NULL, df.crs = "+init=epsg:4326",
                         req.coords=TRUE, lat.field = "LATITUDE",
                         lon.field = "LONGITUDE",
                         keep_nullsets = FALSE,
                         formats = c('csv', 'shp')){
  .Deprecated("save_data")
  save_data(db = db, df=df, 
            filename=filename, 
            df.crs=df.crs, 
            req.coords = req.coords, 
            lat.field=lat.field, 
            lon.field,
            keep_nullsets=keep_nullsets, 
            formats=formats
            )
}
