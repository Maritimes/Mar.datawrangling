#' @title df_qc_spatial
#' @description This function identifies points that aren't in the northern 
#' hemisphere (i.e. LAT between 0 and 90) and aren't in the western hemisphere 
#' (i.e. LON between 0 and 180).  By default, it returns the "good" points, but 
#' the "return.bad" flag allows it to return only the "bad" points. 
#' @param df the dataframe to be cleaned
#' @param lat.field default is \code{LATITUDE}.  The field in the dataframe holding the latitudes
#' @param lon.field default is \code{LONGITUDE}.  The field in the dataframe holding the longitudes
#' @param return.bad the default is \code{FALSE}.  Normally, only the plottable points are returned.
#' If TRUE, only the rejected points are returned.
#' @return data.frame contents depend of the return.bad parameter
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
df_qc_spatial <-function(df, lat.field = "LATITUDE", lon.field = "LONGITUDE", return.bad=FALSE){
  pass = df[!is.na(df[lat.field]) & !is.na(df[lon.field]),]
  pass = pass[pass[lat.field] > 0 & pass[lat.field] < 90,] #northern hemisphere only
  pass = pass[pass[lon.field] > -180 & pass[lon.field] < 0,] #western hemisphere only
  if (return.bad){
    fail = rbind(df, pass)
    fail = fail[!duplicated(fail,fromLast = FALSE)&!duplicated(fail,fromLast = TRUE),]
    return(fail)
  }else{
    return(pass)
  }
}