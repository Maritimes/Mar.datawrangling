#' @title save_data
#' @description This function takes either a dataframe or all of the data you've filtered, and rolls it up into a csv and/
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
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude values (in decimal degrees)
#' @param formats This is a vector of the formats in which you would like to save the current data,
#' including "raw" for a (local) dataframe, "sp" for a (local) SpatialPointsDataFrame,  
#' "shp" for a shapefile or "csv" (both written to the wd). The raw and sp objects will
#' just have the name specified by filename, while the csv and shapefiles, since they're
#' written externally also get a timestamp.  
#' @param env This the the environment you want this function to work in.  The 
#' default value is \code{.GlobalEnv}.
#' @importFrom rgdal writeOGR
#' @importFrom sp SpatialPoints
#' @importFrom utils write.csv
#' @importFrom Mar.utils df_qc_spatial
#' @importFrom Mar.utils prepare_shape_fields
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
save_data <- function(db = NULL, df= NULL, filename = NULL, df.crs = "+init=epsg:4326",
                      req.coords=TRUE, 
                      lat.field = "LATITUDE",
                      lon.field = "LONGITUDE",
                      formats = c('csv', 'shp'),
                      env=.GlobalEnv){
  if (req.coords == FALSE & 'shp' %in% formats) warning("\nSince req.coords = FALSE, not all of the
records necessarily have positions and will not be visible in your shapefile\n")
  if (is.null(df)) {
    df = summarize_catches(db=ds_all[[.GlobalEnv$db]]$db, valid.coords = req.coords, env=env, drop.na.cols = F)
    
    if (is.null(df)){
      cat("\nNo records to save")
      return(NULL)
    }
  }
  if (is.null(filename)) {
    name = match.call()[2]
  }else {
    name = filename
  }
  name = gsub('()','',name)
  name = gsub('\\.','',name)
  ts = format(Sys.time(), "%Y%m%d_%H%M")
  fn = paste(name,"_",ts,sep="" )
  #id posix and date fields
  df=data.frame(lapply(df, function(x) if(inherits(x, "POSIXct")|inherits(x, "Date")) as.Date(strftime(x, format="%Y-%m-%d")) else x))
  if ('raw' %in% formats) assign(paste0("raw_",name), df, envir = env)
  if ('shp' %in% formats | 'sp' %in% formats){
    df.sp = Mar.utils::df_qc_spatial(df, lat.field, lon.field)
    df.sp = sp::SpatialPointsDataFrame(
      coords = df.sp[, c(lon.field, lat.field)],
      data = df.sp,
      proj4string = sp::CRS(df.crs)
    )
    if (nrow(df.sp@data) != nrow(df)) {
      cat("\n",paste0(nrow(df)-nrow(df.sp@data), " records were lost due to invalid coordinates"))
    }
    if ('sp' %in% formats) assign(paste0("sp_",name), df.sp, envir = env)
    if ('shp' %in% formats){
      df.sp = Mar.utils::prepare_shape_fields(df.sp)
      rgdal::writeOGR(df.sp, ".", fn, driver="ESRI Shapefile", overwrite_layer=TRUE)
      cat(paste0("\nWrote file to: ",file.path(getwd(),fn)))
    }
  }
  if ('csv' %in% formats){
    write.csv(df, paste0(fn,".csv"))
  }
  if ('rds' %in% formats){
    saveRDS(df, paste0(fn,".rds"))
  }
  return(invisible(df))
}
