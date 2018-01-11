#' @title clip_by_poly
#' @description This function takes either a db identifier or a dataframe and a polygon, and clips
#' the data to the extent of the polygon.  The polygon can be buffered as required to select nearby 
#' data as well.
#' @param db default is \code{NULL}. This identifies the dataset you are working 
#' with.
#' @param df default is \code{NULL}.  This is the dataframe to be clipped.
# @param filter.wrangled default is \code{TRUE}.  If TRUE, the local object holding the position 
# for this db will be replaced by the filtered version, and all other local objects will be 
# filtered accordingly (i.e. all data will be limited to the same extent).
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values 
#' (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude 
#' values (in decimal degrees)
#' @param clip.poly default is \code{NULL}. This is the full path to a shapefile 
#' that the data will be clipped by (including the '.shp' extension).
#' @param buffer.m default is \code{NULL}. This is the distance in meters to buffer the border of 
#' \code{clip.poly}
#' @param return.spatial default is \code{FALSE}. If this is TRUE, a 
#' SpatialPointsDataFrame will be returned. Otherwise it will return a df.
#' @importFrom rgeos gBuffer
#' @importFrom sp CRS
#' @importFrom sp proj4string
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp spTransform
#' @importFrom rgdal readOGR
#' @importFrom Mar.utils df_qc_spatial
#' @return spatialPointsDataFrame
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' @note If the input polygon has no projection assigned, it will be assumed to be in Geographic, 
#' WGS84. FYI, during buffering, the polygon is briefly converted to UTMZone20, and back again, 
#' since the use of distances requires projecting the data.
clip_by_poly <- function(db = NULL, df=NULL,
                         #filter.wrangled = TRUE,
                         lat.field = "LATITUDE", 
                         lon.field = "LONGITUDE", 
                         clip.poly = NULL,
                         buffer.m = NULL,
                         return.spatial = FALSE){
  if (!is.null(db)) {
    df = get(ds_all[[.GlobalEnv$db]]$table_pos)
  } 
  df=Mar.utils::df_qc_spatial(df)
  df.sp = SpatialPointsDataFrame(
    coords = df[, c(lon.field, lat.field)],
    data = df,
    proj4string = CRS('+init=epsg:4326')
  )
  
  if (class(clip.poly)=="character"){
    #extract the full path and name of the shapefile 
    ogrPath = dirname(clip.poly)
    ogrLayer = sub('\\.shp$', '', basename(clip.poly))
    clip.poly_this <- readOGR(dsn = ogrPath, layer = ogrLayer, verbose = FALSE)
  }else if(class(clip.poly)=="SpatialPolygonsDataFrame"){
    clip.poly_this = clip.poly
  }

  if (is.na(proj4string(clip.poly_this))) {
    cat('\nNo projection found for input shapefile - assuming geographic.')
    proj4string(clip.poly_this) = CRS("+init=epsg:4326")
  } else if (proj4string(clip.poly_this)!="+init=epsg:4326") {
    clip.poly_this = spTransform(clip.poly_this, CRS('+init=epsg:4326'))
  }
  
  if (!is.null(buffer.m)){
    #if a buffer is specified, convert poly to UTM20N, apply buffer, and convert back
    clip.poly_this = spTransform(clip.poly_this, CRS("+init=epsg:2220"))
    clip.poly_this = gBuffer(clip.poly_this, width=buffer.m)
    clip.poly_this = spTransform(clip.poly_this, CRS('+init=epsg:4326'))
  }
  if (NROW(df.sp[clip.poly_this, ]) ==0) {
    stop("\nNo data lies inside this polygon, aborting clip.\n\n")
  }
  df.sp_subset <- df.sp[clip.poly_this, ] 
  
  if (!is.null(db)){
    assign(ds_all[[.GlobalEnv$db]]$table_pos, df.sp_subset@data, envir = .GlobalEnv)
  } 
  if (!return.spatial){
    df.sp_subset = df.sp_subset@data
  }

  return(df.sp_subset)

}