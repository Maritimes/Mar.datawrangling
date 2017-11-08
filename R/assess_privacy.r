#' @title assess_privacy
#' @description At this time, data with privacy considerations must be aggregated such that each 
#' polygon has a minimum of 5 unique values for sensitive fields like Licenses, License Holders, and 
#' Vessels.  This function takes a dataframe and shapefile and for each polygon in the 
#' shapefile calculates 1) aggregate values for a number of (user-specified) fields , and 2) 
#' how many unique values exist in each polygon for each of a number of sensitive fields. A 
#' shapefile is generated with all of the data, as well as a field indicating whether or not the 
#' data can be displayed.
#' @param df a dataframe to be analyzed. If left \code{NULL}, a value for 
#' \code{db} should be provided
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values 
#' (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude 
#' values (in decimal degrees)
#' @param rule.of default is \code{5} Whether or not data can be shown (even 
#' aggregated) depends on the presence of a threshold number of unique values 
#' for certain sensitive fields.  This parameter sets that threshold.
#' @param agg.fields the default is \code{NULL}, but if a db is provided, they 
#' will be guessed.  These are the fields on which to aggregate (i.e. generate 
#' values of \code{MEAN}, \code{COUNT} and \code{SUM}).
#' @param sens.fields the defaults are \code{NULL}  These are fields
#' to which the "rule of 5" should be applied. The Treasury Secretariat states that when data is 
#' shown to the public, certain fields must have at least 5 unique values for these fields 
#' aggregated together. When run, this function will look at these fields, and calculate how many 
#' unique values exist for each.  It will then populate a field 'TOTUNIQUE' with the minimum number 
#' of unique values of all the assessed fields. If this is 5 or more, a field called 'CAN_SHOW' will 
#' be marked as 'YES' (otherwise it will be 'NO').
#' @param show.plot default is \code{TRUE} This indicates whether or not a plot 
#' of your aggregated data should be shown.
#' @param save.plot default is \code{FALSE} This indicates whether or not you 
#' would like a png file saved of your data. If TRUE - a plot will be saved 
#' INSTEAD of being shown
#' @param fun the default is \code{mean}.  This is the aggregated data that 
#' will be used to symnbolize your plotted data.  Options include "mean", "sum" 
#' and "length" (which is a count)
#' @param create.shps default is \code{FALSE}.  This indicates whether or not 
#' shapefiles should be created for 1) the polygon file (with aggregated values 
#' for each polygon and an indication of whether or not each polygon meets the 
#' privacy constraints), and 2) the 2 min gridded data (only for within those 
#' polygons that meet the privacy constraints).
#' @param file_id default is \code{NULL} Whatever is entered here will be used 
#' to name the output shapefiles and/or plots.  If nothing is enetered, the 
#' output files will just be named using timestamps. 
#' @param agg.poly.shp default is \code{NULL}.  This is the shapefile that has 
#' polygons that should be checked for sufficient unique values of the 
#' sens.fields.  If NULL, NAFO zones will be used.  Otherwise, a path to any 
#' polygon shapefile can be provided. 
#' @param agg.poly.field default is \code{NULL}.  This identifies the field within 
#' the shapefile provided to agg.poly.shp that should be used to check for 
#' sufficient unique values of the sens.fields.
#' @param create.shps default is \code{FALSE}.   This controls whether or not to
#' create a shapefile of the NAFO zones showing the aggregated data (and whether or not 
#' data within each zone can be shown at all.
#' @param nclasses default is \code{4} This is how many discrete classes you 
#' want to use to categorize data on the output plot.
#' @return a SpatialPolygonsDataFrame, and generates a shapefile
#' @family aggregation
#' @importFrom stats aggregate
#' @importFrom rgdal readOGR
#' @importFrom rgdal writeOGR
#' @importFrom sp CRS
#' @importFrom sp coordinates
#' @importFrom sp proj4string
#' @importFrom sp over
#' @importFrom classInt classIntervals
#' @importFrom RColorBrewer brewer.pal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note If sensitive fields have names that are 
#' different than what is provided in the \code{sen.fields}, they will not be detected, or 
#' included in the checks.  Please make very sure you correctly identify such fields.
#' @export
assess_privacy <- function(
  df= NULL, 
  lat.field = 'LATITUDE',
  lon.field = 'LONGITUDE',
  rule.of = 5,
  agg.fields = NULL,
  sens.fields = NULL,
  nclasses = 4,
  show.plot = TRUE,
  save.plot = FALSE,
  fun = "mean",
  create.shps = FALSE,
  file_id = NULL,
  agg.poly.shp = NULL,
  agg.poly.field = NULL
){
  ts = format(Sys.time(), "%Y%m%d_%H%M")
  sel.fun = eval(parse(text=fun))
  plotcol = switch(fun, "mean" =3,
                         "length" =4,
                         "sum" =5)
  
  df = df_qc_spatial(df, lat.field, lon.field, FALSE)
  sp::coordinates(df) = c(lon.field, lat.field)
  sp::proj4string(df) = sp::CRS("+proj=longlat +datum=WGS84")

  if (is.null(agg.poly.shp)){
    agg.poly= NAFOSubunits
    if (is.null(agg.poly.field)){
      agg.poly.field = 'NAFO_BEST'
    }
  }else{
    agg.poly <- rgdal::readOGR(dsn = agg.poly.shp, verbose = FALSE)
    if (is.na(proj4string(agg.poly))) {
      cat('\nNo projection found for input shapefile - assuming geographic.')
      sp::proj4string(agg.poly) = sp::CRS("+proj=longlat +datum=WGS84")
    }
      #convert the shape to geographic
      agg.poly <- sp::spTransform(agg.poly,sp::CRS("+proj=longlat +datum=WGS84"))
  }
  
  
  df@data = cbind(df@data,sp::over( df, agg.poly , fn = NULL))
  
  for (i in 1:length(agg.fields)) {
    if (nrow(df@data[is.na(df@data[, agg.fields[i]]), ]) > 0) df@data[is.na(df@data[, agg.fields[i]]), ][, agg.fields[i]] <- 0
  }
  df@data[agg.fields] <- lapply(df@data[agg.fields], as.numeric)
  #aggregate - calculate statistics by polygon area ####
  #this df will be merged to the original poly
  POLY.agg = as.data.frame(as.list(aggregate(
    df@data[agg.fields],
    by = df@data[c(agg.poly.field)],
    FUN = function(x)
      c(
        MEAN = round(mean(x), 4),
        CNT = round(length(x), 4),
        SUM = round(sum(x), 4)
      )
  )))
  
  POLY.agg[,2:ncol(POLY.agg)] <- sapply(POLY.agg[,2:ncol(POLY.agg)], as.numeric)
  if (!is.null(sens.fields)){
    POLY.agg.sens = as.data.frame(as.list(aggregate(
      df@data[intersect(sens.fields, colnames(df@data))],
      by = df@data[c(agg.poly.field)],
      FUN = function(x)
        c(
          CNT = round(length(unique(x)), 4)
        )
    )))
    POLY.agg.sens$TOTUNIQUE = apply(as.data.frame(POLY.agg.sens[,2:ncol(POLY.agg.sens)]), 1, min)
    POLY.agg.sens$CAN_SHOW <- 'NA'
    if (nrow(POLY.agg.sens[POLY.agg.sens$TOTUNIQUE>=rule.of,])>0) POLY.agg.sens[POLY.agg.sens$TOTUNIQUE>=rule.of,]$CAN_SHOW <- 'YES'
    if (nrow(POLY.agg.sens[POLY.agg.sens$TOTUNIQUE<rule.of,])>0) POLY.agg.sens[POLY.agg.sens$TOTUNIQUE< rule.of,]$CAN_SHOW <- 'NO'
    POLY.agg = merge(POLY.agg, POLY.agg.sens)
    POLY.agg = merge(agg.poly, POLY.agg)
    rm(POLY.agg.sens) 
  }else{
    POLY.agg$CAN_SHOW = 'YES'
  }

  allowed.areas  = POLY.agg[!is.na(POLY.agg$CAN_SHOW) & POLY.agg$CAN_SHOW=='YES',agg.poly.field]@data[[agg.poly.field]]
  allowed.areas.sp = agg.poly[agg.poly@data[[agg.poly.field]] %in% allowed.areas,]

  df$ORD_df = seq.int(nrow(df))
  grid2Min$ORD_gr <-  seq.int(nrow(grid2Min)) 

    if (length(allowed.areas)>0){
    #clip the data to those overlaying acceptable NAFO
    df.allowed <- df[allowed.areas.sp, ]
    grid2Min.allowed <- grid2Min[allowed.areas.sp,]
    
    #step 1 -- figure out which grid each point is in.
    join <- over(df.allowed, grid2Min.allowed)
    join$ORD_df <- seq.int(nrow(join)) 
    test <- merge(df.allowed,join)    
    
    #step 2 -- aggregate the points by the grids
    grid.agg = as.data.frame(as.list(aggregate(
          test@data[agg.fields],
              by = test@data[c('ORD_gr')],
              FUN = function(x)
                c(
                  MEAN = round(mean(x), 4),
                  CNT = round(length(x), 4),
                  SUM = round(sum(x), 4)
                )
            )))
    
    #step 3 -- append the aggregated data back onto the grid 
    grid2Min.allowed@data <- data.frame(grid2Min.allowed@data, grid.agg[match(grid2Min.allowed@data[,"ORD_gr"], grid.agg[,"ORD_gr"]),])
    grid2Min.allowed@data$ORD_gr.1 <- NULL
    
    test =   grid2Min.allowed[!is.na(grid2Min.allowed[[plotcol]]),]

       if (nrow(df.allowed@data)){
  
      show.this = test
      
      file_id = ifelse(!is.null(file_id),paste0(file_id,"_"),"")
      POLY.agg.name = paste0(file_id,'screened_areas_', ts)
      this.df.name = paste0(file_id,'2MinGrid_', ts)

      if(show.plot | save.plot){
        if (save.plot) {
          plot.new()
          png(paste0(this.df.name,'.png'), width = 1600, height=1600)
          plot(POLY.agg, col=NA, border="gray85")
        }else{
          plot(POLY.agg, col=NA, border=NA)
        }
        cols <- RColorBrewer::brewer.pal(n = nclasses, name = "Blues")
        breaks <- classInt::classIntervals(show.this@data[[plotcol]], n = nclasses, style = "fisher", unique = TRUE)$brks
        plot(show.this, main = fun, col = cols[findInterval(show.this@data[[plotcol]], breaks, all.inside = TRUE)], border = NA, add=T)
        plot(POLY.agg, border = "gray85", add=T, col = ifelse(is.na(POLY.agg$CAN_SHOW),"grey65", ifelse(POLY.agg$CAN_SHOW == "YES", NA, "red")))
        if (save.plot) dev.off()
      }


        
        if (create.shps){
        POLY.agg = prepare_shape_fields(POLY.agg)
        this.df = prepare_shape_fields(show.this)
        
        rgdal::writeOGR(POLY.agg, ".", POLY.agg.name, driver="ESRI Shapefile", overwrite_layer=TRUE)
        cat(paste0("\nCreated shapefile ", getwd(), .Platform$file.sep, POLY.agg.name,".shp"))
        
        rgdal::writeOGR(this.df, ".", this.df.name, driver="ESRI Shapefile", overwrite_layer=TRUE)
        cat(paste0("\nCreated shapefile ", getwd(), .Platform$file.sep, this.df.name,".shp"))
      }
      results= list("POLY_AGG" = POLY.agg, "Grid2Min" = show.this)
    }else{
      print("no data to show")
      results= list("POLY_AGG" = POLY.agg, "Grid2Min" = NULL)
    }
    
  }else{
    print("No polygon has enough unique values to allow aggregated data to be shown")
    results= list("POLY_AGG" = NULL, "Grid2Min" = NULL)

  }
  
  return(results)
}

