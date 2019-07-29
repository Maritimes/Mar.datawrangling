#' @title total_fishing_picture
#' @description This function extracts all of the observer, Marfis and VMS data 
#' for a particular date range, for a particular Fleet (identified by the 
#' monitoring document code). 
#' @param fn.oracle.username default is \code{'_none_'} This is your username for
#' accessing oracle objects. If you have a value for \code{oracle.username} 
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take 
#' priority over your existing value.
#' @param fn.oracle.password default is \code{'_none_'} This is your password for
#' accessing oracle objects. If you have a value for \code{oracle.password}  
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take 
#' priority over your existing value.
#' @param fn.oracle.dsn default is \code{'_none_'} This is your dsn/ODBC
#' identifier for accessing oracle objects. If you have a value for 
#' \code{oracle.dsn} stored in your environment (e.g. from an rprofile file), 
#' this can be left and that value will be used.  If a value for this is 
#' provided, it will take priority over your existing value.
#' @param usepkg default is \code{'rodbc'}. This indicates whether the connection to Oracle should
#' use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is slightly easier to setup, but
#' roracle will extract data ~ 5x faster.
#' @param dateStart default is \code{NULL}. This is the start date (YYYY-MM-DD)
#' of the window of time you want to look at.
#' @param dateEnd default is \code{NULL}. This is the end date (YYYY-MM-DD)
#' of the window of time you want to look at.  If this is left blank, 1 year of 
#' data will be returned.
#' @param mdCode default is \code{NULL}. This is the MARFIS Monitoring
#' Document code used to identify fleets. If this is left as NULL, a popup
#' will allow the user to select one from a list.
#' @param gearCode default is \code{NULL}. In some cases, a fleet will contain multiple
#' gear types. Setting this to \code{NULL} (the default) will prompt you to 
#' select gears from the available values (if there are multiple).  Setting it to 
#' \code{'all'} will get all gear types.  Entering a vector of MARFIS gear codes 
#' (e.g. \code{c(51,81)}) will only return those gear codes.
#' @param sectors default is \code{7} (i.e. Maritimes). This identifies the region.
#' @param getVMS default is \code{FALSE}.  This indicates whether or not you want to 
#' also get VMS data as part of your call.  If TRUE, it will try to associate the 
#' VMS data with the Observer data.
#' @param maxBreak_mins default is \code{1440} (minutes, or 24 hours).  This is the 
#' amount of time that can pass for a vessel between successive VMS points before 
#' a new "trek" is imposed.  For example, if a vessel sits motionless at port 
#' for 13 hours before heading out again, the new data will count towards the 
#' previous "trek".  However, if it sits for 25 hours, a new trek will be created.
#' This does not change the data that is extracted, just how the data is broken 
#' into discrete line segments. 
#' @param obsCovByArea default is \code{FALSE}. Observer coverage is calculated 
#' simply as a percentage of observer trips vs all trips.  If this is set to TRUE,
#' coverage will be calculated on an area by area basis using the shapefile identified 
#' in \code{agg_poly_shape} and the field identified in \code{agg.poly.field}.
#' @param agg.poly.shp default is \code{NULL}.  This is the shapefile that has 
#' polygons that should be checked for sufficient unique values of the 
#' sens.fields.  If NULL, NAFO zones will be used.  Otherwise, a path to any 
#' polygon shapefile can be provided. 
#' @param agg.poly.field default is \code{NULL}.  This identifies the field within 
#' the shapefile provided to agg.poly.shp that should be used to check for 
#' sufficient unique values of the sens.fields.
#' @param quiet default is \code{FALSE}. If set to TRUE, no output messages will
#' be displayed as the script runs. 
#' @param data.dir  The default is your working directory. If you are hoping to 
#' load existing data, this folder should identify the folder containing your 
#' *.rdata files.
#' @param qplot  default is \code{FALSE}. If set to TRUE, a quick and dirty plot 
#' of the vms (grey (unobserved) and blue (observed)), Marfis (small black crosses) 
#' and Observer data (blue dots) will be generated.
#' @param create.shps default is \code{FALSE}.  This indicates whether or not 
#' shapefiles should be created for 1) Marfis data, 2) Observer data and 3) VMS 
#' data.  
#' @family fleets
#' @return a list of 6 objects - "obs_raw" & "marfis_raw" contain the data extracted
#' from their respected databases.  "obs_sp" & "marfis_sp" are the same data, but as 
#' SpatialPointsDataFrames. "vmstracks" is a SpatialLinesDataFrame with identified 
#' "treks". "obs_coverage" is a table showing the number of observed trips vs the 
#' total number of trips (by area if obsCovByArea is TRUE)  
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' 
total_fishing_picture<-function(fn.oracle.username = "_none_", 
                                fn.oracle.password = "_none_", 
                                fn.oracle.dsn = "_none_",
                                usepkg = "rodbc",
                                dateStart = NULL, dateEnd =NULL, 
                                mdCode = NULL, gearCode = NULL,
                                sectors = 7,
                                getVMS = FALSE,
                                maxBreak_mins = 1440,
                                obsCovByArea = FALSE, agg.poly.shp = NULL, 
                                agg.poly.field =NULL,
                                data.dir = NULL, qplot=FALSE, 
                                create.shps = FALSE, quiet = FALSE){
  
  #following are vars that will be created by data.table, and build errors
  #appear if we don't define them
  .I <- cnt <- elapsedDist_m <- elapsedTime_min<- TRIP_ID <- NA
  
  if (is.null(dateEnd)) dateEnd = as.Date(dateStart,origin = "1970-01-01")+lubridate::years(1)
  thisFleet = get_fleet(dateStart = dateStart, dateEnd = dateEnd, 
                        mdCode = mdCode, gearCode = gearCode, sectors = sectors, 
                        data.dir=data.dir, fn.oracle.username = fn.oracle.username, 
                        fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  
  if (nrow(thisFleet)==0)stop("\n","No vessels found for the supplied criteria")
  #have to set explicitly in case it wasn't specified in call (use what we got from get_fleet)
  mdCode = unique(thisFleet$MON_DOC_DEFN_ID)
  thisFleetLics = unique(thisFleet$LICENCE_ID)
  thisFleetUnq = paste0(thisFleet$MON_DOC_ID,"_",thisFleet$LICENCE_ID,"_",thisFleet$VR_NUMBER,"_",thisFleet$GEAR_CODE)
  #create an environ for all this stuff
  tfpEnv = new.env()
  
  
  # MARFIS ------------------------------------------------------------------  
  get_MARFIS <- function(dateStart= dateStart, dateEnd=dateEnd,data.dir = data.dir, env = tfpEnv, 
                         fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, 
                         fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg, quiet = F, thisFleetUnq = thisFleetUnq){
    # MARFIS Data  
    get_data_custom('marfissci', tables = "PRO_SPC_INFO", data.dir = data.dir, quiet=T, env = tfpEnv, 
                                       fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, 
                                       fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
    tfpEnv$PRO_SPC_INFO = Mar.utils::clean_dfo_fields(tfpEnv$PRO_SPC_INFO)
    tfpEnv$PRO_SPC_INFO =  tfpEnv$PRO_SPC_INFO[which((tfpEnv$PRO_SPC_INFO$DATE_FISHED >= dateStart & tfpEnv$PRO_SPC_INFO$DATE_FISHED <= dateEnd)
                                                     & ((paste0(tfpEnv$PRO_SPC_INFO$MON_DOC_ID,"_",tfpEnv$PRO_SPC_INFO$LICENCE_ID,"_",tfpEnv$PRO_SPC_INFO$VR_NUMBER_FISHING,"_",tfpEnv$PRO_SPC_INFO$GEAR_CODE) %in% thisFleetUnq)
                                                        | (paste0(tfpEnv$PRO_SPC_INFO$MON_DOC_ID,"_",tfpEnv$PRO_SPC_INFO$LICENCE_ID,"_",tfpEnv$PRO_SPC_INFO$VR_NUMBER_LANDING,"_",tfpEnv$PRO_SPC_INFO$GEAR_CODE) %in% thisFleetUnq))), ]
    get_data_custom('marfissci', tables = "LOG_EFRT_STD_INFO", data.dir = data.dir, quiet=T, env = tfpEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
    tfpEnv$LOG_EFRT_STD_INFO = Mar.utils::clean_dfo_fields(tfpEnv$LOG_EFRT_STD_INFO)
    
    get_data_custom('marfissci', tables = "MON_DOC_ENTRD_DETS", data.dir = data.dir, quiet=T,env = tfpEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
    tfpEnv$MON_DOC_ENTRD_DETS = tfpEnv$MON_DOC_ENTRD_DETS[tfpEnv$MON_DOC_ENTRD_DETS$COLUMN_DEFN_ID %in%  c(741,72),c("MON_DOC_ID","COLUMN_DEFN_ID","DATA_VALUE")] #741 is the defn where MARF stores Obs #
    tfpEnv$MON_DOC_ENTRD_DETS$DATA_VALUE = gsub(pattern = "[^[:alnum:]]", replacement = "", x= tfpEnv$MON_DOC_ENTRD_DETS$DATA_VALUE)
    colnames(tfpEnv$MON_DOC_ENTRD_DETS)[colnames(tfpEnv$MON_DOC_ENTRD_DETS)=="DATA_VALUE"] <- "TRIP_cln"
    
    this  = merge(tfpEnv$PRO_SPC_INFO,tfpEnv$LOG_EFRT_STD_INFO, by = c("LOG_EFRT_STD_INFO_ID", "MON_DOC_ID"), all.x =T)
    crap =merge(this, tfpEnv$MON_DOC_ENTRD_DETS, all.x=T)
    browser()
    if (nrow(this)>0){
      # save_data(df=res,filename = 'marfis',formats = c("sp","shp","raw"), lat.field = "LATITUDE", lon.field = "LONGITUDE", env = tfpEnv)
      cat(paste0("\n","MARFIS data range: ",min(this$DATE_FISHED), " - ", max(this$DATE_FISHED)))
    }else{
      cat("\n","No MARFIS data matches filters")
      stop()
    }
    return(this)
  }
  
  # VMS ---------------------------------------------------------------------
  get_VMS <- function(fn.oracle.username = fn.oracle.username, 
                      fn.oracle.password = fn.oracle.password, 
                      fn.oracle.dsn = fn.oracle.dsn, maxBreak_mins=maxBreak_mins,
                      dateStart = dateStart, dateEnd = dateEnd, vrnList = NULL,
                      marfisRange=marfisRange){
    
    if (!quiet)cat("\n","Retrieving VMS data")
    vmsRecs = Mar.utils::VMS_get_recs(fn.oracle.username = fn.oracle.username, 
                           fn.oracle.password = fn.oracle.password, 
                           fn.oracle.dsn = fn.oracle.dsn,usepkg = usepkg,
                           dateStart = dateStart, dateEnd = dateEnd, hrBuffer = 0, vrnList = vrnList)
    vmsRecsCln = Mar.utils::VMS_clean_recs(vmsRecs, maxBreak_mins = maxBreak_mins)
    vmsRecsCln$OBSERVED <- 0
    vmsRecsCln$MARFISMATCH <- 0
    if (nrow(vmsRecsCln)<1){
      warning("\nNo VMS data matches filters")
      return(NA)
    }
    vmstracks = Mar.utils::make_segments(vmsRecsCln,objField = "trek",seqField = "POSITION_UTC_DATE",
                                         filename = "vms",createShp = F,plot = FALSE)
    vmstracks = vmstracks$segments
    #ensure that only VMS data with some overlap of marfis data is retained
    vmstracks <- vmstracks[vmstracks$trekMin < max(marfisRange) & vmstracks$trekMax >= min(marfisRange),]
    res = list()
    res[["data"]] = vmsRecsCln
    res[["tracks"]] = vmstracks
    return(res)
  }
  
  # Observer ----------------------------------------------------------------
  get_OBS <- function(fn.oracle.username = fn.oracle.username, 
                      fn.oracle.password = fn.oracle.password, 
                      fn.oracle.dsn = fn.oracle.dsn, 
                      thisFleet = thisFleet){
    get_data('isdb' ,data.dir = data.dir, quiet = TRUE, env = tfpEnv, 
             fn.oracle.username = fn.oracle.username, 
             fn.oracle.password = fn.oracle.password, 
             fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
    browser()
    tfpEnv$ISTRIPS$TRIP_cln <- gsub(pattern = "[^[:alnum:]]", replacement = "", x=  tfpEnv$ISTRIPS$TRIP)
    obsTrips = unique(thisFleet[!is.na(thisFleet$TRIP_cln),"TRIP_cln"])
    tfpEnv$ISTRIPS = tfpEnv$ISTRIPS[tfpEnv$ISTRIPS$TRIP_cln %in% obsTrips,]
    if (nrow(tfpEnv$ISTRIPS)>0){
      self_filter(env = tfpEnv, quiet = TRUE)
      this = summarize_catches(db="isdb", drop.na.cols = FALSE, env = tfpEnv)
      if (!is.data.frame(this)){
        cat("\n","No Observer data matches filters")
        return(NA)
      }
    }else{
      cat("\n","No Observer data matches filters")
      return(NA)
    }
    cleanup('isdb', env = tfpEnv) 
    return(this)
  }
  
  # Tweak VMS tracks to only get those matching MARFIS and indicate OBS -----
  informVMSTracks <-function(marfisTrips = marfisTrips, thisFleet = thisFleet, vmstracks =vmsRecsCln$tracks, obTrips=obTrips){
    if (class(vmstracks) != "SpatialLinesDataFrame")return(NA)
    marfisTrips = marfisTrips[paste0(marfisTrips$LICENCE_ID,"_",marfisTrips$VR_NUMBER_FISHING) %in% paste0(thisFleet$LICENCE_ID,"_",thisFleet$VR_NUMBER) |
                                paste0(marfisTrips$LICENCE_ID,"_",marfisTrips$VR_NUMBER_LANDING) %in% paste0(thisFleet$LICENCE_ID,"_",thisFleet$VR_NUMBER),]
    marfisTrips = Mar.utils::simple_date(marfisTrips, c("LANDED_DATE","DATE_FISHED" ))
    marfisTrips$meanDate = as.Date((as.integer(marfisTrips[,"LANDED_DATE"]) + as.integer(marfisTrips[,"DATE_FISHED"]))/2, origin="1970-01-01")
    marfisTrips$DATE_FISHED<-marfisTrips$LANDED_DATE <- NULL
    marfisTrips = unique(marfisTrips)
    
    for (m in 1:nrow(marfisTrips)){
      if (nrow(vmstracks@data)>0){
        vmstracks@data[(vmstracks@data[,"trekMin"]<=marfisTrips[m,"meanDate"] & vmstracks@data[,"trekMax"]>=marfisTrips[m,"meanDate"]) &
                         (vmstracks@data[,"VR_NUMBER"] %in% as.character(c(marfisTrips[m,"VR_NUMBER_FISHING"], marfisTrips[m,"VR_NUMBER_LANDING"]))),"MARFISMATCH"]<-1
      }
    }
    if(is.data.frame(obTrips) && nrow(obTrips)>0){
    if (!quiet)cat("\n","Associating VMS data with Observer data")
    obTrips = unique(obTrips[,c("MARFIS_LICENSE_NO","LICENSE_NO","BOARD_DATE","LANDING_DATE")])
    obTrips = Mar.utils::simple_date(obTrips, datefields = c("BOARD_DATE","LANDING_DATE"))
    if (!is.null(obTrips)) obTrips[,"meanDate"] = as.Date((as.integer(obTrips[,"LANDING_DATE"]) + as.integer(obTrips[,"BOARD_DATE"]))/2, origin="1970-01-01")
    if (!is.null(obTrips) && nrow(obTrips) >0){
      if (nrow(vmstracks@data)>0){
        for (i in 1:nrow(obTrips)){
          vmstracks@data[vmstracks@data[,"trekMin"]<obTrips[i,"meanDate"] & vmstracks@data[,"trekMax"]>obTrips[i,"meanDate"] 
                         & vmstracks@data[,"VR_NUMBER"] == obTrips[i,"LICENSE_NO"],"OBSERVED"]<-1 
        }
      }
    }
  }else{
    if (!quiet)cat("\n","No observed trips to associate with the VMS data.")
  }
    return(vmstracks)
  } 
  
  # Calculate Coverage ------------------------------------------------------
  calcOBSCoverage<-function(obsCovByArea = obsCovByArea, 
                            OBS = NULL, 
                            MARFIS = NULL, 
                            agg.poly.shp = agg.poly.shp,
                            agg.poly.field = agg.poly.field){
    #get distribution percentages of observer data
    if (!obsCovByArea){
      if (!is.data.frame(OBS)){
        theseObs = 0
      }else{
        theseObs = length(unique(OBS$TRIP_ID))
      }
      allAreas <- data.frame(OBS = theseObs,
                             MARFIS = length(unique(MARFIS$TRIP_ID)),
                             PERCENT = (theseObs/
                                          length(unique(MARFIS$TRIP_ID))*100))
 
    }else{
      if (is.null(agg.poly.field))agg.poly.field="NAFO_1"
  
      if (is.data.frame(OBS)){
        if (!quiet)cat("\n","Determining location of Observer data.","\n",
                       "\tNote that observer data locations are recorded for each set while MARFIS data is recorded for each logbook record.","\n",
                       "n\tIn some cases, different sets within a trip can occur in different defined areas.",
                       "n\tIn order to assess observer coverage, the Observer data is converted to trips, and the trips are attributed to the area which had the majority of the trip's sets. ")
        OBS = Mar.utils::identify_area(OBS,
                                       agg.poly.shp = agg.poly.shp,
                                       agg.poly.field = agg.poly.field)
        tt2 = stats::aggregate(
          x = list(cnt =  OBS$TRIP_ID),
          by = list(TRIP_ID = OBS$TRIP_ID,
                    area =  OBS[,agg.poly.field]
          ),
          length
        )
        #Following is a hack.  We have the area for each set, but can only compare
        #against MARFIS at a trip level.  The following assigns each trip to the area
        #with the most sets.
        tt2 <- data.table::setDT(tt2)
        tt2 <- tt2[tt2[, .I[which.max(cnt)], by=TRIP_ID]$V1]
        tt2 <- as.data.frame(tt2)
        tt2$cnt<-NULL
        isdb_agg = stats::aggregate(
          x = list(OBS = tt2$TRIP_ID),
          by = list(area = tt2$area),
          length
        )
       
        
      }
      if(!is.null(MARFIS)){
        if (!quiet)cat("\n","Determining location of MARFIS data")
        MARFIS = Mar.utils::identify_area(MARFIS, 
                                          agg.poly.shp = agg.poly.shp,
                                          agg.poly.field = agg.poly.field)
        raw_marfisTrip = unique(MARFIS[,c(agg.poly.field,"TRIP_ID")])
        marfis_agg = stats::aggregate(
          x = list(MARFIS = raw_marfisTrip$TRIP_ID),
          by = list(area = raw_marfisTrip[,agg.poly.field]
          ),
          length
        )
      }
      if (!is.null(agg.poly.shp)){
        agg.poly <- rgdal::readOGR(dsn = agg.poly.shp, verbose = FALSE)
      }else{
        agg.poly <- Mar.data::NAFOSubunits
      }
      
      allAreas = data.frame(area = unique(agg.poly@data[,agg.poly.field]))
      if (exists("isdb_agg")) {
        allAreas = merge(allAreas, isdb_agg, all.x = T)
      }else{
        allAreas$OBS <- 0
      }
      if (nrow(marfis_agg)>0) {
        allAreas = merge(allAreas, marfis_agg, all.x = T)
      }else{
        allAreas$MARFIS <- 0
      }
        allAreas[c("OBS", "MARFIS")][is.na(allAreas[c("OBS", "MARFIS")])] <- 0
        allAreas = allAreas[allAreas$OBS+allAreas$MARFIS >0,]
        allAreas$PERCENT <- allAreas$OBS/allAreas$MARFIS *100
      
    }
    return(allAreas)
  }
  
  # quickplot ---------------------------------------------------------------
  drawPlot<-function(qplot = qplot, marfData=marfData, obsData=obsData,smartTracks=smartTracks) {
    #establish a boundbox based on marfis and/or observer data - not VMS which can be crazy
    if (!qplot)return(NULL)
    if (is.data.frame(marfData)){
      marfData_sp = Mar.utils::df_to_sp(Mar.utils::prepare_shape_fields(marfData))
    }else{
      marfData_sp= NA
    }
    if (is.data.frame(obsData)){
      obsData_sp = Mar.utils::df_to_sp(Mar.utils::prepare_shape_fields(obsData))
    }else{
      obsData_sp= NA
    }
    if ( class(marfData_sp) == "SpatialPointsDataFrame" &  class(obsData_sp) == "SpatialPointsDataFrame"){  
      datwindow = data.frame(LATITUDES = range(c(marfData_sp@bbox[2,], 
                                                 obsData_sp@bbox[2,])), 
                             LONGITUDES = range(c(marfData_sp@bbox[1,], 
                                                  obsData_sp@bbox[1,])))
    }else if ( class(marfData_sp) == "SpatialPointsDataFrame"){
      datwindow = data.frame(LATITUDES = marfData_sp@bbox[2,], 
                             LONGITUDES = marfData_sp@bbox[1,])
    }else if ( class(obsData_sp) == "SpatialPointsDataFrame"){
      datwindow = data.frame(LATITUDES = obsData_sp@bbox[2,], 
                             LONGITUDES = obsData_sp@bbox[1,])
    }
    graphics::plot(datwindow$LONGITUDES, datwindow$LATITUDES, col="transparent")
    sp::plot(Mar.data::coast_lores,add=T, col="tan")
    if (class(smartTracks) == "SpatialLinesDataFrame"){
      colors = c("grey", "cornflowerblue")
      sp::plot(smartTracks,add=T, col=colors[cut(smartTracks$OBSERVED, 2)])
    } 
    if (class(marfData_sp) == "SpatialPointsDataFrame") sp::plot(marfData_sp,add=T, col="black", pch=3, cex =0.5)
    if (class(obsData_sp) == "SpatialPointsDataFrame") sp::plot(obsData_sp,add=T, col="blue", pch=16, cex =0.5)
    return(NULL)
  }
  

  # create shapefiles -------------------------------------------------------
  createShapefiles<-function(obsData=obsData, marfData=marfData, smartTracks=smartTracks){
    ts = format(Sys.time(), "%Y%m%d_%H%M")
    if (is.data.frame(marfData)){
      fn_m =  paste0("marfis_",ts)
      marfData_sp = Mar.utils::df_to_sp(Mar.utils::prepare_shape_fields(marfData))
      rgdal::writeOGR(marfData_sp, ".",fn_m, driver="ESRI Shapefile", overwrite_layer=TRUE)
      cat(paste0("\nCreated shapefile: ", getwd(), .Platform$file.sep, fn_m,".shp"))
    }
    
    if (is.data.frame(obsData)){
      fn_o =  paste0("obs_",ts)
      obsData_sp = Mar.utils::df_to_sp(Mar.utils::prepare_shape_fields(obsData))
      rgdal::writeOGR(obsData_sp, ".",  fn_o, driver="ESRI Shapefile", overwrite_layer=TRUE)
      cat(paste0("\nCreated shapefile: ", getwd(), .Platform$file.sep, fn_o,".shp"))
    }
    
    if (class(smartTracks) == "SpatialLinesDataFrame"){
      fn_v =  paste0("vms_",ts)
      smartTracks = Mar.utils::prepare_shape_fields(smartTracks)
      rgdal::writeOGR(smartTracks, ".", fn_v, driver="ESRI Shapefile", overwrite_layer=TRUE)
      cat(paste0("\nCreated shapefile: ", getwd(), .Platform$file.sep, fn_v,".shp"))
    }
    cat("\nNote that some field names may be slightly different due to limitations of the Shapefile format.")
  }
  
  # Calls -------------------------------------------------------------------
  marfData  <- get_MARFIS(fn.oracle.username = fn.oracle.username, 
                          fn.oracle.password = fn.oracle.password, 
                          fn.oracle.dsn = fn.oracle.dsn, data.dir = data.dir, 
                          dateStart= dateStart, dateEnd=dateEnd, 
                          quiet=T, env = tfpEnv, 
                          usepkg = usepkg,thisFleetUnq = thisFleetUnq)
  
  thisFleetVRNs = unique(c(marfData$VR_NUMBER_FISHING, marfData$VR_NUMBER_LANDING))
  
  if (getVMS) {
    vmsRecsCln <- get_VMS(fn.oracle.username = fn.oracle.username, 
                        fn.oracle.password = fn.oracle.password, 
                        fn.oracle.dsn = fn.oracle.dsn,maxBreak_mins=maxBreak_mins,
                        dateStart = dateStart, dateEnd = dateEnd, vrnList = unique(thisFleet$VR_NUMBER),
                        marfisRange=range(marfData$DATE_FISHED)) 
  }else{
    vmsRecsCln <-NA
  }
  
  obsData = get_OBS(fn.oracle.username = fn.oracle.username, 
                    fn.oracle.password = fn.oracle.password, 
                    fn.oracle.dsn = fn.oracle.dsn, 
                    thisFleet = thisFleet)

  if (!is.na(vmsRecsCln)) {
    smartTracks = informVMSTracks(marfisTrips =  marfData[,c("VR_NUMBER_FISHING","VR_NUMBER_LANDING","LICENCE_ID","DATE_FISHED", "LANDED_DATE")], 
                                thisFleet = thisFleet, vmstracks =vmsRecsCln$tracks, obTrips = obsData)
  }else{
    smartTracks<-NA
  }
  
  
  obsCov = calcOBSCoverage(obsCovByArea = obsCovByArea, 
                           OBS = obsData, MARFIS = marfData, 
                           agg.poly.shp=agg.poly.shp, 
                           agg.poly.field=agg.poly.field)

  drawPlot(qplot, marfData=marfData, obsData=obsData,smartTracks=smartTracks )
  if (create.shps) createShapefiles(obsData=obsData, marfData=marfData, smartTracks=smartTracks)
  res = list()
  res[["obs_raw"]]<- obsData
  res[["marfis_raw"]]<- marfData
  res[["vmstracks"]]<- smartTracks
  res[["obs_coverage"]]<- obsCov
  
  return(res)
}

