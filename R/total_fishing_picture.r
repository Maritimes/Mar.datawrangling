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
#' @family fleets
#' @return a list of 6 objects - "obs_raw" & "marfis_raw" contain the data extracted
#' from their respected databases.  "obs_sp" & "marfis_sp" are the same data, but as 
#' SpatialPointsDataFrames. "vmstracks" is a SpatialLinesDataFrame with identified 
#' "treks". "obs_coverage" is a table showing the number of observed trips vs the 
#' total number of trips (by area if obsCovByArea is TRUE)  
#' @importFrom rgdal readOGR
#' @importFrom lubridate years
#' @importFrom stats aggregate
#' @importFrom graphics plot
#' @importFrom Mar.utils VMS_get_recs
#' @importFrom Mar.utils VMS_clean_recs
#' @importFrom Mar.utils make_segments
#' @importFrom Mar.utils identify_area
#' @importFrom Mar.utils prepare_shape_fields
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
                                maxBreak_mins = 1440,
                                obsCovByArea = FALSE, agg.poly.shp = "NAFO", 
                                agg.poly.field = "NAFO_1",
                                data.dir = NULL, qplot=FALSE, quiet = FALSE){
  if (is.null(dateEnd)) dateEnd = as.Date(dateStart,origin = "1970-01-01")+lubridate::years(1)
  thisFleet = get_fleet(dateStart = dateStart, dateEnd = dateEnd, mdCode = mdCode, gearCode = gearCode, sectors = sectors, data.dir=data.dir, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  if (nrow(thisFleet)==0)stop("\n","No vessels found for the supplied criteria")
  #have to set explicitly in case it wasn't specified in call (use what we got from get_fleet)
  mdCode = unique(thisFleet$MON_DOC_DEFN_ID)
  thisFleetLics = unique(thisFleet$LICENCE_ID)
  thisFleetUnq = paste0(thisFleet$MON_DOC_ID,"_",thisFleet$LICENCE_ID,"_",thisFleet$VR_NUMBER,"_",thisFleet$FV_GEAR_CODE)
  # thisFleetVRNs = unique(thisFleet$VR_NUMBER)
  #create an environ for all this stuff
  tfpEnv = new.env()
  if (!quiet)cat("\n","Retrieving MARFIS data")
  # MARFIS Data  
  Mar.datawrangling::get_data_custom('marfissci', tables = "PRO_SPC_INFO", data.dir = data.dir, quiet=T, env = tfpEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  
  tfpEnv$PRO_SPC_INFO =  tfpEnv$PRO_SPC_INFO[which((tfpEnv$PRO_SPC_INFO$DATE_FISHED >= dateStart | tfpEnv$PRO_SPC_INFO$LANDED_DATE >= dateStart) 
                                                   & (tfpEnv$PRO_SPC_INFO$DATE_FISHED < dateEnd | tfpEnv$PRO_SPC_INFO$LANDED_DATE < dateEnd)
                                                   & ((paste0(tfpEnv$PRO_SPC_INFO$MON_DOC_ID,"_",tfpEnv$PRO_SPC_INFO$LICENCE_ID,"_",tfpEnv$PRO_SPC_INFO$VR_NUMBER_FISHING,"_",tfpEnv$PRO_SPC_INFO$GEAR_CODE) %in% thisFleetUnq)
                                                      | (paste0(tfpEnv$PRO_SPC_INFO$MON_DOC_ID,"_",tfpEnv$PRO_SPC_INFO$LICENCE_ID,"_",tfpEnv$PRO_SPC_INFO$VR_NUMBER_LANDING,"_",tfpEnv$PRO_SPC_INFO$GEAR_CODE) %in% thisFleetUnq))), ]
  Mar.datawrangling::get_data_custom('marfissci', tables = "LOG_EFRT_STD_INFO", data.dir = data.dir, quiet=T, env = tfpEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  marfData = merge(tfpEnv$PRO_SPC_INFO,tfpEnv$LOG_EFRT_STD_INFO, by = c("LOG_EFRT_STD_INFO_ID", "MON_DOC_ID"), all.x =T)
  
  
  if (nrow(marfData[!is.na(marfData$LATITUDE)& !is.na(marfData$LONGITUDE),])>0){
    save_data(df=marfData,filename = 'marfis',formats = c("sp","shp","raw"), lat.field = "LATITUDE", lon.field = "LONGITUDE", env = tfpEnv)
  }
  cat(paste0("\n","MARFIS data range: ",min(marfData$DATE_FISHED), " - ", max(marfData$DATE_FISHED)))
  marfisRange = range(marfData$DATE_FISHED)
  thisFleetVRNs = unique(c(marfData$VR_NUMBER_FISHING, marfData$VR_NUMBER_LANDING))
  #rm("PRO_SPC_INFO", envir = tfpEnv)
  if(!exists("raw_marfis",envir = tfpEnv)){
    stop("\n","No MARFIS data matches filters")
  }
  #VMS Data (1)
  if (length(thisFleetVRNs)>1000){
    cat("\n","Too many vessels to extract VMS data (max of 1000)")
  }else{
    if (!quiet)cat("\n","Retrieving VMS data")
    vmsRecs = Mar.utils::VMS_get_recs(dateStart = dateStart, dateEnd = dateEnd, hrBuffer = 0, vrnList = thisFleetVRNs)
    vmsRecsCln = Mar.utils::VMS_clean_recs(vmsRecs, maxBreak_mins = maxBreak_mins)
    vmsRecsCln$OBSERVED <- 0
    vmsRecsCln$MARFISMATCH <- 0
    #wait till after ISDB to convert VMS to segments
  }
  # Observer Data
  if (!quiet)cat("\n","Retrieving Observer data")
  get_data('isdb',data.dir = data.dir, quiet = TRUE, env = tfpEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  #add specific dates so we can search by finer date range than year:
  thed = data.frame(fsDate = as.POSIXct(
    ifelse(lubridate::year(tfpEnv$ISSETPROFILE_WIDE$DATE_TIME1) > 2500, 
           ifelse(lubridate::year(tfpEnv$ISSETPROFILE_WIDE$DATE_TIME2) > 2500, 
                  ifelse(lubridate::year(tfpEnv$ISSETPROFILE_WIDE$DATE_TIME3) > 2500, tfpEnv$ISSETPROFILE_WIDE$DATE_TIME4, 
                         tfpEnv$ISSETPROFILE_WIDE$DATE_TIME3), 
                  tfpEnv$ISSETPROFILE_WIDE$DATE_TIME2), tfpEnv$ISSETPROFILE_WIDE$DATE_TIME1), 
    origin = "1970-01-01"))
  thed = Mar.utils::simple_date(thed, "fsDate")
  tfpEnv$ISSETPROFILE_WIDE = cbind(tfpEnv$ISSETPROFILE_WIDE,thed)
  tfpEnv$ISSETPROFILE_WIDE = tfpEnv$ISSETPROFILE_WIDE[tfpEnv$ISSETPROFILE_WIDE$fsDate >= min(marfisRange) 
                                                      & tfpEnv$ISSETPROFILE_WIDE$fsDate < max(marfisRange),]
  tfpEnv$ISTRIPTYPECODES = tfpEnv$ISTRIPTYPECODES[tfpEnv$ISTRIPTYPECODES$TRIPCD_ID < 7010 
                                                  | tfpEnv$ISTRIPTYPECODES$TRIPCD_ID == 7099 ,]
  #first part of condition below seems enough, but fisheries like clam don't have values for marfis licence No 
  #or marfis conf id - danger here of getting boats fishing under another licence?
  if (13 %in% mdCode) {
    tfpEnv$ISTRIPS = tfpEnv$ISTRIPS[ tfpEnv$ISTRIPS$MARFIS_LICENSE_NO %in% thisFleetLics 
                                     | tfpEnv$ISTRIPS$LICENSE_NO %in% thisFleetVRNs, ]
  }else{
    tfpEnv$ISTRIPS = tfpEnv$ISTRIPS[tfpEnv$ISTRIPS$MARFIS_LICENSE_NO %in% thisFleetLics, ]
    #MARFIS_CONF_NUMBER?
  }
  if (nrow(tfpEnv$ISTRIPS)>0){
    self_filter(env = tfpEnv, quiet = TRUE)
    save_data(db="isdb",filename = "isdb",formats = c("shp","sp","raw"), env = tfpEnv)
  }
  cleanup('isdb', env = tfpEnv)
  if(!exists("raw_isdb",envir = tfpEnv)){
    cat("\n","No Observer data matches filters")
  } else {
    if (length(thisFleetVRNs)<=1000){
      #VMS Data (2)
      #try to identify which VMS tracks had an observer (i.e. correct VRN and in appropriate time span)
      if (!quiet)cat("\n","Associating VMS data with Observer data")
      obTrips = unique(tfpEnv$raw_isdb[,c("MARFIS_LICENSE_NO","LICENSE_NO","BOARD_DATE","LANDING_DATE")])
      if (!is.null(obTrips)) obTrips[,"meanDate"] = as.Date((as.integer(obTrips[,"LANDING_DATE"]) + as.integer(obTrips[,"BOARD_DATE"]))/2, origin="1970-01-01")
    }
    #make segments to get start and end times of tracks
    if(nrow(vmsRecsCln)>1){
    vmstracks = Mar.utils::make_segments(vmsRecsCln,objField = "trek",seqField = "POSITION_UTC_DATE",
                                         filename = "vms",createShp = F,plot = FALSE)
    vmstracks = vmstracks$segments
    #ensure that only VMS data with some overlap of marfis data is retained
    vmstracks <- vmstracks[vmstracks$trekMin < max(marfisRange) & vmstracks$trekMax >= min(marfisRange),]
    }else{
      vmstracks<-NA
    }
    #make an effort to only get VMS data for known marfis catches
    if (!quiet)cat("\n","Removing VMS recs not associated with MARFIS catches")
    marfisTrips = marfData[,c("VR_NUMBER_FISHING","VR_NUMBER_LANDING","LICENCE_ID","DATE_FISHED", "LANDED_DATE")]
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

    if (!is.null(obTrips) && nrow(obTrips) >0){
      if (nrow(vmstracks@data)>0){
      for (i in 1:nrow(obTrips)){
        vmstracks@data[vmstracks@data[,"trekMin"]<obTrips[i,"meanDate"] & vmstracks@data[,"trekMax"]>obTrips[i,"meanDate"] 
                       & vmstracks@data[,"VR_NUMBER"] == obTrips[i,"LICENSE_NO"],"OBSERVED"]<-1 
      }
      }
    }
    if (!is.na(vmstracks) && nrow(vmstracks@data)>0){
    vmstracksShp = Mar.utils::prepare_shape_fields(vmstracks)
    ts = format(Sys.time(), "%Y%m%d_%H%M")
    nm = paste0("vms_",ts)
    rgdal::writeOGR(obj = vmstracksShp, layer = nm, dsn = getwd(), driver = "ESRI Shapefile",  overwrite_layer = TRUE)
    cat("\n","VMS data saved as shapefile to ",paste0(getwd(),"/",nm,".shp"))
    }
  }
  if(obsCovByArea){
    #get distribution percentages of observer data
    if (agg.poly.shp == "NAFO"){
      agg.poly.shp <- NULL
    }
    if (exists("raw_isdb",envir = tfpEnv)){
      if (!quiet)cat("\n","Determining location of Observer data")
      
      tfpEnv$raw_isdb = Mar.utils::identify_area(tfpEnv$raw_isdb,
                                                 agg.poly.shp = agg.poly.shp,
                                                 agg.poly.field = agg.poly.field)
      tt2 = stats::aggregate(
        x = list(cnt =  tfpEnv$raw_isdb$TRIP_ID),
        by = list(TRIP_ID = tfpEnv$raw_isdb$TRIP_ID,
                  area =  tfpEnv$raw_isdb[,agg.poly.field]
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
    if(exists("raw_marfis",envir = tfpEnv)){
      if (!quiet)cat("\n","Determining location of MARFIS data")
      tfpEnv$raw_marfis = Mar.utils::identify_area(tfpEnv$raw_marfis, 
                                                   agg.poly.shp = agg.poly.shp,
                                                   agg.poly.field = agg.poly.field)
      raw_marfisTrip = unique(tfpEnv$raw_marfis[,c(agg.poly.field,"TRIP_ID")])
      marfis_agg = stats::aggregate(
        x = list(MARFIS = raw_marfisTrip$TRIP_ID),
        by = list(area = raw_marfisTrip[,agg.poly.field]
        ),
        length
      )
    }
    
    if (!quiet)cat("\n","Determining relative observer coverage for this data")
    
    if (!is.null(agg.poly.shp)){
      agg.poly <- rgdal::readOGR(dsn = agg.poly.shp, verbose = FALSE)
    }else{
      agg.poly <- Mar.data::NAFOSubunits
    }
    allAreas = data.frame(area = unique(agg.poly@data[,agg.poly.field]))
    
    if (exists("raw_isdb",envir = tfpEnv)){
      if (nrow(isdb_agg)>0) {
        allAreas = merge(allAreas, isdb_agg, all.x = T)
      }else{
        allAreas$OBS <- 0
      }
    }else{
      allAreas$OBS <- 0
    }
    if (exists("raw_marfis",envir = tfpEnv)){
      if (nrow(marfis_agg)>0) {
        allAreas = merge(allAreas, marfis_agg, all.x = T)
      }else{
        allAreas$MARFIS <- 0
      }
    }else{
      allAreas$MARFIS <- 0
    }
    if (class(allAreas)=="data.frame"){
      allAreas = allAreas[which(!is.na(allAreas$OBS) | !is.na(allAreas$MARFIS)),]
      allAreas[is.na(allAreas)] <- 0
      allAreas$PERCENT <- allAreas$OBS/allAreas$MARFIS *100
    }else{
      allAreas <- NA
    }
    
  }else{
    allAreas <- data.frame(OBS = length(unique(tfpEnv$raw_isdb$TRIP_ID)),
                           MARFIS = length(unique(tfpEnv$raw_marfis$TRIP_ID)),
                           PERCENT = (length(unique(tfpEnv$raw_isdb$TRIP_ID))/
                                        length(unique(tfpEnv$raw_marfis$TRIP_ID)))*100)
  }

  res = list()
  res[["obs_raw"]]<- tfpEnv$raw_isdb
  res[["obs_sp"]]<- tfpEnv$sp_isdb
  res[["marfis_raw"]]<- tfpEnv$raw_marfis
  res[["marfis_sp"]]<- tfpEnv$sp_marfis
  res[["vmstracks"]]<- vmstracks
  res[["obs_coverage"]]<- allAreas
  if (qplot){

    #establish a boundbox based on marfis and/or observer data - not VMS which can be crazy
    if (exists("sp_marfis",envir = tfpEnv) & exists("sp_isdb",envir = tfpEnv)){
      datwindow = data.frame(LATITUDES = range(c(tfpEnv$sp_marfis@bbox[2,], 
                                                 tfpEnv$sp_isdb@bbox[2,])), 
                             LONGITUDES = range(c(tfpEnv$sp_marfis@bbox[1,], 
                                                  tfpEnv$sp_isdb@bbox[1,])))
    }else if (exists("sp_marfis",envir = tfpEnv)){
      datwindow = data.frame(LATITUDES = tfpEnv$sp_marfis@bbox[2,], 
                             LONGITUDES = tfpEnv$sp_marfis@bbox[1,])
    }else if (exists("sp_isdb",envir = tfpEnv)){
      datwindow = data.frame(LATITUDES = tfpEnv$sp_isdb@bbox[2,], 
                             LONGITUDES = tfpEnv$sp_isdb@bbox[1,])
    }
    plot(datwindow$LONGITUDES, datwindow$LATITUDES, col="transparent")
    sp::plot(Mar.data::coast_lores,add=T, col="tan")
    if (!is.na(vmstracks) && exists("sp_isdb", envir = tfpEnv)){
      colors = c("grey", "cornflowerblue")
      sp::plot(vmstracks,add=T, col=colors[cut(vmstracks$OBSERVED, 2)])
    } else if (!is.na(vmstracks)){
      sp::plot(vmstracks,add=T, col="grey")
    }
    if (exists("sp_marfis",envir = tfpEnv)) sp::plot(tfpEnv$sp_marfis,add=T, col="black", pch=3, cex =0.5)
    if (exists("sp_isdb", envir = tfpEnv))sp::plot(tfpEnv$sp_isdb,add=T, col="blue", pch=16, cex =0.5)
  }
  return(res)
}

