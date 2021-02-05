#' @title data_tweaks2
#' @description After the initial extraction, this function is called to do minor housekeeping
#' to the extracted datasets to ensure they works with the other functions.
#' @param db default is \code{"ALL"}. This identifies the dataset you are working 
#' with.
#' @param data.dir  The default is a "data" folder within your working directory. This is the path to where you want the
#' extracted files to go.
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' @note data is not added to the global environment by this function - changes are made, saved, and
#' dropped.
data_tweaks2 <- function(db="ALL", data.dir = NULL){
  cat("\nApplying tweaks v.2 ...")
  if (db %in% c("ALL","isdb")){
    # ISDB ----------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(data.dir,"ISDB.ISCATCHES.RData")) & file.exists(file.path(data.dir,"ISDB.ISFISHSETS.RData"))){
      load(file.path(data.dir,"ISDB.ISCATCHES.RData"))
      load(file.path(data.dir,"ISDB.ISFISHSETS.RData"))
      if (!'S_EST_NUM_CAUGHT' %in% colnames(ISCATCHES)){
        ISFISHSETS.directed=ISFISHSETS[c("FISHSET_ID","SPECSCD_ID")]  #keep only the field identifying the sought spp for each set
        ISCATCHES.directed = merge(ISCATCHES,ISFISHSETS.directed, by.x=c("FISHSET_ID","SPECCD_ID"), by.y=c("FISHSET_ID","SPECSCD_ID")) #get the catches of directed for each set
        ISCATCHES.directed = ISCATCHES.directed[c("FISHSET_ID", "SPECCD_ID", "EST_NUM_CAUGHT", "EST_KEPT_WT", "EST_DISCARD_WT", "EST_REDUCTION_WT", "EST_COMBINED_WT")] #keep some fields
        names(ISCATCHES.directed) <- c("FISHSET_ID", "SPECSCD_ID","S_EST_NUM_CAUGHT","S_EST_KEPT_WT","S_EST_DISCARD_WT","S_EST_REDUCTION_WT","S_EST_COMBINED_WT") #rename to reflect sought nature
        ISCATCHES = merge(ISCATCHES, ISCATCHES.directed, all.x=T, by.x=c("FISHSET_ID"), by.y=c("FISHSET_ID")) #get the catches of directed for each set
        
        save( ISCATCHES, file=file.path(data.dir, "ISDB.ISCATCHES.RData"), compress=TRUE)
        cat("\nISCATCHES: Added directed species catch numbers and weights onto each record......")
      }
      rm(ISCATCHES)
      rm(ISFISHSETS)
    }
    if (file.exists(file.path(data.dir,"ISDB.ISSETPROFILE_WIDE.RData"))){
      load(file.path(data.dir,"ISDB.ISSETPROFILE_WIDE.RData"))
      if (!'LATITUDE' %in% colnames(ISSETPROFILE_WIDE) | !'LONGITUDE' %in% colnames(ISSETPROFILE_WIDE)){
        
        ISSETPROFILE_WIDE$LATITUDE =
          ifelse(is.na(ISSETPROFILE_WIDE$LAT1)| ISSETPROFILE_WIDE$LAT1 == 0,
                 ifelse(is.na(ISSETPROFILE_WIDE$LAT2)| ISSETPROFILE_WIDE$LAT2 == 0,
                        ifelse(is.na(ISSETPROFILE_WIDE$LAT3)| ISSETPROFILE_WIDE$LAT3 == 0,
                               ISSETPROFILE_WIDE$LAT4, ISSETPROFILE_WIDE$LAT3),
                        ISSETPROFILE_WIDE$LAT2),
                 ISSETPROFILE_WIDE$LAT1)
        
        ISSETPROFILE_WIDE$LONGITUDE =
          ifelse(is.na(ISSETPROFILE_WIDE$LONG1) | ISSETPROFILE_WIDE$LONG1 == 0,
                 ifelse(is.na(ISSETPROFILE_WIDE$LONG2) | ISSETPROFILE_WIDE$LONG2 == 0,
                        ifelse(is.na(ISSETPROFILE_WIDE$LONG3) | ISSETPROFILE_WIDE$LONG3 == 0,
                               ISSETPROFILE_WIDE$LONG4, ISSETPROFILE_WIDE$LONG3),
                        ISSETPROFILE_WIDE$LONG2),
                 ISSETPROFILE_WIDE$LONG1)
        
        cat("\nISSETPROFILE_WIDE:  For convenience, added LONGITUDE and LATITUDE fields from first non-NA value from p1-p4 positions")
      }
      if (!'YEAR' %in% colnames(ISSETPROFILE_WIDE)){
        ISSETPROFILE_WIDE$YEAR  =
          lubridate::year(as.POSIXct(ifelse(lubridate::year(ISSETPROFILE_WIDE$DATE_TIME1)>2500,
                                            ifelse(lubridate::year(ISSETPROFILE_WIDE$DATE_TIME2)>2500,
                                                   ifelse(lubridate::year(ISSETPROFILE_WIDE$DATE_TIME3)>2500,
                                                          ISSETPROFILE_WIDE$DATE_TIME4, ISSETPROFILE_WIDE$DATE_TIME3), ISSETPROFILE_WIDE$DATE_TIME2), ISSETPROFILE_WIDE$DATE_TIME1), origin = "1970-01-01"))
        cat("\nISSETPROFILE_WIDE:  For convenience, added YEAR fields from first non-NA value from p1-p4 positions")
      }
      save( ISSETPROFILE_WIDE, file=file.path(data.dir, "ISDB.ISSETPROFILE_WIDE.RData"), compress=TRUE)
      rm(ISSETPROFILE_WIDE)
    }
  }
  if (db %in% c("ALL","rv")){
    # GROUNDFISH ----------------------------------------------------------------------------------------------------------------------------------------------
    #'the following are special data handling processes specific to the GROUNDFISH tables (beyond
    #'getting the whole table)
    if (file.exists(file.path(data.dir,"GROUNDFISH.GSCAT.RData"))){
      load(file.path(data.dir,"GROUNDFISH.GSCAT.RData"))
      if ('WEIGHT_TYPE' %in% colnames(GSCAT)){
        GSCAT[is.na(GSCAT$TOTNO),"TOTNO"]<-0
        GSCAT[is.na(GSCAT$TOTWGT),"TOTWGT"]<-0
        GSCAT[is.na(GSCAT$SAMPWGT),"SAMPWGT"]<-0
        
        GSCAT <- stats::aggregate(
          x = list(
            SAMPWGT = GSCAT$SAMPWGT,
            TOTWGT = GSCAT$TOTWGT,
            TOTNO = GSCAT$TOTNO),
          by = list(
            MISSION = GSCAT$MISSION,
            SETNO = GSCAT$SETNO,
            SPEC = GSCAT$SPEC),
          sum
        )
        
        cat(paste("\nGSCAT:  Combined numbers and weights for different size classes within a set..."))
        save(GSCAT, file=file.path(data.dir, "GROUNDFISH.GSCAT.RData"), compress=TRUE)
      }
      rm(GSCAT)
    }
    if (file.exists(file.path(data.dir,"GROUNDFISH.GSINF.RData"))){
      load(file.path(data.dir,"GROUNDFISH.GSINF.RData"))
      if (!'LATITUDE' %in% colnames(GSINF)){
        GSINF$LATITUDE = (as.numeric(substr(GSINF$SLAT,1,2))+(GSINF$SLAT - as.numeric(substr(GSINF$SLAT,1,2))*100)/60)
        GSINF$LONGITUDE = (as.numeric(substr(GSINF$SLONG,1,2))+(GSINF$SLONG - as.numeric(substr(GSINF$SLONG,1,2))*100)/60)*-1
        GSINF$ELATITUDE = (as.numeric(substr(GSINF$ELAT,1,2))+(GSINF$ELAT - as.numeric(substr(GSINF$ELAT,1,2))*100)/60)
        GSINF$ELONGITUDE = (as.numeric(substr(GSINF$ELONG,1,2))+(GSINF$ELONG - as.numeric(substr(GSINF$ELONG,1,2))*100)/60)*-1
        cat(paste("\nGSINF:  Converted DDMM coordinates to DDDD.DD ..."))
        save( GSINF, file=file.path(data.dir, "GROUNDFISH.GSINF.RData"), compress=TRUE)
      }
      rm(GSINF)
    }
    
  }
  if (db %in% c("ALL","marfis")){
    # MARFISSCI -----------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(data.dir,"MARFISSCI.HAIL_IN_CALLS.RData"))){
      load(file.path(data.dir,"MARFISSCI.HAIL_IN_CALLS.RData"))
      if ("CUSER" %in% names(HAIL_IN_CALLS)){
        HAIL_IN_CALLS$CUSER <- NULL
        HAIL_IN_CALLS$CDATE <- NULL
        HAIL_IN_CALLS$UUSER <- NULL
        HAIL_IN_CALLS$UDATE <- NULL
        save(HAIL_IN_CALLS, file=file.path(data.dir, "MARFISSCI.HAIL_IN_CALLS.RData"), compress=TRUE)
      }
      rm(HAIL_IN_CALLS)
    }
    if (file.exists(file.path(data.dir,"MARFISSCI.LOG_SPC_STD_INFO.RData"))){
      load(file.path(data.dir,"MARFISSCI.LOG_SPC_STD_INFO.RData"))
      if ("CUSER" %in% names(LOG_SPC_STD_INFO)){
        LOG_SPC_STD_INFO$CUSER <- NULL
        LOG_SPC_STD_INFO$CDATE <- NULL
        LOG_SPC_STD_INFO$UUSER <- NULL
        LOG_SPC_STD_INFO$UDATE <- NULL
        save(LOG_SPC_STD_INFO, file=file.path(data.dir, "MARFISSCI.LOG_SPC_STD_INFO.RData"), compress=TRUE)
      }
      rm(LOG_SPC_STD_INFO)
    }
    if (file.exists(file.path(data.dir,"MARFISSCI.MON_DOCS.RData"))){
      load(file.path(data.dir,"MARFISSCI.MON_DOCS.RData"))
      if ("CUSER" %in% names(MON_DOCS)){
        MON_DOCS$CUSER <- NULL
        MON_DOCS$CDATE <- NULL
        MON_DOCS$UUSER <- NULL
        MON_DOCS$UDATE <- NULL
        save(MON_DOCS, file=file.path(data.dir, "MARFISSCI.MON_DOCS.RData"), compress=TRUE)
      }
      rm(MON_DOCS)
    }
    if (file.exists(file.path(data.dir,"MARFISSCI.PRO_SPC_INFO.RData"))){
      load(file.path(data.dir,"MARFISSCI.PRO_SPC_INFO.RData"))
      if (!"YEAR" %in% names(PRO_SPC_INFO)){
        PRO_SPC_INFO$FV_GEAR_CODE <-NULL
        PRO_SPC_INFO$SSF_SPECIES_CODE <-NULL
        PRO_SPC_INFO$SSF_SPECIES_SIZE_CODE <-NULL
        PRO_SPC_INFO$SSF_LANDED_FORM_CODE <-NULL
        PRO_SPC_INFO$CDATE <- NULL
        
        PRO_SPC_INFO$LATITUDE[!is.na(PRO_SPC_INFO$LATITUDE)] =
          as.numeric(substr(PRO_SPC_INFO$LATITUDE[!is.na(PRO_SPC_INFO$LATITUDE)], 1, 2)) +
          as.numeric(substr(PRO_SPC_INFO$LATITUDE[!is.na(PRO_SPC_INFO$LATITUDE)], 3, 4)) / 60 +
          as.numeric(substr(PRO_SPC_INFO$LATITUDE[!is.na(PRO_SPC_INFO$LATITUDE)], 5, 6)) / 3600
        PRO_SPC_INFO$LONGITUDE[!is.na(PRO_SPC_INFO$LONGITUDE)] = -1 *
          (as.numeric(substr(PRO_SPC_INFO$LONGITUDE[!is.na(PRO_SPC_INFO$LONGITUDE)], 1, 2)) +
             as.numeric(substr(PRO_SPC_INFO$LONGITUDE[!is.na(PRO_SPC_INFO$LONGITUDE)], 3, 4)) / 60 +
             as.numeric(substr(PRO_SPC_INFO$LONGITUDE[!is.na(PRO_SPC_INFO$LONGITUDE)], 5, 6)) / 3600)
        cat(paste("\nPRO_SPC_INFO:  Converted DDMM coordinates to DDDD.DD and added default coord fields..."))
        PRO_SPC_INFO$YEAR <- NA
        PRO_SPC_INFO$YEAR[!is.na(PRO_SPC_INFO$DATE_FISHED)]  =
          lubridate::year(as.POSIXct(PRO_SPC_INFO$DATE_FISHED[!is.na(PRO_SPC_INFO$DATE_FISHED)], origin = "1970-01-01"))
        cat("\nPRO_SPC_INFO: Added a year field")
        PRO_SPC_INFO$YEAR_LANDED <- NA
        PRO_SPC_INFO$YEAR_LANDED[!is.na(PRO_SPC_INFO$LANDED_DATE)]  =
          lubridate::year(as.POSIXct(PRO_SPC_INFO$LANDED_DATE[!is.na(PRO_SPC_INFO$LANDED_DATE)], origin = "1970-01-01"))
        cat("\nPRO_SPC_INFO: Added a year field")
        PRO_SPC_INFO$YEAR[PRO_SPC_INFO$SPECIES_CODE==700] = lubridate::year(as.POSIXct(PRO_SPC_INFO$LANDED_DATE[PRO_SPC_INFO$SPECIES_CODE==700], origin = "1970-01-01"))
        PRO_SPC_INFO$YEAR_LANDED[PRO_SPC_INFO$SPECIES_CODE==700] = PRO_SPC_INFO$YEAR[PRO_SPC_INFO$SPECIES_CODE==700]
        cat("\nPRO_SPC_INFO: Ensured correct year for lobster data (i.e. LANDED_DATE)")
        save( PRO_SPC_INFO, file=file.path(data.dir, "MARFISSCI.PRO_SPC_INFO.RData"), compress=TRUE)
      }
      rm(PRO_SPC_INFO)
    }
    if (file.exists(file.path(data.dir,"MARFISSCI.GEARS.RData"))){
      load(file.path(data.dir,"MARFISSCI.GEARS.RData"))
      if (!"GEAR_DESC" %in% names(GEARS)) names(GEARS)[names(GEARS) == "DESC_ENG"] <- "GEAR_DESC"
      if ("CUSER" %in% names(GEARS)){
        GEARS$DESC_FRE <- NULL
        GEARS$CUSER <- NULL
        GEARS$CDATE <- NULL
        GEARS$UUSER <- NULL
        GEARS$UDATE <- NULL
      }
      save(GEARS, file=file.path(data.dir, "MARFISSCI.GEARS.RData"), compress=TRUE)
      rm(GEARS)
    }
    if (file.exists(file.path(data.dir,"MARFISSCI.SPECIES.RData"))){
      load(file.path(data.dir,"MARFISSCI.SPECIES.RData"))
      if (!"SPECIES_NAME" %in% names(SPECIES)) names(SPECIES)[names(SPECIES) == "DESC_ENG"] <- "SPECIES_NAME"
      if (!"SPECIES_ABBREV" %in% names(SPECIES)) names(SPECIES)[names(SPECIES) == "SPECIES_ABBREV_ENG"] <- "SPECIES_ABBREV"
      if (!"LICENCE_DESC" %in% names(SPECIES)) names(SPECIES)[names(SPECIES) == "LICENCE_DESC_ENG"] <- "LICENCE_DESC"
      if ("CUSER" %in% names(SPECIES)){
        SPECIES$DESC_FRE <- NULL
        SPECIES$LICENCE_DESC_FRE <- NULL
        SPECIES$SPECIES_ABBREV_FRE <- NULL
        SPECIES$CUSER <- NULL
        SPECIES$CDATE <- NULL
        SPECIES$UUSER <- NULL
        SPECIES$UDATE <- NULL
      }
      save(SPECIES, file=file.path(data.dir, "MARFISSCI.SPECIES.RData"), compress=TRUE)
      rm(SPECIES)
    }
    if (file.exists(file.path(data.dir,"MARFISSCI.CATCH_USAGES.RData"))){
      load(file.path(data.dir,"MARFISSCI.CATCH_USAGES.RData"))
      if (!"CATCH_USAGE" %in% names(CATCH_USAGES)) names(CATCH_USAGES)[names(CATCH_USAGES) == "DESC_ENG"] <- "CATCH_USAGE"
      if ("CUSER" %in% names(CATCH_USAGES)){
        CATCH_USAGES$DESC_FRE <- NULL
        CATCH_USAGES$ABBREV_FRE <- NULL
        CATCH_USAGES$ABBREV_ENG <- NULL
        CATCH_USAGES$CUSER <- NULL
        CATCH_USAGES$CDATE <- NULL
        CATCH_USAGES$UUSER <- NULL
        CATCH_USAGES$UDATE <- NULL
      }
      save(CATCH_USAGES, file=file.path(data.dir, "MARFISSCI.CATCH_USAGES.RData"), compress=TRUE)
      rm(CATCH_USAGES)
    }
    if (file.exists(file.path(data.dir,"MARFISSCI.SPECIES_CATEGORIES.RData"))){
      load(file.path(data.dir,"MARFISSCI.SPECIES_CATEGORIES.RData"))
      if (!"SPECIES_CATEGORY" %in% names(SPECIES_CATEGORIES)) names(SPECIES_CATEGORIES)[names(SPECIES_CATEGORIES) == "DESC_ENG"] <- "SPECIES_CATEGORY"
      save(SPECIES_CATEGORIES, file=file.path(data.dir, "MARFISSCI.SPECIES_CATEGORIES.RData"), compress=TRUE)
      rm(SPECIES_CATEGORIES)
    }
    if (file.exists(file.path(data.dir,"MARFISSCI.AREAS.RData"))){
      load(file.path(data.dir,"MARFISSCI.AREAS.RData"))
      if (!"FISHING_AREA" %in% names(AREAS)) names(AREAS)[names(AREAS) == "DESC_ENG"] <- "FISHING_AREA"
      if ("CUSER" %in% names(AREAS)){
        AREAS$CUSER <- NULL
        AREAS$CDATE <- NULL
        AREAS$UUSER <- NULL
        AREAS$UDATE <- NULL
      }
      save(AREAS, file=file.path(data.dir, "MARFISSCI.AREAS.RData"), compress=TRUE)
      rm(AREAS)
    }
    if (file.exists(file.path(data.dir,"MARFISSCI.NAFO_UNIT_AREAS.RData"))){
      load(file.path(data.dir,"MARFISSCI.NAFO_UNIT_AREAS.RData"))
      if (!"NAFO_AREA" %in% names(NAFO_UNIT_AREAS)) names(NAFO_UNIT_AREAS)[names(NAFO_UNIT_AREAS) == "AREA"] <- "NAFO_AREA"
      if ("CUSER" %in% names(NAFO_UNIT_AREAS)){
        NAFO_UNIT_AREAS$CUSER <- NULL
        NAFO_UNIT_AREAS$CDATE <- NULL
        NAFO_UNIT_AREAS$UUSER <- NULL
        NAFO_UNIT_AREAS$UDATE <- NULL
      }
      save(NAFO_UNIT_AREAS, file=file.path(data.dir, "MARFISSCI.NAFO_UNIT_AREAS.RData"), compress=TRUE)
      rm(NAFO_UNIT_AREAS)
    }
    if (file.exists(file.path(data.dir,"MARFISSCI.LOG_EFRT_STD_INFO.RData"))){
      load(file.path(data.dir,"MARFISSCI.LOG_EFRT_STD_INFO.RData"))
      if (!"LATITUDE_EFRT" %in% names(LOG_EFRT_STD_INFO)){
        LOG_EFRT_STD_INFO$CUSER <- NULL
        LOG_EFRT_STD_INFO$CDATE <- NULL
        LOG_EFRT_STD_INFO$UUSER <- NULL
        LOG_EFRT_STD_INFO$UDATE <- NULL
        LOG_EFRT_STD_INFO$ENT_LATITUDE[!is.na(LOG_EFRT_STD_INFO$ENT_LATITUDE)] =
          as.numeric(substr(LOG_EFRT_STD_INFO$ENT_LATITUDE[!is.na(LOG_EFRT_STD_INFO$ENT_LATITUDE)], 1, 2)) +
          as.numeric(substr(LOG_EFRT_STD_INFO$ENT_LATITUDE[!is.na(LOG_EFRT_STD_INFO$ENT_LATITUDE)], 3, 4)) / 60 +
          as.numeric(substr(LOG_EFRT_STD_INFO$ENT_LATITUDE[!is.na(LOG_EFRT_STD_INFO$ENT_LATITUDE)], 5, 6)) / 3600
        LOG_EFRT_STD_INFO$DET_LATITUDE[!is.na(LOG_EFRT_STD_INFO$DET_LATITUDE)] =
          as.numeric(substr(LOG_EFRT_STD_INFO$DET_LATITUDE[!is.na(LOG_EFRT_STD_INFO$DET_LATITUDE)], 1, 2)) +
          as.numeric(substr(LOG_EFRT_STD_INFO$DET_LATITUDE[!is.na(LOG_EFRT_STD_INFO$DET_LATITUDE)], 3, 4)) / 60 +
          as.numeric(substr(LOG_EFRT_STD_INFO$DET_LATITUDE[!is.na(LOG_EFRT_STD_INFO$DET_LATITUDE)], 5, 6)) / 3600
        LOG_EFRT_STD_INFO$ENT_LONGITUDE[!is.na(LOG_EFRT_STD_INFO$ENT_LONGITUDE)] = -1 *
          (as.numeric(substr(LOG_EFRT_STD_INFO$ENT_LONGITUDE[!is.na(LOG_EFRT_STD_INFO$ENT_LONGITUDE)], 1, 2)) +
             as.numeric(substr(LOG_EFRT_STD_INFO$ENT_LONGITUDE[!is.na(LOG_EFRT_STD_INFO$ENT_LONGITUDE)], 3, 4)) / 60 +
             as.numeric(substr(LOG_EFRT_STD_INFO$ENT_LONGITUDE[!is.na(LOG_EFRT_STD_INFO$ENT_LONGITUDE)], 5, 6)) / 3600)
        LOG_EFRT_STD_INFO$DET_LONGITUDE[!is.na(LOG_EFRT_STD_INFO$DET_LONGITUDE)] = -1 *
          (as.numeric(substr(LOG_EFRT_STD_INFO$DET_LONGITUDE[!is.na(LOG_EFRT_STD_INFO$DET_LONGITUDE)], 1, 2)) +
             as.numeric(substr(LOG_EFRT_STD_INFO$DET_LONGITUDE[!is.na(LOG_EFRT_STD_INFO$DET_LONGITUDE)], 3, 4)) / 60 +
             as.numeric(substr(LOG_EFRT_STD_INFO$DET_LONGITUDE[!is.na(LOG_EFRT_STD_INFO$DET_LONGITUDE)], 5, 6)) / 3600)
        LOG_EFRT_STD_INFO$LATITUDE_EFRT = ifelse(is.na(LOG_EFRT_STD_INFO$ENT_LATITUDE),LOG_EFRT_STD_INFO$DET_LATITUDE,LOG_EFRT_STD_INFO$ENT_LATITUDE)
        LOG_EFRT_STD_INFO$LONGITUDE_EFRT = ifelse(is.na(LOG_EFRT_STD_INFO$ENT_LONGITUDE),LOG_EFRT_STD_INFO$DET_LONGITUDE,LOG_EFRT_STD_INFO$ENT_LONGITUDE)
        cat(paste("\nLOG_EFRT_STD_INFO:  Converted DDMM coordinates to DDDD.DD and added coord fields..."))
        save( LOG_EFRT_STD_INFO, file=file.path(data.dir, "MARFISSCI.LOG_EFRT_STD_INFO.RData"), compress=TRUE)
      }
      rm(LOG_EFRT_STD_INFO)
    }
    
  }
  if (db %in% c("ALL","usnefsc")){
    # USNEFSC -------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(data.dir,"USNEFSC.USS_STATION.RData"))){
      load(file.path(data.dir,"USNEFSC.USS_STATION.RData"))
      if (!'LATITUDE' %in% colnames(USS_STATION)){
        USS_STATION$LATITUDE = USS_STATION$DECDEG_BEGLAT
        USS_STATION$LONGITUDE = USS_STATION$DECDEG_BEGLON
        USS_STATION$ELATITUDE = USS_STATION$DECDEG_ENDLAT
        USS_STATION$ELONGITUDE = USS_STATION$DECDEG_ENDLON
        cat("\nUSS_STATION:  For convenience, added LATITUDE, LONGITUDE, ELATITUDE AND ELONGITUDE fields")
        save(USS_STATION, file=file.path(data.dir, "USNEFSC.USS_STATION.RData"), compress=TRUE)
      }
      rm(USS_STATION)
    }
    
  }
  if (db %in% c("ALL","stomach")){
    # Stomach -------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(data.dir,"STOMACH.SDINF.RData"))){
      load(file.path(data.dir,"STOMACH.SDINF.RData"))
      if (!'YEAR' %in% colnames(SDINF)){
        SDINF$YEAR = lubridate::year(SDINF$SDATE)
        save(SDINF, file=file.path(data.dir, "STOMACH.SDINF.RData"), compress=TRUE)
        cat("\nSDINF:  For convenience, added a YEAR field")
      }
      rm(SDINF)
    }
  }
  if (db %in% c("ALL","juvesh")){
    # JUVESH --------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(data.dir,"JUVESH.JVINF.RData"))){
    load(file.path(data.dir,"JUVESH.JVINF.RData"))
    if (!'YEAR' %in% colnames(JVINF)){
      JVINF$YEAR = lubridate::year(JVINF$SDATE)
      cat("\nJVINF:  For convenience, added a YEAR field")
      JVINF$LATITUDE = (as.numeric(substr(JVINF$SLAT,1,2))+(JVINF$SLAT - as.numeric(substr(JVINF$SLAT,1,2))*100)/60)
      JVINF$LONGITUDE = (as.numeric(substr(JVINF$SLONG,1,2))+(JVINF$SLONG - as.numeric(substr(JVINF$SLONG,1,2))*100)/60)*-1
      JVINF$ELATITUDE = (as.numeric(substr(JVINF$ELAT,1,2))+(JVINF$ELAT - as.numeric(substr(JVINF$ELAT,1,2))*100)/60)
      JVINF$ELONGITUDE = (as.numeric(substr(JVINF$ELONG,1,2))+(JVINF$ELONG - as.numeric(substr(JVINF$ELONG,1,2))*100)/60)*-1
      cat(paste("\nJVINF:  Converted DDMM coordinates to DDDD.DD ..."))
      #sometimes the CRUNOs had leading 0s, and in the case of JVCAT, it sometimes has leading Os.  The following
      #removes these so that data can be related properly without orphaning records.
      
      JVINF[substr(JVINF$CRUNO,1,1)=="O","CRUNO"]<- substring(JVINF[substr(JVINF$CRUNO,1,1)=="O","CRUNO"],2)
      JVINF[grepl("[[:alpha:]]", JVINF$CRUNO)==F,"CRUNO"]<-as.integer(JVINF[grepl("[[:alpha:]]", JVINF$CRUNO)==F,"CRUNO"])
      #discovered 2 setno "70"s for a cruise N-11, but when looking at time of the sets, one of them occurred 
      #right when setno "30" should have happened, but was missing.  That missing set had catch records 
      #which would have been orphaned.
      #The code below makes this correction
      JVINF[JVINF$VESEL=="N" & JVINF$CRUNO=="11" & JVINF$SETNO=="70" & JVINF$SDATE == "1983-06-24","SETNO"]<-30
      save(JVINF, file=file.path(data.dir, "JUVESH.JVINF.RData"), compress=TRUE)
      cat("\nJVINF:  For convenience, removed leading zeroes from CRUNO")
    }
    rm(JVINF)
    }
    if (file.exists(file.path(data.dir,"JUVESH.JVCAT.RData"))){
    load(file.path(data.dir,"JUVESH.JVCAT.RData"))
    if(length(JVCAT[substr(JVCAT$CRUNO,1,1)=="O","CRUNO"])>0){
      JVCAT[substr(JVCAT$CRUNO,1,1)=="O","CRUNO"]<- substring(JVCAT[substr(JVCAT$CRUNO,1,1)=="O","CRUNO"],2)
      JVCAT[grepl("[[:alpha:]]", JVCAT$CRUNO)==F,"CRUNO"]<-as.integer(JVCAT[grepl("[[:alpha:]]", JVCAT$CRUNO)==F,"CRUNO"])
      save(JVCAT, file=file.path(data.dir, "JUVESH.JVCAT.RData"), compress=TRUE)
      cat("\nJVCAT:  For convenience, removed leading zeroes from CRUNO")
    }
    rm(JVCAT)
    }
    
    if (file.exists(file.path(data.dir,"JUVESH.JVDET.RData"))){
    load(file.path(data.dir,"JUVESH.JVDET.RData"))
    if(length(JVDET[substr(JVDET$CRUNO,1,1)=="O","CRUNO"])>0){
      JVDET[substr(JVDET$CRUNO,1,1)=="O","CRUNO"]<- substring(JVDET[substr(JVDET$CRUNO,1,1)=="O","CRUNO"],2)
      JVDET[grepl("[[:alpha:]]", JVDET$CRUNO)==F,"CRUNO"]<-as.integer(JVDET[grepl("[[:alpha:]]", JVDET$CRUNO)==F,"CRUNO"])
      save(JVDET, file=file.path(data.dir, "JUVESH.JVDET.RData"), compress=TRUE)
      cat("\nJVDET:  For convenience, removed leading zeroes from CRUNO")
    }
    rm(JVDET)
    }
    
    
  }
  if (db %in% c("ALL","rvp70")){
    # Pre 1970s GROUNDFISH ------------------------------------------------------------------------------------------------------------------------------------
    
    if (file.exists(file.path(data.dir,"RVP70.GSINFP70.RData"))){
    load(file.path(data.dir,"RVP70.GSINFP70.RData"))
    if (!'LATITUDE' %in% colnames(GSINFP70)){
      GSINFP70$LATITUDE = (as.numeric(substr(GSINFP70$SLAT,1,2))+(GSINFP70$SLAT - as.numeric(substr(GSINFP70$SLAT,1,2))*100)/60)
      GSINFP70$LONGITUDE = (as.numeric(substr(GSINFP70$SLONG,1,2))+(GSINFP70$SLONG - as.numeric(substr(GSINFP70$SLONG,1,2))*100)/60)*-1
      GSINFP70$ELATITUDE = (as.numeric(substr(GSINFP70$ELAT,1,2))+(GSINFP70$ELAT - as.numeric(substr(GSINFP70$ELAT,1,2))*100)/60)
      GSINFP70$ELONGITUDE = (as.numeric(substr(GSINFP70$ELONG,1,2))+(GSINFP70$ELONG - as.numeric(substr(GSINFP70$ELONG,1,2))*100)/60)*-1
      cat(paste("\nGSINFP70:  Converted DDMM coordinates to DDDD.DD ..."))
      save( GSINFP70, file=file.path(data.dir, "RVP70.GSINFP70.RData"), compress=TRUE)
    }
    rm(GSINFP70)
    }
  }
  if (db %in% c("ALL","chid")){
    # Chidley -------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(data.dir,"CHID.DSINF.RData"))){
    load(file.path(data.dir,"CHID.DSINF.RData"))
    if (!'LATITUDE' %in% colnames(DSINF)){
      DSINF$LATITUDE = (as.numeric(substr(DSINF$SLAT,1,2))+(DSINF$SLAT - as.numeric(substr(DSINF$SLAT,1,2))*100)/60)
      DSINF$LONGITUDE = (as.numeric(substr(DSINF$SLONG,1,2))+(DSINF$SLONG - as.numeric(substr(DSINF$SLONG,1,2))*100)/60)*-1
      DSINF$ELATITUDE = (as.numeric(substr(DSINF$ELAT,1,2))+(DSINF$ELAT - as.numeric(substr(DSINF$ELAT,1,2))*100)/60)
      DSINF$ELONGITUDE = (as.numeric(substr(DSINF$ELONG,1,2))+(DSINF$ELONG - as.numeric(substr(DSINF$ELONG,1,2))*100)/60)*-1
      cat(paste("\nDSINF:  Converted DDMM coordinates to DDDD.DD ..."))
      DSINF$YEAR = lubridate::year(DSINF$SDATE)
      cat("\nDSINF: Added a year field")
      months <- strptime(DSINF$SDATE, format='%Y-%m-%d %H:%M')$mon +1
      indx <- stats::setNames( rep(c('Winter', 'Spring', 'Summer', 'Fall'),each=3), c(12,1:11))
      DSINF$SEASON <- unname(indx[as.character(months)])
      
      cat("\nDSINF: Added a season field")
      
      save( DSINF, file=file.path(data.dir, "CHID.DSINF.RData"), compress=TRUE)
    }
    rm(DSINF)
    }
  }
  if (db %in% c("ALL","meso")){
    # MESO ----------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(data.dir,"MESO.MESOPELAGIC.RData"))){
    load(file.path(data.dir,"MESO.MESOPELAGIC.RData"))
    if (!'LATITUDE' %in% colnames(MESOPELAGIC)){
      MESOPELAGIC$theLat = paste0(sprintf("%02d",MESOPELAGIC$LAT_DEG),sprintf("%02d",MESOPELAGIC$LAT_MIN))  
      MESOPELAGIC$theLong = paste0(sprintf("%02d",MESOPELAGIC$LON_DEG),sprintf("%02d",MESOPELAGIC$LON_MIN))  
      MESOPELAGIC$LATITUDE = (as.numeric(substr(MESOPELAGIC$theLat,1,2))+(as.numeric(MESOPELAGIC$theLat) - as.numeric(substr(MESOPELAGIC$theLat,1,2))*100)/60)
      MESOPELAGIC$LONGITUDE = (as.numeric(substr(MESOPELAGIC$theLong,1,2))+(as.numeric(MESOPELAGIC$theLong) - as.numeric(substr(MESOPELAGIC$theLong,1,2))*100)/60)*-1
      
      MESOPELAGIC$theLat<-NULL
      MESOPELAGIC$theLong<-NULL
      MESOPELAGIC$LAT_DEG<-NULL
      MESOPELAGIC$LAT_MIN<-NULL
      MESOPELAGIC$LON_DEG<-NULL
      MESOPELAGIC$LON_MIN<-NULL
      cat(paste("\nMESOPELAGIC:  Converted DDMM coordinates to DDDD.DD ..."))
      save( MESOPELAGIC, file=file.path(data.dir, "MESO.MESOPELAGIC.RData"), compress=TRUE)
    }
    rm(MESOPELAGIC)
    }
  }
  if (db %in% c("ALL","meso_gully")){
    # MESO_GULLY ----------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(data.dir,"MESO_GULLY.GSINF.RData"))){
    load(file.path(data.dir,"MESO_GULLY.GSINF.RData"))
    if (!'LATITUDE' %in% colnames(GSINF)){
      GSINF$LATITUDE = (as.numeric(substr(GSINF$SLAT,1,2))+(GSINF$SLAT - as.numeric(substr(GSINF$SLAT,1,2))*100)/60)
      GSINF$LONGITUDE = (as.numeric(substr(GSINF$SLONG,1,2))+(GSINF$SLONG - as.numeric(substr(GSINF$SLONG,1,2))*100)/60)*-1
      GSINF$ELATITUDE = (as.numeric(substr(GSINF$ELAT,1,2))+(GSINF$ELAT - as.numeric(substr(GSINF$ELAT,1,2))*100)/60)
      GSINF$ELONGITUDE = (as.numeric(substr(GSINF$ELONG,1,2))+(GSINF$ELONG - as.numeric(substr(GSINF$ELONG,1,2))*100)/60)*-1
      
      GSINF$SLAT<-NULL
      GSINF$SLONG<-NULL
      GSINF$ELAT<-NULL
      GSINF$ELONG<-NULL
      cat(paste("\nGSINF:  Converted DDMM coordinates to DDDD.DD ..."))
      save( GSINF, file=file.path(data.dir, "MESO_GULLY.GSINF.RData"), compress=TRUE)
    }
    rm(GSINF)
    }
  }
  if (db %in% c("ALL","inshore")){
    # INSHORE -------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(data.dir,"INSHORE.INS_INF.RData"))){
    load(file.path(data.dir,"INSHORE.INS_INF.RData"))
    if (!'LATITUDE' %in% colnames(INS_INF)){
      INS_INF$LATITUDE = (as.numeric(substr(INS_INF$SLATDDMM,1,2))+(INS_INF$SLATDDMM - as.numeric(substr(INS_INF$SLATDDMM,1,2))*100)/60)
      INS_INF$LONGITUDE = (as.numeric(substr(INS_INF$SLONGDDMM,1,2))+(INS_INF$SLONGDDMM - as.numeric(substr(INS_INF$SLONGDDMM,1,2))*100)/60)*-1
      INS_INF$ELATITUDE = (as.numeric(substr(INS_INF$ELATDDMM,1,2))+(INS_INF$ELATDDMM - as.numeric(substr(INS_INF$ELATDDMM,1,2))*100)/60)
      INS_INF$ELONGITUDE = (as.numeric(substr(INS_INF$ELONGDDMM,1,2))+(INS_INF$ELONGDDMM - as.numeric(substr(INS_INF$ELONGDDMM,1,2))*100)/60)*-1
      #remove the many coord fields
      INS_INF$BLATDEG<-NULL
      INS_INF$BLATMIN<-NULL
      INS_INF$BLONGDEG<-NULL
      INS_INF$BLONGMIN<-NULL
      
      INS_INF$ELATDEG<-NULL
      INS_INF$ELATMIN<-NULL
      INS_INF$ELONGDEG<-NULL
      INS_INF$ELONGMIN<-NULL
      
      INS_INF$SLATDDMM<-NULL
      INS_INF$SLONGDDMM<-NULL
      INS_INF$ELATDDMM<-NULL
      INS_INF$ELONGDDMM<-NULL
      cat(paste("\nINS_INF:  Converted DDMM coordinates to DDDD.DD ..."))
      save( INS_INF, file=file.path(data.dir, "INSHORE.INS_INF.RData"), compress=TRUE)
    }
    rm(INS_INF)
    }
  }
  if (db %in% c("ALL","redfish")){
    # REDFISH -------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(data.dir,"REDFISH.RFINF.RData"))){
    load(file.path(data.dir,"REDFISH.RFINF.RData"))
    if (!'LATITUDE' %in% colnames(RFINF)){
      RFINF$LATITUDE = (as.numeric(substr(RFINF$SLAT,1,2))+(RFINF$SLAT - as.numeric(substr(RFINF$SLAT,1,2))*100)/60)
      RFINF$LONGITUDE = (as.numeric(substr(RFINF$SLONG,1,2))+(RFINF$SLONG - as.numeric(substr(RFINF$SLONG,1,2))*100)/60)*-1
      RFINF$ELATITUDE = (as.numeric(substr(RFINF$ELAT,1,2))+(RFINF$ELAT - as.numeric(substr(RFINF$ELAT,1,2))*100)/60)
      RFINF$ELONGITUDE = (as.numeric(substr(RFINF$ELONG,1,2))+(RFINF$ELONG - as.numeric(substr(RFINF$ELONG,1,2))*100)/60)*-1
      cat(paste("\nRFINF:  Converted DDMM coordinates to DDDD.DD ..."))
      
      RFINF$YEAR = lubridate::year(RFINF$SDATE)
      cat("\nRFINF: Added a year field")
      
      months <- strptime(RFINF$SDATE, format='%Y-%m-%d %H:%M')$mon +1
      indx <- stats::setNames( rep(c('Winter', 'Spring', 'Summer', 'Fall'),each=3), c(12,1:11))
      RFINF$SEASON <- unname(indx[as.character(months)])
      
      cat("\nRFINF: Added a season field")
      save( RFINF, file=file.path(data.dir, "REDFISH.RFINF.RData"), compress=TRUE)
    }
    rm(RFINF)
    }
  }
  if (db %in% c("ALL","comland86")){
    # COMLAND86 -----------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(data.dir,"COMLAND86.PROVINCES.RData"))){
    load(file.path(data.dir,"COMLAND86.PROVINCES.RData"))
    if(class(PROVINCES$PROV_CODE)=="integer"){
      PROVINCES$PROV_CODE <- as.character(PROVINCES$PROV_CODE)
      cat("\nPROVINCES: Changed provinces codes to characters so they can be used in filtering")
      save( PROVINCES, file=file.path(data.dir, "COMLAND86.PROVINCES.RData"), compress=TRUE)
    }
    rm(PROVINCES)
    }
  }
  if (db %in% c("ALL","comland67")){
    # COMLAND67 -----------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(data.dir,"COMLAND67.PROVINCES.RData"))){
    load(file.path(data.dir,"COMLAND67.PROVINCES.RData"))
    if(class(PROVINCES$PROV_CODE)=="integer"){
      PROVINCES$PROV_CODE <- as.character(PROVINCES$PROV_CODE)
      cat("\nPROVINCES: Changed provinces codes to characters so they can be used in filtering")
      save( PROVINCES, file=file.path(data.dir, "COMLAND67.PROVINCES.RData"), compress=TRUE)
    }
    rm(PROVINCES)
    }
  }
  if (db %in% c("ALL","asef")){
    # ASEF ----------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(data.dir,"ASEF.TRINFO.RData"))){
    load(file.path(data.dir,"ASEF.TRINFO.RData"))
    if (!'RLYEAR' %in% colnames(TRINFO)){
      TRINFO$RLYEAR <- lubridate::year(TRINFO$RLDATE)
      cat("\nTRINFO: RLYEAR added so it can be used in filtering")
      save( TRINFO, file=file.path(data.dir, "ASEF.TRINFO.RData"), compress=TRUE)
    }
    rm(TRINFO)
    }
    if (file.exists(file.path(data.dir,"ASEF.RCSITE.RData"))){
    load(file.path(data.dir,"ASEF.RCSITE.RData"))
    if (!'LATITUDE' %in% colnames(RCSITE)){
      RCSITE$LATITUDE = (as.numeric(substr(RCSITE$SLAT,1,2))+(RCSITE$SLAT - as.numeric(substr(RCSITE$SLAT,1,2))*100)/60)
      RCSITE$LONGITUDE = (as.numeric(substr(RCSITE$SLONG,1,2))+(RCSITE$SLONG - as.numeric(substr(RCSITE$SLONG,1,2))*100)/60)*-1
      cat(paste("\nRCSITE:  Converted DDMM coordinates to DDDD.DD ..."))
      save( RCSITE, file=file.path(data.dir, "ASEF.RCSITE.RData"), compress=TRUE)
    } 
    rm(RCSITE)
    }
  }
  
}