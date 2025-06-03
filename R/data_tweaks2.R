#' @title data_tweaks2
#' @description After the initial extraction, this function is called to do minor housekeeping
#' to the extracted datasets to ensure they works with the other functions.
#' @param db default is \code{"ALL"}. This identifies the dataset you are working 
#' with.
#' @param extract_user default is \code{NULL}.  This parameter can be used with
#' \code{extract_computer} to load encypted data files extracted by another user 
#' and/or computer
#' @param extract_computer  default is \code{NULL}.  This parameter can be used with
#' \code{extract_user} to load encypted data files extracted by another user 
#' and/or computer
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' @note data is not added to the global environment by this function - changes are made, saved, and
#' dropped.
data_tweaks2 <- function(db="ALL", extract_user = NULL, extract_computer = NULL){
  cat("\nApplying tweaks v.2 ...")
  if (db %in% c("ALL","isdb")){
    # ISDB ----------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(get_pesd_dw_dir(),"ISCATCHES.RData")) & file.exists(file.path(get_pesd_dw_dir(),"ISFISHSETS.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"ISCATCHES.RData"), extract_user = extract_user, extract_computer = extract_computer)
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"ISFISHSETS.RData"), extract_user = extract_user, extract_computer = extract_computer)
      if (!'S_EST_NUM_CAUGHT' %in% colnames(ISCATCHES)){
        ISFISHSETS.directed=ISFISHSETS[c("FISHSET_ID","SPECSCD_ID")]  #keep only the field identifying the sought spp for each set
        ISCATCHES.directed = merge(ISCATCHES,ISFISHSETS.directed, by.x=c("FISHSET_ID","SPECCD_ID"), by.y=c("FISHSET_ID","SPECSCD_ID")) #get the catches of directed for each set
        ISCATCHES.directed = ISCATCHES.directed[c("FISHSET_ID", "SPECCD_ID", "EST_NUM_CAUGHT", "EST_KEPT_WT", "EST_DISCARD_WT", "EST_REDUCTION_WT", "EST_COMBINED_WT")] #keep some fields
        names(ISCATCHES.directed) <- c("FISHSET_ID", "SPECSCD_ID","S_EST_NUM_CAUGHT","S_EST_KEPT_WT","S_EST_DISCARD_WT","S_EST_REDUCTION_WT","S_EST_COMBINED_WT") #rename to reflect sought nature
        ISCATCHES = merge(ISCATCHES, ISCATCHES.directed, all.x=T, by.x=c("FISHSET_ID"), by.y=c("FISHSET_ID")) #get the catches of directed for each set
        
        Mar.utils::save_encrypted( ISCATCHES, file=file.path(get_pesd_dw_dir(), "ISCATCHES.RData"), compress=TRUE)
        cat("\nISCATCHES: Added directed species catch numbers and weights onto each record......")
      }
      rm(ISCATCHES)
      rm(ISFISHSETS)
    }
    if (file.exists(file.path(get_pesd_dw_dir(),"ISSETPROFILE.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"ISSETPROFILE.RData"), extract_user = extract_user, extract_computer = extract_computer)
      ISSETPROFILE <- Mar.utils::ISSETPROFILE_enwidener(ISSETPROFILE)
      Mar.utils::save_encrypted( ISSETPROFILE, file=file.path(get_pesd_dw_dir(), "ISSETPROFILE.RData"), compress=TRUE)
    rm(ISSETPROFILE)
    }
    
    if (file.exists(file.path(get_pesd_dw_dir(),"ISDB.ISCATCHES.RData")) & file.exists(file.path(get_pesd_dw_dir(),"ISDB.ISFISHSETS.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"ISDB.ISCATCHES.RData"), extract_user = extract_user, extract_computer = extract_computer)
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"ISDB.ISFISHSETS.RData"), extract_user = extract_user, extract_computer = extract_computer)
      if (!'S_EST_NUM_CAUGHT' %in% colnames(ISCATCHES)){
        ISFISHSETS.directed=ISFISHSETS[c("FISHSET_ID","SPECSCD_ID")]  #keep only the field identifying the sought spp for each set
        ISCATCHES.directed = merge(ISCATCHES,ISFISHSETS.directed, by.x=c("FISHSET_ID","SPECCD_ID"), by.y=c("FISHSET_ID","SPECSCD_ID")) #get the catches of directed for each set
        ISCATCHES.directed = ISCATCHES.directed[c("FISHSET_ID", "SPECCD_ID", "EST_NUM_CAUGHT", "EST_KEPT_WT", "EST_DISCARD_WT", "EST_REDUCTION_WT", "EST_COMBINED_WT")] #keep some fields
        names(ISCATCHES.directed) <- c("FISHSET_ID", "SPECSCD_ID","S_EST_NUM_CAUGHT","S_EST_KEPT_WT","S_EST_DISCARD_WT","S_EST_REDUCTION_WT","S_EST_COMBINED_WT") #rename to reflect sought nature
        ISCATCHES = merge(ISCATCHES, ISCATCHES.directed, all.x=T, by.x=c("FISHSET_ID"), by.y=c("FISHSET_ID")) #get the catches of directed for each set
        
        Mar.utils::save_encrypted( ISCATCHES, file=file.path(get_pesd_dw_dir(), "ISDB.ISCATCHES.RData"), compress=TRUE)
        cat("\nISCATCHES: Added directed species catch numbers and weights onto each record......")
      }
      rm(ISCATCHES)
      rm(ISFISHSETS)
    }
  }
  if (db %in% c("ALL","rv")){
    # GROUNDFISH ----------------------------------------------------------------------------------------------------------------------------------------------
    #'the following are special data handling processes specific to the GROUNDFISH tables (beyond
    #'getting the whole table)
    if (file.exists(file.path(get_pesd_dw_dir(),"GROUNDFISH.GSCAT.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"GROUNDFISH.GSCAT.RData"), extract_user = extract_user, extract_computer = extract_computer)
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
        save(GSCAT, file=file.path(get_pesd_dw_dir(), "GROUNDFISH.GSCAT.RData"), compress=TRUE)
      }
      rm(GSCAT)
    }
    if (file.exists(file.path(get_pesd_dw_dir(),"GROUNDFISH.GSINF.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"GROUNDFISH.GSINF.RData"), extract_user = extract_user, extract_computer = extract_computer)
      if (!'LATITUDE' %in% colnames(GSINF)){
        GSINF <- Mar.utils::DDMMx_to_DD(df=GSINF, format = "DDMMMM", lat.field = "SLAT", lon.field = "SLONG", WestHemisphere = T)
        colnames(GSINF)[colnames(GSINF)=="LAT_DD"] <- "LATITUDE"
        colnames(GSINF)[colnames(GSINF)=="LON_DD"] <- "LONGITUDE"
        GSINF <- Mar.utils::DDMMx_to_DD(df=GSINF, format = "DDMMMM", lat.field = "ELAT", lon.field = "ELONG", WestHemisphere = T)
        colnames(GSINF)[colnames(GSINF)=="LAT_DD"] <- "ELATITUDE"
        colnames(GSINF)[colnames(GSINF)=="LON_DD"] <- "ELONGITUDE"
        
        
        GSINF$SLAT <- GSINF$SLONG <- GSINF$ELAT <- GSINF$ELONG <- NULL
        
        cat(paste("\nGSINF:  Converted DDMM coordinates to DDDD.DD ..."))
        save( GSINF, file=file.path(get_pesd_dw_dir(), "GROUNDFISH.GSINF.RData"), compress=TRUE)
      }
      rm(GSINF)
    }
    
  }
  if (db %in% c("ALL","marfis")){
    # MARFISSCI -----------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(get_pesd_dw_dir(),"MARFISSCI.HAIL_IN_CALLS.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"MARFISSCI.HAIL_IN_CALLS.RData"), extract_user = extract_user, extract_computer = extract_computer)
      if ("CUSER" %in% names(HAIL_IN_CALLS)){
        HAIL_IN_CALLS$CUSER <- NULL
        HAIL_IN_CALLS$CDATE <- NULL
        HAIL_IN_CALLS$UUSER <- NULL
        HAIL_IN_CALLS$UDATE <- NULL
        Mar.utils::save_encrypted(HAIL_IN_CALLS, file=file.path(get_pesd_dw_dir(), "MARFISSCI.HAIL_IN_CALLS.RData"), compress=TRUE)
      }
      rm(HAIL_IN_CALLS)
    }
    if (file.exists(file.path(get_pesd_dw_dir(),"MARFISSCI.LOG_SPC_STD_INFO.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"MARFISSCI.LOG_SPC_STD_INFO.RData"), extract_user = extract_user, extract_computer = extract_computer)
      if ("CUSER" %in% names(LOG_SPC_STD_INFO)){
        LOG_SPC_STD_INFO$CUSER <- NULL
        LOG_SPC_STD_INFO$CDATE <- NULL
        LOG_SPC_STD_INFO$UUSER <- NULL
        LOG_SPC_STD_INFO$UDATE <- NULL
        Mar.utils::save_encrypted(LOG_SPC_STD_INFO, file=file.path(get_pesd_dw_dir(), "MARFISSCI.LOG_SPC_STD_INFO.RData"), compress=TRUE)
      }
      rm(LOG_SPC_STD_INFO)
    }
    if (file.exists(file.path(get_pesd_dw_dir(),"MARFISSCI.MON_DOCS.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"MARFISSCI.MON_DOCS.RData"), extract_user = extract_user, extract_computer = extract_computer)
      if ("CUSER" %in% names(MON_DOCS)){
        MON_DOCS$CUSER <- NULL
        MON_DOCS$CDATE <- NULL
        MON_DOCS$UUSER <- NULL
        MON_DOCS$UDATE <- NULL
        Mar.utils::save_encrypted(MON_DOCS, file=file.path(get_pesd_dw_dir(), "MARFISSCI.MON_DOCS.RData"), compress=TRUE)
      }
      rm(MON_DOCS)
    }
    if (file.exists(file.path(get_pesd_dw_dir(),"MARFISSCI.PRO_SPC_INFO.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"MARFISSCI.PRO_SPC_INFO.RData"), extract_user = extract_user, extract_computer = extract_computer)
      if (!"YEAR" %in% names(PRO_SPC_INFO)){
        PRO_SPC_INFO$FV_GEAR_CODE <-NULL
        PRO_SPC_INFO$SSF_SPECIES_CODE <-NULL
        PRO_SPC_INFO$SSF_SPECIES_SIZE_CODE <-NULL
        PRO_SPC_INFO$SSF_LANDED_FORM_CODE <-NULL
        PRO_SPC_INFO$CDATE <- NULL
        PRO_SPC_INFO <- Mar.utils::DDMMx_to_DD(df=PRO_SPC_INFO, format = "DDMMMM", lat.field = "LATITUDE", lon.field = "LONGITUDE", WestHemisphere = T)
        PRO_SPC_INFO$LATITUDE <- PRO_SPC_INFO$LONGITUDE <- NULL
        colnames(PRO_SPC_INFO)[colnames(PRO_SPC_INFO)=="LAT_DD"] <- "LATITUDE"
        colnames(PRO_SPC_INFO)[colnames(PRO_SPC_INFO)=="LON_DD"] <- "LONGITUDE"
        cat(paste("\nPRO_SPC_INFO:  Converted DDMMMM coordinates to DD.DDDDDD and added default coord fields..."))
        PRO_SPC_INFO$YEAR <- NA
        PRO_SPC_INFO$YEAR[!is.na(PRO_SPC_INFO$DATE_FISHED)]  =
          lubridate::year(as.POSIXct(PRO_SPC_INFO$DATE_FISHED[!is.na(PRO_SPC_INFO$DATE_FISHED)], origin = "1970-01-01"))
        PRO_SPC_INFO$YEAR_LANDED <- NA
        PRO_SPC_INFO$YEAR_LANDED[!is.na(PRO_SPC_INFO$LANDED_DATE)]  =
          lubridate::year(as.POSIXct(PRO_SPC_INFO$LANDED_DATE[!is.na(PRO_SPC_INFO$LANDED_DATE)], origin = "1970-01-01"))
        cat("\nPRO_SPC_INFO: Added a year field")
        PRO_SPC_INFO$YEAR[PRO_SPC_INFO$SPECIES_CODE==700] = lubridate::year(as.POSIXct(PRO_SPC_INFO$LANDED_DATE[PRO_SPC_INFO$SPECIES_CODE==700], origin = "1970-01-01"))
        PRO_SPC_INFO$YEAR_LANDED[PRO_SPC_INFO$SPECIES_CODE==700] = PRO_SPC_INFO$YEAR[PRO_SPC_INFO$SPECIES_CODE==700]
        cat("\nPRO_SPC_INFO: Ensured correct year for lobster data (i.e. LANDED_DATE)")
        Mar.utils::save_encrypted( PRO_SPC_INFO, file=file.path(get_pesd_dw_dir(), "MARFISSCI.PRO_SPC_INFO.RData"), compress=TRUE)
      }
      rm(PRO_SPC_INFO)
    }
    if (file.exists(file.path(get_pesd_dw_dir(),"MARFISSCI.GEARS.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"MARFISSCI.GEARS.RData"), extract_user = extract_user, extract_computer = extract_computer)
      if (!"GEAR_DESC" %in% names(GEARS)) names(GEARS)[names(GEARS) == "DESC_ENG"] <- "GEAR_DESC"
      if ("CUSER" %in% names(GEARS)){
        GEARS$DESC_FRE <- NULL
        GEARS$CUSER <- NULL
        GEARS$CDATE <- NULL
        GEARS$UUSER <- NULL
        GEARS$UDATE <- NULL
      }
      Mar.utils::save_encrypted(GEARS, file=file.path(get_pesd_dw_dir(), "MARFISSCI.GEARS.RData"), compress=TRUE)
      rm(GEARS)
    }
    if (file.exists(file.path(get_pesd_dw_dir(),"MARFISSCI.SPECIES.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"MARFISSCI.SPECIES.RData"), extract_user = extract_user, extract_computer = extract_computer)
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
      Mar.utils::save_encrypted(SPECIES, file=file.path(get_pesd_dw_dir(), "MARFISSCI.SPECIES.RData"), compress=TRUE)
      rm(SPECIES)
    }
    if (file.exists(file.path(get_pesd_dw_dir(),"MARFISSCI.CATCH_USAGES.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"MARFISSCI.CATCH_USAGES.RData"), extract_user = extract_user, extract_computer = extract_computer)
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
      Mar.utils::save_encrypted(CATCH_USAGES, file=file.path(get_pesd_dw_dir(), "MARFISSCI.CATCH_USAGES.RData"), compress=TRUE)
      rm(CATCH_USAGES)
    }
    if (file.exists(file.path(get_pesd_dw_dir(),"MARFISSCI.SPECIES_CATEGORIES.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"MARFISSCI.SPECIES_CATEGORIES.RData"), extract_user = extract_user, extract_computer = extract_computer)
      if (!"SPECIES_CATEGORY" %in% names(SPECIES_CATEGORIES)) names(SPECIES_CATEGORIES)[names(SPECIES_CATEGORIES) == "DESC_ENG"] <- "SPECIES_CATEGORY"
      Mar.utils::save_encrypted(SPECIES_CATEGORIES, file=file.path(get_pesd_dw_dir(), "MARFISSCI.SPECIES_CATEGORIES.RData"), compress=TRUE)
      rm(SPECIES_CATEGORIES)
    }
    if (file.exists(file.path(get_pesd_dw_dir(),"MARFISSCI.AREAS.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"MARFISSCI.AREAS.RData"), extract_user = extract_user, extract_computer = extract_computer)
      if (!"FISHING_AREA" %in% names(AREAS)) names(AREAS)[names(AREAS) == "DESC_ENG"] <- "FISHING_AREA"
      if ("CUSER" %in% names(AREAS)){
        AREAS$CUSER <- NULL
        AREAS$CDATE <- NULL
        AREAS$UUSER <- NULL
        AREAS$UDATE <- NULL
      }
      Mar.utils::save_encrypted(AREAS, file=file.path(get_pesd_dw_dir(), "MARFISSCI.AREAS.RData"), compress=TRUE)
      rm(AREAS)
    }
    if (file.exists(file.path(get_pesd_dw_dir(),"MARFISSCI.NAFO_UNIT_AREAS.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"MARFISSCI.NAFO_UNIT_AREAS.RData"), extract_user = extract_user, extract_computer = extract_computer)
      if (!"NAFO_AREA" %in% names(NAFO_UNIT_AREAS)) names(NAFO_UNIT_AREAS)[names(NAFO_UNIT_AREAS) == "AREA"] <- "NAFO_AREA"
      if ("CUSER" %in% names(NAFO_UNIT_AREAS)){
        NAFO_UNIT_AREAS$CUSER <- NULL
        NAFO_UNIT_AREAS$CDATE <- NULL
        NAFO_UNIT_AREAS$UUSER <- NULL
        NAFO_UNIT_AREAS$UDATE <- NULL
      }
      Mar.utils::save_encrypted(NAFO_UNIT_AREAS, file=file.path(get_pesd_dw_dir(), "MARFISSCI.NAFO_UNIT_AREAS.RData"), compress=TRUE)
      rm(NAFO_UNIT_AREAS)
    }
    if (file.exists(file.path(get_pesd_dw_dir(),"MARFISSCI.LOG_EFRT_STD_INFO.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"MARFISSCI.LOG_EFRT_STD_INFO.RData"), extract_user = extract_user, extract_computer = extract_computer)
      if (!"LATITUDE_EFRT" %in% names(LOG_EFRT_STD_INFO)){
        LOG_EFRT_STD_INFO$CUSER <- NULL
        LOG_EFRT_STD_INFO$CDATE <- NULL
        LOG_EFRT_STD_INFO$UUSER <- NULL
        LOG_EFRT_STD_INFO$UDATE <- NULL
        LOG_EFRT_STD_INFO <- Mar.utils::DDMMx_to_DD(df=LOG_EFRT_STD_INFO, format = "DDMMMM", lat.field = "ENT_LATITUDE", lon.field = "ENT_LONGITUDE", WestHemisphere = T)
        LOG_EFRT_STD_INFO$ENT_LATITUDE <- LOG_EFRT_STD_INFO$ENT_LONGITUDE <- NULL
        colnames(LOG_EFRT_STD_INFO)[colnames(LOG_EFRT_STD_INFO)=="LAT_DD"] <- "ENT_LATITUDE"
        colnames(LOG_EFRT_STD_INFO)[colnames(LOG_EFRT_STD_INFO)=="LON_DD"] <- "ENT_LONGITUDE"
        LOG_EFRT_STD_INFO <- Mar.utils::DDMMx_to_DD(df=LOG_EFRT_STD_INFO, format = "DDMMMM", lat.field = "DET_LATITUDE", lon.field = "DET_LONGITUDE", WestHemisphere = T)
        LOG_EFRT_STD_INFO$DET_LATITUDE <- LOG_EFRT_STD_INFO$DET_LONGITUDE <- NULL
        colnames(LOG_EFRT_STD_INFO)[colnames(LOG_EFRT_STD_INFO)=="LAT_DD"] <- "DET_LATITUDE"
        colnames(LOG_EFRT_STD_INFO)[colnames(LOG_EFRT_STD_INFO)=="LON_DD"] <- "DET_LONGITUDE"
        LOG_EFRT_STD_INFO$LATITUDE_EFRT = ifelse(is.na(LOG_EFRT_STD_INFO$ENT_LATITUDE),LOG_EFRT_STD_INFO$DET_LATITUDE,LOG_EFRT_STD_INFO$ENT_LATITUDE)
        LOG_EFRT_STD_INFO$LONGITUDE_EFRT = ifelse(is.na(LOG_EFRT_STD_INFO$ENT_LONGITUDE),LOG_EFRT_STD_INFO$DET_LONGITUDE,LOG_EFRT_STD_INFO$ENT_LONGITUDE)
        
        cat(paste("\nLOG_EFRT_STD_INFO:  Converted DDMM coordinates to DDDD.DD and added coord fields..."))
        Mar.utils::save_encrypted( LOG_EFRT_STD_INFO, file=file.path(get_pesd_dw_dir(), "MARFISSCI.LOG_EFRT_STD_INFO.RData"), compress=TRUE)
      }
      rm(LOG_EFRT_STD_INFO)
    }
    
  }
  if (db %in% c("ALL","usnefsc")){
    # USNEFSC -------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(get_pesd_dw_dir(),"USNEFSC.USS_STATION.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"USNEFSC.USS_STATION.RData"), extract_user = extract_user, extract_computer = extract_computer)
      if (!'LATITUDE' %in% colnames(USS_STATION)){
        USS_STATION$LATITUDE = USS_STATION$DECDEG_BEGLAT
        USS_STATION$LONGITUDE = USS_STATION$DECDEG_BEGLON
        USS_STATION$ELATITUDE = USS_STATION$DECDEG_ENDLAT
        USS_STATION$ELONGITUDE = USS_STATION$DECDEG_ENDLON
        cat("\nUSS_STATION:  For convenience, added LATITUDE, LONGITUDE, ELATITUDE AND ELONGITUDE fields")
        save(USS_STATION, file=file.path(get_pesd_dw_dir(), "USNEFSC.USS_STATION.RData"), compress=TRUE)
      }
      rm(USS_STATION)
    }
    
  }
  if (db %in% c("ALL","stomach")){
    # Stomach -------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(get_pesd_dw_dir(),"STOMACH.SDINF.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"STOMACH.SDINF.RData"), extract_user = extract_user, extract_computer = extract_computer)
      if (!'YEAR' %in% colnames(SDINF)){
        SDINF$YEAR = lubridate::year(SDINF$SDATE)
        Mar.utils::save_encrypted(SDINF, file=file.path(get_pesd_dw_dir(), "STOMACH.SDINF.RData"), compress=TRUE)
        cat("\nSDINF:  For convenience, added a YEAR field")
      }
      rm(SDINF)
    }
  }
  if (db %in% c("ALL","juvesh")){
    # JUVESH --------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(get_pesd_dw_dir(),"JUVESH.JVINF.RData"))){
    Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"JUVESH.JVINF.RData"), extract_user = extract_user, extract_computer = extract_computer)
    if (!'YEAR' %in% colnames(JVINF)){
      JVINF$YEAR = lubridate::year(JVINF$SDATE)
      cat("\nJVINF:  For convenience, added a YEAR field")
      
      JVINF <- Mar.utils::DDMMx_to_DD(df=JVINF, format = "DDMMMM", lat.field = "SLAT", lon.field = "SLONG", WestHemisphere = T)
      colnames(GSINF)[colnames(GSINF)=="LAT_DD"] <- "LATITUDE"
      colnames(GSINF)[colnames(GSINF)=="LON_DD"] <- "LONGITUDE"
      JVINF <- Mar.utils::DDMMx_to_DD(df=JVINF, format = "DDMMMM", lat.field = "ELAT", lon.field = "ELONG", WestHemisphere = T)
      colnames(GSINF)[colnames(GSINF)=="LAT_DD"] <- "ELATITUDE"
      colnames(GSINF)[colnames(GSINF)=="LON_DD"] <- "ELONGITUDE"
      
      JVINF$SLAT<- JVINF$SLONG<- JVINF$ELAT<- JVINF$ELONG<- NULL
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
      save(JVINF, file=file.path(get_pesd_dw_dir(), "JUVESH.JVINF.RData"), compress=TRUE)
      cat("\nJVINF:  For convenience, removed leading zeroes from CRUNO")
    }
    rm(JVINF)
    }
    if (file.exists(file.path(get_pesd_dw_dir(),"JUVESH.JVCAT.RData"))){
    Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"JUVESH.JVCAT.RData"), extract_user = extract_user, extract_computer = extract_computer)
    if(length(JVCAT[substr(JVCAT$CRUNO,1,1)=="O","CRUNO"])>0){
      JVCAT[substr(JVCAT$CRUNO,1,1)=="O","CRUNO"]<- substring(JVCAT[substr(JVCAT$CRUNO,1,1)=="O","CRUNO"],2)
      JVCAT[grepl("[[:alpha:]]", JVCAT$CRUNO)==F,"CRUNO"]<-as.integer(JVCAT[grepl("[[:alpha:]]", JVCAT$CRUNO)==F,"CRUNO"])
      save(JVCAT, file=file.path(get_pesd_dw_dir(), "JUVESH.JVCAT.RData"), compress=TRUE)
      cat("\nJVCAT:  For convenience, removed leading zeroes from CRUNO")
    }
    rm(JVCAT)
    }
    
    if (file.exists(file.path(get_pesd_dw_dir(),"JUVESH.JVDET.RData"))){
    Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"JUVESH.JVDET.RData"), extract_user = extract_user, extract_computer = extract_computer)
    if(length(JVDET[substr(JVDET$CRUNO,1,1)=="O","CRUNO"])>0){
      JVDET[substr(JVDET$CRUNO,1,1)=="O","CRUNO"]<- substring(JVDET[substr(JVDET$CRUNO,1,1)=="O","CRUNO"],2)
      JVDET[grepl("[[:alpha:]]", JVDET$CRUNO)==F,"CRUNO"]<-as.integer(JVDET[grepl("[[:alpha:]]", JVDET$CRUNO)==F,"CRUNO"])
      save(JVDET, file=file.path(get_pesd_dw_dir(), "JUVESH.JVDET.RData"), compress=TRUE)
      cat("\nJVDET:  For convenience, removed leading zeroes from CRUNO")
    }
    rm(JVDET)
    }
    
    
  }
  if (db %in% c("ALL","rvp70")){
    # Pre 1970s GROUNDFISH ------------------------------------------------------------------------------------------------------------------------------------
    
    if (file.exists(file.path(get_pesd_dw_dir(),"RVP70.GSINFP70.RData"))){
    Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"RVP70.GSINFP70.RData"), extract_user = extract_user, extract_computer = extract_computer)
    if (!'LATITUDE' %in% colnames(GSINFP70)){
      GSINFP70 <- Mar.utils::DDMMx_to_DD(df=GSINFP70, format = "DDMMSS", lat.field = "SLAT", lon.field = "SLONG", WestHemisphere = T)
      colnames(GSINFP70)[colnames(GSINFP70)=="LAT_DD"] <- "LATITUDE"
      colnames(GSINFP70)[colnames(GSINFP70)=="LON_DD"] <- "LONGITUDE"
      GSINFP70 <- Mar.utils::DDMMx_to_DD(df=GSINFP70, format = "DDMMSS", lat.field = "ELAT", lon.field = "ELONG", WestHemisphere = T)
      colnames(GSINFP70)[colnames(GSINFP70)=="LAT_DD"] <- "ELATITUDE"
      colnames(GSINFP70)[colnames(GSINFP70)=="LON_DD"] <- "ELONGITUDE"
      
      GSINFP70$SLAT<- GSINFP70$SLONG<- GSINFP70$ELAT<- GSINFP70$ELONG<- NULL
      cat(paste("\nGSINFP70:  Converted DDMM.SS coordinates to DD.DD ..."))
      #M McMahon 2024 - I can't find a reference specifying how the coordinate were formatted (DDMM.MM vs DDMM.SS, etc)
      #but only 1 coordinate (out of ~3500) had a value immediately following the decimal with a value more than 5.
      #If decimal minutes, they should be relatively between 0 and 9.  If seconds, they should be restricted between 0 and 5)
      save( GSINFP70, file=file.path(get_pesd_dw_dir(), "RVP70.GSINFP70.RData"), compress=TRUE)
    }
    rm(GSINFP70)
    }
  }
  if (db %in% c("ALL","chid")){
    # Chidley -------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(get_pesd_dw_dir(),"CHID.DSINF.RData"))){
    Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"CHID.DSINF.RData"), extract_user = extract_user, extract_computer = extract_computer)
    if (!'LATITUDE' %in% colnames(DSINF)){
      DSINF <- Mar.utils::DDMMx_to_DD(df=DSINF, format = "DDMMMM", lat.field = "SLAT", lon.field = "SLONG", WestHemisphere = T)
      colnames(DSINF)[colnames(DSINF)=="LAT_DD"] <- "LATITUDE"
      colnames(DSINF)[colnames(DSINF)=="LON_DD"] <- "LONGITUDE"
      DSINF <- Mar.utils::DDMMx_to_DD(df=DSINF, format = "DDMMMM", lat.field = "ELAT", lon.field = "ELONG", WestHemisphere = T)
      colnames(DSINF)[colnames(DSINF)=="LAT_DD"] <- "ELATITUDE"
      colnames(DSINF)[colnames(DSINF)=="LON_DD"] <- "ELONGITUDE"
      
      DSINF$SLAT<- DSINF$SLONG<- DSINF$ELAT<- DSINF$ELONG<- NULL
      cat(paste("\nDSINF:  Converted DDMM coordinates to DDDD.DD ..."))
      DSINF$YEAR = lubridate::year(DSINF$SDATE)
      cat("\nDSINF: Added a year field")
      months <- strptime(DSINF$SDATE, format='%Y-%m-%d %H:%M')$mon +1
      indx <- stats::setNames( rep(c('Winter', 'Spring', 'Summer', 'Fall'),each=3), c(12,1:11))
      DSINF$SEASON <- unname(indx[as.character(months)])
      
      cat("\nDSINF: Added a season field")
      
      save( DSINF, file=file.path(get_pesd_dw_dir(), "CHID.DSINF.RData"), compress=TRUE)
    }
    rm(DSINF)
    }
  }
  if (db %in% c("ALL","meso")){
    # MESO ----------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(get_pesd_dw_dir(),"MESO.MESOPELAGIC.RData"))){
    Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"MESO.MESOPELAGIC.RData"), extract_user = extract_user, extract_computer = extract_computer)
    if (!'LATITUDE' %in% colnames(MESOPELAGIC)){
      MESOPELAGIC$theLat = paste0(sprintf("%02d",MESOPELAGIC$LAT_DEG),sprintf("%02d",MESOPELAGIC$LAT_MIN))  
      MESOPELAGIC$theLong = paste0(sprintf("%02d",MESOPELAGIC$LON_DEG),sprintf("%02d",MESOPELAGIC$LON_MIN))  
      
      MESOPELAGIC <- Mar.utils::DDMMx_to_DD(df=MESOPELAGIC, format = "DDMMMM", lat.field = "theLat", lon.field = "theLong", WestHemisphere = T)
      colnames(MESOPELAGIC)[colnames(MESOPELAGIC)=="LAT_DD"] <- "LATITUDE"
      colnames(MESOPELAGIC)[colnames(MESOPELAGIC)=="LON_DD"] <- "LONGITUDE"

      MESOPELAGIC$theLat<- MESOPELAGIC$theLong<- MESOPELAGIC$LAT_DEG<- MESOPELAGIC$LAT_MIN<- MESOPELAGIC$LON_DEG<- MESOPELAGIC$LON_MIN<-NULL
      cat(paste("\nMESOPELAGIC:  Converted DDMM coordinates to DDDD.DD ..."))
      save( MESOPELAGIC, file=file.path(get_pesd_dw_dir(), "MESO.MESOPELAGIC.RData"), compress=TRUE)
    }
    rm(MESOPELAGIC)
    }
  }
  if (db %in% c("ALL","meso_gully")){
    # MESO_GULLY ----------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(get_pesd_dw_dir(),"MESO_GULLY.GSINF.RData"))){
    Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"MESO_GULLY.GSINF.RData"), extract_user = extract_user, extract_computer = extract_computer)
    if (!'LATITUDE' %in% colnames(GSINF)){
      GSINF <- Mar.utils::DDMMx_to_DD(df=GSINF, format = "DDMMMM", lat.field = "SLAT", lon.field = "SLONG", WestHemisphere = T)
      colnames(GSINF)[colnames(GSINF)=="LAT_DD"] <- "LATITUDE"
      colnames(GSINF)[colnames(GSINF)=="LON_DD"] <- "LONGITUDE"
      GSINF <- Mar.utils::DDMMx_to_DD(df=GSINF, format = "DDMMMM", lat.field = "ELAT", lon.field = "ELONG", WestHemisphere = T)
      colnames(GSINF)[colnames(GSINF)=="LAT_DD"] <- "ELATITUDE"
      colnames(GSINF)[colnames(GSINF)=="LON_DD"] <- "ELONGITUDE"
      
      GSINF$SLAT <- GSINF$SLONG <- GSINF$ELAT <- GSINF$ELONG<-NULL
      cat(paste("\nGSINF:  Converted DDMM coordinates to DDDD.DD ..."))
      save( GSINF, file=file.path(get_pesd_dw_dir(), "MESO_GULLY.GSINF.RData"), compress=TRUE)
    }
    rm(GSINF)
    }
  }
  if (db %in% c("ALL","inshore")){
    # INSHORE -------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(get_pesd_dw_dir(),"INSHORE.INS_INF.RData"))){
    Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"INSHORE.INS_INF.RData"), extract_user = extract_user, extract_computer = extract_computer)
    if (!'LATITUDE' %in% colnames(INS_INF)){
      INS_INF <- Mar.utils::DDMMx_to_DD(df=INS_INF, format = "DDMMMM", lat.field = "SLATDDMM", lon.field = "SLONGDDMM", WestHemisphere = T)
      colnames(INS_INF)[colnames(INS_INF)=="LAT_DD"] <- "LATITUDE"
      colnames(INS_INF)[colnames(INS_INF)=="LON_DD"] <- "LONGITUDE"
      INS_INF <- Mar.utils::DDMMx_to_DD(df=INS_INF, format = "DDMMMM", lat.field = "ELATDDMM", lon.field = "ELONGDDMM", WestHemisphere = T)
      colnames(INS_INF)[colnames(INS_INF)=="LAT_DD"] <- "ELATITUDE"
      colnames(INS_INF)[colnames(INS_INF)=="LON_DD"] <- "ELONGITUDE"
      
      INS_INF$SLAT <- INS_INF$SLONG <- INS_INF$ELAT <- INS_INF$ELONG <-NULL
      #remove the many coord fields
      INS_INF$BLATDEG<-NULL
      INS_INF$BLATMIN<-NULL
      INS_INF$BLONGDEG<-NULL
      INS_INF$BLONGMIN<-NULL
      
      INS_INF$ELATDEG<-NULL
      INS_INF$ELATMIN<-NULL
      INS_INF$ELONGDEG<-NULL
      INS_INF$ELONGMIN<-NULL

      cat(paste("\nINS_INF:  Converted DDMM coordinates to DDDD.DD ..."))
      save( INS_INF, file=file.path(get_pesd_dw_dir(), "INSHORE.INS_INF.RData"), compress=TRUE)
    }
    rm(INS_INF)
    }
  }
  if (db %in% c("ALL","redfish")){
    # REDFISH -------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(get_pesd_dw_dir(),"REDFISH.RFINF.RData"))){
    Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"REDFISH.RFINF.RData"), extract_user = extract_user, extract_computer = extract_computer)
    if (!'LATITUDE' %in% colnames(RFINF)){
      RFINF <- Mar.utils::DDMMx_to_DD(df=RFINF, format = "DDMMMM", lat.field = "SLAT", lon.field = "SLONG", WestHemisphere = T)
      colnames(RFINF)[colnames(RFINF)=="LAT_DD"] <- "LATITUDE"
      colnames(RFINF)[colnames(RFINF)=="LON_DD"] <- "LONGITUDE"
      RFINF <- Mar.utils::DDMMx_to_DD(df=RFINF, format = "DDMMMM", lat.field = "ELAT", lon.field = "ELONG", WestHemisphere = T)
      colnames(RFINF)[colnames(RFINF)=="LAT_DD"] <- "ELATITUDE"
      colnames(RFINF)[colnames(RFINF)=="LON_DD"] <- "ELONGITUDE"
      
      RFINF$SLAT <- RFINF$SLONG <- RFINF$ELAT <- RFINF$ELONG <-NULL
      cat(paste("\nRFINF:  Converted DDMM coordinates to DDDD.DD ..."))
      
      RFINF$YEAR = lubridate::year(RFINF$SDATE)
      cat("\nRFINF: Added a year field")
      
      months <- strptime(RFINF$SDATE, format='%Y-%m-%d %H:%M')$mon +1
      indx <- stats::setNames( rep(c('Winter', 'Spring', 'Summer', 'Fall'),each=3), c(12,1:11))
      RFINF$SEASON <- unname(indx[as.character(months)])
      
      cat("\nRFINF: Added a season field")
      save( RFINF, file=file.path(get_pesd_dw_dir(), "REDFISH.RFINF.RData"), compress=TRUE)
    }
    rm(RFINF)
    }
  }
  if (db %in% c("ALL","comland86")){
    # COMLAND86 -----------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(get_pesd_dw_dir(),"COMLAND86.PROVINCES.RData"))){
    Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"COMLAND86.PROVINCES.RData"), extract_user = extract_user, extract_computer = extract_computer)
      if(is.integer(PROVINCES$PROV_CODE)){
        PROVINCES$PROV_CODE <- as.character(PROVINCES$PROV_CODE)
      cat("\nPROVINCES: Changed provinces codes to characters so they can be used in filtering")
      Mar.utils::save_encrypted( PROVINCES, file=file.path(get_pesd_dw_dir(), "COMLAND86.PROVINCES.RData"), compress=TRUE)
    }
    rm(PROVINCES)
    }
  }
  if (db %in% c("ALL","comland67")){
    # COMLAND67 -----------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(get_pesd_dw_dir(),"COMLAND67.PROVINCES.RData"))){
      Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"COMLAND86.PROVINCES.RData"), extract_user = extract_user, extract_computer = extract_computer)
      if(is.integer(PROVINCES$PROV_CODE)){
        PROVINCES$PROV_CODE <- as.character(PROVINCES$PROV_CODE)
      cat("\nPROVINCES: Changed provinces codes to characters so they can be used in filtering")
      Mar.utils::save_encrypted( PROVINCES, file=file.path(get_pesd_dw_dir(), "COMLAND67.PROVINCES.RData"), compress=TRUE)
    }
    rm(PROVINCES)
    }
  }
  if (db %in% c("ALL","asef")){
    # ASEF ----------------------------------------------------------------------------------------------------------------------------------------------------
    if (file.exists(file.path(get_pesd_dw_dir(),"ASEF.TRINFO.RData"))){
    Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"ASEF.TRINFO.RData"), extract_user = extract_user, extract_computer = extract_computer)
    if (!'RLYEAR' %in% colnames(TRINFO)){
      TRINFO$RLYEAR <- lubridate::year(TRINFO$RLDATE)
      cat("\nTRINFO: RLYEAR added so it can be used in filtering")
      save( TRINFO, file=file.path(get_pesd_dw_dir(), "ASEF.TRINFO.RData"), compress=TRUE)
    }
    rm(TRINFO)
    }
    if (file.exists(file.path(get_pesd_dw_dir(),"ASEF.RCSITE.RData"))){
    Mar.utils::load_encrypted(file.path(get_pesd_dw_dir(),"ASEF.RCSITE.RData"), extract_user = extract_user, extract_computer = extract_computer)
    if (!'LATITUDE' %in% colnames(RCSITE)){
      RCSITE <- Mar.utils::DDMMx_to_DD(df=RCSITE, format = "DDMMMM", lat.field = "SLAT", lon.field = "SLONG", WestHemisphere = T)
      colnames(RCSITE)[colnames(RCSITE)=="LAT_DD"] <- "LATITUDE"
      colnames(RCSITE)[colnames(RCSITE)=="LON_DD"] <- "LONGITUDE"
      RCSITE$SLAT <- RCSITE$SLONG <- NULL
      cat(paste("\nRCSITE:  Converted DDMM coordinates to DDDD.DD ..."))
      save( RCSITE, file=file.path(get_pesd_dw_dir(), "ASEF.RCSITE.RData"), compress=TRUE)
    } 
    rm(RCSITE)
    }
  }
  
}