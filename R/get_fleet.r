#' @title get_fleet
#' @description This function extracts all of the Vessels, Licences and MARFIS 
#' Monitoring Doc IDs associated with a particular fleet for a particular date 
#' range.
#' @param fn.oracle.username default is \code{'_none_'} This is your username for
#' accessing oracle objects. If you have a value for this stored in your
#' environment (e.g. from an rprofile file), this can be left and that value will
#' be used.  If a value for this is provided, it will take priority over your
#' existing value.
#' @param fn.oracle.password default is \code{'_none_'} This is your password for
#' accessing oracle objects. If you have a value for this stored in your
#' environment (e.g. from an rprofile file), this can be left and that value will
#' be used.  If a value for this is provided, it will take priority over your
#' existing value.
#' @param fn.oracle.dsn default is \code{'_none_'} This is your dsn/ODBC
#' identifier for accessing oracle objects. If you have a value for this stored
#' in your environment (e.g. from an rprofile file), this can be left and that
#' value will be used.  If a value for this is provided, it will take priority
#' over your existing value.
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
#' Valid codes include numbers 1-53, including
#' \itemize{
#' \item 2 = "MOBILE GEAR GROUNDFISH"
#' \item 5 = "SWORDFISH/SHARK LONGLINE"
#' \item 10 = "CRAB"
#' \item 12 = "INSHORE DREDGE SHELLFISH"
#' \item 13 = "OFFSHORE CLAM"
#' \item 14 = "SCALLOP"
#' \item 15 = "OFFSHORE SCALLOP"
#' \item 16 = "MOBILE SHRIMP"
#' \item 18 = "SEA URCHIN"
#' \item 19 = "OFFSHORE LOBSTER"
#' \item 20 = "FIXED GEAR (GROUNDFISH)"
#' \item 21 = "GROUNDFISH - SHRIMP"
#' \item 22 = "ATLANTIC BLUEFIN TUNA"
#' \item 25 = "CRAB LOG BOOK"
#' \item 23 = "PELAGICS"
#' \item 28 = "SHRIMP TRAP"
#' \item 31 = "NORTHERN SHRIMP SLIP"
#' \item 39 = "SEA CUCUMBER"
#' \item 41 = "SEA CUCUMBER (2012)"
#' \item 47 = "HAGFISH"
#' \item 49 = "LOBSTER 38 B"
#' \item 52 = "SEA URCHIN FOR LFA 36 & 38"
#' }
#' @param sectors default is \code{7}. This identifies the region.
#' @param data.dir  The default is your working directory. If you are hoping to 
#' load existing data, this folder should identify the folder containing your 
#' *.rdata files.
#' @importFrom Mar.utils clean_crap_fields
#' @importFrom Mar.utils rename_fields
#' @importFrom Mar.utils simple_date
#' @return returns a data.frame with 4 columns - "MON_DOC_DEFN_ID", "MON_DOC_ID", 
#' "LICENCE_ID" and "VR_NUMBER".
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_fleet<-function(fn.oracle.username = "_none_", 
                    fn.oracle.password = "_none_", 
                    fn.oracle.dsn = "_none_",
                    usepkg = "rodbc",
                    dateStart = NULL, dateEnd = NULL, mdCode = NULL, 
                    sectors = 7, data.dir = getwd()){
  fleetEnv = new.env()
  # tweak_it <- function(df = NULL, mdCode = NULL){
  #   #   cat("\n")
  #   #   if (length(mdCode)>1) browser()
  #   if (mdCode ==  1){
  #     cat("FIXED GEAR GROUNDFISH MONITORI","\n")
  # 
  #       }else if (mdCode ==  2){
  #         cat("MOBILE GEAR GROUNDFISH MONITOR","\n")
  #     #     browser()
  #       }else if (mdCode ==  3){
  #         cat("ATLANTIC BLUEFIN TUNA LOG DOCU","\n")
  #     #     browser()
  #     #     browser()
  #       }else if (mdCode ==  4){
  #         cat("SWORDFISH HARPOON","\n")
  #     #     browser()
  #       }else if (mdCode ==  5){
  #         cat("SWORDFISH/SHARK LONGLINE MONIT","\n")
  #     #     browser()
  #       }else if (mdCode ==  6){
  #         cat("FIXED GEAR HERRING MONITORING","\n")
  #     #     browser()
  #       }else if (mdCode ==  7){
  #         cat("HERRING MOBILE GEAR MONITORING","\n")
  #     #     browser()
  #       }else if (mdCode ==  8){
  #         cat("TRANSPORT MONITORING DOCUMENT","\n")
  #     #     browser()
  #       }else if (mdCode ==  9){
  #         cat("MACKEREL - FIXED GEAR","\n")
  #     #     browser()
  #       }else if (mdCode == 10){
  #         cat("CRAB MONITORING DOCUMENT","\n")
  #     #     detRecs =  unique(MON_DOC_ENTRD_DETS[MON_DOC_ENTRD_DETS$COLUMN_DEFN_ID %in%  146,])
  #     #     thisSp = merge(df, detRecs, by="MON_DOC_ID")
  #     #     thisSp =unique(thisSp[,c("VR_NUMBER","DATA_VALUE")])
  #     #     thisSpAgg = aggregate(
  #     #       x = list(cnt = thisSp$VR_NUMBER),
  #     #       by = list(AREA = thisSp$DATA_VALUE),
  #     #       length
  #     #     )
  #     #     print(thisSpAgg)
  #       }else if (mdCode == 11){
  #         cat("EXPLORATORY ROCK/JONAH CRAB FI","\n")
  #     #     browser()
  #       }else if (mdCode == 12){
  #         cat("INSHORE DREDGE SHELLFISH","\n")
  #     #     browser()
  #       }else if (mdCode == 13){
  #          cat("OFFSHORE CLAM FISHING LOG","\n")
  #     #     # browser()
  #   }else if (mdCode == 14){
  #     cat("SCALLOP MONITORING DOCUMENT","\n")
  #   #   detRecs =  unique(fleetEnv$MON_DOC_ENTRD_DETS[fleetEnv$MON_DOC_ENTRD_DETS$COLUMN_DEFN_ID %in%  189,])
  #   #   thisSp = merge(df, detRecs, by="MON_DOC_ID")
  #   #   thisSp =unique(thisSp[,c("VR_NUMBER","DATA_VALUE")])
  #   #   thisSpAgg = stats::aggregate(
  #   #     x = list(cnt = thisSp$VR_NUMBER),
  #   #     by = list(AREA = thisSp$DATA_VALUE),
  #   #     length
  #   #   )
  #   #   print(thisSpAgg)
  #       }else if (mdCode == 15){
  #         cat("OFFSHORE SCALLOP MONITORING DO","\n")
  #     #     browser()
  #       }else if (mdCode == 16){
  #         cat("MOBILE SHRIMP","\n")
  #     #     browser()
  #       }else if (mdCode == 17){
  #         cat("WHELK/MOONSNAIL MONITORING DOC","\n")
  #     #     browser()
  #       }else if (mdCode == 18){
  #         cat("SEA URCHIN MONITORING DOCUMENT","\n")
  #     #     browser()
  #       }else if (mdCode == 19){
  #         cat("OFFSHORE LOBSTER MONITORING DO","\n")
  #     #     browser()
  #       }else if (mdCode == 20){
  #         cat("FIXED GEAR (GROUNDFISH)/ ENGIN","\n")
  #     #     browser()
  #       }else if (mdCode == 21){
  #         cat("GROUNDFISH - SHRIMP / POISSON","\n")
  #     #     browser()
  #       }else if (mdCode == 22){
  #         cat("ATLANTIC BLUEFIN TUNA LOG","\n")
  #     #     browser()
  #       }else if (mdCode == 23){
  #         cat("PELAGICS / PELAGIQUES","\n")
  #     #     browser()
  #       }else if (mdCode == 24){
  #         cat("GENERIC BUYERS SLIP / <<GENERI","\n")
  #     #     browser()
  #       }else if (mdCode == 25){
  #         cat("CRAB LOG BOOK / LANDING REPORT","\n")
  #     #     browser()
  #       }else if (mdCode == 26){
  #         cat("TEST MONITORING DOCUMENT","\n")
  #     #     browser()
  #       }else if (mdCode == 27){
  #         cat("ATLANTIC STURGEON LOGBOOK","\n")
  #     #     browser()
  #       }else if (mdCode == 28){
  #         cat("SHRIMP TRAP","\n")
  #     #     browser()
  #       }else if (mdCode == 29){
  #         cat("INTERNATIONAL FISHING LOG","\n")
  #     #     browser()
  #       }else if (mdCode == 30){
  #         cat("DECK LOG","\n")
  #     #     browser()
  #       }else if (mdCode == 31){
  #         cat("NORTHERN SHRIMP SLIP - C","\n")
  #     #     browser()
  #       }else if (mdCode == 32){
  #         cat("NATIONAL SEA SLIP","\n")
  #     #     browser()
  #       }else if (mdCode == 39){
  #         cat("SEA CUCUMBER","\n")
  #     #     browser()
  #       }else if (mdCode == 40){
  #         cat("TEST BLANK DOCUMENT","\n")
  #     #     browser()
  #       }else if (mdCode == 41){
  #         cat("SEA CUCUMBER (2012)","\n")
  #     #     browser()
  #       }else if (mdCode == 46){
  #         cat("GENERIC CATCH ENTRY DOCUMENT","\n")
  #     #     browser()
  #       }else if (mdCode == 47){
  #         cat("HAGFISH MONITORING DOCUMENT","\n")
  #     #     browser()
  #       }else if (mdCode == 48){
  #         cat("COMMERCIAL OYSTER DOCUMENT","\n")
  #     #     browser()
  #       }else if (mdCode == 49){
  #         cat("LOBSTER 38 B DOCUMENT","\n")
  #     #     detRecs =  unique(MON_DOC_ENTRD_DETS[MON_DOC_ENTRD_DETS$COLUMN_DEFN_ID %in%  189,])
  #     #     browser()
  #       }else if (mdCode == 50){
  #         cat("SHRIMP TRANSPORT DOCUMENT","\n")
  #     #     browser()
  #       }else if (mdCode == 52){
  #         cat("SEA URCHIN FOR LFA 36 & 38","\n")
  #     #     browser()
  #       }else if (mdCode == 53){
  #         cat("SCALLOP DIVE","\n")
  #     #     browser()
  #   }
  #   #   # cat("Doing lobster-y things","\n")
  #   #   # cat("scalloping the data","\n")
  #   #   # cat("swordfishing for facts","\n")
  #   #   # df = df[df$SECTOR_ID == 7,]
  #   #   # return(df)
  # }
  
  
  filternator<-function(){
    #funky function that drops irrelevant records
    LOOPAGAIN = T
    tbls = list(fleetEnv$MON_DOC_ENTRD_DETS, fleetEnv$MON_DOC_LICS, fleetEnv$V_LICENCE_HISTORY, fleetEnv$MON_DOCS, fleetEnv$MON_DOC_DEFNS, fleetEnv$COLUMN_DEFNS)
    while (LOOPAGAIN){
      precnt = sum(sapply(tbls, NROW))
      fleetEnv$MON_DOC_LICS = fleetEnv$MON_DOC_LICS[fleetEnv$MON_DOC_LICS$LICENCE_ID %in% fleetEnv$V_LICENCE_HISTORY$LICENCE_ID,]
      fleetEnv$MON_DOC_ENTRD_DETS = fleetEnv$MON_DOC_ENTRD_DETS[fleetEnv$MON_DOC_ENTRD_DETS$MON_DOC_ID %in%  fleetEnv$MON_DOC_LICS$MON_DOC_ID,]
      fleetEnv$V_LICENCE_HISTORY = fleetEnv$V_LICENCE_HISTORY[fleetEnv$V_LICENCE_HISTORY$LICENCE_ID %in% fleetEnv$MON_DOC_LICS$LICENCE_ID,]
      fleetEnv$MON_DOCS = fleetEnv$MON_DOCS[fleetEnv$MON_DOCS$MON_DOC_ID %in% fleetEnv$MON_DOC_LICS$MON_DOC_ID,]
      fleetEnv$MON_DOC_DEFNS = fleetEnv$MON_DOC_DEFNS[fleetEnv$MON_DOC_DEFNS$MON_DOC_DEFN_ID %in% fleetEnv$MON_DOCS$MON_DOC_DEFN_ID,]
      fleetEnv$COLUMN_DEFNS = fleetEnv$COLUMN_DEFNS[fleetEnv$COLUMN_DEFNS$COLUMN_DEFN_ID %in% fleetEnv$MON_DOC_ENTRD_DETS$COLUMN_DEFN_ID,]
      postcnt =  sum(sapply(tbls, NROW))
      #cat(paste(precnt,"vs", postcnt,"\n"))
      if(postcnt==precnt) LOOPAGAIN=FALSE
    }
    assign("MON_DOC_LICS", fleetEnv$MON_DOC_LICS, envir = fleetEnv)
    assign("MON_DOC_ENTRD_DETS", fleetEnv$MON_DOC_ENTRD_DETS, envir = fleetEnv)
    assign("V_LICENCE_HISTORY", fleetEnv$V_LICENCE_HISTORY, envir = fleetEnv)
    assign("MON_DOCS", fleetEnv$MON_DOCS, envir = fleetEnv)
    assign("MON_DOC_DEFNS", fleetEnv$MON_DOC_DEFNS, envir = fleetEnv)
    assign("COLUMN_DEFNS", fleetEnv$COLUMN_DEFNS, envir = fleetEnv)
  }
  
  #get the data
  Mar.datawrangling::get_data_custom('marfissci', tables = "MON_DOC_ENTRD_DETS", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  Mar.datawrangling::get_data_custom('marfissci', tables = "MON_DOC_LICS", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  Mar.datawrangling::get_data_custom('marfissci', tables = "V_LICENCE_HISTORY", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)  
  Mar.datawrangling::get_data_custom('marfissci', tables = "SPECIES", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  Mar.datawrangling::get_data_custom('marfissci', tables = "MON_DOCS", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg) 
  Mar.datawrangling::get_data_custom('marfissci', tables = "MON_DOC_DEFNS", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  Mar.datawrangling::get_data_custom('marfissci', tables = "COLUMN_DEFNS", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  fleetEnv$MON_DOC_LICS= Mar.utils::clean_crap_fields(fleetEnv$MON_DOC_LICS, "MON_DOC_CUSER")
  fleetEnv$V_LICENCE_HISTORY = Mar.utils::clean_crap_fields(fleetEnv$V_LICENCE_HISTORY)
  fleetEnv$MON_DOC_ENTRD_DETS= Mar.utils::clean_crap_fields(fleetEnv$MON_DOC_ENTRD_DETS, "MON_DOC_CUSER")
  fleetEnv$SPECIES = Mar.utils::clean_crap_fields(fleetEnv$SPECIES,c("SPECIES_ABBREV_FRE","DESC_FRE","LICENCE_DESC_FRE"))
  fleetEnv$MON_DOC_DEFNS = Mar.utils::clean_crap_fields(fleetEnv$MON_DOC_DEFNS,c())
  fleetEnv$COLUMN_DEFNS = fleetEnv$COLUMN_DEFNS[,c("COLUMN_DEFN_ID","DESC_ENG","HINT_TEXT_ENG")]
  # Filter by Year ----------------------------------------------------------
  dateFields = c(12	#DATE SAILED (12)
                 ,18	#DATE SET
                 ,19	#DATE HAULED (19)
                 ,29	#DATE SHOT
                 ,37	#DATE FISHED (37)
                 ,79	#DATE SAILED (79)
                 ,166	#START DATE
                 ,527	#DATE FISHED (527)
                 ,557	#Date of Capture
                 ,667	#Date Landed
                 ,685	#START TIME (date fished)
                 ,849	#DATE
                 #end pts
                 ,54	#DATE HAULED (54)
                 ,81	#DATE FISHED (81)
                 ,107	#DATE HAULED (107)
                 ,168	#END DATE
                 ,682	#END TIME (date fished)
                 ,629	#Date Sold
                 ,746	#Date Returned
                 ,836	#ENTRY DATE
                 ,881	#At Sea Obs Date Collected
  ) 

  MD_filt = fleetEnv$MON_DOC_ENTRD_DETS[fleetEnv$MON_DOC_ENTRD_DETS$COLUMN_DEFN_ID %in%  dateFields,]
  
  MD_filt$date = as.Date(MD_filt$DATA_VALUE, format = "%Y-%b-%d")
  MD_filt = unique(MD_filt[MD_filt$date>= dateStart & MD_filt$date < dateEnd,c("MON_DOC_ID")])
  fleetEnv$MON_DOC_ENTRD_DETS = fleetEnv$MON_DOC_ENTRD_DETS[fleetEnv$MON_DOC_ENTRD_DETS$MON_DOC_ID %in%  MD_filt,]
  filternator()
  # MON_DOC_LICS = MON_DOC_LICS[MON_DOC_LICS$MON_DOC_ID %in% MD_filt$MON_DOC_ID,]
  # V_LICENCE_HISTORY = V_LICENCE_HISTORY[V_LICENCE_HISTORY$LICENCE_ID %in% MON_DOC_LICS$LICENCE_ID,]
  rm(MD_filt)
  
  fleetEnv$V_LICENCE_HISTORY = fleetEnv$V_LICENCE_HISTORY[fleetEnv$V_LICENCE_HISTORY$SECTOR_ID %in% sectors,]
  #still have recs in V_* where the end date is beyond what we specified
  fleetEnv$V_LICENCE_HISTORY = Mar.utils::simple_date(fleetEnv$V_LICENCE_HISTORY, c("START_DATE_TIME","END_DATE_TIME","EXPIRY_DATE"))
  fleetEnv$V_LICENCE_HISTORY = Mar.utils::rename_fields(fleetEnv$V_LICENCE_HISTORY, c("START_DATE_TIME","END_DATE_TIME"), c("START_DATE_HIS","END_DATE_TIME_HIS"))
  fleetEnv$V_LICENCE_HISTORY = fleetEnv$V_LICENCE_HISTORY[fleetEnv$V_LICENCE_HISTORY$START_DATE_HIS != fleetEnv$V_LICENCE_HISTORY$END_DATE_TIME_HIS,]
  #following line ensures that we're only looking at licences that were valid for some point in the date range 
  #i.e. they had their start point before our end date and their end date was after our end date
  fleetEnv$V_LICENCE_HISTORY = fleetEnv$V_LICENCE_HISTORY[fleetEnv$V_LICENCE_HISTORY$START_DATE_HIS <= dateEnd & fleetEnv$V_LICENCE_HISTORY$END_DATE_TIME_HIS >= dateEnd, ]
  
  filternator()
  # end yr filter -----------------------------------------------------------
  #sort in prep of select list
  fleetEnv$MON_DOC_DEFNS = fleetEnv$MON_DOC_DEFNS[with(fleetEnv$MON_DOC_DEFNS,order(SHORT_DOC_TITLE, MON_DOC_DEFN_ID)),]

  if (is.null(mdCode)){
    mdPick<-NA
    while(is.na(mdPick)){
      mdPick = utils::select.list(
        paste0(fleetEnv$MON_DOC_DEFNS[,"SHORT_DOC_TITLE"], " (",fleetEnv$MON_DOC_DEFNS[,"MON_DOC_DEFN_ID"],")"),
                                  preselect=NULL,
                                  multiple=T, graphics=T,
                                  title='Monitoring Document Code')
      mdPick = sub(".*\\((.*)\\).*", "\\1", mdPick)
      if (length(mdPick) ==0 || is.na(mdPick) ) print("You must select one of provided options")
    }
    mdCode = mdPick
  }
  fleetEnv$MON_DOCS  = fleetEnv$MON_DOCS[fleetEnv$MON_DOCS$MON_DOC_DEFN_ID %in% mdCode,]  
  filternator()
  res = merge(fleetEnv$MON_DOCS, fleetEnv$MON_DOC_LICS, by = "MON_DOC_ID")
  res = unique(res[,c("MON_DOC_DEFN_ID","MON_DOC_ID","LICENCE_ID","VR_NUMBER")]) #"FV_GEAR_CODE","FV_NAFO_UNIT_AREA_ID", "FV_FISHING_AREA_ID")])
 return(res)
  
}
