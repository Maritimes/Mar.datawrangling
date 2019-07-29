#' @title get_fleet
#' @description This function extracts all of the Vessels, Licences and MARFIS 
#' Monitoring Doc IDs associated with a particular fleet for a particular date 
#' range.
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
#' @param gearCode default is \code{NULL}. In some cases, a fleet will contain multiple
#' gear types. Setting this to \code{NULL} (the default) will prompt you to 
#' select gears from the available values (if there are multiple).  Setting it to 
#' \code{'all'} will get all gear types.  Entering a vector of MARFIS gear codes 
#' (e.g. \code{c(51,81)}) will only return those gear codes.
#' @param sectors default is \code{7}. This identifies the region.
#' @param data.dir  The default is your working directory. If you are hoping to 
#' load existing data, this folder should identify the folder containing your 
#' *.rdata files.
#' @family fleets
#' @return returns a data.frame with 4 columns - "MON_DOC_DEFN_ID", "MON_DOC_ID", 
#' "LICENCE_ID" and "VR_NUMBER".
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_fleet<-function(fn.oracle.username = "_none_", 
                    fn.oracle.password = "_none_", 
                    fn.oracle.dsn = "_none_",
                    usepkg = "rodbc",
                    dateStart = NULL, dateEnd = NULL, 
                    mdCode = NULL, gearCode = NULL,
                    sectors = 7, data.dir = getwd()){
  mdCode=tolower(mdCode)
  gearCode=tolower(gearCode)
  #if no end date, do it for 1 year 
  if (is.null(dateEnd)) dateEnd = as.Date(dateStart,origin = "1970-01-01")+lubridate::years(1)
  fleetEnv = new.env()
  getGCd<-function(gearCode = NULL){
    if (length(gearCode)>0){
      if (gearCode =="all")return(unique(fleetEnv$GEARS$GEAR_CODE))
    }
    
    choice<-utils::select.list(paste0(fleetEnv$GEARS$DESC_ENG, " (",fleetEnv$GEARS$GEAR_CODE,")"),
                               preselect=NULL,
                               multiple=T, graphics=T,
                               title='Gear Code')
    choice = sub(".*\\((.*)\\).*", "\\1", choice)
    if ((choice=="" || is.na(choice)))stop("\n\nNo selection made - Aborting.")
    return(as.integer(choice))
  }
  getMD<-function(md = NULL){
    if (length(md)>0){
      if (md =="all")return(unique(fleetEnv$MON_DOC_DEFNS$MON_DOC_DEFN_ID))
    }
    
    choice<-utils::select.list(paste0(fleetEnv$MON_DOC_DEFNS[,"DOCUMENT_TITLE"], " (",fleetEnv$MON_DOC_DEFNS[,"MON_DOC_DEFN_ID"],")"),
                               preselect=NULL,
                               multiple=T, graphics=T,
                               title='Monitoring Document Code')
    choice = sub(".*\\((.*)\\).*", "\\1", choice)
    if ((choice=="" || is.na(choice)))stop("\n\nNo selection made - Aborting.")
    return(as.integer(choice))
  }
  
  filternator<-function(){
    #funky function that drops irrelevant records
    LOOPAGAIN = T
    tbls = list(fleetEnv$GEARS, 
                fleetEnv$MON_DOCS,
                fleetEnv$MON_DOC_DEFNS, 
                fleetEnv$V_LICENCE_HISTORY, 
                fleetEnv$MON_DOC_LICS
                )
    while (LOOPAGAIN){
      precnt = sum(sapply(tbls, NROW))
      fleetEnv$GEARS = unique(fleetEnv$GEARS[fleetEnv$GEARS$GEAR_CODE %in% fleetEnv$MON_DOCS$FV_GEAR_CODE,])
      fleetEnv$MON_DOCS = unique(fleetEnv$MON_DOCS[fleetEnv$MON_DOCS$FV_GEAR_CODE %in% fleetEnv$GEARS$GEAR_CODE 
                                                   & fleetEnv$MON_DOCS$MON_DOC_ID %in% fleetEnv$MON_DOC_LICS$MON_DOC_ID,])
      fleetEnv$MON_DOC_DEFNS = unique(fleetEnv$MON_DOC_DEFNS[fleetEnv$MON_DOC_DEFNS$MON_DOC_DEFN_ID %in% fleetEnv$MON_DOCS$MON_DOC_DEFN_ID,])
      fleetEnv$V_LICENCE_HISTORY = unique(fleetEnv$V_LICENCE_HISTORY[fleetEnv$V_LICENCE_HISTORY$LICENCE_ID %in% fleetEnv$MON_DOC_LICS$LICENCE_ID,])
      fleetEnv$MON_DOC_LICS = unique(fleetEnv$MON_DOC_LICS[fleetEnv$MON_DOC_LICS$MON_DOC_ID %in% fleetEnv$MON_DOCS$MON_DOC_ID,])
      postcnt =  sum(sapply(tbls, NROW))
      #cat(paste(precnt,"vs", postcnt,"\n"))
      if(postcnt==precnt) LOOPAGAIN=FALSE
    }
    assign("GEARS", fleetEnv$GEARS, envir = fleetEnv)
    assign("MON_DOCS", fleetEnv$MON_DOCS, envir = fleetEnv)
    assign("MON_DOC_DEFNS", fleetEnv$MON_DOC_DEFNS, envir = fleetEnv)
    assign("V_LICENCE_HISTORY", fleetEnv$V_LICENCE_HISTORY, envir = fleetEnv)
    assign("MON_DOC_LICS", fleetEnv$MON_DOC_LICS, envir = fleetEnv)
    if(nrow(fleetEnv$MON_DOCS)<1)stop("No records found")
  }

  # Get code tables or select boxes -----------------------------------------  
  get_data_custom('MARFISSCI', tables = "GEARS", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg) 
  fleetEnv$GEARS = unique(fleetEnv$GEARS[, names(fleetEnv$GEARS) %in% c("GEAR_CODE", "GEAR","DESC_ENG")])

  get_data_custom('MARFISSCI', tables = "MON_DOC_DEFNS", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  fleetEnv$MON_DOC_DEFNS = unique(fleetEnv$MON_DOC_DEFNS[, names(fleetEnv$MON_DOC_DEFNS) %in% c("MON_DOC_DEFN_ID", 
                                                                                                "DOCUMENT_TITLE",
                                                                                                "SECTOR_ID")] )
  # Get Data ----------------------------------------------------------------
  get_data_custom('MARFISSCI', tables = "MON_DOCS", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg) 
  fleetEnv$MON_DOCS = fleetEnv$MON_DOCS[, names(fleetEnv$MON_DOCS) %in% c("MON_DOC_ID", 
                                                                          "MON_DOC_DEFN_ID",
                                                                          "VR_NUMBER",
                                                                          "FV_GEAR_CODE")]
  
  get_data_custom('MARFISSCI', tables = "V_LICENCE_HISTORY", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)  
  fleetEnv$V_LICENCE_HISTORY = Mar.utils::clean_dfo_fields(fleetEnv$V_LICENCE_HISTORY)
  fleetEnv$V_LICENCE_HISTORY = unique(fleetEnv$V_LICENCE_HISTORY[, names(fleetEnv$V_LICENCE_HISTORY) %in% c("START_DATE_TIME", 
                                                                                                            "END_DATE_TIME",
                                                                                                            "SECTOR_ID",
                                                                                                            "LICENCE_ID")])
  fleetEnv$V_LICENCE_HISTORY = fleetEnv$V_LICENCE_HISTORY[fleetEnv$V_LICENCE_HISTORY$START_DATE_TIME != fleetEnv$V_LICENCE_HISTORY$END_DATE_TIME 
                                                          & fleetEnv$V_LICENCE_HISTORY$START_DATE_TIME< dateEnd
                                                          & fleetEnv$V_LICENCE_HISTORY$END_DATE_TIME> dateStart,]
  
  get_data_custom(schema = 'MARFISSCI', tables = "MON_DOC_LICS", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)  
  fleetEnv$MON_DOC_LICS = Mar.utils::clean_dfo_fields(fleetEnv$MON_DOC_LICS)
  fleetEnv$MON_DOC_LICS = fleetEnv$MON_DOC_LICS[, names(fleetEnv$MON_DOC_LICS) %in% c("MON_DOC_ID", "LICENCE_ID")]

  get_data_custom('MARFISSCI', tables = "MON_DOC_ENTRD_DETS", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  fleetEnv$MON_DOC_ENTRD_DETS = fleetEnv$MON_DOC_ENTRD_DETS[fleetEnv$MON_DOC_ENTRD_DETS$COLUMN_DEFN_ID %in%  c(21,741,835),c("MON_DOC_ID","COLUMN_DEFN_ID","DATA_VALUE")] 
  fleetEnv$MON_DOC_ENTRD_DETS = reshape2::dcast(fleetEnv$MON_DOC_ENTRD_DETS, MON_DOC_ID ~ COLUMN_DEFN_ID, value.var = "DATA_VALUE")
  colnames(fleetEnv$MON_DOC_ENTRD_DETS)[colnames(fleetEnv$MON_DOC_ENTRD_DETS)=="21"] <- "OBS_PRESENT"
  colnames(fleetEnv$MON_DOC_ENTRD_DETS)[colnames(fleetEnv$MON_DOC_ENTRD_DETS)=="741"] <- "OBS_TRIP_CLN"
  colnames(fleetEnv$MON_DOC_ENTRD_DETS)[colnames(fleetEnv$MON_DOC_ENTRD_DETS)=="835"] <- "OBS_ID"
  
  fleetEnv$MON_DOC_ENTRD_DETS$OBS_TRIP_CLN = gsub(pattern = "[^[:alnum:]]", replacement = "", x= fleetEnv$MON_DOC_ENTRD_DETS$OBS_TRIP_CLN)
  
   if (gearCode != "all" && length(gearCode)>0) {
    fleetEnv$GEARS = fleetEnv$GEARS[fleetEnv$GEARS$GEAR_CODE %in% gearCode,]
    fleetEnv$MON_DOCS = fleetEnv$MON_DOCS[fleetEnv$MON_DOCS$FV_GEAR_CODE %in% gearCode,]
  }
  if (length(mdCode)>0 && mdCode != "all") fleetEnv$MON_DOCS  = fleetEnv$MON_DOCS[fleetEnv$MON_DOCS$MON_DOC_DEFN_ID %in% mdCode,] 
  if (!is.null(sectors)){
    fleetEnv$V_LICENCE_HISTORY = fleetEnv$V_LICENCE_HISTORY[fleetEnv$V_LICENCE_HISTORY$SECTOR_ID %in% sectors,]
    fleetEnv$MON_DOC_DEFNS = fleetEnv$MON_DOC_DEFNS[fleetEnv$MON_DOC_DEFNS$SECTOR_ID  %in% sectors,]
  } 
  
  filternator()
  
  fleetEnv$MON_DOC_DEFNS = fleetEnv$MON_DOC_DEFNS[with(fleetEnv$MON_DOC_DEFNS,order(DOCUMENT_TITLE, MON_DOC_DEFN_ID)),]
  mdCheck = unique(fleetEnv$MON_DOC_DEFNS$MON_DOC_DEFN_ID)
  
  if (length(mdCode)<1 && length(mdCheck)>1 ){
    mdPick = getMD(mdCode)
    fleetEnv$MON_DOCS  = fleetEnv$MON_DOCS[fleetEnv$MON_DOCS$MON_DOC_DEFN_ID %in% mdPick,]  
  }
  filternator()
  grCheck = unique(fleetEnv$GEARS$GEAR_CODE)
  if (length(gearCode)<1 && length(grCheck)>1 ){
    gearPick = getGCd(gearCode)
    fleetEnv$GEARS  = fleetEnv$GEARS[fleetEnv$GEARS$GEAR_CODE %in% gearPick,]  
  }
  filternator()
  res = unique(merge(fleetEnv$MON_DOC_LICS,fleetEnv$MON_DOCS, all.x = T))
  res = merge(res, fleetEnv$MON_DOC_ENTRD_DETS, all.x=T)
  return(res)
}





