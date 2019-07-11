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
                    dateStart = NULL, dateEnd = NULL, 
                    mdCode = NULL, gearCode = 'all',
                    sectors = 7, data.dir = getwd()){
  if (!is.null(mdCode))mdCode=tolower(mdCode)
  if (!is.null(gearCode))gearCode=tolower(gearCode)
  
  if (is.null(dateEnd)) dateEnd = as.Date(dateStart,origin = "1970-01-01")+lubridate::years(1)
  fleetEnv = new.env()
  filternator<-function(){
    #funky function that drops irrelevant records
    LOOPAGAIN = T
    tbls = list(fleetEnv$GEARS, fleetEnv$MON_DOC_ENTRD_DETS, fleetEnv$MON_DOC_LICS, fleetEnv$V_LICENCE_HISTORY, fleetEnv$MON_DOCS, fleetEnv$MON_DOC_DEFNS, fleetEnv$COLUMN_DEFNS)
    while (LOOPAGAIN){
      precnt = sum(sapply(tbls, NROW))
      fleetEnv$MON_DOC_LICS = fleetEnv$MON_DOC_LICS[fleetEnv$MON_DOC_LICS$LICENCE_ID %in% fleetEnv$V_LICENCE_HISTORY$LICENCE_ID,]
      fleetEnv$V_LICENCE_HISTORY = fleetEnv$V_LICENCE_HISTORY[fleetEnv$V_LICENCE_HISTORY$LICENCE_ID %in% fleetEnv$MON_DOC_LICS$LICENCE_ID,]
      fleetEnv$MON_DOC_LICS = fleetEnv$MON_DOC_LICS[fleetEnv$MON_DOC_LICS$MON_DOC_ID %in% fleetEnv$MON_DOCS$MON_DOC_ID,]
      fleetEnv$MON_DOC_ENTRD_DETS = fleetEnv$MON_DOC_ENTRD_DETS[fleetEnv$MON_DOC_ENTRD_DETS$MON_DOC_ID %in%  fleetEnv$MON_DOCS$MON_DOC_ID,]
      fleetEnv$MON_DOCS = fleetEnv$MON_DOCS[fleetEnv$MON_DOCS$FV_GEAR_CODE %in% fleetEnv$GEARS$GEAR_CODE,]
      fleetEnv$MON_DOC_DEFNS = fleetEnv$MON_DOC_DEFNS[fleetEnv$MON_DOC_DEFNS$MON_DOC_DEFN_ID %in% fleetEnv$MON_DOCS$MON_DOC_DEFN_ID,]
      fleetEnv$COLUMN_DEFNS = fleetEnv$COLUMN_DEFNS[fleetEnv$COLUMN_DEFNS$COLUMN_DEFN_ID %in% fleetEnv$MON_DOC_ENTRD_DETS$COLUMN_DEFN_ID,]
      fleetEnv$GEARS = fleetEnv$GEARS[fleetEnv$GEARS$GEAR_CODE %in% fleetEnv$MON_DOCS$FV_GEAR_CODE,]
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
    assign("GEARS", fleetEnv$GEARS, envir = fleetEnv)
    if(nrow(fleetEnv$MON_DOCS)==1)stop("No records found")
  }
  
  #get the data
  Mar.datawrangling::get_data_custom('marfissci', tables = "MON_DOC_ENTRD_DETS", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  Mar.datawrangling::get_data_custom('marfissci', tables = "MON_DOC_LICS", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  Mar.datawrangling::get_data_custom('marfissci', tables = "V_LICENCE_HISTORY", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)  
  Mar.datawrangling::get_data_custom('marfissci', tables = "SPECIES", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  Mar.datawrangling::get_data_custom('marfissci', tables = "MON_DOCS", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg) 
  Mar.datawrangling::get_data_custom('marfissci', tables = "MON_DOC_DEFNS", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  Mar.datawrangling::get_data_custom('marfissci', tables = "COLUMN_DEFNS", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg)
  Mar.datawrangling::get_data_custom('marfissci', tables = "GEARS", data.dir = data.dir, quiet=T,env = fleetEnv, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn, usepkg = usepkg) 
  fleetEnv$MON_DOC_LICS= Mar.utils::clean_crap_fields(fleetEnv$MON_DOC_LICS, "MON_DOC_CUSER")
  
  fleetEnv$V_LICENCE_HISTORY = Mar.utils::clean_crap_fields(fleetEnv$V_LICENCE_HISTORY)
  fleetEnv$V_LICENCE_HISTORY = Mar.utils::simple_date(fleetEnv$V_LICENCE_HISTORY, c("START_DATE_TIME","END_DATE_TIME","EXPIRY_DATE"))
  fleetEnv$V_LICENCE_HISTORY = Mar.utils::rename_fields(fleetEnv$V_LICENCE_HISTORY, c("START_DATE_TIME","END_DATE_TIME"), c("START_DATE_HIS","END_DATE_TIME_HIS"))
  fleetEnv$V_LICENCE_HISTORY = fleetEnv$V_LICENCE_HISTORY[fleetEnv$V_LICENCE_HISTORY$START_DATE_HIS != fleetEnv$V_LICENCE_HISTORY$END_DATE_TIME_HIS,]
  # Filter by Sector ---------------------------------------------------------  
  fleetEnv$V_LICENCE_HISTORY = fleetEnv$V_LICENCE_HISTORY[fleetEnv$V_LICENCE_HISTORY$SECTOR_ID %in% sectors,]
  # Filter by Dates ----------------------------------------------------------
  fleetEnv$V_LICENCE_HISTORY = fleetEnv$V_LICENCE_HISTORY[fleetEnv$V_LICENCE_HISTORY$START_DATE_HIS< dateEnd,]
  fleetEnv$V_LICENCE_HISTORY = fleetEnv$V_LICENCE_HISTORY[fleetEnv$V_LICENCE_HISTORY$END_DATE_TIME_HIS> dateStart,]
  fleetEnv$MON_DOC_ENTRD_DETS= Mar.utils::clean_crap_fields(fleetEnv$MON_DOC_ENTRD_DETS, "MON_DOC_CUSER")
  fleetEnv$SPECIES = Mar.utils::clean_crap_fields(fleetEnv$SPECIES,c("SPECIES_ABBREV_FRE","DESC_FRE","LICENCE_DESC_FRE"))
  fleetEnv$MON_DOC_DEFNS = Mar.utils::clean_crap_fields(fleetEnv$MON_DOC_DEFNS,c())
  fleetEnv$COLUMN_DEFNS = fleetEnv$COLUMN_DEFNS[,c("COLUMN_DEFN_ID","DESC_ENG","HINT_TEXT_ENG")]

  if (!is.null(mdCode) && mdCode != "all") fleetEnv$MON_DOCS  = fleetEnv$MON_DOCS[fleetEnv$MON_DOCS$MON_DOC_DEFN_ID %in% mdCode,] 
  if (!is.null(gearCode) && gearCode != "all") fleetEnv$GEARS <- fleetEnv$GEARS[fleetEnv$GEARS$GEAR_CODE %in% gearCode,]
  filternator()
  #sort in prep of select list
  fleetEnv$MON_DOC_DEFNS = fleetEnv$MON_DOC_DEFNS[with(fleetEnv$MON_DOC_DEFNS,order(SHORT_DOC_TITLE, MON_DOC_DEFN_ID)),]
  mdCheck = unique(fleetEnv$MON_DOC_DEFNS$MON_DOC_DEFN_ID)
  grCheck = unique(fleetEnv$GEARS$GEAR_CODE)
  ####
  getMD<-function(md = NULL){
    if (!is.null(md)){
      if (md =="all")return(unique(fleetEnv$MON_DOC_DEFNS$MON_DOC_DEFN_ID))
    }
    
    choice<-utils::select.list(paste0(fleetEnv$MON_DOC_DEFNS[,"SHORT_DOC_TITLE"], " (",fleetEnv$MON_DOC_DEFNS[,"MON_DOC_DEFN_ID"],")"),
                               preselect=NULL,
                               multiple=T, graphics=T,
                               title='Monitoring Document Code')
    choice = sub(".*\\((.*)\\).*", "\\1", choice)
    if ((choice=="" || is.na(choice)))stop("\n\nNo selection made - Aborting.")
    return(as.integer(choice))
  }
  getGCd<-function(gearCode = NULL){
    if (!is.null(gearCode)){
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
  if (length(mdCheck)>1 ){
    mdPick = getMD(mdCode)
    fleetEnv$MON_DOCS  = fleetEnv$MON_DOCS[fleetEnv$MON_DOCS$MON_DOC_DEFN_ID %in% mdPick,]  
  }
  filternator()
  if (length(grCheck)>1 ){
    gearPick = getGCd(gearCode)
    fleetEnv$GEARS  = fleetEnv$GEARS[fleetEnv$GEARS$GEAR_CODE %in% gearPick,]  
  }
  filternator()

  res = merge(fleetEnv$MON_DOCS, fleetEnv$MON_DOC_LICS, by = c("MON_DOC_ID","TRIP_DMP_COMPANY_ID"))
  res = merge(res,fleetEnv$GEARS, by.x="FV_GEAR_CODE", by.y="GEAR_CODE",all.x=T)
  res = unique(res[,c("MON_DOC_DEFN_ID","MON_DOC_ID","LICENCE_ID","VR_NUMBER","FV_GEAR_CODE")]) #"FV_GEAR_CODE","FV_NAFO_UNIT_AREA_ID", "FV_FISHING_AREA_ID")])
  return(res)
}
