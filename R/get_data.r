#' @title get_data
#' @description This function contains the logic for deciding whether to extract or load data.  It
#' is aware of the required tables, and if all are present,it will load them into the
#' global environment.  If some are missing, it will offer to re-extract them.  If the user chooses
#' to re-extract, it will get their oracle credentials and verify that the user has access to all of
#' the required tables prior to attempting an extraction.
#' @param cxn A valid Oracle connection object. This parameter allows you to 
#' pass an existing connection, reducing the need to establish a new connection 
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
#' @param force.extract The default value is FALSE.  By default, existing data will be loaded.  If
#' \code{force.extract ==TRUE}, than a full extraction will take place, overwriting any existing
#' data.
#' @param reextract.override default is \code{FALSE}. By default, if data is 
#' missing, the script will ask the user the script will ask the user whether
#' to download all of the data again or just the missing data.  If set to \code{TRUE},
#' all of the data will be downloaded without prompting the user for anything.  
#' This supports running the script automatically.
#' @param db default is \code{NULL}. This identifies the dataset you are working
#' with. Valid values include the following (assuming you have Oracle access)
#' \itemize{
#' \item \code{rv} - Bottom Trawl Surveys conducted in shore waters off south west Nova Scotia
#' \item \code{rvp70} - Bottom Trawl Surveys conducted in shore waters off south west Nova Scotia conducted prior to 1970
#' \item \code{isdb} - DFO at-sea fish catch observations from commercial fishing vessels operating in the North West Atlantic.
#' \item \code{chid} - Exploratory fishing surveys of the benthic fish fauna at 900-1800m.
#' \item \code{redfish} - A survey using stratified random design with day/night replication targeting deep sea redfish
#' \item \code{marfis} - A Policy and Economics Branch database that houses information on the fisheries of the Scotia-Fundy region, including data related to catch and effort.
#' \item \code{comland67} - Commercial Landings (1967-1985) - like MARFIS, but earlier.
#' \item \code{comland86} - Commercial Landings (1986-2001) - like MARFIS, but earlier. This is separated from the 1967 - 1985 COMLAND data due to differences in the code tables.
#' \item \code{stomach} - 	Stomach Contents Database
#' \item \code{asef} - Tagged salmon records at fishways
#' \item \code{meso} - Mesopelagic Database
#' \item \code{meso_gully} - Mesopelagic Gully Database
#' \item \code{juvesh} - Juvenile Silver Hake Database
#' }
#' @param env This the the environment you want this function to work in.  The 
#' default value is \code{.GlobalEnv}.
#' @param quiet default is \code{FALSE}.  If True, no text describing progress
#' will be shown.
#' @param extract_user default is \code{NULL}.  This parameter can be used with
#' \code{extract_computer} to load encypted data files extracted by another user 
#' and/or computer
#' @param extract_computer  default is \code{NULL}.  This parameter can be used with
#' \code{extract_user} to load encypted data files extracted by another user 
#' and/or computer
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export

get_data <- function(db = NULL, cxn = NULL, force.extract = FALSE, reextract.override = FALSE,
                     env = .GlobalEnv, quiet = FALSE, extract_user = NULL, extract_computer = NULL) {
  
  # Normalize DB name using synonyms
  db_key <- normalize_db_key(db)
  assign("db", db_key, envir = .GlobalEnv)
  
  schema <- get_schema(db_key)
  prefix <- if (!is.null(schema)) schema else toupper(db_key)
  
  # Required tables with prefix
  reqd <- paste0(prefix, ".", toupper(get_ds_all()[[db_key]]$tables))
  
  # Check local status
  status <- local_table_status_check(db_key)
  
  if (length(status) == 0 && !force.extract) {
    try_load(reqd, env, quiet, extract_user, extract_computer)
    
  } else if (length(status) == 0 && force.extract) {
    try_extract(cxn, reqd, quiet, extract_user, extract_computer)
    try_load(reqd, env, quiet, extract_user, extract_computer)
    
  } else {
    # Adjust missing tables with schema prefix
    if (!is.null(schema)) {
      status <- paste0(schema, ".", status)
    }
    
    message(paste0("\nMissing tables in '", get_pesd_dw_dir(), "':"))
    cat("\n", status)
    
    if (toupper(.GlobalEnv$db) == "ISDB") {
      message("(Note: ISDB schema changes may require a full re-extraction)")
    }
    
    choice <- if (reextract.override) {
      "A"
    } else {
      toupper(readline(prompt = "\nPress 'c' to cancel, 'a' to extract all, or any other key to extract missing only: "))
    }
    
    if (choice == "C") {
      stop("Cancelled. Check your working directory and try again.")
    } else if (choice == "A") {
      try_extract(cxn, reqd, quiet, extract_user, extract_computer)
      try_load(reqd, env, quiet, extract_user, extract_computer)
    } else {
      try_extract(cxn, status, quiet, extract_user, extract_computer)
      try_load(reqd, env, quiet, extract_user, extract_computer)
    }
  }
}
