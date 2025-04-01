#' @title get_data
#' @description This function contains the logic for deciding whether to extract or load data.  It
#' is aware of the required tables, and if all are present,it will load them into the
#' global environment.  If some are missing, it will offer to re-extract them.  If the user chooses
#' to re-extract, it will get their oracle credentials and verify that the user has access to all of
#' the required tables prior to attempting an extraction.
#' @param data.dir  The default is your working directory. If you are hoping to 
#' load existing data, this folder should identify the folder containing your 
#' *.rdata files.
#' @param cxn A valid Oracle connection object. This parameter allows you to 
#' pass an existing connection, reducing the need to establish a new connection 
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
#' @param force.extract The default value is FALSE.  By default, existing data will be loaded.  If
#' \code{force.extract ==TRUE}, than a full extraction will take place, overwriting any existing
#' data.
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
#' @param usepkg default is \code{'rodbc'}. This indicates whether the connection to Oracle should
#' use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is slightly easier to setup, but
#' roracle will extract data ~ 5x faster.Deprecated; use \code{cxn} instead.
#' @param fn.oracle.username Default is \code{'_none_'}. This is your username 
#' for accessing Oracle objects. If you have a value for \code{oracle.username} 
#' stored in your environment (e.g., from an rprofile file), this can be left 
#' out and that value will be used. If a value for this is provided, it will 
#' take priority over your existing value. Deprecated; use \code{cxn} instead.
#' @param fn.oracle.password Default is \code{'_none_'}. This is your password 
#' for accessing Oracle objects. If you have a value for \code{oracle.password} 
#' stored in your environment (e.g., from an rprofile file), this can be left 
#' out and that value will be used. If a value for this is provided, it will 
#' take priority over your existing value. Deprecated; use \code{cxn} instead.
#' @param fn.oracle.dsn Default is \code{'_none_'}. This is your DSN/ODBC 
#' identifier for accessing Oracle objects. If you have a value 
#' for \code{oracle.dsn} stored in your environment (e.g., from an rprofile 
#' file), this can be left out and that value will be used. If a value for this 
#' is provided, it will take priority over your existing value. Deprecated; use 
#' \code{cxn} instead.
#' @param env This the the environment you want this function to work in.  The 
#' default value is \code{.GlobalEnv}.
#' @param quiet default is \code{FALSE}.  If True, no text describing progress
#' will be shown.
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_data <- function(db = NULL, cxn = NULL, usepkg = "rodbc", force.extract = FALSE, 
                     data.dir = file.path(getwd(), "data"), fn.oracle.username = "_none_", 
                     fn.oracle.password = "_none_", fn.oracle.dsn = "_none_", 
                     env = .GlobalEnv, quiet = FALSE) {
  Mar.utils::deprecationCheck(fn.oracle.username = fn.oracle.username, 
                   fn.oracle.password = fn.oracle.password, 
                   fn.oracle.dsn = fn.oracle.dsn,
                   usepkg = usepkg)
  if (substring(data.dir, nchar(data.dir)) == "/") 
    data.dir = substr(data.dir, 1, nchar(data.dir) - 1)
  if (is.null(db)) {
    ds_nms = data.frame(list(names = unlist(lapply(ds_all, "[[", "name")), db = names(unlist(lapply(ds_all,  "[[", "db"))), desc = unlist(lapply(ds_all, "[[", "desc"))))
    db_choice = utils::select.list(c(as.character(ds_nms$names), "Cancel"), multiple = F, graphics = T, title = "Please select a data source")
    if (db_choice == "Cancel") {
      cat("\nCancelled at user request")
      return(NULL)
    }
    assign("db", ds_nms[ds_nms$names == db_choice, ]$db, envir = .GlobalEnv)
  } else {
    assign("db", tolower(db), envir = .GlobalEnv)
  }
  local_table_status_check <- function(db = .GlobalEnv$db) {
    reqdTables.clean = gsub("*.*?\\.", "", paste0(toupper(db), ".", ds_all[[.GlobalEnv$db]]$tables))
    
    vers <- c(toupper(db), toupper(ds_all[[.GlobalEnv$db]]$schema))
    locTables.clean <- NA 
    for (v in 1:length(vers)) {
      these <- gsub("*.*?\\.", "", gsub(".RData", "", gsub(paste0(data.dir, .Platform$file.sep), "", list.files(path = data.dir, pattern = paste0("^", vers[v], ".*\\.rdata$"), full.names = T, ignore.case = TRUE, recursive = FALSE))))
      locTables.clean <- c(locTables.clean, these) 
    }
    locTables.clean <- unique(locTables.clean[!is.na(locTables.clean)])
    missingTables = sort(setdiff(reqdTables.clean, locTables.clean))
    return(missingTables)
  }
  oracle_activity <- function(tables = NULL, cxn, thecmd, 
                              theschema, prefix, action = "verify_access") {
    if (action == "verify_access") {
      if (!quiet) cat(paste0("\nVerifying access to ", tables, " ..."))
      qry = paste0("select '1' from ", theschema, ".", 
                   gsub(".*\\.", "", tables), " WHERE ROWNUM<=1")
      if (is.character(thecmd(cxn, qry, rows_at_time = 1))) {
        if (!quiet) cat(" failed")
        return(FALSE)
      } else {
        if (!quiet) cat(" success")
        return(TRUE)
      }
    } else if (action == "extract") {
      if (!quiet) cat(paste0("\nExtracting ", tables, "... "))
      add.where = "1=1"
      if (tables %in% names(ds_all[[.GlobalEnv$db]]$table_err_roracle)) {
        this = ds_all[[.GlobalEnv$db]]$table_err_roracle[[tables]]
        badvalues = paste(unlist(gsub("(.*)", "\\1", 
                                      this$badvalues)), sep = "", collapse = ",")
        add.where = paste0(this$field, " NOT IN (", badvalues, 
                           ")")
        if (!quiet) cat(paste0("\n\tSkipping records ", theschema, 
                               ".", tables, ".", this$field, " IN (", badvalues, 
                               ")\n\n                   \tThis\\These are from ", 
                               this$comments, "\n                   \n                   \tIf this is critical, use RODBC instead of ROracle\n"))
      }
      table_naked = table_naked1 = gsub(".*\\.", "", tables)
      qry = paste0("SELECT * from ", theschema, ".", table_naked, 
                   " WHERE ", add.where)
      res = thecmd(cxn, qry, rows_at_time = 1)
      assign(table_naked, res)
      save(list = table_naked1, file = file.path(data.dir, 
                                                 paste0(tables, ".RData")))
      if (!quiet) cat(paste("Got", tables))
    }
  }
  try_extract <- function(cxn, tables) {
      thecmd = Mar.utils::connectionCheck(cxn)

    verified = sapply(tables, oracle_activity, cxn, thecmd, 
                      ds_all[[.GlobalEnv$db]]$schema, 
                      toupper(db), "verify_access")
    if (!all(verified)) 
      stop("You do not have access to all of the required tables.\n      \nPlease ask the db custodian to grant you access to the tables listed above, and try again.\n\n      ")
    dir.create(data.dir, recursive = TRUE, showWarnings = FALSE)
    if (!quiet) cat("\n\nStarting extractions... ")
    timer.start = proc.time()
    sapply(tables, oracle_activity, cxn, thecmd,
           ds_all[[.GlobalEnv$db]]$schema, toupper(db), "extract")
    elapsed = timer.start - proc.time()
    if (!quiet) cat(paste("\n\nExtraction completed in", round(elapsed[3], 0) * -1, "seconds"))
    data_tweaks2(db = .GlobalEnv$db, data.dir = data.dir)
  }
  try_load <- function(tables, data.dir, thisenv = env) {
    loadit <- function(x, data.dir) {
      this = paste0(x, ".RData")
      thisP = file.path(data.dir, this)
      load(file = thisP, envir = env)
      if (!quiet) cat(paste0("\nLoaded ", x, "... "))
      fileAge = file.info(thisP)$mtime
      fileAge = round(difftime(Sys.time(), fileAge, units = "days"), 
                      0)
      if (!quiet) {
        cat(paste0(" (Data modified ", fileAge, " days ago.)"))
        if (fileAge > 90) cat(paste("\n!!! This data was extracted more than 90 days ago - consider re-extracting it"))
      }
    }
    if (!quiet) cat("\nLoading data...")
    timer.start = proc.time()
    sapply(tables, simplify = TRUE, loadit, data.dir)
    elapsed = timer.start - proc.time()
    if (!quiet) cat(paste0("\n\n", round(elapsed[3], 0) * -1, " seconds to load..."))
  }
  prefix = toupper(.GlobalEnv$db)
  if (prefix == "RV")     prefix = "GROUNDFISH"
  if (prefix == "MARFIS") prefix = "MARFISSCI"
  reqd = paste0(prefix, ".", ds_all[[.GlobalEnv$db]]$tables)
  if (dir.exists(data.dir) == TRUE) {
    status = local_table_status_check()
    if (length(status) == 0 & force.extract == F) {
      try_load(reqd, data.dir)
    } else if (length(status) == 0 & force.extract == T) {
      try_extract(cxn, reqd)
      try_load(reqd, data.dir)
    } else {
      if (toupper(.GlobalEnv$db) %in% c("RV", "MARFIS")) status = paste0(ds_all[[.GlobalEnv$db]]$schema,".",status)
      if (toupper(.GlobalEnv$db)=="ISDB") status = paste0("ISDB.",status)
      cat(paste0("\nLooked in '", data.dir, "' for required *.rdata files, but you are missing the following:"))
      cat("\n",status)
      choice = toupper(readline(prompt = "\nPress 'c' to (c)ancel this request, 'a' to re-extract (a)ll of the tables for\nthis datasource, or any other key to extract the missing data only\n(case-insensitive)"))
      print(choice)
      if (toupper(choice) == "C") {
        stop("Cancelled.   (Maybe check that your working directory is set to the folder *containing* your data folder and try again)")
      } else if (toupper(choice) == "A") {
        try_extract(cxn, reqd)
        try_load(reqd, data.dir)
      } else {
        try_extract(cxn, status)
        try_load(reqd, data.dir)
      }
    }
  }
  else {
    cat(paste0("\nWarning: The specified data.dir ('", data.dir, 
               "') does not exist."))
    goahead = toupper(readline(prompt = "\nType 'y' to create this folder and extract the data into it.  Press any other key to cancel the operation."))
    print(goahead)
    if (toupper(goahead) != "Y") {
      stop("Cancelled.")
    }
    else {
      dir.create(data.dir)
      try_extract(usepkg, reqd)
      try_load(reqd, data.dir)
    }
  }
}