#' @title get_data
#' @description This function contains the logic for deciding whether to extract or load data.  It
#' is aware of the required tables, and if all are present,it will load them into the
#' global environment.  If some are missing, it will offer to re-extract them.  If the user chooses
#' to re-extract, it will get their oracle credentials and verify that the user has access to all of
#' the required tables prior to attempting an extraction.
#' @param data.dir  The default is your working directory. If you are hoping to load existing data,
#' this folder should contain a data folder containing your rdata files. If you are extracting data,
#' a data folder will be created under this folder.
#' extracted files to go.
#' @param force.extract The default value is FALSE.  By default, existing data will be loaded.  If
#' \code{force.extract ==TRUE}, than a full extraction will take place, overwriting any existing
#' data.
#' @param db default is \code{NULL}. This identifies the dataset you are working
#' with.
#' @param usepkg default is \code{'rodbc'}. This indicates whether the connection to Oracle should
#' use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is slightly easier to setup, but
#' roracle will extract data ~ 5x faster.
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
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom lubridate ymd
#' @importFrom utils data
#' @importFrom Mar.utils make_oracle_cxn
#' @export
get_data <-function(db = NULL,
                    usepkg = 'rodbc',
                    force.extract = FALSE,
                    data.dir = file.path(getwd(), 'data'),
                    fn.oracle.username ="_none_",
                    fn.oracle.password="_none_",
                    fn.oracle.dsn="_none_"){
  #remove possible trailing slash from data.dir path
  if (substring(data.dir, nchar(data.dir))=="/") data.dir = substr(data.dir, 1, nchar(data.dir)-1)
  
  if (is.null(db)) {
    # Prompt for choice if no db selected ------------------------------------
    ds_nms = data.frame(list(names = unlist(lapply(ds_all, "[[", "name")), 
                             db = names(unlist(lapply(ds_all, "[[", "db"))), 
                             desc = unlist(lapply(ds_all, "[[",                                                                                                                               "desc"))))
                             db_choice = select.list(c(as.character(ds_nms$names), 
                              "Cancel"), multiple = F, graphics = T, title = "Please select a data source")
    if (db_choice == "Cancel") {
      cat("\nCancelled at user request")
      return(NULL)
    }
    assign("db", ds_nms[ds_nms$names == db_choice, ]$db, envir = .GlobalEnv)
  }
  else {
    assign("db", db, envir = .GlobalEnv)
  }
  # Setup -------------------------------------------------------------------
  # Compare the local cache of tables with what is required -----------------
  local_table_status_check <- function(db = .GlobalEnv$db) {
    localTables <- list.files(path = data.dir, pattern = paste0("^", toupper(db), ".*\\.rdata$"), 
                              full.names = T, ignore.case = TRUE, 
                              recursive = FALSE)
    prefixed.localTables = gsub(".RData", "", gsub(paste0(data.dir, .Platform$file.sep), "", localTables))
    prefixed.reqdTables = paste0(toupper(db), ".", ds_all[[.GlobalEnv$db]]$tables)
    reqdTables.clean = gsub("*.*?\\.", "", prefixed.reqdTables)

    missingTables = sort(setdiff(prefixed.reqdTables, prefixed.localTables))
    missingTables.clean = gsub("*.*?\\.", "", missingTables)
    results = list(missingTables, missingTables.clean)
    return(results)
  }
  # oracle_activity -----------------------------------------------------
  ## table access verification ------------------------------------------ 
  ## data extraction ----------------------------------------------------
  oracle_activity <- function(tables = NULL, oracle_cxn, usepkg, theschema, prefix, action = "verify_acess") {
    if (usepkg == "roracle") {
      thecmd = ROracle::dbGetQuery
    }
    else {
      thecmd = RODBC::sqlQuery
    }
    if (action == "verify_access") {
      cat(paste0("\nVerifying access to ", tables, " ..."))
      qry = paste0("select '1' from ", theschema, ".", 
                   gsub(paste0(prefix, "."), "", tables), " WHERE ROWNUM<=1")
      if (is.character(thecmd(oracle_cxn, qry, rows_at_time = 1))) {
        cat(" failed")
        return(FALSE)
      }
      else {
        cat(" success")
        return(TRUE)
      }
    }
    else if (action == "extract") {
      cat(paste0("\nExtracting ", tables, "... "))
      
      add.where = "1=1"
      if (tables %in% names(ds_all[[.GlobalEnv$db]]$table_err_roracle)) {
        this = ds_all[[.GlobalEnv$db]]$table_err_roracle[[tables]]
        badvalues = paste(unlist(gsub("(.*)", "\\1", 
                                      this$badvalues)), sep = "", collapse = ",")
        add.where = paste0(this$field, " NOT IN (", badvalues, 
                           ")")
        cat(paste0("\n\tSkipping records ", theschema, 
                   ".", tables, ".", this$field, " IN (", badvalues, ")\n
                   \tThis\\These are from ", this$comments, "\n                   
                   \tIf this is critical, use RODBC instead of ROracle\n"))
      }
      table_naked = table_naked1 = gsub(paste0(prefix, "."), "", tables)
      qry = paste0("SELECT * from ", theschema, ".", table_naked, 
                   " WHERE ", add.where)
      res= thecmd(oracle_cxn, qry, rows_at_time = 1)
      assign(table_naked, res)
      save(list = table_naked1, file = file.path(data.dir, paste0(tables, ".RData")))
      cat(paste("Got", tables))
    }
  }
  # extraction process --------------------------------------------------
  ## connect to oracle --------------------------------------------------
  ## verify access to reqd tables ---------------------------------------
  ## extract the tables -------------------------------------------------
  ## apply the necessary tweaks -----------------------------------------
  try_extract <- function(usepkg, tables) {
    oracle_cxn = Mar.utils::make_oracle_cxn(usepkg, fn.oracle.username, fn.oracle.password, fn.oracle.dsn)
    if (!is.list(oracle_cxn)) {
      tries = 0
      while (tries < 2 & !(is.list(oracle_cxn))) {
        oracle_cxn = Mar.utils::make_oracle_cxn(usepkg, fn.oracle.username, fn.oracle.password, fn.oracle.dsn)
        tries = tries + 1
      }
      if (oracle_cxn == -1) 
        stop("Please check your credentials")
    }
    verified = sapply(tables, oracle_activity, oracle_cxn[[2]], 
                      oracle_cxn[[1]], ds_all[[.GlobalEnv$db]]$schema, 
                      toupper(db), "verify_access")
    if (!all(verified)) 
      stop("You do not have access to all of the required tables.\n      
Please ask the db custodian to grant you access to the tables listed above, and try again.\n\n      ")
    dir.create(data.dir, recursive = TRUE, showWarnings = FALSE)
    cat("\n\nStarting extractions... ")
    timer.start = proc.time()
    sapply(tables, oracle_activity, oracle_cxn[[2]], oracle_cxn[[1]], ds_all[[.GlobalEnv$db]]$schema, toupper(db), "extract")
     elapsed = timer.start - proc.time()
    cat(paste("\n\nExtraction completed in", round(elapsed[3], 0) * -1, "seconds"))
    data_tweaks(data.dir = data.dir)
  }
  # load the data -------------------------------------------------------
  try_load <- function(tables, data.dir) {
    loadit <- function(x, data.dir) {
      this = paste0(x,".RData")
      thisP = file.path(data.dir,this)
      load(file =thisP, envir = .GlobalEnv)
      cat(paste0("\nLoaded ", x, "... "))
      fileAge = file.info(thisP)$mtime
      fileAge = round(difftime(Sys.time(), fileAge, units = "days"), 0)
      cat(paste0(" (Data modified ", fileAge, " days ago.)"))
      if (fileAge > 90) 
        cat(paste("\n!!! This data was extracted more than 90 days ago - consider re-extracting it"))
    }
    cat("\nLoading data...\n")
    timer.start = proc.time()
    sapply(tables, simplify = TRUE, loadit, data.dir)
    elapsed = timer.start - proc.time()
    cat(paste0("\n\n", round(elapsed[3], 0) * -1, " seconds to load...\n"))
  }
  # run the function/decide what to do ----------------------------------
  reqd = paste0(toupper(.GlobalEnv$db), ".", ds_all[[.GlobalEnv$db]]$tables)
  if (dir.exists(data.dir)==TRUE){
    status = local_table_status_check()
    if (length(status[[1]]) == 0 & force.extract == F) {
      try_load(reqd, data.dir)
    } else if (length(status[[1]]) == 0 & force.extract == T) {
      try_extract(usepkg, reqd)
      try_load(reqd, data.dir)
    } else {
      cat(paste0("\nLooked in '", data.dir, "' for required *.rdata files, but you are missing the following:\n"))
      cat(status[[1]])
      choice = toupper(readline(prompt = "Press 'c' to (c)ancel this request, 'a' to re-extract (a)ll of the tables for\nthis datasource, or any other key to extract the missing data only\n(case-insensitive)"))
      print(choice)
      if (toupper(choice) == "C") {
        stop("Cancelled.   (Maybe check that your working directory is set to the folder *containing* your data folder and try again)")
      }
      else if (toupper(choice) == "A") {
        try_extract(usepkg, reqd)
        try_load(reqd, data.dir)
      }
      else {
        try_extract(usepkg, status[[1]])
        try_load(reqd, data.dir)
      }
    }
  }else{
    cat(paste0("\nWarning: The specified data.dir ('", data.dir, "') does not exist.\n"))
    goahead = toupper(readline(prompt = "Type 'y' to create this folder and extract the data into it.  Press any other key to cancel the operation. \n"))
    print(goahead)
    if (toupper(goahead) != "Y") {
      stop("Cancelled.")
    }else {
      dir.create(data.dir)
      try_extract(usepkg, reqd)
      try_load(reqd, data.dir)
    }
  }
}
