#' @title get_data_custom
#' @description This function allows users to extract other tables within a
#' known database that the script does not get by default.  They must have 
#' access to the tables to extract them.
#' @param schema default is \code{NULL}. This is the schema you want to access 
#' a additional tables from.
#' @param tables The default value is \code{NULL}.  This is a vector of table 
#' names you want to extract that exist in the database specified by \code{db}.
#' @param usepkg default is \code{oracle_cxn$usepkg}. This indicates whether the 
#' connection to Oracle should use \code{'rodbc'} or \code{'roracle'} to 
#' connect.  rodbc is slightly easier to setup, but roracle will extract data ~ 
#' 5x faster. Deprecated; use \code{cxn} instead.
#' @param cxn A valid Oracle connection object. This parameter allows you to 
#' pass an existing connection, reducing the need to establish a new connection 
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
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
#' @param quiet default is \code{FALSE}.  If TRUE, no output messages will be shown.
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_data_custom<-function(schema=NULL,
                          tables = NULL,
                          cxn = NULL,
                          env=.GlobalEnv,
                          quiet=F){

  .Deprecated(old = "get_data_custom",new = "Mar.utils::get_data_tables", package="Mar.utils")

  try_load <- function(tables, thisenv = env) {
    loadit <- function(x) {
      this = paste0(x, ".RData")
      thisP = file.path(get_pesd_dw_dir(), this)
      if (!file.exists(thisP) & file.exists(gsub(x= thisP,pattern = "MARFISSCI",replacement ="MARFIS",ignore.case = T))) thisP = gsub(x= thisP,pattern = "MARFISSCI",replacement ="MARFIS",ignore.case = T)
      if (!file.exists(thisP) & file.exists(gsub(x= thisP,pattern = "GROUNDFISH",replacement ="RV",ignore.case = T))) thisP = gsub(x= thisP,pattern = "GROUNDFISH",replacement ="RV",ignore.case = T)
      if (!file.exists(thisP) & file.exists(gsub(x= thisP,pattern = "OBSERVER",replacement ="ISDB",ignore.case = T))) thisP = gsub(x= thisP,pattern = "OBSERVER",replacement ="ISDB",ignore.case = T)
      
      Mar.utils::load_encrypted(file = thisP,envir = env, ...)
      if (!quiet) cat(paste0("\nLoaded ", x, "... "))
      fileAge = file.info(thisP)$mtime
      fileAge = round(difftime(Sys.time(), fileAge, units = "days"), 0)
      if (!quiet) cat(paste0(" (Data modified ", fileAge, " days ago.)"))
      if ((!quiet)  & fileAge > 90) 
        cat(paste("\n!!! This data was extracted more than 90 days ago - consider re-extracting it"))
    }
    if (!quiet) cat("\nLoading data...")
    timer.start = proc.time()
    sapply(tables, simplify = TRUE, loadit)
    elapsed = timer.start - proc.time()
    if (!quiet) cat(paste0("\n\n", round(elapsed[3], 0) * -1, " seconds to load..."))
  }
  
  reqd = toupper(paste0(schema, ".", tables))
  loadsuccess = tryCatch(
    {
      try_load(reqd)
    }, 
    warning = function(w) {
      print()
    },
    error=function(cond){
      return(-1)
    }
  )
  if (is.null(loadsuccess)){
    return(invisible(NULL))
  } else if (loadsuccess==-1){
    if (is.null(cxn)) {
      oracle_cxn_custom = Mar.utils::make_oracle_cxn(usepkg, fn.oracle.username, fn.oracle.password, fn.oracle.dsn)  
      if (!inherits(oracle_cxn_custom, "list")) {
        cat("\nCan't get the data without a DB connection. Aborting.\n")
        return(NULL)
      }
      cxn = oracle_cxn_custom$channel
      thecmd = oracle_cxn_custom$thecmd
    } else {
      thecmd = Mar.utils::connectionCheck(cxn)
    }
    prefix=theschema=toupper(schema)
    for (i in 1:length(tables)){
      if (!quiet) cat(paste0("\n","Verifying access to ",tables[i]," ..."))
      qry = paste0("select '1' from ",theschema,".",gsub(paste0(prefix,"."),"",tables[i])," WHERE ROWNUM<=1")
      if (is.character(thecmd(cxn, qry, rows_at_time = 1))) {
        break("Can't find or access specified table")
      }else{
        cat(paste0("\n","Extracting ",tables[i],"..."))
        table_naked = gsub(paste0(prefix,"."),"",tables[i])
        table_naked1 = table_naked
        qry = paste0("SELECT * from ", theschema, ".",table_naked)
        res = thecmd(cxn, qry, rows_at_time = 1)
        assign(table_naked, res)
        Mar.utils::save_encrypted(list = table_naked1, file = file.path(get_pesd_dw_dir(), paste0(prefix,".",tables[i],".RData")))
        if (!quiet) cat(paste("\n","Got", tables[i]))
        assign(x = tables[i],value = get(table_naked), envir = env)
        if (!quiet) cat(paste0("\n","Loaded ",tables[i]))
      }
    }
  }
}