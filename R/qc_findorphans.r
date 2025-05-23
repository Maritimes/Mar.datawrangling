#' @title qc_findorphans
#' @description This function creates zzz_orph_* tables with records that don't
#' appear to be linked to the other tables (within Mar.datawrangling)
#' @param db default is \code{NULL}. This identifies the dataset you are working
#' with.
#' @param usepkg default is \code{'rodbc'}. This indicates whether the connection to Oracle should
#' use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is slightly easier to setup, but
#' roracle will extract data ~ 5x faster. Deprecated; use \code{cxn} instead.
#' @param debug default = \code{FALSE}.  This is passed to self_filter() which 
#' causes each filter to be printed out.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
qc_findorphans<-function(db = NULL, usepkg = 'rodbc', debug=F){
  Mar.utils::deprecationCheck(fn.oracle.username = fn.oracle.username, 
                   fn.oracle.password = fn.oracle.password, 
                   fn.oracle.dsn = fn.oracle.dsn,
                   usepkg = usepkg)
  
  if (is.null(db)) stop("Please supply a value for db")
  
  prefix = toupper(db)
  name_prefix = "zzz_orph_"
  get_data(db, usepkg = usepkg)
  self_filter(db, looponce=TRUE, debug = debug)
  cat("\n","Orphans within code tables are generally expected.
This will take a moment...")
  get_orphans <- function(this){
    x <- rbind(get(this, envir = orig), get(this, envir = .GlobalEnv))
    x = x[! duplicated(x, fromLast=TRUE) & seq(nrow(x)) <= nrow(get(this, envir = orig)), ]
    assign(paste0(name_prefix,this), x, envir = .GlobalEnv)
    cat(paste0("\n","Created ",name_prefix,this," ..."))
  }
  
  these.tables.prefixed = paste0(prefix,".",get_ds_all()[[db]]$tables)
  orig = new.env()

  for (i in 1:length(these.tables.prefixed)){
    this = paste0(these.tables.prefixed[i],".RData")
    thisP = file.path(get_pesd_dw_dir(),this)
    load(file =thisP, envir = orig)
  }
  sapply(get_ds_all()[[db]]$tables, get_orphans)
  return(invisible())
}

