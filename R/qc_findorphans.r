#' @title qc_findorphans
#' @description This function creates zzz_orph_* tables with records that don't
#' appear to be linked to the other tables (within Mar.datawrangling)
#' @param cxn A valid Oracle connection object. This parameter allows you to 
#' pass an existing connection, reducing the need to establish a new connection 
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
#' @param db default is \code{NULL}. This identifies the dataset you are working
#' with.
#' @param extract_user default is \code{NULL}.  This parameter can be used with
#' \code{extract_computer} to load encypted data files extracted by another user 
#' and/or computer
#' @param extract_computer  default is \code{NULL}.  This parameter can be used with
#' \code{extract_user} to load encypted data files extracted by another user 
#' and/or computer
#' @param debug default = \code{FALSE}.  This is passed to self_filter() which 
#' causes each filter to be printed out.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
qc_findorphans<-function(cxn = NULL, db = NULL, extract_user= NULL,  extract_computer = NULL, debug=F){
  
  if (is.null(db)) stop("Please supply a value for db")
  
  prefix = toupper(get_ds_all()[[.GlobalEnv$db]]$schema)
  name_prefix = "zzz_orph_"
  get_data(cxn = cxn, db=db, extract_user = extract_user, extract_computer = extract_computer)
  self_filter(db, looponce=TRUE, debug = debug)
  cat("\n","Orphans within code tables are generally expected.
This will take a moment...")
  get_orphans <- function(this){
    x <- rbind(get(this, envir = orig), get(this, envir = .GlobalEnv))
    x = x[! duplicated(x, fromLast=TRUE) & seq(nrow(x)) <= nrow(get(this, envir = orig)), ]
    assign(paste0(name_prefix,this), x, envir = .GlobalEnv)
    cat(paste0("\n","Created ",name_prefix,this," ..."))
  }
  
  these.tables.prefixed = paste0(prefix,".",get_ds_all()[[.GlobalEnv$db]]$tables)
  orig = new.env()

  for (i in 1:length(these.tables.prefixed)){
    this = paste0(these.tables.prefixed[i],".RData")
    thisP = file.path(get_pesd_dw_dir(),this)
    Mar.utils::load_encrypted(file =thisP, envir = orig, 
                              extract_user = extract_user, extract_computer = extract_computer)
  }
  sapply(get_ds_all()[[.GlobalEnv$db]]$tables, get_orphans)
  return(invisible())
}

