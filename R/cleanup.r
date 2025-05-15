#' @title cleanup
#' @description This function removes the R objects associated with the various objects created
#' by each of the databases it connects to.
#' @param db default is \code{NULL}. This identifies the dataset you are working 
#' with.
#' @param var.like The default value is NULL.  Providing a value to this parameter will remove ALL
#' elements from your environment that match it.  Be careful and be as specific as you can.
#' @param env This the the environment you want this function to work in.  The 
#' default value is \code{.GlobalEnv}.
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
cleanup <- function(db=NULL, var.like = NULL, env=.GlobalEnv){
 
  if (!is.null(var.like)) {
    rm(list = ls(pattern=var.like, envir = env), envir = env)
  } 
  if (!is.null(db)) {
    db = tolower(db)
    tables = get_ds_all()[[db]]$tables
    if (exists(as.character(paste0("zzz_orph_",get_ds_all()[[db]]$tables[1]))))tables = c(tables, paste0("zzz_orph_",ds_all[[db]]$tables))
    rm(list = tables, envir = env)
  }
  
}