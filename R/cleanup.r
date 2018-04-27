#' @title cleanup
#' @description This function removes the R objects associated with the various objects created
#' by each of the databases it connects to.
#' @param db default is \code{NULL}. This identifies the dataset you are working 
#' with.
#' @param var.like The default value is NULL.  Providing a value to this parameter will remove ALL
#' elements from your environment that match it.  Be careful and be as specific as you can.
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
cleanup <- function(db=NULL, var.like = NULL){
  
  if (!is.null(var.like)) {
    rm(list = ls(pattern=var.like, envir = .GlobalEnv), envir = .GlobalEnv)
  } 
  if (!is.null(db)) {
    tables = ds_all[[db]]$tables
    if (exists(as.character(paste0("zzz_orph_",ds_all[[db]]$tables[1]))))tables = c(tables, paste0("zzz_orph_",ds_all[[db]]$tables))
    rm(list = tables, envir = .GlobalEnv)
  }
  
}