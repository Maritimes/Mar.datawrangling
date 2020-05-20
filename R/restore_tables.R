#' @title restore_tables
#' @description This function retrieves all of the tables stored by save_tables() and restores them
#' to the global environment.  Existing tables with the same names will be overwritten.
#' @param db default is \code{NULL}. This identifies the dataset you are working 
#' with.
#' @return nothing
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
restore_tables <- function(db=NULL){
  if (is.null(db)){
    cat("\nNothing restored.  Please specify the db.")
  } else{
    if(!exists("dw")){
      cat("\nsave_tables() must be run before restore_tables() can be run (Missing 'dw').\n")
    }else{
      sapply(ds_all[[db]]$tables, USE.NAMES = F, simplify = TRUE, function(x) {
        assign(x,value = get(paste0("tmp_",x), envir = dw), envir = .GlobalEnv)
      }
      ) 
      rm(dw, envir = .GlobalEnv)
    }
  }
  return(invisible())
}
