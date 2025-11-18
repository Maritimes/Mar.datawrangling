#' @title restore_tables
#' @description This function retrieves all of the tables stored by save_tables() and restores them
#' to the global environment.  Existing tables with the same names will be overwritten.
#' @param db default is \code{NULL}. This identifies the dataset you are working 
#' with.
#' @param clean default is \code{TRUE}. If this is true, the temporary environment that held the 
#' tables will be removed when the tables are restored.  If it is false, the environment will not be 
#' removed when the data is loaded.
#' @return nothing
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
restore_tables <- function(db=NULL, clean="TRUE"){
  if (is.null(db)) {
    cat("\nNothing restored. Please specify the db.")
  } else {
    if (!exists("dw", envir = .pkgenv)) {
      cat("\nsave_tables() must be run before restore_tables() can be run (Missing 'dw').\n")
    } else {
      sapply(get_ds_all()[[.GlobalEnv$db]]$tables, USE.NAMES = F, simplify = TRUE, function(x) {
        assign(x, value = get(paste0("tmp_", x), envir = .pkgenv$dw), envir = .GlobalEnv)
      })
      if (clean) rm("dw", envir = .pkgenv)
    }
  }
  return(invisible())
}
