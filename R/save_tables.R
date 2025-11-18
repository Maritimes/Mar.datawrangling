#' @title save_tables
#' @description This function takes a snapshot of all of the tables of a database as they currently
#' exist in your environment, and stores them in a new environment (i.e.'dw').
#' @param db default is \code{NULL}. This identifies the dataset you are working 
#' with.
#' @return nothing
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
save_tables <- function(db=NULL){
  if (is.null(db)){
    cat("\nNothing saved.  Please specify the db.")
  } else{
    .pkgenv$dw <- new.env()
    sapply(get_ds_all()[[.GlobalEnv$db]]$tables, USE.NAMES = F, simplify = TRUE, function(x) {
      assign(paste0("tmp_", x), value = get(x), envir = .pkgenv$dw)
    }
    ) 
  }
  return(invisible())
}
