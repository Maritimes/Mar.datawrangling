#' @title giddyup
#' @description This function walks people through an extraction, by helping select a database, and
#' and R/Oracle connection package.
#' @param db default is \code{NULL}. This identifies the dataset you are working 
#' with.
#' @param usepkg default is \code{'rodbc'}. his indicates whether the connection to Oracle should
#' use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is slightly easier to setup, but
#' roracle will extract data ~ 5x faster.
#' @param force.extract The default value is FALSE.  By default, existing data will be loaded.  If
#' \code{force.extract ==TRUE}, than a full extraction will take place, overwriting any existing
#' data.
#' @param QC The default value is FALSE.  In most large datasets are 'orphan' records that can't be
#' joined to other records.  These are normally ignored and discarded. If  \code{QC == TRUE}, they
#' will be retained so that they can be investigated.
#' \code{force.extract ==TRUE}, than a full extraction will take place, overwriting any existing
#' data.
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' @note When prompted for a way to connect to Oracle, most people will select 'RODBC' since it's
#' easier to install, but ROracle is MUCH faster.
giddyup <- function(db=NULL, usepkg = NULL, force.extract = FALSE, QC = FALSE){
  if (QC == TRUE) force.extract = TRUE
  if (!is.null(db)){
    get_data(db = db, usepkg = usepkg, force.extract = force.extract)
    data_filter(db)
  }else{
    ds_nms = data.frame(list(names=unlist(lapply(ds_all,'[[','name')),
                             db = names(unlist(lapply(ds_all,'[[','db'))),
                             desc = unlist(lapply(ds_all,'[[','desc'))))

    selected.db = select.list(
      c(as.character(ds_nms$names),
        'Cancel'),
      multiple = F,
      graphics = T,
      title = 'Please select a data source'
    )
    
    if (selected.db == 'Cancel') return("Cancelled at user request")
    
    cat(paste0("You selected: ",selected.db, " (identified in the package as '", db,"')"))
    cat(paste0("\nThis data source is described as:\n"))
    cat(ds_nms[ds_nms$names ==selected.db,]$desc)
    continue = toupper(readline(prompt = "Press X to choose a different data set, or any other key to continue with this selection: "))
    
    print(continue)
    if (continue == 'X') {
      giddyup(force.extract = force.extract, usepkg = usepkg)
    } else {
      get_data(db = ds_nms[ds_nms$names ==selected.db,]$db, usepkg = usepkg, force.extract = force.extract)
      data_filter(ds_nms[ds_nms$names ==selected.db,]$db)
    }
  }
}
