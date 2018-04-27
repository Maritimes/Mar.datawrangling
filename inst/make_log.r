#' @title make_log
#' @description This function tracks the users' filtering selections by writing 
#' them to an object called "activity_log".  It tracks when the data was 
#' loaded, as well as what filters were applied.
#' @param refresh.data default is \code{FALSE}.  If TRUE, the existing activity_
#' log will be erased and replaced with a new one.
#' @return a list of lists describing what filters have been applied to the data
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
make_log = function(refresh.data=FALSE){
  new_log<-function(){
    activity_log = list()
    this.filter = paste0('filter_0')
    activity_log[["Metadata"]] = "Metadata"
    activity_log[["Metadata"]][["LoadTime"]] = strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
    activity_log[["Metadata"]][["Dataset"]] = db
    activity_log[["Metadata"]][["tables"]] = list(ds_all[[.GlobalEnv$db]]$tables)
    activity_log[["Metadata"]][["schema"]] = ds_all[[.GlobalEnv$db]]$schema
    activity_log[['filterno']] = 1
    assign("activity_log", activity_log, envir = .GlobalEnv)
  }
  
  append_log<-function(){
    iter = activity_log[['filterno']]+1
    activity_log[['filterno']]=iter
    this.filter = paste0('filter_',iter)
    activity_log[[this.filter]][["RunTime"]]= strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
    assign("activity_log", activity_log, envir = .GlobalEnv)
    return(this.filter)
  }
  if (exists('activity_log')){
    if(activity_log[["Metadata"]][["Dataset"]] != db| refresh.data == TRUE) {
      rm(activity_log, envir = .GlobalEnv)
      new_log()
    }
    append_log()
  }else{
    new_log()
  }
}