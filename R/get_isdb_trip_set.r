#' @title get_isdb_trip_set
#' @description This function takes  one or more FISHSET_IDs and returns the 
#' name and set number for the FISHSET_ID.
#' @param FS_ID default is \code{NULL}. This is one or more FISHSET_IDs for 
#' which you want the associated trip name and set number.
#' @param data.dir default is \code{NULL}. This is the folder that contains the
#' extracted RData files for ISDB.  Specifically, it should contain both 
#' ISDB.ISTRIPS.RData and ISDB.ISFISHSETS.RData.
#' @return DataFrame
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_isdb_trip_set <- function(FS_ID=NULL, data.dir=NULL){
  tmp=new.env()
  fs="ISDB.ISFISHSETS.RData"
  tr="ISDB.ISTRIPS.RData"
  if (!is.null(data.dir)){
    data.dir<-sub("/$","",sub("\\$","",trimws(data.dir)))  
    if (file.exists(file.path(data.dir,"data",fs)) & file.exists(file.path(data.dir,"data",tr))){
      fs = file.path(data.dir,"data",fs)
      tr = file.path(data.dir,"data",tr)
    }else if (file.exists(file.path(data.dir,fs)) & file.exists(file.path(data.dir,tr))){
      fs = file.path(data.dir,fs)
      tr = file.path(data.dir,tr)
    }
    load(fs, envir = tmp)
    load(tr, envir = tmp)
  }else if (all(c(exists("ISFISHSETS"),exists("ISTRIPS")))){
    data(list = list("ISDB.ISFISHSETS", "ISDB.ISTRIPS"), envir = tmp)
  }else{
    cat("\n","Please include a value for data.dir that contains your extracted RData files for ISDB")
    stop()
  }
    
  
  FS = tmp$ISFISHSETS[tmp$ISFISHSETS$FISHSET_ID %in% FS_ID,c("FISHSET_ID","TRIP_ID", "SET_NO")]
  TR = tmp$ISTRIPS[tmp$ISTRIPS$TRIP_ID %in% FS$TRIP_ID, c("TRIP_ID","TRIP")]
  res=merge(FS,TR)
  res = res[, c("FISHSET_ID","TRIP_ID","TRIP","SET_NO")]
  tmp<-NULL
  return(res)
}