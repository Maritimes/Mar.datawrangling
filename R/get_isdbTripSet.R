#' @title get_isdbTripSet
#' @description This function takes  one or more FISHSET_IDs and returns the 
#' name and set number for the FISHSET_ID.
#' @param FS_ID default is \code{NULL}. This is one or more FISHSET_IDs for 
#' which you want the associated trip name and set number.
#' @return DataFrame
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_isdbTripSet <- function(FS_ID=NULL){
  FS = ISFISHSETS[ISFISHSETS$FISHSET_ID %in% FS_ID,c("FISHSET_ID","TRIP_ID", "SET_NO")]
  TR = ISTRIPS[ISTRIPS$TRIP_ID %in% FS$TRIP_ID, c("TRIP_ID","TRIP")]
  res=merge(FS,TR)
  res = res[, c("FISHSET_ID","TRIP","SET_NO")]
  return(res)
}