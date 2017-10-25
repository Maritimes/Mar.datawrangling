#' @title prepare_shape_fields
#' @description Shapefiles are limited in the naming conventions of the fields - they can only
#' be 10 characters, and must be devoid of special characters that are allowed in R.  This function
#' attempts to ensure that the shortened names that result from calls to writeOGR() can still be 
#' understood and are not simply truncated.
#' @param shape a dataframe or spatial object whose fieldnames are to be processed
#' @return a datafraame - spatial or otherwise
#' @family general_use
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom stats ave
#' @export
prepare_shape_fields <- function(shape){
  names(shape) = gsub('\\.','_', names(shape))
  #do something about names that are more than 10 characters long (not allowed by shapefile)
  
  #check if there's a problem - are all names 10 chars or less, and unique?
  field.names.ok <-function(fields){
    if (length(fields[nchar(fields)>10])>0 | length(fields) != length(unique(fields))){
      return(FALSE)
    }else{
      return(TRUE)
    }

  }
  names(shape) = gsub('COMBINED','COMB', names(shape))
  names(shape) = gsub('CAUGHT','CGHT', names(shape))
  names(shape) = gsub('LICENSE','LIC', names(shape))
  names(shape) = gsub('VESSEL','VES', names(shape))
  names(shape) = gsub('MARFIS','MARF', names(shape))
  
  #see if removing the underscores from fields with > 10 chars fixes it
  if (field.names.ok(gsub('_','', names(shape)[nchar(names(shape))>10]))){
    names(shape)[nchar(names(shape))>10]= gsub('_','', names(shape)[nchar(names(shape))>10])
  }
  #see if trimming fields with > 10 chars fixes it
  if (field.names.ok(substr(names(shape)[nchar(names(shape))>10],1,10))){
    names(shape)[nchar(names(shape))>10]= substr(names(shape)[nchar(names(shape))>10],1,10)
  }

  if (!field.names.ok(names(shape))){
    cutpo = regexpr("_[^_]*$",  names(shape)[nchar(names(shape))>10])
    nch = nchar(names(shape)[nchar(names(shape))>10])
    #get everything after last underscore
    ext=substr(names(shape)[nchar(names(shape))>10],cutpo+1, nchar(names(shape)[nchar(names(shape))>10]))
    #get everything before underscore, remove all underscores, and trim it enough so that when the ext
    #is added, its only 10 chars
    start =  substr(gsub('_','', substr(names(shape)[nchar(names(shape))>10], 1, cutpo-1)),1,10-nchar(ext))
    names(shape)[nchar(names(shape))>10] = paste0(start,ext)
    #if not sufficient writeOGR will make modifications, but we've saved the critical parts
  }
  #still have duplicates? remove last character, and replace with integer
  if (!field.names.ok(names(shape))){
    names(shape) = ave(as.character(names(shape)), names(shape), 
       FUN=function(x) if (length(x)>1) paste0(substr(x[1],1,nchar(x[1])-1), seq_along(x)) else x[1])
  }

  return(shape)
}