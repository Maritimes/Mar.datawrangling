#' @title aggregator
#' @description This function takes a dataframe and the number of minutes to aggregate by, and
#' returns the mean, count and sum for the specified field
#' @param df a dataframe to be analyzed. If left \code{NULL}, a value for 
#' \code{db} should be provided
#' @param db default is \code{NULL}. This identifies the dataset you are working 
#' with.
#' @param lat.field the default is \code{"LATITUDE"}. the name of the field holding latitude values (in decimal degrees)
#' @param lon.field the default is \code{"LONGITUDE"}.  the name of the field holding longitude values (in decimal degrees)
#' @param sens.fields the defaults are \code{NULL}  These are fields
#' to which the "rule of 5" should be applied. The Treasury Secretariat states that when data is 
#' shown to the public, certain fields must have at least 5 unique values for these fields 
#' aggregated together. When run, this function will look at these fields, and calculate how many 
#' unique values exist for each.  It will then populate a field 'PRIVUNIQUE' with the minimum number 
#' of unique values of all the assessed fields. If this is 5 or more, a field called 'CAN_SHOW' will 
#' be marked as 'YES' (otherwise it will be 'No').
#' @param rule.of default is \code{5} Whether or not a polygon is flagged as "CAN_SHOW" or not 
#' depends on the presence of a threshold number of unique values for certain sensitive fields.  
#' This parameter sets that threshold.
#' @param agg.fields the default is \code{NULL}, but if a db is provided, they 
#' will be guessed.  These are the fields on which to aggregate (i.e. generate 
#' values of \code{MEAN}, \code{COUNT} and \code{SUM}).
#' @param agg.minutes the default is 5.  The number of minutes by which to aggregate.
#' @param quiet  the default is \code{FALSE}.  If TRUE, no processing messages will be shown.
#' @return a data frame
#' @family general_use
#' @importFrom stats aggregate
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note This function is not a replacement for your brain.  If sensitive fields have names that are 
#' different than what is provided in the \code{sen.fields}, they will not be detected, or 
#' included in the checks.  Please make very sure you correctly identify such fields.
#' @export
aggregator = function(df = NULL,
                      db = NULL,
                      lat.field = "LATITUDE",
                      lon.field = "LONGITUDE",
                      rule.of = 5,
                      agg.fields = NULL,
                      sens.fields = NULL,
                      agg.minutes = 5,
                      quiet = FALSE) {

    if (is.null(df) & is.null(db)){
      df = summarize_catches(ds_all[[.GlobalEnv$db]]$db)
      if (is.null(sens.fields)) sens.fields = ds_all[[.GlobalEnv$db]]$field_private
      if (is.null(agg.fields)) agg.fields = ds_all[[.GlobalEnv$db]]$field_default
    }
  
  agg = agg.minutes / 60

  df = df[!is.na(df[lat.field]) & !is.na(df[lon.field]), ]

  df$LATITUDE = (round(df[, lat.field] / agg) * agg) #+ 0.5 * agg
  df$LONGITUDE = (round(df[, lon.field] / agg) * agg) #- 0.5 * agg

  #about to do analytics on specified field, remove NAs and convert to number
  for (i in 1:length(agg.fields)) {
    if (nrow(df[is.na(df[, agg.fields[i]]), ]) > 0)
      df[is.na(df[, agg.fields[i]]), ][, agg.fields[i]] <- 0
  }
  df[agg.fields] <- sapply(df[agg.fields], as.numeric)
  df.agg = as.data.frame(as.list(aggregate(
    df[agg.fields],
    by = df[c("LATITUDE", "LONGITUDE")],
    FUN = function(x)
      c(
        MEAN = round(mean(x), 4),
        CNT = round(length(x), 4),
        SUM = round(sum(x), 4)
      )
  )))
  #finished data aggregation - now aggregate by sensitive fields (i.e. get count of unique values of
  #each sensitive field)
  if (length(intersect(sens.fields, colnames(df)))>0){
  df.agg.sens = as.data.frame(as.list(aggregate(
    df[intersect(sens.fields, colnames(df))],
    by = df[c("LATITUDE", "LONGITUDE")],
    FUN = function(x)
      c(
        CNT = round(length(unique(x)), 4)
      )
  )))
  privcolnum = ncol(df.agg.sens)
  if (privcolnum==3){
    df.agg.sens$TOTUNIQUE = min(df.agg.sens[,3])
  }else if (privcolnum>3){
    df.agg.sens$TOTUNIQUE = apply(df.agg.sens[,3:privcolnum], 1, min)
  } else{
    cat("You should never see this")
  }
  df.agg.sens$CAN_SHOW = "No"
  df.agg.sens$CAN_SHOW[df.agg.sens$TOTUNIQUE >= rule.of]<- "Yes"
  df.agg = merge(df.agg, df.agg.sens)
  }else{
    if (!quiet) cat("\nNo sensitive fields detected or processed")
  }
  return(df.agg)
}
