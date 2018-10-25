#' @title self_filter
#' @description This function draws upon the relationships between the tables 
#' reported in load_datasources() so that if one is filtered via the GUI, or via
#' a subset like \code{ISGEARCODES = ISGEARCODES[ISGEARCODES$GEARCD_ID==7,]}), 
#' all other tables will be filtered until the remaining records all relate to 
#' eachother.
#' For example, if the ISVESSELS table was filtered to only contain a single 
#' vessel - all other tables would be filtered until all remaing records in all 
#' tables relate directly to that vessel.
#' @param db default is \code{NULL}. This identifies the dataset you are working 
#' with.
#' @param looponce default is \code{NULL}.  This is only used for QC purposes.
#' @param debug default is \code{FALSE}.  This is used for debugging, and will
#' cause the filtering commands to be printed out.
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' @note This line is here to prevent an error message
self_filter <- function(db = NULL, looponce = FALSE, debug = FALSE, env=.GlobalEnv) {
  if (is.null(db))db = ds_all[[.GlobalEnv$db]]$db
  loopagain = TRUE
  loopLast = FALSE
  cat("\nFiltering...\n")
 
  timer.start = proc.time()
  prefix = toupper(db)
  catchTable = ds_all[[.GlobalEnv$db]]$table_cat

  cat("\nRecords remaining in each table after each loop:\n")
  get_joiner = function(combine){
    #beware that if no joiner is passed, it will default to requiring all

    if (is.null(combine)) combine = "missing"
    switch(combine,
           "ALL" = " & ",
           "OR" = " | ",
           "missing" = " & ")
  }
  while (loopagain == TRUE) {
    #only loop once (for QC)If there are scripts

    #as long as the number of rows in all tables is changes in a loop, filters are being applied
    #only stop looping when the rows are consistent

    #'Count the rows in all of the tables for this dataset

    count.pre = sum(sapply(sapply(catchTable, get, env), NROW))
    count.pre.all = sapply(sapply(ds_all[[.GlobalEnv$db]]$tables, get, env), NROW)
    print(count.pre.all)
    for (i in 1:length(ds_all[[.GlobalEnv$db]]$joins)){
      tab_prim=names(ds_all[[.GlobalEnv$db]]$joins)[i]
      p_stuff = NULL
      f_stuff = NULL
      combine = NULL
      for (j in 1:length(names(ds_all[[.GlobalEnv$db]]$joins[[i]])[names(ds_all[[.GlobalEnv$db]]$joins[[i]]) !="combine"])){
        if (j>1) combine = ds_all[[.GlobalEnv$db]]$joins[[i]]$combine
        tab_foreign = names(ds_all[[.GlobalEnv$db]]$joins[[i]][j])
        if (length(ds_all[[.GlobalEnv$db]]$joins[[i]][[j]]$pk_fields)>1){
          p_stuff1 = paste0("paste0(",paste0("env$",tab_prim,"$",ds_all[[.GlobalEnv$db]]$joins[[i]][[j]]$pk_fields, collapse=",'_',"),")")
          f_stuff1 = paste0("paste0(",paste0("env$",tab_foreign,"$",ds_all[[.GlobalEnv$db]]$joins[[i]][[j]]$fk_fields, collapse=",'_',"),")")
        }else{
          p_stuff1 = c(paste0("env$",tab_prim,"$",ds_all[[.GlobalEnv$db]]$joins[[i]][[j]]$pk_fields))
          f_stuff1 = c(paste0("env$",tab_foreign,"$",ds_all[[.GlobalEnv$db]]$joins[[i]][[j]]$fk_fields))
        }
      p_stuff = c(p_stuff, p_stuff1)
      f_stuff = c(f_stuff, f_stuff1)
      }
      filt= paste(p_stuff, "%in%", f_stuff, collapse=get_joiner(combine))
      if (debug) cat(paste0("\n-----\n",filt))
      if (debug & NROW(get(tab_prim,envir = env)[eval(parse(text = filt)),])==0)browser()
      if (ncol(get(tab_prim, env))==1){
        theName = ds_all[[.GlobalEnv$db]]$joins[[i]][[j]]$pk_fields

        assign(tab_prim, as.data.frame(get(tab_prim, env)[eval(parse(text = filt)),]), env)
        tmp_tab_prim<-get(tab_prim, env)
        colnames(tmp_tab_prim)<-theName
        assign(tab_prim,tmp_tab_prim, envir =env)
      }else{
        assign(tab_prim, get(tab_prim, env)[eval(parse(text = filt)),], env)
      }
      if (debug) cat(paste0("\n",tab_prim,": ",nrow(get(tab_prim, env)),"\n"))
      p_stuff = NULL
      f_stuff = NULL
    }

    count.post.all = sapply(sapply(.GlobalEnv$ds$tables, get, env), NROW)

    count.post = sum(sapply(sapply(catchTable, get, env), NROW))
    if (count.post == 0)
      stop("No data remains. To try again, run data_filter(db='x', refresh.data = TRUE) (GUI) or
data_load(list_tables(db='x')) to re-load the data")
    if ((count.pre == count.post & loopLast == TRUE) | looponce){
      loopagain = FALSE
    }else if (count.pre == count.post){
      loopLast=TRUE
    }
      # if (loopLast == TRUE) loopagain = FALSE
      # loopLast=TRUE
      # loopagain = TRUE

     cat("--------------------\n") 
  }
  cat("Filtering completed\n")
  elapsed = timer.start - proc.time()
  cat(paste0("\n",round(elapsed[3], 0) * -1," seconds elapsed\n"))
  return(invisible(NULL))
}
