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
#' @param env This the the environment you want this function to work in.  The
#' default value is \code{.GlobalEnv}.
#' @param keep_nullsets default is \code{TRUE}  If you're working the data that can
#' be extracted via this package, it can be useful to know where fishing
#' occurred that did not result in catches.  Note that for industry fishing
#' (i.e. ISDB, MARFIS, COMLAND**), this will just ensure that all sets matching
#' your criteria are retained.  If you have selected data by the "caught
#' species", you will have already exluded the nullsets.
#' @param quiet default is \code{FALSE}.  If True, the filtering process will
#' not show text describing progress.
#' @param debug default is \code{FALSE}.  This is used for debugging, and will
#' cause the filtering commands to be printed out.
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' @note This line is here to prevent an error message
self_filter <-
  function(db = NULL,
           looponce = FALSE,
           debug = FALSE,
           env = .GlobalEnv,
           keep_nullsets = TRUE,
           quiet= FALSE) {
    if (is.null(db)) db = ds_all[[.GlobalEnv$db]]$db
    loopagain = TRUE
    loopLast = FALSE
    if (!quiet) cat("\n","Filtering...")
    
    timer.start = proc.time()
    prefix = toupper(db)
    catchTable = ds_all[[.GlobalEnv$db]]$table_cat
    
    posTable = ds_all[[.GlobalEnv$db]]$table_pos
    
    if (!quiet) cat("\n","Records remaining in each table after each loop:","\n")
    get_joiner = function(combine) {
      if (is.null(combine)) combine = "missing"
      switch(combine,
             "ALL" = " & ",
             "OR" = " | ",
             "missing" = " & ")
    }
    while (loopagain == TRUE) {
      count.pre = sum(sapply(sapply(catchTable, get, env), NROW))
      count.pre.all = sapply(sapply(ds_all[[.GlobalEnv$db]]$tables, get, env), NROW)
      precnt = sum(count.pre.all)
      if (!quiet) print(count.pre.all)
      for (i in 1:length(ds_all[[.GlobalEnv$db]]$joins)) {
        nullSetFlag = F
        tab_prim = names(ds_all[[.GlobalEnv$db]]$joins)[i]
        p_stuff = NULL
        f_stuff = NULL
        p_stuff_ns = NULL
        f_stuff_ns = NULL
        combine = NULL
        for (j in 1:length(names(ds_all[[.GlobalEnv$db]]$joins[[i]])[names(ds_all[[.GlobalEnv$db]]$joins[[i]]) != "combine"])) {
          if (j > 1) combine = ds_all[[.GlobalEnv$db]]$joins[[i]]$combine
          tab_foreign = names(ds_all[[.GlobalEnv$db]]$joins[[i]][j])
          if (nrow(get(tab_prim, envir = env)) == 0 & nrow(get(tab_foreign, envir = env)) == 0) {
            next
          }
          if (length(ds_all[[.GlobalEnv$db]]$joins[[i]][[j]]$pk_fields) > 1) {
            p_stuff1 = paste0("paste0(",paste0("env$", tab_prim, "$", ds_all[[.GlobalEnv$db]]$joins[[i]][[j]]$pk_fields, collapse = ",'_'," ),")")
            f_stuff1 = paste0("paste0(",paste0("env$", tab_foreign, "$", ds_all[[.GlobalEnv$db]]$joins[[i]][[j]]$fk_fields, collapse = ",'_',"),")")
          } else{
            p_stuff1 = c(paste0("env$", tab_prim, "$", ds_all[[.GlobalEnv$db]]$joins[[i]][[j]]$pk_fields))
            f_stuff1 = c(paste0("env$", tab_foreign, "$", ds_all[[.GlobalEnv$db]]$joins[[i]][[j]]$fk_fields))
          }
          if (tab_prim == catchTable & tab_foreign == posTable) {
            if (!keep_nullsets) nullSetFlag = T
            p_stuff_ns = p_stuff1
            f_stuff_ns = f_stuff1
          }
          p_stuff = c(p_stuff, p_stuff1)
          f_stuff = c(f_stuff, f_stuff1)
        }
        
        if (is.null(p_stuff)) {
          next
        }
        filt = paste(p_stuff, "%in%", f_stuff, collapse = get_joiner(combine))
        if (nullSetFlag) {
          filt2 = paste(f_stuff_ns, "%in%", p_stuff_ns, collapse = get_joiner(combine))
        }
        
        if (debug) {
          # cat(paste0("\n-----\n",tab_prim,":\n",filt,"\n"))
          tab_prim_n_0 = nrow(get(tab_prim, env))
        }
        if (ncol(get(tab_prim, env)) == 1) {
          theName = ds_all[[.GlobalEnv$db]]$joins[[i]][[j]]$pk_fields
          assign(tab_prim, as.data.frame(get(tab_prim, env)[eval(parse(text = filt)), ]), env)
          tmp_tab_prim <- get(tab_prim, env)
          colnames(tmp_tab_prim) <- theName
          assign(tab_prim, tmp_tab_prim, envir = env)
          if (nullSetFlag) {
            assign(posTable, get(posTable, env)[eval(parse(text = filt2)), ], env)
          }
        } else{
          assign(tab_prim, get(tab_prim, env)[eval(parse(text = filt)), ], env)
          if (nullSetFlag) {
            assign(posTable, get(posTable, env)[eval(parse(text = filt2)), ], env)
          }
        }
        
        if (debug) {
          tab_prim_n_1 = nrow(get(tab_prim, env))
          if (tab_prim_n_0 != tab_prim_n_1) {
            cat(paste0("\n-----\n", tab_prim, ":\n", filt, "\n"))
            cat(paste0(tab_prim_n_0, "  --> ", tab_prim_n_1, "\n"))
          }
        }
        p_stuff = NULL
        f_stuff = NULL
        p_stuff_ns = NULL
        f_stuff_ns = NULL
        nullSetFlag = F
        filt2 = NULL
      }
      
      count.post.all = sapply(sapply(ds_all[[.GlobalEnv$db]]$tables, get, env), NROW)
      postcnt = sum(count.post.all)
      count.post = sum(sapply(sapply(catchTable, get, env), NROW))
      if (postcnt == 0)
        if (!quiet) cat("\n","No data remains.")
      if ((precnt == postcnt & loopLast == TRUE) | looponce) {
        loopagain = FALSE
      } else if (count.pre == count.post) {
        loopLast = TRUE
      }
      if (!quiet) cat("--------------------\n")
    }
    if (!quiet) cat("\n","Filtering completed")
    elapsed = timer.start - proc.time()
    if (!quiet) cat(paste0("\n", round(elapsed[3], 0) * -1, " seconds elapsed"))
    return(invisible(NULL))
  }
