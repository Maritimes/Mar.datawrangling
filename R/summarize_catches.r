#' @title summarize_catches
#' @description This function joins together all the rdata files into a single
#' data frame.
#' @param db default is \code{NULL}. This identifies the dataset you are working 
#' with.
#' @param valid.coords default is FALSE.  This uses 
#' \code{Mar.utils::df.qc.spatial()} to remove coordinates that are not valid.  
#' If TRUE and no valid records remain, this function will abort.
#' @param morph_dets default is FALSE.  For some databases, more detailed
#' records can be added for each catch (e.g. gsdet for 'rv' data).  This flag
#' indicates that the detailed records will be returned.  This means that every
#' catch location might have many records (e.g. length and weight data for all
#' measured fish in a tow). This generally only desired when individual fish
#' properties are needed (e.g. length, weight)
#' @param gear_dets default is FALSE.  For some databases (eg isdb), more
#' detailed records can be added for each gear.  This means that a single catch
#' will be duplicated multiple times - once for each new gear parameter (e.g.
#' mesh shape, door height, etc).  If there's a good reason to show this with
#' catch data, I can't think of it.
#' @param quiet default is \code{FALSE}.  If coordinates are required
#' (\code{valid.coords=TRUE}), the script default is to report issues to the
#' user.  If \code{quiet=TRUE}, these notifications are not shown.
#' @param drop.na.cols default is \code{TRUE}.  If TRUE, any columns that are
#' do not have any values in them will be dropped.
#' @param debug default is \code{FALSE}.  If TRUE, messages describing the 
#' merging steps will be printed out
#' @param env This the the environment you want this function to work in.  The 
#' default value is \code{.GlobalEnv}.
#' @return a data frame, and if \code{req.coords=TRUE}, a message indicating how
#' many positions were lost due to the constraint.
#' @importFrom Mar.utils df_qc_spatial
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
summarize_catches <- function(db=NULL,
                              morph_dets = FALSE,
                              gear_dets = FALSE,
                              valid.coords = FALSE,
                              quiet = FALSE,
                              drop.na.cols = TRUE,
                              debug=FALSE,
                              env=.GlobalEnv){
  if (is.null(db))db = ds_all[[.GlobalEnv$db]]$db
  #create place to make a bunch of temporary duplicate data
  summ = new.env()
  drop_fields <- function(table.name, sens.fields){
    table = get(table.name,envir = env)
    assign(table.name,(unique(table[,!(names(table) %in% sens.fields)])), envir = summ)
  }
  #drop certain fields from all tables
  drops = ds_all[[.GlobalEnv$db]]$field_drops
  sapply(ds_all[[.GlobalEnv$db]]$tables,drop_fields, drops)
  tab_prim = ds_all[[.GlobalEnv$db]]$table_pos
  tab_foreign = ds_all[[.GlobalEnv$db]]$joins[[tab_prim]][names(ds_all[[.GlobalEnv$db]]$joins[[tab_prim]]) !="combine"]
  all_recs=summ[[tab_prim]]
  joiners = ds_all[[.GlobalEnv$db]]$tables[!(ds_all[[.GlobalEnv$db]]$tables %in% c(names(tab_foreign), tab_prim))]
  doMerge = function(this_tab_prim_data, this_tab_foreign, morph_dets, debug){
    
    this_tab_foreign_nm = names(this_tab_foreign)
    if (!is.null(ds_all[[.GlobalEnv$db]]$table_det)){
      if (morph_dets == FALSE & this_tab_foreign_nm %in% ds_all[[.GlobalEnv$db]]$table_det) {
        if (debug) cat(paste0("Skipping merge of ",ds_all[[.GlobalEnv$db]]$table_det,"\n"))
        return(this_tab_prim_data)
      }else{

      }
    }
    if (!is.null(ds_all[[.GlobalEnv$db]]$table_gear)){
      if (gear_dets == FALSE & this_tab_foreign_nm %in% ds_all[[.GlobalEnv$db]]$table_gear) return(this_tab_prim_data)
    }
    this_tab_foreign_dets = this_tab_foreign[[1]]
    this_tab_foreign = summ[[this_tab_foreign_nm]]
    if (NROW(this_tab_foreign)==0 | NCOL(this_tab_foreign)<2)return(this_tab_prim_data)
    for (j in 1:length(this_tab_foreign_dets$pk_fields)){
      #duplicate key fields for sole purpose of merging
      this_tab_prim_data[paste0("joinx_",j)]=this_tab_prim_data[this_tab_foreign_dets$pk_fields[j]]
      this_tab_foreign[paste0("joinx_",j)]=this_tab_foreign[this_tab_foreign_dets$fk_fields[j]]
    }
    joinnames=names(this_tab_prim_data[grepl("joinx", names(this_tab_prim_data))])
    
    merged = merge(x= this_tab_prim_data, y=this_tab_foreign, by = joinnames,  all.x = TRUE)
    merged = merged[, -grep("joinx_", colnames(merged))]
    dups.x = sort(names(merged)[grepl("\\.x", names(merged))])
    dups.y = sort(names(merged)[grepl("\\.y", names(merged))])
    if (length(dups.x)>0){
      for (i in 1:length(dups.x)){
        if (identical(unname(merged[dups.x[i]]),unname(merged[dups.y[i]]))==TRUE){
          #identical cols, so retain name of first and drop second
          merged[dups.y[i]]=NULL
          names(merged)[names(merged)==dups.x[i]]<-gsub("\\.x","",dups.x[i])
        }else {
          names(merged)[names(merged)==dups.x[i]]<-gsub("\\.x","",dups.x[i])
          names(merged)[names(merged)==dups.y[i]]<-gsub("\\.y",paste0(".",this_tab_foreign_nm),dups.y[i])
        }
      }
    }
    return(merged)
  }
  #ensure that tables that can be linked to the primary table are done first
  if(length(tab_foreign)>0){
    for (m in 1:length(tab_foreign)){
      if (debug) cat(paste0("PRIME: Trying merge(",tab_prim,", ",names(tab_foreign[m]),", by.x='",tab_foreign[[1]]$pk_fields,"', by.y='",tab_foreign[[1]]$fk_fields, "')\n"))
      all_recs = doMerge(all_recs, tab_foreign[m], morph_dets, debug)
    }
  }
  z=1
  while (length(joiners)>0 & z <= length(joiners)){
    
    all = ds_all[[.GlobalEnv$db]]$joins[names(ds_all[[.GlobalEnv$db]]$joins) %in% joiners]
    all_names = names(sapply(all, names))
    if (length(setdiff(all_names, joiners))!=0) warning("The tables remaining to be joined do not match the join information in the data_sources.")
    for (i in 1:length(all)){
      joinTable = names(all[i])
      pk = all[[i]][!names(all[[i]]) %in% 'combine'][[1]]$pk_fields
      fk = all[[i]][!names(all[[i]]) %in% 'combine'][[1]]$fk_fields
      all_this = all[[i]][!names(all[[i]]) %in% 'combine'][1]
      names(all_this) = joinTable
      if (all(fk %in% colnames(all_recs))){
        names(all_this[[1]]) = c("fk_fields","pk_fields")
        if (debug) {
          cat(paste0("\tAssessing merge(<all n=",nrow(all_recs),">, ",joinTable,", by.x='",paste(pk, collapse=","),"', by.y='",paste(fk, collapse=","), "')\n"))
        }
        all_recs = doMerge(all_recs, all_this, morph_dets, debug)
        joiners = joiners[!joiners %in% joinTable]
        z=1
      }else{
        z=z+1
      }
      pk = NULL
    }
    if (z > length(joiners) & length(joiners)>0 & (gear_dets | morph_dets) ) {
      print("The following tables were not joined:")
      print(joiners, sep=" ", collapse = NULL)
    }
  }
  rm(summ)
  gc()
  if (drop.na.cols) all_recs[sapply(all_recs, function(x) all(is.na(x)))] <- NULL
  if (nrow(all_recs) == 0)return(invisible(NULL))
  if (valid.coords == TRUE) {
    all_recs_spat = Mar.utils::df_qc_spatial(all_recs)
    if (NROW(all_recs_spat)==0) stop("\nNone of your records has valid coordinates...")
    if (!quiet) cat(paste0("\nYou indicated you only wanted data with valid coordinates.  ",NROW(all_recs) - NROW(all_recs_spat)," of ",NROW(all_recs)," records were lost due to this constraint"))
    all_recs = all_recs_spat
  }
  return(invisible(all_recs))
}
