#' @title get_std_rv
#' @description This function prepares a detailed length frequency table based on 1, 2 or 3 cm intervals
#' depending on the species being processed. It provides the same data as results from NWAGS.gsd3lf_mv.
#' @param fn.oracle.username default is \code{'_none_'} This is your username for
#' accessing oracle objects. If you have a value for this stored in your
#' environment (e.g. from an rprofile file), this can be left and that value will
#' be used.  If a value for this is provided, it will take priority over your
#' existing value.
#' @param fn.oracle.password default is \code{'_none_'} This is your password for
#' accessing oracle objects. If you have a value for this stored in your
#' environment (e.g. from an rprofile file), this can be left and that value will
#' be used.  If a value for this is provided, it will take priority over your
#' existing value.
#' @param fn.oracle.dsn default is \code{'_none_'} This is your dsn/ODBC
#' identifier for accessing oracle objects. If you have a value for this stored
#' in your environment (e.g. from an rprofile file), this can be left and that
#' value will be used.  If a value for this is provided, it will take priority
#' over your existing value.
#' @param usepkg default is \code{'rodbc'}. This indicates whether the connection to Oracle should
#' use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is slightly easier to setup, but
#' roracle will extract data ~ 5x faster.
#' @param data.dir  The default is your working directory. If you are hoping to load existing data,
#' this folder should contain a data folder containing your rdata files. If you are extracting data,
#' a data folder will be created under this folder.
#' extracted files to go.
#' @param year_min default is \code{NULL}.  This is the minimum year you want data to be returned for.
#' @param year_max default is \code{NULL}. This is the maximum year you want data to be returned for.
#' @param season default is \code{NULL}. This is the season you want data for (e.g. "SUMMER")
#' @param strat default is \code{NULL}.  These are the strata you want results for (e.g. \code{c(440:495)})
#' @param sp default is \code{NULL}.  This is the species code you want data for
#' @param len_min default is \code{NULL}.  This is the minimum length you want data to be returned for.
#' @param len_max default is \code{NULL}. This is the maximum length you want data to be returned for.
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_std_rv<-function(fn.oracle.username = "_none_", 
                          fn.oracle.password = "_none_", 
                          fn.oracle.dsn = "_none_",
                          usepkg = "rodbc",
                          data.dir = NULL,
                          year_min = NULL,
                          year_max = NULL,
                          season = NULL,
                          strat =  NULL,
                          sp = NULL,
                          len_min = NULL,
                          len_max = NULL){
  infn <- new.env()
  # Get underlying data -----------------------------------------------------

  Mar.datawrangling::get_data('rv', data.dir = data.dir, env = infn, fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn)
  infn$GSMISSIONS = infn$GSMISSIONS[infn$GSMISSIONS$YEAR >=year_min & infn$GSMISSIONS$YEAR <= year_max, ]
  infn$GSMISSIONS = infn$GSMISSIONS[infn$GSMISSIONS$SEASON == season,]
  infn$GSXTYPE = infn$GSXTYPE[infn$GSXTYPE$XTYPE==1,]
  infn$GSSTRATUM = infn$GSSTRATUM[infn$GSSTRATUM$STRAT %in% strat,]
  infn$GSSPECIES = infn$GSSPECIES[infn$GSSPECIES$CODE == sp,]
  infn$GSDET = infn$GSDET[infn$GSDET$FLEN >= len_min & infn$GSDET$FLEN <= len_max,]
  Mar.datawrangling::self_filter(db='rv', keep_nullsets = T, env = infn, debug = F)
  res = Mar.datawrangling::summarize_catches(morph_dets = T, env = infn)
  
  Mar.datawrangling::get_data_custom(schema = 'groundfish', tables = "GSSPEC", data.dir = data.dir, env = infn,fn.oracle.username = fn.oracle.username, fn.oracle.password = fn.oracle.password, fn.oracle.dsn = fn.oracle.dsn)
  res = merge(res, infn$GSSPEC[,c("SPEC","LENWGTA","LENWGTB","LGRP")], all.x = T)
  
  res_sets = unique(res[,c("SETNO","MISSION","LATITUDE","LONGITUDE","YEAR","STRAT","AREA")])
  res_dets = res[,c("SETNO","MISSION","SPEC","CLEN","FWT","FLEN","LGRP","DIST")]
  res_dets = merge(res_dets, infn$GSCAT[,c("SETNO","MISSION","SPEC","SAMPWGT","TOTWGT")], all.x=T)
  
  # Deal with NAs -----------------------------------------------------------
  res_dets[is.na(res_dets$FWT),"FWT"]<-0
  res_dets[is.na(res_dets$CLEN),"CLEN"]<-0
  res_dets[is.na(res_dets$FLEN),"FLEN"]<-0
  res_dets[is.na(res_dets$LGRP),"LGRP"]<-0
  res_dets[is.na(res_dets$SAMPWGT),"SAMPWGT"]<-0
  res_dets[is.na(res_dets$TOTWGT),"TOTWGT"]<-0
  
  # Calculate std wgt and no for each length group --------------------------
  res_dets$FLEN = floor(res_dets$FLEN/res_dets$LGRP)*res_dets$LGRP +1
  res_dets$STDCLEN = res_dets$CLEN*res_dets$TOTWGT/res_dets$SAMPWGT*1.75/res_dets$DIST
  
  res_dets = aggregate(
    x = list(STDCLEN = res_dets$STDCLEN,
             CLEN = res_dets$CLEN),
    by = list(MISSION = res_dets$MISSION, 
              SETNO = res_dets$SETNO, 
              SPEC = res_dets$SPEC, 
              FLEN = res_dets$FLEN
    ),
    sum
  )
  res_dets = merge(res_dets, infn$GSSPEC[,c("SPEC","LENWGTA","LENWGTB")], by = c("SPEC"),all.x=T)
  res_dets$STDWGT = res_dets$LENWGTA*(res_dets$FLEN^res_dets$LENWGTB)*res_dets$STDCLEN
  
  # Calculate std wgt and no for each set -----------------------------------
  results =merge(res_sets, res_dets[,c("MISSION","SETNO","SPEC","STDWGT","STDCLEN")], by=c("MISSION","SETNO"), all.x=T)
  
  # Deal with NAs and add in sp code ----------------------------------------
  results[is.na(results$SPEC),"SPEC"]<-sp 
  results[is.na(results$STDCLEN),"STDCLEN"]<-0
  results[is.na(results$STDWGT),"STDWGT"]<-0
  results = aggregate(
    x = list(STDNO = results$STDCLEN,
             STDWGT = results$STDWGT),
    by = list(MISSION = results$MISSION, 
              SETNO = results$SETNO, 
              LATITUDE = results$LATITUDE, 
              LONGITUDE = results$LONGITUDE, 
              YEAR = results$YEAR, 
              STRAT = results$STRAT, 
              AREA = results$AREA, 
              SPEC = results$SPEC
    ),
    sum
  )
  results = results[with(results,order(YEAR, SETNO)),]
  Mar.datawrangling::cleanup('rv',env = infn)
  return(results)
}

# 
# # # Parameters --------------------------------------------------------------
# this.data.dir = "C:/git/wrangledData"
# this_year_min = 2013
# this_year_max = 2018
# this_season = "SUMMER"
# this_strat =  c('443','444','445','459')
# this_sp = 10
# this_len_min = 0.1
# this_len_max = 29
# # # - -----------------------------------------------------------------------
# # 
# # library(Mar.datawrangling)
# test = NumbersAndWeights(data.dir = this.data.dir,
#                        year_min = this_year_min,
#                        year_max = this_year_max,
#                        season = this_season,
#                        strat =  this_strat,
#                        sp = this_sp,
#                        len_min = this_len_min,
#                        len_max = this_len_max)
