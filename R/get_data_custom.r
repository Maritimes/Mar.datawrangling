#' @title get_data_custom
#' @description This function allows users to extract other tables within a
#' known database that the script does not get by default.  They must have 
#' access to the tables to extract them.
#' @param db default is \code{NULL}. This identifies the dataset you want to get 
#' additional tables from.
#' @param data.dir  The default is your working directory. If you are hoping to load existing data,
#' this folder should contain a data folder containing your rdata files. If you are extracting data,
#' a data folder will be created under this folder.
#' extracted files to go.
#' @param tables The default value is \code{NULL}.  This is a vector of table 
#' names you want to extract that exist in the database specified by \code{db}.
#' @param usepkg default is \code{oracle_cxn$usepkg}. This indicates whether the 
#' connection to Oracle should use \code{'rodbc'} or \code{'roracle'} to 
#' connect.  rodbc is slightly easier to setup, but roracle will extract data ~ 
#' 5x faster.
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
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom lubridate ymd
#' @importFrom utils data
#' @importFrom Mar.utils make_oracle_cxn
#' @export
get_data_custom<-function(db=NULL,
                  data.dir = file.path(getwd(), 'data'),
                  tables = NULL,
                  usepkg = 'rodbc', 
                  fn.oracle.username ="_none_",
                  fn.oracle.password="_none_",
                  fn.oracle.dsn="_none_"){

    oracle_cxn_custom = Mar.utils::make_oracle_cxn(usepkg, fn.oracle.username, fn.oracle.password, fn.oracle.dsn)
  
  if (class(oracle_cxn_custom$channel)=="RODBC"){
    thecmd= RODBC::sqlQuery
  }else if (class(oracle_cxn_custom$channel)=="OraConnection"){
    thecmd = ROracle::dbGetQuery
  }
  prefix=toupper(db)
  theschema = ds_all[[db]]$schema

  for (i in 1:length(tables)){
    cat(paste0("\nVerifying access to ",tables[i]," ..."))
    qry = paste0("select '1' from ",theschema,".",gsub(paste0(prefix,"."),"",tables[i])," WHERE ROWNUM<=1")
    if (is.character(thecmd(oracle_cxn_custom$channel, qry, rows_at_time = 1))){
      break("Can't find or access specified table")
    }else{
      cat(paste0("\nExtracting ",tables[i],"...\n"))
      table_naked = gsub(paste0(prefix,"."),"",tables[i])
      qry = paste0("SELECT * from ", theschema, ".",table_naked)
      assign(table_naked, thecmd(oracle_cxn_custom$channel, qry, rows_at_time = 1))

 
      save(table_naked, file=file.path(data.dir, paste0(prefix,".",tables[i],".RData")), compress=TRUE)
      cat(paste("Got",tables[i],"\n"))
      assign(x = tables[i],value = get(table_naked), envir = .GlobalEnv)
      cat(paste0("Loaded ",tables[i],"\n"))
    }
  }
}