#' @title make_oracle_cxn
#' @description This function facilitates creating a connection to Oracle, and
#' allows connection via RODBC or ROracle, depending on the value of usepkg.
#' @param usepkg default is \code{'rodbc'}. This indicates whether the connection to Oracle should
#' use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is slightly easier to setup, but
#' roracle will extract data ~ 5x faster.
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
#' @export
make_oracle_cxn <- function(usepkg = 'rodbc', 
                            fn.oracle.username ="_none_",
                            fn.oracle.password="_none_",
                            fn.oracle.dsn="_none_") {

  use.roracle <-function(oracle.dsn, oracle.username, oracle.password){
    oracle_cxn <-tryCatch(
      {
        assign('oracle_cxn', ROracle::dbConnect( DBI::dbDriver("Oracle"), oracle.username,oracle.password,oracle.dsn))
      }, 
      error=function(cond){
        message(cond)
      }
    )
    if (class(oracle_cxn)[1]=="OraConnection") {
      cat("\nSuccessfully connected to Oracle via ROracle\n")
      results = list('roracle', oracle_cxn)
      return(results)
    } else {
      cat("\nROracle attempt failed\n")
      return(-1)
    }
  }
  use.rodbc <-function(oracle.dsn, oracle.username, oracle.password){
    oracle_cxn <-tryCatch(
      {
        assign('oracle_cxn', RODBC::odbcConnect(oracle.dsn, uid = oracle.username, pwd = oracle.password, believeNRows = F))
      }, 
      error=function(cond){
        message(cond)
      }
    )
    if (class(oracle_cxn)[1]=="RODBC") {
      cat("\nSuccessfully connected to Oracle via RODBC\n")
      results = list('rodbc', oracle_cxn)
      return(results)
    } else {
      cat("\nRODBC attempt failed\n")
      return(-1)
    }
  }
  
  if (exists("oracle_cxn")){
    if (class(oracle_cxn) == 'RODBC'){
      results = list('rodbc', oracle_cxn)
      return(results)
    }else if (class(oracle_cxn)[1]=="OraConnection") {
      results = list('roracle', oracle_cxn)
      return(results)
    } 
  } else {
    #get connection info - only prompt for values not in rprofile
    if (fn.oracle.username != "_none_"){
      oracle.username= fn.oracle.username
    } else if (exists('oracle.username')){
      oracle.username <- oracle.username
      cat("\nUsing stored 'oracle.username'")
    }else{
      oracle.username <- readline(prompt = "Oracle Username: ")
      print(oracle.username)
    }
    if (fn.oracle.password != "_none_"){
         oracle.password= fn.oracle.password
    }else if (exists('oracle.password')){
      oracle.password <- oracle.password
      cat("\nUsing stored 'oracle.password'")
    } else {
      oracle.password <- readline(prompt = "Oracle Password: ")
      print(oracle.password)
    }
    if (fn.oracle.dsn != "_none_"){
         oracle.dsn= fn.oracle.dsn
    }else if (exists('oracle.dsn')){
      oracle.dsn <- oracle.dsn
      cat("\nUsing stored 'oracle.dsn'")
    }else{
      oracle.dsn <- readline(prompt = "Oracle DSN (e.g. PTRAN): ")
      print(oracle.dsn)
    }
    
    if (usepkg=='roracle'){
      use.roracle(oracle.dsn, oracle.username, oracle.password)
    }else{
      use.rodbc(oracle.dsn, oracle.username, oracle.password)
    }
  }
}