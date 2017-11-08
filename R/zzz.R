.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("###\nVersion: ", utils::packageDescription('Mar.datawrangling')$Version))
    #packageStartupMessage("")
}
.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
  assign("ds_all", load_datasources(), envir = .GlobalEnv)
}
