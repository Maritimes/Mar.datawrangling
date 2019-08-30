.onAttach <- function(libname, pkgname) {
  
  assign("ds_all", load_datasources(), envir = .GlobalEnv)
  localVer = utils::packageDescription('Mar.datawrangling')$Version
  packageStartupMessage(paste0("Version: ", localVer))
}

.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
  Mar.utils::updateCheck(gitPkg = 'Maritimes/Mar.datawrangling')
}
