.onAttach <- function(libname, pkgname) {
  localVer = utils::packageDescription('Mar.datawrangling')$Version
  packageStartupMessage(paste0("Version: ", localVer))
}

.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
  assign("ds_all", load_datasources(), envir = .GlobalEnv)
  Mar.utils::updateCheck(gitPkg = 'Maritimes/Mar.datawrangling')
}
