.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("###\nVersion: ", utils::packageDescription('Mar.datawrangling')$Version))
  #bad marfis coordinate conversion - delete
  packageStartupMessage("!!ALERT!!\nMARFIS coordinates were not calculated correctly prior to version 2017.06.28
Please delete the MARFIS.PRO_SPC_INFO.RData file from your data folder and
re-extract before continuing")
}
.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
  assign("ds_all", load_datasources(), envir = .GlobalEnv)
}
