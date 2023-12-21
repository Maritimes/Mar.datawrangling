.onAttach <- function(libname, pkgname) {
  #onAttach is for interactive sessions
  assign("ds_all", Mar.datawrangling::load_datasources(), envir = .GlobalEnv)
  Mar.utils::updateCheck(gitPkg = 'Maritimes/Mar.datawrangling')
  localVer = utils::packageDescription('Mar.datawrangling')$Version
  packageStartupMessage(paste0("Version: ", localVer))
  packageStartupMessage("Timezone set to 'America/Halifax' to match data")
}

.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
  Sys.setenv(TZ = "America/Halifax")
}

utils::globalVariables(c("DETS_COL_LOOKUP","COLUMN_DEFN_ID", "SUM_DOC_DEFN_COL_ID"))