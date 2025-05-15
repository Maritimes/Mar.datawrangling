.pkgenv <- new.env(parent = emptyenv())

get_pesd_dw_dir <- function() {
  file.path("C:", "DFO-MPO", "PESDData","MarDatawrangling")
}

get_ds_all <- function() {
  .pkgenv$ds_all
}

.onAttach <- function(libname, pkgname) {
  #onAttach is for interactive sessions
  assign("ds_all", get_ds_all(), envir = .GlobalEnv)
  
  Mar.utils::updateCheck(gitPkg = 'Maritimes/Mar.datawrangling')
  localVer = utils::packageDescription('Mar.datawrangling')$Version
  msg <- paste(
    #paste0("Version: ", localVer),
"Timezone set to 'America/Halifax' to match data",
"\n\n!! IMPORTANT:  Security Update - Protected B Data Encryption !!!",
"Protected B data extracted by this package (i.e. MARFIS;ISDB;COMLAND;MFD_STOMACH) has been found on unrestricted network drives, which violates security protocols. These datasets carry responsibilities that were agreed to when your account was granted permissions.",
"\nKey changes implemented:",
"1) 'data.dir' is now deprecated; all extracted data will be stored in 'C:\\DFO-MPO\\PESDData\\MarDatawrangling'",
"\t(Any existing data can be moved to this folder to avoid re-extraction)",
"2) Protected B data will now be encrypted when extracted (unclassified data will remain unencrypted)",
"\nTo access Protected B Rdata files as a different user or on a different computer than the one that performed the extraction, you will need:",
"\t'extract_user': The original extractor's username (from Sys.info()['user'])",
"\t'extract_computer': The original computer name (from Sys.info()['nodename'])",
"\nExamples:",
"\tget_data(..., extract_user='McMahonM', extract_computer='WLNSXXX9999999')",
"\tMar.utils::load_encrypted(..., extract_user='McMahonM', extract_computer='WLNSXXX9999999')",
"\nWARNING: Any unencrypted Protected B data extracted by this package found on unsecured network drives will be deleted immediately.",
"For questions or assistance, please contact Mike.McMahon@dfo-mpo.gc.ca",
    sep = "\n"
  )
  
  .pkgenv$ds_all <- load_datasources()
  
  packageStartupMessage(msg)
}

.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
  Sys.setenv(TZ = "America/Halifax")
  base_dir <- file.path("C:", "DFO-MPO")
  pesd_dir <- file.path(base_dir, "PESDData","MarDatawrangling")
  if (!dir.exists(pesd_dir)) dir.create(pesd_dir, recursive = T)
}

utils::globalVariables(c("DETS_COL_LOOKUP","COLUMN_DEFN_ID", "SUM_DOC_DEFN_COL_ID","AIR_TEMPERATURE","AIR_TEMPERATURE1","AIR_TEMPERATURE2","AIR_TEMPERATURE3","AIR_TEMPERATURE4","AIR_TMP1","AIR_TMP2","AIR_TMP3","AIR_TMP4","BAR_PRESS1","BAR_PRESS2","BAR_PRESS3","BAR_PRESS4","BAR_PRESSURE","BAR_PRESSURE1","BAR_PRESSURE2","BAR_PRESSURE3","BAR_PRESSURE4","DATE_TIME","DATE_TIME1","DATE_TIME2","DATE_TIME3","DATE_TIME4","DEP1","DEP2","DEP3","DEP4","DEPTH","DEPTH1","DEPTH2","DEPTH3","DEPTH4","DISTNM_32","DISTNM_41","DUR_32","DUR_41","FISHSET_ID","ISFISHSETS","LAT1","LAT2","LAT3","LAT4","LATITUDE","LATITUDE1","LATITUDE2","LATITUDE3","LATITUDE4","LONG1","LONG2","LONG3","LONG4","LONGITUDE","LONGITUDE1","LONGITUDE2","LONGITUDE3","LONGITUDE4","NET_TEMPERATURE","NET_TEMPERATURE1","NET_TEMPERATURE2","NET_TEMPERATURE3","NET_TEMPERATURE4","NET_TMP1","NET_TMP2","NET_TMP3","NET_TMP4","PNTCD_ID","SETDATE","SETTIME","SET_NO","VESSEL_SPEED","VESSEL_SPEED1","VESSEL_SPEED2","VESSEL_SPEED3","VESSEL_SPEED4","VESS_SPD1","VESS_SPD2","VESS_SPD3","VESS_SPD4","WATER_TEMPERATURE","WATER_TEMPERATURE1","WATER_TEMPERATURE2","WATER_TEMPERATURE3","WATER_TEMPERATURE4","WAT_TMP1","WAT_TMP2","WAT_TMP3","WAT_TMP4","YEAR","deprecationCheck","ds_all","dw","fn.oracle.dsn","fn.oracle.password","fn.oracle.username"))