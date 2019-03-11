.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("Version: ", utils::packageDescription('Mar.datawrangling')$Version))
  
  #Compare local and Github versions and inform user if different
  Mar.datawranglingRemote  <- tryCatch({
    scan('https://raw.githubusercontent.com/Maritimes/Mar.datawrangling/master/tools/version.txt',quiet = T)
  },
  error = function(cond) {
  })
  Mar.datawranglingLocal = gsub(pattern = "\\.",replacement = "",x = utils::packageDescription('Mar.datawrangling')$Version)
  if (is.null(Mar.datawranglingRemote)){
    cat("Can't reach git to check version")
  }else if (Mar.datawranglingLocal > Mar.datawranglingRemote){
    cat("&lt;Push to Github!&gt;")
  }else if (Mar.datawranglingLocal < Mar.datawranglingRemote){
    cat(paste0("This version of Mar.datawrangling is outdated -- v.",gsub('^([0-9]{4})([0-9]{2})([0-9]{2})$','\\1\\.\\2\\.\\3',Mar.datawranglingRemote)," is now available"))
    choice = toupper(readline(prompt = "Press 'y' to update this package (or any other key to skip the update)\nUpdating is ALWAYS recommended, and will not impact your previously extracted data"))
    if (choice =="Y"){
      updated = tryCatch({
      devtools::install_github('Maritimes/Mar.datawrangling',) 
        },
      error = function(cond) {
      })
    }else{
      cat("Fine, keep your crappy old version. Maybe next time :)\n")
    }
  }
  # else{
  #   cat(paste0("This is the current version of Mar.datawrangling"))
  # }
  
  
}
.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
  assign("ds_all", load_datasources(), envir = .GlobalEnv)
}
