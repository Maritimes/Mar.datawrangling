#' @title get_survey
#' @description This function facilitates the extraction of particular survey from the rv database.
#' Within the groundfish data are a number of survey all mixed together.  They are typically 
#' differentiated via specific selections of 1) type; 2) date (i.e. month and year), and 3) strata.
#' Since this can be confusing, this function was written to help ensure that the extracted sets 
#' match the survey they should.
#' @param db default is \code{NULL}. This identifies the dataset you are working
#' with. 
#' @param survey default is \code{NULL}. This identifies which survey you want tto extract. Valid 
#' values include the following:
#' \itemize{
#' \item \code{4X} - Type 1; Spring (i.e. months 1:4); 2008+; specific strata 
#' \item \code{GEORGES} - Type 1; Spring (i.e. months 1:4); strata 5Z*
#' \item \code{SPRING} - Type 1; Spring (i.e. months 1:4); pre-2008; specific strata 
#' \item \code{4VSW}  - Type 1; Spring (i.e. months 1:4); 4VSW strata;  
#' \item \code{SUMMER} - Type 1; Summer (i.e. months 5:8); specific strata
#' \item \code{FALL} - Type 1; Fall (i.e. months 9:12)
#' }
#' @param keepBadSets default is \code{FALSE}.  This determines whether or not both type 1 and 3 
#' sets are returned, or just type 1. Type 3 sets indicate an invalid tow.
#' @param quiet default is \code{TRUE}.  If TRUE, no output messages will be shown.
#' @param env This the the environment you want this function to work in.  The 
#' default value is \code{.GlobalEnv}.
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_survey<- function(db=NULL, survey=NULL, keepBadSets = FALSE, quiet=TRUE, env=.GlobalEnv){
  setTypes = 1
  if (keepBadSets) setTypes <- c(setTypes,3)
  if (is.null(db))db = get_ds_all()[[.GlobalEnv$db]]$db
  if(db !='rv'){
    cat(paste0("\n","This function currently only works for the rv database"))
    return(NULL)
  }
  

  get_data(db=db, quiet=quiet, env=env)
  if (db == 'rv'){
    #ensure that we only resturn missions that had type 1 sets - don't want exploratory, etc
    env$GSMISSIONS <- env$GSMISSIONS[env$GSMISSIONS$MISSION %in% unique(env$GSINF[env$GSINF$TYPE ==1, "MISSION"]),]
    #US Stations
    # env$GSMISSIONS = env$GSMISSIONS[env$GSMISSIONS$SEASON == 'SPRING',]
    # env$GSINF = env$GSINF[env$GSINF$TYPE ==1 &
    #                 env$GSINF$STRAT %in% c('551', '552', '553', '554', '555', '556', '557', '558', '559' ),]
    do4X <-function(){
      #4X
      env$GSMISSIONS = env$GSMISSIONS[env$GSMISSIONS$SEASON == 'SPRING' & env$GSMISSIONS$YEAR >= 2008,] 
      env$GSINF = env$GSINF[env$GSINF$TYPE %in% setTypes &
                              env$GSINF$STRAT %in% c('434', '436', '437', '438', '439', 
                                                     '440', '441', '442', '443', '444', '445', '446', '447', '448', '449', 
                                                     '450', '451', '452', '453', '454', '455', '456', '457', '458', '459', 
                                                     '460', '461', '462', '463', '464', '465', '466', 
                                                     '470', '471', '472', '473', '474', '475', '476', '477', '478', 
                                                     '480', '481', '482', '483', '484', '485', 
                                                     '490', '491', '492', '493', '494', '495', '496', '497', '498', 
                                                     '501', '502', '503', '504', '505', '557', '558', '559'),]
    }
    doGEORGES <-function(){
      #GEORGES
      env$GSMISSIONS = env$GSMISSIONS[env$GSMISSIONS$SEASON == 'SPRING',]
      env$GSINF = env$GSINF[env$GSINF$TYPE %in% setTypes &
                              env$GSINF$STRAT %in% c("5Z1", "5Z2", "5Z3", "5Z4", "5Z5", "5Z6", "5Z7", "5Z8", "5Z9"),]
      
    }
    doSPRING <-function(){
      #SPRING DATA (TYPE 1; MONTHS 1,2,3,4)
      #i.e. not 4X, not US stations, and not 4VVSW
      env$GSMISSIONS = env$GSMISSIONS[env$GSMISSIONS$SEASON == 'SPRING' & env$GSMISSIONS$YEAR < 2008,]
      env$GSINF = env$GSINF[env$GSINF$TYPE %in% setTypes &
                              !grepl(pattern = "5Z", x = env$GSINF$STRAT) &
                              !(env$GSINF$STRAT %in% c('551', '552', '553', '554', '555', '556', '557', '558', '559')) &
                              !(env$GSINF$STRAT %in% c('396','397', '398', '399',
                                                       '400', '401', '402', '403', '404', '405', '406', '407', '408', '409', 
                                                       '410', '411')),]
    }
    doSUMMER <-function(){
      #SUMMER SURVEY (TYPE 1; MONTHS 5,6,7,8)
      env$GSMISSIONS = env$GSMISSIONS[env$GSMISSIONS$SEASON == 'SUMMER',]
      env$GSINF = env$GSINF[env$GSINF$TYPE %in% setTypes &
                              env$GSINF$STRAT %in% c("434", "436", "437", "438", "439", #1971 only, Gulf region
                                                     "440", "441", "442", "443", "444", "445", "446", "447", "448", "449", 
                                                     "450", "451", "452", "453", "454", "455", "456", "457", "458", "459", 
                                                     "460", "461", "462", "463", "464", "465", "466", 
                                                     "470", "471", "472", "473", "474", "475", "476", "477", "478", 
                                                     "480", "481", "482", "483", "484", "485", 
                                                     "490", "491", "492", "493", "494", "495", "496", "497", "498", 
                                                     "501", "502", "503", "504", "505", "557", "558", "559", 
                                                     "5Z1", "5Z2", "5Z3", "5Z4", "5Z5", "5Z6", "5Z7", "5Z8", "5Z9"),]
      
    }
    doFALL <-function(){
      #FALL
      env$GSMISSIONS = env$GSMISSIONS[env$GSMISSIONS$SEASON == 'FALL',]
      env$GSINF = env$GSINF[env$GSINF$TYPE %in% setTypes,]
    }
    do4VSW <-function(){
      #4VSW
      env$GSMISSIONS = env$GSMISSIONS[env$GSMISSIONS$SEASON == 'SPRING',]
      env$GSINF = env$GSINF[env$GSINF$TYPE %in% setTypes &
                              env$GSINF$STRAT %in% c('396','397', '398', '399', '400', 
                                                     '401', '402', '403', '404', '405', '406', '407', '408', '409', 
                                                     '410', '411'),]
    }
    switch(survey,
           "4X" = do4X(),
           "GEORGES" = doGEORGES(),
           "SPRING" = doSPRING(),
           "4VSW" = do4VSW(),
           "SUMMER" = doSUMMER(),
           "FALL" = doFALL()
    )
  }
  self_filter(db = get_ds_all()[[.GlobalEnv$db]]$db, quiet=quiet, env=env)
}











