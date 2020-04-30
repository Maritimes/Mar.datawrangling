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
#' \item \code{FALL} - Type 1; Fall (i.e. months 9:!2)
#' }
#' @param env This the the environment you want this function to work in.  The 
#' default value is \code{.GlobalEnv}.
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_survey<- function(db=NULL, survey=NULL, env=.GlobalEnv){
  if (is.null(db))db = ds_all[[.GlobalEnv$db]]$db
  if(db !='rv'){
    cat(paste0("\n","This function currently only works for the rv database"))
    return(NULL)
  }
  
  chooseSurvey <- function(db=NULL){
    rvSurvs <- c("4X", 
                 "GEORGES",
                 "SPRING",
                 "4VSW",
                 "SUMMER",
                 "FALL")
    isdbSurvs <- c("<COMMERCIAL/OBSERVER>", 
                   '4VN SENTINEL SURVEY',
                   '4VSW SENTINEL PROGRAM',
                   '4VWX HALIBUT PORT SAMPLE',
                   '4VWX SKATE SURVEY',
                   '4X MOBILE GEAR SURVEY',
                   '4X MONKFISH SURVEY',
                   '5Z FIXED GEAR SURVEY',
                   'FSRS - ECO - EXPERIMENTAL',
                   'GEAC JUV. AND FORAGE SURVEY',
                   'GULF RESEARCH SURVEY',
                   'HALIBUT LONGLINE SURVEY',
                   'LOBSTER SURVEY',
                   'LOBSTER TAG REPLACEMENT',
                   'N. ATL. BLUEFIN SURVEY',
                   'SCALLOP RESEARCH',
                   'SNOWCRAB SURVEY',
                   'FSRS - ECO - AT SEA SAMPLING')
    thisSurv <- switch(db,
           "rv" = rvSurvs,
           "isdb" = isdbSurvs
           )
    survey = utils::select.list(thisSurv,
                                multiple = F,
                                graphics = T,
                                title = paste("Select a standard survey")
    )
    return(survey)
  }
    if (is.null(survey)){
      survey = chooseSurvey(db)
    }
    if (db == 'rv'){
    #US Stations
    # env$GSMISSIONS = env$GSMISSIONS[env$GSMISSIONS$SEASON == 'SPRING',]
    # env$GSINF = env$GSINF[env$GSINF$TYPE ==1 &
    #                 env$GSINF$STRAT %in% c('551', '552', '553', '554', '555', '556', '557', '558', '559' ),]
    do4X <-function(){
      #4X
      env$GSMISSIONS = env$GSMISSIONS[env$GSMISSIONS$SEASON == 'SPRING' & env$GSMISSIONS$YEAR >= 2008,] 
      env$GSINF = env$GSINF[env$GSINF$TYPE ==1 &
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
      env$GSINF = env$GSINF[env$GSINF$TYPE ==1 &
                              env$GSINF$STRAT %in% c("5Z1", "5Z2", "5Z3", "5Z4", "5Z5", "5Z6", "5Z7", "5Z8", "5Z9"),]
      
    }
    doSPRING <-function(){
      #SPRING DATA (TYPE 1; MONTHS 1,2,3,4)
      #i.e. not 4X, not US stations, and not 4VVSW
      env$GSMISSIONS = env$GSMISSIONS[env$GSMISSIONS$SEASON == 'SPRING' & env$GSMISSIONS$YEAR < 2008,]
      env$GSINF = env$GSINF[env$GSINF$TYPE ==1 &
                              !grepl(pattern = "5Z", x = env$GSINF$STRAT) &
                              !(env$GSINF$STRAT %in% c('551', '552', '553', '554', '555', '556', '557', '558', '559')) &
                              !(env$GSINF$STRAT %in% c('396','397', '398', '399',
                                                       '400', '401', '402', '403', '404', '405', '406', '407', '408', '409', 
                                                       '410', '411')),]
    }
    doSUMMER <-function(){
      #SUMMER SURVEY (TYPE 1; MONTHS 5,6,7,8)
      env$GSMISSIONS = env$GSMISSIONS[env$GSMISSIONS$SEASON == 'SUMMER',]
      env$GSINF = env$GSINF[env$GSINF$TYPE ==1 &
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
      env$GSINF = env$GSINF[env$GSINF$TYPE ==1,]
    }
    do4VSW <-function(){
      #4VSW
      env$GSMISSIONS = env$GSMISSIONS[env$GSMISSIONS$SEASON == 'SPRING',]
      env$GSINF = env$GSINF[env$GSINF$TYPE ==1 &
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
 
# } else if (db=='isdb'){
#   doCOMM <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID <7010 | obs_TRIPS_all$TRIPCD_ID == 7099,]
#   }
#   do4VN <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==7052,]
#   }
#   do4VSW <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==7050,]
#     }
#   do4VWXHAL <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==7058,]
#   }
#   do4VWXSKAT <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==7054,]
#   }
#   do4XMOB <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==7051,]
#   }
#   do4XMONK <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==7053,]
#   }
#   do5Z <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==7055,]
#   }
#   doATSEA <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==7010,]
#   }
#   doEXP <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==7011,]
#   }
#   doGEAC <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==7060,]
#   }
#   doGULF <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==8998,]
#   }
#   doHAL <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==7057,]
#   }
#   doLOBS <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==7065,]
#   }
#   doLOBSTAG <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==7068,]
#   }
#   doBLUEFIN <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==7059,]
#   }
#   doSCALL <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==7062,]
#   }
#   doSNOWCRAB <- function(){
#     env$ISTRIPTYPECODES[env$ISTRIPTYPECODES$TRIPCD_ID==7061,]
#   }
#   switch(survey,
#          '<COMMERCIAL/OBSERVER>'= doCOMM(),
#          '4VN SENTINEL SURVEY'= do4VN(),
#          '4VSW SENTINEL PROGRAM'= do4VSW(),
#          '4VWX HALIBUT PORT SAMPLE'= do4VWXHAL(),
#          '4VWX SKATE SURVEY'= do4VWXSKAT(),
#          '4X MOBILE GEAR SURVEY'= do4XMOB(),
#          '4X MONKFISH SURVEY'= do4XMONK(),
#          '5Z FIXED GEAR SURVEY'= do5Z(),
#          'FSRS - ECO - AT SEA SAMPLING'= doATSEA(),
#          'FSRS - ECO - EXPERIMENTAL'= doEXP(),
#          'GEAC JUV. AND FORAGE SURVEY'= doGEAC(),
#          'GULF RESEARCH SURVEY'= doGULF(),
#          'HALIBUT LONGLINE SURVEY'= doHAL(),
#          'LOBSTER SURVEY'= doLOBS(),
#          'LOBSTER TAG REPLACEMENT'= doLOBSTAG(),
#          'N. ATL. BLUEFIN SURVEY'= doBLUEFIN(),
#          'SCALLOP RESEARCH'= doSCALL(),
#          'SNOWCRAB SURVEY'= doSNOWCRAB()
#   )
}
  self_filter(quiet = F, db = ds_all[[.GlobalEnv$db]]$db, env=.GlobalEnv)
}











