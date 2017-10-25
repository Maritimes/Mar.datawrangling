#' @title load_datasources
#' @description This file was introduced in version 2.0 of Mar.datawrangling as an attempt simplify the process
#' of adding additional data sources.
#' If done correctly, this should remove the need for a variety of files and 
#' generally clarify the code.
#' @param db default is \code{NULL}. This identifies the dataset you are working 
#' with.
#' @family internal
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
load_datasources <- function(db=NULL){
  rv = list (
    db="rv",
    name = "Groundfish/RV/Ecosystem Surveys",
    schema = "GROUNDFISH",
    desc = "
	DFO\'s annual Multi-Species Bottom Trawl began on the Scotian Shelf/Bay of 
	Fundy in 1970, and on Georges Bank in 1987. Primarily, these surveys are used 
	as fisheries independent tools to estimate stock abundance (the magnitude of the 
	marine populations) and recruitment (the abundance of juveniles) over time for 
	a number of fish and invertebrate species. This information is then used along 
	with fisheries catch data to assess the status of commercial species such as 
	cod, haddock, pollock, halibut, offshore lobster, shrimp etc.",
    tables = c("GSCAT","GSINF","GSDET","GSMISSIONS","GSSTRATUM","GSXTYPE","GSSPECIES","FGP_TOWS_NW2"),
    table_cat = "GSCAT",
    table_det = "GSDET",
    table_pos = "GSINF",
    field_default = "TOTNO",
    field_drops = c('SLAT','SLONG','ELAT','ELON'),
    joins = list(
      "GSSPECIES" = list(
        "GSCAT" = list(pk_fields=c("CODE"),
                       fk_fields=c("SPEC")),
        "GSDET" = list(pk_fields=c("CODE"),
                       fk_fields=c("SPEC")),
        combine = "OR"
      ),
      "GSMISSIONS" = list(
        "GSINF" = list(pk_fields=c("MISSION"),
                       fk_fields=c("MISSION"))
      ),
      "GSCAT" = list(
        "GSINF" = list(pk_fields=c("MISSION","SETNO"),
                       fk_fields=c("MISSION","SETNO")),
        "GSSPECIES" = list(pk_fields=c("SPEC"),
                           fk_fields=c("CODE")),
        combine = "ALL"
      ),
      "GSDET" = list(
        "GSCAT" = list(pk_fields=c("MISSION","SETNO","SPEC"),
                       fk_fields=c("MISSION","SETNO","SPEC")),
        "GSSPECIES" = list(pk_fields=c("SPEC"),
                          fk_fields=c("CODE")),
        combine = "ALL"
      ),
      "GSINF" = list(
        "GSMISSIONS" = list(pk_fields=c("MISSION"),
                                fk_fields=c("MISSION")),
        "GSSTRATUM" = list(pk_fields=c("STRAT"),
                                fk_fields=c("STRAT")),
        "GSXTYPE" = list(pk_fields=c("TYPE"),
                                fk_fields=c("XTYPE")),
        combine = "ALL"
      ),
      "GSSTRATUM" = list(
        "GSINF" = list(pk_fields=c("STRAT"),
                                fk_fields=c("STRAT"))
      ),
      "GSXTYPE" = list(
        "GSINF" = list(pk_fields=c("XTYPE"),
                                fk_fields=c("TYPE"))
      ),
      "FGP_TOWS_NW2" = list(
        "GSCAT" = list(pk_fields=c("MISSION","SETNUMBER","SPECIES"),
                       fk_fields=c("MISSION","SETNO","SPEC"))
      )
      ),
    filters = list(
      "Mission Name" = list(filt_tab = "GSMISSIONS",
                            filt_field = c("MISSION"),
                            filt_disp = c("MISSION"),
                            filt_ord = 1
      ),
      "Mission Year" = list(filt_tab = "GSMISSIONS",
                            filt_field = c("YEAR"),
                            filt_disp = c("YEAR"),
                            filt_ord = 1
      ),
      "Mission Season" = list(filt_tab = "GSMISSIONS",
                              filt_field = c("SEASON"),
                              filt_disp = c("SEASON"),
                              filt_ord = 1
      ),
      "Strata" = list(filt_tab = "GSSTRATUM",
                      filt_field = c("STRAT"),
                      filt_disp = c("STRAT"),
                      filt_ord = 1
      ),
      "NAFO Areas" = list(filt_tab = "GSSTRATUM",
                          filt_field = c("NAME"),
                          filt_disp = c("NAME"),
                          filt_ord = 1
      ),
      "Set Type" = list(filt_tab = "GSXTYPE",
                        filt_field = c("XTYPE"),
                        filt_disp = c("XTYPEDESC","XTYPE"),
                        filt_ord = 1
      ),
      "Species Caught (by name)" = list(filt_tab = "GSSPECIES",
                                        filt_field = c("CODE"),
                                        filt_disp = c("COMM","CODE"),
                                        filt_ord = 1
      ),
      "Species Caught (by code)" = list(filt_tab = "GSSPECIES",
                                        filt_field = c("CODE"),
                                        filt_disp = c("COMM","CODE"),
                                        filt_ord = 2
      )
    )
  )
  
  isdb = list (
    db="isdb",
    name = "Industry Surveys Database",
    schema = "OBSERVER",
    desc = "Department of Fisheries and Oceans (DFO) at-sea fish catch observations from 
	commercial fishing vessels operating in the North West Atlantic. Data are 
	collected by trained fisheries observers and industry technicians. The program 
	provides extremely detailed data particularly in terms of information on the 
	type of gear used, size of organisms caught and by-catches (e.g. non target 
	species). All of which are not available from conventional log and landings data 
	systems. Data are organized by trip, gear, set, sample and specimen. Trip types 
	can be either commercial fishing or industry surveys employing a range of mobile 
	and fixed gears (e.g. trawls, traps and hooks). \n
	\n
	Notable data within this source includes:\n
	\n
	-Maritimes Observer Program
	-ITQ Survey
	-4VN Sentinel Survey
	-4VsW Sentinel Sentinel
	-4VsW Skate Survey
	-4X Monkfish Survey
	-5Z Fixed Gear Survey
	-GEAC (Groundfish Enterprise Allocation Council) Juvenile and Forage Survey
	-Snow Crab Survey",
    tables = c("ISSPECIESCODES","ISSPECIESSOUGHTCODES","ISOBSERVERCODES","ISGEARFEATURECLASSES","ISSETTYPECODES","ISVESSELS","ISTRIPTYPECODES","ISGEARFEATURECODES","ISGEARFEATURES","ISCATCHES","ISGEARCODES","ISSETPROFILE_WIDE","ISTRIPS","ISGEARS","ISFISHSETS","ISFISH","ISFISHMORPHS","ISMORPHCODES","ISMORPHVALUECODES"),
    table_cat = "ISCATCHES",
    table_det = c("ISFISH", "ISFISHMORPHS", "ISMORPHCODES", "ISMORPHVALUECODES"),
    table_gear = c("ISGEARFEATURECLASSES","ISGEARFEATURECODES","ISGEARFEATURES"),
    table_pos = "ISSETPROFILE_WIDE",
    field_default = "EST_COMBINED_WT",
    field_drops = c('LAST_UPDATE_BY','LAST_UPDATE_DATE','CREATED_BY','CREATED_DATE','OWNER_GROUP','COMMENTS'),
    field_private = c('VESS_ID','VESSEL_NAME','CFV', 'LICENSE_NO', 'MARFIS_LICENSE_NO'),
    
    #      #due to weird values in the ISDB database, the following records are
    #      #avoided until we figure out how to handle them
    table_err_roracle = list(ISTRIPS = list(field="TRIP_ID",badvalues=c(100011618, 100011519)),comment="4VSW Sentinel, 2000",
                             ISFISHSETS = list(field="TRIP_ID",badvalues=c(100000990), comment="4VWX Skate Survey, 1999")
    ),  
    joins = list(
       "ISFISHSETS" = list(
         "ISTRIPS" = list(pk_fields=c("TRIP_ID"),
                                 fk_fields=c("TRIP_ID")),
         "ISSETTYPECODES" = list(pk_fields=c("SETCD_ID"),
                                 fk_fields=c("SETCD_ID")),
         "ISGEARS" = list(pk_fields=c("GEAR_ID"),
                                 fk_fields=c("GEAR_ID")),
         "ISSPECIESSOUGHTCODES" = list(pk_fields=c("SPECSCD_ID"),
                                 fk_fields=c("SPECSCD_ID")),
         "ISSETPROFILE_WIDE" = list(pk_fields=c("FISHSET_ID","SET_NO"),
                                       fk_fields=c("FISHSET_ID","SET_NO")),
         combine = "ALL"
       ),
       "ISOBSERVERCODES" = list(
         "ISTRIPS" = list(pk_fields=c("OBSCD_ID"),
                                 fk_fields=c("OBSCD_ID"))
       ),
       "ISCATCHES" = list(
         "ISFISHSETS" = list(pk_fields=c("FISHSET_ID","SET_NO"),
                                 fk_fields=c("FISHSET_ID","SET_NO")),
         "ISSPECIESCODES" = list(pk_fields=c("SPECCD_ID"),
                                 fk_fields=c("SPECCD_ID")),
         combine = "ALL"
       ),
       "ISSETPROFILE_WIDE" = list(
         "ISFISHSETS" = list(pk_fields=c("FISHSET_ID","SET_NO"),
                                 fk_fields=c("FISHSET_ID","SET_NO"))
       ),
       "ISGEARS" = list(
         "ISFISHSETS" = list(pk_fields=c("GEAR_ID"),
                                 fk_fields=c("GEAR_ID")),
         "ISGEARCODES" = list(pk_fields=c("GEARCD_ID"),
                                 fk_fields=c("GEARCD_ID")),
         combine = "ALL"
       ),
       "ISGEARCODES" = list(
         "ISGEARS" = list(pk_fields=c("GEARCD_ID"),
                                 fk_fields=c("GEARCD_ID"))
       ),
       "ISGEARFEATURES" = list(
         "ISGEARS" = list(pk_fields=c("GEAR_ID"),
                                 fk_fields=c("GEAR_ID"))
       ),
       "ISGEARFEATURECODES" = list(
         "ISGEARFEATURES" = list(pk_fields=c("GEARFCD_ID"),
                                 fk_fields=c("GEARFCD_ID"))
       ),
       "ISGEARFEATURECLASSES" = list(
         "ISGEARFEATURECODES" = list(pk_fields=c("GEARFCL_ID"),
                                 fk_fields=c("GEARFCL_ID"))
       ),
       "ISTRIPTYPECODES" = list(
         "ISTRIPS" = list(pk_fields=c("TRIPCD_ID"),
                                 fk_fields=c("TRIPCD_ID"))
       ),
       "ISTRIPS" = list(
         "ISFISHSETS" = list(pk_fields=c("TRIP_ID"),
                                 fk_fields=c("TRIP_ID")),
         "ISTRIPTYPECODES" = list(pk_fields=c("TRIPCD_ID"),
                                 fk_fields=c("TRIPCD_ID")),
         "ISOBSERVERCODES" = list(pk_fields=c("OBSCD_ID"),
                                 fk_fields=c("OBSCD_ID")),
         "ISVESSELS" = list(pk_fields=c("VESS_ID"),
                                 fk_fields=c("VESS_ID")),
         combine = "ALL"
       ),
       "ISSPECIESCODES" = list(
         "ISCATCHES" = list(pk_fields=c("SPECCD_ID"),
                                 fk_fields=c("SPECCD_ID")),
         "ISFISHSETS" = list(pk_fields=c("SPECCD_ID"),
                                 fk_fields=c("SPECSCD_ID")),
         combine = "OR"
       ),
       "ISSPECIESSOUGHTCODES" = list(
         "ISFISHSETS" = list(pk_fields=c("SPECSCD_ID"),
                                 fk_fields=c("SPECSCD_ID"))
         ),
       "ISSETTYPECODES" = list(
         "ISFISHSETS" = list(pk_fields=c("SETCD_ID"),
                                 fk_fields=c("SETCD_ID"))
       ),
       "ISVESSELS" = list(
         "ISTRIPS" = list(pk_fields=c("VESS_ID"),
                                 fk_fields=c("VESS_ID"))
       ),
       "ISFISH" = list(
         "ISCATCHES" = list(pk_fields=c("CATCH_ID"),
                                 fk_fields=c("CATCH_ID"))
       ),
       "ISFISHMORPHS" = list(
         "ISFISH" = list(pk_fields=c("FISH_ID"),
                                 fk_fields=c("FISH_ID"))
       ),
       "ISMORPHCODES" = list(
         "ISFISHMORPHS" = list(pk_fields=c("MRPHCD_ID"),
                                 fk_fields=c("MRPHCD_ID"))
       ),
       "ISMORPHVALUECODES" = list(
         "ISFISHMORPHS" = list(pk_fields=c("MRPHCD_ID","MRPHVCD_ID"),
                                 fk_fields=c("MRPHCD_ID","MRPHVCD_ID"))
       )
    ),
    filters = list(
      "Species Caught (by name)" = list(filt_tab = "ISSPECIESCODES",
                                        filt_field = c("SPECCD_ID"),
                                        filt_disp = c("COMMON","SPECCD_ID"),
                                        filt_ord = 1
      ),
      "Species Caught (by code)" = list(filt_tab = "ISSPECIESCODES",
                                        filt_field = c("SPECCD_ID"),
                                        filt_disp = c("COMMON","SPECCD_ID"),
                                        filt_ord = 2
      ),
      "Species Sought (by name)" = list(filt_tab = "ISSPECIESSOUGHTCODES",
                                        filt_field = c("SPECSCD_ID"),
                                        filt_disp = c("COMMON","SPECSCD_ID"),
                                        filt_ord = 1
      ),
      "Species Sought (by code)" = list(filt_tab = "ISSPECIESSOUGHTCODES",
                                        filt_field = c("SPECSCD_ID"),
                                        filt_disp = c("COMMON","SPECSCD_ID"),
                                        filt_ord = 2
      ),
      "Observer IDs" = list(filt_tab = "ISOBSERVERCODES",
                            filt_field = c("OBSCD_ID"),
                            filt_disp = c("OBSERVER","OBSCD_ID"),
                            filt_ord = 1
      ),
      "Set Type Codes" = list(filt_tab = "ISSETTYPECODES",
                              filt_field = c("SETCD_ID"),
                              filt_disp = c("SET_TYPE","SETCD_ID"),
                              filt_ord = 1
      ),
      
      "Vessel (by name)" = list(filt_tab = "ISVESSELS",
                                filt_field = c("VESSEL_NAME","LICENSE_NO"),
                                filt_disp = c("VESSEL_NAME","CFV"),
                                filt_ord = 1
      ),
      "Vessel (by VRN)" = list(filt_tab = "ISVESSELS",
                  filt_field = c("VESSEL_NAME","LICENSE_NO"),
                  filt_disp = c("VESSEL_NAME","CFV"),
                  filt_ord = 2
      ),
      "Country Codes" = list(filt_tab = "ISVESSELS",
                             filt_field = c("CTRYCD_ID"),
                             filt_disp = c("CTRYCD_ID"),
                             filt_ord = 1
      ),
      "Trip Type Code" = list(filt_tab = "ISTRIPTYPECODES",
                              filt_field = c("TRIPCD_ID"),
                              filt_disp = c("TRIP_TYPE","TRIPCD_ID"),
                              filt_ord = 2
      ),
      "Gear IDs" = list(filt_tab = "ISGEARCODES",
                        filt_field = c("GEARCD_ID"),
                        filt_disp = c("DESCRIPTION","GEARCD_ID"),
                        filt_ord = 2
      ),
      "Trip Names" = list(filt_tab = "ISTRIPS",
                          filt_field = c("TRIP"),
                          filt_disp = c("TRIP"),
                          filt_ord = 1
      ),
      "Date Range" = list(filt_tab = "ISTRIPS",
                          filt_field = c("BOARD_DATE"),
                          filt_disp = c("BOARD_DATE"),
                          filt_ord = 1
      ),
      "NAFO Areas" = list(filt_tab = "ISFISHSETS",
                          filt_field = c("NAFAREA_ID"),
                          filt_disp = c("NAFAREA_ID"),
                          filt_ord = 1
      ),
      'By Year' = list(filt_tab = 'ISSETPROFILE_WIDE', 
                       filt_fields = c('YEAR'),
                       filt_disp = c('YEAR'), 
                       filt_ord = 1
      )
    )
  )
  
  chid = list (
    db="chid",
    name = "Cape Chidley Surveys",
    schema = "CAPECHIDLEY",
    desc = "Exploratory fishing surveys of the benthic fish fauna at 900-1800m.(Bottom 
	Trawl Survey",
    tables = c("DSINF","DSDET","DSCAT","DSSPEC","DSSTRATUM","GSXTYPE"),
    table_cat = "DSCAT",
    table_det = "DSDET",
    table_pos = "DSINF",
    field_default = "TOTNO",
     joins = list(
       "DSSPEC" = list(
         "DSDET" = list(pk_fields=c("SPEC"),
                                 fk_fields=c("SPEC")),
         "DSCAT" = list(pk_fields=c("SPEC"),
                                 fk_fields=c("SPEC")),
         combine = "OR"
       ),
       "DSINF" = list(
         "DSSTRATUM" = list(pk_fields=c("STRAT"),
                                 fk_fields=c("STRAT")),
         "GSXTYPE" = list(pk_fields=c("TYPE"),
                                 fk_fields=c("XTYPE")),
         combine = "ALL"
       ),
       "DSCAT" = list(
         "DSINF" = list(pk_fields=c("CRUNO","SETNO"),
                                 fk_fields=c("CRUNO","SETNO")),
         "DSSPEC" = list(pk_fields=c("SPEC"),
                                 fk_fields=c("SPEC")),
         combine = "ALL"
       ),
       "DSDET" = list(
         "DSCAT" = list(pk_fields=c("CRUNO","SETNO","SPEC"),
                                 fk_fields=c("CRUNO","SETNO","SPEC"))
       ),
       "DSSTRATUM" = list(
         "DSINF" = list(pk_fields=c("STRAT"),
                                 fk_fields=c("STRAT"))
       ),
       "GSXTYPE" = list(
         "DSINF" = list(pk_fields=c("XTYPE"),
                        fk_fields=c("TYPE"))
       )
     ),
    filters = list(
      "Year" = list(filt_tab = "DSINF",
                    filt_field = c("YEAR"),
                    filt_disp = c("YEAR"),
                    filt_ord = 1
      ),
      "Season" = list(filt_tab = "DSINF",
                      filt_field = c("SEASON"),
                      filt_disp = c("SEASON"),
                      filt_ord = 1
      ),
      "Species Caught (by name)" = list(filt_tab = "DSSPEC",
                                        filt_field = c("SPEC"),
                                        filt_disp = c("COMMON","SPEC"),
                                        filt_ord = 1
      ),
      "Species Caught (by code)" = list(filt_tab = "DSSPEC",
                                        filt_field = c("SPEC"),
                                        filt_disp = c("COMMON","SPEC"),
                                        filt_ord = 2
      ),
      "Stratum" = list(filt_tab = "DSSTRATUM",
                       filt_field = c("STRAT"),
                       filt_disp = c("STRAT"),
                       filt_ord = 1
      ),
      "NAFO area" = list(filt_tab = "DSSTRATUM",
                         filt_field = c("NAME"),
                         filt_disp = c("NAME"),
                         filt_ord = 1
      ),
      "Set Type" = list(filt_tab = "GSXTYPE",
                        filt_field = c("XTYPE"),
                        filt_disp = c("XTYPEDESC","XTYPE"),
                        filt_ord = 2
      )
    )
  )
  
  redfish = list(
    db="redfish",
    name="Redfish Surveys",
    schema = "REDFISH",
    desc= "A survey using stratified random design with day/night replication targeting 
	deep sea redfish.  Data collected includes bycatch, hydro, morphometrics, 
	length, and weight. Database components include Fishing events, catch, and 
	sampling data",
    tables = c("RFINF","RFDET","RFCAT","GSSPECIES","GSSTRATUM","GSXTYPE"),
    table_cat = "RFCAT",
    table_det = "RFDET",
    table_pos = "RFINF",
    field_default = "TOTNO",
    joins = list(
      "GSSPECIES" = list(
        "RFCAT" = list(pk_fields=c("CODE"),
                           fk_fields=c("SPEC")),
        "RFDET" = list(pk_fields=c("CODE"),
                         fk_fields=c("SPEC")),
        combine = "OR"
      ),
      "RFINF" = list(
        "GSSTRATUM" = list(pk_fields=c("STRAT"),
                           fk_fields=c("STRAT")),
        "GSXTYPE" = list(pk_fields=c("TYPE"),
                         fk_fields=c("XTYPE")),
        combine = "ALL"
        
      ),
      "RFCAT" = list(
        "RFINF" = list(pk_fields=c("CRUNO","SETNO"),
                     fk_fields=c("CRUNO","SETNO")),
        "GSSPECIES" = list(pk_fields=c("SPEC"),
                           fk_fields=c("CODE")),
        combine = "ALL"
      ),
      "RFDET" = list(
        "RFCAT" = list(pk_fields=c("CRUNO","SETNO","SPEC"),
                      fk_fields=c("CRUNO","SETNO","SPEC"))
      ),
      "GSSTRATUM" = list(
        "RFINF" = list(pk_fields=c("STRAT"),
                      fk_fields=c("STRAT"))
      ),
      "GSXTYPE" = list(
        "RFINF" = list(pk_fields=c("XTYPE"),
                      fk_fields=c("TYPE"))
      )
    ),
    filters = list(
      "Year" = list(filt_tab = "RFINF",
                    filt_field = c("YEAR"),
                    filt_disp = c("YEAR"),
                    filt_ord = 1
      ),
      "Season" = list(filt_tab = "RFINF",
                      filt_field = c("SEASON"),
                      filt_disp = c("SEASON"),
                      filt_ord = 1
      ),
      "Species Caught (by name)" = list(filt_tab = "GSSPECIES",
                                        filt_field = "CODE",
                                        filt_disp = c("COMM","CODE"),
                                        filt_ord = 1
      ),
      "Species Caught (by code)" = list(filt_tab = "GSSPECIES",
                                        filt_field = c("CODE"),
                                        filt_disp = c("COMM","CODE"),
                                        filt_ord = 2
      ),
      "Stratum" = list(filt_tab = "GSSTRATUM",
                       filt_field = c("STRAT"),
                       filt_disp = c("STRAT"),
                       filt_ord = 1
      ),
      "NAFO area" = list(filt_tab = "GSSTRATUM",
                         filt_field = c("NAME"),
                         filt_disp = c("NAME"),
                         filt_ord = 1
      ),
      "Set Type" = list(filt_tab = "GSXTYPE",
                        filt_field = c("XTYPE"),
                        filt_disp = c("XTYPEDESC","XTYPE"),
                        filt_ord = 1
      )
    )
  )
  
  rvp70 = list (
    db="rvp70",
    name = "Pre-1970s Research Surveys",
    schema = "GROUNDFISH",
    desc = "Data collected on Groundfish Surveys prior to 1970. Database components include 
	Fishing event, catch, and sampling data.",
    tables = c("GSCATP70","GSINFP70","GSDETP70","GSCRUP70","GSSEXP70","GSSPECP70","GSVESP70","GSXTYPE","SPECIES_CODES","GSGEAR"),
    table_cat = "GSCATP70",
    table_det = "GSDETP70",
    table_pos = "GSINFP70",
    field_default = "TOTNO",
    joins = list(
       "SPECIES_CODES" = list(
         "GSCATP70" = list(pk_fields=c("RESEARCH"),
                                 fk_fields=c("SPEC")),
         "GSDETP70" = list(pk_fields=c("RESEARCH"),
                                 fk_fields=c("SPEC")),
         combine = "OR"
       ),
       "GSSPECP70" = list(
         "GSCATP70" = list(pk_fields=c("SPEC"),
                                 fk_fields=c("SPEC")),
         "GSDETP70" = list(pk_fields=c("SPEC"),
                                 fk_fields=c("SPEC")),
         combine = "OR"
       ),
       "GSVESP70" = list(
         "GSINFP70" = list(pk_fields=c("VESEL"),
                                 fk_fields=c("VESEL"))
       ),
       "GSGEAR" = list(
         "GSINFP70" = list(pk_fields=c("GEAR"),
                                 fk_fields=c("GEAR"))
       ),
       "GSCRUP70" = list(
         "GSVESP70" = list(pk_fields=c("VESEL"),
                                 fk_fields=c("VESEL"))
       ),
       "GSCATP70" = list(
         "GSINFP70" = list(pk_fields=c("MISSION","SETNO"),
                                 fk_fields=c("MISSION","SETNO")),
         "SPECIES_CODES" = list(pk_fields=c("SPEC"),
                                 fk_fields=c("RESEARCH")),
         "GSVESP70" = list(pk_fields=c("VESEL"),
                                fk_fields=c("VESEL")),
         combine = "ALL"
       ),
       "GSDETP70" = list(
         "GSCATP70" = list(pk_fields=c("MISSION","SETNO","SPEC"),
                                 fk_fields=c("MISSION","SETNO","SPEC"))
       ),
       "GSINFP70" = list(
         "GSXTYPE" = list(pk_fields=c("TYPE"),
                                 fk_fields=c("XTYPE")),
         "GSVESP70" = list(pk_fields=c("VESEL"),
                                 fk_fields=c("VESEL")),
         combine = "ALL"
       )
    ),
    filters = list(
      "Mission Name" = list(filt_tab = "GSINFP70",
                            filt_field = c("MISSION"),
                            filt_disp = c("MISSION"),
                            filt_ord = 1
      ),
      "Areas (Strata)" = list(filt_tab = "GSINFP70",
                              filt_field = c("AREA"),
                              filt_disp = c("AREA"),
                              filt_ord = 1
      ),
      "Mission Year" = list(filt_tab = "GSCRUP70",
                            filt_field = c("CR_YEAR"),
                            filt_disp = c("CR_YEAR"),
                            filt_ord = 1
      ),
      "Vessel" = list(filt_tab = "GSVESP70",
                      filt_field = c("VESEL"),
                      filt_disp = c("VNAME","VESEL"),
                      filt_ord = 1
      ),
      "Set Type" = list(filt_tab = "GSXTYPE",
                        filt_field = c("XTYPE"),
                        filt_disp = c("XTYPEDESC"),
                        filt_ord = 2
      ),
      "Species Caught (by name)" = list(filt_tab = "SPECIES_CODES",
                                        filt_field = c("RESEARCH"),
                                        filt_disp = c("COMMON","RESEARCH"),
                                        filt_ord = 1
      ),
      "Species Caught (by code)" = list(filt_tab = "SPECIES_CODES",
                                        filt_field = c("RESEARCH"),
                                        filt_disp = c("COMMON","RESEARCH"),
                                        filt_ord = 2
      ),
      "Gear" = list(filt_tab = "GSGEAR",
                    filt_field = c("GEAR"),
                    filt_disp = c("GEARDESC","GEAR"),
                    filt_ord = 2
      )
    )
  )
  
  marfis = list (
    db="marfis",
    name = "MARFIS (Maritime Fishery Information System)",
    schema = "MARFISSCI",
    desc = "A Policy and Economics Branch database that houses information on the fisheries 
	of the Scotia-Fundy region, including data related to catch and effort.",
    tables = c("LOG_SPC_STD_INFO","LOG_EFRT_STD_INFO","GEARS","SPECIES","SPECIES_CATEGORIES","NAFO_UNIT_AREAS","AREAS","CATCH_USAGES","MON_DOCS","PRO_SPC_INFO","VESSELS","HAIL_IN_CALLS"),
    table_cat = "LOG_SPC_STD_INFO",
    table_pos = "PRO_SPC_INFO",
    field_default = "RND_WEIGHT_KGS",
    field_drops = c('CDATE',
                    'UUSER',
                    'UDATE',
                    'CUSER',
                    'MON_DOC_CUSER', 
                    'CE_FLAG',
                    'TRIP_DMP_COMPANY_ID',
                    'COMMENTS'),
    field_private = c('CALLERS_NAME',
                      'VR_NUMBER',
                      'VR_NUMBER_LANDING',
                      'VR_NUMBER_FISHING',
                      'VESSEL_NAME',
                      'LICENCE_ID',
                      'MON_DOC_LIC_ID'),
    joins = list(
       "MON_DOCS" = list(
         #not sure I can reference same table 3 different times like this
         "VESSELS" = list(pk_fields=c("VR_NUMBER"),
                                 fk_fields=c("VR_NUMBER")),
         "GEARS" = list(pk_fields=c("FV_GEAR_CODE"),
                                 fk_fields=c("GEAR_CODE")),
         "HAIL_IN_CALLS" = list(pk_fields=c("HAIL_IN_CALL_ID"),
                                 fk_fields=c("HAIL_IN_CALL_ID")),
         combine = "ALL"
       ),
       "MON_DOCS" = list(
         "LOG_EFRT_STD_INFO" = list(pk_fields=c("MON_DOC_ID"),
                                 fk_fields=c("MON_DOC_ID")),
         "LOG_SPC_STD_INFO" = list(pk_fields=c("MON_DOC_ID"),
                                 fk_fields=c("MON_DOC_ID")),
         combine = "OR"
       ),
       "MON_DOCS" = list(
         "NAFO_UNIT_AREAS" = list(pk_fields=c("FV_NAFO_UNIT_AREA_ID"),
                                    fk_fields=c("AREA_ID")),
         "AREAS" = list(pk_fields=c("FV_FISHING_AREA_ID"),
                                   fk_fields=c("AREA_ID")),
         combine = "OR"
       ),
       "VESSELS" = list(
         #not sure I can reference same table 3 different times like this
         "PRO_SPC_INFO" = list(pk_fields=c("VR_NUMBER"),
                                 fk_fields=c("VR_NUMBER_FISHING")),
         "PRO_SPC_INFO" = list(pk_fields=c("VR_NUMBER"),
                                 fk_fields=c("VR_NUMBER_LANDING")),
         combine = "OR"
       ),
       "SPECIES_CATEGORIES" = list(
         "SPECIES" = list(pk_fields=c("SPECIES_CATEGORY_ID"),
                                 fk_fields=c("SPECIES_CATEGORY_ID"))
       ),
       "LOG_EFRT_STD_INFO" = list(
         # "MON_DOCS" = list(pk_fields=c("MON_DOC_ID"),
         #                         fk_fields=c("MON_DOC_ID"))
         # ,
         "PRO_SPC_INFO" = list(pk_fields=c("LOG_EFRT_STD_INFO_ID"),
                               fk_fields=c("LOG_EFRT_STD_INFO_ID"))
         # combine = "AND"
       ),
       "LOG_SPC_STD_INFO" = list(
         "PRO_SPC_INFO" = list(pk_fields=c("LOG_EFRT_STD_INFO_ID"),
                                 fk_fields=c("LOG_EFRT_STD_INFO_ID")),
         "SPECIES" = list(pk_fields=c("SSF_SPECIES_CODE"),
                                  fk_fields=c("SPECIES_CODE"))
         # "CATCH_USAGES" = list(pk_fields=c("CATCH_USAGE_CODE"),
         #                       fk_fields=c("CATCH_USAGE_CODE")),
         # combine = "ALL"
       ),
       "PRO_SPC_INFO" = list(
         "LOG_EFRT_STD_INFO" = list(pk_fields=c("LOG_EFRT_STD_INFO_ID"),
                                    fk_fields=c("LOG_EFRT_STD_INFO_ID")),
         "SPECIES" = list(pk_fields=c("SPECIES_CODE"),
                          fk_fields=c("SPECIES_CODE")),
         "CATCH_USAGES" = list(pk_fields=c("CATCH_USAGE_CODE"),
                               fk_fields=c("CATCH_USAGE_CODE")),
         combine = "ALL"
       ),
       "PRO_SPC_INFO" = list(
         "AREAS" = list(pk_fields=c("FISHING_AREA_ID"),
                                 fk_fields=c("AREA_ID")),
         "NAFO_UNIT_AREAS" = list(pk_fields=c("NAFO_UNIT_AREA_ID"),
                                 fk_fields=c("AREA_ID")),
         combine = "ALL"
       ),
       "HAIL_IN_CALLS" = list(
         "MON_DOCS" = list(pk_fields=c("HAIL_IN_CALL_ID"),
                                 fk_fields=c("HAIL_IN_CALL_ID")),
         "VESSELS" = list(pk_fields=c("VR_NUMBER"),
                                 fk_fields=c("VR_NUMBER")),
         combine = "ALL"
       ),
       "GEARS" = list(
         "MON_DOCS" = list(pk_fields=c("GEAR_CODE"),
                                 fk_fields=c("FV_GEAR_CODE"))
       ),
       "SPECIES" = list(
         "PRO_SPC_INFO" = list(pk_fields=c("SPECIES_CODE"),
                                 fk_fields=c("SPECIES_CODE"))
       ),
       "AREAS" = list(
         "PRO_SPC_INFO" = list(pk_fields=c("AREA_ID"),
                                 fk_fields=c("FISHING_AREA_ID"))
       ),
       "NAFO_UNIT_AREAS" = list(
         "PRO_SPC_INFO" = list(pk_fields=c("AREA_ID"),
                                 fk_fields=c("NAFO_UNIT_AREA_ID"))
       ),
       "CATCH_USAGES" = list(
         "LOG_SPC_STD_INFO" = list(pk_fields=c("CATCH_USAGE_CODE"),
                                 fk_fields=c("CATCH_USAGE_CODE")),
         "PRO_SPC_INFO" = list(pk_fields=c("CATCH_USAGE_CODE"),
                                 fk_fields=c("CATCH_USAGE_CODE")),
         combine = "OR"
       )
    ),
    filters = list(
      "Gear" = list(filt_tab = "GEARS",
                    filt_field = c("GEAR_CODE"),
                    filt_disp = c("GEAR","GEAR_CODE"),
                    filt_ord = 1
      ),
      "Species Caught (by name)" = list(filt_tab = "SPECIES",
                                        filt_field = c("SPECIES_CODE"),
                                        filt_disp = c("SPECIES_NAME","SPECIES_CODE"),
                                        filt_ord = 1
      ),
      "Species Caught (by code)" = list(filt_tab = "SPECIES",
                                        filt_field = c("SPECIES_CODE"),
                                        filt_disp = c("SPECIES_NAME","SPECIES_CODE"),
                                        filt_ord = 2
      ),
      "Species Caught Category" = list(filt_tab = "SPECIES_CATEGORIES",
                                       filt_field = c("SPECIES_CATEGORY_ID"),
                                       filt_disp = c("SPECIES_CATEGORY","SPECIES_CATEGORY_ID"),
                                       filt_ord = 1
      ),
      "NAFO area" = list(filt_tab = "NAFO_UNIT_AREAS",
                         filt_field = c("NAFO_AREA"),
                         filt_disp = c("NAFO_AREA"),
                         filt_ord = 1
      ),
      "Other area" = list(filt_tab = "AREAS",
                          filt_field = c("AREA"),
                          filt_disp = c("AREA"),
                          filt_ord = 1
      ),
      "Catch Usage" = list(filt_tab = "CATCH_USAGES",
                           filt_field = c("CATCH_USAGE_CODE"),
                           filt_disp = c("CATCH_USAGE","CATCH_USAGE_CODE"),
                           filt_ord = 1
      ),
      "Year" = list(filt_tab = "PRO_SPC_INFO",
                    filt_field = c("YEAR"),
                    filt_disp = c("YEAR"),
                    filt_ord = 1
      ),
      "Vessel (by name)" = list(filt_tab = "VESSELS",
                                filt_field = c("VR_NUMBER"),
                                filt_disp = c("VESSEL_NAME","VR_NUMBER"),
                                filt_ord = 1
      ),
      "Vessel (by VRN)" = list(filt_tab = "VESSELS",
                               filt_field = c("VR_NUMBER"),
                               filt_disp = c("VESSEL_NAME","VR_NUMBER"),
                               filt_ord = 2
      )
    )
  )
  ##########
  comland86 = list (
    db="comland86",
    name = "Commercial Landings (1986-2001)",
    schema = "comland",
    desc = "
    COMLAND 1986-2001",
    tables = c("C_1986_2001","I_1986_2001","S_1986_2001","DFO_REGIONS","PROVINCES","SPECIES","SPECIES_SOUGHT","UNITS_OF_MEASURE","GEAR_TYPES"),
    table_cat = "I_1986_2001",
    table_pos = "S_1986_2001",
    field_default = "LIVE_WT",
    joins = list(
      #sets might not include a catch so don't join i_
      "S_1986_2001" = list(
        "C_1986_2001" = list(pk_fields=c("YEAR_OF_ACTIVITY","CFV_NUMBER","CATCHERS_RECID","REGION_CODE"),
                             fk_fields=c("YEAR_OF_ACTIVITY","CFV_NUMBER","CATCHERS_RECID","REGION_CODE")),
        "GEAR_TYPES" = list(pk_fields=c("GEAR_TYPE"),
                            fk_fields=c("GEAR_TYPE_CODE")),
        "DFO_REGIONS" = list(pk_fields=c("REGION_CODE"),
                             fk_fields=c("REGION_CODE")),
        "SPECIES" = list(pk_fields=c("MAIN_SPECIES_CAUGHT"),
                         fk_fields=c("SPECIES_CODE")),
        "SPECIES_SOUGHT" = list(pk_fields=c("MAIN_SPECIES_SOUGHT"),
                                fk_fields=c("SPECIES_CODE")),
        combine = "ALL"
      ),
      "I_1986_2001" = list(
        #catches require a set, so join s_
        "C_1986_2001" = list(pk_fields=c("YEAR_OF_ACTIVITY","CFV_NUMBER","CATCHERS_RECID","REGION_CODE"),
                             fk_fields=c("YEAR_OF_ACTIVITY","CFV_NUMBER","CATCHERS_RECID","REGION_CODE")),
        "S_1986_2001" = list(pk_fields=c("YEAR_OF_ACTIVITY","CFV_NUMBER","CATCHERS_RECID","TRIP_NUM","SUB_TRIP_NUM"),
                             fk_fields=c("YEAR_OF_ACTIVITY","CFV_NUMBER","CATCHERS_RECID","TRIP_NUM","SUB_TRIP_NUM")),
        "SPECIES" = list(pk_fields=c("SPECIES_CODE"),
                         fk_fields=c("SPECIES_CODE")),
        combine = "ALL"
      ),
      "C_1986_2001" = list(
        #don't join i_ just need to join sets 
        "PROVINCES" = list(pk_fields=c("PROV_CODE"),
                           fk_fields=c("PROV_CODE")),
        "S_1986_2001" = list(pk_fields=c("YEAR_OF_ACTIVITY","CFV_NUMBER","CATCHERS_RECID"),
                             fk_fields=c("YEAR_OF_ACTIVITY","CFV_NUMBER","CATCHERS_RECID")),
        combine = "ALL"
      ),
      "SPECIES" = list(
        "S_1986_2001" = list(pk_fields=c("SPECIES_CODE"),
                             fk_fields=c("MAIN_SPECIES_CAUGHT"))
      ),
      "SPECIES_SOUGHT" = list(
        "S_1986_2001" = list(pk_fields=c("SPECIES_CODE"),
                             fk_fields=c("MAIN_SPECIES_SOUGHT"))
      ),
      "GEAR_TYPES" = list(
        "S_1986_2001" = list(pk_fields=c("GEAR_TYPE_CODE"),
                             fk_fields=c("GEAR_TYPE"))
      ),
      "PROVINCES" = list(
        "I_1986_2001" = list(pk_fields=c("PROV_CODE"),
                             fk_fields=c("LAND_PROV_CODE"))
        ),
        "UNITS_OF_MEASURE" = list(
          "I_1986_2001" = list(pk_fields=c("UNIT_CODE"),
                               fk_fields=c("UNIT_CODE"))
      )
    ),
    filters = list(
      "Year" = list(filt_tab = "I_1986_2001",
                    filt_field = c("YEAR_OF_ACTIVITY"),
                    filt_disp = c("YEAR_OF_ACTIVITY"),
                    filt_ord = 1
      ),
      "Province" = list(filt_tab = "PROVINCES",
                        filt_field = c("PROV_NAME"),
                        filt_disp = c("PROV_NAME"),
                        filt_ord = 1
      ),
      "DFO Region" = list(filt_tab = "DFO_REGIONS",
                          filt_field = c("REGION_NAME"),
                          filt_disp = c("REGION_NAME"),
                          filt_ord = 1
      ),
      "Gear Types" = list(filt_tab = "GEAR_TYPES",
                          filt_field = c("GEAR_TYPE_DESC"),
                          filt_disp = c("GEAR_TYPE_DESC"),
                          filt_ord = 1
      ),
      "NAFO Division" = list(filt_tab = "S_1986_2001",
                             filt_field = c("NAFO_DIVISION_CODE"),
                             filt_disp = c("NAFO_DIVISION_CODE"),
                             filt_ord = 1
      ),
      "Species Caught" = list(filt_tab = "SPECIES",
                              filt_field = c("SPECIES_CODE"),
                              filt_disp = c("SPECIES_NAME","SPECIES_CODE"),
                              filt_ord = 1
      ),
      "Species Sought" = list(filt_tab = "SPECIES_SOUGHT",
                              filt_field = c("SPECIES_CODE"),
                              filt_disp = c("SPECIES_NAME","SPECIES_CODE"),
                              filt_ord = 1
      ),
      "CFV" = list(filt_tab = "C_1986_2001",
                   filt_field = c("CFV_NUMBER"),
                   filt_disp = c("CFV_NUMBER"),
                   filt_ord = 1
      )
    )
  )
  ##########
  comland67 = list (
    db="comland67",
    name = "Commercial Landings (1967-1985)",
    schema = "comland",
    desc = "
    COMLAND 1967-1985",
    tables = c("C_1967_1985","I_1967_1985","S_1967_1985","DFO_REGIONS","PROVINCES","SPECIES_PRE_1986","UNITS_OF_MEASURE","GEAR_TYPES_PRE_1986"),
    table_cat = "I_1967_1985",
    table_pos = "S_1967_1985",
    field_default = "LIVE_WT",
    joins = list(
      "S_1967_1985" = list(
        "C_1967_1985" = list(pk_fields=c("YEAR_OF_ACTIVITY","CFV_NUMBER","CATCHERS_RECID","REGION_CODE"),
                             fk_fields=c("YEAR_OF_ACTIVITY","CFV_NUMBER","CATCHERS_RECID","REGION_CODE")),
        "GEAR_TYPES_PRE_1986" = list(pk_fields=c("GEAR_TYPE"),
                            fk_fields=c("GEAR_TYPE_CODE")),
        "DFO_REGIONS" = list(pk_fields=c("REGION_CODE"),
                             fk_fields=c("REGION_CODE")),
        combine = "ALL"
      ),
      "I_1967_1985" = list(
        "SPECIES_PRE_1986" = list(pk_fields=c("SPECIES_CODE"),
                         fk_fields=c("SPECIES_CODE")),
        "S_1967_1985" = list(pk_fields=c("YEAR_OF_ACTIVITY","CFV_NUMBER","CATCHERS_RECID","TRIP_NUM","SUB_TRIP_NUM"),
                             fk_fields=c("YEAR_OF_ACTIVITY","CFV_NUMBER","CATCHERS_RECID","TRIP_NUM","SUB_TRIP_NUM")),
        "PROVINCES" = list(pk_fields=c("LAND_PROV_CODE"),
                           fk_fields=c("PROV_CODE")),
        combine = "ALL"
      ),
      "C_1967_1985" = list(
        "I_1967_1985" = list(pk_fields=c("YEAR_OF_ACTIVITY","CFV_NUMBER","CATCHERS_RECID"),
                             fk_fields=c("YEAR_OF_ACTIVITY","CFV_NUMBER","CATCHERS_RECID"))
      ),
      "SPECIES_PRE_1986" = list(
        "I_1967_1985" = list(pk_fields=c("SPECIES_CODE"),
                             fk_fields=c("SPECIES_CODE"))
      ),
      "DFO_REGIONS" = list(
        "S_1967_1985" =  list(pk_fields=c("REGION_CODE"),
                              fk_fields=c("REGION_CODE"))
      ),
      "GEAR_TYPES_PRE_1986" = list(
        "S_1967_1985" = list(pk_fields=c("GEAR_TYPE_CODE"),
                             fk_fields=c("GEAR_TYPE"))
      ),
      "PROVINCES" = list(
        "I_1967_1985" = list(pk_fields=c("PROV_CODE"),
                             fk_fields=c("LAND_PROV_CODE"))
      ),
      "UNITS_OF_MEASURE" = list(
        "I_1967_1985" = list(pk_fields=c("UNIT_CODE"),
                             fk_fields=c("UNIT_CODE"))
      )
    ),
    filters = list(
      "Year" = list(filt_tab = "I_1967_1985",
                    filt_field = c("YEAR_OF_ACTIVITY"),
                    filt_disp = c("YEAR_OF_ACTIVITY"),
                    filt_ord = 1
      ),
       "Landed Province" = list(filt_tab = "PROVINCES",
                         filt_field = c("PROV_NAME"),
                         filt_disp = c("PROV_NAME"),
                         filt_ord = 1
       ),
      "DFO Region" = list(filt_tab = "DFO_REGIONS",
                          filt_field = c("REGION_NAME"),
                          filt_disp = c("REGION_NAME"),
                          filt_ord = 1
      ),
      "Gear Types" = list(filt_tab = "GEAR_TYPES_PRE_1986",
                          filt_field = c("GEAR_TYPE_DESC"),
                          filt_disp = c("GEAR_TYPE_DESC"),
                          filt_ord = 1
      ),
      "NAFO Division" = list(filt_tab = "S_1967_1985",
                             filt_field = c("NAFO_DIVISION_CODE"),
                             filt_disp = c("NAFO_DIVISION_CODE"),
                             filt_ord = 1
      ),
      "Species Caught" = list(filt_tab = "SPECIES_PRE_1986",
                              filt_field = c("SPECIES_CODE"),
                              filt_disp = c("SPECIES_NAME","SPECIES_CODE"),
                              filt_ord = 1
      ),
      # "Species Sought" = list(filt_tab = "SPECIES_SOUGHT_PRE_1986",
      #                         filt_field = c("SPECIES_CODE"),
      #                         filt_disp = c("SPECIES_NAME","SPECIES_CODE"),
      #                         filt_ord = 1
      # ),
      "CFV" = list(filt_tab = "C_1967_1985",
                   filt_field = c("CFV_NUMBER"),
                   filt_disp = c("CFV_NUMBER"),
                   filt_ord = 1
      )
    )
  )
  
  datasources = list(rv=rv, rvp70=rvp70, chid=chid, redfish=redfish, 
                     isdb=isdb, 
                     marfis=marfis, comland86=comland86, comland67=comland67)
  
  
  generic_filts = list(
    'By Polygon' = list(filt_tab = 'table_pos',
                        filt_field = ('thePoly')
    ),
    'Location' = list(filt_tab = 'table_pos',
                      filt_field = ('COORDS')
                      
    ),
    'All Done' = list()
  )
  
  for (i in 1:length(datasources)){
    for (j in 1:length(generic_filts)){
      generic_filts[j][[1]]$filt_tab=datasources[[i]]$table_pos
    }
    datasources[[i]]$filters = c(datasources[[i]]$filters, generic_filts)
  }
  
  if (!is.null(db)) datasources = datasources[[db]]
  return(datasources)
}