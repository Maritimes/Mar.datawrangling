 #' @title data_filter
#' @description This is a GUI-driven script. It loads stored data objects into the
#' workspace, and presents the user with a bunch of methods for filtering. As filters are
#' applied, the data objects in the workspace are whittled down such that the only data that is
#' presented in successive filters directly applies to the users' previous selections.
#' For example, if the user only wants data where the species caught is halibut, all records where
#' caught species is not halibut are dropped, and this might mean that certain areas, gears,
#' vessels, etc will also be dropped, since they are not associated with catches of halibut.
#' @param refresh.data default is \code{FALSE}.  The purpose of this function is to filter the 
#' data you have stored on your computer to get the records you're interested in.  If this is set to
#' FALSE, you can perform multiple filters on the same data.  If it is set to TRUE, any filtered
#' data will be discarded, and the raw data will be reloaded - all previous filtering will be lost.
#' @param db default is \code{NULL}. This identifies the dataset you are working 
#' with.
#' @param safe default is \code{TRUE}.  If TRUE, this will ensure that selection 
#' boxes are only populated with viable options.  If FALSE, invalid values are 
#' possible.  Turning this off omits the initial self_filter(), and causes the 
#' script to run faster.  It's only advisable if you know your selections are 
#' appropriate, and in that case, it's a good idea.
#' @note
#' If editing, please be aware of the scope of the data - the approach was to load the data into the
#' global environment, and then do all the filtering there.  This is why subsets require
#' \code{assign(..., envir = GlobalEnv)} and references to \code{envir = GlobalEnv} are littered
#' everywhere.
#' @return list of the filters that were applied to generate the remaining dataset
#' @family dfo_extractions
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom utils select.list
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom lubridate ymd
#' @importFrom RODBC odbcConnect
#' @importFrom RODBC sqlQuery
#' @importFrom RODBC odbcClose
#' @export
#' @note This line is here to prevent an error message claiming the export is mult-line
data_filter = function(db=.GlobalEnv$db, refresh.data = FALSE, safe = TRUE) {
  if (is.null(db))db = ds_all[[.GlobalEnv$db]]$db
  this.filter = make_log(refresh.data)
  filters = ds_all[[.GlobalEnv$db]]$filters
  
  get_location <- function() {
    #' This function prompts the user to provide the bounding coordinates for an area of
    #' interest. Values for longitude are automatically converted to negative if they were not provided
    #' that way.  While it asks for specific values for N/S and E/W values, the script determines the
    #' users intent by look at the max and min values provided.
    #' it returns a vector of 4 values, corresponding with c(N, S, E, W)
    S = as.numeric(readline("Enter the Southern Limit in Decimal Degrees: "))
    N = as.numeric(readline("Enter the Northern Limit in Decimal Degrees: "))
    E = as.numeric(readline("Enter the Eastern Limit in Decimal Degrees: "))
    W = as.numeric(readline("Enter the Western Limit in Decimal Degrees: "))
    if (!is.na(S) & !is.na(N)){
      lats = c(S, N)
      S = min(range(lats))
      N = max(range(lats))
    }
    if (!is.na(W) & W > 0) W = W * -1
    if (!is.na(E) & E > 0) E = E * -1
    if (!is.na(W) & !is.na(E)){
      lons = c(W, E)
      W = min(range(lons))
      E = max(range(lons))
    }
    bounds = c(S, N, E, W)
    return(bounds)
  }
  
  get_date <- function(df) {
    #This function whittles down a simple df to help identify start and end dates of
    #' interest.  If cancelled during the "start" date selections, it will get the minimum/earliest
    #' values, if cancelled during the "end" date selection, it will get the maximimum/most recent
    #' values.
    #' It returns a vector of 2 dates corresponding with the selected start and end values
    start.df = df
    start.year = select.list(
      as.character.POSIXt(sort(unique(year(start.df[,1])))),
      multiple = F,
      graphics = T,
      title = paste("Data as early as (Year)")
    )
    if (start.year == "") start.year = min(year(start.df))
    start.df = start.df[,1][year(start.df[,1]) >= start.year]
    start.month = select.list(
      as.character.POSIXt(formatC(sort(unique(month(start.df[year(start.df) == start.year]))), width = 2, flag = "0")),
      multiple = F,
      graphics = T,
      title = paste("Data as early as (Month)")
    )
    
    if (start.month == "") start.month = min(month(start.df))
    start.day = select.list(
      as.character.POSIXt(formatC(sort(unique(day(start.df[year(start.df) == start.year &  month(start.df) == as.numeric(start.month)]))), width = 2, flag = "0")),
      multiple = F,
      graphics = T,
      title = paste("Data as early as (Day)")
    )
    if (start.day == "") start.day = min(day(start.df))
    end.df = start.df
    end.year = select.list(
      as.character.POSIXt(sort(unique(year(end.df)))),
      multiple = F,
      graphics = T,
      title = paste("Until Date (Year)")
    )
    if (end.year == "") end.year = max(year(end.df))
    end.df = end.df[year(end.df) == end.year]
    end.month = select.list(
      as.character.POSIXt(formatC(sort(unique(month(end.df[year(end.df) == end.year]))), width = 2, flag = "0")),
      multiple = F,
      graphics = T,
      title = paste("Until Date (Month)")
    )
    if (end.month == "") end.month = max(month(end.df))
    end.day = select.list(
      as.character.POSIXt(formatC(sort(unique(day(end.df[year(end.df) == end.year & month(end.df) == as.numeric(end.month)]))), width = 2, flag = "0")),
      multiple = F,
      graphics = T,
      title = paste("Until Date (Day)")
    )
    if (end.day == "") end.day = max(day(end.df))
    start.date = ymd(paste(start.year,start.month, start.day))
    end.date = ymd(paste(end.year, end.month, end.day))
    
    res = c(start.date, end.date)
    return(res)
  }
  
  
  get.licence.field = function(filt, df) {
    #get licence_no and vess names (using names)get('ISCATCHES')
    choice = filt[[1]]
    this.df = get(choice$filt_tab)
    the.res = unique(this.df[this.df[, choice$filt_field[1]] %in% df, ][c(choice$filt_field[1], choice$filt_field[2])])
    return(the.res)
  }
  getChoice = function(choice) {
    if (names(choice) == 'Date Range') {
      this.df = sort(unique(get(choice[[1]]$filt_tab)[choice[[1]]$filt_disp[1]][, 1]))
      df.yr = year(this.df)
      df.mo = month(this.df)
      df.day = day(this.df)
      this.df.date = data.frame(sort(unique(ymd(paste(df.yr, df.mo, df.day)))))
      colnames(this.df.date)[1] = "date"
      selected.option.sanitized = get_date(this.df.date)
    } else if (names(choice) == 'By Polygon') {
      selected.option.sanitized = ''
      f=1
      while (!file.exists(selected.option.sanitized) | 
             (ifelse(regexpr("\\.([[:alnum:]]+)$", selected.option.sanitized)>-1L, substring(selected.option.sanitized, regexpr("\\.([[:alnum:]]+)$", selected.option.sanitized) + 1L), "") !='shp')){
      if (f>1) {
        cat("Invalid path, or filename does not include the .shp extension\n")
        cat("Ensure you are referencing the full path, or that it's relative to your working directory")
      }
        selected.option.sanitized = readline("Enter the full path to the shapefile \nyou want to use to clip (include the '.shp'): \n")
      f=f+1
      }
    }else if (names(choice) == 'Location') {
      
      selected.option.sanitized <- get_location()
    } else {

      #present filtering options for selection, and capture choice
      this.df = get(choice[[1]]$filt_tab)
      this.name = choice[[1]]$filt_disp[1]
      if (length(choice[[1]]$filt_disp) > 1) {
        this.id = choice[[1]]$filt_disp[2]

        #' create the select list, and order it as specified
        these.options = unique(cbind(this.df[this.name], this.df[this.id]))
        these.options = these.options[order(these.options[, choice[[1]]$filt_disp[choice[[1]]$filt_ord]]), ]

        selected.option = select.list(
          paste(these.options[, 1], " (", these.options[, 2], ")", sep = ""),
          multiple = T,
          graphics = T,
          title = paste("Filter by", names(choice))
        )
        if ((names(choice) == 'Vessel (by name)' | names(choice) == 'Vessel (by VRN)') & choice[[1]]$filt_tab == 'ISVESSELS') {
          #'Vessels are a pain - replace special chars and remove spaces
          selected.option.sanitized = gsub('\\s\\([^)]*\\)', '\\1', selected.option)
        } else if (names(choice) == 'Vessel' & choice[[1]]$filt_tab == 'GSVESP70') {
          selected.option.sanitized = gsub('.+\\(([a-zA-Z]+)\\).*?$', '\\1', selected.option)
        }else{
          selected.option.sanitized = as.numeric(gsub('.+\\(([0-9]+)\\).*?$', '\\1', selected.option))
        }
      } else{
        these.options = unique(this.df[this.name])
        these.options = these.options[order(these.options[, choice[[1]]$filt_field]), ]
        selected.option = select.list(
          as.character(these.options),
          multiple = T,
          graphics = T,
          title = paste("Filter by", names(choice))
        )
        selected.option.sanitized = selected.option
      }
    }
    sel.df = data.frame(selected.option.sanitized)
    colnames(sel.df)[1] <- choice[[1]]$filt_field[1]

    if ((names(choice) == 'Vessel (by name)' | names(choice) == 'Vessel (by VRN)') & choice[[1]]$filt_tab == 'ISVESSELS') {
      #for vessels, we need to get their license_no using their name
      sel.df = get.licence.field(choice, selected.option.sanitized)
    }
    
    choices = list(choice, sel.df)
    return(choices)
  }
  deBloat = function(filt) {
    #filt contains 2 lists:
    #[[1]] the filter details - identical to choice from getchoice()
    #[[2]] a dataframe of the selections
    filt_dets = filt[[1]]
    filt_data = filt[[2]]
    
    if (names(filt_dets) == 'Date Range') {
       assign(
        filt_dets[[1]]$filt_tab,
        subset(get(filt_dets[[1]]$filt_tab), get(filt_dets[[1]]$filt_field[1]) >= filt_data[1,1] &
                                                             get(filt_dets[[1]]$filt_field[1]) <= filt_data[2,1])
        , envir = .GlobalEnv)
    } else if (names(filt_dets) == 'Location') {
      #coords: c(S, N, E, W)
      if (!is.na(filt_data[1, 1]))
        assign(filt_dets[[1]]$filt_tab, subset(get(filt_dets[[1]]$filt_tab, envir = .GlobalEnv),
          get(filt_dets[[1]]$filt_tab)[, ("LATITUDE")] >=  filt_data[1, 1]), envir = .GlobalEnv)
      if (!is.na(filt_data[2, 1]))
        assign(filt_dets[[1]]$filt_tab, subset(get(filt_dets[[1]]$filt_tab, envir = .GlobalEnv),
          get(filt_dets[[1]]$filt_tab)[, ("LATITUDE")] <=  filt_data[2, 1]), envir = .GlobalEnv)
      if (!is.na(filt_data[3, 1]))
        assign(filt_dets[[1]]$filt_tab, subset(get(filt_dets[[1]]$filt_tab, envir = .GlobalEnv),
          get(filt_dets[[1]]$filt_tab)[, ("LONGITUDE")] <=  filt_data[3, 1]), envir = .GlobalEnv)
      if (!is.na(filt_data[4, 1]))
        assign(filt_dets[[1]]$filt_tab, subset(get(filt_dets[[1]]$filt_tab, envir = .GlobalEnv),
         get(filt_dets[[1]]$filt_tab)[, ("LONGITUDE")] >=  filt_data[4, 1]), envir = .GlobalEnv)
    } else if (names(filt_dets) == 'By Polygon') {
      buffer.m = readline("If you want to add a buffer so that data can fall slightly beyond \nyour polygon, please enter it here (in meters):\n")
      if (buffer.m == "") buffer.m =0
      assign(filt_dets[[1]]$filt_tab, clip_by_poly(df=get(filt_dets[[1]]$filt_tab), buffer.m = buffer.m, clip.poly = filt_data[[1]]), envir = .GlobalEnv)
    } else{
      if (length(filt_dets[[1]]$filt_field) == 2) {
        assign(
          filt_dets[[1]]$filt_tab,
          subset(
            get(filt_dets[[1]]$filt_tab),
            get(filt_dets[[1]]$filt_tab)[, filt_dets[[1]]$filt_field[1]] %in% filt_data[, filt_dets[[1]]$filt_field[1]] &
              get(filt_dets[[1]]$filt_tab)[, filt_dets[[1]]$filt_field[2]] %in% filt_data[, filt_dets[[1]]$filt_field[2]]
          ),
          envir = .GlobalEnv
        )
      } else{
        assign(filt_dets[[1]]$filt_tab,
               subset(
                 get(filt_dets[[1]]$filt_tab),
                 get(filt_dets[[1]]$filt_tab)[, filt_dets[[1]]$filt_field] %in% filt_data[, filt_dets[[1]]$filt_field]
               ),
               envir = .GlobalEnv)
      }
      #instantiate filtered object
      assign(filt_dets[[1]]$filt_tab, get(filt_dets[[1]]$filt_tab))
    }
    self_filter(db) 
  }
  if (refresh.data) sapply(ds_all[[.GlobalEnv$db]]$tables, simplify=TRUE, get_data)
  if (activity_log$filterno<4 & safe == TRUE)cat("
The following step removes filtering options that would result in no records 
being returned.  It is performed even before any selections are made since many 
of the code tables have values that never show up in the actual data.  It is 
also performed every time a selection is chosen.\n
As filtering proceeds, the number of records remaining in each table of your
datasource will be displayed.  Sometimes the filtering happens multiple times, 
as removals from one table result in removals from another.\n
If you want to see what records are being removed, you can run install the 
bio.qcdata package and run qc_data(db).  It will capture the 'orphaned' records 
so that you can examine them more closely.\n")
if (safe==TRUE)self_filter(db)
  while (length(filters) > 0) {
    cat("\nR isn't frozen - it's awaiting your selection in the pop-ups.\n")
    this.filter.name =  select.list(unlist(lapply(names(filters), function(l)l)),
      multiple = F,
      graphics = T,
      title = "Choose the Filter to Apply"
    )
    if (this.filter.name == "" | this.filter.name == "All Done") {
      break
    } else {
      filtIndex = match(this.filter.name, lapply(names(filters), function(l)l))
      checkApply = getChoice(filters[filtIndex])

      #if a filter was used, apply it and then remove it
      if (nrow(checkApply[[2]]) > 0) {
        deBloat(checkApply)
        if (length(checkApply[[1]][[1]]$filt_field) == 1) {
          activity_log[[this.filter]][[checkApply[[1]][[1]]$filt_field]] = checkApply[2]
        }else{
          activity_log[[this.filter]][[paste(checkApply[[1]][[1]]$filt_field, collapse="_")]] = paste(checkApply[[2]],collapse="_")
        }
        assign("activity_log", activity_log, envir = .GlobalEnv)
        filters[filtIndex] = NULL
      }
    }
  }
  #return(activity_log)
}
