
#' Global configuration for encrypted schemas
#' @keywords internal
encrypted_schemas <- c("MARFISSCI", "COMLAND", "MFD_STOMACH", "ISDB")

# Global configuration for DB synonyms
#' @keywords internal
db_synonyms <- c(
  "marfissci" = "marfis",
  "observer"  = "isdb"
  # Add more as needed
)

# Normalize DB name using synonyms
#' @keywords internal
normalize_db_key <- function(db) {
  db_lower <- tolower(db)
  if (db_lower %in% names(db_synonyms)) {
    return(db_synonyms[[db_lower]])
  }
  return(db_lower)
}

# Get schema for a given DB
#' @keywords internal
get_schema <- function(db) {
  schema <- get_ds_all()[[db]]$schema
  if (is.na(schema)) return(NULL)
  return(toupper(schema))
}

# Check which required tables are missing locally
#' @keywords internal
local_table_status_check <- function(db) {
  reqdTables <- toupper(get_ds_all()[[db]]$tables)
  schema <- get_schema(db)
  
  local_files <- list.files(
    path = get_pesd_dw_dir(),
    pattern = "\\.rdata$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  locTables.clean <- if (!is.null(schema)) {
    these <- local_files[grepl(paste0("^", schema, "\\."), basename(local_files), ignore.case = TRUE)]
    these <- tools::file_path_sans_ext(basename(these))
    gsub("^.*?\\.", "", these) |> toupper()
  } else if (toupper(db) == "ISDB") {
    these <- local_files[grepl("^[^\\.]+\\.rdata$", basename(local_files), ignore.case = TRUE)]
    tools::file_path_sans_ext(basename(these)) |> toupper()
  } else {
    character(0)
  }
  
  missingTables <- sort(setdiff(reqdTables, unique(locTables.clean)))
  return(missingTables)
}

# Verify access or extract data from Oracle
#' @keywords internal
oracle_activity <- function(tables, cxn, thecmd, schema, action = c("verify_access", "extract"), quiet = FALSE) {
  action <- match.arg(action)
  strip_schema <- function(x) gsub(".*\\.", "", x)
  schema_prefix <- if (!is.null(schema)) paste0(schema, ".") else ""
  table_naked <- strip_schema(tables)
  
  if (action == "verify_access") {
    if (!quiet) cat(paste0("\nVerifying access to ", tables, " ..."))
    qry <- paste0("SELECT '1' FROM ", schema_prefix, table_naked, " WHERE ROWNUM <= 1")
    res <- thecmd(cxn, qry, rows_at_time = 1)
    if (is.character(res)) {
      if (!quiet) cat(" failed")
      return(FALSE)
    } else {
      if (!quiet) cat(" success")
      return(TRUE)
    }
  } else if (action == "extract") {
    if (!quiet) cat(paste0("\nExtracting ", tables, "... "))
    add.where <- "1=1"
    err_tables <- get_ds_all()[[.GlobalEnv$db]]$table_err_roracle
    if (tables %in% names(err_tables)) {
      this <- err_tables[[tables]]
      badvalues <- paste(this$badvalues, collapse = ",")
      add.where <- paste0(this$field, " NOT IN (", badvalues, ")")
      if (!quiet) {
        cat(paste0("\n\tSkipping records ", schema, ".", tables, ".", this$field,
                   " IN (", badvalues, ")\n\n\tReason: ", this$comments,
                   "\n\tIf critical, use RODBC instead of ROracle\n"))
      }
    }
    qry <- paste0("SELECT * FROM ", schema_prefix, table_naked, " WHERE ", add.where)
    res <- thecmd(cxn, qry, rows_at_time = 1)
    assign(table_naked, res, envir = .GlobalEnv)

    # Normalize filename to uppercase
    save_path <- file.path(get_pesd_dw_dir(), paste0(toupper(tables), ".RData"))
    if (is.null(schema) || schema %in% encrypted_schemas) {
      Mar.utils::save_encrypted(list = table_naked, file = save_path, envir = .GlobalEnv)
    } else {
      save(list = table_naked, file = save_path, envir = .GlobalEnv)
    }
    if (!quiet) cat(paste("Got", tables))
  }
}

# Extract all or missing tables
#' @keywords internal
try_extract <- function(cxn, tables, quiet = FALSE, extract_user = NULL, extract_computer = NULL) {
  thecmd <- Mar.utils::connectionCheck(cxn)
  schema <- get_schema(.GlobalEnv$db)
  db_name <- toupper(.GlobalEnv$db)
  
  if (!quiet) cat("\nVerifying access to required tables...")
  verified <- sapply(tables, oracle_activity, cxn, thecmd, schema, "verify_access", quiet)
  if (!all(verified)) {
    stop("You do not have access to all required tables.\nPlease ask the DB custodian to grant access and try again.")
  }
  
  if (!dir.exists(get_pesd_dw_dir())) {
    dir.create(get_pesd_dw_dir(), recursive = TRUE, showWarnings = FALSE)
  }
  
  if (!quiet) cat("\n\nStarting extractions... ")
  timer.start <- proc.time()
  sapply(tables, oracle_activity, cxn, thecmd, schema, "extract", quiet)
  elapsed <- proc.time() - timer.start
  if (!quiet) cat(paste("\n\nExtraction completed in", round(elapsed[3], 0), "seconds"))
  
  data_tweaks2(db = .GlobalEnv$db, extract_user = extract_user, extract_computer = extract_computer)
}

# Load tables from local files
#' @keywords internal
try_load <- function(tables, thisenv, quiet = FALSE, extract_user = NULL, extract_computer = NULL) {
  loadit <- function(x, extract_user, extract_computer) {
    file_name <- paste0(toupper(x), ".RData")
    file_path <- file.path(get_pesd_dw_dir(), file_name)
    
    if (.GlobalEnv$db == "isdb" && !file.exists(file_path)) {
      file_path <- gsub("ISDB\\.", "", file_path)
      x <- sub("\\.[^.]*$", "", basename(file_path))
    }
    
    Mar.utils::load_encrypted(file = file_path, envir = thisenv,
                              extract_user = extract_user, extract_computer = extract_computer)
    
    if (!quiet) cat(paste0("\nLoaded ", x, "... "))
    file_age_days <- round(difftime(Sys.time(), file.info(file_path)$mtime, units = "days"), 0)
    if (!quiet) {
      cat(paste0(" (Data modified ", file_age_days, " days ago.)"))
      if (file_age_days > 90) {
        cat("\n!!! This data was extracted more than 90 days ago - consider re-extracting it")
      }
    }
  }
  
  if (!quiet) cat("\nLoading data...")
  timer.start <- proc.time()
  invisible(lapply(tables, loadit, extract_user, extract_computer ))
  elapsed <- proc.time() - timer.start
  if (!quiet) cat(paste0("\n\n", round(elapsed[3], 0), " seconds to load..."))
}

