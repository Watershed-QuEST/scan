##==============================================================================
## Project: QuEST
## Script to merge scan files in one (using timestamp)
##==============================================================================

library(readxl) #to read excel 
library(googledrive)
library(dplyr)

########################################
#### Clear folders that we will use ####
########################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

##########################
#### Import scan data ####
##########################
#### list, download once, and read BOTH sheets from that same local file ####
# this is the "raw" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1x6tPgXn-DgmBVvFTMG0TEo2AxqHqQLwV")
# list all CSV files in the folder
scan_csvs <- googledrive::drive_ls(path = scan)

# Retry wrapper so one stalled/flaky transfer doesn't kill the whole loop.
# Google Drive downloads occasionally hang (curl "Operation too slow" after
# ~10 min of near-zero throughput); this retries a few times with a short
# pause before giving up on that one file and moving on.
download_with_retry <- function(file, path, max_tries = 3, wait_sec = 15) {
  for (attempt in seq_len(max_tries)) {
    ok <- tryCatch({
      googledrive::drive_download(file = file, path = path, overwrite = TRUE)
      TRUE
    }, error = function(e) {
      message(sprintf("  attempt %d/%d failed for %s: %s", attempt, max_tries, path, conditionMessage(e)))
      FALSE
    })
    if (isTRUE(ok)) return(TRUE)
    if (attempt < max_tries) Sys.sleep(wait_sec)
  }
  FALSE
}

# Each xlsx has the params on sheet 1, the compensated fingerprint (abs) on
# sheet 2, and the raw (uncompensated) fingerprint on sheet 3 -- download it
# ONCE per file and read all three sheets from the same local copy.
#
# CHANGED (Sep 28), two fixes:
# 1. Downloads are the slow part (network-bound, one file at a time used to
#    mean waiting on ~N sequential Drive round-trips). Split into two
#    phases: download all files in parallel first (parallel::mclapply --
#    base R, no extra package needed, fork-based so Mac/Linux only, which
#    is what this runs on), THEN read them sequentially from local disk
#    (fast, no network involved). Downloading is embarrassingly parallel
#    across files, so this should meaningfully cut wall-clock time.
# 2. Some raw-fingerprint (sheet 3) files don't have a "Measured status"
#    column at all -- the old `!c(DateTime, 'Measured status')` selector
#    requires that column to exist and errors out ("Column `Measured
#    status` doesn't exist"), which stopped the ENTIRE loop on the first
#    file missing it. Switched to `!any_of(c("DateTime", "Measured
#    status"))` -- any_of() is a tidyselect helper that silently ignores
#    names that aren't present, instead of erroring. Also wrapped each
#    file's whole read/parse step in tryCatch so any other per-file error
#    (a malformed sheet, an unexpected layout, etc.) skips just that file
#    -- logged to failed_reads and reported at the end -- instead of
#    halting the whole run.
n_workers <- max(1, parallel::detectCores() - 1)
message(sprintf("Downloading %d files (%d in parallel)...", nrow(scan_csvs), n_workers))

download_ok <- unlist(parallel::mclapply(seq_along(scan_csvs$id), function(i) {
  local_path <- file.path("googledrive", scan_csvs$name[i])
  download_with_retry(file = scan_csvs$id[i], path = local_path, max_tries = 3, wait_sec = 15)
}, mc.cores = n_workers))

failed_downloads <- scan_csvs$name[!download_ok]
if (length(failed_downloads) > 0) {
  warning("The following files failed to download after retries and were skipped:\n",
          paste(" -", failed_downloads, collapse = "\n"))
}

scan_list_param <- list()  # sheet 1: params
scan_list       <- list()  # sheet 2: compensated fingerprint / abs
scan_list_raw   <- list()  # sheet 3: raw (uncompensated) fingerprint
failed_reads    <- character(0)

# loop over each SUCCESSFULLY DOWNLOADED file and read it (local disk, fast)
for (i in seq_along(scan_csvs$id)) {
  if (!download_ok[i]) next
  local_path <- file.path("googledrive", scan_csvs$name[i])

  result <- tryCatch({
    #### sheet 1: params ####
    header_param <- read_excel(local_path, skip = 1, n_max = 1, col_names = FALSE)
    col_names_param <- as.character(unlist(header_param[1, ]))
    col_names_param[col_names_param == ""] <- paste0("X", seq_along(col_names_param[col_names_param == ""]))
    data_param <- read_excel(local_path, skip = 4, col_names = col_names_param)

    #### sheet 2: compensated fingerprint / abs ####
    header_abs <- read_excel(local_path, sheet = 2, skip = 1, n_max = 1, col_names = FALSE)
    col_names_abs <- as.character(unlist(header_abs[1, ]))
    col_names_abs[col_names_abs == ""] <- paste0("X", seq_along(col_names_abs[col_names_abs == ""]))
    data_abs <- read_excel(local_path, sheet = 2, skip = 4, col_names = col_names_abs)
    colnames(data_abs)[1] <- "DateTime"
    data_abs <- data_abs %>%
      mutate(across(!any_of(c("DateTime", "Measured status")), as.numeric))

    #### sheet 3: raw (uncompensated) fingerprint ####
    header_abs_raw <- read_excel(local_path, sheet = 3, skip = 1, n_max = 1, col_names = FALSE)
    col_names_abs_raw <- as.character(unlist(header_abs_raw[1, ]))
    col_names_abs_raw[col_names_abs_raw == ""] <- paste0("X", seq_along(col_names_abs_raw[col_names_abs_raw == ""]))
    data_abs_raw <- read_excel(local_path, sheet = 3, skip = 4, col_names = col_names_abs_raw)
    colnames(data_abs_raw)[1] <- "DateTime"
    data_abs_raw <- data_abs_raw %>%
      mutate(across(!any_of(c("DateTime", "Measured status")), as.numeric))

    list(param = data_param, abs = data_abs, abs_raw = data_abs_raw)
  }, error = function(e) {
    message(sprintf("  failed to read/parse %s: %s", scan_csvs$name[i], conditionMessage(e)))
    NULL
  })

  if (is.null(result)) {
    failed_reads <- c(failed_reads, scan_csvs$name[i])
    next
  }

  scan_list_param[[scan_csvs$name[i]]] <- result$param
  scan_list[[scan_csvs$name[i]]]       <- result$abs
  scan_list_raw[[scan_csvs$name[i]]]   <- result$abs_raw
}

if (length(failed_reads) > 0) {
  warning("The following files downloaded but failed to read/parse and were skipped:\n",
          paste(" -", failed_reads, collapse = "\n"))
}

####################################
#### Combine data for each site ####
####################################
# loop through each data frame in the list to change DateTime column name
for (i in seq_along(scan_list_param)) {
  # Access the current data frame
  df <- scan_list_param[[i]]
  
  # change names for easier handling
  colnames(df)[1] ="DateTime"
  
  # update the data frame in the list
  scan_list_param[[i]] <- df
}

# site names
site_names <- c("SSM01", "SSM20", "SST13")

# group files in `scan_list_param` by matching `site_names` in file names

scan_list_by_site <- lapply(site_names, function(site) {
  # names(scan_list_param) gives the names of all files in scan_list_param
  site_files <- names(scan_list_param)[grepl(site, names(scan_list_param))] 
  # grep checks if the current site (e.g., SSM01) appears in each file name in scan_list_param. 
  # This returns a logical vector (TRUE for matches, FALSE otherwise).
  scan_list_param[site_files] # select only the files for this site
  # The [ ] indexing selects only the file names where the match is TRUE.
})

# name the list by site
names(scan_list_by_site) <- site_names

# combine data for each site
combined_by_site <- lapply(scan_list_by_site, function(site_data_list) {
  # bind rows of all data frames for the site
  bind_rows(site_data_list) %>%
    arrange(DateTime) %>%  # chronological order if 'DateTime' exists
    distinct(DateTime, .keep_all = TRUE) # remove duplicates
})

##############################
#### Save combined files  ####
##############################
# ensure DateTime column is properly formatted
combined_by_site <- lapply(combined_by_site, function(df) {
  df$DateTime <- format(df$DateTime, "%Y-%m-%d %H:%M:%S") 
  return(df)
})

lapply(names(combined_by_site), function(site) {
  # row.names = FALSE to match the _abs.csv write below -- without it, R
  # writes an extra unnamed leading row-index column, which read.csv() on
  # the other end turns into a stray "X" column (already dropped as junk
  # in 02_SS_clean.R, but there's no reason to write it in the first place).
  write.csv(combined_by_site[[site]], file.path("data", paste0(site, "_params.csv")), row.names = FALSE)
})
  
lapply(names(combined_by_site), function(site) {
  file <- paste0("data/", site, "_params.csv")
  # this is the time stamps folder
  drive_folder_id <- "1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR"
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})
  
##==============================================================================
## now we combine the compensated abs tab (sheet 2 of the same excel files)
## -- already read into `scan_list` in the combined download loop above, so
## there's nothing left to download or read here.
##==============================================================================

####################################
#### Combine data for each site ####
####################################
# site names
site_names <- c("SSM01", "SSM20", "SST13")

# group files in `scan_list` by matching `site_names` in file names
scan_list_by_site <- lapply(site_names, function(site) {
  # names(scan_list) gives the names of all files in scan_list.
  site_files <- names(scan_list)[grepl(site, names(scan_list))] 
  # grep checks if the current site (e.g., SSM01) appears in each file name in scan_list. 
  scan_list[site_files] # select only the files for this site
})

# name the list by site
names(scan_list_by_site) <- site_names

# combine data for each site
combined_by_site <- lapply(scan_list_by_site, function(site_data_list) {
  
  # --- NEW: Check and standardize columns before binding ---
  
  # 1. Get the union of all column names across all files for this site
  all_names <- unique(unlist(lapply(site_data_list, names)))
  
  # 2. Iterate through each data frame and align its columns
  site_data_list_aligned <- lapply(site_data_list, function(df) {
    # Identify columns that are missing in the current dataframe
    missing_cols <- setdiff(all_names, names(df))
    
    # Add missing columns filled with NA
    for (col in missing_cols) {
      # Use `NA_real_` to ensure new columns are added as numeric (double) type, 
      # matching the expected type of spectral data.
      df[[col]] <- NA_real_ 
    }
    
    # Select and reorder columns to match the 'canonical' order (DateTime first)
    df <- df[, all_names]
    
    return(df)
  })
  
  # --- END NEW: Alignment is complete ---
  
  # bind rows of all aligned data frames for the site
  bind_rows(site_data_list_aligned) %>% # Use the ALIGNED list
    arrange(DateTime) %>%  # ensure chronological order
    distinct(DateTime, .keep_all = TRUE) # remove duplicates
})

SSM20_EXAMPLE <- scan_list$"2024-09-06_SSM20_SN24160203.xlsx"
SSM20_combined <- combined_by_site$SSM20

###############
#### Clean ####
###############
combined_by_site <- lapply(scan_list_by_site, function(site_data_list) {
  
  # Step A: Label every row in every file before merging
  processed_files <- lapply(site_data_list, function(df) {
    df$DateTime <- as.POSIXct(df$DateTime)
    
    # DYNAMIC SELECTION: Find spectral columns for THIS specific data frame
    current_spec_cols <- grep("^[0-9]", colnames(df))
    
    # Calculate variance using the columns found in this specific file
    df$row_variance <- apply(df[, current_spec_cols, drop = FALSE], 1, sd, na.rm = TRUE)
    
    # Initial flagging
    df$temp_status <- ifelse(!is.na(df$row_variance) & df$row_variance > 0.001, "Good", "Corrupted")
    return(df)
  })
  
  # Step B: Bind and prioritize
  bind_rows(processed_files) %>%
    group_by(DateTime) %>%
    arrange(DateTime, desc(temp_status), desc(row_variance)) %>%
    mutate(
      Status = case_when(
        n() == 1 & first(temp_status) == "Good" ~ "Original_Good",
        n() > 1 & first(temp_status) == "Good" ~ "Replaced_Good",
        first(temp_status) == "Corrupted" ~ "Corrupted_No_Match",
        TRUE ~ "Original_Good"
      )
    ) %>%
    slice(1) %>% 
    ungroup() %>%
    select(-row_variance, -temp_status)
})

# Access your data
SSM01_clean <- combined_by_site[["SSM01"]]
SSM20_clean <- combined_by_site[["SSM20"]]
SST13_clean <- combined_by_site[["SST13"]]

# see how many "corrupted" rows were removed
nrow(bind_rows(scan_list_by_site[["SSM01"]])) - nrow(SSM01_clean)
nrow(bind_rows(scan_list_by_site[["SSM20"]])) - nrow(SSM20_clean)
nrow(bind_rows(scan_list_by_site[["SST13"]])) - nrow(SST13_clean)

##############################
#### Save combined files  ####
##############################
# ensure DateTime column is properly formatted
combined_by_site <- lapply(combined_by_site, function(df) {
  df$DateTime <- format(df$DateTime, "%Y-%m-%d %H:%M:%S") # ensure consistent format
  return(df)
})

lapply(names(combined_by_site), function(site) {
  write.csv(
    combined_by_site[[site]], 
    file.path("data", paste0(site, "_abs.csv")),
    row.names = FALSE # <--- THIS IS THE FIX TO AVOID REPEATED SPECTRAL VALUES
  )
})

lapply(names(combined_by_site), function(site) {
  file <- paste0("data/", site, "_abs.csv")
  # this is the time stamps folder
  drive_folder_id <- "1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR"
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})

##==============================================================================
## NEW (Sep 25): same combine/clean/save steps as above, but for the RAW
## (uncompensated) fingerprint from sheet 3 -- scan_list_raw, read alongside
## sheet 2 in the download loop above. This produces a parallel
## <site>_abs_raw.csv next to <site>_abs.csv so the raw version can be run
## through calibration and compared against the compensated one (Ariel's
## suggestion -- see script-unification-todo.md). Identical logic to the
## compensated block above, just renamed to _raw throughout so nothing here
## collides with or overwrites the compensated variables.
##==============================================================================

####################################
#### Combine data for each site (raw) ####
####################################
scan_list_by_site_raw <- lapply(site_names, function(site) {
  site_files <- names(scan_list_raw)[grepl(site, names(scan_list_raw))]
  scan_list_raw[site_files]
})
names(scan_list_by_site_raw) <- site_names

combined_by_site_raw <- lapply(scan_list_by_site_raw, function(site_data_list) {
  all_names <- unique(unlist(lapply(site_data_list, names)))

  site_data_list_aligned <- lapply(site_data_list, function(df) {
    missing_cols <- setdiff(all_names, names(df))
    for (col in missing_cols) {
      df[[col]] <- NA_real_
    }
    df <- df[, all_names]
    return(df)
  })

  bind_rows(site_data_list_aligned) %>%
    arrange(DateTime) %>%
    distinct(DateTime, .keep_all = TRUE)
})

###############
#### Clean (raw) ####
###############
combined_by_site_raw <- lapply(scan_list_by_site_raw, function(site_data_list) {

  processed_files <- lapply(site_data_list, function(df) {
    df$DateTime <- as.POSIXct(df$DateTime)
    current_spec_cols <- grep("^[0-9]", colnames(df))
    df$row_variance <- apply(df[, current_spec_cols, drop = FALSE], 1, sd, na.rm = TRUE)
    df$temp_status <- ifelse(!is.na(df$row_variance) & df$row_variance > 0.001, "Good", "Corrupted")
    return(df)
  })

  bind_rows(processed_files) %>%
    group_by(DateTime) %>%
    arrange(DateTime, desc(temp_status), desc(row_variance)) %>%
    mutate(
      Status = case_when(
        n() == 1 & first(temp_status) == "Good" ~ "Original_Good",
        n() > 1 & first(temp_status) == "Good" ~ "Replaced_Good",
        first(temp_status) == "Corrupted" ~ "Corrupted_No_Match",
        TRUE ~ "Original_Good"
      )
    ) %>%
    slice(1) %>%
    ungroup() %>%
    select(-row_variance, -temp_status)
})

SSM01_raw_clean <- combined_by_site_raw[["SSM01"]]
SSM20_raw_clean <- combined_by_site_raw[["SSM20"]]
SST13_raw_clean <- combined_by_site_raw[["SST13"]]

nrow(bind_rows(scan_list_by_site_raw[["SSM01"]])) - nrow(SSM01_raw_clean)
nrow(bind_rows(scan_list_by_site_raw[["SSM20"]])) - nrow(SSM20_raw_clean)
nrow(bind_rows(scan_list_by_site_raw[["SST13"]])) - nrow(SST13_raw_clean)

##############################
#### Save combined files (raw) ####
##############################
combined_by_site_raw <- lapply(combined_by_site_raw, function(df) {
  df$DateTime <- format(df$DateTime, "%Y-%m-%d %H:%M:%S")
  return(df)
})

lapply(names(combined_by_site_raw), function(site) {
  write.csv(
    combined_by_site_raw[[site]],
    file.path("data", paste0(site, "_abs_raw.csv")),
    row.names = FALSE
  )
})

lapply(names(combined_by_site_raw), function(site) {
  file <- paste0("data/", site, "_abs_raw.csv")
  drive_folder_id <- "1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR"
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})

# #### --------------------------------------------------------------------- ####
# #####################################################################
# ## 1. SETUP: LIST, DOWNLOAD, AND CLEAN INDIVIDUAL FILES
# #####################################################################
# # Define the Google Drive folder 
# drive_id <- as_id("1x6tPgXn-DgmBVvFTMG0TEo2AxqHqQLwV")
# # List all files in the folder
# scan_csvs <- drive_ls(path = drive_id)
# 
# # Create a directory to store downloaded files temporarily
# if (!dir.exists("googledrive_temp")) {
#   dir.create("googledrive_temp")
# }
# 
# # Create empty list to store processed data frames
# scan_list <- list()
# 
# # Loop over each file
# for (i in seq_along(scan_csvs$id)) {
#   file_name <- scan_csvs$name[i]
#   local_path <- file.path("googledrive_temp", file_name)
#   
#   # Download the file
#   drive_download(
#     file = scan_csvs$id[i],
#     path = local_path,
#     overwrite = TRUE,
#     verbose = FALSE
#   )
#   
#   # 1. Read the header row (Row 2, so skip 1)
#   header <- read_excel(local_path, sheet = 2, skip = 1, n_max = 1, col_names = FALSE)
#   
#   # Convert to a character vector and clean empty names
#   col_names <- as.character(unlist(header[1, ]))
#   col_names[col_names == ""] <- paste0("X_Unused_", seq_along(col_names[col_names == ""]))
#   
#   # Clean up column names to ensure consistency (e.g., "200.00 nm" -> "X200.00.nm")
#   col_names <- make.names(col_names, unique = TRUE)
#   
#   # 2. Define strict column types to prevent incorrect coercion
#   num_cols <- length(col_names)
#   
#   # Create a vector of column types: Col 1='DateTime' (date), Col 2='Measured Status' (text), 
#   # and all others (spectral data) as 'numeric'.
#   col_type_vector <- c(
#     'date',       # Column 1: Date/Time (readxl interprets Excel date formats)
#     'text',       # Column 2: Measured status (must be text)
#     rep('numeric', num_cols - 2) # Columns 3 to end: Spectral data (must be numeric)
#   )
#   
#   # 3. Read the data starting from row 5 (skip 4)
#   data <- read_excel(
#     local_path,
#     sheet = 2,
#     skip = 4,
#     col_names = col_names,
#     col_types = col_type_vector
#   )
#   
#   # 4. Final cleaning and standardization of essential column names
#   colnames(data)[1] <- "DateTime"
#   colnames(data)[2] <- "Measured_Status"
#   
#   # Store the clean data frame
#   scan_list[[file_name]] <- data
# }
# 
# # Clean up downloaded files (optional but recommended)
# # unlink("googledrive_temp", recursive = TRUE)
# 
# ## -------------------------------------------------------------------
# 
# ##############################################################
# #### 2. COMBINE DATA FRAMES BY SITE WITH ROBUST ALIGNMENT ####
# ##############################################################
# site_names <- c("SSM01", "SSM20", "SST13")
# 
# # Group files in `scan_list` by site name
# scan_list_by_site <- lapply(site_names, function(site) {
#   site_files <- names(scan_list)[grepl(site, names(scan_list))] 
#   scan_list[site_files]
# })
# names(scan_list_by_site) <- site_names
# 
# # Combine data for each site with explicit column alignment (to prevent data repetition)
# combined_by_site <- lapply(scan_list_by_site, function(site_data_list) {
#   
#   # 1. Find the canonical list of all column names across all files for this site
#   all_names <- unique(unlist(lapply(site_data_list, names)))
#   
#   # 2. Align columns of every data frame in the list
#   site_data_list_aligned <- lapply(site_data_list, function(df) {
#     missing_cols <- setdiff(all_names, names(df))
#     
#     # Add missing columns, filling with NA_real_ to preserve numeric type
#     for (col in missing_cols) {
#       df[[col]] <- NA_real_ 
#     }
#     
#     # Reorder columns to ensure consistent binding order
#     df <- df[, all_names]
#     
#     return(df)
#   })
#   
#   # 3. Bind rows and clean duplicates/sorting
#   bind_rows(site_data_list_aligned) %>%
#     arrange(DateTime) %>%
#     # Use distinct() to handle repeated timestamps: .keep_all = TRUE keeps the first entry found
#     distinct(DateTime, .keep_all = TRUE) 
# })
# 
# ## -------------------------------------------------------------------
# 
# #####################################################################
# ## 3. FINAL OUTPUT (OPTIONAL: SAVE TO CSV)
# #####################################################################
# # Example: Access the combined data for the SSM20 site
# # combined_data_ssm20 <- combined_by_site[["SSM20"]]
# 
# # Optional: Save the combined data frames to CSV files
# # Create output directory
# if (!dir.exists("data_combined")) {
#   dir.create("data_combined")
# }
# 
# lapply(names(combined_by_site), function(site) {
#   file_path <- file.path("data_combined", paste0(site, "_combined_abs.csv"))
#   write.csv(
#     combined_by_site[[site]], 
#     file_path,
#     # Crucially, write_csv defaults to row.names=FALSE and is generally cleaner than write.csv
#     na = "" # Specify how NAs should be written (as empty string)
#   )
#   message(paste("Saved combined data for", site, "to", file_path))
# })
