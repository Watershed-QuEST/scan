##==============================================================================
## Project: QuEST
## Script to merge scan files in one (using timestamp)
##
## THIS IS NOT THE NORMAL PIPELINE STEP. It reads a manually-assembled,
## whole-site-history Excel (SSM01_merged.xlsx, etc, 2 sheets) from a
## separate "manual merge" Drive folder, and does the sheet-reading +
## tidying + params/abs join all in one script, bypassing
## 00_SS_merge_timestamps.R entirely.
##
## For the regular pipeline (raw per-deployment files -> 00_SS_merge_timestamps.R
## -> 01_SS_merge_params_and_abs.R), use those two scripts instead. Only run
## this one if you specifically need to (re)process the manual-merge source
## data -- e.g. historical backfill or recovering from missing raw files.
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
#### list and download all files in the folder ####
# this is the "manual merge" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1Ih7yibvk2KnD6i70s1ejSrZdgBnn6I9s")
# list all CSV files in the folder
scan_csvs <- googledrive::drive_ls(path = scan)
3

# create empty list to store data frames
scan_list_param <- list()

# loop over each file in the `scan_csvs` data frame
for (i in seq_along(scan_csvs$id)) {
  # define the local file path
  local_path <- file.path("googledrive", scan_csvs$name[i])
  
  # download the file
  googledrive::drive_download(
    file = scan_csvs$id[i],
    path = local_path,
    overwrite = TRUE
  )
  
  # read the header row (row 2)
  header <- read_excel(local_path, skip = 1, n_max = 1, col_names = FALSE)
  # convert the header to a character vector and clean empty names
  col_names <- as.character(unlist(header[1, ]))
  col_names[col_names == ""] <- paste0("X", seq_along(col_names[col_names == ""]))
  
  # read the data starting from row 4 using the header as column names
  data <- read_excel(local_path, skip = 4, col_names = col_names)
  
  # store the data in the list
  scan_list_param[[scan_csvs$name[i]]] <- data
}

##==============================================================================
## now we need to do the same thing but for the compensated abs tab
## abs file is in the same excel in second tab of file
##==============================================================================
##########################
#### Import scan data #### 
##########################
# create empty list to store data frames
scan_list <- list()

for (i in seq_along(scan_csvs$id)) {
  local_path <- file.path("googledrive", scan_csvs$name[i])
  
  googledrive::drive_download(
    file = scan_csvs$id[i],
    path = local_path,
    overwrite = TRUE
  )
  
  # 1. Extract the Header Names from Row 2
  # We read just one row. col_names = FALSE so R doesn't use Row 1.
  header_row <- read_excel(local_path, sheet = 2, skip = 1, n_max = 1, col_names = FALSE)
  header_names <- as.character(unlist(header_row))
  
  # 2. Fix the first column name (it's empty in Row 2)
  header_names[1] <- "DateTime"
  
  # 3. Read the Actual Data (Starts at Row 5)
  # skip = 4 bypasses the first 4 rows of junk/metadata
  data <- read_excel(local_path, sheet = 2, skip = 4, col_names = FALSE)
  
  # 4. Assign the names we extracted to the data
  # This ensures the 223 columns match perfectly
  colnames(data) <- header_names
  
  # 5. Clean up names for R (converts "200.00 nm" to "X200.00.nm" to be safe)
  colnames(data) <- make.names(colnames(data), unique = TRUE)
  
  # 6. Convert to numeric (handling the DateTime separately)
  data <- data %>%
    mutate(across(-1, ~as.numeric(as.character(.)))) %>% # Everything except 1st column
    mutate(DateTime = as.POSIXct(DateTime))             # Ensure 1st column is Time
  
  # Store in list
  scan_list[[scan_csvs$name[i]]] <- data
  
  message(paste("Successfully processed:", scan_csvs$name[i]))
}

#######################################
#### Extract each individual file  ####
#######################################
SSM01_params <- scan_list_param[["SSM01_merged.xlsx"]]
SSM20_params <- scan_list_param[["SSM20_merged.xlsx"]]
SST13_params <- scan_list_param[["SST13_merged.xlsx"]]

SSM01_abs <- scan_list[["SSM01_merged.xlsx"]]
SSM20_abs <- scan_list[["SSM20_merged.xlsx"]]
SST13_abs <- scan_list[["SST13_merged.xlsx"]]

#############################
#### Tidy both data sets ####
#############################
# Fix SSM01
SSM01_params <- SSM01_params %>%
  rename(DateTime = `Parameter:`) %>%  # Use backticks because of the colon
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

# Fix SSM20
SSM20_params <- SSM20_params %>%
  rename(DateTime = `Parameter:`) %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

# Fix SST13
SST13_params <- SST13_params %>%
  rename(DateTime = `Parameter:`) %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

# change datetime format
SSM01_abs <- SSM01_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

SSM20_abs <- SSM20_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

SST13_abs <- SST13_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

#################################
#### Merge parameter and abs ####
#################################
# param data first
SSM01_merged <- left_join(SSM01_params, SSM01_abs, by = "DateTime")
SSM20_merged <- left_join(SSM20_params, SSM20_abs, by = "DateTime")
SST13_merged <- left_join(SST13_params, SST13_abs, by = "DateTime")

# make sure it is in datetime format
SSM01_merged$DateTime <- format(SSM01_merged$DateTime, "%Y-%m-%d %H:%M:%S")
SSM20_merged$DateTime <- format(SSM20_merged$DateTime, "%Y-%m-%d %H:%M:%S")
SST13_merged$DateTime <- format(SST13_merged$DateTime, "%Y-%m-%d %H:%M:%S")

####################################
#### Save merged files to Drive ####
####################################
# save the new data frame to a CSV file
write.csv(SSM01_merged,"data/SSM01_absparams.csv" , row.names=FALSE, quote=FALSE)
write.csv(SSM20_merged,"data/SSM20_absparams.csv" , row.names=FALSE, quote=FALSE)
write.csv(SST13_merged,"data/SST13_absparams.csv" , row.names=FALSE, quote=FALSE)

# define the target folder ID in Google Drive
# this is the "params and abs" folder
drive_folder_id <- "1WbfZWpSeXVLoSEvxqbVnjgvgo4uUwGtm"

# upload the file to the specified Google Drive folder
drive_put(media = "data/SSM01_absparams.csv", path = as_id(drive_folder_id))
drive_put(media = "data/SSM20_absparams.csv", path = as_id(drive_folder_id))
drive_put(media = "data/SST13_absparams.csv", path = as_id(drive_folder_id))
