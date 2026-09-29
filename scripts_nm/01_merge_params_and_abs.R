##==============================================================================
## Project: QuEST
## Script to merge parameters and fingerprint s::can data
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive)
library(dplyr)
library(openxlsx)

##==============================================================================
## USF12
##==============================================================================

#######################################
#### Import abs and parameter data ####
#######################################
# this is the "merge_timestamps" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1-dUxVn1hBWy2MpHeIjVt-2QSujpVhijy")

# list all the files in the folder
scan_csvs <- googledrive::drive_ls(path = scan, type = "csv")

#USF12 parameters
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="USF12_params.csv"], 
                            path = "googledrive/USF12_params.csv",
                            overwrite = T)
#USF12 abs
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="USF12_abs.csv"], 
                            path = "googledrive/USF12_abs.csv",
                            overwrite = T)


# load fingerprints and parameter data
USF12_params <- read.csv("googledrive/USF12_params.csv")
USF12_abs <- read.csv("googledrive/USF12_abs.csv")

colSums(USF12_params == 0, na.rm = TRUE)

#############################
#### Tidy both data sets ####
#############################
# remove extra rows
USF12_params <- USF12_params[-c(1:9), ] 
#USF12_abs <- USF12_abs[-c(1:9), ] 

# change datetime format
USF12_params <- USF12_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
USF12_abs <- USF12_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

#################################
#### Merge parameter and abs ####
#################################
# param data first
USF12_merged <- left_join(USF12_params, USF12_abs, by = "DateTime")

##################
#### Clean up ####
##################
# Remove data before deployment
USF12_merged <- USF12_merged %>%
  filter(DateTime > "2024-05-07 13:00:00")

USF12_merged <- USF12_merged %>%
  select(-X.x, -X.y)

#########################################
#### Save merged USF12 file to Drive ####
#########################################
# make sure it is in datetime format
USF12_merged$DateTime <- format(USF12_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(USF12_merged,"googledrive/USF12_absparams.csv" , row.names=FALSE, quote=FALSE)

# define the target folder ID in Google Drive
# this is the "merged" folder
drive_folder_id <- "1hlc9U54d70T5-hml_F9RM8FAiUCVRFmp"

# upload the file to the specified Google Drive folder
drive_put(media = "googledrive/USF12_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## USF20
##==============================================================================
#######################################
#### Import abs and parameter data ####
#######################################
#USF20 parameters
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="USF20_params.csv"], 
                            path = "googledrive/USF20_params.csv",
                            overwrite = T)
#USF20 abs
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="USF20_abs.csv"], 
                            path = "googledrive/USF20_abs.csv",
                            overwrite = T)

# load fingerprints and parameter data
USF20_params <- read.csv("googledrive/USF20_params.csv")
USF20_abs <- read.csv("googledrive/USF20_abs.csv")

colSums(USF20_params == 0, na.rm = TRUE)

#############################
#### Tidy both data sets ####
#############################
# remove extra rows
USF20_params <- USF20_params[-c(1:85), ] 
USF20_abs <- USF20_abs[-c(1:45), ] 

# change datetime format
USF20_params <- USF20_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
USF20_abs <- USF20_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

#################################
#### Merge parameter and abs ####
#################################
# param data first
USF20_merged <- left_join(USF20_params, USF20_abs, by = "DateTime")

##################
#### Clean up ####
##################
# Remove data before deployment
USF20_merged <- USF20_merged %>%
  filter(DateTime > "2024-05-08 14:30:00") %>%
  filter(DateTime != "2024-11-13 13:04:32") %>%
  filter(!(DateTime >= "2025-03-28 12:15:00" & DateTime <= "2025-03-28 21:15:00"))

USF20_merged <- USF20_merged %>%
  select(-X.x, -X.y)

################################
#### Save merged USF20 file ####
################################
# make sure it is in datetime format
USF20_merged$DateTime <- format(USF20_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(USF20_merged,"googledrive/USF20_absparams.csv" , row.names=FALSE, quote=FALSE)

# define the target folder ID in Google Drive
# this is the "merged" folder
drive_folder_id <- "1hlc9U54d70T5-hml_F9RM8FAiUCVRFmp"

# upload the file to the specified Google Drive folder
drive_put(media = "googledrive/USF20_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## USF21
##==============================================================================
#######################################
#### Import abs and parameter data ####
#######################################
#USF21 parameters
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="USF21_params.csv"], 
                            path = "googledrive/USF21_params.csv",
                            overwrite = T)
#USF21 abs
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="USF21_abs.csv"], 
                            path = "googledrive/USF21_abs.csv",
                            overwrite = T)

# load fingerprints and parameter data
USF21_params <- read.csv("googledrive/USF21_params.csv")
USF21_abs <- read.csv("googledrive/USF21_abs.csv")

colSums(USF21_params == 0, na.rm = TRUE)

#############################
#### Tidy both data sets ####
#############################
# remove extra rows
USF21_params <- USF21_params[-c(1:28), ] 
USF21_abs <- USF21_abs[-c(1:28), ] 

# change datetime format
USF21_params <- USF21_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
USF21_abs <- USF21_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

#################################
#### Merge parameter and abs ####
#################################
# param data first
USF21_merged <- left_join(USF21_params, USF21_abs, by = "DateTime")

##################
#### Clean up ####
##################
# Remove data before deployment
USF21_merged <- USF21_merged %>%
  filter(DateTime > "2024-06-27 17:30:00") %>%
  filter(!(DateTime >= "2025-05-01 12:45:00" & DateTime <= "2025-05-18 12:45:00"))

USF21_merged <- USF21_merged %>%
  select(-X.x, -X.y)

################################
#### Save merged USF21 file ####
################################
# make sure it is in datetime format
USF21_merged$DateTime <- format(USF21_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(USF21_merged,"googledrive/USF21_absparams.csv" , row.names=FALSE, quote=FALSE)

# define the target folder ID in Google Drive
# this is the "merged" folder
drive_folder_id <- "1hlc9U54d70T5-hml_F9RM8FAiUCVRFmp"

# upload the file to the specified Google Drive folder
drive_put(media = "googledrive/USF21_absparams.csv", path = as_id(drive_folder_id))

