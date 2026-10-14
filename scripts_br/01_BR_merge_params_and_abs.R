##==============================================================================
## Project: QuEST
## Script to merge parameters and fingerprint s::can data for Brush Creek
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive)
library(dplyr)
library(openxlsx)

##==============================================================================
## BRMQ1
##==============================================================================

#######################################
#### Import abs and parameter data ####
#######################################
# this is the "merge_timestamps" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1ROxMHt-mzsMrym5Gisi2mNM9RaepuhZc")

# list all the files in the folder
scan_csvs <- googledrive::drive_ls(path = scan, type = "csv")

#BRMQ1 parameters
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="BRMQ1_params.csv"],
                            path = "googledrive/BRMQ1_params.csv",
                            overwrite = T)
#BRMQ1 abs
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="BRMQ1_abs.csv"],
                            path = "googledrive/BRMQ1_abs.csv",
                            overwrite = T)


# load fingerprints and parameter data
BRMQ1_params <- read.csv("googledrive/BRMQ1_params.csv")
BRMQ1_abs <- read.csv("googledrive/BRMQ1_abs.csv")

colSums(BRMQ1_params == 0, na.rm = TRUE)

#############################
#### Tidy both data sets ####
#############################
# remove extra rows
BRMQ1_params <- BRMQ1_params[-c(1:9), ]
#BRMQ1_abs <- BRMQ1_abs[-c(1:9), ]

# change datetime format
BRMQ1_params <- BRMQ1_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
BRMQ1_abs <- BRMQ1_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

#################################
#### Merge parameter and abs ####
#################################
# param data first
BRMQ1_merged <- left_join(BRMQ1_params, BRMQ1_abs, by = "DateTime")

#########################################
#### Save merged BRMQ1 file to Drive ####
#########################################
# make sure it is in datetime format
BRMQ1_merged$DateTime <- format(BRMQ1_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(BRMQ1_merged,"googledrive/BRMQ1_absparams.csv" , row.names=FALSE, quote=FALSE)

# define the target folder ID in Google Drive
# this is the "merged" folder
drive_folder_id <- "1pbRqXX5NqSgS4Roh0Ap9gsP9SicKX5-A"

# upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/BRMQ1_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## BRMQ4
##==============================================================================
#######################################
#### Import abs and parameter data ####
#######################################
#BRMQ4 parameters
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="BRMQ4_params.csv"], 
                            path = "googledrive/BRMQ4_params.csv",
                            overwrite = T)
#BRMQ4 abs
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="BRMQ4_abs.csv"], 
                            path = "googledrive/BRMQ4_abs.csv",
                            overwrite = T)

# load fingerprints and parameter data
BRMQ4_params <- read.csv("googledrive/BRMQ4_params.csv")
BRMQ4_abs <- read.csv("googledrive/BRMQ4_abs.csv")

colSums(BRMQ4_params == 0, na.rm = TRUE)

#############################
#### Tidy both data sets ####
#############################
# # remove extra rows
# BRMQ4_params <- BRMQ4_params[-c(1:85), ] 
# BRMQ4_abs <- BRMQ4_abs[-c(1:45), ] 

# change datetime format
BRMQ4_params <- BRMQ4_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
BRMQ4_abs <- BRMQ4_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

#################################
#### Merge parameter and abs ####
#################################
# param data first
BRMQ4_merged <- left_join(BRMQ4_params, BRMQ4_abs, by = "DateTime")

################################
#### Save merged BRMQ4 file ####
################################
# make sure it is in datetime format
BRMQ4_merged$DateTime <- format(BRMQ4_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(BRMQ4_merged,"googledrive/BRMQ4_absparams.csv" , row.names=FALSE, quote=FALSE)

# define the target folder ID in Google Drive
# this is the "merged" folder
drive_folder_id <- "1pbRqXX5NqSgS4Roh0Ap9gsP9SicKX5-A"

# upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/BRMQ4_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## BRM06
##==============================================================================
#######################################
#### Import abs and parameter data ####
#######################################
#BRM06 parameters
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="BRM06_params.csv"], 
                            path = "googledrive/BRM06_params.csv",
                            overwrite = T)
#BRM06 abs
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="BRM06_abs.csv"], 
                            path = "googledrive/BRM06_abs.csv",
                            overwrite = T)

# load fingerprints and parameter data
BRM06_params <- read.csv("googledrive/BRM06_params.csv")
BRM06_abs <- read.csv("googledrive/BRM06_abs.csv")

colSums(BRM06_params == 0, na.rm = TRUE)

#############################
#### Tidy both data sets ####
#############################
# # remove extra rows
# BRM06_params <- BRM06_params[-c(1:28), ] 
# BRM06_abs <- BRM06_abs[-c(1:28), ] 

# change datetime format
BRM06_params <- BRM06_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
BRM06_abs <- BRM06_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

#################################
#### Merge parameter and abs ####
#################################
# param data first
BRM06_merged <- left_join(BRM06_params, BRM06_abs, by = "DateTime")

################################
#### Save merged BRM06 file ####
################################
# make sure it is in datetime format
BRM06_merged$DateTime <- format(BRM06_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(BRM06_merged,"googledrive/BRM06_absparams.csv" , row.names=FALSE, quote=FALSE)

# define the target folder ID in Google Drive
# this is the "merged" folder
drive_folder_id <- "1pbRqXX5NqSgS4Roh0Ap9gsP9SicKX5-A"

# upload the file to the specified Google Drive folder
drive_upload(media = "googledrive/BRM06_absparams.csv", path = as_id(drive_folder_id))

