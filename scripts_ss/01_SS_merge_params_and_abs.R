##==============================================================================
## Project: QuEST
## Script to merge parameters and fingerprint s::can data
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##
## This is the NORMAL pipeline step: it reads the per-site *_params.csv and
## *_abs.csv files that 00_SS_merge_timestamps.R produces (from the "merged"
## Drive folder), joins them on DateTime, and uploads *_absparams.csv.
## Run this one after 00_SS_merge_timestamps.R.
##
## NOTE: "01_SS_start with manual merge.R" looks similar but is NOT a
## duplicate of this script -- it's a separate path for a manually-assembled,
## whole-site-history Excel from a different Drive folder ("manual merge"),
## used to bypass the 00 script entirely (e.g. for historical backfill or
## recovery). If that manual-merge workflow is no longer used, that script
## can be retired; if unsure, leave both and use this one for the regular run.
##==============================================================================

library(googledrive)
library(dplyr)
library(openxlsx)

##==============================================================================
## SSM01
##==============================================================================
#######################################
#### Import abs and parameter data ####
#######################################
# load data from Google drive, this is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1qpsqrmcnALNS9OVtoIDICdEuW5LkVuIR")
# list all CSV files in the folder
scan_csvs <- googledrive::drive_ls(path = scan)

# load only SSM01 files
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="SSM01_params.csv"], 
                            path = "googledrive/SSM01_params.csv",
                            overwrite = T)
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="SSM01_abs.csv"], 
                            path = "googledrive/SSM01_abs.csv",
                            overwrite = T)

SSM01_params <- read.csv("googledrive/SSM01_params.csv")
SSM01_abs <- read.csv("googledrive/SSM01_abs.csv")

#############################
#### Tidy both data sets ####
#############################
# change datetime format
SSM01_params <- SSM01_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
SSM01_abs <- SSM01_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

#################################
#### Merge parameter and abs ####
#################################
# param data first
SSM01_merged <- left_join(SSM01_params, SSM01_abs, by = "DateTime")

#########################################
#### Save merged SSM01 file to Drive ####
#########################################
# make sure it is in datetime format
SSM01_merged$DateTime <- format(SSM01_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(SSM01_merged,"data/SSM01_absparams.csv" , row.names=FALSE, quote=FALSE)

# define the target folder ID in Google Drive
# this is the "merged" folder
drive_folder_id <- "1WbfZWpSeXVLoSEvxqbVnjgvgo4uUwGtm"

# upload the file to the specified Google Drive folder
drive_upload(media = "data/SSM01_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## SSM01 -- raw (uncompensated) fingerprint version, NEW (Sep 25)
## Same merge as above, but against the sheet-3 raw fingerprint
## (SSM01_abs_raw.csv, produced by 00_SS_merge_timestamps.R) instead of the
## compensated one. Written to a separate file/name throughout (_raw suffix)
## so it never overwrites or gets confused with the normal compensated
## pipeline output above -- this is purely for comparing the two per
## Ariel's suggestion, not a replacement.
##==============================================================================
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="SSM01_abs_raw.csv"],
                            path = "googledrive/SSM01_abs_raw.csv",
                            overwrite = T)

SSM01_abs_raw <- read.csv("googledrive/SSM01_abs_raw.csv")
SSM01_abs_raw <- SSM01_abs_raw %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

SSM01_merged_raw <- left_join(SSM01_params, SSM01_abs_raw, by = "DateTime")
SSM01_merged_raw$DateTime <- format(SSM01_merged_raw$DateTime, "%Y-%m-%d %H:%M:%S")
write.csv(SSM01_merged_raw, "data/SSM01_absparams_raw.csv", row.names = FALSE, quote = FALSE)

drive_upload(media = "data/SSM01_absparams_raw.csv", path = as_id(drive_folder_id))


##==============================================================================
## SSM20
##==============================================================================
#######################################
#### Import abs and parameter data ####
#######################################
# load only SSM01 files
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="SSM20_params.csv"], 
                            path = "googledrive/SSM20_params.csv",
                            overwrite = T)

googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="SSM20_abs.csv"], 
                            path = "googledrive/SSM20_abs.csv",
                            overwrite = T)

SSM20_params <- read.csv("googledrive/SSM20_params.csv")
SSM20_abs <- read.csv("googledrive/SSM20_abs.csv")

#############################
#### Tidy both data sets ####
#############################
# change datetime format
SSM20_params <- SSM20_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
SSM20_abs <- SSM20_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

#################################
#### Merge parameter and abs ####
#################################
# param data first
SSM20_merged <- left_join(SSM20_params, SSM20_abs, by = "DateTime")

#########################################
#### Save merged SSM20 file to Drive ####
#########################################
# make sure it is in datetime format
SSM20_merged$DateTime <- format(SSM20_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(SSM20_merged,"data/SSM20_absparams.csv" , row.names=FALSE, quote=FALSE)

# define the target folder ID in Google Drive
# this is the "merged" folder
drive_folder_id <- "1WbfZWpSeXVLoSEvxqbVnjgvgo4uUwGtm"

# upload the file to the specified Google Drive folder
drive_upload(media = "data/SSM20_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## SSM20 -- raw (uncompensated) fingerprint version, NEW (Sep 25)
## Same merge as above, but against the sheet-3 raw fingerprint
## (SSM20_abs_raw.csv, produced by 00_SS_merge_timestamps.R) instead of the
## compensated one. Written to a separate file/name throughout (_raw suffix)
## so it never overwrites or gets confused with the normal compensated
## pipeline output above -- this is purely for comparing the two per
## Ariel's suggestion, not a replacement.
##==============================================================================
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="SSM20_abs_raw.csv"],
                            path = "googledrive/SSM20_abs_raw.csv",
                            overwrite = T)

SSM20_abs_raw <- read.csv("googledrive/SSM20_abs_raw.csv")
SSM20_abs_raw <- SSM20_abs_raw %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

SSM20_merged_raw <- left_join(SSM20_params, SSM20_abs_raw, by = "DateTime")
SSM20_merged_raw$DateTime <- format(SSM20_merged_raw$DateTime, "%Y-%m-%d %H:%M:%S")
write.csv(SSM20_merged_raw, "data/SSM20_absparams_raw.csv", row.names = FALSE, quote = FALSE)

drive_upload(media = "data/SSM20_absparams_raw.csv", path = as_id(drive_folder_id))


##==============================================================================
## SST13
##==============================================================================
#######################################
#### Import abs and parameter data ####
#######################################
# load only SST13 files
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="SST13_params.csv"], 
                            path = "googledrive/SST13_params.csv",
                            overwrite = T)
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="SST13_abs.csv"], 
                            path = "googledrive/SST13_abs.csv",
                            overwrite = T)

SST13_params <- read.csv("googledrive/SST13_params.csv")
SST13_abs <- read.csv("googledrive/SST13_abs.csv")

#############################
#### Tidy both data sets ####
#############################
# change datetime format
SST13_params <- SST13_params %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
SST13_abs <- SST13_abs %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

#################################
#### Merge parameter and abs ####
#################################
# param data first
SST13_merged <- left_join(SST13_params, SST13_abs, by = "DateTime")

########################################s#
#### Save merged SST13 file to Drive ####
#########################################
# make sure it is in datetime format
SST13_merged$DateTime <- format(SST13_merged$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(SST13_merged,"data/SST13_absparams.csv" , row.names=FALSE, quote=FALSE)

# define the target folder ID in Google Drive
# this is the "merged" folder
drive_folder_id <- "1WbfZWpSeXVLoSEvxqbVnjgvgo4uUwGtm"

# upload the file to the specified Google Drive folder
drive_upload(media = "data/SST13_absparams.csv", path = as_id(drive_folder_id))

##==============================================================================
## SST13 -- raw (uncompensated) fingerprint version, NEW (Sep 25)
## Same merge as above, but against the sheet-3 raw fingerprint
## (SST13_abs_raw.csv, produced by 00_SS_merge_timestamps.R) instead of the
## compensated one. Written to a separate file/name throughout (_raw suffix)
## so it never overwrites or gets confused with the normal compensated
## pipeline output above -- this is purely for comparing the two per
## Ariel's suggestion, not a replacement.
##==============================================================================
googledrive::drive_download(file = scan_csvs$id[scan_csvs$name=="SST13_abs_raw.csv"],
                            path = "googledrive/SST13_abs_raw.csv",
                            overwrite = T)

SST13_abs_raw <- read.csv("googledrive/SST13_abs_raw.csv")
SST13_abs_raw <- SST13_abs_raw %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

SST13_merged_raw <- left_join(SST13_params, SST13_abs_raw, by = "DateTime")
SST13_merged_raw$DateTime <- format(SST13_merged_raw$DateTime, "%Y-%m-%d %H:%M:%S")
write.csv(SST13_merged_raw, "data/SST13_absparams_raw.csv", row.names = FALSE, quote = FALSE)

drive_upload(media = "data/SST13_absparams_raw.csv", path = as_id(drive_folder_id))


