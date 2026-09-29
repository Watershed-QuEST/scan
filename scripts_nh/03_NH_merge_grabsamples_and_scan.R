##==============================================================================
## Project: QuEST
## Here we will prep grab sample data by matching the grab samples with the time stamp of the s::can for South Sandy
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive) 
library(googlesheets4)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(lubridate) 

########################################
#### Clear folders that we will use ####
########################################
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

##########################
#### Import chem data ####
##########################
# Chem data is for all the sites, this is the chem data folder
chem <- googledrive::as_id("https://drive.google.com/drive/folders/1ZCVAoIamyMMtwh-Cy3SpeQx2IWYu6gg2")

# List all CSV files in the folder
chem_csv <- googledrive::drive_ls(path = chem, type = "csv")
3

# call the specific file you want (go to link and check which one is the most recent one)
googledrive::drive_download(file = chem_csv$id[chem_csv$name=="2026-06-08_Laprey_chem_data.csv"], 
                            path = "googledrive/2026-06-08_Laprey_chem_data.csv",
                            overwrite = T)
# load it into R
wqual = read.csv("googledrive/2026-06-08_Laprey_chem_data.csv")

# Format date columns
wqual$Collection.Date <- as.Date(wqual$Collection.Date, format = "%Y-%m-%d")

# Rename Collection Date column
wqual <- wqual %>% rename(Date = Collection.Date)

# Clean up a bit
drops <- c("X", "Project", "Sub_ProjectA", "pH", "Cond", "Spec_Cond", "DO_Conc",  "DO.", "Temperature Turbidity", "ID")
wqual <- wqual[ , !(names(wqual) %in% drops)]

# # Filter to get just SS data
# NH <- filter(wqual, Sub_Project == "QuEST")

NH <- wqual

# Format time columns
NH$Time <- format(as.POSIXct(NH$Collection.Time, format = "%I:%M:%S %p"), "%H:%M:%S")

# Combine Date and Time columns into a new DateTime column
NH$DateTime <- paste(NH$Date, NH$Time, sep = " ")
# Convert the DateTime column to POSIXct
NH$DateTime <- as.POSIXct(NH$DateTime, format = "%Y-%m-%d %H:%M")

# Round DateTime to the nearest 15-minute interval 
NH$DateTime <- round_date(NH$DateTime, unit="15 mins")

#### There are no reps for NH so no need to merge here (reps and bottles) ####

#### Load sample info to get grab sample collection time ####
samplelogsheet <- drive_get("https://docs.google.com/spreadsheets/d/1n8OM50ziPBYJz4dYDLmBwyaOWTLGs_5G/edit?gid=1416523950#gid=1416523950")

# Download spreadsheet from Webster Lab Sample Log Sheet
drive_download(as_id(samplelogsheet$id), path = "googledrive/NH_synoptic.xlsx", overwrite = T)

# Fetch the file
samplelogsheet <- read.xlsx("googledrive/NH_synoptic.xlsx")

# Convert the numeric column to a proper time format
samplelogsheet$Collection_Time <- convertToDateTime(samplelogsheet$Collection_Time)
samplelogsheet$Collection_Date <- convertToDateTime(samplelogsheet$Collection_Date)


# Format date and time columns
samplelogsheet$Date <- as.Date(samplelogsheet$Collection_Date, format = "%Y-%m-%d")
samplelogsheet$Time <- format(as.POSIXct(samplelogsheet$Collection_Time, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
  
# Clean up a bit
drops <- c("DTWT", "Volume", "Dilution", "Start.Date/Time", "Lab_Notes", "Refrigerated_Received", "Frozen_Received", "BatchID", "Salinity", 
           "Sample_Type", "UNH#", "Project", "Sub_Project", "Sub_ProjectA", "Sub_ProjectB", "Field_Notes", "Logger")
samplelogsheet <- samplelogsheet[ , !(names(samplelogsheet) %in% drops)]

# #### Change sample time to fit scan time ####
# ###USF12###
# samplelogsheet$Time[samplelogsheet$Site == "USF12" &
#                       samplelogsheet$Date == "2024-05-23" &
#                       samplelogsheet$Time == "09:30:00"] <- "09:45:00"

####################################
#### Formatting collection time ####
####################################
# Combine Date and Time columns into a new DateTime column
samplelogsheet$DateTime <- paste(samplelogsheet$Date, samplelogsheet$Time, sep = " ")
# Convert the DateTime column to POSIXct
samplelogsheet$DateTime <- as.POSIXct(samplelogsheet$DateTime, format = "%Y-%m-%d %H:%M:%S")

#######################################################################
#### Merge chem and sample log sheet to get sample collection time ####
#######################################################################
# Replace all occurrences of "SMB" with "SBM" in the 'Sample.Name' column
NH$Sample.Name[NH$Sample.Name == "SBM"] <- "SMB"

# filter only data for "NHCTB", "NHLMP07", "NHLMP27", "NHLMP72", "NHSBM", "NHNCBd"  (scan sites) 
wqual_scans <- NH %>% filter(Sample.Name %in% c("CTB", "LMP07", "LMP27", "LMP72", "SMB", "NCB-down"))
samplelogsheet_scans <- samplelogsheet %>% filter(Sample_Name %in% c("CTB", "LMP07", "LMP27", "LMP72", "SMB", "NCB-down"))

wqual_scans <- wqual_scans %>% 
  rename(Sample_Name = Sample.Name)

### Merge wqual data first ###
sample_times <- merge(wqual_scans, samplelogsheet_scans, by = c("Date", "Sample_Name"), all = T)

sample_times <- sample_times %>%
  mutate(DateTime = coalesce(DateTime.y, DateTime.x))

# Clean up a bit
drops <- c("Date.y", "UNH.ID..", "Sub_Project", "Site", "Field.Notes", "Lab_Notes", "Temperature.y", "Turbidity.y", 
           "WL.(mm)", "DateTime.x", "DateTime.y", "pH", "Cond", "Spec_Cond", "DO_Conc", "DO%", "Atm_Pressure_mb",
           "Collection_Time", "Collection.Time")
sample_times <- sample_times[ , !(names(sample_times) %in% drops)]

# Round DateTime to the nearest 1 hour interval 
sample_times$DateTime <- round_date(sample_times$DateTime, unit="hour")

##########################
#### Import scan data ####
##########################
# This is the "abs and params" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1CeCmX0mGh1wZ3IL4Exu4oPHUuzYOk4T_")

# List all the files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")

#CTB
googledrive::drive_download(file = merged$id[merged$name=="CTB_absparams.csv"], 
                            path = "googledrive/CTB_absparams.csv",
                            overwrite = T)
#SMB
googledrive::drive_download(file = merged$id[merged$name=="SMB_absparams.csv"], 
                            path = "googledrive/SMB_absparams.csv",
                            overwrite = T)
#NCBd
googledrive::drive_download(file = merged$id[merged$name=="NCB_absparams.csv"], 
                            path = "googledrive/NCB_absparams.csv",
                            overwrite = T)
#LMP07
googledrive::drive_download(file = merged$id[merged$name=="LMP07_absparams.csv"], 
                            path = "googledrive/LMP07_absparams.csv",
                            overwrite = T)
#LMP27
googledrive::drive_download(file = merged$id[merged$name=="LMP27_absparams.csv"], 
                            path = "googledrive/LMP27_absparams.csv",
                            overwrite = T)

#LMP72
googledrive::drive_download(file = merged$id[merged$name=="LMP72_absparams.csv"], 
                            path = "googledrive/LMP72_absparams.csv",
                            overwrite = T)
# Load them separately
CTB <- read.csv("googledrive/CTB_absparams.csv")
SMB <- read.csv("googledrive/SMB_absparams.csv")
NCBd <- read.csv("googledrive/NCB_absparams.csv")
LMP07 <- read.csv("googledrive/LMP07_absparams.csv")
LMP27 <- read.csv("googledrive/LMP27_absparams.csv")
LMP72 <- read.csv("googledrive/LMP72_absparams.csv")

# Convert the DateTime column to POSIXct
CTB$DateTime <- as.POSIXct(CTB$DateTime, format = "%Y-%m-%d %H:%M:%S")
SMB$DateTime <- as.POSIXct(SMB$DateTime, format = "%Y-%m-%d %H:%M:%S")
NCBd$DateTime <- as.POSIXct(NCBd$DateTime, format = "%Y-%m-%d %H:%M:%S")
LMP07$DateTime <- as.POSIXct(LMP07$DateTime, format = "%Y-%m-%d %H:%M:%S")
LMP27$DateTime <- as.POSIXct(LMP27$DateTime, format = "%Y-%m-%d %H:%M:%S")
LMP72$DateTime <- as.POSIXct(LMP72$DateTime, format = "%Y-%m-%d %H:%M:%S")

##################################
#### Merge chem and scan data ####
##################################
# Remove NAs from DateTime column
sample_times <- sample_times %>%
  filter(!is.na(DateTime))

# Filter to get just one site at a time
UCTB <- filter(sample_times, Sample_Name == "CTB")
USMB <- filter(sample_times, Sample_Name == "SMB")
UNCBd <- filter(sample_times, Sample_Name == "NCB-down")
ULMP07 <- filter(sample_times, Sample_Name == "LMP07")
ULMP27 <- filter(sample_times, Sample_Name == "LMP27")
ULMP72 <- filter(sample_times, Sample_Name == "LMP72")

# First check if the merge works
datCTB <- merge(CTB, UCTB, by = "DateTime")
datSMB <- merge(SMB, USMB, by = "DateTime")
datNCBd <- merge(NCBd, UNCBd, by = "DateTime")
datLMP07 <- merge(LMP07, ULMP07, by = "DateTime")
datLMP27 <- merge(LMP27, ULMP27, by = "DateTime")
datLMP72 <- merge(LMP72, ULMP72, by = "DateTime")

# scan data first - perform a left join
dataCTB <- merge(CTB, UCTB, by = "DateTime", all.x = TRUE)
dataSMB <- merge(SMB, USMB, by = "DateTime", all.x = TRUE)
dataNCBd <- merge(NCBd, UNCBd, by = "DateTime", all.x = TRUE)
dataLMP07 <- merge(LMP07, ULMP07, by = "DateTime", all.x = TRUE)
dataLMP27 <- merge(LMP27, ULMP27, by = "DateTime", all.x = TRUE)
dataLMP72 <- merge(LMP72, ULMP72, by = "DateTime", all.x = TRUE)

# Check for duplicates in the original datasets
sum(duplicated(dataCTB))
sum(duplicated(dataSMB))
sum(duplicated(dataNCBd))
sum(duplicated(dataLMP07))
sum(duplicated(dataLMP27))
sum(duplicated(dataLMP72))

##################
#### Clean up ####
##################
dataCTB <- dataCTB %>%
  dplyr::select(-c(Temperature_26...F....Measured.status, Temperature_26...F....Measured.value,
                   Device.Rotation.......Measured.value, Device.Tilt.......Measured.value,
                   Supply.Current..mA....Measured.value, Supply.Voltage..V....Measured.status))
dataSMB <- dataSMB %>%
  dplyr::select(-c(Temperature_26...F....Measured.value, Temperature_28...F....Measured.value,
                   Temperature_19...C....Measured.value, Temperature_21...C....Measured.status,
                   Device.Rotation.......Measured.value, Device.Tilt.......Measured.value, 
                   Supply.Voltage..V....Measured.status, X.y, Device.Rotation.......Measured.status,
                   Supply.Current..mA....Measured.status, Temperature_28...F....Measured.status))
dataNCBd <- dataNCBd %>% 
  dplyr::select(-c(X.x,X.y, Temperature_26...F....Measured.value, Temperature_26...F....Measured.status,
                Device.Rotation.......Measured.value, Device.Tilt.......Measured.value,
                Supply.Current..mA....Measured.value, Supply.Voltage..V....Measured.status))
dataLMP07 <- dataLMP07 %>% 
  dplyr::select(-c(Temperature_26...F....Measured.status, Temperature_26...F....Measured.value,
                   Device.Rotation.......Measured.value, Device.Tilt.......Measured.value,
                   Supply.Current..mA....Measured.value, Supply.Voltage..V....Measured.status))
dataLMP27 <- dataLMP27 %>% 
  dplyr::select(-c(Temperature_26...F....Measured.status, Temperature_26...F....Measured.value,
                   Device.Rotation.......Measured.value, Device.Tilt.......Measured.value))
dataLMP72 <- dataLMP72 %>% 
  dplyr::select(-c(Temperature_37...F....Measured.status, Temperature_37...F....Measured.value,
                   Device.Rotation.......Measured.value, Device.Tilt.......Measured.value,
                   Device.Tilt.......Measured.status, Supply.Voltage..V....Measured.status,
                   Device.Rotation.......Measured.status, Supply.Current..mA....Measured.value,
                   Supply.Current..mA....Measured.status))

##########################
#### Clean up spectra ####
##########################
# Here you find when your spectra go negative. For USF data is around 450-460nm (column 123)
# Do not remove any spectral values if for tss
dataCTB_clean <- dataCTB[,-c(120:225)]
dataSMB_clean <- dataSMB[,-c(120:225)]
dataNCBd_clean <- dataNCBd[,-c(120:225)]
dataLMP07_clean <- dataLMP07[,-c(120:225)]
dataLMP27_clean <- dataLMP27[,-c(120:225)]
dataLMP72_clean <- dataLMP72[,-c(120:225)]

# data12 <- data12[,-c(220:228)]
# data20 <- data20[,-c(220:228)]
# data21 <- data21[,-c(220:228)]

################################################
#### Clean up spectra very low or high rows ####
################################################
# Remove rows where the condition under -0.5 and above 60 is not met.
dataCTB_clean <- dataCTB_clean %>%
  dplyr::filter(!if_any(c(16:119),
                        ~ . < -0.2 | . > 60))
dataSMB_clean <- dataSMB_clean %>%
  dplyr::filter(!if_any(c(16:119),
                        ~ . < -0.2 | . > 60))
dataNCBd_clean <- dataNCBd_clean %>%
  dplyr::filter(!if_any(c(16:119),
                        ~ . < -0.2 | . > 60))
dataLMP07_clean <- dataLMP07_clean %>%
  dplyr::filter(!if_any(c(16:119),
                        ~ . < -0.2 | . > 60))
dataLMP27_clean <- dataLMP27_clean %>%
  dplyr::filter(!if_any(c(16:119),
                        ~ . < -0.2 | . > 60))
dataLMP72_clean <- dataLMP72_clean %>%
  dplyr::filter(!if_any(c(16:119),
                        ~ . < -0.2 | . > 60))
############################
#### Save matched files ####
############################ 
# Make sure it is in datetime format
dataCTB_clean$DateTime <- format(dataCTB_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(dataCTB_clean,"googledrive/CTB_merged.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
dataSMB_clean$DateTime <- format(dataSMB_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(dataSMB_clean,"googledrive/SMB_merged.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
dataNCBd_clean$DateTime <- format(dataNCBd_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(dataNCBd_clean,"googledrive/NCBd_merged.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
dataLMP07_clean$DateTime <- format(dataLMP07_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(dataLMP07_clean,"googledrive/LMP07_merged.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
dataLMP27_clean$DateTime <- format(dataLMP27_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(dataLMP27_clean,"googledrive/LMP27_merged.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
dataLMP72_clean$DateTime <- format(dataLMP72_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(dataLMP72_clean,"googledrive/LMP72_merged.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "merged" folder
drive_folder_id <- "1PXTqYFSc6yArhhILFGxLF9a7_Rgu9A3q"

# Upload the file to the specified Google Drive folder
drive_put(media = "googledrive/CTB_merged.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/SMB_merged.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/NCBd_merged.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/LMP07_merged.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/LMP27_merged.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/LMP72_merged.csv", path = as_id(drive_folder_id))

