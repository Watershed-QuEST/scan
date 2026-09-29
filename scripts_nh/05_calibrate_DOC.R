##==============================================================================
## Project: QuEST
## Here we will be Calibrating s::can data using Partial Least Squares Regression (PLSR) 
## Following Arial's s::can guide
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive) 
library(data.table)

library(xts)
library(dplyr)
library(pls)
library(merTools)
library(devtools)
#install.packages("devtools")
#install.packages("devtools", repos = "http://cran.us.r-project.org")
library(spectrolab)
#install_github("meireles/spectrolab")
#install_github(repo = "meireles/spectrolab") # Install analysis package
# Make sure to hit "no" for install
library(ggplot2)
library(plotly)

###################################
#### Clear folders we will use ####
###################################
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

files <- list.files(path = "predicted", full.names = TRUE)
file.remove(files)

######################################
#### STEP 1: Prep grab sample data ###
######################################
# What you need to do here is match the grab samples with the time stamp of the s::can
# This data was matched using previous scripts #
# See scripts merge_params_and_abs and merge_grabsamples_and_scan

#######################################################
#### STEP 2: Upload scan data frame [with spectra] ####
#######################################################
# This is the "with chem" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1PXTqYFSc6yArhhILFGxLF9a7_Rgu9A3q")

# List all xlsx files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")
3

#CTB
googledrive::drive_download(file = merged$id[merged$name=="CTB_merged.csv"],
                            path = "googledrive/CTB_merged.csv",
                            overwrite = T)
#SMB
googledrive::drive_download(file = merged$id[merged$name=="SMB_merged.csv"],
                            path = "googledrive/SMB_merged.csv",
                            overwrite = T)
#NCBd
googledrive::drive_download(file = merged$id[merged$name=="NCBd_merged.csv"],
                            path = "googledrive/NCBd_merged.csv",
                            overwrite = T)
#LMP07
googledrive::drive_download(file = merged$id[merged$name=="LMP07_merged.csv"],
                            path = "googledrive/LMP07_merged.csv",
                            overwrite = T)
#LMP27
googledrive::drive_download(file = merged$id[merged$name=="LMP27_merged.csv"],
                            path = "googledrive/LMP27_merged.csv",
                            overwrite = T)
#LMP72
googledrive::drive_download(file = merged$id[merged$name=="LMP72_merged.csv"],
                            path = "googledrive/LMP72_merged.csv",
                            overwrite = T)


# Let's load them separately first
CTB <- read.csv("googledrive/CTB_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
SMB <- read.csv("googledrive/SMB_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
NCBd <- read.csv("googledrive/NCBd_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
LMP07 <- read.csv("googledrive/LMP07_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
LMP27 <- read.csv("googledrive/LMP27_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
LMP72 <- read.csv("googledrive/LMP72_merged.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)

# DateTime at midnight is missing 00:00:00 time, so filling in using grep
CTB$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",CTB$DateTime)] <- paste(
  CTB$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",CTB$DateTime)],"00:00:00")
SMB$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SMB$DateTime)] <- paste(
  SMB$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SMB$DateTime)],"00:00:00")
NCBd$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",NCBd$DateTime)] <- paste(
  NCBd$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",NCBd$DateTime)],"00:00:00")
LMP07$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",LMP07$DateTime)] <- paste(
  LMP07$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",LMP07$DateTime)],"00:00:00")
LMP27$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",LMP27$DateTime)] <- paste(
  LMP27$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",LMP27$DateTime)],"00:00:00")
LMP72$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",LMP72$DateTime)] <- paste(
  LMP72$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",LMP72$DateTime)],"00:00:00")

# Convert the DateTime column to POSIXct
CTB$DateTime <- as.POSIXct(CTB$DateTime, format = "%Y-%m-%d %H:%M:%S")
SMB$DateTime <- as.POSIXct(SMB$DateTime, format = "%Y-%m-%d %H:%M:%S")
NCBd$DateTime <- as.POSIXct(NCBd$DateTime, format = "%Y-%m-%d %H:%M:%S")
LMP07$DateTime <- as.POSIXct(LMP07$DateTime, format = "%Y-%m-%d %H:%M:%S")
LMP27$DateTime <- as.POSIXct(LMP27$DateTime, format = "%Y-%m-%d %H:%M:%S")
LMP72$DateTime <- as.POSIXct(LMP72$DateTime, format = "%Y-%m-%d %H:%M:%S")

# Remove NAs from DateTime column
CTB <- CTB %>%
  filter(!is.na(DateTime))
SMB <- SMB %>%
  filter(!is.na(DateTime))
NCBd <- NCBd %>%
  filter(!is.na(DateTime))
LMP07 <- LMP07 %>%
  filter(!is.na(DateTime))
LMP27 <- LMP27 %>%
  filter(!is.na(DateTime))
LMP72 <- LMP72 %>%
  filter(!is.na(DateTime))

# Rename columns by removing the X in front of the spectra (that brakes the code somehow)
# rename_columns <- function(df) {
#   colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
#   return(df)
# }
# # Apply the renaming to each data frame
# USFCTB <- rename_columns(USFCTB)
# USFSMB <- rename_columns(USFSMB)
# USFNCBd <- rename_columns(USFNCBd)

# Extract DOC data as time series objects (xts)
scan_DOC_SMB <- xts(SMB$DOCeq..mg.l....Measured.value, order.by = SMB$DateTime)
scan_DOC_CTB <- xts(CTB$DOCeq..mg.l....Measured.value, order.by = CTB$DateTime)
scan_DOC_NCBd <- xts(NCBd$DOCeq..mg.l....Measured.value, order.by = NCBd$DateTime)
scan_DOC_LMP07 <- xts(LMP07$DOCeq..mg.l....Measured.value, order.by = LMP07$DateTime)
scan_DOC_LMP27 <- xts(LMP27$DOCeq..mg.l....Measured.value, order.by = LMP27$DateTime)
scan_DOC_LMP72 <- xts(LMP72$DOCeq..mg.l....Measured.value, order.by = LMP72$DateTime)

# Extract spectral data (assuming spectral columns are in range "SMB0.00.nm" to "4.00.nm")
scan.specSMB= xts(SMB[16:119], as.POSIXct(SMB$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.specCTB = xts(CTB[16:119], as.POSIXct(CTB$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.specNCBd = xts(NCBd[16:119], as.POSIXct(NCBd$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.spec07 = xts(LMP07[16:119], as.POSIXct(LMP07$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.spec27 = xts(LMP27[16:119], as.POSIXct(LMP27$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.spec72 = xts(LMP72[16:119], as.POSIXct(LMP72$DateTime, format = "%Y-%m-%d %H:%M:%S")) 

# select full spectra
# note here that if there are 0s in your spectra, this code will throw an error
# so only use the wavelengths where you have detectable absorbance

#################################################
####  STEP 3: Compare grab and raw scan data ####
#################################################
# This is just a check to see how well the s::can did relative to your known concentrations 
# I upload this as a new data frame, just because in the previous step I had assigned these XTS values
# Feel free to change this! It's not the most efficient way to do this...
# USFCTB <- USFCTB[,-1]
# USFSMB <- USFSMB[,-1]
# USFNCBd <- USFNCBd[,-1]

# Creating "Grab_sample" column based on values in "Sample.Name"
# Modify the Grab_sample column
SMB <- SMB %>% 
  mutate(Grab_sample = case_when(
    !is.na(NPOC..mg.C.L.) & NPOC..mg.C.L. != "" ~ "Y",  # Assign "Y" if data exists
    TRUE ~ NA_character_  # Leave as NA otherwise
  ))
CTB <- CTB %>% 
  mutate(Grab_sample = case_when(
    !is.na(NPOC..mg.C.L.) & NPOC..mg.C.L. != "" ~ "Y",
    TRUE ~ NA_character_
  ))

NCBd <- NCBd %>%
  mutate(NPOC..mg.C.L. = ifelse(DateTime == "2025-08-11 11:00:00" | is.na(NPOC..mg.C.L.),NA,NPOC..mg.C.L.))

NCBd <- NCBd %>% 
  mutate(Grab_sample = case_when(
    !is.na(NPOC..mg.C.L.) & NPOC..mg.C.L. != "" ~ "Y",
    TRUE ~ NA_character_
  ))

LMP07 <- LMP07 %>% 
  mutate(Grab_sample = case_when(
    !is.na(NPOC..mg.C.L.) & NPOC..mg.C.L. != "" ~ "Y",
    TRUE ~ NA_character_
  ))

LMP27 <- LMP27 %>%
  mutate(NPOC..mg.C.L. = ifelse(DateTime %in% as.POSIXct(c("2025-09-08 13:00:00", 
                                                           "2025-01-13 16:00:00", 
                                                           "2024-12-11 14:00:00")) | 
                                  is.na(NPOC..mg.C.L.), NA, NPOC..mg.C.L.))

LMP27 <- LMP27 %>% 
  mutate(Grab_sample = case_when(
    !is.na(NPOC..mg.C.L.) & NPOC..mg.C.L. != "" ~ "Y",
    TRUE ~ NA_character_
  ))


LMP72 <- LMP72 %>% 
  mutate(Grab_sample = case_when(
    !is.na(NPOC..mg.C.L.) & NPOC..mg.C.L. != "" ~ "Y",
    TRUE ~ NA_character_
  ))

# Filter using "Grab_sample" column
grab_SMB = SMB[SMB$Sample_Name == "Y",] # Ony gets data when there is a Y
grab_CTB = CTB[CTB$Grab_sample == "Y",] # Ony gets data when there is a Y
grab_NCBd = NCBd[NCBd$Grab_sample == "Y",] # Ony gets data when there is a Y
grab_LMP07 = LMP07[LMP07$Grab_sample == "Y",] # Ony gets data when there is a Y
grab_LMP27 = LMP27[LMP27$Grab_sample == "Y",] # Ony gets data when there is a Y
grab_LMP72 = LMP72[LMP72$Grab_sample == "Y",] # Ony gets data when there is a Y

grab.DOCSMB = grab_SMB$NPOC..mg.C.L.
grab.DOCCTB = grab_CTB$NPOC..mg.C.L.
grab.DOCNCBd = grab_NCBd$NPOC..mg.C.L.
grab.DOC07 = grab_LMP07$NPOC..mg.C.L.
grab.DOC27 = grab_LMP27$NPOC..mg.C.L.
grab.DOC72 = grab_LMP72$NPOC..mg.C.L.

#### remove a couple of problematic samples ####
# grab_LMP27 <- grab_LMP27 %>%
#   mutate(NPOC..mg.C.L. = ifelse(DateTime == "2025-09-08 13:00:00" | is.na(NPOC..mg.C.L.),NA,NPOC..mg.C.L.))

# grab_USFSMB <- grab_USFSMB %>%
#   mutate(NPOC..mg.C.L. = ifelse(DateTime == "SMB24-06-19 14:00:00" | is.na(NPOC..mg.C.L.),NA,NPOC..mg.C.L.))

# compare grab vs scan DOC
ggplot(grab_CTB, aes(x = NPOC..mg.C.L., y = DOCeq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOCCTB = lm(grab_CTB$DOCeq..mg.l....Measured.value ~ grab_CTB$NPOC..mg.C.L.)
summary(calib.mod.DOCCTB)

ggplot(grab_SMB, aes(x = NPOC..mg.C.L., y = DOCeq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOCSMB = lm(grab_SMB$DOCeq..mg.l....Measured.value ~ grab_SMB$NPOC..mg.C.L.)
summary(calib.mod.DOCSMB)

ggplot(grab_NCBd, aes(x = NPOC..mg.C.L., y = DOCeq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOCNCBs = lm(grab_NCBd$DOCeq..mg.l....Measured.value ~ grab_NCBd$NPOC..mg.C.L.)
summary(calib.mod.DOCNCBs)

ggplot(grab_LMP07, aes(x = NPOC..mg.C.L., y = DOCeq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOC07 = lm(grab_LMP07$DOCeq..mg.l....Measured.value ~ grab_LMP07$NPOC..mg.C.L.)
summary(calib.mod.DOC07)

ggplot(grab_LMP27, aes(x = NPOC..mg.C.L., y = DOCeq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOC27 = lm(grab_LMP27$DOCeq..mg.l....Measured.value ~ grab_LMP27$NPOC..mg.C.L.)
summary(calib.mod.DOC27)

ggplot(grab_LMP72, aes(x = NPOC..mg.C.L., y = DOCeq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOC27 = lm(grab_LMP72$DOCeq..mg.l....Measured.value ~ grab_LMP72$NPOC..mg.C.L.)
summary(calib.mod.DOC27)

#######################################################################################
#### STEP 4: Create matrices of GRAB spectral data - this is the training data set ####
#######################################################################################
# 1. Index data set with columns with absorbances
# raw spectra
grab.spec.datCTB = grab_CTB[16:119] # Full spectra, with no NAs?
# grab.spec.datSMB = grab_SMB[16:95]
grab.spec.datNCBd = grab_NCBd[16:119] 
grab.spec.dat07 = grab_LMP07[16:119]
grab.spec.dat27 = grab_LMP27[16:119]
grab.spec.dat72 = grab_LMP72[16:119]

# Rename columns for all data frames (e.g., USFCTB, USFSMB, USFNCBd)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}
# Apply the renaming to each data frame
grab.spec.datCTB <- rename_columns(grab.spec.datCTB)
# grab.spec.datSMB <- rename_columns(grab.spec.datSMB)
grab.spec.datNCBd <- rename_columns(grab.spec.datNCBd)
grab.spec.dat07 <- rename_columns(grab.spec.dat07)
grab.spec.dat27 <- rename_columns(grab.spec.dat27)
grab.spec.dat72 <- rename_columns(grab.spec.dat72)

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
absCTB = (grab.spec.datCTB)  # this is not doing anything and just copying grab.spec.datCTB again as absCTB?
# absSMB = (grab.spec.datSMB)
absNCBd = (grab.spec.datNCBd)
abs07 = (grab.spec.dat07)
abs27 = (grab.spec.dat27)
abs72 = (grab.spec.dat72)
#str(abs)

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wlCTB <- gsub("_clean", "", colnames(absCTB))   
wlCTB <- as.numeric(wlCTB)
# wlSMB <- gsub("_clean", "", colnames(absSMB))   
# wlSMB <- as.numeric(wlSMB)
wlNCBd <- gsub("_clean", "", colnames(absNCBd))   
wlNCBd <- as.numeric(wlNCBd)
wl07 <- gsub("_clean", "", colnames(abs07))   
wl07 <- as.numeric(wl07)
wl27 <- gsub("_clean", "", colnames(abs27))   
wl27 <- as.numeric(wl27)
wl72 <- gsub("_clean", "", colnames(abs72))   
wl72 <- as.numeric(wl72)

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrowCTB = as.numeric(nrow(absCTB))
NumCTB = c(1:lastrowCTB)
# lastrowSMB = as.numeric(nrow(absSMB))
# NumSMB = c(1:lastrowSMB)
lastrowNCBd = as.numeric(nrow(absNCBd))
NumNCBd = c(1:lastrowNCBd)
lastrow07 = as.numeric(nrow(abs07))
Num07 = c(1:lastrow07)
lastrow27 = as.numeric(nrow(abs27))
Num27 = c(1:lastrow27)
lastrow72 = as.numeric(nrow(abs72))
Num72 = c(1:lastrow72)

# 5. Create the final matrix 
grab.matrixCTB = cbind(absCTB) # this is not binding anything and just copying absCTB again as grab.matrixCTB 
rownames(grab.matrixCTB) = as.numeric(NumCTB)
colnames(grab.matrixCTB) = as.numeric(wlCTB)
grab.matrixCTB = as.matrix(grab.matrixCTB)
str(grab.matrixCTB)
attributes(grab.matrixCTB)

# grab.matrixSMB = cbind(absSMB)
# rownames(grab.matrixSMB) = as.numeric(NumSMB)
# colnames(grab.matrixSMB) = as.numeric(wlSMB)
# grab.matrixSMB = as.matrix(grab.matrixSMB)
# str(grab.matrixSMB)
# attributes(grab.matrixSMB)

grab.matrixNCBd = cbind(absNCBd)
rownames(grab.matrixNCBd) = as.numeric(NumNCBd)
colnames(grab.matrixNCBd) = as.numeric(wlNCBd)
grab.matrixNCBd = as.matrix(grab.matrixNCBd)
str(grab.matrixNCBd)
attributes(grab.matrixNCBd)

grab.matrix07 = cbind(abs07)
rownames(grab.matrix07) = as.numeric(Num07)
colnames(grab.matrix07) = as.numeric(wl07)
grab.matrix07 = as.matrix(grab.matrix07)
str(grab.matrix07)
attributes(grab.matrix07)

grab.matrix27 = cbind(abs27)
rownames(grab.matrix27) = as.numeric(Num27)
colnames(grab.matrix27) = as.numeric(wl27)
grab.matrix27 = as.matrix(grab.matrix27)
str(grab.matrix27)
attributes(grab.matrix27)

grab.matrix72 = cbind(abs72)
rownames(grab.matrix72) = as.numeric(Num72)
colnames(grab.matrix72) = as.numeric(wl72)
grab.matrix72 = as.matrix(grab.matrix72)
str(grab.matrix72)
attributes(grab.matrix72)

# 6. Make this into spectral matrix for model
# Must be in format: grab.spectra = spectra(value = abs, bands = wl, names = Num)
grab.spectraCTB = spectra(value = absCTB, bands = wlCTB, names = NumCTB)
attributes(grab.spectraCTB)
plot(grab.spectraCTB) # Note, bands here = absorbance from the scans

# grab.spectraSMB = spectra(value = absSMB, bands = wlSMB, names = NumSMB)
# attributes(grab.spectraSMB)
# plot(grab.spectraSMB) # Note, bands here = absorbance from the scans

grab.spectraNCBd = spectra(value = absNCBd, bands = wlNCBd, names = NumNCBd)
attributes(grab.spectraNCBd)
plot(grab.spectraNCBd) # Note, bands here = absorbance from the scans

grab.spectra07 = spectra(value = abs07, bands = wl07, names = Num07)
attributes(grab.spectra07)
plot(grab.spectra07)

grab.spectra27 = spectra(value = abs27, bands = wl27, names = Num27)
attributes(grab.spectra27)
plot(grab.spectra27)

grab.spectra72 = spectra(value = abs72, bands = wl72, names = Num72)
attributes(grab.spectra72)
plot(grab.spectra72)

#grab.spectra = as_spectra.list(grab.spectra, wave_unit = "wavenumber", measurement_nit = "absorbance")
grab.spectraCTB = as.matrix(grab.spectraCTB)
# grab.spectraSMB = as.matrix(grab.spectraSMB)
grab.spectraNCBd = as.matrix(grab.spectraNCBd)
grab.spectra07 = as.matrix(grab.spectra07)
grab.spectra27 = as.matrix(grab.spectra27)
grab.spectra72 = as.matrix(grab.spectra72)
#str(grab.spectra)

# Change attributes so this is correct for scan data
attr(grab.spectraCTB, 'wave_unit') = 'wavelength'
attr(grab.spectraCTB, 'measurement_unit') = 'absorbance'
attributes(grab.spectraCTB)

# attr(grab.spectraSMB, 'wave_unit') = 'wavelength'
# attr(grab.spectraSMB, 'measurement_unit') = 'absorbance'
# attributes(grab.spectraSMB)

attr(grab.spectraNCBd, 'wave_unit') = 'wavelength'
attr(grab.spectraNCBd, 'measurement_unit') = 'absorbance'
attributes(grab.spectraNCBd)

attr(grab.spectra07, 'wave_unit') = 'wavelength'
attr(grab.spectra07, 'measurement_unit') = 'absorbance'
attributes(grab.spectra07)

attr(grab.spectra27, 'wave_unit') = 'wavelength'
attr(grab.spectra27, 'measurement_unit') = 'absorbance'
attributes(grab.spectra27)

attr(grab.spectra72, 'wave_unit') = 'wavelength'
attr(grab.spectra72, 'measurement_unit') = 'absorbance'
attributes(grab.spectra72)

########################################################################################
#### STEP 5: Create matrices of ALL spectral data - raw data that needs calibration ####
########################################################################################
# 1. Index FULL dataset with columns with absorbances
# raw spectra
scan.specCTB = CTB[16:119]
# scan.specSMB = SMB[16:95] 
scan.specNCBd = NCBd[16:119]
scan.spec07 = LMP07[16:119]
scan.spec27 = LMP27[16:119]
scan.spec72 = LMP72[16:119]

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
absCTB = (scan.specCTB)
# absSMB = (scan.specSMB) 
absNCBd = (scan.specNCBd) 
abs07 = (scan.spec07)
abs27 = (scan.spec27)
abs72 = (scan.spec72)

# Rename columns for all data frames (e.g., USFCTB, USFSMB, USFNCBd)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}
# Apply the renaming to each data frame
absCTB <- rename_columns(absCTB)
# absSMB <- rename_columns(absSMB)
absNCBd <- rename_columns(absNCBd)
abs07 <- rename_columns(abs07)
abs27 <- rename_columns(abs27)
abs72 <- rename_columns(abs72)

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wlCTB <- gsub("_clean", "", colnames(absCTB))   
wlCTB <- as.numeric(wlCTB)
# wlSMB <- gsub("_clean", "", colnames(absSMB))   
# wlSMB <- as.numeric(wlSMB)
wlNCBd <- gsub("_clean", "", colnames(absNCBd))   
wlNCBd <- as.numeric(wlNCBd)
wl07 <- gsub("_clean", "", colnames(abs07))   
wl07 <- as.numeric(wl07)
wl27 <- gsub("_clean", "", colnames(abs27))   
wl27 <- as.numeric(wl27)
wl72 <- gsub("_clean", "", colnames(abs72))   
wl72 <- as.numeric(wl27)

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrowCTB = as.numeric(nrow(absCTB))
NumCTB = c(1:lastrowCTB)

# lastrowSMB = as.numeric(nrow(absSMB))
# NumSMB = c(1:lastrowSMB)

lastrowNCBd = as.numeric(nrow(absNCBd))
NumNCBd = c(1:lastrowNCBd)

lastrow07 = as.numeric(nrow(abs07))
Num07 = c(1:lastrow07)

lastrow27 = as.numeric(nrow(abs27))
Num27 = c(1:lastrow27)

lastrow72 = as.numeric(nrow(abs72))
Num72 = c(1:lastrow72)

# 5. Create the final matrix 
#CTB
scan.matrixCTB = cbind(absCTB)
rownames(scan.matrixCTB) = as.numeric(NumCTB)
colnames(scan.matrixCTB) = as.numeric(wlCTB)

scan.matrixCTB = as.matrix(scan.matrixCTB)
specCTB = spectra(value = absCTB, bands = wlCTB, names = NumCTB)
plot(specCTB) # Note = reflectance here = absorbance from the scans

# #SMB
# scan.matrixSMB = cbind(absSMB)
# rownames(scan.matrixSMB) = as.numeric(NumSMB)
# colnames(scan.matrixSMB) = as.numeric(wlSMB)
# 
# scan.matrixSMB = as.matrix(scan.matrixSMB)
# specSMB = spectra(value = absSMB, bands = wlSMB, names = NumSMB)
# plot(specSMB) # Note = reflectance here = absorbance from the scans

#NCBd
scan.matrixNCBd = cbind(absNCBd)
rownames(scan.matrixNCBd) = as.numeric(NumNCBd)
colnames(scan.matrixNCBd) = as.numeric(wlNCBd)

scan.matrixNCBd = as.matrix(scan.matrixNCBd)
specNCBd = spectra(value = absNCBd, bands = wlNCBd, names = NumNCBd)
plot(specNCBd) # Note = reflectance here = absorbance from the scans

#LMP07
scan.matrix07 = cbind(abs07)
rownames(scan.matrix07) = as.numeric(Num07)
colnames(scan.matrix07) = as.numeric(wl07)

scan.matrix07 = as.matrix(scan.matrix07)
spec07 = spectra(value = abs07, bands = wl07, names = Num07)
plot(spec07) # Note = reflectance here = absorbance from the scans

#LMP27
scan.matrix27 = cbind(abs27)
rownames(scan.matrix27) = as.numeric(Num27)
colnames(scan.matrix27) = as.numeric(wl27)

scan.matrix27 = as.matrix(scan.matrix27)
spec27 = spectra(value = abs27, bands = wl27, names = Num27)
plot(spec27) # Note = reflectance here = absorbance from the scans

#LMP72
scan.matrix72 = cbind(abs72)
rownames(scan.matrix72) = as.numeric(Num72)
colnames(scan.matrix72) = as.numeric(wl72)

scan.matrix72 = as.matrix(scan.matrix72)
spec72 = spectra(value = abs72, bands = wl72, names = Num72)
plot(spec72) # Note = reflectance here = absorbance from the scans

# NOTE: this is where you can identify problem spectra & remove them

# = as.spectra.list(spec)
scan.spectraCTB = as.matrix(specCTB)
str(scan.spectraCTB)
attr(scan.spectraCTB, 'wave_unit') = 'wavelength'
attr(scan.spectraCTB, 'measurement_unit') = 'absorbance'
attributes(scan.spectraCTB)

# scan.spectraSMB = as.matrix(specSMB)
# str(scan.spectraSMB)
# attr(scan.spectraSMB, 'wave_unit') = 'wavelength'
# attr(scan.spectraSMB, 'measurement_unit') = 'absorbance'
# attributes(scan.spectraSMB)

scan.spectraNCBd = as.matrix(specNCBd)
str(scan.spectraNCBd)
attr(scan.spectraNCBd, 'wave_unit') = 'wavelength'
attr(scan.spectraNCBd, 'measurement_unit') = 'absorbance'
attributes(scan.spectraNCBd)

scan.spectra07 = as.matrix(spec07)
str(scan.spectra07)
attr(scan.spectra07, 'wave_unit') = 'wavelength'
attr(scan.spectra07, 'measurement_unit') = 'absorbance'
attributes(scan.spectra07)

scan.spectra27 = as.matrix(spec27)
str(scan.spectra27)
attr(scan.spectra27, 'wave_unit') = 'wavelength'
attr(scan.spectra27, 'measurement_unit') = 'absorbance'
attributes(scan.spectra27)

scan.spectra72 = as.matrix(spec72)
str(scan.spectra72)
attr(scan.spectra72, 'wave_unit') = 'wavelength'
attr(scan.spectra72, 'measurement_unit') = 'absorbance'
attributes(scan.spectra72)

####################################################################
#### STEP 6: Create a new data frame with the spectral matrices ####
####################################################################
# This creates a data frame with 
# 1. DOC (scan)
# 3. Full s::can spectra (from 2SMB-750nm)

length(scan_DOC_CTB)
dim(scan.spectraCTB) 
class(scan.spectraCTB)

# NOTE: We use the I() function to protect the Spectra 
spectralcal.dfCTB = data.frame(DOCCTB = scan_DOC_CTB, SpectraCTB = I(scan.spectraCTB))
str(spectralcal.dfCTB)

# spectralcal.dfSMB = data.frame(DOCSMB = scan_DOC_SMB, SpectraSMB = I(scan.spectraSMB))
# str(spectralcal.dfSMB)

spectralcal.dfNCBd = data.frame(DOCNCBd = scan_DOC_NCBd, SpectraNCBd = I(scan.spectraNCBd))
str(spectralcal.dfNCBd)

spectralcal.df07 = data.frame(DOC07 = scan_DOC_LMP07, Spectra07 = I(scan.spectra07))
str(spectralcal.df07)

spectralcal.df27 = data.frame(DOC27 = scan_DOC_LMP27, Spectra27 = I(scan.spectra27))
str(spectralcal.df27)

spectralcal.df72 = data.frame(DOC72 = scan_DOC_LMP72, Spectra72 = I(scan.spectra72))
str(spectralcal.df72)

# Also do this for the GRAB sample data
grabcal.dfCTB = data.frame(DOCCTB = grab.DOCCTB, SpectraCTB = I(grab.spectraCTB))
str(grabcal.dfCTB)

# grabcal.dfSMB = data.frame(DOCSMB = grab.DOCSMB, SpectraSMB = I(grab.spectraSMB))
# str(grabcal.dfSMB)

grabcal.dfNCBd = data.frame(DOCNCBd = grab.DOCNCBd, SpectraNCBd = I(grab.spectraNCBd))
str(grabcal.dfNCBd)

grabcal.df07 = data.frame(DOC07 = grab.DOC07, Spectra07 = I(grab.spectra07))
str(grabcal.df07)

grabcal.df27 = data.frame(DOC27 = grab.DOC27, Spectra27 = I(grab.spectra27))
str(grabcal.df27)

grabcal.df72 = data.frame(DOC72 = grab.DOC72, Spectra72 = I(grab.spectra72))
str(grabcal.df72)

##################################################### 
#### STEP 7 CTB: Develop PLSR training data sets ####
##################################################### 
# Create a training and test data set
# Carbon
CTrainCTB = grabcal.dfCTB
CTestCTB = spectralcal.dfCTB

# PLSR Model with "training"data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
CmodCTB = plsr(DOCCTB ~ SpectraCTB, ncomp = 5, data = CTrainCTB, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(CmodCTB) # optimized for 4 components

?plsr
  
# Plot RMSE of the predictions to optimize model
plot(RMSEP(CmodCTB), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
# NOTE: This plot may be messy,given low number of grab samples 
plot(CmodCTB, ncomp = 4, asp = 1, line = TRUE)

########################################################################
#### STEP 8 CTB: Make predictions based on reduced-error PLSR model #### 
########################################################################
# Predict model!
predictedCCTB = predict(CmodCTB, ncomp = 4, newdata = spectralcal.dfCTB) # use reduced error model
str(predictedCCTB)
plot(predictedCCTB)

write.csv(predictedCCTB, file = "predicted/PredictedC_CTB.csv") # <- this is your newly calibrated dataset!

# Convert predictedCCTB to a data frame
pred_df <- data.frame(
  DateTime = as.POSIXct(dimnames(predictedCCTB)[[1]]),
  Predicted = as.numeric(predictedCCTB))
# Plot
p <- ggplot(pred_df, aes(x = DateTime, y = Predicted)) +
  geom_line(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted DOC (mg/L)",
    title = "Predicted DOC over Time (CTB)"
  ) +
  theme_minimal()
ggplotly(p)

## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)

# #####################################################
# #### STEP 7 SMB: Develop PLSR training data sets ####
# #####################################################
# # Create a training and test dataset
# # Carbon
# CTrainSMB = grabcal.dfSMB
# CTestSMB = spectralcal.dfSMB
# 
# # PLSR Model with "training" data, use # of grab samples - 1
# # LOO = Leave One Out cross-comparison
# CmodSMB = plsr(DOCSMB ~ SpectraSMB, ncomp = 1, data = CTrainSMB, validation = "LOO") # usually ncomp is N-1 grab samples you have
# summary(CmodSMB) # optimized for 4 components
# 
# # Plot RMSE of the predictions to optimize model
# plot(RMSEP(CmodSMB), legendpos = "topright")
# 
# # Plot predicted vs. measured from optimized model
# # Pick the number of components with the least error (in this case, x)
# # NOTE: This plot may be messy, given low number of grab samples 
# plot(CmodSMB, ncomp = 1, asp = 1, line = TRUE)
# 
# ########################################################################
# #### STEP 8 SMB: Make predictions based on reduced-error PLSR model #### 
# ########################################################################
# # Predict model!
# predictedCSMB = predict(CmodSMB, ncomp = 1, newdata = spectralcal.dfSMB) # use reduced error model
# str(predictedCSMB)
# # Plot final predictions
# plot(predictedCSMB)
# 
# write.csv(predictedCSMB, file = "predicted/PredictedC_SMB.csv") # <- this is your newly calibrated dataset!
# 
# ## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# # You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)
# # This is a bit more complicated, so start with this simple calibration first. 
# 
# # Convert predictedCSMB to a data frame
# pred_df <- data.frame(
#   DateTime = as.POSIXct(dimnames(predictedCSMB)[[1]]),
#   Predicted = as.numeric(predictedCSMB))
# # Plot
# ggplot(pred_df, aes(x = DateTime, y = Predicted)) +
#   geom_point(color = "steelblue") +
#   labs(
#     x = "DateTime",
#     y = "Predicted DOC (mg/L)",
#     title = "Predicted DOC over Time (USFSMB)"
#   ) +
#   theme_minimal()

######################################################
#### STEP 7 NCBd: Develop PLSR training data sets ####
######################################################
# Create a training and test dataset
# Carbon
CTrainNCBd = grabcal.dfNCBd
CTestNCBd = spectralcal.dfNCBd

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
CmodNCBd = plsr(DOCNCBd ~ SpectraNCBd, ncomp = 11, data = CTrainNCBd, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(CmodNCBd) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(CmodNCBd), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples 
plot(CmodNCBd, ncomp = 1, asp = 1, line = TRUE)

#########################################################################
#### STEP 8 NCBd: Make predictions based on reduced-error PLSR model #### 
#########################################################################
# Predict model!
predictedCNCBd = predict(CmodNCBd, ncomp = 1, newdata = spectralcal.dfNCBd) # use reduced error model
str(predictedCNCBd)
# Plot
plot(predictedCNCBd)

write.csv(predictedCNCBd, file = "predicted/PredictedC_NCBd.csv") # <- this is your newly calibrated dataset!

# 1. Loadings Plot for USFCTB (Opposite Trend)
# This shows how the wavelengths contribute to each component (ncomp = 1, 2, 3, etc.)
plot(CmodCTB, plottype = "loading",
     comps = 1:2, # Plot the first two components for initial inspection
     main = "CTB NO3-N PLSR Loadings")

# 2. Loadings Plot for USFSMB (Flat Trend)
plot(CmodSMB, plottype = "loading",
     comps = 1:2, # Plot the first two components
     main = "SMB NO3-N PLSR Loadings")

# 3. Loadings Plot for USFNCBd (Flat Trend)
# Examine the first few components for USFNCBd
plot(CmodNCBd, plottype = "loading",
     comps = 1:2, # Plot the first two components
     main = "NCBd NO3-N PLSR Loadings")

# Convert predictedCNCBd to a data frame
pred_df <- data.frame(
  DateTime = as.POSIXct(dimnames(predictedCNCBd)[[1]]),
  Predicted = as.numeric(predictedCNCBd))
# Plot
ggplot(pred_df, aes(x = DateTime, y = Predicted)) +
  geom_point(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted DOC (mg/L)",
    title = "Predicted DOC over Time (USFNCBd)"
  ) +
  theme_minimal()

#######################################################
#### STEP 7 LMP07: Develop PLSR training data sets ####
#######################################################
# Create a training and test dataset
# Carbon
CTrain07 = grabcal.df07
CTest07 = spectralcal.df07

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
Cmod07 = plsr(DOC07 ~ Spectra07, ncomp = 5, data = CTrain07, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod07) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Cmod07), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples 
plot(Cmod07, ncomp = 3, asp = 1, line = TRUE)

##########################################################################
#### STEP 8 LMP07: Make predictions based on reduced-error PLSR model #### 
##########################################################################
# Predict model!
predictedC07 = predict(Cmod07, ncomp = 3, newdata = spectralcal.df07) # use reduced error model
str(predictedC07)
# Plot
plot(predictedC07)

write.csv(predictedC07, file = "predicted/PredictedC_LMP07.csv") # <- this is your newly calibrated dataset!

#######################################################
#### STEP 7 LMP27: Develop PLSR training data sets ####
#######################################################
# Create a training and test dataset
# Carbon
CTrain27 = grabcal.df27
CTest27 = spectralcal.df27

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
Cmod27 = plsr(DOC27 ~ Spectra27, ncomp = 8, data = CTrain27, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod27) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Cmod27), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples 
plot(Cmod27, ncomp = 4, asp = 1, line = TRUE)

##########################################################################
#### STEP 8 LMP27: Make predictions based on reduced-error PLSR model #### 
##########################################################################
# Predict model!
predictedC27 = predict(Cmod27, ncomp = 1, newdata = spectralcal.df27) # use reduced error model
str(predictedC27)
# Plot
plot(predictedC27)

write.csv(predictedC27, file = "predicted/PredictedC_LMP27.csv") # <- this is your newly calibrated dataset!

#######################################################
#### STEP 7 LMP72: Develop PLSR training data sets ####
#######################################################
# Create a training and test dataset
# Carbon
CTrain72 = grabcal.df72
CTest72 = spectralcal.df72

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
Cmod72 = plsr(DOC72 ~ Spectra72, ncomp = 10, data = CTrain72, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(Cmod72) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(Cmod72), legendpos = "topright")

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples 
plot(Cmod72, ncomp = 2, asp = 1, line = TRUE)

##########################################################################
#### STEP 8 LMP72: Make predictions based on reduced-error PLSR model #### 
##########################################################################
# Predict model!
predictedC72 = predict(Cmod72, ncomp = 2, newdata = spectralcal.df72) # use reduced error model
str(predictedC72)
# Plot
plot(predictedC72)

write.csv(predictedC72, file = "predicted/PredictedC_LMP72.csv") # <- this is your newly calibrated dataset!

#######################
#### Save in Drive #### 
#######################
# Define the target folder ID in Google Drive
# This is the "predicted" folder
drive_folder_id <- "1I7v6sk8h9DZ1mju3LVx0RvuL4PCZ72tz"

# Upload the file to the specified Google Drive folder
drive_put(media = "predicted/PredictedC_CTB.csv", path = as_id(drive_folder_id))
# drive_put(media = "predicted/PredictedC_SMB.csv", path = as_id(drive_folder_id))
drive_put(media = "predicted/PredictedC_NCBd.csv", path = as_id(drive_folder_id))
drive_put(media = "predicted/PredictedC_LMP07.csv", path = as_id(drive_folder_id))
drive_put(media = "predicted/PredictedC_LMP27.csv", path = as_id(drive_folder_id))
drive_put(media = "predicted/PredictedC_LMP72.csv", path = as_id(drive_folder_id))

## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)
# This is a bit more complicated, so start with this simple calibration first. 
