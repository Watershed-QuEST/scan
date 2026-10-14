##==============================================================================
## Project: QuEST
## Here we will be Calibrating s::can data using Partial Least Squares Regression (PLSR) 
## Following Arial's s::can guide
##==============================================================================

library(googledrive)
library(data.table)
library(xts)
library(dplyr)
library(pls)
library(spectrolab)
library(ggplot2)
library(plotly)

graphics.off()

######################################
#### Auto-save calibration figures ###
######################################
# so all of DV's figures land in one folder, "scan-figs".
fig_dir <- "scan_figs"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

save_plot <- function(filename, plot_expr, width = 900, height = 600, res = 120) {
  png(file.path(fig_dir, filename), width = width, height = height, res = res)
  print(plot_expr)
  dev.off()
}

###################################
#### Clear folders we will use ####
###################################
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

######################################
#### STEP 1: Prep grab sample data ###
######################################
# What you need to do here is match the grab samples with the time stamp of the s::can
# This data was matched using previous scripts 
# See scripts merge_params_and_abs and merge_grabsamples_and_scan

#######################################################
#### STEP 2: Upload scan data frame [with spectra] ####
#######################################################
# This is the "with chem" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/12N6uUxXTttdnadDrn43mL6ilz-Ui2eQX")

# List all xlsx files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")
2

#DVO
googledrive::drive_download(file = merged$id[merged$name=="DVO_chem.csv"], 
                            path = "googledrive/DVO_chem.csv",
                            overwrite = T)
#DVMS1
googledrive::drive_download(file = merged$id[merged$name=="DVMS1_chem.csv"], 
                            path = "googledrive/DVMS1_chem.csv",
                            overwrite = T)
#DVNWT5
googledrive::drive_download(file = merged$id[merged$name=="DVNWT5_chem.csv"], 
                            path = "googledrive/DVNWT5_chem.csv",
                            overwrite = T)

# Let's load them separately first
DVO <- read.csv("googledrive/DVO_chem.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
DVMS1 <- read.csv("googledrive/DVMS1_chem.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
DVNWT5 <- read.csv("googledrive/DVNWT5_chem.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)

# DateTime at midnight is missing 00:00:00 time, so filling in using grep
DVO$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVO$DateTime)] <- paste(
  DVO$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVO$DateTime)],"00:00:00")
DVMS1$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVMS1$DateTime)] <- paste(
  DVMS1$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVMS1$DateTime)],"00:00:00")
DVNWT5$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVNWT5$DateTime)] <- paste(
  DVNWT5$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",DVNWT5$DateTime)],"00:00:00")

# Convert the DateTime column to POSIXct
DVO$DateTime <- as.POSIXct(DVO$DateTime, format = "%Y-%m-%d %H:%M:%S")
DVMS1$DateTime <- as.POSIXct(DVMS1$DateTime, format = "%Y-%m-%d %H:%M:%S")
DVNWT5$DateTime <- as.POSIXct(DVNWT5$DateTime, format = "%Y-%m-%d %H:%M:%S")

# Remove NAs from DateTime column
DVO <- DVO %>%
  filter(!is.na(DateTime))
DVMS1 <- DVMS1 %>%
  filter(!is.na(DateTime))
DVNWT5 <- DVNWT5 %>%
  filter(!is.na(DateTime))

#########################################################
#### Plot raw s::can DOC (DOCeq) over time, per site ####
#########################################################
# This is the sensor's factory-calibrated DOC estimate
# (DOCeq..mg.l....Measured.value) -- for biofouling check
plot_raw_doc <- function(df, site_label) {
  ggplot(df, aes(x = DateTime, y = DOCeq..mg.l....Measured.value)) +
    geom_point(size = 0.6, alpha = 0.5, color = "darkorange") +
    labs(
      x = "DateTime",
      y = "Raw DOCeq (mg/L)",
      title = paste0(site_label, ": Raw s::can DOC over Time"),
      subtitle = "Sensor's own DOCeq, before PLSR recalibration"
    ) +
    theme_minimal()
}

save_plot("DVO_rawDOC_over_time.png", plot_raw_doc(DVO, "DVO"))
save_plot("DVMS1_rawDOC_over_time.png", plot_raw_doc(DVMS1, "DVMS1"))
save_plot("DVNWT5_rawDOC_over_time.png", plot_raw_doc(DVNWT5, "DVNWT5"))

# Rename columns by removing the X in front of the spectra (that brakes the code somehow)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}
# Apply the renaming to each data frame
DVO <- rename_columns(DVO)
DVMS1 <- rename_columns(DVMS1)
DVNWT5 <- rename_columns(DVNWT5)

# Extract DOC data as time series objects (xts)
scan_DOC_DVO <- xts(DVO$DOCeq..mg.l....Measured.value , order.by = DVO$DateTime)
scan_DOC_DVMS1 <- xts(DVMS1$DOCeq..mg.l....Measured.value, order.by = DVMS1$DateTime)
scan_DOC_DVNWT5 <- xts(DVNWT5$DOCeq..mg.l....Measured.value, order.by = DVNWT5$DateTime)

# Extract spectral data (assuming spectral columns are in range "DVMS10.00.nm" to "4.00.nm")
scan.specDVO = xts(DVO[15:116], as.POSIXct(DVO$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.specDVMS1 = xts(DVMS1[15:99], as.POSIXct(DVMS1$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.specDVNWT5 = xts(DVNWT5[15:96], as.POSIXct(DVNWT5$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
# select full spectra
# note here that if there are 0s in your spectra, this code will throw an error
# so only use the wavelengths where you have detectable absorbance

#################################################
####  STEP 3: Compare grab and raw scan data ####
#################################################
# This is just a check to see how well the s::can did relative to your known concentrations 
# I upload this as a new data frame, just because in the previous step I had assigned these XTS values
# Feel free to change this! It's not the most efficient way to do this...
# DVO <- DVO[,-1]
# DVMS1 <- DVMS1[,-1]
# DVNWT5 <- DVNWT5[,-1]

# Creating "Grab_sample" column based on values in "Sample.Name"
# Modify the Grab_sample column
DVO <- DVO %>% 
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",  # Assign "Y" if data exists
    TRUE ~ NA_character_  # Leave as NA otherwise
  ))

DVMS1 <- DVMS1 %>% 
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",
    TRUE ~ NA_character_
  ))

DVNWT5 <- DVNWT5 %>% 
  mutate(Grab_sample = case_when(
    !is.na(Sample.Name) & Sample.Name != "" ~ "Y",
    TRUE ~ NA_character_
  ))

# Filter using "Grab_sample" column
grab_DVO = DVO[DVO$Grab_sample == "Y",] # Ony gets data when there is a Y
grab_DVMS1 = DVMS1[DVMS1$Grab_sample == "Y",] # Ony gets data when there is a Y
grab_DVNWT5 = DVNWT5[DVNWT5$Grab_sample == "Y",] # Ony gets data when there is a Y

grab.DOCDVO = grab_DVO$NPOC..mg.C.L.
grab.DOCDVMS1 = grab_DVMS1$NPOC..mg.C.L.
grab.DOCDVNWT5 = grab_DVNWT5$NPOC..mg.C.L.

#### remove a couple of problematic samples ####
grab_DVO <- grab_DVO %>%
  mutate(NPOC..mg.C.L. = ifelse(DateTime == "2025-01-02 12:15:00" | is.na(NPOC..mg.C.L.),NA,NPOC..mg.C.L.))
grab_DVMS1 <- grab_DVMS1 %>%
  mutate(NPOC..mg.C.L. = ifelse(DateTime == "2024-06-19 14:00:00" | is.na(NPOC..mg.C.L.),NA,NPOC..mg.C.L.))

# compare grab vs scan DOC
plot(grab_DVO$DOCeq..mg.l....Measured.value ~ grab_DVO$NPOC..mg.C.L.)
ggplot(grab_DVO, aes(x = NPOC..mg.C.L., y = DOCeq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOCDVO = lm(grab_DVO$DOCeq..mg.l....Measured.value ~ grab_DVO$NPOC..mg.C.L.)
summary(calib.mod.DOCDVO)

ggplot(grab_DVMS1, aes(x = NPOC..mg.C.L., y = DOCeq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOCDVMS1 = lm(grab_DVMS1$DOCeq..mg.l....Measured.value ~ grab_DVMS1$NPOC..mg.C.L.)
summary(calib.mod.DOCDVMS1)

ggplot(grab_DVNWT5, aes(x = NPOC..mg.C.L., y = DOCeq..mg.l....Measured.value)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOCDVNWT5 = lm(grab_DVNWT5$DOCeq..mg.l....Measured.value ~ grab_DVNWT5$NPOC..mg.C.L.)
summary(calib.mod.DOCDVNWT5)

#######################################################################################
#### STEP 4: Create matrices of GRAB spectral data - this is the training data set ####
#######################################################################################
# 1. Index data set with columns with absorbances
# raw spectra
grab.spec.datDVO = grab_DVO[15:116] # Full spectra, with no NAs?
grab.spec.datDVMS1 = grab_DVMS1[15:99]
grab.spec.datDVNWT5 = grab_DVNWT5[15:96] 

# Rename columns for all data frames (e.g., DVO, DVMS1, DVNWT5)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}

# Apply the renaming to each data frame
grab.spec.datDVO <- rename_columns(grab.spec.datDVO)
grab.spec.datDVMS1 <- rename_columns(grab.spec.datDVMS1)
grab.spec.datDVNWT5 <- rename_columns(grab.spec.datDVNWT5)

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
absDVO = (grab.spec.datDVO)  # this is not doing anything and just copying grab.spec.datDVO again as absDVO
absDVMS1 = (grab.spec.datDVMS1)
absDVNWT5 = (grab.spec.datDVNWT5)
#str(abs)

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wlDVO <- gsub("_clean", "", colnames(absDVO))   
wlDVO <- as.numeric(wlDVO)
wlDVMS1 <- gsub("_clean", "", colnames(absDVMS1))   
wlDVMS1 <- as.numeric(wlDVMS1)
wlDVNWT5 <- gsub("_clean", "", colnames(absDVNWT5))   
wlDVNWT5 <- as.numeric(wlDVNWT5)
str(wlDVNWT5)

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrowDVO = as.numeric(nrow(absDVO))
NumDVO = c(1:lastrowDVO)

lastrowDVMS1 = as.numeric(nrow(absDVMS1))
NumDVMS1 = c(1:lastrowDVMS1)

lastrowDVNWT5 = as.numeric(nrow(absDVNWT5))
NumDVNWT5 = c(1:lastrowDVNWT5)

# 5. Create the final matrix 
grab.matrixDVO = cbind(absDVO) # this is not binding anything and just copying absDVO again as grab.matrixDVO?
rownames(grab.matrixDVO) = as.numeric(NumDVO)
colnames(grab.matrixDVO) = as.numeric(wlDVO)
grab.matrixDVO = as.matrix(grab.matrixDVO)
str(grab.matrixDVO)
attributes(grab.matrixDVO)

grab.matrixDVMS1 = cbind(absDVMS1)
rownames(grab.matrixDVMS1) = as.numeric(NumDVMS1)
colnames(grab.matrixDVMS1) = as.numeric(wlDVMS1)
grab.matrixDVMS1 = as.matrix(grab.matrixDVMS1)
str(grab.matrixDVMS1)
attributes(grab.matrixDVMS1)

grab.matrixDVNWT5 = cbind(absDVNWT5)
rownames(grab.matrixDVNWT5) = as.numeric(NumDVNWT5)
colnames(grab.matrixDVNWT5) = as.numeric(wlDVNWT5)
grab.matrixDVNWT5 = as.matrix(grab.matrixDVNWT5)
str(grab.matrixDVNWT5)
attributes(grab.matrixDVNWT5)

# 6. Make this into spectral matrix for model
# Must be in format: grab.spectra = spectra(value = abs, bands = wl, names = Num)
grab.spectraDVO = spectra(value = absDVO, bands = wlDVO, names = NumDVO)
attributes(grab.spectraDVO)
plot(grab.spectraDVO) # Note, bands here = absorbance from the scans
# Training-dataset wavelengths (the grab-sample-matched spectra PLSR trains on).
save_plot("DVO_train.png", plot(grab.spectraDVO))

grab.spectraDVMS1 = spectra(value = absDVMS1, bands = wlDVMS1, names = NumDVMS1)
attributes(grab.spectraDVMS1)
plot(grab.spectraDVMS1) # Note, bands here = absorbance from the scans
save_plot("DVMS1_train.png", plot(grab.spectraDVMS1))

grab.spectraDVNWT5 = spectra(value = absDVNWT5, bands = wlDVNWT5, names = NumDVNWT5)
attributes(grab.spectraDVNWT5)
plot(grab.spectraDVNWT5) # Note, bands here = absorbance from the scans
save_plot("DVNWT5_train.png", plot(grab.spectraDVNWT5))

#grab.spectra = as_spectra.list(grab.spectra, wave_unit = "wavenumber", measurement_nit = "absorbance")
grab.spectraDVO = as.matrix(grab.spectraDVO)
grab.spectraDVMS1 = as.matrix(grab.spectraDVMS1)
grab.spectraDVNWT5 = as.matrix(grab.spectraDVNWT5)
#str(grab.spectra)

# Change attributes so this is correct for scan data
attr(grab.spectraDVO, 'wave_unit') = 'wavelength'
attr(grab.spectraDVO, 'measurement_unit') = 'absorbance'
attributes(grab.spectraDVO)

attr(grab.spectraDVMS1, 'wave_unit') = 'wavelength'
attr(grab.spectraDVMS1, 'measurement_unit') = 'absorbance'
attributes(grab.spectraDVMS1)

attr(grab.spectraDVNWT5, 'wave_unit') = 'wavelength'
attr(grab.spectraDVNWT5, 'measurement_unit') = 'absorbance'
attributes(grab.spectraDVNWT5)

########################################################################################
#### STEP 5: Create matrices of ALL spectral data - raw data that needs calibration ####
########################################################################################
# 1. Index FULL dataset with columns with absorbances
# raw spectra
scan.specDVO = DVO[15:116]
scan.specDVMS1 = DVMS1[15:99] 
scan.specDVNWT5 = DVNWT5[15:96]

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
absDVO = (scan.specDVO)
absDVMS1 = (scan.specDVMS1) 
absDVNWT5 = (scan.specDVNWT5) 

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wlDVO <- gsub("_clean", "", colnames(absDVO))   
wlDVO <- as.numeric(wlDVO)
wlDVMS1 <- gsub("_clean", "", colnames(absDVMS1))   
wlDVMS1 <- as.numeric(wlDVMS1)
wlDVNWT5 <- gsub("_clean", "", colnames(absDVNWT5))   
wlDVNWT5 <- as.numeric(wlDVNWT5)

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrowDVO = as.numeric(nrow(absDVO))
NumDVO = c(1:lastrowDVO)

lastrowDVMS1 = as.numeric(nrow(absDVMS1))
NumDVMS1 = c(1:lastrowDVMS1)

lastrowDVNWT5 = as.numeric(nrow(absDVNWT5))
NumDVNWT5 = c(1:lastrowDVNWT5)

# 5. Create the final matrix 
#DVO
scan.matrixDVO = cbind(absDVO)
rownames(scan.matrixDVO) = as.numeric(NumDVO)
colnames(scan.matrixDVO) = as.numeric(wlDVO)

scan.matrixDVO = as.matrix(scan.matrixDVO)
specDVO = spectra(value = absDVO, bands = wlDVO, names = NumDVO)
plot(specDVO) # Note = reflectance here = absorbance from the scans
# Full deployment-record wavelengths (every scan, not just grab-matched ones).
save_plot("DVO_full_spectra.png", plot(specDVO))

#DVMS1
scan.matrixDVMS1 = cbind(absDVMS1)
rownames(scan.matrixDVMS1) = as.numeric(NumDVMS1)
colnames(scan.matrixDVMS1) = as.numeric(wlDVMS1)

scan.matrixDVMS1 = as.matrix(scan.matrixDVMS1)
specDVMS1 = spectra(value = absDVMS1, bands = wlDVMS1, names = NumDVMS1)
plot(specDVMS1) # Note = reflectance here = absorbance from the scans
save_plot("DVMS1_full_spectra.png", plot(specDVMS1))

#DVNWT5
scan.matrixDVNWT5 = cbind(absDVNWT5)
rownames(scan.matrixDVNWT5) = as.numeric(NumDVNWT5)
colnames(scan.matrixDVNWT5) = as.numeric(wlDVNWT5)

scan.matrixDVNWT5 = as.matrix(scan.matrixDVNWT5)
specDVNWT5 = spectra(value = absDVNWT5, bands = wlDVNWT5, names = NumDVNWT5)
plot(specDVNWT5) # Note = reflectance here = absorbance from the scans
save_plot("DVNWT5_full_spectra.png", plot(specDVNWT5))

# NOTE: this is where you can identify problem spectra & remove them

# = as.spectra.list(spec)
scan.spectraDVO = as.matrix(specDVO)
str(scan.spectraDVO)
attr(scan.spectraDVO, 'wave_unit') = 'wavelength'
attr(scan.spectraDVO, 'measurement_unit') = 'absorbance'
attributes(scan.spectraDVO)

scan.spectraDVMS1 = as.matrix(specDVMS1)
str(scan.spectraDVMS1)
attr(scan.spectraDVMS1, 'wave_unit') = 'wavelength'
attr(scan.spectraDVMS1, 'measurement_unit') = 'absorbance'
attributes(scan.spectraDVMS1)

scan.spectraDVNWT5 = as.matrix(specDVNWT5)
str(scan.spectraDVNWT5)
attr(scan.spectraDVNWT5, 'wave_unit') = 'wavelength'
attr(scan.spectraDVNWT5, 'measurement_unit') = 'absorbance'
attributes(scan.spectraDVNWT5)

####################################################################
#### STEP 6: Create a new data frame with the spectral matrices ####
####################################################################
# This creates a data frame with 
# 1. DOC (scan)
# 3. Full s::can spectra (from 2DVMS1-750nm)

length(scan_DOC_DVO)
dim(scan.spectraDVO) 
class(scan.spectraDVO)

# NOTE: We use the I() function to protect the Spectra 
spectralcal.dfDVO = data.frame(DOCDVO = scan_DOC_DVO, SpectraDVO = I(scan.spectraDVO))
str(spectralcal.dfDVO)

spectralcal.dfDVMS1 = data.frame(DOCDVMS1 = scan_DOC_DVMS1, SpectraDVMS1 = I(scan.spectraDVMS1))
str(spectralcal.dfDVMS1)

spectralcal.dfDVNWT5 = data.frame(DOCDVNWT5 = scan_DOC_DVNWT5, SpectraDVNWT5 = I(scan.spectraDVNWT5))
str(spectralcal.dfDVNWT5)

# Also do this for the GRAB sample data
grabcal.dfDVO = data.frame(DOCDVO = grab.DOCDVO, SpectraDVO = I(grab.spectraDVO))
str(grabcal.dfDVO)

grabcal.dfDVMS1 = data.frame(DOCDVMS1 = grab.DOCDVMS1, SpectraDVMS1 = I(grab.spectraDVMS1))
str(grabcal.dfDVMS1)

grabcal.dfDVNWT5 = data.frame(DOCDVNWT5 = grab.DOCDVNWT5, SpectraDVNWT5 = I(grab.spectraDVNWT5))
str(grabcal.dfDVNWT5)

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# Create a training and test data set
# Carbon
CTrainDVO = grabcal.dfDVO
CTestDVO = spectralcal.dfDVO

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
CmodDVO = plsr(DOCDVO ~ SpectraDVO, ncomp = 8, data = CTrainDVO, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(CmodDVO) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(CmodDVO), legendpos = "topright")
save_plot("DVO_RMSEP.png", plot(RMSEP(CmodDVO), legendpos = "topright"))

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples
plot(CmodDVO, ncomp = 3, asp = 1, line = TRUE)
save_plot("DVO_PLSR.png", plot(CmodDVO, ncomp = 3, asp = 1, line = TRUE))

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model!
predictedCDVO = predict(CmodDVO, ncomp = 3, newdata = spectralcal.dfDVO) # use reduced error model
str(predictedCDVO)
plot(predictedCDVO)
save_plot("DVO_pred.png", plot(predictedCDVO))

write.csv(predictedCDVO, file = "predicted/PredictedC_DVO.csv") # <- this is your newly calibrated dataset!

## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# Create a training and test dataset
# Carbon
CTrainDVMS1 = grabcal.dfDVMS1
CTestDVMS1 = spectralcal.dfDVMS1

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
CmodDVMS1 = plsr(DOCDVMS1 ~ SpectraDVMS1, ncomp = 4, data = CTrainDVMS1, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(CmodDVMS1) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(CmodDVMS1), legendpos = "topright")
save_plot("DVMS1_RMSEP.png", plot(RMSEP(CmodDVMS1), legendpos = "topright"))

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error (in this case, x)
# NOTE: This plot may be messy, given low number of grab samples
plot(CmodDVMS1, ncomp = 2, asp = 1, line = TRUE)
save_plot("DVMS1_PLSR.png", plot(CmodDVMS1, ncomp = 2, asp = 1, line = TRUE))

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model!
predictedCDVMS1 = predict(CmodDVMS1, ncomp = 2, newdata = spectralcal.dfDVMS1) # use reduced error model
str(predictedCDVMS1)
# Plot final predictions
plot(predictedCDVMS1)
save_plot("DVMS1_pred.png", plot(predictedCDVMS1))

write.csv(predictedCDVMS1, file = "predicted/PredictedC_DVMS1.csv") # <- this is your newly calibrated dataset!

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# Create a training and test dataset
# Carbon
CTrainDVNWT5 = grabcal.dfDVNWT5
CTestDVNWT5 = spectralcal.dfDVNWT5

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
CmodDVNWT5 = plsr(DOCDVNWT5 ~ SpectraDVNWT5, ncomp = 3, data = CTrainDVNWT5, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(CmodDVNWT5) # optimized for 4 components

# Plot RMSE of the predictions to optimize model
plot(RMSEP(CmodDVNWT5), legendpos = "topright")
save_plot("DVNWT5_RMSEP.png", plot(RMSEP(CmodDVNWT5), legendpos = "topright"))

# Plot predicted vs. measured from optimized model
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples
plot(CmodDVNWT5, ncomp = 1, asp = 1, line = TRUE)
save_plot("DVNWT5_PLSR.png", plot(CmodDVNWT5, ncomp = 1, asp = 1, line = TRUE))

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model!
predictedCDVNWT5 = predict(CmodDVNWT5, ncomp = 1, newdata = spectralcal.dfDVNWT5) # use reduced error model
str(predictedCDVNWT5)
# Plot
plot(predictedCDVNWT5)
save_plot("DVNWT5_pred.png", plot(predictedCDVNWT5))

write.csv(predictedCDVNWT5, file = "predicted/PredictedC_DVNWT5.csv") # <- this is your newly calibrated dataset!

# Convert predictedCDVNWT5 to a data frame
pred_df_DVNWT5 <- data.frame(
  DateTime = as.POSIXct(dimnames(predictedCDVNWT5)[[1]]),
  Predicted = as.numeric(predictedCDVNWT5))
# Plot
save_plot("DVNWT5_Cpred.png", ggplot(pred_df_DVNWT5, aes(x = DateTime, y = Predicted)) +
  geom_point(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted DOC (mg/L)",
    title = "Predicted DOC over Time (DVNWT5)"
  ) +
  theme_minimal())

# Convert predictedCDVMS1 to a data frame
pred_df_DVMS1 <- data.frame(
  DateTime = as.POSIXct(dimnames(predictedCDVMS1)[[1]]),
  Predicted = as.numeric(predictedCDVMS1))
# Plot
save_plot("DVMS1_Cpred.png", ggplot(pred_df_DVMS1, aes(x = DateTime, y = Predicted)) +
  geom_point(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted DOC (mg/L)",
    title = "Predicted DOC over Time (DVMS1)"
  ) +
  theme_minimal())

# Convert predictedCDVO to a data frame
pred_df_DVO <- data.frame(
  DateTime = as.POSIXct(dimnames(predictedCDVO)[[1]]),
  Predicted = as.numeric(predictedCDVO))
# Plot
save_plot("DVO_Cpred.png", ggplot(pred_df_DVO, aes(x = DateTime, y = Predicted)) +
  geom_point(color = "steelblue") +
  labs(
    x = "DateTime",
    y = "Predicted DOC (mg/L)",
    title = "Predicted DOC over Time (DVO)"
  ) +
  theme_minimal())

#######################
#### Save in Drive #### 
#######################
# Define the target folder ID in Google Drive
# This is the "predicted" folder
drive_folder_id <- "13bh64kWtdgknMUqdfDKkJ4JAzvWqLpu8"

# Upload the file to the specified Google Drive folder
drive_put(media = "predicted/PredictedC_DVO.csv", path = as_id(drive_folder_id))
drive_put(media = "predicted/PredictedC_DVMS1.csv", path = as_id(drive_folder_id))
drive_put(media = "predicted/PredictedC_DVNWT5.csv", path = as_id(drive_folder_id))
