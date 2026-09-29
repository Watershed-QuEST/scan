##==============================================================================
## Project: QuEST
## Here we will prep grab sample data by matching the grab samples with the time stamp of the s::can
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive)
library(googlesheets4)
library(dplyr)
library(xts)
library(readxl)
library(tidyverse)
library(lubridate)

########################################
#### Clear folders that we will use ####
########################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

##########################
#### Import chem data ####
##########################
# chem data is for all the sites
chem <- googledrive::as_id("https://drive.google.com/drive/folders/1yNoGf43hkNDC5TX7wMWc27PYvcRD0vul")

# list all CSV files in the folder
chem_csv <- googledrive::drive_ls(path = chem, type = "csv")
3

# call the specific file you want (most recent one)
googledrive::drive_download(file = chem_csv$id[chem_csv$name=="chem_avg_flagged.csv"], 
                            path = "googledrive/chem_avg_flagged.csv",
                            overwrite = T)
# load it into R
wqual = read.csv("googledrive/chem_avg_flagged.csv")

# format date columns
wqual$Date <- as.Date(wqual$Date, format = "%Y-%m-%d")

# clean up a bit
drops <- c("X", "Project", "Sub_ProjectA", "pH", "Cond", "Spec_Cond", "DO_Conc",  "DO.", "Temperature Turbidity", "ID")
wqual <- wqual[ , !(names(wqual) %in% drops)]

# filter to get just NM data
NM <- filter(wqual, Sub_Project == "New Mexico")

#### combine same day-same site samples (reps and bottles) ####

# when there are reps per site per date we need to average them and use the average chem to calculate leverage
head(NM)
# define the columns that need to be averaged
columns_to_average <- c("NPOC..mg.C.L.", "NO3..mg.N.L.", "NH4..ug.N.L.", "TDN..mg.N.L.", 
                        "PO4..ug.P.L.", "Cl..mg.Cl.L.", "SO4..mg.S.L.", "Na..mg.Na.L.", 
                        "K..mg.K.L.", "Mg..mg.Mg.L.", "Ca..mg.Ca.L.")

# calculate averages or fill non-NA values for each Site and Date
data_avg <- NM %>%
  # Group by columns Date and Site, and other unique identifiers if necessary
  group_by(Date, Site) %>%
  
  # summarize: for each column, take the mean if there are multiple values or the single non-NA value
  summarise(across(all_of(columns_to_average),
                   ~ if (all(is.na(.))) NA_real_ else mean(., na.rm = TRUE)),  # calculate mean if there are values
            Sample.Name = paste0(first(Site), "_", first(Date), "_Avg"),    # create a new Sample Name with _Avg
            .groups = "drop") # Ungroup to avoid nested data frames


# count non-NA values in Q column using dplyr
nonna_counts_dplyr <- data_avg %>%
  summarise_all(~ sum(!is.na(.)))

# save the averaged chem data to a CSV file
# write.csv(data_avg,"googledrive/avg_chem.csv" , row.names=FALSE, quote=FALSE)

#### load sample info to get grab sample collection time ####
samplelogsheet <- drive_get("https://docs.google.com/spreadsheets/d/1xxSKNQiXFZ-jtFHj2ruqwq5LqSrl9hc37rCcNp8gQ0s/edit?gid=0#gid=0")

# download spreadsheet from Webster Lab Sample Log Sheet
drive_download(as_id(samplelogsheet$id), path = "googledrive/samplelogsheet.xlsx", overwrite = T)

# fetch the file
samplelogsheet <- readxl::read_excel("googledrive/samplelogsheet.xlsx")

# format date and time columns
samplelogsheet$Date <- as.Date(samplelogsheet$Date, format = "%Y-%m-%d")
samplelogsheet$Time <- as.POSIXct(samplelogsheet$Time, format = "%Y-%mm-%dd %H:%M:%S")
samplelogsheet$Time <- format(as.POSIXct(samplelogsheet$Time, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")

# clean up a bit
samplelogsheet <- samplelogsheet[ -c(1, 5:25) ]

#### change sample time to fit scan time ####
###USF12###
samplelogsheet$Time[samplelogsheet$Site == "USF12" & 
                      samplelogsheet$Date == "2024-05-23" & 
                      samplelogsheet$Time == "09:30:00"] <- "09:45:00"
###USF20###
samplelogsheet$Time[samplelogsheet$Site == "USF20" & 
                      samplelogsheet$Date == "2024-05-23" & 
                      samplelogsheet$Time == "12:30:00"] <- "12:45:00"

# samplelogsheet$Time[samplelogsheet$Site == "USF20" & 
#                       samplelogsheet$Date == "2024-06-19" & 
#                       samplelogsheet$Time == "14:45:00"] <- "17:00:00"
###USF21###
samplelogsheet$Time[samplelogsheet$Site == "USF21" & 
                      samplelogsheet$Date == "2024-06-27" & 
                      samplelogsheet$Time == "11:00:00"] <- "18:30:00"

##########################
#### Import TSS data ####
##########################
tss <- drive_get("https://docs.google.com/spreadsheets/d/1f7LXHGubFcBGVTavNCwS7rve_djajuqd/edit?gid=1123637453#gid=1123637453")
# download spreadsheet
drive_download(as_id(tss$id), path = "googledrive/TSS & AFDM Data.xlsx", overwrite = T)
# fetch the file
TSS <- readxl::read_excel("googledrive/TSS & AFDM Data.xlsx")

# format date and time columns
TSS$Date <- as.Date(TSS$Date, format = "%Y-%m-%d")

TSS <- TSS[, -c(3:12, 14:17, 19)]
 
############################################################################
#### Merge chem, TSS and sample log sheet to get sample collection time ####
############################################################################
# filter only data for USF12, 20 and 21 (scan sites)
wqual_scans <- data_avg %>% filter(Site %in% c("USF12", "USF21", "USF20"))

# wqual data first
sample_times <- merge(wqual_scans, samplelogsheet, by = c("Date", "Site"))

# check for duplicates in the original datasets
sum(duplicated(sample_times))
# remove duplicates from the original datasets
sample_times <- sample_times %>% distinct()
# # remove duplicates ignoring QuEST_ID column
# sample_times <- sample_times %>% 
#   distinct(
#     across(-QuEST_ID), # Check for distinctness on all columns EXCEPT QuEST_ID
#     .keep_all = TRUE # This argument is not strictly needed here because across() is used to select columns
#     # but it is good practice to ensure all columns are kept.
#   )

# filter only data for USF12, 20 and 21 (scan sites)
TSS <- TSS %>% filter(Site %in% c("USF12", "USF21", "USF20"))

# Now do TSS
sample_times <- merge(sample_times, TSS, by = c("Date", "Site"), all.x = TRUE, all.y = TRUE)

# combine Date and Time columns into a new DateTime column
sample_times$DateTime <- paste(sample_times$Date, sample_times$Time, sep = " ")
# convert the DateTime column to POSIXct
sample_times$DateTime <- as.POSIXct(sample_times$DateTime, format = "%Y-%m-%d %H:%M")

##########################
#### Import scan data ####
##########################
#### import abs and parameter data ####
# this is the "clean abs" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1g6aSuGnb--Qeyk-rceX82Y5wSNzCqFg0")

# list all the files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")

#USF12
googledrive::drive_download(file = merged$id[merged$name=="USF12_clean.csv"], 
                            path = "googledrive/USF12_clean.csv",
                            overwrite = T)
#USF20
googledrive::drive_download(file = merged$id[merged$name=="USF20_clean.csv"], 
                            path = "googledrive/USF20_clean.csv",
                            overwrite = T)
#USF21
googledrive::drive_download(file = merged$id[merged$name=="USF21_clean.csv"], 
                            path = "googledrive/USF21_clean.csv",
                            overwrite = T)

# load them separately 
USF12 <- read.csv("googledrive/USF12_clean.csv")
USF20 <- read.csv("googledrive/USF20_clean.csv")
USF21 <- read.csv("googledrive/USF21_clean.csv")

# DateTime at midnight is missing 00:00:00 time, so filling in that time using grep
USF12$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF12$DateTime)] <- paste(
  USF12$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF12$DateTime)],"00:00:00")
USF20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF20$DateTime)] <- paste(
  USF20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF20$DateTime)],"00:00:00")
USF21$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF21$DateTime)] <- paste(
  USF21$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",USF21$DateTime)],"00:00:00")

# convert the DateTime column to POSIXct
USF12$DateTime <- as.POSIXct(USF12$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF20$DateTime <- as.POSIXct(USF20$DateTime, format = "%Y-%m-%d %H:%M:%S")
USF21$DateTime <- as.POSIXct(USF21$DateTime, format = "%Y-%m-%d %H:%M:%S")

# # check for duplicates
# sum(duplicated(USF12))
# sum(duplicated(USF20))
# sum(duplicated(USF21))

##################################
#### Merge chem and scan data ####
##################################
# filter to get just one site at a time
U12 <- filter(sample_times, Site == "USF12")
U20 <- filter(sample_times, Site == "USF20")
U21 <- filter(sample_times, Site == "USF21")

# first check if the merge works
dat12 <- merge(USF12, U12, by = "DateTime")
dat20 <- merge(USF20, U20, by = "DateTime")
dat21 <- merge(USF21, U21, by = "DateTime")

# scan data first - perform a left join
data12 <- merge(USF12, U12, by = "DateTime", all.x = TRUE)
data20 <- merge(USF20, U20, by = "DateTime", all.x = TRUE)
data21 <- merge(USF21, U21, by = "DateTime", all.x = TRUE)

# # check for duplicates in the original datasets
# sum(duplicated(data12))
# sum(duplicated(data20))
# sum(duplicated(data21))

##################
#### Clean up ####
##################
data12 <- data12 %>%
  dplyr::select(-c(Temperature_40...F....Measured.value, Temperature_26...F....Measured.value,
                   Temperature_26....F....Measured.status, Temperature_40...F....Measured.status,
                   Temperature_26...F....Measured.status, Temperature_19....C....Measured.status,
                   Device.Rotation.......Measured.value, Device.Tilt.......Measured.value,
                   Supply.Current..mA....Measured.status, Supply.Voltage..V....Measured.status, serial_number,
                   Device.Rotation........Measured.value, Device.Rotation.......Measured.status,
                   Device.Tilt.......Measured.status, Device.Tilt.......Measured.value, 
                   Device.Tilt........Measured.value, Temperature_19...C....Measured.status,
                   Temperature_26....F....Measured.value
                   ))
data20 <- data20 %>%
  dplyr::select(-Temperature_19...C....Measured.status, -Temperature_20...F....Measured.status,
                -X725.00.nm, -X727.50.nm, -serial_number,-Device.Rotation.......Measured.value,
                -Device.Tilt.......Measured.value, -Device.Rotation.......Measured.value)
data21 <- data21 %>% 
  dplyr::select(-Temperature_26...F....Measured.value, -Temperature_26...F....Measured.status,
         -Device.Rotation.......Measured.status, -Device.Tilt.......Measured.status, 
         -Supply.Current..mA....Measured.status, -Supply.Voltage..V....Measured.status, -serial_number,
         -Device.Rotation.......Measured.value, Device.Tilt.......Measured.value,
         -Temperature_28...F....Measured.value, -Temperature_28...F....Measured.status,
         -Temperature_21...C....Measured.status, -Temperature_28...F....Measured.value,
         -Temperature_21...C....Measured.status, -Temperature_19...C....Measured.status,
         -Device.Tilt.......Measured.value)

##########################
#### Clean up spectra ####
##########################
# Wavelength columns are named like "X200.00.nm" ... "X750.00.nm". Instead of
# hardcoded column positions (which drift whenever a column gets added/removed
# upstream -- that's why USF12/20/21 needed three different index ranges for
# what is really the same 450-460nm cutoff), select spectral columns by their
# actual wavelength value. USF absorbance goes negative/unreliable above
# ~450nm, so that's the DOC cutoff; TSS keeps the full spectrum.
is_wl_col     <- function(nm) grepl("^X[0-9]+\\.[0-9]+\\.nm$", nm)
wl_from_name  <- function(nm) as.numeric(gsub("^X|\\.nm$", "", nm))
DOC_WL_MAX    <- 450  # nm; above this USF spectra go negative/unreliable for DOC

drop_high_wavelengths <- function(df, wl_max = DOC_WL_MAX) {
  nm   <- names(df)
  drop <- is_wl_col(nm) & (wl_from_name(nm) > wl_max)
  df[, !drop, drop = FALSE]
}

data12_clean <- drop_high_wavelengths(data12)
data20_clean <- drop_high_wavelengths(data20)
data21_clean <- drop_high_wavelengths(data21)

#################################################
#### Clean up spectra, very low or high rows ####
#################################################
# Flag a row as bad if:
#  - any single wavelength falls outside a physically plausible absorbance
#    range, OR
#  - the whole spectrum is essentially flat (max-min across wavelengths is
#    too small). A flatlined/garbage scan (sensor fouled, out of water, mid-
#    maintenance) can sit entirely inside [-1, 60] and still not be real
#    data -- this is exactly what happened at USF12 in January 2026: every
#    scan that month was a near-constant line instead of the usual decay
#    curve, and it fed straight into the DOC PLSR model as if it were valid.
#    Compare scan_figs/absorbances/USF12_Absorbance_January_2026.png against
#    December_2025 / February_2026 for the same site.
# NOTE: min_range is a starting guess -- check it against a histogram of your
# own good-vs-bad row ranges (good USF spectra span roughly 30-40 units from
# 200 to 450nm; January 2026's flatlined scans spanned only ~1 unit) and
# tighten or loosen as needed.
filter_good_spectra <- function(df, lo = -1, hi = 60, min_range = 5) {
  wl_cols <- names(df)[is_wl_col(names(df))]
  spec    <- as.matrix(df[, wl_cols])
  out_of_range <- apply(spec, 1, function(x) any(x < lo | x > hi, na.rm = TRUE))
  flat         <- apply(spec, 1, function(x) diff(range(x, na.rm = TRUE)) < min_range)
  df[!(out_of_range | flat), , drop = FALSE]
}

data12_clean <- filter_good_spectra(data12_clean, lo = -1, hi = 60, min_range = 5)
data20_clean <- filter_good_spectra(data20_clean, lo = -1, hi = 60, min_range = 5)
data21_clean <- filter_good_spectra(data21_clean, lo = -1, hi = 60, min_range = 5)

# for tss: keep the full spectrum (including the high-wavelength band), just
# drop rows with clearly bad values in that band
filter_good_spectra_tss <- function(df, wl_min = DOC_WL_MAX, lo = -5, hi = 60) {
  nm      <- names(df)
  wl_cols <- nm[is_wl_col(nm) & wl_from_name(nm) > wl_min]
  spec    <- as.matrix(df[, wl_cols])
  out_of_range <- apply(spec, 1, function(x) any(x < lo | x > hi, na.rm = TRUE))
  df[!out_of_range, , drop = FALSE]
}

data12_tss <- filter_good_spectra_tss(data12)
data20_tss <- filter_good_spectra_tss(data20)
data21_tss <- filter_good_spectra_tss(data21)

############################
#### Save matched files ####
############################
# make sure it is in datetime format
data12_clean$DateTime <- format(data12_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(data12_clean,"googledrive/USF12_chem.csv" , row.names=FALSE, quote=FALSE)
# make sure it is in datetime format
data20_clean$DateTime <- format(data20_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(data20_clean,"googledrive/USF20_chem.csv" , row.names=FALSE, quote=FALSE)
# make sure it is in datetime format
data21_clean$DateTime <- format(data21_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(data21_clean,"googledrive/USF21_chem.csv" , row.names=FALSE, quote=FALSE)

# For TSS
# make sure it is in datetime format
data12_tss$DateTime <- format(data12_tss$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(data12_tss,"googledrive/USF12_tss.csv" , row.names=FALSE, quote=FALSE)
# make sure it is in datetime format
data20_tss$DateTime <- format(data20_tss$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(data20_tss,"googledrive/USF20_tss.csv" , row.names=FALSE, quote=FALSE)
# make sure it is in datetime format
data21_tss$DateTime <- format(data21_tss$DateTime, "%Y-%m-%d %H:%M:%S")
# save the new data frame to a CSV file
write.csv(data21_tss,"googledrive/USF21_tss.csv" , row.names=FALSE, quote=FALSE)

# define the target folder ID in Google Drive
# this is the "with chem" folder
drive_folder_id <- "1qjM3Zze-I5ycFCHNcd997UG6gYXBUoX8"

# upload the file to the specified Google Drive folder
drive_put(media = "googledrive/USF12_chem.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/USF20_chem.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/USF21_chem.csv", path = as_id(drive_folder_id))

# upload the file to the specified Google Drive folder
drive_put(media = "googledrive/USF12_tss.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/USF20_tss.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/USF21_tss.csv", path = as_id(drive_folder_id))

