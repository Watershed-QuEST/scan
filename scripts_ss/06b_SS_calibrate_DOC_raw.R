##==============================================================================
## Project: QuEST
## Here we will be Calibrating s::can data using Partial Least Squares Regression (PLSR) 
## Following Arial's s::can guide
##
## RAW (uncompensated) FINGERPRINT VERSION -- NEW (Sep 28)
## Identical calibration pipeline to 06_SS_calibrate_DOC.R, but reads the
## raw/uncompensated spectra (*_merged_raw.csv, produced by
## 05_SS_merge_grabsamples_and_scan.R from 01/02's *_absparams_raw*.csv)
## instead of the default turbidity-compensated spectra. This tests Ariel's
## suggestion that the compensation algorithm may be overcorrecting and
## removing real DOC-related spectral signal. Run this alongside
## 06_SS_calibrate_DOC.R and compare the report_fit_stats() R2/r/RMSE
## output for each site -- that's the actual side-by-side test. Every
## output file (figures, predicted CSVs) is suffixed "_raw" so nothing here
## ever overwrites the compensated pipeline's output. If you change
## something here (ncomp, a QC threshold, etc.), consider whether the same
## change should be mirrored in 06_SS_calibrate_DOC.R for a fair
## comparison, and vice versa -- these two files are NOT auto-synced.
##==============================================================================

library(googledrive)
library(data.table)
library(xts)
library(dplyr)
library(pls)
library(spectrolab)
library(ggplot2)
library(plotly)

# Defensive insurance against a real bug hit today: if this script is run
# interactively in chunks (rather than sourced top-to-bottom in one clean
# pass) and an earlier run errored out partway through, a png() device from
# that earlier run can be left open. A later save_plot() call can then write
# to -- or pick up leftover content from -- the wrong device, so one site's
# PNG silently ends up holding another site's plot (this happened: SSM01's
# comps_natural.png came out holding SST13's plot instead). graphics.off()
# force-closes every open graphics device before this script opens any of
# its own, so a stray device from a previous partial run can't contaminate
# this one. Still best to run the whole script top-to-bottom in one go
# rather than by selection.
graphics.off()

######################################################
#### Spectral wavelength cutoff (testable, Sep 25) ###
######################################################
# The predictor spectrum was hardcoded to columns [17:117], i.e. every
# retained wavelength from 200-450nm. We discussed testing a narrower
# range (e.g. cutting off around 300-350nm instead of 450nm) -- DOC's
# strongest/most informative UV-Vis signal is typically in the UV region
# (~200-300nm; a254 is the classic single-wavelength DOC proxy), so a lot
# of the 350-450nm range may just be adding noisy/uninformative predictors
# to the PLSR fit without adding real DOC signal. This makes that cutoff a
# single tunable constant instead of six hardcoded [17:117] slices, so it's
# easy to try different values and rerun.
#
# Change ONLY this line to test a different cutoff (e.g. 300, 350):
DOC_WL_MAX <- 450

# Selects the column *positions* (not names) in df whose header is a
# wavelength (matches "200.00", "202.50", etc -- the format left after
# rename_columns() strips "X"/".nm") and whose numeric value is
# <= DOC_WL_MAX. Works on SSM01/SSM20/SST13 and on grab_SSM01/etc (same
# columns, fewer rows) since both keep the original column names.
spec_col_idx <- function(df, wl_max = DOC_WL_MAX) {
  wl_col <- grep("^[0-9]+\\.[0-9]+$", colnames(df))
  wl_val <- as.numeric(colnames(df)[wl_col])
  wl_col[wl_val <= wl_max]
}

######################################
#### Auto-save calibration figures ###
######################################
# All the plot() calls below used to only go to the interactive graphics
# device, so saving any of them to scan_figs/ meant manually exporting each
# one by hand -- easy to skip (SSM20's rmse/comps plots were missing for
# exactly this reason). save_plot() opens a png() device, evaluates the
# plot expression into it, and closes it, so every figure below is written
# to disk automatically as the script runs. Works for both base-R plot()
# calls (which draw as a side effect) and ggplot objects (via print()).
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
# This data was matched using previous scripts #
# See scripts merge_params_and_abs and merge_grabsamples_and_scan

#######################################################
#### STEP 2: Upload scan data frame [with spectra] ####
#######################################################
# This data is already matched #
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1Wju54VbyACZ_RFtfeInSvBCiVDKFScGj")

# List all CSVs files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")
2

#SSM01
googledrive::drive_download(file = merged$id[merged$name=="SSM01_merged_raw.csv"], 
                            path = "googledrive/SSM01_merged_raw.csv",
                            overwrite = T)
#SSM20
googledrive::drive_download(file = merged$id[merged$name=="SSM20_merged_raw.csv"], 
                            path = "googledrive/SSM20_merged_raw.csv",
                            overwrite = T)
#SST13
googledrive::drive_download(file = merged$id[merged$name=="SST13_merged_raw.csv"], 
                            path = "googledrive/SST13_merged_raw.csv",
                            overwrite = T)

# Load them separately 
SSM01 <- read.csv("googledrive/SSM01_merged_raw.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
SSM20 <- read.csv("googledrive/SSM20_merged_raw.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)
SST13 <- read.csv("googledrive/SST13_merged_raw.csv", na = c("", "NaN", "Na", "NA")) # make sure this matches your non-detects)

# DateTime at midnight is missing 00:00:00 time, so filling in using grep
SSM01$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SSM01$DateTime)] <- paste(
  SSM01$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SSM01$DateTime)],"00:00:00")
SSM20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SSM20$DateTime)] <- paste(
  SSM20$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SSM20$DateTime)],"00:00:00")
SST13$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SST13$DateTime)] <- paste(
  SST13$DateTime[grep("[0-9]{4}-[0-9]{2}-[0-9]{2}$",SST13$DateTime)],"00:00:00")

# Convert the DateTime column to POSIXct
SSM01$DateTime <- as.POSIXct(SSM01$DateTime, format = "%Y-%m-%d %H:%M:%S")
SSM20$DateTime <- as.POSIXct(SSM20$DateTime, format = "%Y-%m-%d %H:%M:%S")
SST13$DateTime <- as.POSIXct(SST13$DateTime, format = "%Y-%m-%d %H:%M:%S")

# Remove NAs from DateTime column
SSM01 <- SSM01 %>%
  filter(!is.na(DateTime))
SSM20 <- SSM20 %>%
  filter(!is.na(DateTime))
SST13 <- SST13 %>%
  filter(!is.na(DateTime))

# Rename columns by removing the X in front of the spectra (that brakes the code somehow)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}
# Apply the renaming to each data frame
SSM01 <- rename_columns(SSM01)
SSM20 <- rename_columns(SSM20)
SST13 <- rename_columns(SST13)

# Extract DOC data as time series objects (xts)
scan_DOC_SSM01 <- xts(SSM01$DOC_mg.l , order.by = SSM01$DateTime)
scan_DOC_SSM20 <- xts(SSM20$DOC_mg.l, order.by = SSM20$DateTime)
scan_DOC_SST13 <- xts(SST13$DOC_mg.l, order.by = SST13$DateTime)

# Extract spectral data (assuming spectral columns are in range "SSM200.00.nm" to "4.00.nm")
scan.specSSM01 = xts(SSM01[spec_col_idx(SSM01)], as.POSIXct(SSM01$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.specSSM20 = xts(SSM20[spec_col_idx(SSM20)], as.POSIXct(SSM20$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
scan.specSST13 = xts(SST13[spec_col_idx(SST13)], as.POSIXct(SST13$DateTime, format = "%Y-%m-%d %H:%M:%S")) 
# select full spectra
# note here that if there are 0s in your spectra, this code will throw an error
# so only use the wavelengths where you have detectable absorbance

#################################################
####  STEP 3: Compare grab and raw scan data ####
#################################################
# This is just a check to see how well the s::can did relative to your known concentrations 
# I upload this as a new data frame, just because in the previous step I had assigned these XTS values
# Feel free to change this! It's not the most efficient way to do this...
# SSM01 <- SSM01[,-1]
# SSM20 <- SSM20[,-1]
# SST13 <- SST13[,-1]

# Creating "Grab_sample" column based on values in "Sample.Name"
# Modify the Grab_sample column
SSM01 <- SSM01 %>% 
  mutate(Grab_sample = case_when(
    !is.na(Site) & Site != "" ~ "Y",  # Assign "Y" if data exists
    TRUE ~ NA_character_  # Leave as NA otherwise
  ))

SSM20 <- SSM20 %>% 
  mutate(Grab_sample = case_when(
    !is.na(Site) & Site != "" ~ "Y",
    TRUE ~ NA_character_
  ))

SST13 <- SST13 %>% 
  mutate(Grab_sample = case_when(
    !is.na(Site) & Site != "" ~ "Y",
    TRUE ~ NA_character_
  ))

#########################################################
#### Drop MDL-replaced grab samples from calibration ####
#########################################################
# UPDATED (Sep 24): now that 05_SS_merge_grabsamples_and_scan.R sources
# chem_raw_flagged.csv and averages replicates itself, each grab sample
# carries an NPOC-SPECIFIC flag (NPOC_flagged_any) instead of the old
# n_below_MDL, which counted MDL hits across every analyte (NH4, PO4,
# Cl...) and was dropping samples whose actual NPOC reading was fine. We
# only exclude on NPOC_flagged_any now. NPOC_high_rpd (replicates
# disagreeing by >20%) is a real QC concern too, but averaging vs. dropping
# a high-RPD sample is a judgment call -- it's reported via message() and
# left IN the calibration set rather than auto-dropped; revisit per-site
# if these turn out to be leverage points in the fit.
drop_mdl_replaced <- function(df, site_name) {
  # Every term here is explicitly NA-guarded so is_mdl is a clean TRUE/FALSE
  # vector with no NAs -- df$Grab_sample[is_mdl] <- ... errors ("NAs are not
  # allowed in subscripted assignments") if is_mdl itself contains NA, which
  # a plain `df$Grab_sample == "Y"` would for the ~30,000 non-grab rows.
  is_mdl <- !is.na(df$Grab_sample) & df$Grab_sample == "Y" &
    !is.na(df$NPOC_flagged_any) & df$NPOC_flagged_any
  n_mdl <- sum(is_mdl)
  if (n_mdl > 0) {
    message(sprintf("%s: dropping %d grab sample(s) with an MDL-flagged NPOC replicate from calibration.", site_name, n_mdl))
  }
  df$Grab_sample[is_mdl] <- NA_character_

  is_high_rpd <- !is.na(df$Grab_sample) & df$Grab_sample == "Y" &
    !is.na(df$NPOC_high_rpd) & df$NPOC_high_rpd
  n_high_rpd <- sum(is_high_rpd)
  if (n_high_rpd > 0) {
    message(sprintf("%s: %d grab sample(s) kept in calibration but have NPOC replicate RPD > 20%% (replicates disagreed) -- worth a manual look.", site_name, n_high_rpd))
  }

  # NEW (Sep 24): drop grab samples whose matched spectrum is flagged
  # flat_spec (abnormally low absorbance dynamic range -- see
  # flag_bad_spectra() in 05_SS_merge_grabsamples_and_scan.R). Unlike
  # NPOC_high_rpd above, this IS auto-dropped: it's independent evidence
  # the SPECTRUM itself is a bad/artifact reading (not just a hard case for
  # the model), so training on it teaches the model a false spectrum-DOC
  # relationship rather than a real one.
  is_flat_spec <- !is.na(df$Grab_sample) & df$Grab_sample == "Y" &
    !is.na(df$flat_spec) & df$flat_spec
  n_flat_spec <- sum(is_flat_spec)
  if (n_flat_spec > 0) {
    message(sprintf("%s: dropping %d grab sample(s) with an abnormally flat (likely corrupted) spectrum from calibration.", site_name, n_flat_spec))
  }
  df$Grab_sample[is_flat_spec] <- NA_character_

  # NEW (Sep 25): drop grab samples whose spectrum is flagged spec_spiky --
  # a single wavelength jumping sharply off the smooth curve the rest of
  # the spectrum follows, relative to that site's own typical grab-sample
  # spectrum (see spec_spiky in 05_SS_merge_grabsamples_and_scan.R's
  # flag_bad_spectra() for the full explanation and validation). Same
  # auto-drop treatment as flat_spec: this is independent evidence the
  # SPECTRUM is a bad reading, not just a hard case for the model. Deliberately
  # does NOT catch SST13's two high-DOC anchors (2024-11-01, 2024-11-11) --
  # that was checked directly against the data before picking the
  # threshold in flag_bad_spectra().
  is_spiky <- !is.na(df$Grab_sample) & df$Grab_sample == "Y" &
    !is.na(df$spec_spiky) & df$spec_spiky
  n_spiky <- sum(is_spiky)
  if (n_spiky > 0) {
    message(sprintf("%s: dropping %d grab sample(s) with a spiky/noisy spectrum from calibration.", site_name, n_spiky))
  }
  df$Grab_sample[is_spiky] <- NA_character_

  df
}
SSM01 <- drop_mdl_replaced(SSM01, "SSM01 (raw fingerprint)")
SSM20 <- drop_mdl_replaced(SSM20, "SSM20 (raw fingerprint)")
SST13 <- drop_mdl_replaced(SST13, "SST13 (raw fingerprint)")

# TRIED (Sep 25) and REVERTED: explicitly dropping SST13's two high-DOC
# grab samples (2024-11-01, NPOC = 63.74 mg/L; 2024-11-11, NPOC = 53.96
# mg/L -- the top two in the site's range, next-highest is ~40) from
# calibration. Neither is flagged by flat_spec/spec_spiky/MDL -- their
# spectra are clean; this was a leverage/extrapolation call, not a
# data-quality one, done because their LOO-CV predictions were unstable
# (2024-11-11 alone was driving R2 = -202 once 2024-11-01 was already
# removed). Reverted because removing them made things worse, not better:
# with no high-DOC anchor left, the model had nothing to calibrate against
# whenever the full deployment record hit a high-absorbance period, and
# the resulting predicted-timeseries plot got visibly worse (uncontrolled
# log-space extrapolation, not a stable prediction). Both points are kept
# in calibration. This tension -- these are simultaneously the only real
# high-DOC training anchors AND the most destabilizing points for LOO-CV
# with so few samples -- is exactly the kind of question flagged for Ariel
# rather than resolved by removing data.

# Filter using "Grab_sample" column
# NOTE: SSM01$Grab_sample == "Y" is NA (not FALSE) for the ~30,000 rows
# where Grab_sample is NA. Using a logical vector with NAs to subset rows
# with df[idx, ] does NOT drop the NA positions -- R keeps them as extra
# rows filled entirely with NA. So grab_SSM01/etc has always been the full
# ~30,000-row dataset (only ~8 real rows), not the small subset everyone
# assumed -- it just never showed up because lm()/plsr() silently drop
# NA rows via na.omit internally. Explicitly excluding NA here is the
# actual fix (my earlier grab.spec.dat / MDL-filter changes were correct
# but couldn't help, since grab_SSM01 itself was never actually small).
grab_SSM01 = SSM01[!is.na(SSM01$Grab_sample) & SSM01$Grab_sample == "Y",]
message(sprintf("CHECKPOINT nrow(grab_SSM01) = %d", nrow(grab_SSM01)))
grab_SSM20 = SSM20[!is.na(SSM20$Grab_sample) & SSM20$Grab_sample == "Y",]
grab_SST13 = SST13[!is.na(SST13$Grab_sample) & SST13$Grab_sample == "Y",]

##########################################################################
#### Save the complete abs record + the training abs (raw fingerprint) #
##########################################################################
# Same idea as the compensated script's version of this block: SSM01/
# SSM20/SST13 here are the full ~30,000-row raw-fingerprint deployment
# record (post drop_mdl_replaced()), grab_SSM01/SSM20/SST13 are the
# grab-matched rows actually used to train the raw-fingerprint PLSR model.
# "_raw" suffix keeps these distinct from the compensated script's output
# in the same "predicted" Drive folder.
write.csv(SSM01, "predicted/SSM01_complete_abs_record_raw.csv", row.names = FALSE)
write.csv(SSM20, "predicted/SSM20_complete_abs_record_raw.csv", row.names = FALSE)
write.csv(SST13, "predicted/SST13_complete_abs_record_raw.csv", row.names = FALSE)
write.csv(grab_SSM01, "predicted/SSM01_training_abs_raw.csv", row.names = FALSE)
write.csv(grab_SSM20, "predicted/SSM20_training_abs_raw.csv", row.names = FALSE)
write.csv(grab_SST13, "predicted/SST13_training_abs_raw.csv", row.names = FALSE)
abs_record_drive_folder_id <- "13bh64kWtdgknMUqdfDKkJ4JAzvWqLpu8"
drive_put(media = "predicted/SSM01_complete_abs_record_raw.csv", path = as_id(abs_record_drive_folder_id))
drive_put(media = "predicted/SSM20_complete_abs_record_raw.csv", path = as_id(abs_record_drive_folder_id))
drive_put(media = "predicted/SST13_complete_abs_record_raw.csv", path = as_id(abs_record_drive_folder_id))
drive_put(media = "predicted/SSM01_training_abs_raw.csv", path = as_id(abs_record_drive_folder_id))
drive_put(media = "predicted/SSM20_training_abs_raw.csv", path = as_id(abs_record_drive_folder_id))
drive_put(media = "predicted/SST13_training_abs_raw.csv", path = as_id(abs_record_drive_folder_id))

grab.DOCSSM01 = grab_SSM01$NPOC..mg.C.L.
grab.DOCSSM20 = grab_SSM20$NPOC..mg.C.L.
grab.DOCSST13 = grab_SST13$NPOC..mg.C.L.

#### remove a couple of problematic samples ####
# grab_SSM01 <- grab_SSM01 %>%
#   mutate(NPOC..mg.C.L. = ifelse(DateTime == "2025-01-02 12:15:00" | is.na(NPOC..mg.C.L.),NA,NPOC..mg.C.L.))
# grab_SSM20 <- grab_SSM20 %>%
#   mutate(NPOC..mg.C.L. = ifelse(DateTime == "2024-06-19 14:00:00" | is.na(NPOC..mg.C.L.),NA,NPOC..mg.C.L.))

# compare grab vs scan DOC 
plot(grab_SSM01$DOC_mg.l ~ grab_SSM01$NPOC..mg.C.L.)
ggplot(grab_SSM01, aes(x = NPOC..mg.C.L., y = DOC_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOCSSM01 = lm(grab_SSM01$DOC_mg.l ~ grab_SSM01$NPOC..mg.C.L.)
summary(calib.mod.DOCSSM01)

ggplot(grab_SSM20, aes(x = NPOC..mg.C.L., y = DOC_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOCSSM20 = lm(grab_SSM20$DOC_mg.l ~ grab_SSM20$NPOC..mg.C.L.)
summary(calib.mod.DOCSSM20)

ggplot(grab_SST13, aes(x = NPOC..mg.C.L., y = DOC_mg.l)) +
  geom_point(color = "blue") +
  geom_text(aes(label = DateTime), vjust = -0.5, size = 3)  # adds date labels above points
calib.mod.DOCSST13 = lm(grab_SST13$DOC_mg.l ~ grab_SST13$NPOC..mg.C.L.)
summary(calib.mod.DOCSST13)

#########################################
####  CLEANING FOR CRAZY WAVELENGTHS ####
#########################################
# filter Corrupted_No_Match FULL dataset
# 1. Identify spectral columns
spec_cols <- grep("^[0-9]", colnames(SSM01), value = TRUE)

# 2. Function to mask out SPECTRA for non-grab samples
# This keeps all 30,000 rows but turns the scan data to NA if Grab_sample isn't "Y"
mask_non_grabs <- function(df, site_name) {
  spec_cols <- grep("^[0-9]", colnames(df), value = TRUE)
  
  df_masked <- df %>%
    mutate(across(all_of(spec_cols), 
                  ~ifelse(Grab_sample == "Y", ., NA_real_))) %>%
    # Also ensure NPOC is only present when Grab_sample is Y
    mutate(NPOC_clean = ifelse(Grab_sample == "Y", NPOC..mg.C.L., NA_real_))
  
  message(paste("Site", site_name, ": Masked all rows except Grab Samples."))
  return(df_masked)
}

# 3. Apply the mask
# NOTE: SSM01_clean/SSM20_clean/SST13_clean are no longer used below --
# see the comment above grab.spec.datSSM01 for why (row-count mismatch
# with grab.DOCSSM01/etc caused a silent recycling bug). Left in place
# in case mask_non_grabs() is useful again for something else later.
SSM01_clean <- mask_non_grabs(SSM01, "SSM01 (raw fingerprint)")
SSM20_clean <- mask_non_grabs(SSM20, "SSM20 (raw fingerprint)")
SST13_clean <- mask_non_grabs(SST13, "SST13 (raw fingerprint)")

# # 4. Verify the result
# # Total rows should still be ~30,000
# nrow(grab_SSM01_full)
# 
# # Number of rows with actual spectral data should be your grab sample count (e.g., 17)
# sum(!is.na(grab_SSM01_full[[spec_cols[1]]]))

#######################################################################################
#### STEP 4: Create matrices of GRAB spectral data - this is the training data set ####
#######################################################################################
# 1. Index data set with columns with absorbances
# raw spectra
# NOTE: previously built from SSM01_clean/SSM20_clean/SST13_clean (the
# mask_non_grabs() output), which keeps ALL ~30,000 rows of the time series
# with spectra set to NA for non-grab rows -- it does NOT drop those rows.
# Combined with the much shorter grab.DOCSSM01/etc vector in grabcal.df
# below, data.frame() silently RECYCLED (tiled) the short DOC vector to
# fill 30,000+ rows. Since the real grab-sample rows sit at scattered,
# arbitrary positions in that 30,000-row series (not positions 1..N), each
# row that survived plsr()'s na.omit got paired with essentially a random
# DOC value from the recycling pattern, not its own true lab value -- a
# silent train-on-garbage bug independent of anything else fixed so far.
# Building this from grab_SSM01/etc instead (the already row-filtered
# subset used to build grab.DOCSSM01 itself) guarantees each spectrum is
# paired with its own correct DOC value, in the same order, same length.
grab.spec.datSSM01 = grab_SSM01[spec_col_idx(grab_SSM01)]
message(sprintf("CHECKPOINT dim(grab.spec.datSSM01) = %s", paste(dim(grab.spec.datSSM01), collapse=" x ")))
grab.spec.datSSM20 = grab_SSM20[spec_col_idx(grab_SSM20)]
grab.spec.datSST13 = grab_SST13[spec_col_idx(grab_SST13)]

# Rename columns for all data frames (e.g., SSM01, SSM20, SST13)
rename_columns <- function(df) {
  colnames(df) <- gsub("^X|\\.nm$", "", colnames(df))
  return(df)
}

# Apply the renaming to each data frame
grab.spec.datSSM01 <- rename_columns(grab.spec.datSSM01)
grab.spec.datSSM20 <- rename_columns(grab.spec.datSSM20)
grab.spec.datSST13 <- rename_columns(grab.spec.datSST13)

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
absSSM01 = (grab.spec.datSSM01)  # this is not doing anything and just copying grab.spec.datSSM01 again as absSSM01
message(sprintf("CHECKPOINT dim(absSSM01) after STEP4 assignment = %s", paste(dim(absSSM01), collapse=" x ")))
absSSM20 = (grab.spec.datSSM20)
absSST13 = (grab.spec.datSST13)
#str(abs)

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wlSSM01 <- gsub("_clean", "", colnames(absSSM01))   
wlSSM01 <- as.numeric(wlSSM01)
wlSSM20 <- gsub("_clean", "", colnames(absSSM20))   
wlSSM20 <- as.numeric(wlSSM20)
wlSST13 <- gsub("_clean", "", colnames(absSST13))   
wlSST13 <- as.numeric(wlSST13)
str(wlSST13)

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrowSSM01 = as.numeric(nrow(absSSM01))
NumSSM01 = c(1:lastrowSSM01)

lastrowSSM20 = as.numeric(nrow(absSSM20))
NumSSM20 = c(1:lastrowSSM20)

lastrowSST13 = as.numeric(nrow(absSST13))
NumSST13 = c(1:lastrowSST13)

# 5. Create the final matrix 
grab.matrixSSM01 = cbind(absSSM01) # this is not binding anything and just copying absSSM01 again as grab.matrixSSM01?
rownames(grab.matrixSSM01) = as.numeric(NumSSM01)
colnames(grab.matrixSSM01) = as.numeric(wlSSM01)
grab.matrixSSM01 = as.matrix(grab.matrixSSM01)
str(grab.matrixSSM01)
attributes(grab.matrixSSM01)

grab.matrixSSM20 = cbind(absSSM20)
rownames(grab.matrixSSM20) = as.numeric(NumSSM20)
colnames(grab.matrixSSM20) = as.numeric(wlSSM20)
grab.matrixSSM20 = as.matrix(grab.matrixSSM20)
str(grab.matrixSSM20)
attributes(grab.matrixSSM20)

grab.matrixSST13 = cbind(absSST13)
rownames(grab.matrixSST13) = as.numeric(NumSST13)
colnames(grab.matrixSST13) = as.numeric(wlSST13)
grab.matrixSST13 = as.matrix(grab.matrixSST13)
str(grab.matrixSST13)
attributes(grab.matrixSST13)

# 6. Make this into spectral matrix for model
# Must be in format: grab.spectra = spectra(value = abs, bands = wl, names = Num)
grab.spectraSSM01 = spectra(value = absSSM01, bands = wlSSM01, names = NumSSM01)
message(sprintf("CHECKPOINT dim(grab.spectraSSM01) right after spectra() = %s", paste(dim(grab.spectraSSM01), collapse=" x ")))
attributes(grab.spectraSSM01)
plot(grab.spectraSSM01) 

grab.spectraSSM20 = spectra(value = absSSM20, bands = wlSSM20, names = NumSSM20)
attributes(grab.spectraSSM20)
plot(grab.spectraSSM20) 

grab.spectraSST13 = spectra(value = absSST13, bands = wlSST13, names = NumSST13)
attributes(grab.spectraSST13)
plot(grab.spectraSST13) 

#grab.spectra = as_spectra.list(grab.spectra, wave_unit = "wavenumber", measurement_nit = "absorbance")
grab.spectraSSM01 = as.matrix(grab.spectraSSM01)
message(sprintf("CHECKPOINT dim(grab.spectraSSM01) after as.matrix() = %s", paste(dim(grab.spectraSSM01), collapse=" x ")))
grab.spectraSSM20 = as.matrix(grab.spectraSSM20)
grab.spectraSST13 = as.matrix(grab.spectraSST13)
#str(grab.spectra)

# Change attributes so this is correct for scan data
attr(grab.spectraSSM01, 'wave_unit') = 'wavelength'
attr(grab.spectraSSM01, 'measurement_unit') = 'absorbance'
attributes(grab.spectraSSM01)

attr(grab.spectraSSM20, 'wave_unit') = 'wavelength'
attr(grab.spectraSSM20, 'measurement_unit') = 'absorbance'
attributes(grab.spectraSSM20)

attr(grab.spectraSST13, 'wave_unit') = 'wavelength'
attr(grab.spectraSST13, 'measurement_unit') = 'absorbance'
attributes(grab.spectraSST13)

########################################################################################
#### STEP 5: Create matrices of ALL spectral data - raw data that needs calibration ####
########################################################################################
# 1. Index FULL dataset with columns with absorbances
# raw spectra
scan.specSSM01 = SSM01[spec_col_idx(SSM01)]
scan.specSSM20 = SSM20[spec_col_idx(SSM20)] 
scan.specSST13 = SST13[spec_col_idx(SST13)]

# 2. Create an absorbance matrix 
# Rows = wavelength
# Columns = date/time
absSSM01 = (scan.specSSM01)
absSSM20 = (scan.specSSM20) 
absSST13 = (scan.specSST13) 

# 3. Create a vector with wavelength labels that match the absorbance matrix columns.
wlSSM01 <- gsub("_clean", "", colnames(absSSM01))   
wlSSM01 <- as.numeric(wlSSM01)
wlSSM20 <- gsub("_clean", "", colnames(absSSM20))   
wlSSM20 <- as.numeric(wlSSM20)
wlSST13 <- gsub("_clean", "", colnames(absSST13))   
wlSST13 <- as.numeric(wlSST13)

# 4. Create a vector with sample labels that match the absorbance matrix rows. 
lastrowSSM01 = as.numeric(nrow(absSSM01))
NumSSM01 = c(1:lastrowSSM01)

lastrowSSM20 = as.numeric(nrow(absSSM20))
NumSSM20 = c(1:lastrowSSM20)

lastrowSST13 = as.numeric(nrow(absSST13))
NumSST13 = c(1:lastrowSST13)

# 5. Create the final matrix 
#SSM01
scan.matrixSSM01 = cbind(absSSM01)
rownames(scan.matrixSSM01) = as.numeric(NumSSM01)
colnames(scan.matrixSSM01) = as.numeric(wlSSM01)

scan.matrixSSM01 = as.matrix(scan.matrixSSM01)
specSSM01 = spectra(value = absSSM01, bands = wlSSM01, names = NumSSM01)
plot(specSSM01) # Note = reflectance here = absorbance from the scans

#SSM20
scan.matrixSSM20 = cbind(absSSM20)
rownames(scan.matrixSSM20) = as.numeric(NumSSM20)
colnames(scan.matrixSSM20) = as.numeric(wlSSM20)

scan.matrixSSM20 = as.matrix(scan.matrixSSM20)
specSSM20 = spectra(value = absSSM20, bands = wlSSM20, names = NumSSM20)
plot(specSSM20) # Note = reflectance here = absorbance from the scans

#SST13
scan.matrixSST13 = cbind(absSST13)
rownames(scan.matrixSST13) = as.numeric(NumSST13)
colnames(scan.matrixSST13) = as.numeric(wlSST13)

scan.matrixSST13 = as.matrix(scan.matrixSST13)
specSST13 = spectra(value = absSST13, bands = wlSST13, names = NumSST13)
plot(specSST13) # Note = reflectance here = absorbance from the scans

# NOTE: this is where you can identify problem spectra & remove them

# = as.spectra.list(spec)
scan.spectraSSM01 = as.matrix(specSSM01)
str(scan.spectraSSM01)
attr(scan.spectraSSM01, 'wave_unit') = 'wavelength'
attr(scan.spectraSSM01, 'measurement_unit') = 'absorbance'
attributes(scan.spectraSSM01)

scan.spectraSSM20 = as.matrix(specSSM20)
str(scan.spectraSSM20)
attr(scan.spectraSSM20, 'wave_unit') = 'wavelength'
attr(scan.spectraSSM20, 'measurement_unit') = 'absorbance'
attributes(scan.spectraSSM20)

scan.spectraSST13 = as.matrix(specSST13)
str(scan.spectraSST13)
attr(scan.spectraSST13, 'wave_unit') = 'wavelength'
attr(scan.spectraSST13, 'measurement_unit') = 'absorbance'
attributes(scan.spectraSST13)

####################################################################
#### STEP 6: Create a new data frame with the spectral matrices ####
####################################################################
# This creates a data frame with 
# 1. DOC (scan)
# 3. Full s::can spectra (from 2SSM20-750nm)
length(scan_DOC_SSM01)
dim(scan.spectraSSM01) 
class(scan.spectraSSM01)

# NOTE: We use the I() function to protect the Spectra 
spectralcal.dfSSM01 = data.frame(DOCSSM01 = scan_DOC_SSM01, SpectraSSM01 = I(scan.spectraSSM01))
str(spectralcal.dfSSM01)

spectralcal.dfSSM20 = data.frame(DOCSSM20 = scan_DOC_SSM20, SpectraSSM20 = I(scan.spectraSSM20))
str(spectralcal.dfSSM20)

spectralcal.dfSST13 = data.frame(DOCSST13 = scan_DOC_SST13, SpectraSST13 = I(scan.spectraSST13))
str(spectralcal.dfSST13)

# Also do this for the GRAB sample data
message(sprintf("CHECKPOINT length(grab.DOCSSM01) = %d, dim(grab.spectraSSM01) just before data.frame() = %s", length(grab.DOCSSM01), paste(dim(grab.spectraSSM01), collapse=" x ")))
grabcal.dfSSM01 = data.frame(DOCSSM01 = grab.DOCSSM01, SpectraSSM01 = I(grab.spectraSSM01))
message(sprintf("CHECKPOINT nrow(grabcal.dfSSM01) right after data.frame() = %d", nrow(grabcal.dfSSM01)))
str(grabcal.dfSSM01)

grabcal.dfSSM20 = data.frame(DOCSSM20 = grab.DOCSSM20, SpectraSSM20 = I(grab.spectraSSM20))
str(grabcal.dfSSM20)

grabcal.dfSST13 = data.frame(DOCSST13 = grab.DOCSST13, SpectraSST13 = I(grab.spectraSST13))
str(grabcal.dfSST13)

# Carry the sample's own Date along (not used for fitting -- just so we can
# later identify WHICH calendar date a bad prediction/outlier belongs to,
# instead of only knowing "row 7 of 13").
grabcal.dfSSM01$Date <- grab_SSM01$Date
grabcal.dfSSM20$Date <- grab_SSM20$Date
grabcal.dfSST13$Date <- grab_SST13$Date

##########################################################################
#### Log-transform DOC before fitting -- DOC error is typically        ####
#### multiplicative (bigger absolute error at higher concentration),   ####
#### so an untransformed PLSR fit lets a few higher-DOC samples        ####
#### dominate the loss and the low end gets fit poorly in relative     ####
#### terms. We fit on log(DOC) and back-transform predictions with     ####
#### exp() everywhere they're used below. No storm/high-leverage       ####
#### samples were identified for these SS sites, so nothing is being   ####
#### filtered out here -- this only changes the fitting scale.         ####
##########################################################################
# log() of a zero or negative DOC value is -Inf/NaN, which would silently
# corrupt the fit rather than erroring cleanly -- check for that specifically
# before transforming. NA lab values are left alone: plsr()'s default
# na.omit already drops NA-response rows when fitting, same as it did
# before this log-transform (e.g. samples below the NPOC detection limit).
check_log_domain <- function(x, label) {
  non_na <- x[!is.na(x)]
  if (any(non_na <= 0)) {
    stop(sprintf("%s has %d zero/negative value(s) -- log-transform is undefined for these.", label, sum(non_na <= 0)))
  }
  n_na <- sum(is.na(x))
  if (n_na > 0) {
    message(sprintf("%s: %d NA value(s) among grab samples -- these rows are dropped when fitting, as before.", label, n_na))
  }
}
check_log_domain(grabcal.dfSSM01$DOCSSM01, "grabcal.dfSSM01$DOCSSM01")
check_log_domain(grabcal.dfSSM20$DOCSSM20, "grabcal.dfSSM20$DOCSSM20")
check_log_domain(grabcal.dfSST13$DOCSST13, "grabcal.dfSST13$DOCSST13")

grabcal.dfSSM01$DOCSSM01_log <- log(grabcal.dfSSM01$DOCSSM01)
grabcal.dfSSM20$DOCSSM20_log <- log(grabcal.dfSSM20$DOCSSM20)
grabcal.dfSST13$DOCSST13_log <- log(grabcal.dfSST13$DOCSST13)

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# Create a training and test data set
# Carbon
CTrainSSM01 = grabcal.dfSSM01
CTestSSM01 = spectralcal.dfSSM01

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
# NOTE: fit on log(DOC) now -- DOCSSM01_log, not DOCSSM01 -- see the
# log-transform block above. RMSEP/comps below are therefore in log(mg/L)
# units, which is what you want for picking ncomp (it's the scale the
# model is actually optimizing), but isn't directly readable in mg/L --
# see the *_comps_natural.png plot below for that.
CmodSSM01 = plsr(DOCSSM01_log ~ SpectraSSM01, ncomp = 10, data = CTrainSSM01, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(CmodSSM01) # optimized for 4 components

# Plot RMSE of the predictions to optimize model (log(mg/L) scale)
plot(RMSEP(CmodSSM01), legendpos = "topright")
save_plot("SSM01_raw_rmse.png", plot(RMSEP(CmodSSM01), legendpos = "topright"))

# Plot predicted vs. measured from optimized model (log(mg/L) scale)
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples 
plot(CmodSSM01, ncomp = 5, asp = 1, line = TRUE,
     main = "DOCSSM01 (raw), 1 comps, validation (log scale)")
save_plot("SSM01_raw_comps.png", plot(CmodSSM01, ncomp = 5, asp = 1, line = TRUE,
     main = "DOCSSM01 (raw), 1 comps, validation (log scale)"))

# Same predicted-vs-measured comparison, but back-transformed to mg/L so
# it's actually readable and comparable to the pre-log-transform version.
# Uses the LOO-CV predictions already computed during fitting (not a second
# fit), at the same ncomp as the plot above.
# LOO-CV predictions (Cmod*$validation$pred) only cover the rows plsr()
# actually used to fit. If any grab sample's spectrum has an NA band (or
# its log-DOC is NA), plsr()'s default na.omit silently drops that row
# before fitting -- but grabcal.df* still has the full row count, since
# nothing upstream removed it. Building "measured" straight from
# grabcal.df* without accounting for that is what caused "arguments imply
# differing number of rows: 25, 21": 25 grab samples went into
# CTrainSSM01, only 21 survived na.omit and have a validation$pred row.
# This pulls out exactly the rows the model actually used, in the same
# order, so measured/predicted always line up regardless of how many rows
# get dropped.
cv_measured <- function(response_vec, model) {
  omitted <- if (!is.null(model$na.action)) as.integer(model$na.action) else integer(0)
  if (length(omitted) > 0) response_vec[-omitted] else response_vec
}

# NEW (Sep 25): fit-quality numbers (R2, Pearson r, RMSE, MAE) for the
# LOO-CV predicted-vs-measured comparison, on the natural (mg/L) scale --
# same scale as the *_comps_natural.png plots, since that's what's
# actually meaningful/reportable. R2 here is 1 - SS_res/SS_tot against the
# 1:1 line (how PLSR's own R2 is defined for a validation set), NOT the
# R2 of a separate best-fit regression line through the points -- so a
# systematic bias (not just scatter) also lowers it, which is the right
# behavior for a calibration check.
report_fit_stats <- function(df, site_name) {
  ok <- stats::complete.cases(df$measured, df$predicted)
  m <- df$measured[ok]
  p <- df$predicted[ok]
  resid <- p - m
  ss_res <- sum(resid^2)
  ss_tot <- sum((m - mean(m))^2)
  r2 <- 1 - ss_res / ss_tot
  r <- suppressWarnings(cor(m, p))
  rmse <- sqrt(mean(resid^2))
  mae <- mean(abs(resid))
  message(sprintf(
    "%s LOO-CV fit (mg/L, n=%d): R2 = %.3f, r = %.3f, RMSE = %.2f, MAE = %.2f",
    site_name, length(m), r2, r, rmse, mae
  ))
  invisible(list(r2 = r2, r = r, rmse = rmse, mae = mae, n = length(m)))
}

ssm01_cv_natural <- data.frame(
  Date      = cv_measured(grabcal.dfSSM01$Date, CmodSSM01),
  measured  = cv_measured(grabcal.dfSSM01$DOCSSM01, CmodSSM01),
  predicted = exp(as.numeric(CmodSSM01$validation$pred[, 1, "5 comps"]))
)
# Which specific grab dates are behind the worst LOO-CV predictions --
# check these against NPOC_high_rpd/bad_spec before dropping anything;
# don't drop a point just because the model predicted it badly (that's
# circular -- it removes whatever the model finds hardest, not
# necessarily a genuinely bad sample).
print(ssm01_cv_natural[order(-abs(ssm01_cv_natural$predicted - ssm01_cv_natural$measured)), ])
report_fit_stats(ssm01_cv_natural, "SSM01 (raw fingerprint)")
save_plot("SSM01_raw_comps_natural.png", ggplot(ssm01_cv_natural, aes(x = measured, y = predicted)) +
  geom_point() +
  geom_text(aes(label = Date), vjust = -0.6, size = 2.6, color = "grey30") +  # grab-sample date next to each point, per Ariel-deck request
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "DOCSSM01 (raw), 5 comps, LOO-CV (log-fit, back-transformed to mg/L)",
       x = "measured (mg/L)", y = "predicted (mg/L)") +
  theme_minimal())

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model! Model was fit on log(DOC), so back-transform with exp()
# to get predictions back in mg/L -- everything downstream (plot, CSV)
# expects mg/L, same as before the log-transform.
predictedCSSM01_log = predict(CmodSSM01, ncomp = 5, newdata = spectralcal.dfSSM01) # use reduced error model
predictedCSSM01 = exp(predictedCSSM01_log)
str(predictedCSSM01)
plot(predictedCSSM01)
save_plot("SSM01_raw_pred.png", plot(predictedCSSM01))

write.csv(predictedCSSM01, file = "predicted/PredictedC_SSM01_raw.csv") # <- this is your newly calibrated dataset!

## NOTE: If your s::can has significant drift (e.g., which often happens when there is biofouling), 
# You might need to use a moving window approach to the calibraiton (i.e., calibrate 1 month at a time)

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# Create a training and test dataset
# Carbon
CTrainSSM20 = grabcal.dfSSM20
CTestSSM20 = spectralcal.dfSSM20

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
# NOTE: fit on log(DOC) -- DOCSSM20_log -- see the log-transform block above.
CmodSSM20 = plsr(DOCSSM20_log ~ SpectraSSM20, ncomp = 12, data = CTrainSSM20, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(CmodSSM20) # optimized for 4 components

# Plot RMSE of the predictions to optimize model (log(mg/L) scale)
plot(RMSEP(CmodSSM20), legendpos = "topright")
save_plot("SSM20_raw_rmse.png", plot(RMSEP(CmodSSM20), legendpos = "topright"))

# Plot predicted vs. measured from optimized model (log(mg/L) scale)
# Pick the number of components with the least error (in this case, x)
# NOTE: This plot may be messy, given low number of grab samples 
plot(CmodSSM20, ncomp = 1, asp = 1, line = TRUE,
     main = "DOCSSM20 (raw), 1 comps, validation (log scale)")
save_plot("SSM20_raw_comps.png", plot(CmodSSM20, ncomp = 1, asp = 1, line = TRUE,
     main = "DOCSSM20 (raw), 1 comps, validation (log scale)"))

# Back-transformed (mg/L) version of the same LOO-CV comparison
ssm20_cv_natural <- data.frame(
  Date      = cv_measured(grabcal.dfSSM20$Date, CmodSSM20),
  measured  = cv_measured(grabcal.dfSSM20$DOCSSM20, CmodSSM20),
  predicted = exp(as.numeric(CmodSSM20$validation$pred[, 1, "1 comps"]))
)
print(ssm20_cv_natural[order(-abs(ssm20_cv_natural$predicted - ssm20_cv_natural$measured)), ])
report_fit_stats(ssm20_cv_natural, "SSM20 (raw fingerprint)")
save_plot("SSM20_raw_comps_natural.png", ggplot(ssm20_cv_natural, aes(x = measured, y = predicted)) +
  geom_point() +
  geom_text(aes(label = Date), vjust = -0.6, size = 2.6, color = "grey30") +  # grab-sample date next to each point, per Ariel-deck request
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "DOCSSM20 (raw), 1 comps, LOO-CV (log-fit, back-transformed to mg/L)",
       x = "measured (mg/L)", y = "predicted (mg/L)") +
  theme_minimal())

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model! Back-transform with exp() -- model was fit on log(DOC).
predictedCSSM20_log = predict(CmodSSM20, ncomp = 1, newdata = spectralcal.dfSSM20) # use reduced error model
predictedCSSM20 = exp(predictedCSSM20_log)
str(predictedCSSM20)
# Plot final predictions
plot(predictedCSSM20)
save_plot("SSM20_raw_pred.png", plot(predictedCSSM20))

write.csv(predictedCSSM20, file = "predicted/PredictedC_SSM20_raw.csv") # <- this is your newly calibrated dataset!

#################################################
#### STEP 7: Develop PLSR training data sets ####
#################################################
# Create a training and test dataset
# Carbon
CTrainSST13 = grabcal.dfSST13
CTestSST13 = spectralcal.dfSST13

# PLSR Model with "training" data, use # of grab samples - 1
# LOO = Leave One Out cross-comparison
# NOTE: fit on log(DOC) -- DOCSST13_log -- see the log-transform block above.
CmodSST13 = plsr(DOCSST13_log ~ SpectraSST13, ncomp = 11, data = CTrainSST13, validation = "LOO") # usually ncomp is N-1 grab samples you have
summary(CmodSST13) # optimized for 4 components

# Plot RMSE of the predictions to optimize model (log(mg/L) scale)
plot(RMSEP(CmodSST13), legendpos = "topright")
save_plot("SST13_raw_rmse.png", plot(RMSEP(CmodSST13), legendpos = "topright"))

# Plot predicted vs. measured from optimized model (log(mg/L) scale)
# Pick the number of components with the least error
# NOTE: This plot may be messy, given low number of grab samples 
plot(CmodSST13, ncomp = 8, asp = 1, line = TRUE,
     main = "DOCSST13 (raw), 4 comps, validation (log scale)")
save_plot("SST13_raw_comps.png", plot(CmodSST13, ncomp = 1, asp = 1, line = TRUE,
     main = "DOCSST13 (raw), 4 comps, validation (log scale)"))

# Back-transformed (mg/L) version of the same LOO-CV comparison
sst13_cv_natural <- data.frame(
  Date      = cv_measured(grabcal.dfSST13$Date, CmodSST13),
  measured  = cv_measured(grabcal.dfSST13$DOCSST13, CmodSST13),
  predicted = exp(as.numeric(CmodSST13$validation$pred[, 1, "8 comps"]))
)
print(sst13_cv_natural[order(-abs(sst13_cv_natural$predicted - sst13_cv_natural$measured)), ])
report_fit_stats(sst13_cv_natural, "SST13 (raw fingerprint)")
save_plot("SST13_raw_comps_natural.png", ggplot(sst13_cv_natural, aes(x = measured, y = predicted)) +
  geom_point() +
  geom_text(aes(label = Date), vjust = -0.6, size = 2.6, color = "grey30") +  # grab-sample date next to each point, per Ariel-deck request
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "DOCSST13 (raw), 8 comps, LOO-CV (log-fit, back-transformed to mg/L)",
       x = "measured (mg/L)", y = "predicted (mg/L)") +
  theme_minimal())

####################################################################
#### STEP 8: Make predictions based on reduced-error PLSR model #### 
####################################################################
# Predict model! Back-transform with exp() -- model was fit on log(DOC).
predictedCSST13_log = predict(CmodSST13, ncomp = 8, newdata = spectralcal.dfSST13) # use reduced error model
predictedCSST13 = exp(predictedCSST13_log)
str(predictedCSST13)
# Plot -- for display only, exclude physically-implausible predictions
# (order of 1e100+ mg/L) caused by the log-space PLSR model extrapolating
# on a scan spectrum well outside the training range: exp() of a large
# log-scale error blows up to an absurd value and squashes the rest of
# the plot flat. 1000 mg/L is a generous ceiling -- no South Sandy DOC
# reading has ever approached it, so this only catches that blow-up, not
# a real high-DOC event. PredictedC_SST13_raw.csv below still gets the
# FULL, unfiltered prediction record -- nothing is dropped from the
# actual output, only from this one figure.
n_implausible <- sum(predictedCSST13 > 1000, na.rm = TRUE)
if (n_implausible > 0) {
  message(sprintf("SST13: excluding %d implausible predicted value(s) (>1000 mg/L, extrapolation artifact) from the plot only -- PredictedC_SST13_raw.csv still has the full record.", n_implausible))
}
predictedCSST13_plot <- predictedCSST13
predictedCSST13_plot[predictedCSST13_plot > 1000] <- NA
plot(predictedCSST13_plot)
save_plot("SST13_raw_pred.png", plot(predictedCSST13_plot))

write.csv(predictedCSST13, file = "predicted/PredictedC_SST13_raw.csv") # <- this is your newly calibrated dataset! (full record, unfiltered)

# 1. Loadings Plot for SSM01 (Opposite Trend)
# This shows how the wavelengths contribute to each component (ncomp = 1, 2, 3, etc.)
save_plot("SSM01_raw_loadings.png", plot(CmodSSM01, plottype = "loading",
     comps = 1:2, # Plot the first two components for initial inspection
     main = "SSM01 NO3-N PLSR Loadings"))

# 2. Loadings Plot for SSM20 (Flat Trend)
save_plot("SSM20_raw_loadings.png", plot(CmodSSM20, plottype = "loading",
     comps = 1:2, # Plot the first two components
     main = "SSM20 NO3-N PLSR Loadings"))

# 3. Loadings Plot for SST13 (Flat Trend)
# Examine the first few components for SST13
save_plot("SST13_raw_loadings.png", plot(CmodSST13, plottype = "loading",
     comps = 1:2, # Plot the first two components
     main = "SST13 NO3-N PLSR Loadings"))

# Convert predictedCSST13 to a data frame
pred_df13 <- data.frame(
  DateTime = as.POSIXct(dimnames(predictedCSST13)[[1]]),
  Predicted = as.numeric(predictedCSST13))
pred_df01 <- data.frame(
  DateTime = as.POSIXct(dimnames(predictedCSSM01)[[1]]),
  Predicted = as.numeric(predictedCSSM01))
pred_df20 <- data.frame(
  DateTime = as.POSIXct(dimnames(predictedCSSM20)[[1]]),
  Predicted = as.numeric(predictedCSSM20))
# Plot -- same implausible-value exclusion as SST13_pred.png above, for
# the same reason (this is a different view of the identical prediction
# vector). Only affects this figure; pred_df itself keeps every row.
pred_df_plot01 <- pred_df01
pred_df_plot01$Predicted[pred_df_plot01$Predicted > 1000] <- NA
save_plot("SSM01_predicted_timeseries_raw.png", ggplot(pred_df_plot01, aes(x = DateTime, y = Predicted)) +
            geom_point(color = "steelblue") +
            labs(
              x = "DateTime",
              y = "Predicted DOC (mg/L)",
              title = "Predicted DOC over Time (SSM01)"
            ) +
            theme_minimal())
pred_df_plot13 <- pred_df13
pred_df_plot13$Predicted[pred_df_plot13$Predicted > 1000] <- NA
save_plot("SST13_predicted_timeseries_raw.png", ggplot(pred_df_plot13, aes(x = DateTime, y = Predicted)) +
            geom_point(color = "steelblue") +
            labs(
              x = "DateTime",
              y = "Predicted DOC (mg/L)",
              title = "Predicted DOC over Time (SST13)"
            ) +
            theme_minimal())

pred_df_plot20 <- pred_df20
pred_df_plot20$Predicted[pred_df_plot20$Predicted > 1000] <- NA
save_plot("SSM20_predicted_timeseries_raw.png", ggplot(pred_df_plot20, aes(x = DateTime, y = Predicted)) +
            geom_point(color = "steelblue") +
            labs(
              x = "DateTime",
              y = "Predicted DOC (mg/L)",
              title = "Predicted DOC over Time (SSM20)"
            ) +
            theme_minimal())

#######################
#### Save in Drive #### 
#######################
# Define the target folder ID in Google Drive
# This is the "predicted" folder
drive_folder_id <- "13bh64kWtdgknMUqdfDKkJ4JAzvWqLpu8"

# Upload the file to the specified Google Drive folder
drive_put(media = "predicted/PredictedC_SSM01_raw.csv", path = as_id(drive_folder_id))
drive_put(media = "predicted/PredictedC_SSM20_raw.csv", path = as_id(drive_folder_id))
drive_put(media = "predicted/PredictedC_SST13_raw.csv", path = as_id(drive_folder_id))
