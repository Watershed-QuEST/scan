##==============================================================================
## Project: QuEST
## Here we will prep grab sample data by matching the grab samples with the time stamp of the s::can for Dog Valley
##==============================================================================

library(googledrive)
library(openxlsx)
library(dplyr)
library(readxl)
library(tidyverse)
library(lubridate)
library(hms) # to fix time from xlsx

# Where diagnostic figures from this script get saved
fig_dir <- "scan_figs"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

########################################
#### Clear folders that we will use ####
########################################
# List and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

##########################
#### Import chem data ####
##########################
#### Load chem data ####
# Chem data is for all the sites 
chem <- googledrive::as_id("https://drive.google.com/drive/folders/1yNoGf43hkNDC5TX7wMWc27PYvcRD0vul")

# List all CSV files in the folder
chem_csv <- googledrive::drive_ls(path = chem, type = "csv")

# the RAW (not pre-averaged), per-analyte flagged export -- 
# multiple rows per Date+Site (one per lab replicate R1/R2/R3,
# further split into separate "DOC" and "Chem" lab submissions), each
# analyte column paired with its own "<analyte>_flag" column (TRUE = that
# specific replicate's value for that specific analyte was below the
# method detection limit and substituted). We average replicates ourselves
# below, analyte-by-analyte.
googledrive::drive_download(file = chem_csv$id[chem_csv$name=="chem_raw_flagged.csv"],
                            path = "googledrive/chem_raw_flagged.csv",
                            overwrite = T)
# load it into R
wqual_raw <- read.csv("googledrive/chem_raw_flagged.csv")

# Format date columns
wqual_raw$Date <- as.Date(wqual_raw$Date, format = "%Y-%m-%d")

##############################################################################
#### Collapse raw per-replicate chem rows to one row per Date + Site      ####
##############################################################################
# analytes to collapse
analyte_cols <- c("NPOC..mg.C.L.", "NO3..mg.N.L.", "NH4..ug.N.L.", "TDN..mg.N.L.",
                   "PO4..ug.P.L.", "Cl..mg.Cl.L.", "SO4..mg.S.L.", "Na..mg.Na.L.",
                   "K..mg.K.L.", "Mg..mg.Mg.L.", "Ca..mg.Ca.L.")

# Averages ONE analyte's replicate values within one Date+Site group,
# dropping any replicate flagged as below-MDL (or NA) for THAT analyte
# before averaging. Returns NA if nothing usable is left.
average_one_analyte <- function(vals, flags) {
  keep <- !is.na(vals) & !(is.na(flags) | flags)
  if (!any(keep)) return(NA_real_)
  mean(vals[keep])
}

# Relative percent difference across an analyte's usable (non-NA) replicate
# values within a group -- a large RPD means the replicates disagreed with
# each other, a real QC signal independent of MDL substitution. NA when
# there are fewer than 2 usable values to compare.
rpd_pct <- function(vals) {
  v <- vals[!is.na(vals)]
  if (length(v) < 2) return(NA_real_)
  100 * (max(v) - min(v)) / mean(v)
}

# Filter only for Nevada samples
DV <- wqual_raw %>%
  filter(Sub_Project == "Nevada", Site %in% c("DVO", "DVMS1", "DVNWT5")) %>%
  group_by(Date, Site) %>%
  group_modify(function(g, keys) {
    out <- list()
    for (col in analyte_cols) {
      flag_col <- paste0(col, "_flag")
      out[[col]] <- average_one_analyte(g[[col]], g[[flag_col]])
    }
    npoc <- g[["NPOC..mg.C.L."]]
    npoc_flag <- g[["NPOC..mg.C.L._flag"]]
    out$NPOC_n_reps <- sum(!is.na(npoc))
    # TRUE if ANY replicate actually used for this Date+Site's NPOC average
    # was below-MDL-flagged -- this is what the DOC calibration step should
    # filter on (NPOC_flagged_any > 0 / == TRUE)
    out$NPOC_flagged_any <- any(npoc_flag[!is.na(npoc)], na.rm = TRUE)
    out$NPOC_rpd_pct <- rpd_pct(npoc)
    # group_modify() strips the grouping columns (Date, Site) out of g --
    # they only get reattached to the result afterward, from the group key.
    # Pull them from `keys` (the group's key, passed in as the 2nd arg)
    # instead of g$Site/g$Date, which are NULL inside this function.
    out$Sample.Name <- paste0(keys$Site, "_", keys$Date, "_Avg")
    tibble::as_tibble(out)
  }) %>%
  ungroup() %>%
  mutate(NPOC_high_rpd = !is.na(NPOC_rpd_pct) & NPOC_rpd_pct > 20)

message(sprintf(
  "DV chem: %d Date x Site sample(s) after averaging replicates -- %d with an MDL-flagged NPOC replicate, %d with NPOC replicate RPD > 20%% (replicates disagreed).",
  nrow(DV), sum(DV$NPOC_flagged_any, na.rm = TRUE), sum(DV$NPOC_high_rpd, na.rm = TRUE)
))

# Kept as "data_avg" so the rest of this script (unchanged below) still works
# against the same variable name the old single-snapshot version produced.
data_avg <- DV

#############################################################
#### Load sample info to get grab sample collection time ####
#############################################################
# Set the Google Sheets file ID
file_id <- "https://docs.google.com/spreadsheets/d/1f4iH0JrE9bNU3SSsXhK-gk3yzFa_k70TaJQ1BOPd3mk/edit?gid=0#gid=0"

# Download the Google Sheet as an Excel file (.xlsx)
drive_download(as_id(file_id), 
               path = "googledrive/All Site Information and Notes", 
               type = "xlsx", 
               overwrite = TRUE)

# Read the Excel file
times <- read_excel("googledrive/All Site Information and Notes.xlsx")

# Fix date
str(times$Date)  # Check structure
class(times$Date)  # Check specific class

# readxl's column-type guessing samples the sheet and picks ONE type for
# the whole Date column -- if the source Google Sheet has inconsistent
# cell formatting across rows (some cells actually formatted as dates in
# Sheets, some as plain numbers/general), readxl can guess "numeric" for
# the entire column instead of "date", and every value comes through as a
# raw Excel serial number (days since 1899-12-30) -- e.g. 45509 instead of
# 2024-08-20. Converts that back to an actual Date; leaves it alone if
# readxl already parsed it correctly as a Date.
if (is.numeric(times$Date)) {
  message(sprintf(
    "times$Date came through as numeric Excel serial dates (e.g. %s) -- converting with origin 1899-12-30.",
    paste(head(times$Date), collapse = ", ")
  ))
  times$Date <- as.Date(times$Date, origin = "1899-12-30")
}

# Fix time
times <- times %>%
  mutate(Time = convertToDateTime(`Time Arrived (if applicable)`),
         # Extract only the Time part and store it as a character string
         Time = format(Time, format = "%H:%M:%S"))

#### This is if you want to change sample time to fit scan time ####
###DVO###
# times$Start_Time[times$`Site ID` == "DVO" &
#                    times$Date == "2024-05-23" &
#                    times$Start_Time == "09:30:00"] <- "09:45:00"

##################################
#### Rounding collection time ####
##################################
# Combine Date and Time columns into a new DateTime column
times$DateTime <- paste(times$Date, times$Time, sep = " ")
# Convert the DateTime column to POSIXct
times$DateTime <- as.POSIXct(times$DateTime, format = "%Y-%m-%d %H:%M")

# Round DateTime to the nearest 15-minute interval 
times$DateTime <- round_date(times$DateTime, unit="15 mins")

# Check if it worked!
str(times)

# filter only data for DVO, DVMS1 and DVNW5 (scan sites) 
times <- times %>% filter(Site %in% c("DVO", "DVMS1", "DVNWT5"))

#######################################################################
#### Merge chem and sample log sheet to get sample collection time ####
#######################################################################
# filter only data for DVO, DVMS1 and DVNW5 (scan sites) 
wqual_scans <- data_avg %>% filter(Site %in% c("DVO", "DVMS1", "DVNWT5"))

# merge wqual data first
sample_times <- merge(wqual_scans, times, by = c("Date", "Site"))

# Check for duplicates in the original datasets
sum(duplicated(sample_times))

# Remove duplicates from the original datasets
# sample_times <- sample_times %>% distinct()

#write.csv(sample_times, "sample_times_DV.csv")

##########################
#### Import scan data ####
##########################
#### Import abs and parameter data ####
# This is the "merged" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1gAfaUZKCoarEaSrPnhdUXy5lQ49Z_xm1")

# List all the files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")

#DVO
googledrive::drive_download(file = merged$id[merged$name=="02_DVO_absparams.csv"], 
                            path = "googledrive/02_DVO_absparams.csv",
                            overwrite = T)
#DVNWT5
googledrive::drive_download(file = merged$id[merged$name=="02_DVNWT5_absparams.csv"], 
                            path = "googledrive/02_DVNWT5_absparams.csv",
                            overwrite = T)
#DVMS1
googledrive::drive_download(file = merged$id[merged$name=="02_DVMS1_absparams.csv"], 
                            path = "googledrive/02_DVMS1_absparams.csv",
                            overwrite = T)

# Load them separately 
DVO <- read.csv("googledrive/02_DVO_absparams.csv")
DVNWT5 <- read.csv("googledrive/02_DVNWT5_absparams.csv")
DVMS1 <- read.csv("googledrive/02_DVMS1_absparams.csv")

# Convert the DateTime column to POSIXct
DVO$DateTime <- as.POSIXct(DVO$DateTime, format = "%Y-%m-%d %H:%M")
DVNWT5$DateTime <- as.POSIXct(DVNWT5$DateTime, format = "%Y-%m-%d %H:%M")
DVMS1$DateTime <- as.POSIXct(DVMS1$DateTime, format = "%Y-%m-%d %H:%M")

# Check for duplicates
sum(duplicated(DVO))
sum(duplicated(DVNWT5))
sum(duplicated(DVMS1))

##################################
#### Merge chem and scan data ####
##################################
# Filter to get just one site at a time
UO <- filter(sample_times, Site == "DVO")
UNWT5 <- filter(sample_times, Site == "DVNWT5")
UMS1 <- filter(sample_times, Site == "DVMS1")

# First check if the merge works
datO <- merge(DVO, UO, by = "DateTime")
datNWT5 <- merge(DVNWT5, UNWT5, by = "DateTime")
datMS1 <- merge(DVMS1, UMS1, by = "DateTime")

# scan data first - perform a left join
dataO <- merge(DVO, UO, by = "DateTime", all.x = TRUE)
dataNWT5 <- merge(DVNWT5, UNWT5, by = "DateTime", all.x = TRUE)
dataMS1 <- merge(DVMS1, UMS1, by = "DateTime", all.x = TRUE)

# Check for duplicates in the original datasets
sum(duplicated(dataO))
sum(duplicated(dataNWT5))
sum(duplicated(dataMS1))

##################
#### Clean up ####
##################
# NOTE: switched from select(-c(...)) to select(-any_of(...)). The old
# single-snapshot chem file came through read.csv() and left behind "X.x"/
# "X.y" row-index columns from the merge; chem_raw_flagged.csv -> the new
# DV tibble above doesn't have an "X" column, so those names may no longer
# exist after the merge. select(-c(...)) errors on a missing column name;
# any_of() just skips it if it's not there -- drops whichever of these
# columns are actually present, same intent either way.
drop_cols <- c("X.x", "X.y", "Time Arrived (if applicable)", "Time Sensor(s) Were Cleaned (removed from water)",
               "Time Sensor(s) Were Cleaned (returned to water)", "Activites Done at Site", "Stage Depth (m)",
               "Actual Depth (m)", "Site Width (m)", "Height of Water Column from Ground (m)",
               "Height of L.L. Bolt from Ground (m)", "Height of L.L. Bolt from water column (m)",
               "* Height when Water Column is Above L.L. Bolt (m) *", "Other Notes about Site", "X725.00.nm")

dataO <- dataO %>% dplyr::select(-dplyr::any_of(drop_cols))
dataNWT5 <- dataNWT5 %>% dplyr::select(-dplyr::any_of(drop_cols))
dataMS1 <- dataMS1 %>% dplyr::select(-dplyr::any_of(drop_cols))

########################################
#### Clear corrupted spectral times ####
########################################
# Same idea as 05_SS_merge_grabsamples_and_scan.R's equivalent step: strip
# any leading "X" that read.csv added to wavelength column names
# (e.g. "X200.00.nm" -> "200.00.nm"), and force spectral columns to numeric
# in case the merge above turned any of them back into character.
#
# SS's version ALSO masks spectral values to NA where Status ==
# "Corrupted_No_Match" and sets a ReadyForCalibration flag from that same
# Status column. DV doesn't have that: 00_DV_merge_timestamps.R never built
# the Good/Corrupted/Replaced Status flagging 00_SS_merge_timestamps.R has,
# so DVO/DVNWT5/DVMS1's absparams.csv has no Status column to key off --
# there's nothing to mask against here. If you want that duplicate-timestamp
# QC for DV too, it would need to be added to 00_DV_merge_timestamps.R
# first (happy to help with that separately); skipping it here rather than
# silently doing nothing or erroring on a missing column.
dfs <- c("dataO", "dataNWT5", "dataMS1")

for (df_name in dfs) {
  if (exists(df_name)) {
    df <- get(df_name)
    colnames(df) <- gsub("^X([0-9])", "\\1", colnames(df))
    spec_cols <- grep("^[0-9]", colnames(df))
    df <- df %>%
      mutate(across(all_of(spec_cols), ~as.numeric(as.character(.))))
    assign(df_name, df)
  }
}

#################################################
#### Clean up spectra, very low or high rows ####
#################################################
# Same bug class SS's version flags in its own history: the old DV version
# of this step selected spectral columns by hardcoded POSITION (c(15:99)).
# That's fragile -- it silently misaligns if any non-spectral column gets
# added or removed upstream (which just happened: the chem side above now
# carries extra columns like NPOC_flagged_any/NPOC_rpd_pct that the old
# single-snapshot chem file didn't have). Select spectral columns by NAME
# instead, so this can't drift out of alignment again.
is_wl_col <- function(nm) grepl("^X?[0-9]+\\.[0-9]+\\.nm$", nm)

filter_extreme_spectra <- function(df, lo = -1, hi = 60) {
  spec_cols <- names(df)[is_wl_col(names(df))]
  dplyr::filter(df, !if_any(dplyr::all_of(spec_cols), ~ . < lo | . > hi))
}

# Kept DV's original -1/60 bounds (SS uses -3/60) -- not changing the
# threshold itself, just how the columns it applies to are selected.
dataO_clean <- filter_extreme_spectra(dataO, lo = -1, hi = 60)
dataNWT5_clean <- filter_extreme_spectra(dataNWT5, lo = -1, hi = 60)
dataMS1_clean <- filter_extreme_spectra(dataMS1, lo = -1, hi = 60)

##############################################
#### Flag flat/spiky grab-matched spectra ####
##############################################
# Same flag_bad_spectra() as 05_SS_merge_grabsamples_and_scan.R, unchanged:
# flags spectra that are wildly out of range (bad_spec), abnormally FLAT
# relative to this site's other grab-matched spectra (flat_spec), or spiky/
# noisy relative to a local moving average (spec_spiky). See that script's
# comments for the full reasoning behind each threshold -- not re-derived
# here, since the logic itself isn't DV-specific, only the data it runs on.
flag_bad_spectra <- function(df, lo = -20, hi = 70, min_range = 5) {
  spec_cols <- names(df)[is_wl_col(names(df))]
  spec <- as.list(df[, spec_cols])
  df$spec_min <- do.call(pmin, c(spec, list(na.rm = TRUE)))
  df$spec_max <- do.call(pmax, c(spec, list(na.rm = TRUE)))
  df$bad_spec <- df$spec_min < lo | df$spec_max > hi
  df$spec_range <- df$spec_max - df$spec_min
  df$flat_spec <- df$spec_range < min_range

  wl_val <- as.numeric(gsub("^X|\\.nm$", "", spec_cols))
  spec_ord <- spec[order(wl_val)]
  spec_mat <- as.matrix(as.data.frame(spec_ord))
  window <- 5
  half <- window %/% 2
  padded <- cbind(
    matrix(spec_mat[, 1], nrow = nrow(spec_mat), ncol = half),
    spec_mat,
    matrix(spec_mat[, ncol(spec_mat)], nrow = nrow(spec_mat), ncol = half)
  )
  ma <- matrix(NA_real_, nrow = nrow(spec_mat), ncol = ncol(spec_mat))
  for (j in seq_len(ncol(spec_mat))) {
    ma[, j] <- rowMeans(padded[, j:(j + window - 1), drop = FALSE], na.rm = TRUE)
  }
  dev <- abs(spec_mat - ma)
  spec_max_dev <- do.call(pmax, c(as.data.frame(dev), list(na.rm = TRUE)))
  df$spec_rel_dev <- ifelse(df$spec_range > 0, spec_max_dev / df$spec_range, NA_real_)

  is_grab <- !is.na(df$Site)
  rel_dev_median <- median(df$spec_rel_dev[is_grab], na.rm = TRUE)
  df$spec_spiky <- is_grab & !is.na(df$spec_rel_dev) & df$spec_rel_dev > (rel_dev_median * 2.5)
  if (any(df$spec_spiky)) {
    message(sprintf("flag_bad_spectra: %d grab-matched spectrum/spectra flagged as spiky/noisy (relative deviation > 2.5x this site's grab-sample median).", sum(df$spec_spiky)))
  }

  df
}

plot_bad_spectra <- function(df, site_label) {
  ggplot(df, aes(x = DateTime, y = spec_min, color = bad_spec)) +
    geom_point(size = 0.5, alpha = 0.5) +
    scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "red")) +
    labs(title = paste0(site_label, ": minimum absorbance value over time"),
         subtitle = "Red = spectra flagged as bad (min < -20)",
         x = "Date", y = "Minimum absorbance across all bands") +
    theme_minimal()
}

dataO_clean <- flag_bad_spectra(dataO_clean)
ggsave(file.path(fig_dir, "DVO_abs_over_time.png"), plot_bad_spectra(dataO_clean, "DVO"), width = 9, height = 6, dpi = 120)

dataNWT5_clean <- flag_bad_spectra(dataNWT5_clean)
ggsave(file.path(fig_dir, "DVNWT5_abs_over_time.png"), plot_bad_spectra(dataNWT5_clean, "DVNWT5"), width = 9, height = 6, dpi = 120)

dataMS1_clean <- flag_bad_spectra(dataMS1_clean)
ggsave(file.path(fig_dir, "DVMS1_abs_over_time.png"), plot_bad_spectra(dataMS1_clean, "DVMS1"), width = 9, height = 6, dpi = 120)

##########################################################
#### Drop spiky scans from the FULL deployment record ####
##########################################################
# Same filter_spiky_scans() as SS's script, unchanged. IMPORTANT, read
# before using: for SS, this was only applied to SSM20 after checking it
# was SAFE there (dropped a small %, didn't touch any grab sample) and
# confirming it was NOT safe for SSM01/SST13 (dropped 16-20%, including a
# high-DOC calibration anchor for SST13) -- so it was deliberately left
# OFF for those two sites. That safety check needs real DV data to run,
# which isn't available from here. The three calls below are DRY RUNS ONLY
# (assigned to *_dryrun, not *_clean) -- they just print the % that would
# be dropped and whether it touches any grab-matched DateTime for THIS
# site. Read that message() output for each site, then uncomment only the
# site(s) where it's safe by the same standard used for SSM20 (low % drop,
# no grab samples touched) -- same way that decision was made for SS.
filter_spiky_scans <- function(df, site_name, factor = 3, window = 5) {
  spec_cols <- names(df)[is_wl_col(names(df))]
  wl_val <- as.numeric(gsub("^X|\\.nm$", "", spec_cols))
  spec_ord_cols <- spec_cols[order(wl_val)]
  spec_mat <- as.matrix(df[, spec_ord_cols])

  half <- window %/% 2
  padded <- cbind(
    matrix(spec_mat[, 1], nrow = nrow(spec_mat), ncol = half),
    spec_mat,
    matrix(spec_mat[, ncol(spec_mat)], nrow = nrow(spec_mat), ncol = half)
  )
  ma <- matrix(NA_real_, nrow = nrow(spec_mat), ncol = ncol(spec_mat))
  for (j in seq_len(ncol(spec_mat))) {
    ma[, j] <- rowMeans(padded[, j:(j + window - 1), drop = FALSE], na.rm = TRUE)
  }
  dev <- abs(spec_mat - ma)
  max_dev <- do.call(pmax, c(as.data.frame(dev), list(na.rm = TRUE)))
  rel_dev <- ifelse(df$spec_range > 0, max_dev / df$spec_range, NA_real_)

  threshold <- median(rel_dev, na.rm = TRUE) * factor
  flagged <- !is.na(rel_dev) & rel_dev > threshold
  n_dropped <- sum(flagged)
  n_grab_touched <- sum(flagged & !is.na(df$Site))
  message(sprintf(
    "%s: filter_spiky_scans would drop %d scan(s) (%.1f%% of %d) at %gx full-record median relative deviation -- %d of those are grab-matched rows (%s to use at this site).",
    site_name, n_dropped, 100 * n_dropped / nrow(df), nrow(df), factor, n_grab_touched,
    if (n_grab_touched > 0) "NOT SAFE" else "safe so far, but still check the % dropped"
  ))
  df[!flagged, , drop = FALSE]
}

dataO_dryrun     <- filter_spiky_scans(dataO_clean, "DVO", factor = 3)
dataNWT5_dryrun  <- filter_spiky_scans(dataNWT5_clean, "DVNWT5", factor = 3)
dataMS1_dryrun   <- filter_spiky_scans(dataMS1_clean, "DVMS1", factor = 3)

# Once you've checked the message() output above for a site and it's safe
# to apply, swap that site's *_clean assignment to use the *_dryrun result,
# e.g.:
#   dataO_clean <- dataO_dryrun

############################
#### Save matched files ####
############################
# Save the new data frame to a CSV file
write.csv(dataO_clean,"googledrive/DVO_chem.csv" , row.names=FALSE, quote=FALSE)
# Save the new data frame to a CSV file
write.csv(dataNWT5_clean,"googledrive/DVNWT5_chem.csv" , row.names=FALSE, quote=FALSE)
# Save the new data frame to a CSV file
write.csv(dataMS1_clean,"googledrive/DVMS1_chem.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "with chem" folder
drive_folder_id <- "12N6uUxXTttdnadDrn43mL6ilz-Ui2eQX"

# Upload the file to the specified Google Drive folder
drive_put(media = "googledrive/DVO_chem.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/DVNWT5_chem.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/DVMS1_chem.csv", path = as_id(drive_folder_id))

