##==============================================================================
## Project: QuEST
## Here we will prep grab sample data by matching the grab samples with the time stamp of the s::can for South Sandy
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

library(googledrive) 
library(googlesheets4)
library(dplyr)
library(readxl)
library(tidyverse)
library(lubridate) 

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
2

# call the specific file you want (most recent one)
# Here we do the replicate-averaging ourselves below
googledrive::drive_download(file = chem_csv$id[chem_csv$name=="chem_raw_flagged.csv"], 
                            path = "googledrive/chem_raw_flagged.csv",
                            overwrite = T)
# load it into R
wqual_raw = read.csv("googledrive/chem_raw_flagged.csv")

# Format date columns
wqual_raw$Date <- as.Date(wqual_raw$Date, format = "%Y-%m-%d")

#########################################################################
#### Collapse raw per-replicate chem rows to one row per Date + Site ####
#########################################################################
# Everything downstream (the merge with samplelogsheet, then with the scan
# spectra) expects exactly one chem row per Date+Site, same as
# chem_avg_flagged.csv used to provide. We rebuild that here, but keep
# control over exactly how each analyte gets averaged and flagged.
analyte_cols <- c("NPOC..mg.C.L.", "TDN..mg.N.L.", "NH4..ug.N.L.", "PO4..ug.P.L.",
                   "Cl..mg.Cl.L.", "NO3..mg.N.L.", "SO4..mg.S.L.", "Na..mg.Na.L.",
                   "K..mg.K.L.", "Mg..mg.Mg.L.", "Ca..mg.Ca.L.")

# Averages ONE analyte's replicate values within one Date+Site group,
# dropping any replicate flagged as below-MDL (or NA) for THAT analyte
# before averaging. Returns NA if nothing usable is left -- same as the
# analyte just not being measured.
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
# Filter to only South Sandy sites
SS <- wqual_raw %>%
  filter(Sub_Project == "Alabama", Site %in% c("SSM01", "SSM20", "SST13")) %>%
  # Groups by Site and Date and collects the groups summary into one, 
  # averaging values per solute
  group_by(Date, Site) %>%
  group_modify(function(g, keys) {
    out <- list()
    for (col in analyte_cols) {
      flag_col <- paste0(col, "_flag")
      out[[col]] <- average_one_analyte(g[[col]], g[[flag_col]])
    }
    # Pull out raw NPOC (≈ DOC) replicate values and their MDL flags specifically
    npoc <- g[["NPOC..mg.C.L."]]
    npoc_flag <- g[["NPOC..mg.C.L._flag"]]
    out$NPOC_n_reps <- sum(!is.na(npoc))
    # NPOC MDL flag
    out$NPOC_flagged_any <- any(npoc_flag[!is.na(npoc)], na.rm = TRUE)
    out$NPOC_rpd_pct <- rpd_pct(npoc)
    # Reference only -- these are the ORIGINAL file's group-level stats,
    # summed/OR'd across every row (all analytes, all replicates) in this
    # Date+Site group. Not what the DOC calibration should filter on; see
    # NPOC_flagged_any / NPOC_high_rpd below for the NPOC-specific version.
    out$n_below_MDL <- sum(g$n_below_MDL, na.rm = TRUE)
    out$n_reps <- nrow(g)
    out$high_rpd_any <- any(g$high_rpd_any, na.rm = TRUE)
    out$Project <- g$Project[1]
    tibble::as_tibble(out)
  }) %>%
  # Drop the Date/Site grouping
  ungroup() %>%
  mutate(NPOC_high_rpd = !is.na(NPOC_rpd_pct) & NPOC_rpd_pct > 20)

message(sprintf(
  "SS chem: %d Date x Site sample(s) after averaging replicates -- %d with an MDL-flagged NPOC replicate, %d with NPOC replicate RPD > 20%% (replicates disagreed).",
  nrow(SS), sum(SS$NPOC_flagged_any, na.rm = TRUE), sum(SS$NPOC_high_rpd, na.rm = TRUE)
))

#### Load sample info to get grab sample collection time ####
samplelogsheet <- drive_get("https://docs.google.com/spreadsheets/d/1JVDwzSoHetQGHhYPoeoTOHzlmRrcWNPs-i5b8t2U754/edit?gid=2118868541#gid=2118868541")
# Download spreadsheet from Webster Lab Sample Log Sheet
drive_download(as_id(samplelogsheet$id), path = "googledrive/samplelogsheet.xlsx", overwrite = T)

# Fetch the file
samplelogsheet <- readxl::read_excel("googledrive/samplelogsheet.xlsx", sheet = "YSI", skip = 1)

# Format date and time columns
samplelogsheet$Date <- as.Date(samplelogsheet$`Collection Date`, format = "%Y-%m-%d")
# Format Arrival time since itis me tmost complete Time record
samplelogsheet$`Arrival Time` <- as.POSIXct(
  as.numeric(samplelogsheet$`Arrival Time`) * 86400,
  origin = "1970-01-01",
  tz = "UTC"
) 
# then pull just the time-of-day if you don't want a date attached:
samplelogsheet$Time <- format(samplelogsheet$`Arrival Time`, "%H:%M:%S")

# Clean up a bit
drops <- c("Sampling Time", "Crew Initials", "Pressure (mmHg)", "Dissolved O2 (%sat)", "pH", "Dissolved O2 (mg/L)",
           "Scanned?", "Picture?", "Weather", "Arrival Time")
samplelogsheet <- samplelogsheet[ , !(names(samplelogsheet) %in% drops)]

##################################
#### Rounding collection time ####
##################################
# Combine Date and Time columns into a new DateTime column
samplelogsheet$DateTime <- paste(samplelogsheet$Date, samplelogsheet$Time, sep = " ")
# Convert the DateTime column to POSIXct
samplelogsheet$DateTime <- as.POSIXct(samplelogsheet$DateTime, format = "%Y-%m-%d %H:%M:%S")

# Round DateTime to the nearest 15-minute interval 
samplelogsheet$DateTime <- round_date(samplelogsheet$DateTime, unit="15 mins")

# Check if it worked!
str(samplelogsheet)

#######################################################################
#### Merge chem and sample log sheet to get sample collection time ####
#######################################################################
# filter only data for SSM20, SST13 and SSM01 (scan sites) 
wqual_scans <- SS %>% filter(Site %in% c("SSM01", "SSM20", "SST13"))

# merge wqual data first
sample_times <- merge(wqual_scans, samplelogsheet, by = c("Date", "Site"))

##########################
#### Import scan data ####
##########################
#### Import abs and parameter data ####
# This is the "params and abs" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1BNCKA7LdysjDH5_REI4WhH_P0Z4FIe0r")

# List all the files in the folder
merged <- googledrive::drive_ls(path = scan, type = "csv")

#SSM01
googledrive::drive_download(file = merged$id[merged$name=="SSM01_absparams_clean.csv"], 
                            path = "googledrive/SSM01_absparams_clean.csv",
                            overwrite = T)
#SSM20
googledrive::drive_download(file = merged$id[merged$name=="SSM20_absparams_clean.csv"], 
                            path = "googledrive/SSM20_absparams_clean.csv",
                            overwrite = T)
#SST13
googledrive::drive_download(file = merged$id[merged$name=="SST13_absparams_clean.csv"], 
                            path = "googledrive/SST13_absparams_clean.csv",
                            overwrite = T)

# Load them separately 
SSM01 <- read.csv("googledrive/SSM01_absparams_clean.csv")
SSM20 <- read.csv("googledrive/SSM20_absparams_clean.csv")
SST13 <- read.csv("googledrive/SST13_absparams_clean.csv")

# Convert the DateTime column to POSIXct
SSM01$DateTime <- as.POSIXct(SSM01$DateTime, format = "%Y-%m-%d %H:%M:%S")
SSM20$DateTime <- as.POSIXct(SSM20$DateTime, format = "%Y-%m-%d %H:%M:%S")
SST13$DateTime <- as.POSIXct(SST13$DateTime, format = "%Y-%m-%d %H:%M:%S")

# --- Drop rows with no usable DateTime before merging ---
drop_na_datetime <- function(df, label) {
  bad <- is.na(df$DateTime)
  if (any(bad)) {
    message(sprintf("%s: dropping %d row(s) with missing/unparseable DateTime before merge.", label, sum(bad)))
  }
  df[!bad, , drop = FALSE]
}
SSM01 <- drop_na_datetime(SSM01, "SSM01")
SSM20 <- drop_na_datetime(SSM20, "SSM20")
SST13 <- drop_na_datetime(SST13, "SST13")

# Check for duplicates
sum(duplicated(SSM01))
sum(duplicated(SSM20))
sum(duplicated(SST13))

##################################
#### Merge chem and scan data ####
##################################
# Filter to get just one site at a time
# Also drop any grab-sample row with a missing DateTime here for the same
# reason as above -- merge() would otherwise match it against every
# NA-DateTime row left on the scan side (and vice versa).
U01 <- filter(sample_times, Site == "SSM01", !is.na(DateTime))
U20 <- filter(sample_times, Site == "SSM20", !is.na(DateTime))
U13 <- filter(sample_times, Site == "SST13", !is.na(DateTime))

# First check if the merge works
dat01 <- merge(SSM01, U01, by = "DateTime")
dat20 <- merge(SSM20, U20, by = "DateTime")
dat13 <- merge(SST13, U13, by = "DateTime")

# scan data first - perform a left join
data01 <- merge(SSM01, U01, by = "DateTime", all.x = TRUE)
data20 <- merge(SSM20, U20, by = "DateTime", all.x = TRUE)
data13 <- merge(SST13, U13, by = "DateTime", all.x = TRUE)

# Check for duplicates in the original datasets
sum(duplicated(data01))
sum(duplicated(data20))
sum(duplicated(data13))

########################################
#### Clear corrupted spectral times ####
########################################
# List of your final dataframes
dfs <- c("data01", "data20", "data13")

for (df_name in dfs) {
  if (exists(df_name)) {
    df <- get(df_name)
    
    # Identify columns starting with X and a digit
    # This avoids accidentally renaming columns like "X" (if it's an ID)
    colnames(df) <- gsub("^X([0-9])", "\\1", colnames(df))
    
    # Force the spectral data to be numeric 
    # (In case the merge turned them back into characters)
    spec_cols <- grep("^[0-9]", colnames(df))
    df <- df %>%
      mutate(across(all_of(spec_cols), ~as.numeric(as.character(.))))
    
    assign(df_name, df)
  }
}

# 1. Define a list of your merged datasets
merged_list <- list(data01 = data01, data20 = data20, data13 = data13)

# 2. Process each to handle corrupted data
cleaned_data_list <- lapply(merged_list, function(df) {
  
  # Identify spectral columns
  spec_cols <- grep("^[0-9]", colnames(df), value = TRUE)
  
  df_clean <- df %>%
    # Masking with NA
    # If Status is 'Corrupted_No_Match', we turn the spectral data into NA
    mutate(across(all_of(spec_cols), 
                  ~ifelse(Status == "Corrupted_No_Match", NA, .))) %>%
    
    # Add a flag for your calibration step
    # This makes it easy to filter only good paired samples later
    mutate(ReadyForCalibration = ifelse(!is.na(Site) & Status != "Corrupted_No_Match", 
                                        TRUE, FALSE))
  
  return(df_clean)
})

# 3. Bring them back to the environment
data01_final <- cleaned_data_list$data01
data20_final <- cleaned_data_list$data20
data13_final <- cleaned_data_list$data13

# 4. QUICK CHECK: How many calibration points did we lose?
# (Where we had a grab sample but the scan was corrupted)
sum(data13_final$Status == "Corrupted_No_Match" & !is.na(data13_final$Site))
sum(data20_final$Status == "Corrupted_No_Match" & !is.na(data20_final$Site))
sum(data01_final$Status == "Corrupted_No_Match" & !is.na(data01_final$Site))

#################################################
#### Clean up spectra, very low or high rows ####
#################################################
# Select spectral columns by name -- matches with or without a
# leading "X" since this step runs after the earlier "Clear corrupted
# spectral times" section already strips it.
is_wl_col <- function(nm) grepl("^X?[0-9]+\\.[0-9]+\\.nm$", nm)

filter_extreme_spectra <- function(df, lo = -3, hi = 60) {
  spec_cols <- names(df)[is_wl_col(names(df))]
  dplyr::filter(df, !if_any(dplyr::all_of(spec_cols), ~ . < lo | . > hi))
}

data01_clean <- filter_extreme_spectra(data01_final, lo = -3, hi = 60)
data20_clean <- filter_extreme_spectra(data20_final, lo = -3, hi = 60)
data13_clean <- filter_extreme_spectra(data13_final, lo = -3, hi = 60)

# Flag bad spectra. Check the FULL retained spectrum (200-450nm)
flag_bad_spectra <- function(df, lo = -20, hi = 70, min_range = 5) {
  spec_cols <- names(df)[is_wl_col(names(df))]
  # pmin()/pmax() across columns are vectorized (C-level) -- much faster
  # than apply(spec, 1, min/max) row-by-row over a ~100-column, many-row
  # spectrum, which is what was making this step slow.
  spec <- as.list(df[, spec_cols])
  df$spec_min <- do.call(pmin, c(spec, list(na.rm = TRUE)))
  df$spec_max <- do.call(pmax, c(spec, list(na.rm = TRUE)))
  df$bad_spec <- df$spec_min < lo | df$spec_max > hi
  # Find spectrum that's abnormally FLAT
  df$spec_range <- df$spec_max - df$spec_min
  df$flat_spec <- df$spec_range < min_range

  df
}

# plot minimum absorbance values over time for each scan
plot_bad_spectra <- function(df, site_label) {
  ggplot(df, aes(x = DateTime, y = spec_min, color = bad_spec)) +
    geom_point(size = 0.5, alpha = 0.5) +
    scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "red")) +
    labs(title = paste0(site_label, ": minimum absorbance value over time"),
         subtitle = "Red = spectra flagged as bad (min < -20)",
         x = "Date", y = "Minimum absorbance across all bands") +
    theme_minimal()
}

# SSM01
data01_clean <- flag_bad_spectra(data01_clean)
print(plot_bad_spectra(data01_clean, "SSM01"))
ggsave(file.path(fig_dir, "SSM01_abs_over_time.png"), plot_bad_spectra(data01_clean, "SSM01"), width = 9, height = 6, dpi = 120)
#SST13
data13_clean <- flag_bad_spectra(data13_clean)
print(plot_bad_spectra(data13_clean, "SST13"))
ggsave(file.path(fig_dir, "SST13_abs_over_time.png"), plot_bad_spectra(data13_clean, "SST13"), width = 9, height = 6, dpi = 120)
#SSM20
data20_clean <- flag_bad_spectra(data20_clean)
print(plot_bad_spectra(data20_clean, "SSM20"))
ggsave(file.path(fig_dir, "SSM20_abs_over_time.png"), plot_bad_spectra(data20_clean, "SSM20"), width = 9, height = 6, dpi = 120)

############################
#### Save matched files ####
############################
# Save the new data frame to a CSV file
write.csv(data01_clean,"googledrive/SSM01_merged.csv" , row.names=FALSE, quote=FALSE)
# Save the new data frame to a CSV file
write.csv(data20_clean,"googledrive/SSM20_merged.csv" , row.names=FALSE, quote=FALSE)
# Save the new data frame to a CSV file
write.csv(data13_clean,"googledrive/SST13_merged.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "with grab" folders
drive_folder_id <- "1Wju54VbyACZ_RFtfeInSvBCiVDKFScGj"

# Upload the file to the specified Google Drive folder
drive_put(media = "googledrive/SSM01_merged.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/SSM20_merged.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/SST13_merged.csv", path = as_id(drive_folder_id))

#############################################################
#### RAW (uncompensated) fingerprint version -- (Sep 28) ####
#############################################################
# Same merge/clean/QC pipeline as above, applied to the raw (uncompensated)
# fingerprint instead of the compensated one -- testing Arial's suggestion
# that the default turbidity compensation may be overcorrecting and removing
# real DOC-related spectral signal. Reuses the exact same helper functions
# defined above (filter_extreme_spectra, flag_bad_spectra, plot_bad_spectra,
# filter_spiky_scans).

#### Download the raw-fingerprint clean files (same "clean" Drive folder) ####
googledrive::drive_download(file = merged$id[merged$name=="SSM01_absparams_raw_clean.csv"],
                            path = "googledrive/SSM01_absparams_raw_clean.csv",
                            overwrite = T)
googledrive::drive_download(file = merged$id[merged$name=="SSM20_absparams_raw_clean.csv"],
                            path = "googledrive/SSM20_absparams_raw_clean.csv",
                            overwrite = T)
googledrive::drive_download(file = merged$id[merged$name=="SST13_absparams_raw_clean.csv"],
                            path = "googledrive/SST13_absparams_raw_clean.csv",
                            overwrite = T)

SSM01_raw <- read.csv("googledrive/SSM01_absparams_raw_clean.csv")
SSM20_raw <- read.csv("googledrive/SSM20_absparams_raw_clean.csv")
SST13_raw <- read.csv("googledrive/SST13_absparams_raw_clean.csv")

SSM01_raw$DateTime <- as.POSIXct(SSM01_raw$DateTime, format = "%Y-%m-%d %H:%M:%S")
SSM20_raw$DateTime <- as.POSIXct(SSM20_raw$DateTime, format = "%Y-%m-%d %H:%M:%S")
SST13_raw$DateTime <- as.POSIXct(SST13_raw$DateTime, format = "%Y-%m-%d %H:%M:%S")

SSM01_raw <- drop_na_datetime(SSM01_raw, "SSM01_raw")
SSM20_raw <- drop_na_datetime(SSM20_raw, "SSM20_raw")
SST13_raw <- drop_na_datetime(SST13_raw, "SST13_raw")

#### Merge chem and raw-fingerprint scan data (same U01/U20/U13 grab-chem data -- the lab measurements don't depend on which spectrum we're testing) ####
data01_raw <- merge(SSM01_raw, U01, by = "DateTime", all.x = TRUE)
data20_raw <- merge(SSM20_raw, U20, by = "DateTime", all.x = TRUE)
data13_raw <- merge(SST13_raw, U13, by = "DateTime", all.x = TRUE)

#### Clear corrupted spectral times (same logic as the compensated "dfs" loop above) ####
raw_dfs <- c("data01_raw", "data20_raw", "data13_raw")
for (df_name in raw_dfs) {
  df <- get(df_name)
  colnames(df) <- gsub("^X([0-9])", "\\1", colnames(df))
  spec_cols <- grep("^[0-9]", colnames(df))
  df <- df %>%
    mutate(across(all_of(spec_cols), ~as.numeric(as.character(.))))
  assign(df_name, df)
}

merged_list_raw <- list(data01_raw = data01_raw, data20_raw = data20_raw, data13_raw = data13_raw)
cleaned_data_list_raw <- lapply(merged_list_raw, function(df) {
  spec_cols <- grep("^[0-9]", colnames(df), value = TRUE)
  df %>%
    mutate(across(all_of(spec_cols), ~ifelse(Status == "Corrupted_No_Match", NA, .))) %>%
    mutate(ReadyForCalibration = ifelse(!is.na(Site) & Status != "Corrupted_No_Match", TRUE, FALSE))
})

data01_raw_final <- cleaned_data_list_raw$data01_raw
data20_raw_final <- cleaned_data_list_raw$data20_raw
data13_raw_final <- cleaned_data_list_raw$data13_raw

#### Clean up spectra, very low or high rows (same filter_extreme_spectra() defined above) ####
data01_raw_clean <- filter_extreme_spectra(data01_raw_final, lo = -3, hi = 60)
data20_raw_clean <- filter_extreme_spectra(data20_raw_final, lo = -3, hi = 60)
data13_raw_clean <- filter_extreme_spectra(data13_raw_final, lo = -3, hi = 60)

#### Flag flat/spiky grab-matched spectra (same flag_bad_spectra() defined above) ####
data01_raw_clean <- flag_bad_spectra(data01_raw_clean)
print(plot_bad_spectra(data01_raw_clean, "SSM01 (raw fingerprint)"))
ggsave(file.path(fig_dir, "SSM01_raw_abs_over_time.png"), plot_bad_spectra(data01_raw_clean, "SSM01 (raw fingerprint)"), width = 9, height = 6, dpi = 120)

data13_raw_clean <- flag_bad_spectra(data13_raw_clean)
print(plot_bad_spectra(data13_raw_clean, "SST13 (raw fingerprint)"))
ggsave(file.path(fig_dir, "SST13_raw_abs_over_time.png"), plot_bad_spectra(data13_raw_clean, "SST13 (raw fingerprint)"), width = 9, height = 6, dpi = 120)

data20_raw_clean <- flag_bad_spectra(data20_raw_clean)
print(plot_bad_spectra(data20_raw_clean, "SSM20 (raw fingerprint)"))
ggsave(file.path(fig_dir, "SSM20_raw_abs_over_time.png"), plot_bad_spectra(data20_raw_clean, "SSM20 (raw fingerprint)"), width = 9, height = 6, dpi = 120)

#### Drop spiky scans from the FULL deployment record (SSM20 only, same factor=3 rule as the compensated version -- see filter_spiky_scans() comment above for why this is SSM20-specific) ####
data20_raw_clean <- filter_spiky_scans(data20_raw_clean, "SSM20 (raw fingerprint)", factor = 3)

##################################
#### Save matched files (raw) ####
##################################
data01_raw_clean$DateTime <- format(data01_raw_clean$DateTime, "%Y-%m-%d %H:%M:%S")
write.csv(data01_raw_clean, "googledrive/SSM01_merged_raw.csv", row.names = FALSE, quote = FALSE)
data20_raw_clean$DateTime <- format(data20_raw_clean$DateTime, "%Y-%m-%d %H:%M:%S")
write.csv(data20_raw_clean, "googledrive/SSM20_merged_raw.csv", row.names = FALSE, quote = FALSE)
data13_raw_clean$DateTime <- format(data13_raw_clean$DateTime, "%Y-%m-%d %H:%M:%S")
write.csv(data13_raw_clean, "googledrive/SST13_merged_raw.csv", row.names = FALSE, quote = FALSE)

drive_put(media = "googledrive/SSM01_merged_raw.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/SSM20_merged_raw.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/SST13_merged_raw.csv", path = as_id(drive_folder_id))

