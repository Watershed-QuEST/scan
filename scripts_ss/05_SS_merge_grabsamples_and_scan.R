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

# Where diagnostic figures from this script get saved -- same convention
# as 06_SS_calibrate_DOC.R's fig_dir/save_plot(). plot_bad_spectra()'s
# output was only ever print()ed before (visible in RStudio's Plots pane
# during an interactive run, but never written to disk), which is why
# these absorbance-over-time figures didn't show up anywhere afterward.
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
# NOTE (Sep 24): switched from "chem_avg_flagged.csv" to
# "chem_raw_flagged.csv". chem_avg_flagged.csv was already averaged across
# replicates upstream of this script, with a single "n_below_MDL" count
# per row -- but that count is summed ACROSS ALL ANALYTES (NH4, PO4, Cl,
# etc.), not just NPOC/DOC. 06_SS_calibrate_DOC.R was using
# "n_below_MDL > 0" to drop grab samples from the DOC calibration, which
# meant a sample got dropped if e.g. its NH4 reading was below detection,
# even when its actual NPOC/DOC value was a perfectly good measurement --
# this was needlessly cutting the already-small SS calibration set roughly
# in half. chem_raw_flagged.csv is the RAW (not yet averaged), per-analyte
# flagged export: multiple rows per Date+Site (one per lab replicate
# R1/R2/R3, further split into separate "DOC" and "Chem" lab submissions),
# each analyte column paired with its own "<analyte>_flag" column (TRUE =
# that specific replicate's value for that specific analyte was below the
# method detection limit and substituted). We do the replicate-averaging
# ourselves below, analyte-by-analyte, so the DOC calibration's MDL
# exclusion can be based on NPOC's own flag instead of a blanket
# any-analyte count.
googledrive::drive_download(file = chem_csv$id[chem_csv$name=="chem_raw_flagged.csv"], 
                            path = "googledrive/chem_raw_flagged.csv",
                            overwrite = T)
# load it into R
wqual_raw = read.csv("googledrive/chem_raw_flagged.csv")

# Format date columns
wqual_raw$Date <- as.Date(wqual_raw$Date, format = "%Y-%m-%d")

##############################################################################
#### Collapse raw per-replicate chem rows to one row per Date + Site      ####
##############################################################################
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

SS <- wqual_raw %>%
  filter(Sub_Project == "Alabama", Site %in% c("SSM01", "SSM20", "SST13")) %>%
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

# #### Change sample time to fit scan time ####
# ###USF12###
# samplelogsheet$Time[samplelogsheet$Site == "USF12" &
#                       samplelogsheet$Date == "2024-05-23" &
#                       samplelogsheet$Time == "09:30:00"] <- "09:45:00"

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

# Some sample log entries are EXACT duplicates -- e.g. 2025-07-28 has the
# identical log row twice for both SSM01 and SST13 (same site/date/time,
# just pasted in twice). Merging on Date+Site below would otherwise
# multiply the matching chem row once per duplicate log entry, producing
# duplicate output rows. Drop exact duplicates from the log sheet itself,
# before merging, instead of trying to clean it up after the fact.
n_before <- nrow(samplelogsheet)
samplelogsheet <- samplelogsheet %>% distinct()
if (nrow(samplelogsheet) < n_before) {
  message(sprintf("Removed %d exact duplicate row(s) from samplelogsheet.", n_before - nrow(samplelogsheet)))
}

# merge wqual data first
sample_times <- merge(wqual_scans, samplelogsheet, by = c("Date", "Site"))

# With samplelogsheet deduplicated above, this should normally be 0. If
# it's not, there's more than one DISTINCT sample-log entry for some
# Date+Site (e.g. two genuinely different samples collected the same day)
# -- that needs a closer look, not a silent hardcoded row deletion.
n_dup <- sum(duplicated(sample_times))
if (n_dup > 0) {
  message(sprintf(
    "%d duplicate row(s) remain in sample_times after merging -- check samplelogsheet for multiple distinct entries on the same Date+Site.",
    n_dup
  ))
}
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
# *_absparams_clean.csv has (upstream) rows with a missing or garbled
# DateTime (in some cases a Status value like "Replaced_Good" ended up in
# the DateTime field instead of a timestamp). Those all become NA here.
# base R's merge() treats NA as a matching join key -- so if SSM01 has,
# say, 500 rows with DateTime == NA and U01 has even one row with
# DateTime == NA, merge(..., by = "DateTime") cross-multiplies them into
# ~500 bogus output rows. That is almost certainly the source of the
# duplicate/inflated grab-sample row counts. Drop unmatched-DateTime rows
# from the scan data before merging so NA can't be used as a join key.
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
# Same class of bug as 02_SS_clean.R and NM's equivalent script: this used
# to select spectral columns by hardcoded position (c(18:126) / c(18:129)).
# Tracing those positions through the actual merged data (spectra + grab
# chem columns appended after) showed two real problems: the range started
# one column too late, so the first wavelength (200.00.nm) was never
# checked, and it ran ~9-12 columns too far, so it was applying the numeric
# "< -3 | > 60" check to the Status column, the datetime column, and several
# appended chem columns (Site name, NPOC, etc.) that aren't spectral at all.
# Select spectral columns by name instead -- matches with or without a
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

# Flag bad spectra. This used to check only a hardcoded, arbitrary slice
# (columns 25:39, ~220-255nm) with a comment saying to "adapt column indices
# to your data" -- there's no evidence that was ever tuned per site, and a
# narrow fixed slice doesn't make sense once sites can have different
# non-spectral column counts ahead of the spectral range. Check the FULL
# retained spectrum (200-450nm) instead, consistent with the filter above.
flag_bad_spectra <- function(df, lo = -20, hi = 70, min_range = 5) {
  spec_cols <- names(df)[is_wl_col(names(df))]
  # pmin()/pmax() across columns are vectorized (C-level) -- much faster
  # than apply(spec, 1, min/max) row-by-row over a ~100-column, many-row
  # spectrum, which is what was making this step slow.
  spec <- as.list(df[, spec_cols])
  df$spec_min <- do.call(pmin, c(spec, list(na.rm = TRUE)))
  df$spec_max <- do.call(pmax, c(spec, list(na.rm = TRUE)))
  df$bad_spec <- df$spec_min < lo | df$spec_max > hi
  # NEW (Sep 24): bad_spec/filter_extreme_spectra above only catch WILD
  # out-of-range values -- they don't catch a spectrum that's abnormally
  # FLAT (low absorbance dynamic range) while still sitting inside those
  # bounds. Found via a real case: SSM20's 2024-09-06 grab-matched spectrum
  # has spec_max - spec_min = 2.55 (vs ~13-40 for every other SSM20 grab
  # sample) despite a perfectly ordinary lab NPOC reading that day (2.41
  # mg/L) -- a near-featureless spectrum paired with a normal DOC value
  # looks like a sensor/fouling artifact on that specific reading, not a
  # real reflection of the water. min_range = 5 matches NM's existing
  # filter_good_spectra() threshold (04_merge_grabsamples_and_scan.R),
  # which this SS pipeline never had an equivalent of -- SS's only
  # flatline check was Status's sd(spectrum) < 0.001 in
  # 00_SS_merge_timestamps.R, which is far stricter and didn't catch this.
  df$spec_range <- df$spec_max - df$spec_min
  df$flat_spec <- df$spec_range < min_range

  # NEWER (Sep 25): flat_spec/bad_spec catch spectra that are flat or
  # wildly out of range, but not a third failure mode found by comparing
  # individual grab-sample spectra visually: some otherwise-normal-looking
  # spectra have a single wavelength (or narrow band) that jumps sharply
  # off the smooth curve every other spectrum follows -- a spike/wobble
  # consistent with sensor noise, not real DOM chemistry (a real
  # absorbance spectrum is a smooth, roughly monotonic curve, not jagged).
  #
  # Measured per spectrum as: how far does it deviate, at its single worst
  # wavelength, from its own 5-band (12.5nm) moving average, as a fraction
  # of its own spec_range? Dividing by spec_range matters -- without it, a
  # genuinely large but SMOOTH high-DOC spectrum (bigger absolute values,
  # more real curvature) scores just as "deviant" as a small, jagged one.
  # Confirmed directly on this data: SST13's two highest-DOC grab samples
  # (2024-11-01, 2024-11-11) have the largest RAW deviation of any SST13
  # sample, but rank near the bottom on this RELATIVE measure -- while five
  # clearly low/mid-DOC, visibly jagged samples (SSM01's 2025-06-23;
  # SSM20's 2025-11-18; SST13's 2025-07-28/09-22/10-20/11-17/11-03) rank
  # far above everything else, including those two high-DOC anchors.
  #
  # The median (and therefore the threshold) is computed only over
  # grab-matched rows (Site not NA) at this site, not the full ~30,000-row
  # time series -- matches how this was validated against the actual grab
  # samples. Threshold = 2.5x that median; this cleanly separated the
  # visibly-jagged samples from everything else, including the two SST13
  # high-DOC anchors, when checked. Do NOT lower this without re-checking
  # that separation still holds -- push it low enough and it starts
  # catching SST13's only two high-DOC anchors too, which is a bigger call
  # than an automatic exclusion (dropping them removes SST13's only
  # training data above ~22 mg/L) -- see script-unification-todo.md.
  wl_val <- as.numeric(gsub("^X|\\.nm$", "", spec_cols))
  spec_ord <- spec[order(wl_val)]  # wavelength-ascending, for the moving average
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

data01_clean <- flag_bad_spectra(data01_clean)
print(plot_bad_spectra(data01_clean, "SSM01"))
ggsave(file.path(fig_dir, "SSM01_abs_over_time.png"), plot_bad_spectra(data01_clean, "SSM01"), width = 9, height = 6, dpi = 120)

data13_clean <- flag_bad_spectra(data13_clean)
print(plot_bad_spectra(data13_clean, "SST13"))
ggsave(file.path(fig_dir, "SST13_abs_over_time.png"), plot_bad_spectra(data13_clean, "SST13"), width = 9, height = 6, dpi = 120)

data20_clean <- flag_bad_spectra(data20_clean)
print(plot_bad_spectra(data20_clean, "SSM20"))
ggsave(file.path(fig_dir, "SSM20_abs_over_time.png"), plot_bad_spectra(data20_clean, "SSM20"), width = 9, height = 6, dpi = 120)

##############################################################
#### Drop spiky scans from the FULL deployment record      ####
##############################################################
# NEW (Sep 25): SSM20's full deployment record (all ~27,000 scans, not just
# grab-matched ones) looked visually chaotic -- a lot of individual scans
# with sharp local jaggedness scattered across the ENTIRE 18-month record
# (not concentrated in one bad period -- checked spec_min/max/range over
# time before assuming that). Same idea as spec_spiky above (deviation from
# a locally-smoothed version of the same spectrum, relative to its own
# range), but the threshold here is based on the site's own FULL-RECORD
# median, not just its ~15-20 grab samples -- a different, much bigger
# population, so it needs its own threshold and its own safety check.
#
# Checked before applying: at 3x the full-record median, SSM20 drops only
# 2.8% of all scans (762 of 27,152) and does NOT touch any of the 15 grab
# samples actually used in calibration. The SAME check on SSM01 and SST13 is
# NOT safe at this threshold -- SSM01 would drop 20% of its full record,
# SST13 16.5%, and SST13's cut would include 2024-11-01, one of only two
# high-DOC calibration anchors. That large a cut risks removing real
# environmental signal (e.g. storm-driven absorbance swings), not just
# instrument noise, so it is NOT applied to SSM01/SST13 here -- see
# script-unification-todo.md for a closer look at why those two sites'
# full records are so much heavier-tailed than SSM20's.
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
  if (n_dropped > 0) {
    message(sprintf("%s: dropping %d scan(s) (%.1f%%) with an abnormally spiky full-spectrum shape from the full deployment record (>%gx full-record median relative deviation).",
                     site_name, n_dropped, 100 * n_dropped / nrow(df), factor))
  }
  df[!flagged, , drop = FALSE]
}

data20_clean <- filter_spiky_scans(data20_clean, "SSM20", factor = 3)

############################
##########################################################################
#### Sample-loss diagnostic: why are we losing grab samples? ###########
##########################################################################
# User question: "I thought we were only removing samples in the MDL step."
# We're not -- there are FOUR distinct places a grab sample can go missing
# between the chem log and what 06_SS_calibrate_DOC.R actually trains on:
#   1. No scan recorded at that exact timestamp at all (never matched)
#   2. Scan's Status == "Corrupted_No_Match" (spectrum masked to NA above)
#   3. filter_extreme_spectra() -- hard row drop, any wavelength outside
#      [-3, 60], all three sites
#   4. filter_spiky_scans() -- hard row drop on the FULL deployment record,
#      SSM20 only
#   5. Would be excluded downstream in 06_SS_calibrate_DOC.R's
#      drop_mdl_replaced(): MDL-flagged NPOC, flat_spec, and/or spec_spiky
#      (checked here from the exact same columns 06 uses, so this mirrors
#      06's logic without needing to run it)
# This re-derives each intermediate stage from the already-defined, already-
# run pure functions above (filter_extreme_spectra, filter_spiky_scans)
# rather than a second copy of their logic, so it can never silently drift
# out of sync with the filters actually applied earlier in this script.
trace_sample_loss <- function(expected, scan_src, data_extreme, data_clean_final, site_name, check_spiky_drop = FALSE) {
  out <- lapply(seq_len(nrow(expected)), function(i) {
    dt <- expected$DateTime[i]
    reason <- NA_character_
    if (!(dt %in% scan_src$DateTime)) {
      reason <- "1. no scan recorded at this timestamp (never matched)"
    } else {
      status_here <- scan_src$Status[scan_src$DateTime == dt][1]
      if (!is.na(status_here) && status_here == "Corrupted_No_Match") {
        reason <- "2. scan Status == Corrupted_No_Match (spectrum masked to NA)"
      } else if (!(dt %in% data_extreme$DateTime)) {
        reason <- "3. dropped: extreme spectral value outside [-3, 60] (filter_extreme_spectra)"
      } else if (check_spiky_drop && !(dt %in% data_clean_final$DateTime)) {
        reason <- "4. dropped: full-record spiky-scan filter (filter_spiky_scans, SSM20 only)"
      } else {
        row <- data_clean_final[!is.na(data_clean_final$DateTime) & data_clean_final$DateTime == dt & !is.na(data_clean_final$Site), ]
        if (nrow(row) == 0) {
          reason <- "5. present in cleaned scan data but never grab-matched (unexpected -- investigate)"
        } else {
          row <- row[1, ]
          flags <- c(
            if (!is.na(row$NPOC_flagged_any) && row$NPOC_flagged_any) "MDL-flagged NPOC",
            if (!is.na(row$flat_spec) && row$flat_spec) "flat_spec",
            if (!is.na(row$spec_spiky) && row$spec_spiky) "spec_spiky"
          )
          if (length(flags) > 0) {
            reason <- paste0("6. would be excluded in 06's drop_mdl_replaced(): ", paste(flags, collapse = " + "))
          } else {
            reason <- "RETAINED for calibration"
          }
        }
      }
    }
    data.frame(Site = site_name, Date = expected$Date[i], DateTime = dt,
               NPOC = expected[["NPOC..mg.C.L."]][i], fate = reason,
               stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(out)
}

# Re-derive the post-extreme-filter (pre-spiky-filter) stage for each site --
# filter_extreme_spectra() is a pure function of data0X_final, so calling it
# again here exactly reproduces that intermediate state without needing an
# extra variable saved earlier in the script.
extreme_dt01 <- filter_extreme_spectra(data01_final, lo = -3, hi = 60)
extreme_dt20 <- filter_extreme_spectra(data20_final, lo = -3, hi = 60)
extreme_dt13 <- filter_extreme_spectra(data13_final, lo = -3, hi = 60)

loss01 <- trace_sample_loss(U01, SSM01, extreme_dt01, data01_clean, "SSM01", check_spiky_drop = FALSE)
loss20 <- trace_sample_loss(U20, SSM20, extreme_dt20, data20_clean, "SSM20", check_spiky_drop = TRUE)
loss13 <- trace_sample_loss(U13, SST13, extreme_dt13, data13_clean, "SST13", check_spiky_drop = FALSE)

sample_loss_report <- dplyr::bind_rows(loss01, loss20, loss13)
write.csv(sample_loss_report, "googledrive/sample_loss_report.csv", row.names = FALSE)
# Same "with grab" output folder this script uploads SSM01_merged.csv etc
# to further down (drive_folder_id is defined later in this script, right
# before those uploads -- hardcoded here since it isn't in scope yet at
# this point in a top-to-bottom run).
googledrive::drive_put(media = "googledrive/sample_loss_report.csv",
                        path = googledrive::as_id("1Wju54VbyACZ_RFtfeInSvBCiVDKFScGj"))

message("\n==== Sample-loss diagnostic: why is a grab sample missing from calibration? ====")
for (s in unique(sample_loss_report$Site)) {
  sub <- sample_loss_report[sample_loss_report$Site == s, ]
  n_ret <- sum(sub$fate == "RETAINED for calibration")
  message(sprintf("\n-- %s: %d/%d grab samples retained --", s, n_ret, nrow(sub)))
  dropped <- sub[sub$fate != "RETAINED for calibration", ]
  if (nrow(dropped) > 0) {
    for (j in seq_len(nrow(dropped))) {
      message(sprintf("  %s: %s", format(dropped$DateTime[j], "%Y-%m-%d %H:%M"), dropped$fate[j]))
    }
  }
}

############################
#### Save matched files ####
############################
# Make sure it is in datetime format
data01_clean$DateTime <- format(data01_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(data01_clean,"googledrive/SSM01_merged.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
data20_clean$DateTime <- format(data20_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(data20_clean,"googledrive/SSM20_merged.csv" , row.names=FALSE, quote=FALSE)
# Make sure it is in datetime format
data13_clean$DateTime <- format(data13_clean$DateTime, "%Y-%m-%d %H:%M:%S")
# Save the new data frame to a CSV file
write.csv(data13_clean,"googledrive/SST13_merged.csv" , row.names=FALSE, quote=FALSE)

# Define the target folder ID in Google Drive
# This is the "with grab" folders
drive_folder_id <- "1Wju54VbyACZ_RFtfeInSvBCiVDKFScGj"

# Upload the file to the specified Google Drive folder
drive_put(media = "googledrive/SSM01_merged.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/SSM20_merged.csv", path = as_id(drive_folder_id))
drive_put(media = "googledrive/SST13_merged.csv", path = as_id(drive_folder_id))

##############################################################################
#### RAW (uncompensated) fingerprint version -- NEW (Sep 28)              ####
##############################################################################
# Same merge/clean/QC pipeline as above, applied to the raw (uncompensated)
# fingerprint instead of the compensated one -- testing Ariel's suggestion
# that the default turbidity compensation may be overcorrecting and removing
# real DOC-related spectral signal. Reuses the exact same helper functions
# defined above (filter_extreme_spectra, flag_bad_spectra, plot_bad_spectra,
# filter_spiky_scans) so this can never silently drift out of sync with
# whatever QC logic is applied to the compensated path -- only the INPUT
# file differs (*_absparams_raw_clean.csv, produced by 02_SS_clean.R from
# 01_SS_merge_params_and_abs.R's *_absparams_raw.csv). Every object name
# below carries a _raw suffix so nothing here can collide with or overwrite
# the compensated pipeline's objects.

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

