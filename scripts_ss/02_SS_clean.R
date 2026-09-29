##==============================================================================
## Project: QuEST - Script to tidy up South Sandy scan data and plot it
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================
library(googledrive) # download docs from Drive
library(tidyverse)
library(readxl) # to read Excel
library(lubridate) # edit date format
library(ggplot2)

########################################
#### Clear folders that we will use ####
########################################
# list and delete all files in the folder
files <- list.files(path = "data", full.names = TRUE)
file.remove(files)

files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

#####################
#### Import Data ####
#####################
# load data from Google Drive. This is the "Params and abs" folder -- i.e.
# the "merged" folder 01_SS_merge_params_and_abs.R actually uploads
# *_absparams.csv (and now *_absparams_raw.csv) into.
#
# FIXED (Sep 25): this used to read from a DIFFERENT folder
# (1fXvhb7aCanVicVbnQPwC26Gy2yZVMHAk, a "from raw" subfolder living INSIDE
# this "manual merge" folder) -- not the folder 01 actually uploads to. User
# confirmed: probably created manually at some point, reason no longer
# remembered, not kept in sync with 01's actual output. Switched to reading
# directly from 01's real output folder instead -- removes the ambiguity
# and the manual step. Old line kept below, commented out, for reference.
# scan <- googledrive::as_id("https://drive.google.com/drive/folders/1fXvhb7aCanVicVbnQPwC26Gy2yZVMHAk")  # old "from raw" subfolder -- not 01's actual output
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1WbfZWpSeXVLoSEvxqbVnjgvgo4uUwGtm")
scan_csvs <- googledrive::drive_ls(path = scan, type = "csv")

# create empty list to store data frames
scan_list <- list()

# loop over each file in the `scan_csvs` data frame
for (i in seq_along(scan_csvs$id)) {
  # define the local file path
  local_path <- file.path("googledrive", scan_csvs$name[i])
  
  # download the file
  googledrive::drive_download(
    file = scan_csvs$id[i],
    path = local_path,
    overwrite = TRUE
  )
  
  # read the header row (row 2)
  header <- read.csv(local_path)
  
  # read the data starting from row 4 using the header as column names
  data <- read.csv(local_path)
  
  # store the data in the list
  scan_list[[scan_csvs$name[i]]] <- data
}

head(scan_list)

#################
#### Tidying ####
#################
# change some names for easier manipulation
scan_list <- lapply(scan_list, function(df) {
  # rename columns by matching the existing names
  df <- df %>%
    dplyr::rename(
      DOC_mg.l = DOCeq..mg.l....Measured.value,
      NO3.N_mg.l = NO3.Neq..mg.l....Measured.value,
      NO3_mg.l = NO3eq..mg.l....Measured.value,
      TOC_mg.l = TOCeq..mg.l....Measured.value,
      TSS_mg.l = TSSeq..mg.l....Measured.value,
    )
  # ensure numeric variables are converted to numeric
  df <- df %>%
    mutate(across(c(DOC_mg.l, NO3.N_mg.l, NO3_mg.l, TOC_mg.l, TSS_mg.l), as.numeric)) %>%
    mutate(datetime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))
  
  return(df)
})

# --- Guard against garbage DateTime rows ---
# We've seen rows in *_absparams.csv where the "DateTime" column literally
# contains text like "Replaced_Good" or "Corrupted_No_Match" (i.e. a Status
# value that ended up in the DateTime field) with every other column blank,
# plus plain rows where DateTime is blank/unparseable. `as.POSIXct()` above
# turns all of these into NA in `datetime`, but the original bad text was
# still silently carried forward in every downstream bind_rows()/merge(),
# and merging on a shared NA/garbage key is almost certainly what has been
# inflating row counts and producing duplicate-looking rows further down the
# pipeline. Drop them here, with a count so it's visible if this gets bad.
scan_list <- lapply(scan_list, function(df) {
  bad <- is.na(df$datetime)
  if (any(bad)) {
    message(sprintf(
      "Dropping %d row(s) with missing/unparseable DateTime (e.g. %s).",
      sum(bad),
      paste(unique(df$DateTime[bad])[seq_len(min(3, length(unique(df$DateTime[bad]))))], collapse = "; ")
    ))
  }
  df[!bad, , drop = FALSE]
})

#################
#### Cleaning ###
#################
SSM01 <- scan_list[["SSM01_absparams.csv"]]
SSM20 <- scan_list[["SSM20_absparams.csv"]]
SST13 <- scan_list[["SST13_absparams.csv"]]

# Wavelength columns are named like "X200.00.nm" ... "X750.00.nm". The old
# "manual merge cleaning" / "my cleaning" steps below used hardcoded column
# POSITIONS, applied one after another -- since the second step's numbers
# were written against positions that had already shifted from the first
# drop, they ended up trimming a different (and for SST13, a gap in the
# MIDDLE of the) wavelength range per site, rather than one consistent
# cutoff. Select spectral columns by their actual wavelength value instead,
# with one consistent cutoff across all three SS sites (matching NM's rule).
is_wl_col    <- function(nm) grepl("^X[0-9]+\\.[0-9]+\\.nm$", nm)
wl_from_name <- function(nm) as.numeric(gsub("^X|\\.nm$", "", nm))
DOC_WL_MAX   <- 450  # nm; consistent cutoff across all SS sites (same as NM)

drop_high_wavelengths <- function(df, wl_max = DOC_WL_MAX) {
  nm   <- names(df)
  drop <- is_wl_col(nm) & (wl_from_name(nm) > wl_max)
  df[, !drop, drop = FALSE]
}

# Non-spectral junk / duplicate-sensor / status columns to drop, by name.
# This preserves exactly what the old hardcoded-index steps were dropping
# for each site (traced through by hand from the old indices to column
# names) -- just written explicitly so it survives column reordering
# upstream instead of silently drifting. any_of() means a column that's
# already missing is skipped rather than erroring.
junk_cols_SSM01 <- c(
  "X", "Measured.status",
  "Temperature_40...F....Measured.value", "Temperature_21...C....Measured.value",
  "TSSeq..mg.l....Measured.status", "Temperature_19...C....Measured.status",
  "Device.Rotation.......Measured.status", "Device.Tilt.......Measured.status",
  "Supply.Current..mA....Measured.status", "Supply.Voltage..V....Measured.status"
)
junk_cols_SSM20 <- c(
  "X", "Measured.status",
  "Temperature_19...C....Measured.value", "Temperature_26...F....Measured.value",
  "Temperature_40...F....Measured.value",
  "Device.Tilt.......Measured.status", "Supply.Current..mA....Measured.status",
  "Supply.Voltage..V....Measured.status", "Temperature_19...C....Measured.status",
  "Temperature_26...F....Measured.status"
)
junk_cols_SST13 <- c(
  "X", "Measured.status",
  "Temperature_22...C....Measured.value", "Temperature_28...F....Measured.value",
  "Turbidity_24..NTUeq....Measured.value", "Turbidity_24..NTUeq....Measured.status",
  "Turbidity_7..FTUeq....Measured.value", "Turbidity_7..FTUeq....Measured.status",
  "UV254f..Abs.m....Measured.value", "UV254f..Abs.m....Measured.status",
  "UV254t..Abs.m....Measured.value", "UV254t..Abs.m....Measured.status",
  "UVT254_30....10cm....Measured.value", "UVT254_30....10cm....Measured.status",
  "UVT254_31....cm....Measured.value", "UVT254_31....cm....Measured.status",
  "Device.Rotation.......Measured.status", "Device.Tilt.......Measured.status",
  "Supply.Current..mA....Measured.status", "Supply.Voltage..V....Measured.status",
  "Temperature_22...C....Measured.status", "Temperature_28...F....Measured.status",
  "Temperature_21...C....Measured.status", "Temperature_40...F....Measured.value"
)

SSM01 <- SSM01 %>% dplyr::select(-dplyr::any_of(junk_cols_SSM01)) %>% drop_high_wavelengths()
SSM20 <- SSM20 %>% dplyr::select(-dplyr::any_of(junk_cols_SSM20)) %>% drop_high_wavelengths()
SST13 <- SST13 %>% dplyr::select(-dplyr::any_of(junk_cols_SST13)) %>% drop_high_wavelengths()

# NEW (Sep 25): raw (uncompensated) fingerprint version, parallel to
# SSM01/SSM20/SST13 above -- same junk-column removal and wavelength
# cutoff, built from *_absparams_raw.csv (produced by
# 01_SS_merge_params_and_abs.R from the sheet-3 raw fingerprint) instead
# of *_absparams.csv. Kept as separate _raw objects throughout so this
# never touches or overwrites the compensated pipeline above -- purely
# for comparing the two per Ariel's suggestion that turbidity compensation
# may be overcorrecting and removing real DOC signal.
SSM01_raw <- scan_list[["SSM01_absparams_raw.csv"]]
SSM20_raw <- scan_list[["SSM20_absparams_raw.csv"]]
SST13_raw <- scan_list[["SST13_absparams_raw.csv"]]

SSM01_raw <- SSM01_raw %>% dplyr::select(-dplyr::any_of(junk_cols_SSM01)) %>% drop_high_wavelengths()
SSM20_raw <- SSM20_raw %>% dplyr::select(-dplyr::any_of(junk_cols_SSM20)) %>% drop_high_wavelengths()
SST13_raw <- SST13_raw %>% dplyr::select(-dplyr::any_of(junk_cols_SST13)) %>% drop_high_wavelengths()

#########################################################################################
#### Count number of service dates (out of water days) and 'ABOVE' and 'BELOW' values####
#########################################################################################
# when scan is out of water it records as NO_MEDIUM
# replace 'NO_MEDIUM' values with NA
# also when it reads < lower error limit or  > upper error limit, it flags as 'VAL_BELOW' or 'VAL_ABOVE'
# replace 'VAL_BELOW' or 'VAL_ABOVE' flagged values with NA

### first, count how many logs with , 'VAL_BELOW' or 'VAL_ABOVE' each one has ###
# initialize a list to store the counts for each file
count_list <- list()

# loop over each data frame in the list
for (file_name in names(scan_list)) {

  # Get the data frame
  data <- scan_list[[file_name]]

  # filter to only character columns
  char_data <- data[, sapply(data, is.character)]

  # count the number of rows that contain VAL_BELOW, VAL_ABOVE, or NO_MEDIUM
  val_below_count <- sum(apply(char_data, 1, function(row) any(row == "VAL_BELOW", na.rm = TRUE)))
  val_above_count <- sum(apply(char_data, 1, function(row) any(row == "VAL_ABOVE", na.rm = TRUE)))
  no_medium_count <- sum(apply(char_data, 1, function(row) any(row == "NO_MEDIUM", na.rm = TRUE)))
  math_err_count <- sum(apply(char_data, 1, function(row) any(row == "MATH_ERR", na.rm = TRUE)))
  volt_low_count <- sum(apply(char_data, 1, function(row) any(row == "VOLT_LOW", na.rm = TRUE)))
  volt_high_count <- sum(apply(char_data, 1, function(row) any(row == "VOLT_HIGH", na.rm = TRUE)))
  hw_deffect_count <- sum(apply(char_data, 1, function(row) any(row == "HW_DEFECT", na.rm = TRUE)))

  # store the counts in a data frame
  count_list[[file_name]] <- data.frame(
    File = file_name,
    VAL_BELOW = val_below_count,
    VAL_ABOVE = val_above_count,
    NO_MEDIUM = no_medium_count,
    MATH_ERR = math_err_count,
    VOLT_LOW = volt_low_count,
    VOLT_HIGH = volt_high_count,
    HW_DEFECT = hw_deffect_count
  )
}

# combine all the individual data frames into one
final_count_table <- do.call(rbind, count_list)

# print the final table
print(final_count_table)

#### save data ####
# save the final table to a CSV file
# write.csv(final_count_table, "final_NAcount_table.csv", row.names = TRUE)

#################################################
#### Clean service dates (out of water days) ####
#################################################
scan_list <- lapply(scan_list, function(df) {
  # rename columns by matching the existing names
  df <- df %>%
    rename(
      DOC_status   = DOCeq..mg.l....Measured.status,
      NO3.N_status = NO3.Neq..mg.l....Measured.status,
      NO3_status   = NO3eq..mg.l....Measured.status,
      TOC_status   = TOCeq..mg.l....Measured.status,
      TSS_status   = TSSeq..mg.l....Measured.status
    )

  # ensure numeric variables are converted to numeric
  df <- df %>%
    mutate(
      across(c(DOC_mg.l, NO3.N_mg.l, NO3_mg.l, TOC_mg.l, TSS_mg.l), as.numeric),
      DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")
    )

  # define status values to replace with NA
  status_values_to_replace <- c("NO_MEDIUM", "VAL_BELOW:NO_MEDIUM")
  #"NEG_MED", "DARK_MAX", "NEG_FP"

  #################################################
  #### Create a single "bad row" logical flag ####
  #################################################
  df <- df %>%
    mutate(
      bad_row = DOC_status   %in% status_values_to_replace |
        NO3.N_status %in% status_values_to_replace |
        NO3_status   %in% status_values_to_replace |
        TOC_status   %in% status_values_to_replace |
        TSS_status   %in% status_values_to_replace |
        Measured.status %in% status_values_to_replace
    )

  ###################################
  #### Clean chemistry variables ####
  ###################################
  df <- df %>%
    mutate(
      DOC_clean   = ifelse(bad_row, NA, DOC_mg.l),
      NO3.N_clean = ifelse(bad_row, NA, NO3.N_mg.l),
      NO3_clean   = ifelse(bad_row, NA, NO3_mg.l),
      TOC_clean   = ifelse(bad_row, NA, TOC_mg.l),
      TSS_clean   = ifelse(bad_row, NA, TSS_mg.l)
    )

################################
#### Clean spectral columns ####
################################
# Was hardcoded to positions 18:228, which actually captured 9 non-spectral
# columns (Temperature_21, several ...Measured.status columns) ahead of the
# real spectral range, AND cut off the last few wavelengths (705-750nm).
# Select by name instead, using the same is_wl_col() helper defined above.
  spectral_cols <- names(df)[is_wl_col(names(df))]
  df <- df %>%
    mutate(across(
      all_of(spectral_cols),
      ~ ifelse(bad_row, NA, .x)
    ))
  # optional: remove helper column
  df <- df %>% select(-bad_row)
  return(df)
})


#########################################
#### remove low volt at end of USF21 ####
#########################################
# USF21_test <- USF21 %>%
#   mutate(across(
#     c("DOC_clean", "NO3.N_clean", "NO3_clean", "TOC_clean", "TSS_clean", 27:236),
#     ~ ifelse(between(DateTime, as.Date("2024-11-15"), as.Date("2024-11-16")), NA, .)
#   ))

#######################
### Return to list ####
#######################
scan_filtered2 <- list()
scan_filtered2[["SSM01_absparams"]] <- SSM01
scan_filtered2[["SSM20_absparams"]] <- SSM20
scan_filtered2[["SST13_absparams"]] <- SST13
# NEW (Sep 25): raw-fingerprint versions, named so the save loop below
# writes them out as <name>_clean.csv alongside the compensated ones --
# see the raw-fingerprint comment above.
scan_filtered2[["SSM01_absparams_raw"]] <- SSM01_raw
scan_filtered2[["SSM20_absparams_raw"]] <- SSM20_raw
scan_filtered2[["SST13_absparams_raw"]] <- SST13_raw

#####################################
#### Plot all variables separate ####
#####################################
# function to plot each variable separately in the same panel
plot_variables <- function(df, file_name) {
  # ensure column selection works correctly
  df_long <- df %>%
    dplyr::select("datetime", "TSS_mg.l", "TOC_mg.l", "NO3.N_mg.l", "NO3_mg.l", "DOC_mg.l") %>%
    pivot_longer(cols = -datetime, names_to = "Variable", values_to = "Value")
  
  # generate the plot
  ggplot(data = df_long, aes(x = datetime, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # separate plot for each variable, stacked vertically
    scale_x_datetime(date_breaks = "15 days", date_labels = "%y/%m/%d") +
    ggtitle(file_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Measured Value") +
    theme(legend.position = "none")  # hide legend since we have separate panels
}

# generate plots
print(plot_variables(scan_filtered2[[1]], names(scan_filtered2)[1]))
print(plot_variables(scan_filtered2[[2]], names(scan_filtered2)[2]))
print(plot_variables(scan_filtered2[[3]], names(scan_filtered2)[3]))

### save figures to folder ###
for (i in seq_along(scan_filtered2)) {
  ggsave(paste0("scan_figs/", names(scan_filtered2)[i], "_separate.png"), plot_variables(scan_filtered2[[i]], names(scan_filtered2)[i]))
}

tail(scan_filtered1[[1]])

###################################
#### Plot just a section of it ####
###################################
Date1 <- as.Date("2025-08-25", "%Y-%m-%d")
Date2 <- as.Date("2025-08-30", "%Y-%m-%d")

# create empty list to store data frames
scan_subdf <- list()

# loop through each data frame in the list to change DateTime column name
for (i in seq_along(scan_filtered2)) {
  # Access the current data frame
  df <- scan_filtered2[[i]]
  
  # change time frame
  subdf <- df[df$DateTime < Date2 & df$DateTime > Date1,]
  
  # update the data frame in the list
  scan_subdf[[i]] <- subdf
}

# function to plot each variable separately in the same panel
plot_variables <- function(df, file_name) {
  # ensure column selection works correctly
  df_long <- df %>%
    dplyr::select("DateTime", "Temp_C", "TSS_clean", "TOC_clean", "NO3.N_clean", "NO3_clean", "DOC_clean") %>%
    pivot_longer(cols = -DateTime, names_to = "Variable", values_to = "Value")
  
  # generate the plot
  ggplot(data = df_long, aes(x = DateTime, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +  # separate plot for each variable, stacked vertically
    scale_x_datetime(date_breaks = "7 days", date_labels = "%m/%d") +
    ggtitle(file_name) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Measured Value") +
    theme(legend.position = "none")  # hide legend since we have separate panels
}

# generate plots
print(plot_variables(scan_subdf[[1]], scan_csvs$name[1]))
print(plot_variables(scan_subdf[[2]], scan_csvs$name[2]))
print(plot_variables(scan_subdf[[3]], scan_csvs$name[3]))

### save figures to folder ###
for (i in seq_along(scan_subdf)) {
  ggsave(paste0("scan_figs/", scan_csvs$name[i], "_subdf.png"), plot_variables(scan_subdf[[i]], scan_csvs$name[i]))
}

####################################
#### Save cleaned data to Drive ####
####################################
# function to remove file extension
remove_extension <- function(file_name) {
  sub("\\.[[:alnum:]]+$", "", file_name)
}

# loop through each data frame in the list
for (i in seq_along(scan_filtered2)) {
  # access the current data frame
  df <- scan_filtered2[[i]]
  
  # define the file name and path
  # FIXED (Sep 25): was remove_extension(scan_csvs$name[i]) -- positional
  # against the Drive folder listing, which broke once *_absparams_raw.csv
  # files landed in the same folder (see comment above the save loop).
  clean_name <- names(scan_filtered2)[i]
  file_name <- paste0("googledrive/", clean_name, "_clean.csv")
  
  # save the new data frame to a CSV file
  write.csv(df, file_name, row.names=FALSE, quote=FALSE)
  
  # define the target folder ID in Google Drive, this is the "clean" folder
  drive_folder_id <- "1BNCKA7LdysjDH5_REI4WhH_P0Z4FIe0r"
  
  # upload the file to the specified Google Drive folder
  drive_put(media = file_name, path = as_id(drive_folder_id))
}

