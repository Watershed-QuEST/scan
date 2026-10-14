##==============================================================================
## Project: QuEST
## Script to merge scan files for BRMQ1 (Brush Creek)
##
## BRMQ1 is different from BRMQ4/BRM06 (and every other site in this project)
## in two ways that this script exists specifically to handle:
##
## 1. BRMQ1 was installed via a TELEMETRY system (Poseidon, then Neptune --
##    see below), not the usual manual/batch s::can export the rest of the
##    pipeline assumes. Its files live in the Drive raw folder in
##    "BRMQ1" -- this script pulls that folder.
##
## 2. The file structure is NOT uniform across BRMQ1's deployment history --
##        because sheet count and content vary by era:
##      - The rollup file "2024-11-02_2025-11-19_SCAN-Poseidon-BRMQ1.xlsx"
##        (the one big file spanning the whole first year) has a fingerprint
##        sheet with NO wavelength columns at all -- just Timestamp +
##        Measured status, across all ~4,950 rows. Not a per-row gap, the
##        sheet itself was only ever exported with 2 columns. Unusable for
##        calibration; EXCLUDED below (and the exclusion is a general
##        "no wavelength columns found" check, not a hardcoded filename, so
##        it will also catch any future export with the same problem).
##      - The standalone per-period files (one per download, e.g.
##        "2025-09-19_BRMQ1_Poseidon.xlsx") DO have a full 213-wavelength
##        RAW (uncompensated) fingerprint sheet (200.00-730.00 nm), but only
##        a RAW sheet --
##        there is no compensated fingerprint at all until the
##        "2025-12-18_SCAN_Poseidon-BRMQ1.xlsx" file, which is the first to
##        also have a "Compensated Fingerprint" sheet alongside the raw one.
##      - Around 2026-04-08 the files switch from "Poseidon" to "Neptune" in
##        their names -- this is when the sensor swap happened (the spec serial
##        number in the sheet names changes from 22450209 to 24130208).
##        Neptune-era files have the same 3-sheet
##        (Parameter / Compensated / Raw) structure as every other site's
##        xlsx, in that sheet order, but a narrower wavelength grid
##        (210 wavelengths, 200.00-722.50 nm, vs. Poseidon's 213, 200.00-
##        730.00 nm -- 210 of those are identical between the two sensors).
##        See the note near BRMQ1_abs below for what this means once the
##        two eras are combined.
##    So: this script does NOT assume a fixed sheet count or position for
##    the fingerprint sheets -- it detects "Parameter" / "Compensated" /
##    raw-fingerprint sheets BY NAME per file, and simply produces no
##    compensated-fingerprint row for files that don't have one.
##
## 3. The three subfolders inside "BRMQ1 - scan" (2025-02, 2025-03, 2025-06)
##    are NOT scan/fingerprint data -- they're per-channel telemetry
##    exports (battery/power voltage, cell signal strength, a separate
##    sonde's temperature/turbidity/FDOM, a stage/level logger, and
##    per-channel copies of DOCeq/TOCeq/NO3/TSS). drive_ls() below only lists the
##    top-level folder contents (it doesn't recurse into subfolders), so
##    those three are skipped automatically.
##==============================================================================

library(readxl)
library(dplyr)
library(googledrive)

#######################################
#### Where this site's files live  ####
#######################################
# Drive link for BRMQ1 here.
drive_folder_link <- "https://drive.google.com/drive/folders/1pRvU1l-1GnvPT0r43FV0Bt4f9W29oIzY"

# Local scratch folder the files get downloaded into for this run -- not a
# permanent copy, just a staging spot so read_excel() has something to open.
local_dir <- "BRMQ1_scan_downloads"
if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

drive_files <- drive_ls(as_id(drive_folder_link))
drive_files <- drive_files[grepl("\\.xlsx$", drive_files$name), ]
message(sprintf("Found %d top-level xlsx file(s) in the Drive folder.", nrow(drive_files)))

# Only download a file if we don't already have a same-named copy from a
# previous run in this session -- avoids re-downloading everything every
# time this script is re-run while testing.
for (i in seq_len(nrow(drive_files))) {
  dest <- file.path(local_dir, drive_files$name[i])
  if (!file.exists(dest)) {
    drive_download(as_id(drive_files$id[i]), path = dest, overwrite = FALSE)
  }
}

all_files <- list.files(local_dir, pattern = "\\.xlsx$", full.names = FALSE)
message(sprintf("Downloaded/found %d usable xlsx file(s) in '%s'.", length(all_files), local_dir))

########################################################
#### Helper: read one file's Parameter + fingerprint ####
#### sheet(s), whatever shape they turn out to have  ####
########################################################
read_header_and_data <- function(path, sheet, skip_header = 1, skip_data = 4) {
  header <- read_excel(path, sheet = sheet, skip = skip_header, n_max = 1, col_names = FALSE)
  col_names <- as.character(unlist(header[1, ]))
  col_names[col_names == "" | is.na(col_names)] <- paste0("X", seq_along(col_names))
  data <- read_excel(path, sheet = sheet, skip = skip_data, col_names = col_names)
  colnames(data)[1] <- "DateTime"
  data
}

# A fingerprint sheet is only usable if it has at least one real wavelength
# column (named like "200.00 nm") -- this is the general check that replaces
# hardcoding the known-bad rollup file by name.
has_wavelength_cols <- function(df) {
  any(grepl("nm$", names(df)))
}

read_one_file <- function(path) {
  sheet_names <- excel_sheets(path)

  if (!("Parameter" %in% sheet_names)) {
    message(sprintf("  %s: no 'Parameter' sheet found -- skipping entirely.", basename(path)))
    return(NULL)
  }
  params <- read_header_and_data(path, sheet = "Parameter")

  # Fingerprint sheets are named "<serial> Compensated Fingerprin..." and
  # "<serial> Fingerprint _Abs_m_..." (Excel truncates sheet names to 31
  # chars, hence the cut-off "Fingerprin"). Detect by content of the name,
  # not position -- sheet count varies 2 vs 3 across this site's history.
  compensated_sheet <- sheet_names[grepl("Compensated", sheet_names, ignore.case = TRUE)]
  raw_sheet <- setdiff(sheet_names, c("Parameter", compensated_sheet))

  abs_compensated <- NULL
  if (length(compensated_sheet) == 1) {
    cand <- read_header_and_data(path, sheet = compensated_sheet)
    if (has_wavelength_cols(cand)) {
      abs_compensated <- cand
    } else {
      message(sprintf("  %s: compensated sheet has no wavelength columns -- treating as unavailable, not an error.", basename(path)))
    }
  }

  abs_raw <- NULL
  if (length(raw_sheet) == 1) {
    cand <- read_header_and_data(path, sheet = raw_sheet)
    if (has_wavelength_cols(cand)) {
      abs_raw <- cand
    } else {
      message(sprintf("  %s: fingerprint sheet has no wavelength columns (known issue with the Nov'24-Nov'25 rollup export) -- excluding this file's spectral data.", basename(path)))
    }
  } else if (length(raw_sheet) > 1) {
    message(sprintf("  %s: found %d candidate raw-fingerprint sheets, expected 1 -- skipping spectral data for this file, check it by hand.", basename(path), length(raw_sheet)))
  }

  list(params = params, abs = abs_compensated, abs_raw = abs_raw)
}

#########################
#### Read every file ####
#########################
results <- list()
for (f in all_files) {
  message(sprintf("Reading %s ...", f))
  results[[f]] <- tryCatch(
    read_one_file(file.path(local_dir, f)),
    error = function(e) {
      message(sprintf("  failed to read %s: %s", f, conditionMessage(e)))
      NULL
    }
  )
}
results <- results[!vapply(results, is.null, logical(1))]

params_list    <- lapply(results, `[[`, "params")
abs_list       <- Filter(Negate(is.null), lapply(results, `[[`, "abs"))
abs_raw_list   <- Filter(Negate(is.null), lapply(results, `[[`, "abs_raw"))

message(sprintf(
  "\n%d file(s) read. %d have params, %d have a usable compensated fingerprint, %d have a usable raw fingerprint.",
  length(results), length(params_list), length(abs_list), length(abs_raw_list)
))

################################################
#### Combine + flag Good/Corrupted/Replaced ####
################################################
combine_params <- function(file_list) {
  bind_rows(file_list) %>%
    mutate(DateTime = as.POSIXct(DateTime)) %>%
    arrange(DateTime) %>%
    distinct(DateTime, .keep_all = TRUE)
}

combine_and_flag_spectra <- function(file_list) {
  if (length(file_list) == 0) {
    return(NULL)
  }
  processed <- lapply(file_list, function(df) {
    df$DateTime <- as.POSIXct(df$DateTime)
    spec_cols <- grep("nm$", colnames(df), value = TRUE)
    df <- df %>% mutate(across(all_of(spec_cols), as.numeric))
    df$row_variance <- apply(df[, spec_cols, drop = FALSE], 1, sd, na.rm = TRUE)
    df$temp_status <- ifelse(!is.na(df$row_variance) & df$row_variance > 0.001, "Good", "Corrupted")
    df
  })

  bind_rows(processed) %>%
    group_by(DateTime) %>%
    arrange(DateTime, desc(temp_status), desc(row_variance)) %>%
    mutate(
      Status = case_when(
        n() == 1 & first(temp_status) == "Good" ~ "Original_Good",
        n() > 1 & first(temp_status) == "Good" ~ "Replaced_Good",
        first(temp_status) == "Corrupted" ~ "Corrupted_No_Match",
        TRUE ~ "Original_Good"
      )
    ) %>%
    slice(1) %>%
    ungroup() %>%
    select(-row_variance, -temp_status)
}

# NOTE on wavelength grid: Poseidon-era files have 213 wavelength columns
# (200.00-730.00 nm) and Neptune-era files have 210 (200.00-722.50 nm) --
# 210 of those are shared exactly. bind_rows() below handles this
# fine mechanically (it unions the columns and fills NA for whichever era
# doesn't have a given wavelength).
BRMQ1_params  <- combine_params(params_list)
BRMQ1_abs     <- combine_and_flag_spectra(abs_list)
BRMQ1_abs_raw <- combine_and_flag_spectra(abs_raw_list)

if (!is.null(BRMQ1_abs)) {
  message(sprintf("BRMQ1 compensated fingerprint record: %d rows, %s to %s.",
                   nrow(BRMQ1_abs), min(BRMQ1_abs$DateTime), max(BRMQ1_abs$DateTime)))
} else {
  message("BRMQ1 compensated fingerprint record: none (no usable compensated sheets found).")
}
message(sprintf("BRMQ1 raw fingerprint record: %d rows, %s to %s.",
                 nrow(BRMQ1_abs_raw), min(BRMQ1_abs_raw$DateTime), max(BRMQ1_abs_raw$DateTime)))

##############################
#### Save combined files  ####
##############################
if (!dir.exists("data")) dir.create("data", recursive = TRUE)

BRMQ1_params$DateTime <- format(BRMQ1_params$DateTime, "%Y-%m-%d %H:%M:%S")
write.csv(BRMQ1_params, "data/BRMQ1_params.csv", row.names = FALSE)

BRMQ1_abs_raw$DateTime <- format(BRMQ1_abs_raw$DateTime, "%Y-%m-%d %H:%M:%S")
write.csv(BRMQ1_abs_raw, "data/BRMQ1_abs_raw.csv", row.names = FALSE)

if (!is.null(BRMQ1_abs)) {
  BRMQ1_abs$DateTime <- format(BRMQ1_abs$DateTime, "%Y-%m-%d %H:%M:%S")
  write.csv(BRMQ1_abs, "data/BRMQ1_abs.csv", row.names = FALSE)
}

#######################
#### Save in Drive ####
#######################
# Same "merged timestamps" folder BRMQ4_params.csv/BRM06_params.csv etc
# already go to (00_BR_merge_timestamps.R), so 01_BR_merge_params_and_abs.R
# can eventually pick BRMQ1 up alongside them with one more block, the same
# way it already handles BRMQ4 and BRM06.
drive_folder_id <- "1ROxMHt-mzsMrym5Gisi2mNM9RaepuhZc"

drive_put(media = "data/BRMQ1_params.csv", path = as_id(drive_folder_id))
drive_put(media = "data/BRMQ1_abs_raw.csv", path = as_id(drive_folder_id))
if (!is.null(BRMQ1_abs)) {
  drive_put(media = "data/BRMQ1_abs.csv", path = as_id(drive_folder_id))
} else {
  message("Skipping BRMQ1_abs.csv upload -- no compensated fingerprint data was found in any file.")
}
