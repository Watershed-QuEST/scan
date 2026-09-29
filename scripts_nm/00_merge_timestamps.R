##==============================================================================
## Project: QuEST
## Script to merge scan files in one (using timestamp) for Santa Fe watershed
##==============================================================================

library(readxl) #to read excel 
library(googledrive)
library(dplyr)

########################################
#### Clear folders that we will use ####
########################################
# list and delete all files in the folder
files <- list.files(path = "googledrive", full.names = TRUE)
file.remove(files)

##########################
#### Import scan data ####
##########################
#### list and download all files in the folder ####
# this is the "raw" folder
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1G6v6i0tLIUthghWr0XL_wi_MqGcqPLgZ")
# list all CSV files in the folder
scan_csvs <- googledrive::drive_ls(path = scan)
2

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
  header <- read_excel(local_path, skip = 1, n_max = 1, col_names = FALSE)
  # convert the header to a character vector and clean empty names
  col_names <- as.character(unlist(header[1, ]))
  col_names[col_names == ""] <- paste0("X", seq_along(col_names[col_names == ""]))
  
  # read the data starting from row 4 using the header as column names
  data <- read_excel(local_path, skip = 4, col_names = col_names)
  
  # store the data in the list
  scan_list[[scan_csvs$name[i]]] <- data
}

head(scan_list)

####################################
#### Combine data for each site ####
####################################
# loop through each data frame in the list to change DateTime column name
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  
  # change names for easier handling
  colnames(df)[1] ="DateTime"
  
  # Update the data frame in the list
  scan_list[[i]] <- df
}

# site names
site_names <- c("USF12", "USF20", "USF21")

# group files in `scan_list` by matching `site_names` in file names

scan_list_by_site <- lapply(site_names, function(site) {
  # names(scan_list) gives the names of all files in scan_list.
  site_files <- names(scan_list)[grepl(site, names(scan_list))] 
  # grep checks if the current site (e.g., USF12) appears in each file name in scan_list. 
  # this returns a logical vector (TRUE for matches, FALSE otherwise).
  scan_list[site_files] # select only the files for this site
  # the [ ] indexing selects only the file names where the match is TRUE.
})

# name the list by site
names(scan_list_by_site) <- site_names

# TEMPORARY REMOVE 20 AND 21
# scan_list_by_site <- scan_list_by_site[-c(2,3)]

# combine data for each site
combined_by_site <- lapply(scan_list_by_site, function(site_data_list) {
  # bind rows of all data frames for the site
  bind_rows(site_data_list) %>%
    arrange(DateTime) %>%  # ensure chronological order if 'DateTime' exists
    distinct(DateTime, .keep_all = TRUE) # remove duplicates
})

##############################
#### Save combined files  ####
##############################
# ensure DateTime column is properly formatted
combined_by_site <- lapply(combined_by_site, function(df) {
  df$DateTime <- format(df$DateTime, "%Y-%m-%d %H:%M:%S") # ensure consistent format
  return(df)
})

lapply(names(combined_by_site), function(site) {
  write.csv(combined_by_site[[site]], file.path("data", paste0(site, "_params.csv")))
})

lapply(names(combined_by_site), function(site) {
  file <- paste0("data/", site, "_params.csv")
  # this is the "merged timestamps" folder
  drive_folder_id <- "1-dUxVn1hBWy2MpHeIjVt-2QSujpVhijy"
  # Upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})

##==============================================================================
## Now we need to do the same thing but for the compensated abs tab
## abs file is in the same excel in second tab of file
##==============================================================================

##########################
#### Import scan data ####
##########################
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
  header <- read_excel(local_path, sheet = 2, skip = 1, n_max = 1, col_names = FALSE)
  # convert the header to a character vector and clean empty names
  col_names <- as.character(unlist(header[1, ]))
  col_names[col_names == ""] <- paste0("X", seq_along(col_names[col_names == ""]))
  
  # read the data starting from row 4 using the header as column names
  data <- read_excel(local_path,sheet = 2, skip = 4, col_names = col_names)
  
  # Store the data in the list
  scan_list[[scan_csvs$name[i]]] <- data
}

####################################
#### Combine data for each site ####
####################################
# change DateTime column name
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  
  # change names for easier handling
  colnames(df)[1] ="DateTime"
  
  # update the data frame in the list
  scan_list[[i]] <- df
}

# site names
site_names <- c("USF12", "USF20", "USF21")

# group files in `scan_list` by matching `site_names` in file names
scan_list_by_site <- lapply(site_names, function(site) {
  # names(scan_list) gives the names of all files in scan_list.
  site_files <- names(scan_list)[grepl(site, names(scan_list))] 
  # grep checks if the current site (e.g., SSM01) appears in each file name in scan_list. 
  # this returns a logical vector (TRUE for matches, FALSE otherwise).
  scan_list[site_files] # select only the files for this site
  # the [ ] indexing selects only the file names where the match is TRUE.
})

# name the list by site
names(scan_list_by_site) <- site_names

# TEMPORARY REMOVE 20 AND 21
#scan_list_by_site <- scan_list_by_site[-c(2,3)]

# makes every non DateTime column to numeric
scan_list_by_site <- lapply(scan_list_by_site, function(site_data_list) {
  lapply(site_data_list, function(df) {
    df %>% mutate(across(-DateTime, ~ suppressWarnings(as.numeric(.))))
  })
})
# combine data for each site
combined_by_site_compfing <- lapply(scan_list_by_site, function(site_data_list) {
  # bind rows of all data frames for the site
  bind_rows(site_data_list) %>%
    arrange(DateTime) %>%  # ensure chronological order if 'DateTime' exists
    distinct(DateTime, .keep_all = TRUE) # remove duplicates
})

##############################
#### Save combined files  ####
##############################
# ensure DateTime column is properly formatted
combined_by_site_compfing <- lapply(combined_by_site_compfing, function(df) {
  df$DateTime <- format(df$DateTime, "%Y-%m-%d %H:%M:%S") # ensure consistent format
  return(df)
})

lapply(names(combined_by_site_compfing), function(site) {
  write.csv(combined_by_site_compfing[[site]], file.path("data", paste0(site, "_abs.csv")))
})

lapply(names(combined_by_site_compfing), function(site) {
  file <- paste0("data/", site, "_abs.csv")
  # this is the "merged timestamps" folder
  drive_folder_id <- "1-dUxVn1hBWy2MpHeIjVt-2QSujpVhijy"
  # upload file to the specified Google Drive folder
  drive_put(
    media = file,
    path = as_id(drive_folder_id)
  )
})

