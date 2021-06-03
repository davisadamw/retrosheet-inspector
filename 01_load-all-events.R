library(tidyverse)
source("retrosheet-helpers.R")

# process event logs one decade at a time since that matches the folder structure
# grab all folders in Retrosheet Events and drop the top-level folder
decade_dirs <- list.dirs("../Retrosheet Events/", full.names = TRUE)[-1]

# simple function to read all event files in each directory
read_all_lines <- function(directory) {
  
  # pattern <- "E[DV][NA]$" # run event logs and deduced logs 
  pattern <- "EV[NA]$" # run complete event logs only
  
  # identify all the files to read
  event_files <- list.files(directory, pattern = pattern, recursive = TRUE, full.names = TRUE)
  
  # read all identified files and smush into a single character vector
  unlist(map(event_files, read_lines))
}

# and a simple function to read the data, process, and write to disk
process_all_lines <- function(directory) {
  start_dir <- str_extract(directory, "[0-9]{4}seve")
  out_filename <- file.path("All_Events", paste0(start_dir, "nts.csv"))
  
  cat("reading ", start_dir, "\n")
  
  # read the raw data
  all_event_rows <- read_all_lines(directory)
  
  cat("processing ", start_dir, "\n")
  
  # process the data
  events_processed <- process_retrosheet_events(all_event_rows)
  
  cat("writing ", start_dir, "\n")
  
  # write to disk
  write_csv(events_processed, out_filename)
}

# run all decades
walk(decade_dirs, process_all_lines)

