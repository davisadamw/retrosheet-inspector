library(tidyverse)

# function to read the event and grab home runs
grab_homers <- function(file) {
  
  # load play data
  all_plays <- read_csv(file)
  
  # grab home runs ... looks like all events that start with an H not immediately followed by a P
  all_plays %>% 
    filter(str_starts(event, "H(?!P)"))
}

all_hrs <- list.files("All_Events", full.names = TRUE) %>% 
  map_dfr(grab_homers)

all_hrs %>% 
  write_csv("Players_Teams/all_homers.csv")
