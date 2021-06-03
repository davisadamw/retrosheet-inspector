library(tidyverse)

# team names first ####
load_team_names <- function(team_file) {
  
  year <- str_extract(team_file, "[0-9]{4}$")
  
  # read the teams file and attach a year column
  read_csv(team_file, 
           col_names = c("team_id", "league", "city", "nickname"),
           col_types = "cccc")  %>% 
    mutate(year = year, .before = 1)
}

all_teams <- list.files("../Retrosheet Events/", 
                        pattern = "TEAM[0-9]{4}",
                        full.names = TRUE,
                        recursive = TRUE) %>% 
  map_dfr(load_team_names)

# then player names ####
load_player_names <- function(roster_file) {
  
  year <- str_extract(roster_file, "[0-9]{4}(?=.ROS$)")
  
  # read the roster file and attach a year column
  read_csv(roster_file,
           col_names = c("player_id", "last_name", "first_name", 
                         "bat", "throw", "team_id", "main_position"),
           col_types = "ccccccc") %>% 
    mutate(year = year, .before = 1)
}

all_players <- list.files("../Retrosheet Events/", 
                          pattern = "[0-9]{4}.ROS",
                          full.names = TRUE,
                          recursive = TRUE) %>% 
  map_dfr(load_player_names)

# simplify players slightly
all_players_simplified <- all_players %>% 
  with_groups(player_id:team_id, summarize,
              first_year = min(year),
              last_year  = max(year),
              positions  = paste(unique(main_position), collapse = ";"))

# store results
all_teams %>% 
  write_csv("Players_Teams/all_teams.csv")

all_players %>% 
  write_csv("Players_Teams/all_players_retrosheet.csv")

