library(tidyverse)
library(Lahman)

# load retrosheet player data
retro_players <- read_csv("Players_Teams/all_players_retrosheet.csv")

# attach heights and weights from the Lahman database, which have retrosheet IDs
lahman_people <- Lahman::People %>% 
  select(retroID, birthDate, weight, height)

retro_players %>% 
  left_join(lahman_people, by = c("player_id" = "retroID")) %>% 
  write_csv("Players_Teams/all_players_alldata.csv")

