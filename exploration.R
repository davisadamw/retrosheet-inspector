library(tidyverse)

practice_file <- "Practice_Data/2020eve/2020SFN.EVN"

# each event file contains all of a team's home games
# start by reading all the data at once
sfn_events_raw <- read_table(practice_file, col_names = FALSE)

# different event types have different fields
# for substitutions and game ID to line up, we need to know which event happens when
sf_events_er <- sfn_events_raw %>% 
  separate(X1, into = c("event_type", "rest"), sep = ",", extra = "merge")

# all the info we need is in plays + who's pitching (start+subs involving position 1)
# we also want to keep game ID
sf_events_filtered <- sf_events_er %>% 
  filter(event_type == "id" | 
           event_type == "info" |
           event_type == "start" | 
           event_type == "sub" | 
           event_type == "play")

# event cumulative totals will be used to link each play to a specific game and pitcher
sf_events_gameid <- sf_events_filtered %>% 
  mutate(game_no = cumsum(event_type == "id"),
         is_sub = event_type == "start" | event_type == "sub",
         home_pitcher_sub = is_sub & str_ends(rest, "1,[0-9],1"),
         road_pitcher_sub = is_sub & str_ends(rest, "0,[0-9],1"),
         home_pitcher_no  = cumsum(home_pitcher_sub),
         road_pitcher_no  = cumsum(road_pitcher_sub))

# game events are easy
games_only <- sf_events_gameid %>% 
  filter(event_type == "id") %>% 
  select(game_no, game_id = rest)

# other game info
info_only <- sf_events_gameid %>% 
  filter(event_type == "info") %>% 
  separate(rest, into = c("var", "val"), sep = ",") %>% 
  pivot_wider(id_cols = game_no, names_from = var, values_from = val) %>% 
  select(game_no, visteam, hometeam, date, number, starttime)

# pull out pitcher info
# the key piece here is to make sure home pitchers get matched up with road hitters and vice versa
pitchers_only <- sf_events_gameid %>% 
  filter(home_pitcher_sub | road_pitcher_sub) %>% 
  # we only need pitcher id and home/road indicator
  separate(rest,
           into = c("pitcher_id", NA, "pitcher_home_road", NA, NA), 
           sep = ",",
           convert = TRUE) %>% 
  # in order to match pitchers and hitters correctly, we switch the home_road indicator
  # grab only the identifier for the relevant pitcher
  mutate(batter_home_road = 1 - pitcher_home_road,
         pitcher_no = if_else(pitcher_home_road == 0, road_pitcher_no, home_pitcher_no)) %>% 
  select(pitcher_id, game_no, batter_home_road, pitcher_no)

# pull out plays and separate into columns /// each will have an identifying game and pitcher event  
plays_only <- sf_events_gameid %>% 
  filter(event_type == "play") %>% 
  mutate(play_no = row_number()) %>% 
  # we can safely ignore count and pitch info here (cols 4 and 5)
  separate(rest, 
           into = c("inning", "batter_home_road", "batter_id", NA, NA, "event"),
           sep = ",",
           convert = TRUE) %>% 
  # grab the correct pitcher_no
  mutate(pitcher_no = if_else(batter_home_road == 0, home_pitcher_no, road_pitcher_no)) %>% 
  select(inning, batter_home_road, batter_id, event, game_no, play_no, pitcher_no)

# join play events to pitchers and game identifiers and drop all NP events
plays_all_info <- plays_only %>% 
  left_join(games_only, by = "game_no") %>% 
  left_join(info_only, by = "game_no") %>% 
  left_join(pitchers_only, by = c("game_no", "batter_home_road", "pitcher_no")) %>% 
  select(game_id, visteam, hometeam, date, number, starttime,
         inning, batter_home_road, batter_id, pitcher_id, event) %>% 
  filter(event != "NP")

# check for home runs ... looks like all events that start with an H not immediately followed by a P
plays_maybe_hr <- plays_all_info %>% 
  mutate(maybe_hr = str_starts(event, "H(?!P)")) %>% 
  filter(maybe_hr) %>% 
  pull(event)
  



