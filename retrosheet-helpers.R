library(tidyverse)

# event_test <- read_lines("../Retrosheet Events/2010seve.zip")

# function to take a vector of event play lines into a list of play events with pitcher and game info attached to each one
process_retrosheet_events <- function(event_vector) {
  
  # different event types have different fields
  # for substitutions and game ID to line up, we need to know which event happens when
  events_mat <- str_split_fixed(event_vector, pattern = ",", n = 2)
  colnames(events_mat) <- c("event_type", "rest")
  
  # convert the events into a tibble and remove irrelevent events
  events_filtered <- events_mat %>% 
    as_tibble() %>% 
    filter(event_type == "id" | 
             event_type == "info" |
             event_type == "start" | 
             event_type == "sub" | 
             event_type == "play")
  
  # event cumulative totals will be used to link each play to a specific game and pitcher
  events_ids <- events_filtered %>% 
    mutate(game_no = cumsum(event_type == "id"),
           is_sub = event_type == "start" | event_type == "sub",
           home_pitcher_sub = is_sub & str_ends(rest, "1,[0-9],1"),
           road_pitcher_sub = is_sub & str_ends(rest, "0,[0-9],1"),
           home_pitcher_no  = cumsum(home_pitcher_sub),
           road_pitcher_no  = cumsum(road_pitcher_sub))
 
  # game events are easy
  games_only <- events_ids %>% 
    filter(event_type == "id") %>% 
    select(game_no, game_id = rest)
  
  # grab other game info we care about
  info_only <- events_ids %>% 
    filter(event_type == "info") %>% 
    separate(rest, into = c("var", "val"), sep = ",", extra = "merge") %>% 
    filter(var == "visteam" |
             var == "hometeam" |
             var == "site" |
             var == "date" |
             var == "number" |
             var == "starttime") %>% 
    pivot_wider(id_cols = game_no, 
                names_from = var, 
                values_from = val,
                values_fn = paste)

  # pull out pitcher info
  # the key piece here is to make sure home pitchers get matched up with road hitters and vice versa
  pitchers_only <- events_ids %>% 
    filter(home_pitcher_sub | road_pitcher_sub) %>% 
    # we only need pitcher id and home/road indicator
    separate(rest,
             into = c("pitcher_id", NA, "pitcher_home_road", NA, NA), 
             sep = ",") %>% 
    # in order to match pitchers and hitters correctly, we switch the home_road indicator
    # grab only the identifier for the relevant pitcher
    mutate(batter_home_road = if_else(pitcher_home_road == "0", "H", "R"),
           pitcher_no = if_else(pitcher_home_road == "0", road_pitcher_no, home_pitcher_no)) %>% 
    select(pitcher_id, game_no, batter_home_road, pitcher_no)
  
  
  # pull out plays and separate into columns /// each will have an identifying game and pitcher event  
  plays_only <- events_ids %>% 
    filter(event_type == "play") %>% 
    # we can safely ignore count and pitch info here (cols 4 and 5)
    separate(rest, 
             into = c("inning", "home_road", "batter_id", "count", "pitches", "event"),
             sep = ",") %>% 
    # grab the correct pitcher_no
    mutate(batter_home_road = if_else(home_road == "1", "H", "R"),
           pitcher_no = if_else(batter_home_road == "H", road_pitcher_no, home_pitcher_no)) %>% 
    select(inning, batter_home_road, batter_id, event, game_no, pitcher_no)
  
  # join play events to pitchers and game identifiers and drop all NP events
  plays_all_info <- plays_only %>% 
    left_join(games_only, by = "game_no") %>% 
    left_join(info_only, by = "game_no") %>% 
    left_join(pitchers_only, by = c("game_no", "batter_home_road", "pitcher_no")) %>% 
    select(game_id, visteam, hometeam, date, number, starttime,
           inning, batter_home_road, batter_id, pitcher_id, event) %>% 
    filter(event != "NP")
  
}
