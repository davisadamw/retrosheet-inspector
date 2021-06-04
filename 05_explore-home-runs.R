library(tidyverse)

# load home runs and player info
all_hrs <- read_csv("Players_Teams/all_homers.csv")

player_info <- read_csv("Players_Teams/all_players_alldata.csv")

player_weights_heights <- player_info %>% 
  mutate(name = paste(first_name, last_name)) %>% 
  distinct(player_id, name, weight, height)

# attach weights 
hr_info <- all_hrs %>% 
  left_join(player_weights_heights,
            by = c("batter_id" = "player_id")) %>% 
  left_join(player_weights_heights, 
            by = c("pitcher_id" = "player_id"),
            suffix = c("_batter", "_pitcher"))


hr_comps <- hr_info %>% 
  select(visteam:date, inning, ends_with("batter"), ends_with("pitcher")) %>% 
  relocate(name_batter, name_pitcher) %>% 
  mutate(tot_weight = weight_batter + weight_pitcher,
         tot_height = height_batter + height_pitcher,
         dif_weight = abs(weight_batter - weight_pitcher),
         dif_height = abs(height_batter - height_pitcher))

top_5 <- function(data, table_var, n = 5) {
  data %>% 
    relocate({{ table_var }}) %>% 
    arrange(desc({{ table_var }})) %>% 
    slice_max({{ table_var }}, n = n)
}

hr_comps %>% top_5(tot_weight)
  
hr_comps %>% top_5(tot_height)

hr_comps %>% top_5(dif_weight)

hr_comps %>% top_5(dif_height)

hr_comps %>% top_5(tot_weight)

hr_comps %>% 
  write_csv("hr_height_weight_comps.csv")
