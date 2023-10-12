#load in packages
library(shiny)
library(tidyverse)
library(nflfastR)
library(nflverse)

#load in data
seasons = 2023
pbp_data = load_pbp(seasons)

pbp_data = 
  pbp_data %>%
    filter(play_type == 'run' & qb_scramble == 0 & season_type == 'REG' & penalty != 1) %>%
    select(play_id, yards_gained, tackle_for_loss_1_player_id, tackle_for_loss_2_player_id, 
           solo_tackle_1_player_id, solo_tackle_2_player_id, assist_tackle_1_player_id, 
           assist_tackle_2_player_id, assist_tackle_3_player_id, assist_tackle_4_player_id,
           solo_tackle, tackled_for_loss, assist_tackle,
           week, season
           )

#solo tackle dataframe
solo_tackle_frame = 
  pbp_data %>%
  filter(solo_tackle == 1 & !is.na(solo_tackle_1_player_id)) %>%
  select(
    play_id,
    yards_gained,
    solo_tackle_1_player_id,
    week,
    season
  )

#assist tackle frames
assist_tackle_frame_1 = 
  pbp_data %>%
  filter(assist_tackle == 1 & !is.na(assist_tackle_1_player_id)) %>%
  select(
    play_id,
    yards_gained,
    assist_tackle_1_player_id,
    week,
    season
  )

assist_tackle_frame_2 = 
  pbp_data %>%
  filter(assist_tackle == 1 & !is.na(assist_tackle_2_player_id)) %>%
  select(
    play_id,
    yards_gained,
    assist_tackle_2_player_id,
    week,
    season
  )

assist_tackle_frame_3 = 
  pbp_data %>%
  filter(assist_tackle == 1 & !is.na(assist_tackle_3_player_id)) %>%
  select(
    play_id,
    yards_gained,
    assist_tackle_3_player_id,
    week,
    season
  )

assist_tackle_frame_4 = 
  pbp_data %>%
  filter(assist_tackle == 1 & !is.na(assist_tackle_4_player_id)) %>%
  select(
    play_id,
    yards_gained,
    assist_tackle_4_player_id,
    week,
    season
  )

#tfl data frames
tfl_frame_1 = 
  pbp_data %>%
  filter(tackled_for_loss == 1 & !is.na(tackle_for_loss_1_player_id)) %>%
  select(
    play_id,
    yards_gained,
    tackle_for_loss_1_player_id,
    week,
    season
  )

tfl_frame_2 = 
  pbp_data %>%
  filter(tackled_for_loss == 1 & !is.na(tackle_for_loss_2_player_id)) %>%
  select(
    play_id,
    yards_gained,
    tackle_for_loss_2_player_id,
    week,
    season
  )

#merge all frames
binded_tackle_frame =
  rbind(solo_tackle_frame, 
        assist_tackle_frame_1, assist_tackle_frame_2, assist_tackle_frame_3, assist_tackle_frame_4,
        tfl_frame_1, tfl_frame_2, use.names = FALSE) %>% 
    rename(tackler_id = solo_tackle_1_player_id)



###
#player info
player_info = 
  load_rosters() %>%
    select(season, team, position, full_name, first_name, last_name, gsis_id)


#join binded_tackle_frame with player_info 
complete_base_frame = left_join(binded_tackle_frame, player_info, by = c("tackler_id" = "gsis_id", "season" = "season"))


### aggregation
complete_base_frame %>%
  group_by(tackler_id, full_name, position) %>%
  summarize(adort = mean(yards_gained),
            num_tackles = n()) %>%
  ungroup() %>%
  view()
  

## filter by position, minimum tackles, range of tackle









