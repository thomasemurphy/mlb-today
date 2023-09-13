library(ggplot2)
library(baseballr)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

my_api_key <- 'my_api_key'

kelvin_to_f <- function(kelvin) {
  (kelvin - 273) * 9/5 + 32
}

latlong <- read_csv('../baseball_latlong.csv')

home_team <- 'CHC'

team_loc <- latlong %>%
  filter(team == home_team)

my_ts <- force_tz(
  ymd_hm(
    paste0(
      2023,
      '-',
      9,
      '-',
      8,
      ' ',
      13,
      ':',
      20
    )
  ),
  tzone = 'US/Central'
)

api_query <- paste0(
  'https://api.openweathermap.org/data/3.0/onecall/timemachine',
  '?lat=',
  team_loc$lat,
  '&lon=',
  team_loc$long,
  '&dt=',
  as.integer(my_ts),
  '&units=imperial',
  '&APPID=',
  my_api_key
)

res <- httr::GET(api_query)

my_data <- fromJSON(rawToChar(res$content), flatten = TRUE)

my_data$data$weather[[1]]

kelvin_to_f(my_data$data$temp)

bref_team_results("NYM", 2015)

team_res <- bref_team_results('Houston Astros', 2023)

to_show <- team_res %>%
  mutate(
    full_date = paste(gsub("\\s*\\([^\\)]+\\)", "", Date), Year),
    game_dt = mdy(full_date)
    ) %>%
  arrange(game_dt) %>%
  slice_tail(n = 10) %>%
  select(Date, H_A, `D/N`, Opp, Result, R, RA)

sched <- mlb_schedule(season = 2023, level_ids = "1")

today_games <- sched %>% filter(date == '2023-08-25')

my_game_pks <- mlb_game_pks('2023-09-08')

my_game_pks %>% data.frame()

batting_orders <- mlb_batting_orders(716886)

stats_23_skinny <- mlb_stats(
  stat_type = 'season',
  stat_group = 'hitting',
  season = 2023,
  player_pool = 'All'
  ) %>%
  mutate(k_rate = strike_outs / plate_appearances) %>%
  select(player_id, plate_appearances, k_rate, avg, obp, slg, ops)

batting_orders %>%
  left_join(
    stats_23_skinny,
    by = c('id' = 'player_id')
    )

pitching_stats_23_skinny <- mlb_stats(
  stat_type = 'season',
  stat_group = 'pitching',
  season = 2023,
  player_pool = 'All'
) %>%
  rename(
    c('ip' = 'innings_pitched',
      'k_per_9' = 'strikeouts_per9inn')
  ) %>%
  mutate(
    ip = as.numeric(ip),
    ra = runs / ip * 9
  ) %>%
  select(player_id, ip, ra, k_per_9, whip, ops)

mlb_probables(
  game_pk = 716886
) %>%
  left_join(
    pitching_stats_23_skinny,
    by = c('id' = 'player_id')
  )