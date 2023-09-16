library(ggplot2)
library(baseballr)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

source('../api_keys.R')

# make api request url
odds_api_request <- paste0(
  'https://api.the-odds-api.com/v4/sports/',
  'baseball_mlb/',
  'odds/',
  '?apiKey=',
  odds_api_key,
  '&regions=us,us2',
  '&markets=',
  'h2h,totals'
)

# get api response
res <- httr::GET(odds_api_request)

# process response
my_odds <- fromJSON(rawToChar(res$content), flatten = TRUE) %>%
  mutate(commence_time = with_tz(as_datetime(commence_time), 'US/Eastern'))

# get only today
todays_odds <- my_odds %>%
  filter(date(commence_time) == today())

# number of games today
n_games_today <- nrow(todays_odds)

moneylines_df <- data.frame()

# loop over games
i_game <- 1
over_under_all_points <- c()
away_team <- todays_odds$away_team[i_game]
home_team <- todays_odds$home_team[i_game]
for (i_bookie in 1:length(my_odds$bookmakers[[i_game]]$markets)) {
  if (length(my_odds$bookmakers[[i_game]]$markets[[i_bookie]]$outcomes) == 2) {
    over_under_all_points <- append(
      over_under_all_points,
      my_odds$bookmakers[[i_game]]$markets[[i_bookie]]$outcomes[[2]]$point[1]
    )
  }
}
over_under_avg <- mean(over_under_all_points)
moneylines_df <- rbind(
  moneylines_df,
  data.frame(
    team = c(away_team, home_team),
    moneyline = c(over_under_avg, over_under_avg)
  )
)

odds_api_request <- paste0(
  'https://api.the-odds-api.com/v4/sports/?apiKey=',
  odds_api_key
)

res <- httr::GET(odds_api_request)

res$content

my_sports <- fromJSON(rawToChar(res$content), flatten = TRUE)

my_odds_df <- make_odds_df()

length(my_odds$bookmakers[[1]]$markets)

names(my_odds)

for (i in 1:length(my_odds$bookmakers[[3]]$markets)) {
  print(my_odds$bookmakers[[3]]$markets[[i]]$outcomes[[1]]$price[1])
}

my_odds$bookmakers[[4]]$markets[[1]]$outcomes[[1]]

my_odds$bookmakers[[1]] %>%
  separate(
    markets,
    into = as.character(seq(14)),
    sep = ','
    )

# kauffman stadium. home-to-center vector is 45 degrees from north
# wind direction is 30 degrees from north
# 45 - 30 = 15 degrees
# 15 degrees is in from left-center
# 
# wind dir is 60 degrees
# 45 - 60 = -15
# -15 is in from right-center
# 
# 0 is in from center
# -15 to 15 is in from center
# -45 to -15 is in from right
# 15 to 45 is in from right
# 
# wind direction is 225 degrees from north
# 45 - 225 = -180
# -180 is out to center
# 
# wind direction is 240 from north
# 45 - 240 = -195
# -195 is out to right-center
# 
# -165 is out to left-center
# 
# -165 to -195 is out to center
# -195 to -225 is out to right
# -135 to -165 is out to left
# 
# what is between out to left and in from right
# out to left is -135 to -165
# in from right is 15 to 45
# 
# if it is negative, add 360
# 
# dealing just with positive degrees between 0 and 360...
# 
# gt 345 or lt 15 is in from center
# gt 15 and lt 45 is in from right
# gt 45 and lt 90 is cross-wind in from right
# gt 90 and lt 135 is cross-wind out to left
# gt 135 and lt 165 is out to left
# gt 165 and lt 195 is out to center
# gt 195 and lt 225 is out to right
# gt 225 and lt 270 is cross-wind out to right
# gt 270 and lt 315 is cross-wind in from left
# gt 315 and lt 345 is in from left

team_res <- bref_team_results('CLE', 2023)

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

bref_team_results("LAA", 2023)

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

my_game_pks <- mlb_game_pks('2023-09-13')

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