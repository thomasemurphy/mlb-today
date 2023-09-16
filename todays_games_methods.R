library(baseballr)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

latlong <- read_csv('../baseball_latlong.csv')

hitting_stats_23_skinny <- mlb_stats(
  stat_type = 'season',
  stat_group = 'hitting',
  season = 2023,
  player_pool = 'All'
) %>%
  mutate(k_rate = strike_outs / plate_appearances) %>%
  select(player_id, plate_appearances, k_rate, avg, obp, slg, ops)

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

get_wind_dir_str <- function(park_home_to_center, wind_deg_from_north) {
  park_dir <- wind_deg_from_north - park_home_to_center
  if (park_dir < 0) {
    park_dir <- park_dir + 360
  }
  wind_dir_str <- ifelse(
    (park_dir >= 345) | (park_dir < 15),
    'in from center',
    ifelse(
      park_dir <= 45,
      'in from right',
      ifelse(
        park_dir <= 90,
        'cross-inward from right',
        ifelse(
          park_dir <= 135,
          'cross-outward to left',
          ifelse(
            park_dir <= 165,
            'out to left',
            ifelse(
              park_dir <= 195,
              'out to center',
              ifelse(
                park_dir <= 225,
                'out to right',
                ifelse(
                  park_dir <= 270,
                  'cross-outward to right',
                  ifelse(
                    park_dir <= 315,
                    'cross-inward from left',
                    'in from left'
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  wind_dir_str
}

make_odds_df <- function(for_date = today()) {
  
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
    filter(date(commence_time) == for_date)
  
  # number of games today
  n_games_today <- nrow(todays_odds)
  
  moneylines_df <- data.frame()
  
  # loop over games
  for (i_game in 1:n_games_today) {
    away_team <- todays_odds$away_team[i_game]
    home_team <- todays_odds$home_team[i_game]
    away_team_all_odds <- c()
    home_team_all_odds <- c()
    over_under_all_points <- c()
    for (i_bookie in 1:length(my_odds$bookmakers[[i_game]]$markets)) {
      away_team_all_odds <- append(
        away_team_all_odds,
        my_odds$bookmakers[[i_game]]$markets[[i_bookie]]$outcomes[[1]] %>%
          filter(name == away_team) %>%
          pull(price)
      )
      home_team_all_odds <- append(
        home_team_all_odds,
        my_odds$bookmakers[[i_game]]$markets[[i_bookie]]$outcomes[[1]] %>%
          filter(name == home_team) %>%
          pull(price)
      )
      if (length(my_odds$bookmakers[[i_game]]$markets[[i_bookie]]$outcomes) == 2) {
        over_under_all_points <- append(
          over_under_all_points,
          my_odds$bookmakers[[i_game]]$markets[[i_bookie]]$outcomes[[2]]$point[1]
        )
      }
    }
    away_team_mean_odds <- mean(away_team_all_odds)
    home_team_mean_odds <- mean(home_team_all_odds)
    over_under_avg <- mean(over_under_all_points)
    moneylines_df <- rbind(
      moneylines_df,
      data.frame(
        team = c(away_team, home_team),
        payout = c(away_team_mean_odds, home_team_mean_odds),
        over_under = c(over_under_avg, over_under_avg)
      )
    )
  }
  moneylines_df <- moneylines_df %>%
    mutate(
      return = payout - 1,
      moneyline_base = ifelse(
        return < 1,
        1 / return,
        return
      ),
      moneyline_text = ifelse(
        return < 1,
        paste0('-', round(moneyline_base, 2) * 100),
        paste0('+', round(moneyline_base, 2) * 100)
      )
    )
  moneylines_df
}