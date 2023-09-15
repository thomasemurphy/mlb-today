library(shiny)
library(shinyWidgets)
library(ggplot2)
library(baseballr)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

source('../weather_api_key.R')

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

ui <- fluidPage(
  
  # tags$head(
  #   tags$style(
  #     type = "text/css",
  #     "label{
  #       display: table-cell;
  #       text-align: center;
  #       vertical-align: middle;
  #       }
  #     .form-group {
  #       display: table-row;
  #     }"
  #     )
  # ),
  
  tags$head(
    tags$style(
      type = "text/css",
      "label{
        display: table-cell;
        text-align: center;
        vertical-align: middle;
        }"
    )
  ),
  
  dateInput(
    'date',
    label = h5('Date'),
    value = today()
  ),
  
  tableOutput("games"),
  
  fluidRow(
    column(
      8,
      h5('Weather'),
      textOutput("weather")
    )
  ),
  
  fluidRow(
    column(
      6,
      tags$h5((htmlOutput("visitors_last_10_header"))),
      tableOutput('visitors_last_10')
      ),
    column(
      6,
      tags$h5((htmlOutput("home_last_10_header"))),
      tableOutput('home_last_10')
    ),
  ),
  
  fluidRow(
    column(
      6,
      tags$h5((htmlOutput("home_starter_header"))),
      tableOutput('home_starter')
    ),
    column(
      6,
      tags$h5((htmlOutput("away_starter_header"))),
      tableOutput('away_starter')
    )
  ),
  
  fluidRow(
    column(
      6,
      tags$h5((htmlOutput("away_lineup_header"))),
      tableOutput('away_batting_order')
    ),
    column(
      6,
      tags$h5((htmlOutput("home_lineup_header"))),
      tableOutput('home_batting_order')
    )
  ),
  
  fluidRow(
    column(
      6,
      tableOutput('away_lineup_summary')
    ),
    column(
      6,
      # tags$h5((htmlOutput("home_lineup_header"))),
      tableOutput('home_lineup_summary')
    )
  )
)

server <- function(input, output, session) {
  
  # get game_pks for this date
  get_game_pks <- reactive({
    mlb_game_pks(input$date) %>%
      mutate(
        local_datetime = with_tz(as_datetime(gameDate), 'US/Eastern'),
        game_time_display = format(
          as.POSIXct(local_datetime),
          format = '%H:%M'
          )
      ) %>%
      mutate(
        game_str = paste0(
          teams.away.team.name,
          ' @ ',
          teams.home.team.name,
          ' (',
          game_time_display,
          ')'
        )
      )
  })
  
  # get weather at this place and time
  get_weather <- reactive({
    game_pk_info <- get_game_pks() %>%
      filter(game_str == input$selected_game_str)
    lat_long_row <- latlong %>%
      filter(team_name == game_pk_info$teams.home.team.name[1])
    start_ts <- with_tz(
      as_datetime(
        game_pk_info$gameDate[1]
        ),
      tzone = lat_long_row$tzone
      )
    api_query <- paste0(
      'https://api.openweathermap.org/data/3.0/onecall/timemachine',
      '?lat=',
      lat_long_row$lat,
      '&lon=',
      lat_long_row$long,
      '&dt=',
      as.integer(start_ts),
      '&units=imperial',
      '&APPID=',
      my_api_key
    )
    api_response <- httr::GET(api_query)
    weather_df <- fromJSON(rawToChar(api_response$content), flatten = TRUE)$data
    weather_str <- paste0(
      round(weather_df$temp[1], 0),
      ' degrees at first pitch, ',
      'wind ',
      round(weather_df$wind_speed[1], 0),
      ' mph ',
      get_wind_dir_str(lat_long_row$home_center_deg, weather_df$wind_deg),
      ', ',
      weather_df$weather[[1]]$description[1]
    )
    weather_str
  })
  
  # get starting lineups
  get_batting_orders <- reactive({
    mlb_batting_orders(
      game_pk = get_game_pks() %>%
        filter(game_str == input$selected_game_str) %>%
        pull(game_pk)
    ) %>%
      left_join(
        hitting_stats_23_skinny,
        by = c('id' = 'player_id')
      )
  })
  
  # get pitchers
  get_probable_starters <- reactive({
    mlb_probables(
      game_pk = get_game_pks() %>%
        filter(game_str == input$selected_game_str) %>%
        pull(game_pk)
    ) %>%
      left_join(
        pitching_stats_23_skinny,
        by = c('id' = 'player_id')
      )
  })
  
  # get away team name
  get_away_team_name <- reactive({
    get_game_pks() %>%
      filter(game_str == input$selected_game_str) %>%
      distinct(teams.away.team.name) %>%
      pull(teams.away.team.name)
  })
  
  get_away_last_10_header <- reactive({
    paste(get_away_team_name(), ' last 10 games')
  })
  
  get_home_last_10_header <- reactive({
    paste(get_home_team_name(), ' last 10 games')
  })
  
  get_away_starter_header <- reactive({
    paste(get_away_team_name(), ' starter')
  })
  
  get_home_starter_header <- reactive({
    paste(get_home_team_name(), ' starter')
  })
  
  get_away_lineup_header <- reactive({
    paste(get_away_team_name(), ' batting order')
  })
  
  get_home_lineup_header <- reactive({
    paste(get_home_team_name(), ' batting order')
  })
  
  # get home team name
  get_home_team_name <- reactive({
    get_game_pks() %>%
      filter(game_str == input$selected_game_str) %>%
      distinct(teams.home.team.name) %>%
      pull(teams.home.team.name)
  })
  
  # split batting order into home (inefficient)
  get_home_batting_order <- reactive({
    get_batting_orders() %>%
      filter(team == 'home') %>%
      select(batting_order, fullName, abbreviation, plate_appearances, k_rate, avg, obp, slg, ops) %>%
      rename(c('#'='batting_order', 'player' = 'fullName', 'pos'='abbreviation',
               'pa' = 'plate_appearances'))
  })
  
  get_home_lineup_summary <- reactive({
    get_home_batting_order() %>%
      filter(pa > 100) %>%
      summarize(
        `#` = paste(get_home_team_name(), 'averages'),
        k_rate = mean(as.numeric(k_rate)),
        avg = mean(as.numeric(avg)),
        obp = mean(as.numeric(obp)),
        slg = mean(as.numeric(slg)),
        ops = mean(as.numeric(ops))
        )
  })

  # split batting order into away (inefficient)
  get_away_batting_order <- reactive({
    get_batting_orders() %>%
      filter(team == 'away') %>%
      select(batting_order, fullName, abbreviation, plate_appearances, k_rate, avg, obp, slg, ops) %>%
      rename(c('#'='batting_order', 'player' = 'fullName', 'pos'='abbreviation',
               'pa' = 'plate_appearances'))
  })
  
  get_away_lineup_summary <- reactive({
    get_away_batting_order() %>%
      filter(pa > 100) %>%
      summarize(
        `#` = paste(get_away_team_name(), 'averages'),
        k_rate = mean(as.numeric(k_rate)),
        avg = mean(as.numeric(avg)),
        obp = mean(as.numeric(obp)),
        slg = mean(as.numeric(slg)),
        ops = mean(as.numeric(ops))
      )
  })
  
  # get home starter
  get_home_pitcher <- reactive({
    get_probable_starters() %>%
      filter(team == get_home_team_name()) %>%
      select(-c(game_pk, game_date, id, team, team_id, home_plate_full_name, home_plate_id)) %>%
      rename(c('starter' = 'fullName'))
  })
  
  # get home starter
  get_away_pitcher <- reactive({
    get_probable_starters() %>%
      filter(team == get_away_team_name()) %>%
      select(-c(game_pk, game_date, id, team, team_id, home_plate_full_name, home_plate_id)) %>%
      rename(c('starter' = 'fullName'))
  })
  
  # make dropdown for games of the date
  output$games <- renderUI({
    pickerInput(
      "selected_game_str",
      label = h5("Game"),
      choices = as.list(get_game_pks()$game_str),
      # selected = as.list(game_pks()$game_str),
      options = list(
        `deselect-all-text` = "None",
        `select-all-text` = "Total",
        `actions-box` = TRUE
      ),
      multiple = F,
      width = "70%"
    )
  })
  
  # get visitors last 10 (should combine with get_home_last_10)
  get_visitors_last_10 <- reactive({
    away_team_name <- get_away_team_name()
    if (away_team_name == 'Los Angeles Angels') {
      away_team_name <- 'LAA'
    }
    bref_team_results(away_team_name, 2023) %>%
      mutate(
        full_date = paste(gsub("\\s*\\([^\\)]+\\)", "", Date), Year),
        game_dt = mdy(full_date),
        R = as.integer(R),
        RA = as.integer(RA)
      ) %>%
      arrange(game_dt) %>%
      slice_tail(n = 10) %>%
      select(Date, H_A, `D/N`, Opp, Result, R, RA)
  })
  
  # get home team last 10
  get_home_last_10 <- reactive({
    home_team_name <- get_home_team_name()
    if (home_team_name == 'Los Angeles Angels') {
      home_team_name <- 'LAA'
    }
    bref_team_results(home_team_name, 2023) %>%
      mutate(
        full_date = paste(gsub("\\s*\\([^\\)]+\\)", "", Date), Year),
        game_dt = mdy(full_date),
        R = as.integer(R),
        RA = as.integer(RA)
      ) %>%
      arrange(game_dt) %>%
      slice_tail(n = 10) %>%
      select(Date, H_A, `D/N`, Opp, Result, R, RA)
  })
  
  output$weather <- renderText(get_weather())
  
  output$visitors_last_10_header <- renderText(get_away_last_10_header())
  output$home_last_10_header <- renderText(get_home_last_10_header())
  
  output$visitors_last_10 <- renderTable(get_visitors_last_10())
  output$home_last_10 <- renderTable(get_home_last_10())
  
  output$away_starter_header <- renderText(get_away_starter_header())
  output$home_starter_header <- renderText(get_home_starter_header())

  output$away_lineup_header <- renderText(get_away_lineup_header())
  output$home_lineup_header <- renderText(get_home_lineup_header())
  
  output$away_starter <- renderTable(get_away_pitcher())
  
  output$home_batting_order <- renderTable(
    get_home_batting_order(),
    colnames = TRUE
    )
  
  output$home_lineup_summary <- renderTable(
    get_home_lineup_summary(),
    digits = 3
  )
  
  output$home_starter <- renderTable(get_home_pitcher())
  
  output$away_batting_order <- renderTable(
    get_away_batting_order(),
    colnames = TRUE
    )
  
  output$away_lineup_summary <- renderTable(
    get_away_lineup_summary(),
    digits = 3
  )
  
}

shinyApp(ui, server)