library(shiny)
library(shinyWidgets)
library(ggplot2)
library(baseballr)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

ui <- fluidPage(
  
  dateInput(
    'date',
    label = 'Date',
    value = today()
    ),
  
  tableOutput("games"),
  
  fluidRow(
    column(
      8,
      h5('Gametime weather'),
      textOutput("weather")
    )
  ),
  
  fluidRow(
    column(
      6,
      h5('Visitors last 10'),
      tableOutput('visitors_last_10')
      ),
    column(
      6,
      h5('Home team last 10'),
      tableOutput('home_last_10')
    ),
  ),
  
  fluidRow(
    column(
      6,
      h5('Home starter'),
      tableOutput('home_starter')
    ),
    column(
      6,
      h5('Visitors Starter'),
      tableOutput('away_starter')
    )
  ),
  
  fluidRow(
    column(
      6,
      h5('Visitors Lineup'),
      tableOutput('away_batting_order')
    ),
    column(
      6,
      h5('Home Lineup'),
      tableOutput('home_batting_order')
    )
  )
)

server <- function(input, output, session) {
  
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
      'from ',
      weather_df$wind_deg,
      ' from north, ',
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

  # split batting order into away (inefficient)
  get_away_batting_order <- reactive({
    get_batting_orders() %>%
      filter(team == 'away') %>%
      select(batting_order, fullName, abbreviation, plate_appearances, k_rate, avg, obp, slg, ops) %>%
      rename(c('#'='batting_order', 'player' = 'fullName', 'pos'='abbreviation',
               'pa' = 'plate_appearances'))
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
    bref_team_results(get_away_team_name(), 2023) %>%
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
  
  # get pitchers
  get_home_last_10 <- reactive({
    bref_team_results(get_home_team_name(), 2023) %>%
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
  
  
  # batting_orders_both <- get_batting_orders()
  
  output$weather <- renderText(get_weather())
  
  output$visitors_last_10 <- renderTable(get_visitors_last_10())
  output$home_last_10 <- renderTable(get_home_last_10())

  output$away_team_name <- renderText(get_away_team_name())
  
  output$away_starter <- renderTable(get_away_pitcher())
  
  output$home_batting_order <- renderTable(
    get_home_batting_order(),
    colnames = TRUE
    )
  
  output$home_team_name <- renderText(get_home_team_name())
  
  output$home_starter <- renderTable(get_home_pitcher())
  
  output$away_batting_order <- renderTable(
    get_away_batting_order(),
    colnames = TRUE
    )
  
}

shinyApp(ui, server)