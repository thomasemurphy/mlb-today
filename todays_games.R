library(shiny)
library(shinyWidgets)
library(ggplot2)
library(baseballr)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

source('../api_keys.R')

source('todays_games_methods.R')

todays_odds_df <- make_odds_df()

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
  ),
  
  fluidRow(
    column(
      6,
      textOutput('away_moneyline_text')
    ),
    column(
      6,
      # tags$h5((htmlOutput("home_lineup_header"))),
      textOutput('home_moneyline_text')
    )
  ),
  
  fluidRow(
    column(
      6,
      textOutput('over_under_text')
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
      weather_api_key
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
    } else if (away_team_name == 'Cleveland Guardians') {
      away_team_name <- 'CLE'
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
    } else if (home_team_name == 'Cleveland Guardians') {
      home_team_name <- 'CLE'
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
  
  get_away_moneyline_text <- reactive({
    away_team_name <- get_away_team_name()
    away_team_moneyline <- todays_odds_df %>%
      filter(team == away_team_name) %>%
      pull(moneyline_text)
    paste(away_team_name, away_team_moneyline)
  })
  
  get_home_moneyline_text <- reactive({
    home_team_name <- get_home_team_name()
    home_team_moneyline <- todays_odds_df %>%
      filter(team == home_team_name) %>%
      pull(moneyline_text)
    paste(home_team_name, home_team_moneyline)
  })
  
  get_over_under_text <- reactive({
    home_team_name <- get_home_team_name()
    over_under <- todays_odds_df %>%
      filter(team == home_team_name) %>%
      pull(over_under)
    paste('Over-under:', round(over_under, 1))    
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
  
  output$away_moneyline_text <- renderText(get_away_moneyline_text())
  output$home_moneyline_text <- renderText(get_home_moneyline_text())
  
  output$over_under_text <- renderText(get_over_under_text())
  
}

shinyApp(ui, server)