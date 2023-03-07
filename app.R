#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(nflreadr)
library(gt)
library(gtExtras)
library(webshot2)

# downloads don't work due to use of webshot2 on shinyapps.io
# We can force chromote to install in shiny through pagedown and curl
# see comments from rlesur on this link: 
# https://community.rstudio.com/t/how-to-properly-configure-google-chrome-on-shinyapps-io-because-of-webshot2/109020/3
library(curl)
library(pagedown)

teams_data <- load_teams()

max_year <- get_latest_season()
#max_week <- get_current_week()

kc_most_recent <- load_schedules(seasons = get_latest_season()) |> 
  filter(home_team == "KC" | away_team == "KC") |> 
  drop_na(total) |> 
  tail(1)

max_week <- kc_most_recent$week[1]

# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("Team Formations"),
  
  # Sidebar with a selector input for team 
  sidebarLayout(
    sidebarPanel(
      selectInput("year_var",
                  "Year:",
                  (2016:max_year),
                  selected = max_year
      ), 
      selectInput("team_var",
                  "Team:",
                  unique(teams_data$team_abbr),
                  selected = "KC"
      ), 
      sliderInput("week_var",
                  "Week:",
                  (1:22), 
                  value = c(1, 22), 
                  min = 1, 
                  max = 22
                  #selected = max_week
      ), 
      checkboxInput("exclude_players", 
                    "Check to exclude player stats", 
                    FALSE
      ), 
      sliderInput("form_percent", 
                  "Filter Formations by %", 
                  value = 0,  
                  min = 0, 
                  max = 100
      ), 
      sliderInput("snap_percent", 
                  "Filter Snaps by %", 
                  value = 0,  
                  min = 0, 
                  max = 100
      ), 
      submitButton("Update", 
                   icon("refresh")),
      downloadButton("download1", "Download Offense as PNG"),
      downloadButton("download2", "Download Defense as PNG"), 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # gt_output("table1"), 
      # gt_output("table2")
      tabsetPanel(type = "tabs", 
                  tabPanel("Offense", gt_output("table1")), 
                  tabPanel("Defense", gt_output("table2")), 
      )
    )
  )
)

# # load initial data
# off_formation_data <- readRDS(gzcon(url(
#   "https://github.com/Josephhero/snap_formation_initial_data/raw/main/preprocessed_offensive_snap_formation_data.rds")))
# 
# # Load initial data
# def_formation_data <- readRDS(gzcon(url(
#   "https://github.com/Josephhero/snap_formation_initial_data/raw/main/preprocessed_defensive_snap_formation_data.rds")))

# Define server logic required to create a gt table
server <- function(input, output) {
  
  # Offense Data----------------------------------------------------------------
  
  # # load initial data
  # off_formation_data <- readRDS(gzcon(url(
  #   "https://github.com/Josephhero/snap_formation_initial_data/raw/main/preprocessed_offensive_snap_formation_data.rds")))

  
  offense_table <- reactive({
    YEAR <- as.numeric(input$year_var)
    TEAM <- input$team_var
    WEEK_START <- input$week_var[1]
    WEEK_END <- input$week_var[2]
    EXCLUDE_PLAYERS <- input$exclude_players
    
    teams_df <- load_teams()
    my_team <- teams_df |> filter(team_abbr == TEAM)
    TEAM_NICK <- my_team$team_nick[1]
    FORM_PERCENT <- input$form_percent
    SNAP_PERCENT <- input$snap_percent

    sched <- load_schedules(YEAR) |> 
      filter(home_team == TEAM | away_team == TEAM, week %in% WEEK_START:WEEK_END) |> 
      mutate()
    
    x_val <- case_when(WEEK_START != WEEK_END ~ 1, 
                       nrow(sched) > 0 && !is.na(sched$result) ~ 1, 
                       nrow(sched) > 0 && is.na(sched$result) ~ 2, 
                       nrow(sched) == 0 ~ 3,
                       TRUE ~ 4)
    
    game_not_updated_yet <- paste0("This game has not yet been updated. ",
                                   "Please verify that this game has already been ", 
                                   "played and try again in an hour")
    game_does_not_exist <- paste0(TEAM, " did not play a game in week ", WEEK_START, " of ", YEAR)
    game_error_message <- paste0("There is an unknown error. ", 
                                 "Please DM @josephjefe on twitter ", 
                                 "for resolution of issue.")
    
    error_message <- case_when(x_val == 2 ~ game_not_updated_yet, 
                               x_val == 3 ~ game_does_not_exist, 
                               x_val == 4 ~ game_error_message)
    
    
    validate(need(x_val == 1, error_message))    
    
    off_formation1 <- readRDS(gzcon(url(
      "https://github.com/Josephhero/snap_formation_initial_data/raw/main/preprocessed_offensive_snap_formation_data.rds"))) |> 
    
    #off_formation1 <- off_formation_data |> 
      filter(season == YEAR, posteam == TEAM, week %in% WEEK_START:WEEK_END) |> 
      group_by(gsis_id) |> 
      mutate(player_snaps = n()) |> 
      ungroup()
    
    off_formation_team1 <- off_formation1 |> 
      select(play_id, formation) |> 
      distinct() |> 
      group_by(formation) |> 
      summarize(formation_snaps = n()) |> 
      mutate(name = TEAM_NICK) |>
      mutate(position_group = "Team") |> 
      mutate(total = sum(formation_snaps)) |> 
      mutate(form_percent = round((formation_snaps / total) * 100)) |> 
      filter(form_percent >= FORM_PERCENT)
    
    FILTERED_FORMATIONS <- unique(off_formation_team1$formation)
    TEAM_TOTAL <- max(off_formation_team1$total)
    
    off_formation_team <- off_formation_team1 |> 
      mutate(filtered_total = sum(formation_snaps)) |> 
      mutate(filtered_perc = round((filtered_total/total) * 100)) |> 
      arrange(-formation_snaps) |>
      mutate(form_data = paste0(formation_snaps, " ", form_percent, "%")) |> 
      mutate(calc_total = paste0(filtered_total, " ", filtered_perc, "%")) |> 
      select(-formation_snaps, -form_percent, -filtered_total, -filtered_perc, -total) |> 
      pivot_wider(names_from = formation, values_from = form_data) |> 
      mutate(sort_order = 1) |> 
      relocate(calc_total, .before = sort_order)
    
    off_formation2 <- off_formation1 |> 
      group_by(formation, gsis_id) |> 
      mutate(formation_player_snaps = n()) |> 
      ungroup() |> 
      select(gsis_id, position_group, name = short_name, player_snaps, formation, formation_player_snaps) |> 
      distinct() |> 
      mutate(snap_percent = round((player_snaps/TEAM_TOTAL) * 100)) |> 
      filter(formation %in% FILTERED_FORMATIONS) |>
      group_by(gsis_id) |> 
      mutate(filtered_player_snaps = sum(formation_player_snaps)) |> 
      mutate(filtered_player_snap_percent = round((filtered_player_snaps/TEAM_TOTAL) * 100)) |> 
      mutate(form_percent = round((formation_player_snaps / player_snaps) * 100)) |> 
      ungroup() |> 
      filter(filtered_player_snap_percent >= SNAP_PERCENT) |> 
      arrange(formation, -filtered_player_snaps)
    
    off_formation2 <- off_formation2 |> 
      mutate(form_data = paste(formation_player_snaps, " ", form_percent, "%", sep = "")) |> 
      select(-form_percent, -formation_player_snaps) |> 
      pivot_wider(names_from = formation, values_from = form_data) |> 
      arrange(position_group, -filtered_player_snaps) |> 
      mutate(calc_total = paste0(filtered_player_snaps, " ", filtered_player_snap_percent, "%")) |> 
      select(-player_snaps, -snap_percent, -filtered_player_snaps, -filtered_player_snap_percent, -gsis_id) |> 
      mutate(sort_order = row_number() + 3)
    
    off_epa_formation <- off_formation1 |> 
      filter(formation %in% FILTERED_FORMATIONS) |> 
      select(play_id, formation, epa) |> 
      distinct() |> 
      mutate(calc_total = as.character(round(mean(epa), 2))) |> 
      group_by(calc_total, formation) |> 
      summarize(formation_epa = as.character(round(mean(epa), 2))) |> 
      ungroup() |> 
      mutate(name = "EPA per Play") |>
      mutate(position_group = "Team") |> 
      pivot_wider(names_from = formation, values_from = formation_epa) |> 
      mutate(sort_order = 2)
    
    off_pass_rate_formation <- off_formation1 |> 
      filter(formation %in% FILTERED_FORMATIONS) |> 
      select(play_id, formation, pass) |> 
      distinct() |> 
      mutate(calc_total = paste0(round((sum(pass)/ n()) * 100, 0), "%")) |> 
      group_by(calc_total, formation) |> 
      summarize(pass_rate = paste0(round((sum(pass)/ n()) * 100, 0), "%")) |> 
      ungroup() |> 
      mutate(name = "Pass Rate") |>
      mutate(position_group = "Team") |> 
      pivot_wider(names_from = formation, values_from = pass_rate) |> 
      mutate(sort_order = 3)
    
    if(EXCLUDE_PLAYERS == TRUE) {
      off_formation <- bind_rows(off_formation_team, off_epa_formation, off_pass_rate_formation) |> 
        arrange(sort_order) |> 
        rename(total = calc_total) |> 
        select(-sort_order)
    } else {
      off_formation <- bind_rows(off_formation_team, off_epa_formation, off_pass_rate_formation, off_formation2) |> 
        arrange(sort_order) |> 
        rename(total = calc_total) |> 
        select(-sort_order)
    }
    
    home_team_abbr <- teams_df |> 
      filter(team_abbr == off_formation1$home_team[1])
    
    away_team_abbr <-  teams_df |> 
      filter(team_abbr == off_formation1$away_team[1])
    
    OPP <- off_formation1$defteam[1]
    
    # Table-------------------------------------------------------------------------
    
    off_formation_cols <- ncol(off_formation) - 1
    
    off_team_html <- paste0('<div style = "line-height:0.01"><img height="20" vertical-align="middle" src="',
                            my_team$team_wordmark,
                            '"></div>')
    
    off_week_html <- paste0('<div style = "display:flex;justify-content:center;align-items:center;"><img height="70"src="',
                            away_team_abbr$team_logo_espn,
                            '">',
                            "&nbsp;",
                            away_team_abbr$team_nick,
                            "&nbsp;",
                            off_formation1$away_score[1], 
                            " &nbsp;@&nbsp; ",
                            off_formation1$home_score[1], 
                            "&nbsp;",
                            home_team_abbr$team_nick, 
                            '&nbsp;',
                            '<img height="70"src="',
                            home_team_abbr$team_logo_espn,
                            '">', 
                            '</div>'
    )
    
    off_season_html <- paste0('<div style = "display:flex;justify-content:center;align-items:center;"><img height="70"src="',
                              my_team$team_logo_espn[1],
                              '">',
                              "&nbsp;",
                              my_team$team_nick[1],
                              "&nbsp;",
                              YEAR, 
                              ' Season', 
                              "&nbsp;",
                              '<img height="70"src="',
                              my_team$team_logo_espn[1],
                              '">', 
                              '</div>'
    )
    
    off_title_html <- case_when(
      WEEK_START == WEEK_END ~ off_week_html, 
      TRUE ~ off_season_html
    )
    
    off_week_subtitle <- paste0(TEAM_NICK, " Offense: Week ", WEEK_START, ", ", YEAR)
    off_season_subtitle <- paste0(TEAM_NICK, 
                                  " Offense: Weeks ",
                                  WEEK_START, 
                                  "-", 
                                  WEEK_END, 
                                  ", ", 
                                  YEAR)
    
    off_subtitle <- case_when(
      WEEK_START == WEEK_END ~ off_week_subtitle, 
      TRUE ~ off_season_subtitle
    )
    
    (off_tab1 <- 
        gt(off_formation,
           groupname_col = "position_group",
        ) |>
        gt_theme_espn()
    )
    
    (off_tab <- off_tab1 |>
        tab_header(title = html(off_title_html),
                   subtitle = off_subtitle
        ) |>
        sub_missing(columns = everything(), rows = everything(), missing_text = "") |>
        cols_label(name = "") |>
        tab_spanner(
          label = "Formations = RB-TE-WR",
          columns = (3:off_formation_cols)
        ) |>
        tab_style(
          style = cell_text(weight = "bold", align = "center"),
          locations = cells_title(groups = c("title", "subtitle"))
        ) |>
        tab_style(
          style = cell_text(size = px(18)),
          locations = cells_title(groups = "subtitle")
        ) |>
        text_transform(
          locations = cells_body(
            columns = (3:(off_formation_cols +1))
          ),
          fn = function(x){
            text <- word(x, 1)
            sup <- coalesce(word(x, 2), "")
            glue::glue("<font size='3'>{text}</font><small><small style='color:#777; font-style:italic'> {sup}</small></small>")
          }
        ) |>
        text_transform(
          locations = cells_body(
            columns = name
          ),
          fn = function(x){
            ifelse(x == TEAM_NICK, html(off_team_html), x)
          }
        ) |>
        cols_align(
          align = c("left"),
          columns = everything()
        ) |>
        tab_source_note(source_note = "Table: @josephjefe     |     Data: nflverse.com")
    )
    
    
    
    
  })
  
  # Defense Data---------------------------------------------------------------
  
  defense_table <- reactive({
    YEAR <- as.numeric(input$year_var)
    TEAM <- input$team_var
    WEEK_START <- input$week_var[1]
    WEEK_END <- input$week_var[2]
    EXCLUDE_PLAYERS <- input$exclude_players
    
    teams_df <- load_teams()
    my_team <- teams_df |> filter(team_abbr == TEAM)
    TEAM_NICK <- my_team$team_nick[1]
    FORM_PERCENT <- input$form_percent
    SNAP_PERCENT <- input$snap_percent
    
    sched <- load_schedules(YEAR) |> 
      filter(home_team == TEAM | away_team == TEAM, week %in% WEEK_START:WEEK_END) |> 
      mutate()
    
    x_val <- case_when(WEEK_START != WEEK_END ~ 1, 
                       nrow(sched) > 0 && !is.na(sched$result) ~ 1, 
                       nrow(sched) > 0 && is.na(sched$result) ~ 2, 
                       nrow(sched) == 0 ~ 3,
                       TRUE ~ 4)
    
    game_not_updated_yet <- paste0("This game has not yet been updated. ",
                                   "Please verify that this game has already been ", 
                                   "played and try again in an hour")
    game_does_not_exist <- paste0(TEAM, " did not play a game in week ", WEEK_START, " of ", YEAR)
    game_error_message <- paste0("There is an unknown error. ", 
                                 "Please DM @josephjefe on twitter ", 
                                 "for resolution of issue.")
    
    error_message <- case_when(x_val == 2 ~ game_not_updated_yet, 
                               x_val == 3 ~ game_does_not_exist, 
                               x_val == 4 ~ game_error_message)
    
    validate(need(x_val == 1, error_message))  
    
    def_formation1 <- readRDS(gzcon(url(
      "https://github.com/Josephhero/snap_formation_initial_data/raw/main/preprocessed_defensive_snap_formation_data.rds"))) |> 
    
    #def_formation1 <- def_formation_data |> 
      filter(season == YEAR, defteam == TEAM, week %in% WEEK_START:WEEK_END) |> 
      group_by(gsis_id) |> 
      mutate(player_snaps = n()) |> 
      ungroup()
    
    def_formation_team1 <- def_formation1 |> 
      select(play_id, formation) |> 
      distinct() |> 
      group_by(formation) |> 
      summarize(formation_snaps = n()) |> 
      mutate(name = TEAM_NICK) |>
      mutate(position_group = "Team") |> 
      mutate(total = sum(formation_snaps)) |> 
      mutate(form_percent = round((formation_snaps / total) * 100)) |> 
      filter(form_percent >= FORM_PERCENT)
    
    FILTERED_FORMATIONS <- unique(def_formation_team1$formation)
    TEAM_TOTAL <- max(def_formation_team1$total)
    
    def_formation_team <- def_formation_team1 |> 
      mutate(filtered_total = sum(formation_snaps)) |> 
      mutate(filtered_perc = round((filtered_total/total) * 100)) |> 
      arrange(-formation_snaps) |>
      mutate(form_data = paste0(formation_snaps, " ", form_percent, "%")) |> 
      mutate(calc_total = paste0(filtered_total, " ", filtered_perc, "%")) |> 
      select(-formation_snaps, -form_percent, -filtered_total, -filtered_perc, -total) |> 
      pivot_wider(names_from = formation, values_from = form_data) |> 
      mutate(sort_order = 1) |> 
      relocate(calc_total, .before = sort_order)
    
    def_formation2 <- def_formation1 |> 
      group_by(formation, gsis_id) |> 
      mutate(formation_player_snaps = n()) |> 
      ungroup() |> 
      select(gsis_id, position_group, name = short_name, player_snaps, formation, formation_player_snaps) |> 
      distinct() |> 
      mutate(snap_percent = round((player_snaps/TEAM_TOTAL) * 100)) |> 
      filter(formation %in% FILTERED_FORMATIONS) |>
      group_by(gsis_id) |> 
      mutate(filtered_player_snaps = sum(formation_player_snaps)) |> 
      mutate(filtered_player_snap_percent = round((filtered_player_snaps/TEAM_TOTAL) * 100)) |> 
      mutate(form_percent = round((formation_player_snaps / player_snaps) * 100)) |> 
      ungroup() |> 
      filter(filtered_player_snap_percent >= SNAP_PERCENT) |> 
      arrange(formation, -filtered_player_snaps)
    
    def_formation2 <- def_formation2 |> 
      mutate(form_data = paste(formation_player_snaps, " ", form_percent, "%", sep = "")) |> 
      select(-form_percent, -formation_player_snaps) |> 
      pivot_wider(names_from = formation, values_from = form_data) |> 
      arrange(position_group, -filtered_player_snaps) |> 
      mutate(calc_total = paste0(filtered_player_snaps, " ", filtered_player_snap_percent, "%")) |> 
      select(-player_snaps, -snap_percent, -filtered_player_snaps, -filtered_player_snap_percent, -gsis_id) |> 
      mutate(sort_order = row_number() + 3)
    
    def_epa_formation <- def_formation1 |> 
      filter(formation %in% FILTERED_FORMATIONS) |> 
      select(play_id, formation, epa) |> 
      distinct() |> 
      mutate(calc_total = as.character(round(mean(epa), 2))) |> 
      group_by(calc_total, formation) |> 
      summarize(formation_epa = as.character(round(mean(epa), 2))) |> 
      ungroup() |> 
      mutate(name = "EPA per Play") |>
      mutate(position_group = "Team") |> 
      pivot_wider(names_from = formation, values_from = formation_epa) |> 
      mutate(sort_order = 2)
    
    def_pass_rate_formation <- def_formation1 |> 
      filter(formation %in% FILTERED_FORMATIONS) |> 
      select(play_id, formation, pass) |> 
      distinct() |> 
      mutate(calc_total = paste0(round((sum(pass)/ n()) * 100, 0), "%")) |> 
      group_by(calc_total, formation) |> 
      summarize(pass_rate = paste0(round((sum(pass)/ n()) * 100, 0), "%")) |> 
      mutate(name = "Pass Rate") |>
      mutate(position_group = "Team") |> 
      pivot_wider(names_from = formation, values_from = pass_rate) |> 
      mutate(sort_order = 3)
    
    if(EXCLUDE_PLAYERS == TRUE) {
      def_formation <- bind_rows(def_formation_team, def_epa_formation, def_pass_rate_formation) |> 
        arrange(sort_order) |> 
        rename(total = calc_total) |> 
        select(-sort_order)
    } else {
      def_formation <- bind_rows(def_formation_team, def_epa_formation, def_pass_rate_formation, def_formation2) |> 
        arrange(sort_order) |> 
        rename(total = calc_total) |> 
        select(-sort_order)
    }
    
    home_team_abbr <- teams_df |> 
      filter(team_abbr == def_formation1$home_team[1])
    
    away_team_abbr <-  teams_df |> 
      filter(team_abbr == def_formation1$away_team[1])
    
    OPP <- def_formation1$posteam[1]
    
    # Table-------------------------------------------------------------------------
    
    def_formation_cols <- ncol(def_formation) - 1
    
    def_team_html <- paste0('<div style = "line-height:0.01"><img height="20" vertical-align="middle" src="',
                            my_team$team_wordmark,
                            '"></div>')
    
    def_week_html <- paste0('<div style = "display:flex;justify-content:center;align-items:center;"><img height="70"src="',
                            away_team_abbr$team_logo_espn,
                            '">',
                            "&nbsp;",
                            away_team_abbr$team_nick,
                            "&nbsp;",
                            def_formation1$away_score[1], 
                            " &nbsp;@&nbsp; ",
                            def_formation1$home_score[1], 
                            "&nbsp;",
                            home_team_abbr$team_nick, 
                            '&nbsp;',
                            '<img height="70"src="',
                            home_team_abbr$team_logo_espn,
                            '">', 
                            '</div>'
    )
    
    def_season_html <- paste0('<div style = "display:flex;justify-content:center;align-items:center;"><img height="70"src="',
                              my_team$team_logo_espn[1],
                              '">',
                              "&nbsp;",
                              my_team$team_nick[1],
                              "&nbsp;",
                              YEAR, 
                              ' Season', 
                              "&nbsp;",
                              '<img height="70"src="',
                              my_team$team_logo_espn[1],
                              '">', 
                              '</div>'
    )
    
    def_title_html <- case_when(
      WEEK_START == WEEK_END ~ def_week_html, 
      TRUE ~ def_season_html
    )
    
    def_week_subtitle <- paste0(TEAM_NICK, " Defense: Week ", WEEK_START, ", ", YEAR)
    def_season_subtitle <- paste0(TEAM_NICK, 
                                  " Defense: Weeks ",
                                  WEEK_START, 
                                  "-", 
                                  WEEK_END, 
                                  ", ", 
                                  YEAR)
    
    def_subtitle <- case_when(
      WEEK_START == WEEK_END ~ def_week_subtitle, 
      TRUE ~ def_season_subtitle
    )
    
    (def_tab1 <- 
        gt(def_formation, 
           groupname_col = "position_group",
        ) |> 
        gt_theme_espn()
    )
    
    (def_tab <- def_tab1 |> 
        tab_header(title = html(def_title_html), 
                   subtitle = def_subtitle
        ) |>
        sub_missing(columns = everything(), rows = everything(), missing_text = "") |>
        cols_label(name = "") |>
        tab_spanner(
          label = "Formations = DL-LB-DB",
          columns = (3:def_formation_cols)
        ) |>
        tab_style(
          style = cell_text(weight = "bold", align = "center"),
          locations = cells_title(groups = c("title", "subtitle"))
        ) |>
        tab_style(
          style = cell_text(size = px(18)),
          locations = cells_title(groups = "subtitle")
        ) |>
        text_transform(
          locations = cells_body(
            columns = (3:(def_formation_cols +1))
          ),
          fn = function(x){
            text <- word(x, 1)
            sup <- coalesce(word(x, 2), "")
            glue::glue("<font size='3'>{text}</font><small><small style='color:#777; font-style:italic'> {sup}</small></small>")
          }
        ) |>
        text_transform(
          locations = cells_body(
            columns = name
          ),
          fn = function(x){
            ifelse(x == TEAM_NICK, html(def_team_html), x)
          }
        ) |>
        cols_align(
          align = c("left"),
          columns = everything()
        ) |>
        tab_source_note(source_note = "Table: @josephjefe     |     Data: nflverse.com") 
    )
    
    
  })
  
  # Output----------------------------------------------------------------------
  
  message(curl::curl_version()) # check curl is installed
  if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
    chromote::set_default_chromote_object(
      chromote::Chromote$new(chromote::Chrome$new(
        args = c("--disable-gpu", 
                 "--no-sandbox", 
                 "--disable-dev-shm-usage", # required bc the target easily crashes
                 c("--force-color-profile", "srgb"))
      ))
    )
  }
  
  output$table1 <- render_gt({
    offense_table() 
  })
  
  output$table2 <- render_gt({
    defense_table() 
  })
  
  output$download1 <- downloadHandler(
    filename = function() {
      if (input$week_var[1] == input$week_var[2]) {
        paste0("Week ", 
               input$week_var[1], 
               " ", 
               input$team_var, 
               " Offense.png")
      } else {
        paste0(input$team_var, 
               " Offense Weeks ", 
               input$week_var[1], 
               "_", 
               input$week_var[2], 
               " ", 
               input$year_var, 
               ".png")
      }
    },
    content = function(file) {gtsave(offense_table(), file = file)},
    contentType = "image/png"
  )
  
  output$download2 <- downloadHandler(
    filename = function() {
      if (input$week_var[1] == input$week_var[2]) {
        paste0("Week ", 
               input$week_var[1], 
               " ", 
               input$team_var, 
               " Defense.png")
      } else {
        paste0(input$team_var, 
               " Defense Weeks ", 
               input$week_var[1], 
               "_", 
               input$week_var[2], 
               " ", 
               input$year_var, 
               ".png")
      }
    },
    content = function(file) {gtsave(defense_table(), file = file)},
    contentType = "image/png"
  )
}




# Run the application 
shinyApp(ui = ui, server = server)
