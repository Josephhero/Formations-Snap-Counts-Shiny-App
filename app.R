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
library(nflverse)
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

# Define UI for application that draws a histogram
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
      selectInput("week_var",
                  "Week (choose 0 for full season):",
                  (0:22),
                  selected = max_week
      ), 
      selectInput("team_var",
                  "Team:",
                  unique(teams_data$team_abbr),
                  selected = "KC"
      ), 
      sliderInput("form_percent", 
                  "Filter Formations by %", 
                  value = 0, 
                  min = 0, 
                  max = 100), 
      sliderInput("snap_percent", 
                  "Filter Snaps by %", 
                  value = 0, 
                  min = 0, 
                  max = 100), 
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

# Define server logic required to create a gt table
server <- function(input, output) {
  
  # Offense Data----------------------------------------------------------------
  offense_table <- reactive({
    YEAR <- as.numeric(input$year_var)
    WEEK <- input$week_var
    TEAM <- input$team_var
    
    teams_df <- load_teams() |> filter(team_abbr == TEAM)
    TEAM_NICK <- teams_df$team_nick[1]
    FORM_PERCENT <- input$form_percent
    SNAP_PERCENT <- input$snap_percent
    
    sched <- load_schedules(YEAR) |> 
      filter(home_team == TEAM | away_team == TEAM, week == WEEK) |> 
      mutate()
    
    x_val <- case_when(WEEK < 1 ~ 1, 
                       nrow(sched) > 0 && !is.na(sched$result) ~ 1, 
                       nrow(sched) > 0 && is.na(sched$result) ~ 2, 
                       nrow(sched) == 0 ~ 3,
                       TRUE ~ 4)
    
    game_not_updated_yet <- paste0("This game has not yet been updated. ",
                                   "Please verify that this game has already been ", 
                                   "played and try again in an hour")
    game_does_not_exist <- paste0(TEAM, " did not play a game in week ", WEEK, " of ", YEAR)
    game_error_message <- paste0("There is an unknown error. ", 
                                 "Please DM @josephjefe on twitter ", 
                                 "for resolution of issue.")
    
    error_message <- case_when(x_val == 2 ~ game_not_updated_yet, 
                               x_val == 3 ~ game_does_not_exist, 
                               x_val == 4 ~ game_error_message)
    
    
    validate(need(x_val == 1, error_message))    
    
    # off_df_raw <- load_participation(seasons = YEAR, include_pbp = TRUE) |> 
    #   filter(posteam == TEAM, week == WEEK, pass == 1 | rush == 1) |> 
    #   drop_na(offense_formation)
    
    if(WEEK > 0) {
      off_df_raw <- load_participation(seasons = YEAR, include_pbp = TRUE) |> 
        filter(posteam == TEAM, week == WEEK, pass == 1 | rush == 1) |> 
        drop_na(offense_formation)
    } else {
      off_df_raw <- load_participation(seasons = YEAR, include_pbp = TRUE) |> 
        filter(posteam == TEAM, pass == 1 | rush == 1) |> 
        drop_na(offense_formation)
    }
    
    off_formation1 <- off_df_raw |> 
      mutate(snap_num = row_number(), .before = play_id) |> 
      select(week, snap_num, posteam, defteam, gsis_id = offense_players, offense_personnel, epa, pass) |> 
      separate_rows(offense_personnel, sep = ",") |> 
      mutate(offense_personnel = str_squish(offense_personnel)) |> 
      separate(offense_personnel, into = c("x1", "x2"), sep = " ") |> 
      mutate(x1 = as.numeric(x1)) |> 
      pivot_wider(names_from = "x2", values_from = "x1") |> 
      janitor::clean_names()
    
    OPP <- unique(off_formation1$defteam)
    
    off_formation2 <- off_formation1 |> 
      separate_rows(gsis_id, sep = ";")
    
    # Get player/roster info
    off_player_positions <- load_players() |> 
      select(short_name, gsis_id, position_group, position)
    
    off_formation3 <-  
      left_join(off_formation2, off_player_positions, by = "gsis_id") |> 
      filter(position_group %in% c("RB", "TE", "WR")) |> 
      relocate(short_name, position_group, position, .after = gsis_id) |> 
      group_by(gsis_id) |> 
      mutate(player_snaps = n()) |> 
      ungroup() |> 
      mutate(position_group = factor(position_group, c("RB", "TE", "WR"))) |> 
      mutate(formation = paste(rb, te, wr, sep = "-")) |> 
      arrange(position_group, player_snaps, desc(short_name))
    
    off_formation_team1 <- off_formation3 |> 
      select(snap_num, formation) |> 
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
    
    off_formation4 <- off_formation3 |> 
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
    
    off_formation5 <- off_formation4 |> 
      mutate(form_data = paste(formation_player_snaps, " ", form_percent, "%", sep = "")) |> 
      select(-form_percent, -formation_player_snaps) |> 
      pivot_wider(names_from = formation, values_from = form_data) |> 
      arrange(position_group, -filtered_player_snaps) |> 
      mutate(calc_total = paste0(filtered_player_snaps, " ", filtered_player_snap_percent, "%")) |> 
      select(-player_snaps, -snap_percent, -filtered_player_snaps, -filtered_player_snap_percent, -gsis_id) |> 
      mutate(sort_order = row_number() + 3)
    
    off_epa_formation <- off_formation3 |> 
      filter(formation %in% FILTERED_FORMATIONS) |> 
      select(snap_num, formation, epa) |> 
      distinct() |> 
      mutate(calc_total = as.character(round(mean(epa), 2))) |> 
      group_by(calc_total, formation) |> 
      summarize(formation_epa = as.character(round(mean(epa), 2))) |> 
      ungroup() |> 
      mutate(name = "EPA per Play") |>
      mutate(position_group = "Team") |> 
      pivot_wider(names_from = formation, values_from = formation_epa) |> 
      mutate(sort_order = 2)
    
    off_pass_rate_formation <- off_formation3 |> 
      filter(formation %in% FILTERED_FORMATIONS) |> 
      select(snap_num, formation, pass) |> 
      distinct() |> 
      mutate(calc_total = paste0(round((sum(pass)/ n()) * 100, 0), "%")) |> 
      group_by(calc_total, formation) |> 
      #mutate(sum_pass = sum(pass)) |> 
      #mutate(count_pass = n()) |> 
      summarize(pass_rate = paste0(round((sum(pass)/ n()) * 100, 0), "%")) |> 
      ungroup() |> 
      mutate(name = "Pass Rate") |>
      mutate(position_group = "Team") |> 
      pivot_wider(names_from = formation, values_from = pass_rate) |> 
      mutate(sort_order = 3)
    
    off_formation <- bind_rows(off_formation_team, off_epa_formation, off_pass_rate_formation, off_formation5) |> 
      arrange(sort_order) |> 
      rename(total = calc_total) |> 
      select(-sort_order)
    
    home_team_abbr <- load_teams() |> 
      filter(team_abbr == off_df_raw$home_team[1]) 
    
    away_team_abbr <- load_teams() |> 
      filter(team_abbr == off_df_raw$away_team[1])
    
    
    # Table-------------------------------------------------------------------------
    
    off_formation_cols <- ncol(off_formation) - 1
    
    off_team_html <- paste0('<div style = "line-height:0.01"><img height="20" vertical-align="middle" src="',
                            teams_df$team_wordmark,
                            '"></div>')
    
    off_week_html <- paste0('<div style = "display:flex;justify-content:center;align-items:center;"><img height="70"src="',
                            away_team_abbr$team_logo_espn,
                            '">',
                            "&nbsp;",
                            away_team_abbr$team_nick,
                            "&nbsp;",
                            off_df_raw$away_score[1], 
                            " &nbsp;@&nbsp; ",
                            off_df_raw$home_score[1], 
                            "&nbsp;",
                            home_team_abbr$team_nick, 
                            '&nbsp;',
                            '<img height="70"src="',
                            home_team_abbr$team_logo_espn,
                            '">', 
                            '</div>'
    )
    
    off_season_html <- paste0('<div style = "display:flex;justify-content:center;align-items:center;"><img height="70"src="',
                              teams_df$team_logo_espn[1],
                              '">',
                              "&nbsp;",
                              teams_df$team_nick[1],
                              "&nbsp;",
                              YEAR, 
                              ' Season', 
                              "&nbsp;",
                              '<img height="70"src="',
                              teams_df$team_logo_espn[1],
                              '">', 
                              '</div>'
    )
    
    off_title_html <- case_when(
      WEEK > 0 ~ off_week_html, 
      TRUE ~ off_season_html
    )
    
    off_week_subtitle <- paste0(TEAM_NICK, " Offense: Week ", WEEK, ", ", YEAR)
    off_season_subtitle <- paste0(TEAM_NICK, " Offense: Weeks 1-7")
    
    off_subtitle <- case_when(
      WEEK > 0 ~ off_week_subtitle, 
      TRUE ~ off_season_subtitle
    )
    
    off_week_filename <- paste0("Week ", WEEK, " ", TEAM, " Offense vs ", OPP, ".png")
    off_season_filename <- paste0(YEAR, " Season Chiefs", " Offense.png")
    
    off_filename <- case_when(
      WEEK > 0 ~ off_week_filename, 
      TRUE ~ off_season_filename
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
        tab_source_note(source_note = "Table: @josephjefe     |     Data: #nflverse")
    )
    
    
    
  })
  
  # Defense Data---------------------------------------------------------------
  
  defense_table <- reactive({
    YEAR <- as.numeric(input$year_var)
    WEEK <- input$week_var
    TEAM <- input$team_var
    
    teams_df <- load_teams() |> filter(team_abbr == TEAM)
    TEAM_NICK <- teams_df$team_nick[1]
    FORM_PERCENT <- input$form_percent
    SNAP_PERCENT <- input$snap_percent
    
    sched <- load_schedules(YEAR) |> 
      filter(home_team == TEAM | away_team == TEAM, week == WEEK) |> 
      mutate()
    
    x_val <- case_when(WEEK < 1 ~ 1, 
                       nrow(sched) > 0 && !is.na(sched$result) ~ 1, 
                       nrow(sched) > 0 && is.na(sched$result) ~ 2, 
                       nrow(sched) == 0 ~ 3,
                       TRUE ~ 4)
    
    game_not_updated_yet <- paste0("This game has not yet been updated. ",
                                   "Please verify that this game has already been ", 
                                   "played and try again in an hour")
    game_does_not_exist <- paste0(TEAM, " did not play a game in week ", WEEK, " of ", YEAR)
    game_error_message <- paste0("There is an unknown error. ", 
                                 "Please DM @josephjefe on twitter ", 
                                 "for resolution of issue.")
    
    error_message <- case_when(x_val == 2 ~ game_not_updated_yet, 
                               x_val == 3 ~ game_does_not_exist, 
                               x_val == 4 ~ game_error_message)
    
    validate(need(x_val == 1, error_message)) 
    
    if(WEEK > 0) {
      def_df_raw <- load_participation(seasons = YEAR, include_pbp = TRUE) |> 
        filter(defteam == TEAM, week == WEEK, pass == 1 | rush == 1) |> 
        drop_na(offense_formation)
    } else {
      def_df_raw <- load_participation(seasons = YEAR, include_pbp = TRUE) |> 
        filter(defteam == TEAM, pass == 1 | rush == 1) |> 
        drop_na(offense_formation)
    }
    
    def_formation1 <- def_df_raw |> 
      mutate(snap_num = row_number(), .before = play_id) |> 
      select(week, snap_num, posteam, defteam, gsis_id = defense_players, defense_personnel, epa, pass) |> 
      separate_rows(defense_personnel, sep = ",") |> 
      mutate(defense_personnel = str_squish(defense_personnel)) |> 
      separate(defense_personnel, into = c("x1", "x2"), sep = " ") |> 
      mutate(x1 = as.numeric(x1)) |> 
      pivot_wider(names_from = "x2", values_from = "x1") |> 
      janitor::clean_names()
    
    OPP <- unique(def_formation1$posteam)
    
    def_formation2 <- def_formation1 |> 
      separate_rows(gsis_id, sep = ";")
    
    # Get player/roster info
    def_player_positions <- load_players() |> 
      select(short_name, gsis_id, position_group, position)
    
    def_formation3 <-  
      left_join(def_formation2, def_player_positions, by = "gsis_id") |> 
      filter(position_group %in% c("DL", "LB", "DB")) |> 
      relocate(short_name, position_group, position, .after = gsis_id) |> 
      group_by(gsis_id) |> 
      mutate(player_snaps = n()) |> 
      ungroup() |> 
      mutate(position_group = factor(position_group, c("DL", "LB", "DB"))) |> 
      mutate(formation = paste(dl, lb, db, sep = "-")) |> 
      arrange(position_group, player_snaps, desc(short_name))
    
    def_formation_team1 <- def_formation3 |> 
      select(snap_num, formation) |> 
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
    
    def_formation4 <- def_formation3 |> 
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
    
    def_formation5 <- def_formation4 |> 
      mutate(form_data = paste(formation_player_snaps, " ", form_percent, "%", sep = "")) |> 
      select(-form_percent, -formation_player_snaps) |> 
      pivot_wider(names_from = formation, values_from = form_data) |> 
      arrange(position_group, -filtered_player_snaps) |> 
      mutate(calc_total = paste0(filtered_player_snaps, " ", filtered_player_snap_percent, "%")) |> 
      select(-player_snaps, -snap_percent, -filtered_player_snaps, -filtered_player_snap_percent, -gsis_id) |> 
      mutate(sort_order = row_number() + 3)
    
    def_epa_formation <- def_formation3 |> 
      filter(formation %in% FILTERED_FORMATIONS) |> 
      select(snap_num, formation, epa) |> 
      distinct() |> 
      mutate(calc_total = as.character(round(mean(epa), 2))) |> 
      group_by(calc_total, formation) |> 
      summarize(formation_epa = as.character(round(mean(epa), 2))) |> 
      ungroup() |> 
      mutate(name = "EPA per Play") |>
      mutate(position_group = "Team") |> 
      pivot_wider(names_from = formation, values_from = formation_epa) |> 
      mutate(sort_order = 2)
    
    def_pass_rate_formation <- def_formation3 |> 
      filter(formation %in% FILTERED_FORMATIONS) |> 
      select(snap_num, formation, pass) |> 
      distinct() |> 
      mutate(calc_total = paste0(round((sum(pass)/ n()) * 100, 0), "%")) |> 
      group_by(calc_total, formation) |> 
      summarize(pass_rate = paste0(round((sum(pass)/ n()) * 100, 0), "%")) |> 
      mutate(name = "Pass Rate") |>
      mutate(position_group = "Team") |> 
      pivot_wider(names_from = formation, values_from = pass_rate) |> 
      mutate(sort_order = 3)
    
    def_formation <- bind_rows(def_formation_team, def_epa_formation, def_pass_rate_formation, def_formation5) |> 
      arrange(sort_order) |> 
      rename(total = calc_total) |> 
      select(-sort_order)
    
    home_team_abbr <- load_teams() |> 
      filter(team_abbr == def_df_raw$home_team[1]) 
    
    away_team_abbr <- load_teams() |> 
      filter(team_abbr == def_df_raw$away_team[1])
    
    
    # Table-------------------------------------------------------------------------
    
    def_formation_cols <- ncol(def_formation) - 1
    
    def_team_html <- paste0('<div style = "line-height:0.01"><img height="20" vertical-align="middle" src="',
                            teams_df$team_wordmark,
                            '"></div>')
    
    def_week_html <- paste0('<div style = "display:flex;justify-content:center;align-items:center;"><img height="70"src="',
                            away_team_abbr$team_logo_espn,
                            '">',
                            "&nbsp;",
                            away_team_abbr$team_nick,
                            "&nbsp;",
                            def_df_raw$away_score[1], 
                            " &nbsp;@&nbsp; ",
                            def_df_raw$home_score[1], 
                            "&nbsp;",
                            home_team_abbr$team_nick, 
                            '&nbsp;',
                            '<img height="70"src="',
                            home_team_abbr$team_logo_espn,
                            '">', 
                            '</div>'
    )
    
    def_season_html <- paste0('<div style = "display:flex;justify-content:center;align-items:center;"><img height="70"src="',
                              teams_df$team_logo_espn[1],
                              '">',
                              "&nbsp;",
                              teams_df$team_nick[1],
                              "&nbsp;",
                              YEAR, 
                              ' Season', 
                              "&nbsp;",
                              '<img height="70"src="',
                              teams_df$team_logo_espn[1],
                              '">', 
                              '</div>'
    )
    
    def_title_html <- case_when(
      WEEK > 0 ~ def_week_html, 
      TRUE ~ def_season_html
    )
    
    def_week_subtitle <- paste0(TEAM_NICK, " Defense: Week ", WEEK, ", ", YEAR)
    def_season_subtitle <- paste0(TEAM_NICK, " Defense: Weeks 1-", max(def_df_raw$week))
    
    def_subtitle <- case_when(
      WEEK > 0 ~ def_week_subtitle, 
      TRUE ~ def_season_subtitle
    )
    
    def_week_filename <- paste0("Week ", WEEK, " ", TEAM, " Defense vs ", OPP, ".png")
    def_season_filename <- paste0(YEAR, " Season ", teams_df$team_nick[1], " Defense.png")
    
    def_filename <- case_when(
      WEEK > 0 ~ def_week_filename, 
      TRUE ~ def_season_filename
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
        tab_source_note(source_note = "Table: @josephjefe     |     Data: #nflverse") 
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
      if (input$week_var > 0) {
        paste0("Week ", input$week_var, " ", input$team_var, " Offense.png")
      } else {
        paste0(input$year_var, " Season ", input$team_var, " Offense.png")
      }
    },
    content = function(file) {gtsave(offense_table(), file = file)},
    contentType = "image/png"
  )
  
  output$download2 <- downloadHandler(
    filename = function() {
      if (input$week_var > 0) {
        paste0("Week ", input$week_var, " ", input$team_var, " Defense.png")
      } else {
        paste0(input$year_var, " Season ", input$team_var, " Defense.png")
      }
    },
    content = function(file) {gtsave(defense_table(), file = file)},
    contentType = "image/png"
  )
}




# Run the application 
shinyApp(ui = ui, server = server)

