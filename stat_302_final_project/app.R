# load packages -------------------------------------
library(tidyverse)
library(dplyr)
library(shiny)
library(shinydashboard)
library(rsconnect)

# load data / data cleaning & wrangling -------------------------------------
game_data <- read_delim('data/game_data.csv', delim = ',') |>
  
  # replace NAs in _items columns with "No Second/Third Item"
  mutate(
    second_item = ifelse(is.na(second_item), 'No Second Item', second_item),
    third_item = ifelse(is.na(third_item), 'No Third Item', third_item)
  )

## convert date column to Date type 
game_data$date <- as.Date(game_data$date, format = '%m/%d')

## Get month and year of the data
min_month <- min(format(game_data$date, "%m"))
min_year <- min(format(game_data$date, "%Y"))
max_month <- max(format(game_data$date, "%m"))
max_year <- max(format(game_data$date, "%Y"))

## Create sequence of dates for a whole month ** ERROR IN HERE** 
month_dates <- seq(as.Date(paste0(min_year, "-", min_month, "-01")), 
                   as.Date(paste0(max_year, "-", max_month, "-01")) - 1, by = "day")

## Create modified data set of number of games played per day
games_per_day <- tibble(date = month_dates) |>
  left_join(game_data |>
              group_by(date) |>
              summarise(games_played = n()), by = 'date') |>
  mutate(games_played = replace_na(games_played, 0))

## find the top 3 lane roles 
top_roles <- game_data |>
  count(role) |>
  arrange(desc(n)) |>
  slice(1:3) |>
  mutate(percentage = n / nrow(game_data))

## create another dataset for the top three roles and their frequency
top_roles_data <- top_roles |>
  select(role, percentage )

## get all the unique charaacters in character column & sort alphabetically
unique_characters <- sort(unique(game_data$character))

# UI -------------------------------------
ui <- dashboardPage(
  skin = 'purple',
  dashboardHeader(title = '2024 Spring Quarter League Stats',
                  titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Game Stats', tabName = 'game_stats', icon = icon('dashboard')),
      menuItem('Character Stats', tabName = 'character_stats', icon = icon('user-group')),
      menuItem('Additional Information', tabName = 'additional_info', icon = icon('file-lines'))
    )
  ),
  dashboardBody(
    
    # game stats tab
    tabItems(tabItem(tabName = 'game_stats',
            h2('General Game Statistics'),
            
            fluidRow(
              
              # render top 3 rows plot 
              box(title = 'Top 3 Roles',
                  status = 'primary',
                  width = 6,
                  plotOutput('top_roles', height = '300px')
              ), 
              
              # boxes for total games + overall win rate + average game time 
              column(
                width = 6,
                
                # total # of games obtained by calculating # of observations
                valueBox(nrow(game_data), 'Total Games', icon = icon('gamepad'), 
                         color = 'blue', width = 12),
                
                # win rate
                valueBox(paste0(round(mean(game_data$win_lose == 'Yes') * 100, 2), '%'), 'Win Rate', icon = icon('trophy'), 
                         color = 'yellow', width = 12),
                
                # average game length in minutes 
                valueBox(round(mean(game_data$game_length_min), 2), 'Average Game Length (minutes)', icon = icon('clock'),
                         color = 'red', width = 12)
              )
            ),
            fluidRow(
              
              # drop down selection menu for month
              column(
                width = 3,
                box(
                  title = 'Choose Month',
                  status = 'primary',
                  width = NULL,
                  selectInput('selected_month', 'Month:',
                              choices = c("All Months", unique(format(games_per_day$date, "%B"))),
                              selected = "All Months"))),
              
              # render games played plot 
              column(
                width = 9,
                box(
                  title = 'Games Played Per Day', 
                  status = 'primary', 
                  width = NULL, 
                  plotOutput('game_data_day', height = '275px')))
                )),
            
    # character stats tab        
    tabItem(tabName = 'character_stats',
            h2('Character Statistics'),
              
            # drop down selection menu for character
            fluidRow(
              column(
                width = 5,
                box(title = 'Select Character',
                    status = 'primary',
                    width = NULL,
                    selectInput('selected_character', 'Character:',
                                choices = unique_characters,
                                selected = unique_characters[1])),
                box(title = 'Wins vs. Losses',
                    status = 'primary',
                    width = NULL,
                    height = '325px',
                    plotOutput('win_lose_pie', height = '270px'))),
              
              # render games played per selected character plot
              column(
                width = 7,
                box(title = uiOutput('character_title'),
                    status = 'primary',
                    width = NULL,
                    plotOutput('character_games_played')
                   )
              )
            ),
            
            # row for average kill/death/assist and cs
            fluidRow(
              uiOutput('character_average_stats'))
            ),
    tabItem(tabName = 'additional_info',
            h2('Additional Information'),
            p('An analysis of the League of Legend Games that I have played this Spring Quarter (2024)!'),
            p('You can access the data', a('here', href = 'https://docs.google.com/spreadsheets/d/1j7cgSQ0gv708b98JkoBLPU0DihQwaAHH81BvxIHPBeQ/edit?usp=sharing')))
  )
 ) 
)
# server ------------------------------------- 
server <- function(input, output) {
  
  # construct plot #1, barplot showing games played on each day of the month
  output$game_data_day <- renderPlot({
    
    # creates copy of games_per_day
    filtered_data <- games_per_day
    
    # filter based on selected month
    if (input$selected_month != "All Months") {
      selected_month <- input$selected_month
      filtered_data <- filtered_data |>
        filter(format(date, "%B") == selected_month)
    }
    
    # plot
    ggplot(data = filtered_data, aes(x = date, y = games_played)) + 
      geom_bar(stat = 'identity', fill = 'lightblue') +
      
      # only label days where there were one or more games
      geom_text(data = filtered_data |>
                  filter(games_played != 0), 
                aes(label = games_played), vjust = -0.5, size = 4) +
      labs(x = 'Date') +
      scale_y_continuous(
        name = 'Number of Games Played',
        breaks = seq(0, 6),
        limits = c(0, 6.5)
      ) + 
      theme_minimal() +
      theme(
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14)
      )
  })
  
  # construct plot #2, the top 3 most played roles 
  output$top_roles <- renderPlot({
    
    # plot
    ggplot(data = top_roles_data, aes(x = reorder(role, -percentage), y = percentage * 100)) +
      geom_col(fill = 'lightblue') +
      geom_text(aes(label = scales::percent(percentage, accuracy = 0.01)),
                vjust = -0.5, size = 5) +
      scale_y_continuous(
        name = 'Frequency',
        breaks = seq(0, 100, 20),
        limits = c(0, 105),
        labels = scales::label_percent(scale = 1)
      ) + 
      xlab('Role') +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
      )
  })
  
  # define colors for each character 
  character_colors <- c(
    'Ezreal' = '#f8c566',
    'Gwen' = '#3277b4',
    'Lillia' = '#c07681',
    'Nunu & Willump' = '#4f8aba',
    'Seraphine' = '#f08cd3'
  )
  
  # construct plot 3, number of games played per selected character
  output$character_games_played <- renderPlot({
    
    # make another copy of game_data but only for selected_characters
    filtered_data_2 <- game_data |>
      filter(character == input$selected_character)
    
    # plot 
    ggplot(data = filtered_data_2, aes(x=date)) +
      geom_bar(stat = 'count', fill = character_colors[[input$selected_character]]) +
      geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 5) +
      labs(x = 'Date', y = 'Number of Games Played') +
      theme_minimal()
  })
  
  # generate valueBoxes for average kill/death/assists and cs for a selected character
  output$character_average_stats <- renderUI({
    
    # filter game data for selected character, again
    filtered_data_3 <- game_data |>
      filter(character == input$selected_character)
    
    # calculate average kill/death/assist and cs
    avg_kill <- round(mean(filtered_data_3$kill), 2)
    avg_death <- round(mean(filtered_data_3$death), 2)
    avg_assist <- round(mean(filtered_data_3$assist), 2)
    avg_cs <- round(mean(filtered_data_3$cs), 2)
    
    # create the fluidrow
    fluidRow(
      valueBox(
        avg_kill, 'Average Kills', icon = icon('hand-back-fist'),
        color = 'red', width = 3),
      
      valueBox(avg_death, 'Average Deaths', icon = icon('skull'),
        color = 'blue', width = 3),
      valueBox(avg_assist, 'Average Assists', icon = icon('handshake-angle'),
        color = 'green', width = 3),
      valueBox(avg_cs, 'Average Creep Score (CS)', icon = icon('people-group'),
        color = 'orange', width = 3)
    )
    
  })
  
  # generate custom title for selected character
  output$character_title <- renderUI({
    title <- paste('Games Played: ', input$selected_character)
    h4(title)
  })
  
  # pie chart for wins/losses for selected character
  output$win_lose_pie <- renderPlot({
    
    # filter game data for selected character... again...
    filtered_data_4 <- game_data |>
      filter(character == input$selected_character)
    
    # calculate win/lose ratio for selected character
    win_lose_data <- filtered_data_4 |>
      group_by(win_lose) |>
      summarise(count = n()) |>
      mutate(win_loss_ratio = count / sum(count))
    
    # plot
    ggplot(data = win_lose_data, aes(x = '', y = count, fill = win_lose)) +
      geom_bar(stat = 'identity') +
      geom_text(aes(label = scales::percent(win_loss_ratio, accuracy = 0.01)), 
                position = position_stack(vjust = 0.5)) +
      coord_polar('y', start = 0) +
      labs(fill = NULL,
           y = NULL,
           x = NULL) +
      scale_fill_manual(values = c('Yes' = '#619cff', 'No' = '#f8766d'),
                        labels = c('Yes' = 'Win', 'No' = 'Lose')) + 
      theme_minimal() +
      theme(legend.position = 'bottom',
            axis.text = element_blank()) 
  })
  
}

# Run the application -------------------------------------
shinyApp(ui = ui, server = server)
