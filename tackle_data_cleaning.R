#load in packages
library(shiny)
library(shinythemes)
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
    select(season, team, depth_chart_position, full_name, first_name, last_name, gsis_id) %>%
  mutate(position = 
    case_when(depth_chart_position == "DE" | depth_chart_position == "OLB" ~ "EDGE",
              depth_chart_position == "DT" | depth_chart_position == "NT" ~ "IDL",
              depth_chart_position == "ILB" | depth_chart_position == "MLB"  |depth_chart_position == "LB" ~ "ILB",
              depth_chart_position == "SS" | depth_chart_position == "FS"  |depth_chart_position == "DB" ~ "S",
              depth_chart_position == "CB" ~ "CB",
              TRUE ~ as.character(depth_chart_position)
    
  ))

#join binded_tackle_frame with player_info 
complete_base_frame = left_join(binded_tackle_frame, player_info, by = c("tackler_id" = "gsis_id", "season" = "season"))

complete_base_frame = complete_base_frame %>% 
  filter(position %in% c("EDGE", "IDL", "ILB", "S", "CB"))

#min tackles
#max tackles

tacklers = unique(complete_base_frame$full_name)


#shiny app construction


ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Average Depth of Run Tackle"),
  
  mainPanel(
    navbarPage("By: Drezdan Dale",
               tabPanel("By Season",
                 fluidRow(
                   column(4, align = "center",
                     selectInput("position_select", "Select Position", choices = c("EDGE", "IDL", "ILB", "S", "CB"), selected = "EDGE"),
                     sliderInput("min_tackles", "Minimum Tackles", value = 21, min = min_tackles, max = max_tackles, step = 1),
                     sliderInput("yard_range", "Tackle Depth Range", value = c(-10, 10), min = -20, max = 75, step = 1)
               )
               ),
               mainPanel(
                 plotOutput(outputId = "tackler_graph")),
               ),
               tabPanel("By Player",
                        fluidRow(
                          column(4, align = "center",
                                 selectInput("player_select", "Select Player", sort(tacklers), selected = "Aaron Donald"),
                                 sliderInput("yard_range_2", "Tackle Depth Range", value = c(-10, 10), min = -20, max = 75, step = 1)
                          )
                        ),
               ),
               tabPanel("About",
                        fluidRow(
                          column(10,align = "left",
                                 tags$h4("I built this app using free resources from the nflfastr package and several resources on Twitter.
                                        This is my first shiny app and was a great learning resource for me. I learned how to construct a shiny app
                                        from reading through O'Reilly's Mastering Shiny free e-book and by referencing Tej Seth's Shiny Code on his GitHub
                                        as it is extremely easy to follow."),
                                 br(),
                                 tags$h4("As for the nflfastr data side, a lot of the learning here has been done over the past 3 years from much 
                                 of Ben Baldwin's work on the nflfastR website. Average Depth of Run Tackle has always been an interesting statistic for me as 
                                         I feel it adds much needed context to the tackle box-score statistic."),
                                 br(),
                                 tags$h4("Feel free to connect with me on LinkedIn (search Drezdan Dale), Twitter (@drezdan_dale), or email (drez.data@gmail.com).
                                        I am always open to feedback or questions!")
                                        
                          )
                        )
               )
    )
  )
)

server <- function(input, output) {
  
  output$tackler_graph <- renderPlot({
    
  aggregated_data = 
  complete_base_frame %>%
    filter(position == as.character(input$position_select)) %>%
    filter(yards_gained >= input$yard_range[1] & yards_gained <= input$yard_range[2])
  
  aggregated_data = 
    aggregated_data %>%
    group_by(tackler_id, full_name) %>%
    summarize(adort = mean(yards_gained),
              num_tackles = n()) %>%
    filter(num_tackles >= input$min_tackles) %>%
    ungroup() 
  
  aggregated_data %>%
    ggplot() + geom_point(aes(x = num_tackles, y = adort))
  
  }, height = 600, width = 850)
}

shinyApp(ui = ui, server = server)























