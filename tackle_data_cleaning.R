#load in packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(nflfastR)
library(nflverse)
library(ggrepel)
library(ggthemes)
library(gt)

#construct a gt theme similar to the gt theme 538 from Thomas Mock, with slight mods
#source: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/
gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        #change font to Roboto
        google_font("Roboto"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "transparent",
      table.border.top.width = px(12),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      heading.subtitle.font.size = 15,
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 24,
      #center align
      heading.align = "center",
      ...
    ) 
}

##data preparation##
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
max_week = max(pbp_data$week)


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

#get team colors
player_info = left_join(player_info, teams_colors_logos, by = c("team" = "team_abbr")) %>% 
  select(season, team, position, full_name, first_name, last_name, gsis_id, team_color, team_color2, team_wordmark)


#join binded_tackle_frame with player_info 
complete_base_frame = left_join(binded_tackle_frame, player_info, by = c("tackler_id" = "gsis_id", "season" = "season"))

#filter to defensive positions
complete_base_frame = complete_base_frame %>% 
  filter(position %in% c("EDGE", "IDL", "ILB", "S", "CB"))

#make unique list of names
tacklers = unique(complete_base_frame$full_name)

#set this
options(ggrepel.max.overlaps = 20)

###
#shiny app construction

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Average Depth of Run Tackle"),
  
  mainPanel(
    navbarPage("By: Drezdan Dale",
               
               #page 1
               tabPanel("2023 by Position",
                 fluidRow(
                   column(4, align = "center",
                     selectInput("position_select", "Select Position", choices = c("EDGE", "IDL", "ILB", "S", "CB"), selected = "ILB")),
                   column(7, align = "left",
                     sliderInput("min_tackles", "Minimum Tackles", value = max_week*3, min = max_week*2 , max = max_week*7, step = 1)),
                   column(4, align = "center",
                     sliderInput("yard_range", "Tackle Depth Range", value = c(-10, 10), min = -20, max = 75, step = 1))
               ),
               mainPanel(
                 plotOutput(outputId = "tackler_graph", width = "100%", height = "50%"),
                 br(),
                 tableOutput(outputId = "tackler_table"),
                 br()
                 ),
               ),
               
               #page 2
               tabPanel("By Player",
                        fluidRow(
                          column(4, align = "center",
                                 selectInput("player_select", "Select Player", sort(tacklers), selected = "Aaron Donald"),
                                 sliderInput("yard_range_2", "Tackle Depth Range", value = c(-10, 10), min = -20, max = 75, step = 1)
                          )
                        ),
                      mainPanel(
                          plotOutput(outputId = "lollipop_player", width = "100%", height = "50%"),
                          br(),
                          tableOutput(outputId = "tackler_table_2"),
                          br()
                        ),
               ),
               
               #page 3
               tabPanel("About",
                        fluidRow(
                          column(10,align = "left",
                                 tags$h4("Average Depth of Run Tackle (ADORT) is meant to provide context to a counting statistic. By 
                                         measuring how far down the field a run tackle occured, we can better understand how much impact
                                         a defender's tackles truly had. This app will hopefully help people understand those tackles more easily."),
                                 br(),
                                 tags$h4("To note, all of the tackles and statistics within this app are for designed rushing plays only, meaning that
                                         all QB Scrambles are filtered out. Also, please use the sliders to filter out outlier tackles - if 
                                         a player has 10 tackles 75 yards down the field, that will skew his average tackle depth, but should not
                                         necessarily be used to judge that player."),
                                 br(),
                            
                                 tags$h4("As for actually building this Shiny App, I used free publicly available data via nflfastR. While I have used
                                         nflfastR over the past 2 years, this was the first Shiny App I have ever designed; I used several online
                                         resources to learn Shiny, including Tej Seth's RYOE Shiny App, the Data Professor's YouTube videos, and 
                                         O'Reilly's Shiny online book."),
                                 br(),
                                 tags$h4("Feel free to connect with me on LinkedIn (search Drezdan Dale), Twitter (@drezdan_dale), or email (drez.data@gmail.com).
                                        I am always open to feedback or questions! And I will be iterating on this app more in the future (EPA will be getting added).")
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
    group_by(tackler_id, full_name, team_color, team_color2) %>%
    summarize(adort = mean(yards_gained),
              num_tackles = n()) %>%
    filter(num_tackles >= max_week*2) %>%
    ungroup() 
  
  filtered_aggregated_data = 
    aggregated_data %>%
    filter(num_tackles >= input$min_tackles)
  
  most_tackles = max(aggregated_data$num_tackles)
  
  least_tackles = min(aggregated_data$num_tackles)
  
  highest_adort = max(aggregated_data$adort)
  
  lowest_adort = min(aggregated_data$adort)
  
  
  filtered_aggregated_data %>%
    ggplot(aes(x = num_tackles, y = adort)) + 
    geom_point(aes(fill = team_color, color = team_color2), size = 3, cex = 0.6) +
    geom_hline(yintercept = mean(aggregated_data$adort), color = "red", linetype = "dashed", alpha=0.5) + 
    geom_vline(xintercept = mean(aggregated_data$num_tackles), color = "red", linetype = "dashed", alpha = 0.5)+
    annotate("text", x = mean(aggregated_data$num_tackles), y = min(aggregated_data$adort), label = "Positional Avg", color = "red")+
    annotate("text", y = mean(aggregated_data$adort), x = 0, label = "Positional\nAvg", color = "red") +
    annotate("label", y = lowest_adort + 1, x = least_tackles - 7, label = "Less Tackles,\n Lower ADORT", color = "red") +
    annotate("label", y = highest_adort - 0.5, x = least_tackles - 7, label = "Less Tackles,\n Higher ADORT", color = "red") +
    annotate("label", y = lowest_adort + 1, x = most_tackles + 3, label = "More Tackles,\n Lower ADORT", color = "red") +
    annotate("label", y = highest_adort - 0.5, x = most_tackles + 3, label = "More Tackles,\n Higher ADORT", color = "red") +
    scale_color_identity(aesthetics =  c("fill", "color")) +
    geom_text_repel(aes(label = full_name)) +
    theme_fivethirtyeight()+
    labs(x = "# of Tackles",
         y = "ADORT",
         caption = "By: Drezdan Dale | data via nflfastR",
         title = paste("Number of Rushing Tackles and ADORT by", input$position_select),
         subtitle = paste("For 2023 season: averages computed using minimum of", max_week * 2, 
         "tackles, with tackles occuring between",input$yard_range[1], "and", input$yard_range[2], "yards from LOS. \n Designed rushing plays only.")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
          plot.subtitle = element_text(hjust = 0.5, size = 14),
          axis.title = element_text(face = "bold", size = 18),
          plot.caption = element_text(face = "bold", size = 10),
          axis.text = element_text(size = 16)) + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8), limits = c(0,max(aggregated_data$num_tackles) + 5)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8), limits = c(min(aggregated_data$adort), max(aggregated_data$adort))) 
    
  }, height = 600, width = 850)

  output$tackler_table <- render_gt({ 
    aggregated_data = 
      complete_base_frame %>%
      filter(position == as.character(input$position_select)) %>%
      filter(yards_gained >= input$yard_range[1] & yards_gained <= input$yard_range[2])
    
    aggregated_data = 
      aggregated_data %>%
      group_by(tackler_id, full_name, team_color, team_color2, team_wordmark) %>%
      summarize(adort = mean(yards_gained),
                num_tackles = n()) %>%
      filter(num_tackles >= max_week*2) %>%
      ungroup() 
    
    filtered_aggregated_data = 
      aggregated_data %>%
      filter(num_tackles >= input$min_tackles)
    
    
    filtered_aggregated_data %>%
      arrange(adort) %>%
      mutate(adort = round(adort, digits = 2),
             rank = row_number()) %>%
      select(rank, full_name, team_wordmark, num_tackles, adort) %>%
      gt() %>%
      text_transform(
        locations = cells_body(c(team_wordmark)),
        fn = function(x){
          web_image(url = x, height = px(25))}) %>% 
      tab_header(
        paste("Number of Tackles and ADORT by", input$position_select),
        subtitle = paste("For 2023 season: averages computed using minimum of", max_week * 2, 
                         "tackles, with tackles occuring between",input$yard_range[1], "and", input$yard_range[2], "yards from LOS")
      ) %>%
      cols_label(rank = "Rank",
                full_name = "Player",
                num_tackles = "Number of Tackles",
                 adort = "ADORT",
                 team_wordmark = "Team") %>%
      cols_width(rank ~ 50,
                num_tackles ~ 200,
                 full_name ~ 200,
                 adort ~ 200,
                 team_wordmark ~ 100) %>%
      data_color(
        columns = c(adort),
        colors = scales::col_numeric(
          palette = "PRGn",
          domain = NULL,
          reverse = TRUE)) %>%
      tab_source_note(source_note = md("By Drezdan Dale | data via nflfastR")) %>%
      gt_theme_538(table.width = px(550))
    
    
    
  }, width = 850)
  
  
  output$lollipop_player <- renderPlot({
    
    complete_base_frame %>%
      filter(full_name == as.character(input$player_select) & 
               yards_gained >= input$yard_range_2[1] & yards_gained <= input$yard_range_2[2]) %>%
      group_by(yards_gained) %>%
      summarize(num_tackles = n()) %>%
      ggplot(aes(x = yards_gained, y = num_tackles)) + geom_point(size = 4) +
      geom_segment(aes(x = yards_gained, xend = yards_gained, 
                       y = 0, yend = num_tackles), linewidth = 2) +
      theme_fivethirtyeight() +
      labs(x = "Tackle Depth", 
           y = "Number of Tackles",
           title = paste(input$player_select, ": Number of Tackles per Tackle Depth", sep = ''),
           subtitle = paste("For 2023 season: with tackles occuring between",input$yard_range_2[1], 
                            "and", input$yard_range_2[2], "yards from LOS. \n Designed rushing plays only."),
           caption = "By: Drezdan Dale | data via nflfastR") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
            plot.subtitle = element_text(hjust = 0.5, size = 14),
            axis.title = element_text(face = "bold", size = 18),
            plot.caption = element_text(face = "bold", size = 10),
            axis.text = element_text(size = 16)) +
      scale_y_continuous(breaks = seq(from = 0, to = 10, by = 1))
    
  }, height = 600, width = 850)
  
  
  
  output$tackler_table_2 <- render_gt({ 
  
    tackle_depth_gt_prep = 
      complete_base_frame %>%
      filter(full_name == as.character(input$player_select) & 
               input$yard_range_2[1] & yards_gained <= input$yard_range_2[2]) %>%
      mutate(yard_grouping = case_when(yards_gained < 0 ~ 'TFL',
                                       yards_gained >= 0 & yards_gained < 3 ~ '0-2',
                                       yards_gained >= 3 & yards_gained < 6 ~ '3-5',
                                       yards_gained >= 6 ~ '6+'
      )) %>%
      group_by(yard_grouping) %>%
      summarize(num_tackles = n()) %>%
      mutate(yard_grouping_order = case_when(yard_grouping == 'TFL' ~ 1,
                                             yard_grouping == '0-2' ~ 2,
                                             yard_grouping == '3-5' ~ 3,
                                             yard_grouping == '6+' ~ 4)) %>%
      arrange(yard_grouping_order) %>%
      select(yard_grouping, num_tackles) %>%
      ungroup()
    
    tackle_depth_gt_prep %>%
      gt(rowname_col  = "yard_grouping") %>%
      tab_header(
        title = paste(input$player_select,": Depth of Run Tackle Distribution", sep = ''),
        subtitle =  paste("By Area for 2023 Season; yard range:",input$yard_range_2[1], 
                            "to", input$yard_range_2[2], "yards from LOS")
                          ) %>%
      cols_label(num_tackles = "Number of Tackles") %>%
      grand_summary_rows(
        fns = Total ~ sum( na.rm = TRUE, columns = num_tackles)
      ) %>%
      cols_width(num_tackles ~ 200,
                 yard_grouping ~ 100) %>%
      data_color(
        columns = c(num_tackles),
        colors = scales::col_numeric(
          palette = "PRGn",
          domain = c(0, max(tackle_depth_gt_prep$num_tackles)))) %>%
      cols_align(
        align = "center",
        columns = num_tackles)%>%
      tab_source_note(source_note = md("By Drezdan Dale | data via nflfastR | designed rushing plays only")) %>%
      gt_theme_538(table.width = px(550))
    
    
  }, width = 850)
  
}

#run app
shinyApp(ui = ui, server = server)


