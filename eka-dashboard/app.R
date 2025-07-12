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
library(shinydashboard)
library(DT)
library(scales)
library(ggtext)
# setwd(here::here("eka-dashboard"))
player_trade_tbl <- read_csv(here::here("player_trade_tbl.csv"))
boxscore_tbl <- read_csv(here::here("boxscore_tbl.csv"))
full_coefs <- read_csv(here::here("coefs_full.csv"))

full_coefs %>%
    slice(3:n()) ->
    clean_coefs
clean_coefs %>%
    pivot_longer(-player_id) %>%
    group_by(name) %>%
    mutate(percentile = percent_rank(value)) ->
    coefs_long

league_avgs <- 
    coefs_long %>%
    group_by(name) %>%
    summarise(avg = mean(value, na.rm = T))

coefs_long <- 
    coefs_long %>%
    left_join(league_avgs, by = 'name')



body <- dashboardBody(
    fluidRow(
        box(
            tableOutput("base_stats")
        ),
        box(
            plotOutput('betas')
        )
    ),
    fluidRow(
        box(width = 12,
            DTOutput("coef_table")
        )
    )
)

# Define UI for application that draws a histogram
ui <- dashboardPage( 
    dashboardHeader(title = "My Dashboard"),
    dashboardSidebar(disable = TRUE),
    body
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    clean_coefs %>%
        left_join(boxscore_tbl %>%
                      filter(stat == 'epm'),
                  by = c("player_id" = 'name')) %>%
        mutate(eng_value_10 = (-p_death*p_no_trade_att_death-
                   p_death*p_fail_trade+
                   p_kill*p_trade_att_kill+
                   p_kill*p_two_piece)*value*10) %>%
        select(player_id,
               kill = p_kill,
               no_trade_attempted_kill = p_no_trade_att_kill,
               two_piece = p_two_piece,
               trade_attempted_death = p_trade_att_death,
               traded_death = p_traded_death,
               eng_value_10) %>%
        DT::datatable() %>%
        formatPercentage(c(2:6), 1) %>%
        formatRound(c(7), digits = 2)->
        coef_table
    
    
    data <- reactive({
        
        if(length(input$coef_table_row_last_clicked) > 0) {
            player_selected <- clean_coefs[input$coef_table_row_last_clicked, 'player_id'] %>%
                pull()
        } else {
            player_selected <- "ABEZY"
        }
        
        
        player_trade_tbl %>%
            filter(player_id == player_selected) %>%
            mutate(name = recode(stat,
                                 kill_pct  = 'Gunfight Win%',
                                 no_trade_att_kill_pct = 'Kill - No Trade Attempt%',
                                 two_piece_pct = 'Get Two-Piece',
                                 trade_att_death_pct  = 'Death - Trade Attempt%',
                                 traded_death_pct = "Death Traded Success%"
            )) ->
            # select(player_id,
            #        kill,
            #        no_trade_attempted_kill,
            #        two_piece,
            #        trade_attempted_death,
            #        traded_death) ->
            box_coefs
        boxscore_tbl %>%
            filter(name == player_selected) %>%
            rename(player_id = name) ->
            boxscore_stats
        
        coefs_long %>%
            filter(player_id == player_selected, name %in% c(
                'p_kill',
                'p_no_trade_att_kill',
                'p_two_piece',
                'p_trade_att_death',
                'p_traded_death'
            )) %>%
            mutate(name = recode(name,
                p_kill  = 'Gunfight Win%',
                p_no_trade_att_kill = 'Kill - No Trade Attempt%',
                p_two_piece = 'Get Two-Piece',
                p_trade_att_death  = 'Death - Trade Attempt%',
                p_traded_death = "Death Traded Success%"
            ),
            ACTUAL = FALSE) %>%
            bind_rows(
                player_trade_tbl %>%
                    filter(player_id == player_selected, stat %in% c(
                        'kill_pct',
                        'no_trade_att_kill_pct',
                        'two_piece_pct',
                        'trade_att_death_pct',
                        'traded_death_pct'
                    )) %>%
                    mutate(name = recode(stat,
                                         kill_pct  = 'Gunfight Win%',
                                         no_trade_att_kill_pct = 'Kill - No Trade Attempt%',
                                         two_piece_pct = 'Get Two-Piece',
                                         trade_att_death_pct  = 'Death - Trade Attempt%',
                                         traded_death_pct = "Death Traded Success%"
                    ),
                    ACTUAL = TRUE)
            ) %>%
            mutate(name = factor(name, levels = c(
                'Gunfight Win%',
                'Kill - No Trade Attempt%',
                'Get Two-Piece',
                'Death - Trade Attempt%',
                "Death Traded Success%"
            ))) ->
            player_coefs

        
        return(list(boxscore_stats = boxscore_stats,
                    player_coefs = player_coefs,
                    coef_table = coef_table))
    })
    
    output$base_stats <- renderTable({
        data()$boxscore_stats
    })
    
    output$betas <- renderPlot({
        ggplot()+
            geom_vline(xintercept = 0.5, size = 2, alpha = 0.3) +
            geom_point(data = data()$player_coefs %>% filter(ACTUAL),
                       aes(percentile, name),pch = "|", cex = 10, color = 'blue')+
            geom_label(data = data()$player_coefs %>% filter(!ACTUAL),
                      aes(percentile, name, label = scales::percent(value,accuracy = 0.1)),
                      size = 3)+
            labs(x = "Percentile", y = element_blank(),
                 title = "<b>Individual Player Impacts vs. Actual Stats</b><br>
    <span style = 'font-size:10pt'>Player estimates are labeled, but plotted on league
    percentile scale. A player's<span style = 'color:blue;'> season ACTUAL stats</span> 
    are indicated by a line. Differences between impact and actual values are things
                 outside a player's control (teammates, man-down situations, etc.)</span>")+
            lims(x = c(0,1))+
            theme_bw() +
            theme(panel.border = element_rect(color = '#3d1866', fill = NA),
                  axis.text = element_text(face = 'bold', color = '#3d1866', size = 16),
                  axis.title = element_text(face = 'bold', color = '#3d1866', size = 17),
                  legend.text = element_text(face = 'bold', color = '#3d1866', size = 12),
                  legend.title = element_text(face = 'bold', color = '#3d1866', size = 14),
                  title = element_text(face = 'bold'),
                  # text = element_text(family = 'mono'),
                  plot.title = element_textbox_simple(
                      size = 18,
                      lineheight = 1,
                      padding = margin(5.5, 5.5, 5.5, 5.5),
                      margin = margin(0, 0, 5.5, 0)
                  ))
    })
    
    output$coef_table <- renderDT(options = list(
                                      selection = 'single'
                                  ),{
        coef_table
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
