library(shiny)
library(tidyverse)
library(bslib)
library(shinyWidgets)


#read/clean the dataframes
df_list = list('home_2021', 'away_2021', 'home_2020', 'away_2020', 'home_2019',
               'away_2019', 'home_2018', 'away_2018', 'home_2017', 'away_2017')
for(name in df_list) {
  year = strsplit(name, '_')[[1]][2]
  location = strsplit(name, '_')[[1]][1]
  url = sprintf('%s.csv', name)
  df = assign(name, read.csv(url))
  df = df[-1,]
  
  df = df %>%
    select(-eFG..Rank, -eFG., -Loc.eFG..Rank, -Loc.eFG., -All.Three.Rank, 
           -All.Three, -All.Mid.Rank, -All.Mid) %>%
    rename(Rim.Freq = Rim) %>%
    rename(Short.Mid.Freq = Short.Mid) %>%
    rename(Long.Mid.Freq = Long.Mid) %>%
    rename(Corner.Three.Freq = Corner.Three) %>%
    rename(Non.Corner.Freq = Non.Corner) %>%
    mutate(Rim.Color = ifelse(Rim.Rank %in% 1:10, 'Red', 
                              ifelse(Rim.Rank %in% 11:20, 'Gray',
                                     ifelse(Rim.Rank %in% 21:30, 'Blue', 'no')))) %>%
    mutate(Rim.Opacity = ifelse(Rim.Rank %in% 6:25, 0.3, 0.7)) %>%
    mutate(Short.Mid.Color = ifelse(Short.Mid.Rank %in% 1:10, 'Red', 
                                    ifelse(Short.Mid.Rank %in% 11:20, 'Gray',
                                           ifelse(Short.Mid.Rank %in% 21:30, 'Blue', 'no')))) %>%
    mutate(Short.Mid.Opacity = ifelse(Short.Mid.Rank %in% 6:25, 0.3, 0.7)) %>%
    mutate(Long.Mid.Color = ifelse(Long.Mid.Rank %in% 1:10, 'Red', 
                                   ifelse(Long.Mid.Rank %in% 11:20, 'Gray',
                                          ifelse(Long.Mid.Rank %in% 21:30, 'Blue', 'no')))) %>%
    mutate(Long.Mid.Opacity = ifelse(Long.Mid.Rank %in% 6:25, 0.3, 0.7)) %>%
    mutate(Corner.Three.Color = ifelse(Corner.Three.Rank %in% 1:10, 'Red', 
                                       ifelse(Corner.Three.Rank %in% 11:20, 'Gray',
                                              ifelse(Corner.Three.Rank %in% 21:30, 'Blue', 'no')))) %>%
    mutate(Corner.Three.Opacity = ifelse(Corner.Three.Rank %in% 6:25, 0.3, 0.7)) %>%
    mutate(Non.Corner.Color = ifelse(Non.Corner.Rank %in% 1:10, 'Red', 
                                     ifelse(Non.Corner.Rank %in% 11:20, 'Gray',
                                            ifelse(Non.Corner.Rank %in% 21:30, 'Blue', 'no')))) %>%
    mutate(Non.Corner.Opacity = ifelse(Non.Corner.Rank %in% 6:25, 0.3, 0.7)) %>%
    mutate(Year = year) %>%
    mutate(Location = location)
  name = assign(name, df)
}


#combine the dataframes into one
final_data = rbind(home_2021, away_2021, home_2020, away_2020, home_2019, 
                   away_2019, home_2018, away_2018, home_2017, away_2017)
write_csv(final_data, file = 'final_data.csv')




# Define UI for application
ui = navbarPage(theme = bs_theme(
  bootswatch = 'flatly'), # add theme
  # add logo
  title = tags$a(tags$img(src = 'NBA logo.png', height = '80', width = '130'),
                 'NBA Shot Chart By Team'),
  tabPanel(title = 'App', 
           titlePanel(p(tags$b('Apply Filters:', style = 'font-size: 110%'))),
           
           # Create inputs
           fluidPage(
             fluidRow(
               column(4, wellPanel(
                 selectInput(inputId = 'Team', label = p(tags$b('Team:', style = 'font-size: 110%')), choices = 
                               unique(final_data$Team)),
                 setSliderColor(c('#4169E1'), c(1)),
                 sliderInput(inputId = 'Year', label = p(tags$b('Season:', style = 'font-size: 110%')), min = 2017,
                             max = 2021, value = 2021, sep = ''),
                 radioButtons(inputId = 'Location', label = p(tags$b('Home/Away:', style = 'font-size: 110%')), choices = 
                                c('Home', 'Away')),
                 submitButton('Apply Changes', icon('refresh')), 
                 style = 'background-color: #E0E0E0; border-color: #2c3e50'
               )),
               
               # Show the plot
               fluidRow(
                 column(7,
                        plotOutput('plot', height = 450, width = 650), offset = 2)
               )),
             fluidRow(
               column(5, wellPanel(tableOutput('table2'))),
               column(4, p('***Above is a shot chart that shows how frequently a team
                             attemtps shots at each zone. The Red and Blue zones denote a high/low
                             volume of shots at that zone, respectively. The higher the opacity,
                             the more extreme the high/low volume of shots is.',
                           style = "font-size:16px"))
             )
           )
           
  ),
  
  tabPanel(title = 'DataTable', dataTableOutput('table')),
  tabPanel(title = 'About', includeMarkdown('about.Rmd'))
)



# Define server logic
server = function(input, output) {
  
  reactive_data = reactive({
    final_data %>%
      filter(Team == input$Team) %>%
      filter(Year == input$Year) %>%
      filter(Location == tolower(input$Location))
  })
  
  output$table = renderDataTable(final_data %>%
                                   select(-contains('Color'),
                                          -contains('Opacity')))
  
  output$plot = renderPlot({
    ggplot(data.frame(x = seq(-22,22,length = 1000),
                      x1 = seq(-14,14,length = 1000),
                      x2 = seq(-22,-14,length = 1000),
                      x3 = seq(14,22,length = 1000),
                      x4 = seq(-4,4,length = 1000),
                      x5 = seq(-14,-4,length = 1000),
                      x6 = seq(4,14,length = 1000),
                      x7 = seq(-0.05,0.05,length = 1000),
                      x8 = seq(-25,-22,length = 1000),
                      x9 = seq(22,25,length = 1000),
                      y = sapply(seq(-22,22,length = 1000), FUN = function(x){-10}),
                      y1 = sapply(seq(-22,22,length = 1000), FUN = function(x){-0.0051*(-x)^2.57 - 18.2}),
                      y2 = sapply(seq(-22,22,length = 1000), FUN = function(x){-0.00505*x^2.57 - 18.2}),
                      y3 = sapply(seq(-14,14,length = 1000), FUN = function(x){-0.0044*(-x)^2.98 - 28.3}),
                      y4 = sapply(seq(-14,14,length = 1000), FUN = function(x){-0.0065*(-x)^2.5 - 18}),
                      y5 = sapply(seq(-14,14,length = 1000), FUN = function(x){-0.0043*x^2.98 - 28.3}),
                      y6 = sapply(seq(-14,14,length = 1000), FUN = function(x){-0.0065*x^2.5 - 18}),
                      y7 = sapply(seq(-22,-14,length = 1000), FUN = function(x){-47}),
                      y8 = sapply(seq(0,22,length = 1000), FUN = function(x){1.05*x^0.74 - 46.8+169/12}),
                      y9 = sapply(seq(-22,0,length = 1000), FUN = function(x){1.05*(-x)^0.74 - 46.8+169/12}),
                      y10 = sapply(seq(-4,4,length = 1000), FUN = function(x){-0.055*(-x)^2.95 - 38}),
                      y11 = sapply(seq(-4,4,length = 1000), FUN = function(x){-0.018*(-x)^2.5 - 28}),
                      y12 = sapply(seq(-4,4,length = 1000), FUN = function(x){-0.053*x^2.95 - 38}),
                      y13 = sapply(seq(-4,4,length = 1000), FUN = function(x){-0.018*x^2.5 - 28}),
                      y14 = sapply(seq(0,14,length = 1000), FUN = function(x){4.65*x^0.395 - 41.5}),
                      y15 = sapply(seq(-14,0,length = 1000), FUN = function(x){4.65*(-x)^0.395 - 41.5}),
                      y16 = sapply(seq(-4,4,length = 1000), FUN = function(x){-0.09*(-x)^2.5 - 37.8}),
                      y17 = sapply(seq(-4,4,length = 1000), FUN = function(x){-0.09*x^2.5 - 37.8}),
                      y18 = sapply(seq(-1,1,length = 1000), FUN = function(x){-18}),
                      y19 = sapply(seq(-25,22,length = 1000), FUN = function(x){-47+169/12}),
                      y20 = sapply(seq(-1,1,length = 1000), FUN = function(x){-28})), 
           aes(x = x, y = y)) +
      
      ### Shade in zones
      #Non Corner Three Zone
      geom_ribbon(aes(x = x, ymin = y1, ymax = y), 
                  fill = reactive_data()$Non.Corner.Color, alpha = reactive_data()$Non.Corner.Opacity) +
      geom_ribbon(aes(x = x, ymin = y2, ymax = y), 
                  fill = reactive_data()$Non.Corner.Color, alpha = reactive_data()$Non.Corner.Opacity) +
      geom_ribbon(aes(x = x7, ymin = y18, ymax = y), 
                  fill = reactive_data()$Non.Corner.Color, alpha = reactive_data()$Non.Corner.Opacity) +
      geom_ribbon(aes(x = x8, ymin = y19, ymax = y), 
                  fill = reactive_data()$Non.Corner.Color, alpha = reactive_data()$Non.Corner.Opacity) +
      geom_ribbon(aes(x = x9, ymin = y19, ymax = y), 
                  fill = reactive_data()$Non.Corner.Color, alpha = reactive_data()$Non.Corner.Opacity) +
      #Corner 3 Zone
      geom_ribbon(aes(x = x8, ymin = y7, ymax = y19), 
                  fill = reactive_data()$Corner.Three.Color, alpha = reactive_data()$Corner.Three.Opacity) +
      geom_ribbon(aes(x = x9, ymin = y7, ymax = y19), 
                  fill = reactive_data()$Corner.Three.Color, alpha = reactive_data()$Corner.Three.Opacity) +
      #Long Range Zone
      geom_ribbon(aes(x = x1, ymin = y3, ymax = y4), 
                  fill = reactive_data()$Long.Mid.Color, alpha = reactive_data()$Long.Mid.Opacity) + 
      geom_ribbon(aes(x = x1, ymin = y5, ymax = y6), 
                  fill = reactive_data()$Long.Mid.Color, alpha = reactive_data()$Long.Mid.Opacity) + 
      geom_ribbon(aes(x = x2, ymin = y7, ymax = y8), 
                  fill = reactive_data()$Long.Mid.Color, alpha = reactive_data()$Long.Mid.Opacity) +
      geom_ribbon(aes(x = x3, ymin = y7, ymax = y9), 
                  fill = reactive_data()$Long.Mid.Color, alpha = reactive_data()$Long.Mid.Opacity) +
      #Mid Range Zone
      geom_ribbon(aes(x = x4, ymin = y10, ymax = y11), 
                  fill = reactive_data()$Short.Mid.Color, alpha = reactive_data()$Short.Mid.Opacity) +
      geom_ribbon(aes(x = x4, ymin = y12, ymax = y13), 
                  fill = reactive_data()$Short.Mid.Color, alpha = reactive_data()$Short.Mid.Opacity) +
      geom_ribbon(aes(x = x5, ymin = y7, ymax = y14), 
                  fill = reactive_data()$Short.Mid.Color, alpha = reactive_data()$Short.Mid.Opacity) +
      geom_ribbon(aes(x = x6, ymin = y7, ymax = y15), 
                  fill = reactive_data()$Short.Mid.Color, alpha = reactive_data()$Short.Mid.Opacity) +
      #Short Range Zone
      geom_ribbon(aes(x = x4, ymin = y7, ymax = y16), 
                  fill = reactive_data()$Rim.Color, alpha = reactive_data()$Rim.Opacity) +
      geom_ribbon(aes(x = x4, ymin = y7, ymax = y17), 
                  fill = reactive_data()$Rim.Color, alpha = reactive_data()$Rim.Opacity) +
      
      ### draw basketball court
      #court boundaries
      geom_path(data=data.frame(x=c(-25,-25,25,25,-25),y=c(-47,-10,-10,-47,-47)),aes(x=x,y=y), size = 0.75)+
      #solid FT semicircle above FT line:
      geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(28-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
      #key:
      geom_path(data=data.frame(x=-c(-8,-8,8,8,-8),y=-c(47,28,28,47,47)),aes(x=x,y=y))+
      #box inside the key:
      geom_path(data=data.frame(x=c(-6,-6,6,6,-6),y=-c(47,28,28,47,47)),aes(x=x,y=y))+
      #three-point line:
      geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=-c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y), size = 0.75)+
      ###boundaries for the different zones
      #corner 3
      geom_path(data=data.frame(x=c(-25,-22),y=c(-47+169/12, -47+169/12)),aes(x=x,y=y), size = 0.75)+
      geom_path(data=data.frame(x=c(22,25),y=c(-47+169/12, -47+169/12)),aes(x=x,y=y), size = 0.75)+
      #mid-range
      geom_path(data=data.frame(x=c(-14,-14,-14000:(-1)/1000,1:14000/1000,14,14),y=-c(47,41.75,42-sqrt(14^2-c(-14000:(-1)/1000,1:14000/1000)^2),41.75,47)),aes(x=x,y=y), size = 0.75)+
      #short_range
      geom_path(data=data.frame(x=c(-4,-4,-4000:(-1)/1000,1:4000/1000,4,4),y=-c(47,41.75,41.75-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2),41.75,47)),aes(x=x,y=y), size = 0.75)+
      #theme
      theme_classic()
    
  })
  
  output$table2 = renderTable(reactive_data() %>%
                                select(Team, contains('Rank')))
  
}


# Run the application 
shinyApp(ui = ui, server = server)
