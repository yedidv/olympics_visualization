setwd("~/Desktop/msba/spring2021/applied_analytics/case_studies/olympics_visualization")

#### Read in Data and packages ######
library(tidyverse) 
library(plotly) 
library(shiny) 
source('funs.r') 

## Read in both csvs, and full join them together
olympics <- read_csv('case_1_data/OlympicHistoryAthletes.csv') %>% 
  full_join(read_csv('case_1_data/noc_regions.csv') ) %>% 
  mutate(Year = as.integer(Year)) 





##### Format the Data #### 

## In the medal category, the null values mean the player didn't win a medal
olympics <- olympics %>% 
  
  ## Replace null with no_medal to show they 
  ## didn't win anything
  mutate(Medal = replace_na(Medal, 'no_medal')) %>% 
  
  ## Order by year
  arrange(Year) 

## Create Dummy variables for the medals 
olympics <- olympics %>% 
  
  ## Gold, silver, bronze, and no medals are the dummy variables
  mutate(gold = ifelse(Medal == 'Gold', 1, 0) ) %>% 
  mutate(silver = ifelse(Medal == 'Silver', 1, 0) ) %>% 
  mutate(no_medal = ifelse(Medal == 'no_medal', 1, 0) ) %>% 
  mutate(bronze = ifelse(Medal == 'Bronze', 1, 0) ) %>% 
  
  ## Remove the one null year variable 
  filter(!is.na(Year)) %>%
  
  ## Singapore is the NOC for a bunch of random region names. 
  ## So it can be fixed here
  mutate(region = ifelse(NOC == 'SGP', 'Singapore', region)) %>% 
  
  ## We only select the columns we need
  select(NOC,region, Year, Season, Sex, gold, silver, bronze, no_medal, Medal)  





#### Shiny App #### 


ui <- fluidPage(
  
  titlePanel('Olympic Country Participation/Awards'), 
  
  sidebarPanel(
    ## Check the Year we are looking at 
  sliderInput("Year", "Range", value = c(1900, 1940), 
              min = min(olympics$Year) , 
              max = max(olympics$Year), 
              sep = '') , 
  
  ## Male vs Female or both 
  checkboxGroupInput('Sex', 'Subset Sexes', 
                     choiceNames = c('Male', 'Female'), 
                     choiceValues = c('M', 'F'), 
                     selected = c('M', 'F')
  ), 
  
  ## Which Awards do we want to look at 
  checkboxGroupInput('Medal', 'Subset Medals', 
                     choiceNames = c('Gold', 'Silver', 'Bronze', 'No Medal'), 
                     choiceValues = c('Gold', 'Silver', 'Bronze', 'no_medal'), 
                     selected = c('Gold', 'Silver', 'Bronze', 'no_medal'))
  , 
  
  checkboxGroupInput('Season', 'Subset Seasons', 
                     choiceNames = c('Summer', 'Winter'), 
                     choiceValues = c('Summer', 'Winter'), 
                     selected = c("Summer", 'Winter'))), 
  
  
  mainPanel(
    plotlyOutput('plot', width = '1400px', height = '1400px')   
  )
)




server <- function(input, output, server){
  
  output$plot <- renderPlotly({
    df <- olympics %>% 

      filter(Year >= input$Year[1] ) %>% 
      filter(Year <= input$Year[2]) %>% 
      filter(Sex %in% input$Sex) %>% 
      filter(Medal %in% input$Medal) %>% 
      filter(Season %in% input$Season) %>%
      group_by(NOC, region) %>% 
      
      summarize(gold = sum(gold), 
                silver = sum(silver), 
                bronze = sum(bronze), 
                no_medal = sum(no_medal)) %>% 
      
      mutate(participants = gold + silver + bronze + no_medal) 
    
    plot_ly(df, 
            type = 'choropleth', 
            locations = df$NOC, 
            z = df$participants, 
            text = df$participants, 
            colorscale = 'Blues', reversescale = T) %>% 
      layout(autosize = F, width =800, height = 500) 
    
    
    
  })
  
}


shinyApp(ui = ui, server = server) 



