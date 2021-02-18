setwd("~/Desktop/msba/spring2021/applied_analytics/case_studies/olympics_visualization")

#### Read in Data and packages ######
library(tidyverse) 
library(plotly) 
library(shiny) 
library(reshape2) 
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
  mutate(region = ifelse(NOC == 'SGP', 'Singapore', region))

#### Number of Events By Season By Year #### 
seasons <- olympics %>% select(Year, Season) %>% 
  mutate(winter = ifelse(Season == 'Winter', 1, 0)) %>% 
  mutate(summer = ifelse(Season == 'Summer', 1, 0)) %>% 
  group_by(Year) %>% summarize(summer = sum(summer), winter = sum(winter)) 

## Stacked Bar Chart of sports by season for each year
plot_ly(seasons, x = ~Year, y = ~summer, type = 'bar', name = 'Summer Games Count') %>% 
  add_trace(y = ~winter, name = 'Winter Games Count')  %>%
  layout( 
         title = "Number of Participants By Year And Season", xaxis =list(title = 'Year'), yaxis = list(title = 'Numer of Participants')) 
  


