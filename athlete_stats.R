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
  mutate(region = ifelse(NOC == 'SGP', 'Singapore', region)) %>% drop_na() 



#### Create BMI #### 
bmi_df <- olympics %>% 
  mutate(bmi = Weight / (0.01 * Height) ^2 ) %>% 
  
  select(Sex, Age, Height, Weight, Games, NOC, Medal, 
         gold, silver, bronze, no_medal, bmi, Event) %>%
  
  mutate(low_bmi = ifelse(bmi <= 18.5, 1, 0)) %>% 
  mutate(medium_bmi = ifelse(bmi <= 25 & bmi > 18.5, 1, 0)) %>%
  mutate(high_bmi = ifelse(bmi <= 30 & bmi > 25, 1, 0)) %>% 
  mutate(obese = ifelse(bmi > 30, 1, 0))

event_groups <- bmi_df %>% 
  group_by(Sex, Event) %>%
  summarize(low_bmi = sum(low_bmi), 
            medium_bmi = sum(medium_bmi), 
            high_bmi = sum(high_bmi), 
            obese = sum(obese) ) %>%
  mutate(Male = ifelse(Sex == 'M', 1, 0)) %>%
  mutate(Female = ifelse(Sex == 'F', 1, 0))




event_bmi_sex <- plot_ly(event_groups %>% filter(Sex == 'F'), y = ~low_bmi, x = ~Event, 
                         type = 'bar') %>%
  add_trace(y = ~medium_bmi) %>% 
  add_trace(y = ~high_bmi) %>%
  add_trace(y = ~obese) %>%
  layout( barmode = 'stack', xaxis = (list(tickangle = 45)), 
    margin = list(b = 300, l = 50) # to fully display the x and y axis labels
  )

event_bmi_sex


