Medal_Summary <- function(df){
  ## Create a function that takes grouped data, and finds the 
  ## totals for each medal winner, participants
  return(
    df %>% summarize(gold = sum(gold), 
                     silver = sum(silver), 
                     bronze = sum(bronze), 
                     no_medal = sum(no_medal)) %>% 
      mutate(participants = gold + silver + bronze + no_medal) 
  )
}

World_Map <- function(df, z){
  ## Create an interactive plot that 
  ## shows different metrics 
  ## given certain conditions
  return(
    plot_ly(medal_count, 
            type = 'choropleth', 
            locations = medal_count$NOC, 
            z = medal_count[[z]], 
            text = medal_count$region, 
            colorscale = 'Blues') 
  )
  
}
