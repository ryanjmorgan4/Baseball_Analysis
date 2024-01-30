library(baseballr)
library(tidyverse)
library(Lahman)


batters_2019 <- read.csv("C:\\Users\\Ryan\\Desktop\\Baseball_Analysis\\GamesWithHit\\2019_batters.csv")


batter_game_logs_fg(playerid = batters_2019$playerid[1], year = 2019)



gamelog_singleyear_multplayer<- function (playerdf, year){
  return_df <- try(batter_game_logs_fg(playerdf$playerid[1], year))
  return_df$playerid = playerdf$playerid[1]
  return_df$playername = playerdf[1,1]
  return_df$Season = year
  
  
  num_players = length(playerdf$playerid)
  
  
  for(i in 2:num_players){
    new_df <- try(batter_game_logs_fg(playerdf$playerid[i], year))
    new_df$playerid = playerdf$playerid[i]
    new_df$playername = playerdf[i,1]
    new_df$Season = year
    
    
    return_df <- bind_rows(return_df, new_df)
    
    Sys.sleep(2)
    
    
  }
  
  return_df <- return_df[(!is.na(return_df$Date)),]
  
  return_df <- return_df[,1:26]
  
  return(return_df)
  
}


fangraphs_scrape_multseasons <- function(playerdf, yearlist){
  numyears = length(yearlist)
  
  return_df <- gamelog_singleyear_multplayer(playerdf, yearlist[1])
  
  for (i in 2:numyears){
    new_df <- gamelog_singleyear_multplayer(playerdf, yearlist[i])
    
    return_df <- bind_rows(return_df, new_df)
    
  }
  
  return(return_df)
  
  
}





gamelog_scrape_mult<- function (playerdf){
  return_df <- try(batter_game_logs_fg(playerdf$playerid[1], playerdf$Season[1]))
  return_df$playerid = playerdf$playerid[1]
  return_df$playername = playerdf[1,2]
  return_df$Season = playerdf$Season[1]
  
  
  num_records = length(playerdf$playerid)
  
  
  for(i in 2:num_records){
    new_df <- try(batter_game_logs_fg(playerdf$playerid[i], playerdf$Season[i]))
    new_df$playerid = playerdf$playerid[i]
    new_df$playername = playerdf[i,2]
    new_df$Season = playerdf$Season[i]
    
    
    return_df <- bind_rows(return_df, new_df)
    
    Sys.sleep(2)
    
    
  }
  
  return_df <- return_df[(!is.na(return_df$Date)),]
  
  return_df <- return_df[,1:26]
  
  return(return_df)
  
}


Full_Batter_Records <- read.csv("C:\\Users\\Ryan\\Desktop\\Baseball_Analysis\\GamesWithHit\\All_Batter_Season_Records.csv")

colnames(Full_Batter_Records)[1] = "Season"
colnames(Full_Batter_Records)[23] = "playerid"

batter_records_min50_2000_2009 <- read.csv("C:\\Users\\Ryan\\Desktop\\Baseball_Analysis\\GamesWithHit\\2000_2019_min50.csv")

colnames(batter_records_min50_2000_2009)[1] = "Season"
colnames(batter_records_min50_2000_2009)[23] = "playerid"

gamelogs_min50_2000_2009 <- gamelog_scrape_mult(batter_records_min50_2000_2009)



Output2 <- gamelogs_min50_2000_2009 %>%mutate(got_hit = (H > 0)) %>%group_by(playername, playerid ,Season) %>% summarise(Num_Games = length(Date), Games_W_Hit = sum(got_hit))%>%
  arrange(-Games_W_Hit) %>% mutate(Percent_Games_W_Hit = Games_W_Hit/Num_Games) %>% select(-playerid) 


Output <- gamelogs_2019 %>%mutate(got_hit = (H > 0)) %>%group_by(playername, Season) %>% summarise(Num_Games = length(Date), Games_W_Hit = sum(got_hit))%>%
  arrange(-Games_W_Hit) %>% mutate(Percent_Games_W_Hit = Games_W_Hit/Num_Games) 


output2 <- gamelogs_2019 %>% mutate(HR = as.numeric(HR), H= as.numeric(H)) %>% group_by (playername, Season) %>% summarise(num_Hr = sum(HR), num_Hits = sum(H), HR_Pct = num_Hr/num_Hits) %>% 
  filter(num_Hits > 19)%>%arrange(-HR_Pct)


output2 %>% ggplot(aes(x=HR_Pct))+geom_histogram()

output2 %>% ggplot(aes(y=num_Hr, x=HR_Pct))+geom_point()


cor.test(output2$num_Hr, output2$HR_Pct)

cor.test(output2$num_Hr, output2$num_Hits)

