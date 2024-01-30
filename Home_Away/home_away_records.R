library(baseballr)
library(tidyverse)
library(Lahman)


YearTeamList <- Teams %>% select(yearID, teamIDBR)
yearTeam2019 <- YearTeamList %>% filter(yearID == 2018) %>% mutate(yearID = 2019)




mk_full_team_results <- function(YearTeam_wanted_List){
  numberscrapes <- length(YearTeam_wanted_List[,1])
  
  returnDB <- team_results_bref(YearTeam_wanted_List[1,2], YearTeam_wanted_List[1,1])
  
  for(i in 2:numberscrapes){
    added <- team_results_bref(YearTeam_wanted_List[i,2], YearTeam_wanted_List[i,1])
    
    returnDB <- bind_rows(returnDB,added)
    
  }
  
  return(returnDB)
  
}



##Filter down to just completed seasons:

Full_Game_Results_2018 <- mk_full_team_results(YearTeamList)




## Create 2019 datasets. These small programs can be run periodically to be updated throughout to compare to history:

Full_Game_Results_2019 <- mk_full_team_results(yearTeam2019)




##Test things out just using 2019, initially


Full_Game_Results_2019$Fixed_Result = substr(Full_Game_Results_2019$Result, start = 1, stop =1)

all_2019 <- Full_Game_Results_2019%>%group_by(Year, Tm, H_A) %>% 
  summarise(Num_Games = sum(Fixed_Result %in% c("L","T","W")), Wins = sum(Fixed_Result == "W"))

all_2019_h <- all_2019 %>% filter(H_A == "H") %>% 
  mutate(Home_win_pct = Wins/Num_Games, Home_Games = Num_Games, Home_Wins = Wins)%>% 
  select(-c(H_A, Num_Games, Wins))


all_2019_a <- all_2019 %>% filter(H_A == "A") %>% 
  mutate(Away_win_pct = Wins/Num_Games, Away_Games = Num_Games, Away_Wins = Wins)%>% 
  select(-c(H_A, Num_Games, Wins))

full_summary_2019 <- right_join(x = all_2019_h, y = all_2019_a, by =c("Year", "Tm"))

##Now, redo it using 2018 dataset


Full_Game_Results_2018$Fixed_Result = substr(Full_Game_Results_2018$Result, start = 1, stop =1)

all_2018 <- Full_Game_Results_2018%>%group_by(Year, Tm, H_A) %>% 
  summarise(Num_Games = sum(Fixed_Result %in% c("L","T","W")), Wins = sum(Fixed_Result == "W"))

all_2018_h <- all_2018 %>% filter(H_A == "H") %>% 
  mutate(Home_win_pct = Wins/Num_Games, Home_Games = Num_Games, Home_Wins = Wins)%>% 
  select(-c(H_A, Num_Games, Wins))


all_2018_a <- all_2018 %>% filter(H_A == "A") %>% 
  mutate(Away_win_pct = Wins/Num_Games, Away_Games = Num_Games, Away_Wins = Wins)%>% 
  select(-c(H_A, Num_Games, Wins))

full_summary_2018 <- right_join(x = all_2018_h, y = all_2018_a, by =c("Year", "Tm"))


##Now, bind rows for the 2019 and 2018 summaries

complete_summary <- bind_rows(full_summary_2018, full_summary_2019)


## So now, just how bad have the Cubs been?

write.csv(complete_summary, file = "home_away_splits_8_15.csv")


complete_summary %>% filter(Year > 1899)  %>% mutate(Win_Pct_Diff = Home_win_pct - Away_win_pct) %>% 
  arrange(-Win_Pct_Diff)

complete_summary %>% filter(Year > 1899)  %>% mutate(Win_Pct_Diff = Home_win_pct - Away_win_pct, YearGroup = (Year ==2019)) %>% 
  ggplot(aes(x=Home_win_pct, y = Away_win_pct, color = YearGroup))+geom_point()+
  geom_abline(slope=1, intercept=0, color = "red")+xlim(c(.15,.85))+ylim(c(.15,.85))+scale_color_manual(values = c("grey","blue"))

complete_summary %>% filter(Year > 1899)  %>% mutate(Win_Pct_Diff = Home_win_pct - Away_win_pct, YearGroup = (Year ==2019)) %>% 
  ggplot(aes(x=Win_Pct_Diff))+geom_histogram()+geom_vline(xintercept = 0, color = "red")



complete_summary %>% filter(Tm == "CHC", Year >1899)  %>% mutate(Win_Pct_Diff = Home_win_pct - Away_win_pct, YearGroup = (Year ==2019)) %>% 
  ggplot(aes(x=Home_win_pct, y = Away_win_pct, color = YearGroup))+geom_point()+
  geom_abline(slope=1, intercept=0, color = "red")+xlim(c(.15,.85))+ylim(c(.15,.85))+scale_color_manual(values = c("grey","blue"))

complete_summary %>% filter(Tm == "CHC", Year > 1899)  %>% mutate(Win_Pct_Diff = Home_win_pct - Away_win_pct, YearGroup = (Year ==2019)) %>% 
  ggplot(aes(x=Win_Pct_Diff))+geom_histogram()+geom_vline(xintercept = 0, color = "red")


