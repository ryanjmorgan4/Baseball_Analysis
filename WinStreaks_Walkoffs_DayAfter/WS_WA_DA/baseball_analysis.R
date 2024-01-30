library(baseballr)
library(tidyverse)
library(Lahman)



mk_full_team_results <- function(YearTeam_wanted_List){
  numberscrapes <- length(YearTeam_wanted_List[,1])
  
  returnDB <- team_results_bref(YearTeam_wanted_List[1,2], YearTeam_wanted_List[1,1])
  
  for(i in 2:numberscrapes){
    added <- team_results_bref(YearTeam_wanted_List[i,2], YearTeam_wanted_List[i,1])
    
    returnDB <- bind_rows(returnDB,added)
    
  }
  
  return(returnDB)
  
}



get_season_summaries <- function(gameDF){

  
  
  
returnDF <- gameDF %>% mutate(Result_Fix = substr(x = Result, start = 1, stop =1)) %>% filter(Result_Fix %in% c("W","L","T")) %>%group_by(Tm,Year,H_A) %>% summarise(WalkOffWins = sum(Result == "W-wo"), NumWins = sum(Result_Fix =="W"), 
                                                           LongestWinStreak = max(Streak), LongestLosingStreak = abs(min(Streak)),
                                                           HomeGames = sum(H_A == "H" & !is.na(R)), NumGames= sum(!is.na(R))) %>% 
  mutate(TotalWins = sum(NumWins), LongestWinStreak = max(LongestWinStreak), LongestLosingStreak = max(LongestLosingStreak),
         HomeWins = NumWins, NumGames = sum(NumGames), WinPct = TotalWins/NumGames) %>%filter(H_A == "H") %>%select(-c(H_A, NumWins))
  
return(returnDF)
}

createNextGameSummaries <- function(gameDF){
  
  nextGameSummary <- gameDF %>% mutate(Result_Fix = substr(x = Result, start = 1, stop =1),
                                                           Result_Fix = gsub(pattern = "W", replacement = "Win", x=Result_Fix),
                                                           Result_Fix = gsub(pattern = "L", replacement = "Loss", x = Result_Fix))

  nextGameSummary$Prev_Game = c(NA,nextGameSummary$Result_Fix[1:(length(nextGameSummary$Result_Fix)-1)])
  
  nextGameSummary$Prev_Game[nextGameSummary$Gm == 1] = NA
  
  
  Test <- nextGameSummary %>%filter(Prev_Game  %in% c("Win","Loss","T")) %>%
    select(Tm, Year, Prev_Game, Result_Fix) %>% group_by(Tm, Year, Prev_Game) %>%
    
    summarise(Wins = sum(Result_Fix == "Win"),
              Losses = sum(Result_Fix == "Loss"),
              Ties = sum(Result_Fix == "T"),
              Games = Wins+Losses+Ties,
              Winning_Pct = Wins/Games )
  
  
  afterLosses <- Test %>% filter(Prev_Game == "Loss") %>% mutate(AfterLoss_Wins = Wins, AfterLoss_Losses = Losses, AfterLoss_Games = Games,
                                                                 AfterLoss_WinPct = Winning_Pct) %>% 
    select(Tm, Year, AfterLoss_Wins, AfterLoss_Losses, AfterLoss_Games, AfterLoss_WinPct)
  
  afterWins <-  Test %>% filter(Prev_Game == "Win") %>% mutate(AfterWin_Wins = Wins, AfterWin_Losses = Losses, AfterWin_Games = Games,
                                                               AfterWin_WinPct = Winning_Pct) %>% 
    select(Tm, Year, AfterWin_Wins, AfterWin_Losses, AfterWin_Games, AfterWin_WinPct)
  
  afterTie <-  Test %>% filter(Prev_Game == "T") %>% mutate(AfterTie_Wins = Wins, AfterTie_Losses = Losses, AfterTie_Games = Games,
                                                               AfterTie_WinPct = Winning_Pct) %>% 
    select(Tm, Year, AfterTie_Wins, AfterTie_Losses, AfterTie_Games, AfterTie_WinPct)
  
  nextGameSummary <- right_join(afterWins, afterLosses, by = c("Tm", "Year"))
  
  nextGameSummary <- left_join(nextGameSummary, afterTie, by = c("Tm","Year"))
  
  
  return(nextGameSummary)
  
}

createFirstStreakOccurences <- function(gameDF){
  
  Streaks <- gameDF %>%mutate(Gm = as.numeric(Gm)) %>%group_by(Tm, Year, Streak) %>% summarise(FirstOccurence = min(Gm))
  
  first1w <- Streaks %>% filter(Streak == 1) %>% mutate(First_1W_Streak = FirstOccurence)%>% select(Tm, Year, First_1W_Streak) 
  first2w <- Streaks %>% filter(Streak == 2) %>% mutate(First_2W_Streak = FirstOccurence)%>% select(Tm, Year, First_2W_Streak) 
  first3w <- Streaks %>% filter(Streak == 3) %>% mutate(First_3W_Streak = FirstOccurence)%>% select(Tm, Year, First_3W_Streak) 
  first4w <- Streaks %>% filter(Streak == 4) %>% mutate(First_4W_Streak = FirstOccurence)%>% select(Tm, Year, First_4W_Streak)
  first5w <- Streaks %>% filter(Streak == 5) %>% mutate(First_5W_Streak = FirstOccurence)%>% select(Tm, Year, First_5W_Streak) 
  first6w <- Streaks %>% filter(Streak == 6) %>% mutate(First_6W_Streak = FirstOccurence)%>% select(Tm, Year, First_6W_Streak) 
  first7w <- Streaks %>% filter(Streak == 7) %>% mutate(First_7W_Streak = FirstOccurence)%>% select(Tm, Year, First_7W_Streak)
  first1L <- Streaks %>% filter(Streak == -1) %>% mutate(First_1L_Streak = FirstOccurence)%>% select(Tm, Year, First_1L_Streak) 
  first2L <- Streaks %>% filter(Streak == -2) %>% mutate(First_2L_Streak = FirstOccurence)%>% select(Tm, Year, First_2L_Streak) 
  first3L <- Streaks %>% filter(Streak == -3) %>% mutate(First_3L_Streak = FirstOccurence)%>% select(Tm, Year, First_3L_Streak) 
  first4L <- Streaks %>% filter(Streak == -4) %>% mutate(First_4L_Streak = FirstOccurence)%>% select(Tm, Year, First_4L_Streak) 
  first5L <- Streaks %>% filter(Streak == -5) %>% mutate(First_5L_Streak = FirstOccurence)%>% select(Tm, Year, First_5L_Streak) 
  first6L <- Streaks %>% filter(Streak == -6) %>% mutate(First_6L_Streak = FirstOccurence)%>% select(Tm, Year, First_6L_Streak) 
  first7L <- Streaks %>% filter(Streak == -7) %>% mutate(First_7L_Streak = FirstOccurence)%>% select(Tm, Year, First_7L_Streak) 
  
  
  OutputFile <- left_join(first1w, first2w, by = c("Tm", "Year"))
  OutputFile <- left_join(OutputFile, first3w, by = c("Tm", "Year"))
  OutputFile <- left_join(OutputFile, first4w, by = c("Tm", "Year"))
  OutputFile <- left_join(OutputFile, first5w, by = c("Tm", "Year"))
  OutputFile <- left_join(OutputFile, first6w, by = c("Tm", "Year"))
  OutputFile <- left_join(OutputFile, first7w, by = c("Tm", "Year"))
  OutputFile <- left_join(OutputFile, first1L, by = c("Tm", "Year"))
  OutputFile <- left_join(OutputFile, first2L, by = c("Tm", "Year"))
  OutputFile <- left_join(OutputFile, first3L, by = c("Tm", "Year"))
  OutputFile <- left_join(OutputFile, first4L, by = c("Tm", "Year"))
  OutputFile <- left_join(OutputFile, first5L, by = c("Tm", "Year"))
  OutputFile <- left_join(OutputFile, first6L, by = c("Tm", "Year"))
  OutputFile <- left_join(OutputFile, first7L, by = c("Tm", "Year"))
  
  return(OutputFile)
  
  
  
}

merge_Together <- function(gameDF){
  seasonSummaries <- get_season_summaries(gameDF)
  nextGameSummaries <- createNextGameSummaries(gameDF)
  streaksSummaries <- createFirstStreakOccurences(gameDF)
  
  outputfile <- left_join(seasonSummaries, nextGameSummaries, by = c("Tm", "Year"))
  outputfile <- left_join(outputfile, streaksSummaries, by = c("Tm","Year"))
  
  return(outputfile)
  
  
  
}



#Through 2018:
YearTeamList <- Teams %>% select(yearID, teamIDBR)
Full_Game_Results_2018 <- mk_full_team_results(YearTeamList)
Through2018_Summaries <- merge_Together(Full_Game_Results_2018)

##2019:
yearTeam2019 <- YearTeamList %>% filter(yearID == 2018) %>% mutate(yearID = 2019)
Full_Game_Results_2019 <- mk_full_team_results(yearTeam2019)
Summaries_2019 <- merge_Together(Full_Game_Results_2019)


appended_Summaries <- bind_rows(Through2018_Summaries, Summaries_2019)


write.csv(x = Through2018_Summaries, file = "Through2018.csv")
write.csv(x = Summaries_2019, file = "Summaries2019.csv")
write.csv(x=appended_Summaries, file = "AppendedSummaries.csv")

####Analysis:


(Summaries_2019 %>% arrange(-First_3L_Streak) %>% select(Tm, Year, First_3L_Streak))
(Summaries_2019 %>% arrange(-First_2L_Streak) %>% select(Tm, Year, First_2L_Streak))

view(Through2018_Summaries %>% arrange(-First_3L_Streak) %>% select(Tm, Year, First_3L_Streak))
view(Through2018_Summaries %>% arrange(-First_2L_Streak) %>% select(Tm, Year, First_2L_Streak))


Through2018_Summaries %>% filter(Year > 1999) %>%mutate(WalkOffPct = WalkOffWins/HomeWins) %>%ggplot(aes(x=HomeWins, y = WalkOffPct))+geom_point()+geom_smooth()

Summaries_2019 %>% filter(Year > 1999) %>%mutate(WalkOffPct = WalkOffWins/HomeWins) %>%ggplot(aes(x=HomeWins, y = WalkOffPct))+geom_point()+geom_smooth()


Through2018_Summaries %>% filter(Year > 1999) %>% ggplot(aes(x=First_4W_Streak))+geom_histogram()

Through2018_Summaries %>% filter(Year > 1999) %>% ggplot(aes(x=First_3L_Streak))+geom_histogram()

Through2018_Summaries %>% filter(Year > 1999) %>% ggplot(aes(x=First_4L_Streak))+geom_histogram()


Through2018_Summaries %>% filter(Year > 1999) %>% ggplot(aes(x=AfterLoss_WinPct, y = AfterWin_WinPct))+geom_point()+geom_abline(slope = 1, intercept = 0)

Summaries_2019 %>% filter(Year > 1999) %>% ggplot(aes(x=AfterLoss_WinPct, y = AfterWin_WinPct))+geom_point()+geom_abline(slope = 1, intercept= 0)


appended_Summaries %>%filter(Year > 1899) %>% mutate(CurrentTeam = Year == 2019) %>% ggplot(aes(x=AfterLoss_WinPct, y = AfterWin_WinPct, color = CurrentTeam))+
  geom_point()+xlim(.15,.85)+ylim(.15,.85)+geom_abline(slope=1, intercept = 0, color = "blue", lty = 2, size = 1)

appended_Summaries %>%filter(Year > 1899) %>% mutate(CurrentTeam = Year == 2019) %>% ggplot(aes(x=AfterLoss_WinPct))+
  geom_histogram(binwidth = .01)+geom_vline(xintercept = .5, color = "red", lty = 2, size = 1)

appended_Summaries %>%filter(Year > 1899) %>% mutate(CurrentTeam = Year == 2019) %>% ggplot(aes(x=WinPct))+
  geom_histogram(binwidth = .01)+geom_vline(xintercept = .5, color = "red", lty = 2, size = 1)

appended_Summaries %>%filter(Year > 1899) %>% mutate(afterLoss_minus_Overall = AfterLoss_WinPct-WinPct) %>% ggplot(aes(x=afterLoss_minus_Overall))+
  geom_histogram(binwidth = .01)+geom_vline(xintercept = 0, color = "red", lty = 2, size = 1)

bestBounceBack <- appended_Summaries %>%filter(Year > 1899) %>% mutate(afterLoss_minus_Overall = AfterLoss_WinPct-WinPct, afterLoss_divided_Overall = AfterLoss_WinPct/WinPct) %>% arrange(-afterLoss_minus_Overall)%>%
  select(Tm, Year,NumGames , TotalWins, AfterLoss_WinPct, AfterWin_WinPct, WinPct, afterLoss_minus_Overall, afterLoss_divided_Overall, First_2L_Streak, First_3L_Streak, First_4L_Streak)

view(bestBounceBack)

### Sandbox:


# To do: Try and see if a team has every avoided a three game losing streak
# - See when the latest in a season an X game winning streak or X game losing streak occured
# - Look for extreme percentages of walk offs by home winning pct
# - Make some cool scatterplots to look at relationships
# - Make a histogram or something with gameofseason on the x axis and "occurence of first XXX game losign streak" on the y
# - Try and find a way to summarize a team's splits by previous day's games (wins or losses)
# - Look at historical data, see if current teams (like the Twins!) are an anomaly thus far on record after losses, 
# low number of walk offs, short losing streaks. 

# - Designated Winners: If a team can select a player before the game who automatically wins the game, if he hits a home run. 








