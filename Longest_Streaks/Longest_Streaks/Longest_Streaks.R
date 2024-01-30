#install.packages("devtools")

library(devtools)

devtools::install_github("BillPetti/baseballr")
#install.packages("Lahman")

library(baseballr)
library(tidyverse)
library(Lahman)


team_results_bref_ryan <-function(Tm, year) {
  
  Tm <- as.character(Tm)
  
  year <- as.character(year)
  
  url <- paste0("https://www.baseball-reference.com/teams/", Tm, "/", year, "-schedule-scores.shtml")
  
  data <- read_html(url) %>%
    html_nodes("table")
  
  data <- data[[length(data)]] %>%
    html_table() %>%
    .[-3]
  
  col_names <- c('Gm','Date','Tm','H_A','Opp','Result','R','RA','Inn','Record','Rank',
                 'GB','Win','Loss','Save','Time','D/N','Attendance', 'cLI', 'Streak', 'Orig_Scheduled')
  
  names(data) <- col_names
  
  data <- data[data$Gm != "Gm#",]
  
  data$H_A <- ifelse(grepl("@", data$H_A, fixed = TRUE), "A", "H")
  
  data$Attendance <- as.numeric(gsub(",", "", data$Attendance))
  
  data$Streak <- ifelse(grepl("-", data$Streak, fixed = TRUE), nchar(data$Streak) * -1, nchar(data$Streak) * 1)
  
  
  
  
  data$Year <- year
  data <- data[, 1:ncol(data)]
  data <- data %>%
    dplyr::filter(!grepl("Gm#", Gm))
  
  return(data)
}

mk_full_team_results <- function(YearTeam_wanted_List){
  numberscrapes <- length(YearTeam_wanted_List[,1])
  
  returnDB <- team_results_bref_ryan(YearTeam_wanted_List[1,1], YearTeam_wanted_List[1,2])
  
  for(i in 2:numberscrapes){
    added <- team_results_bref_ryan(YearTeam_wanted_List[i,1], YearTeam_wanted_List[i,2])
    
    returnDB <- bind_rows(returnDB,added)
    
  }
  
  return(returnDB)
  
}





#Through 2020:
YearTeamList <- Teams %>% select(teamIDBR, yearID)
Full_Game_Results <- mk_full_team_results(YearTeamList)

#Select the teams longest win and longest losing streak in each season

Longest_Streaks <- Full_Game_Results %>% mutate(Year = as.integer(Year)) %>%
  group_by(Year, Tm) %>% summarise(Longest_Win_Streak = max(Streak), Longest_Losing_Streak = abs(min(Streak)))

#Select How the team did each season:

Season_Results <- Teams %>% mutate(Year = yearID, Tm = teamIDBR, Win_Pct = W/G, WinningRecord = Win_Pct >= .5, DivRank = Rank) %>% 
  select(Year, Tm, G, W, L, Win_Pct, WinningRecord , DivRank, DivWin, WCWin, LgWin, WSWin)



# Select how teams did in the postseason

SeriesPost_winners <-
SeriesPost %>% select(yearID, round, teamIDwinner) %>% mutate(Year = yearID, Tm = teamIDwinner , MadePostSeason = 1, PostSeason_Result = paste("Won",round))%>%
  select(-c(yearID, round, teamIDwinner))


SeriesPost_losers <-
  SeriesPost %>% select(yearID, round, teamIDloser) %>% mutate(Year = yearID, Tm = teamIDloser , MadePostSeason = 1, PostSeason_Result = paste("Lost",round))%>%
  select(-c(yearID, round, teamIDloser))


SeriesResults <- bind_rows(SeriesPost_winners, SeriesPost_losers) 

SeriesResults$PostSeason_Result = gsub("[1-9]", "", SeriesResults$PostSeason_Result)

SeriesResults <- SeriesResults  %>% mutate(Made_WC_Round = ifelse(PostSeason_Result %in% c("Won NLWC", "Won ALWC", "Lost NLWC", "Lost ALWC"), 1, 0),
                                                  Won_WC_Round = ifelse(PostSeason_Result %in% c("Won NLWC", "Won ALWC"), 1, 0),
                                                  Made_Division_Series = ifelse(PostSeason_Result %in% c("Won NLDS", "Won ALDS", "Won AEDIV", "Won AWDIV", "Won NEDIV", "Won NWDIV", "Lost NLDS", "Lost ALDS", "Lost AEDIV", "Lost AWDIV", "Lost NEDIV", "Lost NWDIV"), 1, 0),
                                                  Won_Division_Series = ifelse(PostSeason_Result %in% c("Won NLDS", "Won ALDS", "Won AEDIV", "Won AWDIV", "Won NEDIV", "Won NWDIV"), 1, 0),
                                                  Made_Championship_Series = ifelse(PostSeason_Result %in% c("Won NLCS", "Won ALCS", "Lost NLCS", "Lost ALCS"), 1, 0),
                                                  Won_Championship_Series = ifelse(PostSeason_Result %in% c("Won NLCS", "Won ALCS"), 1, 0),
                                                  Made_WS = ifelse(PostSeason_Result %in% c("Won WS", "Lost WS", "Won CS", "Lost CS"), 1, 0),
                                                  Won_WS = ifelse(PostSeason_Result %in% c("Won WS", "Won CS"), 1, 0)
                                                  ) %>%
  group_by(Year, Tm) %>% summarise(MadePostSeason = 1,
                                   Made_WC_Round = max(Made_WC_Round),
                                   Won_WC_Round = max(Won_WC_Round),
                                   Made_Division_Series = max(Made_Division_Series),
                                   Won_Division_Series = max(Won_Division_Series),
                                   Made_Championship_Series = max(Made_Championship_Series),
                                   Won_Championship_Series = max(Won_Championship_Series),
                                   Made_WS = max(Made_WS),
                                   Won_WS = max(Won_WS)
                                   )



#Make Consistent Team Names

SeriesResults$Tm = gsub("NYA", "NYY", SeriesResults$Tm)
SeriesResults$Tm = gsub("NYN", "NYM", SeriesResults$Tm)
SeriesResults$Tm = gsub("CHN", "CHC", SeriesResults$Tm)
SeriesResults$Tm = gsub("CHA", "CHW", SeriesResults$Tm)
SeriesResults$Tm = gsub("SLN", "STL", SeriesResults$Tm)
SeriesResults$Tm = gsub("LAN", "LAD", SeriesResults$Tm)
SeriesResults$Tm = gsub("SDN", "SDP", SeriesResults$Tm)
SeriesResults$Tm = gsub("SFN", "SFG", SeriesResults$Tm)
SeriesResults$Tm = gsub("KCA", "KCR", SeriesResults$Tm)
SeriesResults$Tm = gsub("TBA", "TBR", SeriesResults$Tm)
SeriesResults$Tm = gsub("WAS", "WSN", SeriesResults$Tm)
SeriesResults$Tm = gsub("FLO", "FLA", SeriesResults$Tm)
SeriesResults$Tm = gsub("ML4", "MIL", SeriesResults$Tm)
SeriesResults$Tm = gsub("ML1", "MLN", SeriesResults$Tm)
SeriesResults$Tm = gsub("NY1", "NYG", SeriesResults$Tm)
SeriesResults$Tm = gsub("NY4", "PRO", SeriesResults$Tm)
SeriesResults$Tm = gsub("SLA", "SLB", SeriesResults$Tm)
SeriesResults$Tm = gsub("WS1", "WSH", SeriesResults$Tm)
SeriesResults$Tm = gsub("SL4", "STL", SeriesResults$Tm)

#Merge Datasets together, only concerned with 1920 and on

Season_Results_1920 = Season_Results %>% filter(Year > 1919)
Longest_Streaks_1920 = Longest_Streaks %>% filter(Year > 1919)
SeriesResults_1920 = SeriesResults %>% filter(Year > 1919)


v1 <- left_join(Season_Results_1920, Longest_Streaks_1920)
Merged_Data <- left_join(v1, SeriesResults_1920)


#Clean Data to replace the NA with 0's for playoff results

Merged_Data$DivWin[is.na(Merged_Data$DivWin)] = "N"
Merged_Data$WCWin[is.na(Merged_Data$WCWin)] = "N"
Merged_Data$LgWin[is.na(Merged_Data$LgWin)] = "N"
Merged_Data$WSWin[is.na(Merged_Data$WSWin)] = "N"


Merged_Data$MadePostSeason[is.na(Merged_Data$MadePostSeason)] = 0
Merged_Data$Made_WC_Round[is.na(Merged_Data$Made_WC_Round)] = 0
Merged_Data$Won_WC_Round[is.na(Merged_Data$Won_WC_Round)] = 0
Merged_Data$Made_Division_Series[is.na(Merged_Data$Made_Division_Series)] = 0
Merged_Data$Won_Division_Series[is.na(Merged_Data$Won_Division_Series)] = 0
Merged_Data$Made_Championship_Series[is.na(Merged_Data$Made_Championship_Series)] = 0
Merged_Data$Won_Championship_Series[is.na(Merged_Data$Won_Championship_Series)] = 0
Merged_Data$Made_WS[is.na(Merged_Data$Made_WS)] = 0
Merged_Data$Won_WS[is.na(Merged_Data$Won_WS)] = 0


Longest_Losing_Streak_WS <- Merged_Data %>% group_by(Longest_Losing_Streak)%>% summarise(Occurences = n(),
                                                                                         WS_Wins = sum(Won_WS),
                                                                                         WS_Appearances = sum(Made_WS),
                                                                                         League_Wins = sum(Won_Championship_Series),
                                                                                         LCS_Appearances = sum(Made_Championship_Series),
                                                                                         Playoff_Appearances = sum(MadePostSeason),
                                                                                         Over_500 = sum(WinningRecord)) %>% 
  arrange(-Longest_Losing_Streak)



Longest_Winning_Streak_WS <- Merged_Data %>% group_by(Longest_Win_Streak)%>% summarise(Occurences = n(),
                                                                                         WS_Wins = sum(Won_WS),
                                                                                         WS_Appearances = sum(Made_WS),
                                                                                         League_Wins = sum(Won_Championship_Series),
                                                                                         LCS_Appearances = sum(Made_Championship_Series),
                                                                                         Playoff_Appearances = sum(MadePostSeason),
                                                                                         Over_500 = sum(WinningRecord)) %>% 
  arrange(-Longest_Win_Streak)


Longer_Than_11 = Merged_Data %>% filter(Longest_Losing_Streak > 10)


Longer_Than_11 %>% arrange(-Win_Pct)



Longest_Winning_Streak_WS %>% mutate(Playoff_Prob = Playoff_Appearances/Occurences) %>%
  ggplot(aes(x=Longest_Win_Streak, y = Playoff_Prob))+geom_line()

Longest_Losing_Streak_WS %>% mutate(Playoff_Prob = Playoff_Appearances/Occurences) %>%
  ggplot(aes(x=Longest_Losing_Streak, y = Playoff_Prob))+geom_line()

Longest_Streaks %>% filter(Year > 1919) %>% ggplot(aes(x=Longest_Win_Streak, y = Longest_Losing_Streak))+geom_point()


Merged_Data %>% filter(WinningRecord == 0) %>% arrange(-Longest_Win_Streak) %>% head()

Merged_Data %>% filter(WinningRecord == 1, Longest_Losing_Streak > 10) %>% arrange(-Longest_Losing_Streak)


Merged_Data %>% filter(WinningRecord == 1) %>% arrange(Longest_Win_Streak) %>% head()
