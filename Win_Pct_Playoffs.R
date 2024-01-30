library(Lahman)
library(tidyverse)
library(dplyr)

teams <- Lahman::Teams

colnames(teams)

table(teams$WSWin)


##Replace NA with 0 for the DivWinner and for WC Win

teams$DivWin[is.na(teams$DivWin)] = 0
teams$WCWin[is.na(teams$WCWin)] = 0
teams$LgWin[is.na(teams$LgWin)] = 0




output <- teams %>% filter(yearID >1919, yearID !=1981 , yearID != 1994) %>% 
          mutate(Win_Pct = W/(W+L),
                 LGWinner = as.numeric(LgWin == "Y"),
                 DivWinner = as.numeric(DivWin=="Y"),
                 WCWinner = as.numeric(WCWin == "Y"),
                 PlayoffApp = as.numeric(LGWinner == 1 | DivWinner == 1 | WCWinner == 1),
                 Pct_Bins = cut(Win_Pct,breaks = seq(from = 0, to = 1, by = .025)),
                 Playoff_Era = cut(yearID, breaks = c(1919,1968,1993,2011,2019), labels = c("One Round","Two Round", "WC Era","Expanded WC")))%>%
          group_by(Pct_Bins, Playoff_Era) %>% 
          summarise(Num_Times = length(Win_Pct),
                    MidPoint =median(Win_Pct),
                    Num_Div_Winners = sum(DivWinner), 
                    DivWinPct = Num_Div_Winners/Num_Times,
                    Num_Lg_Winners = sum(LGWinner),
                    LGWinPct = Num_Lg_Winners/Num_Times,
                    Num_WC_Winners = sum(WCWinner),
                    WCWinPct = Num_WC_Winners/Num_Times,
                    Num_Playoff_Apps = sum(PlayoffApp),
                    PlayoffPct = Num_Playoff_Apps/Num_Times
                    )

output %>% ggplot(aes(x=MidPoint, y = LGWinPct))+geom_line()+facet_wrap(~Playoff_Era)+xlab("Winning Percentage")
output %>% ggplot(aes(x=MidPoint, y = DivWinPct))+geom_line()+facet_wrap(~Playoff_Era)+xlab("Winning Percentage")
output %>% ggplot(aes(x=MidPoint, y = WCWinPct))+geom_line()+facet_wrap(~Playoff_Era)+xlab("Winning Percentage")
output %>% ggplot(aes(x=MidPoint, y = PlayoffPct))+geom_line()+facet_wrap(~Playoff_Era)+xlab("Winning Percentage")



output %>% ggplot(aes(x=MidPoint, y = PlayoffPct, color = Playoff_Era))+geom_line()+xlab("Winning Percentage")+
  geom_hline(yintercept = .5, color = "red", size = .1, lty = 3)+geom_hline(yintercept = .9, size = .1, lty=3, color = "red")+
  geom_vline(xintercept = 92/162, color = "blue", size = .1, lty=3)+
  theme(legend.position = "bottom")

output %>% ggplot(aes(x=MidPoint, y = DivWinPct, color = Playoff_Era))+geom_line()+xlab("Winning Percentage")+
  geom_hline(yintercept = .5, color = "red", size = .1, lty = 3)+geom_hline(yintercept = .9, size = .1, lty=3, color = "red")+
  geom_vline(xintercept = 92/162, color = "blue", size = .1, lty=3)+
  theme(legend.position = "bottom")

output %>% ggplot(aes(x=MidPoint, y = LGWinPct, color = Playoff_Era))+geom_line()+xlab("Winning Percentage")+
  geom_hline(yintercept = .5, color = "red", size = .1, lty = 3)+geom_hline(yintercept = .9, size = .1, lty=3, color = "red")+
  geom_vline(xintercept = 92/162, color = "blue", size = .1, lty=3)+
  theme(legend.position = "bottom")

output %>% ggplot(aes(x=MidPoint, y = WCWinPct, color = Playoff_Era))+geom_line()+xlab("Winning Percentage")+
  geom_hline(yintercept = .5, color = "red", size = .1, lty = 3)+geom_hline(yintercept = .9, size = .1, lty=3, color = "red")+
  geom_vline(xintercept = 92/162, color = "blue", size = .1, lty=3)+
  theme(legend.position = "bottom")

