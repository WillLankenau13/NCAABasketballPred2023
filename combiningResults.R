library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")




#temp = list.files(pattern="*.csv")
#myfiles = lapply(temp, read_csv)


#Load Files
m_tournament_detailed <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MNCAATourneyDetailedResults.csv") %>% 
  mutate(type = "n") 
m_reg_conf_detailed <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MRegularSeasonDetailedResults.csv")
m_conference_helper <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MConferenceTourneyGames.csv") %>% 
  mutate(type = "c")
w_reg_conf_detailed <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/WRegularSeasonDetailedResults.csv") %>% 
  mutate(type = "rc") %>% 
  mutate(ConfAbbrev = NA)
w_tournament_detailed <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/WNCAATourneyDetailedResults.csv") %>% 
  mutate(type = "n") %>% 
  mutate(ConfAbbrev = NA)



################
#Getting type of game
m_reg_conf_detailed <- m_reg_conf_detailed %>% 
  full_join(m_conference_helper, by = c("Season", "DayNum", "WTeamID", "LTeamID"))
m_regular_detailed <- m_reg_conf_detailed %>% 
  filter(is.na(type)) %>% 
  mutate(type = "r")
m_conference_detailed <- m_reg_conf_detailed %>% 
  filter(type == "c") %>% 
  filter(!is.na(WScore))
m_tournament_detailed <- m_tournament_detailed %>% 
  mutate(ConfAbbrev = NA)



regular_detailed <- rbind(w_reg_conf_detailed, m_regular_detailed)
write_csv(regular_detailed, "~/R Stuff/NCAABasketballPred2023/Data/myCreated/regular_detailed.csv")

#combined the datasets
results <- rbind(m_tournament_detailed, m_conference_detailed, m_regular_detailed, w_reg_conf_detailed, w_tournament_detailed)


results <- results %>% 
  select(Season, DayNum, WTeamID, WScore, LTeamID, LScore, WLoc, NumOT, type) %>% 
  mutate(Team_A = ifelse(WTeamID < LTeamID, WTeamID, LTeamID),
         Team_B = ifelse(WTeamID < LTeamID, LTeamID, WTeamID),
         Score_A = ifelse(WTeamID < LTeamID, WScore, LScore),
         Score_B = ifelse(WTeamID < LTeamID, LScore, WScore)) %>% 
  mutate(result = ifelse(Score_A > Score_B, 1, 0)) %>% 
  mutate(Loc_A = ifelse(WLoc == "N", "N", ifelse(result == 1, WLoc, ifelse(WLoc == "H", "A", "H"))))

#Select only Some columns
results <- results %>% 
  select(Season, DayNum, Loc_A, type, NumOT, Team_A, Team_B, Score_A, Score_B, result) %>% 
  filter(Score_A != Score_B)

#write csv files
write_csv(results, "~/R Stuff/NCAABasketballPred2023/Data/myCreated/results.csv")



