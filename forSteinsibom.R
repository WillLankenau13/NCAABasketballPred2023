library("ggplot2")
library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")
library("modelr")
library("leaps")



#Load files
log_model <- read_csv("~/R Stuff/NCAABasketballPred2023/Predictions/log_reg_modB.csv")
t_data <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/finalData/data2023.csv")
m_teams <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MTeams.csv") %>% 
  select(TeamID, TeamName)
w_teams <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/WTeams.csv")

#format
log_model$Team_A <- substr(log_model$ID, 6, 9)
log_model$Team_B <- substr(log_model$ID, 11, 14)

log_model <- log_model %>% 
  mutate(Team_A = as.numeric(Team_A),
         Team_B = as.numeric(Team_B))

#Combine teams
team_names <- rbind(m_teams, w_teams)

#Combine
games <- t_data %>% 
  left_join(log_model, by = c("Team_A", "Team_B")) %>% 
  left_join(team_names, by = c("Team_A" = "TeamID")) %>% 
  rename("Name_A" = "TeamName") %>% 
  left_join(team_names, by = c("Team_B" = "TeamID")) %>% 
  rename("Name_B" = "TeamName") %>% 
  arrange(Team_A, Team_B) %>% 
  filter(!is.na(Seed_A)) %>% 
  filter(!is.na(Seed_B))

#Split into mens and womens and select
m_games <- games %>% 
  filter(Team_A < 2000) %>% 
  select(Name_A, Name_B, pred)

w_games <- games %>% 
  filter(Team_A > 2000) %>% 
  select(Name_A, Name_B, pred)

#Pivot Wider
m_stein <- pivot_wider(m_games, names_from = "Name_B", values_from = "pred")
w_stein <- pivot_wider(w_games, names_from = "Name_B", values_from = "pred")

#Select
games <- games %>% 
  select(Team_A, Name_A, Seed_A, Team_B, Name_B, Seed_B, round, pred)

#Favorite and Underdog
round_1 <- full_pred %>% 
  filter(round == 1) %>% 
  select(Team_A, Name_A, Seed_A, Team_B, Name_B, Seed_B, round, pred) %>% 
  mutate(Favorite = ifelse(pred > 0.5, Name_A, Name_B),
         Underdog = ifelse(pred > 0.5, Name_B, Name_A),
         Probability = ifelse(pred < 0.5, 1 - pred, pred))

#Round 1
m_round_1 <- round_1 %>% 
  filter(Team_A < 2000) %>% 
  select(Favorite, Underdog, Probability)

w_round_1 <- round_1 %>% 
  filter(Team_A > 2000) %>% 
  select(Favorite, Underdog, Probability)


write_csv(m_stein, "~/R Stuff/NCAABasketballPred2023/forClass/m_relevant_matchups.csv")
write_csv(w_stein, "~/R Stuff/NCAABasketballPred2023/forClass/w_relevant_matchups.csv")
write_csv(m_round_1, "~/R Stuff/NCAABasketballPred2023/forClass/m_round_1.csv")
write_csv(w_round_1, "~/R Stuff/NCAABasketballPred2023/forClass/w_round_1.csv")

