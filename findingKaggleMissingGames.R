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

results <- read_csv("~/R Stuff/NCAABasketballPred2023/kaggle84.csv")
m_teams <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MTeams.csv") %>% 
  select(TeamID, TeamName)
w_teams <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/WTeams.csv")
r <- read_csv("~/R Stuff/NCAABasketballPred2023/results.csv")
r2 <- read_csv("~/R Stuff/NCAABasketballPred2023/forClass/m_round_2.csv")
r3 <- read_csv("~/R Stuff/NCAABasketballPred2023/forClass/w_round_2.csv")

our_results <- rbind(r2, r3)

results$Team_A <- substr(results$ID, 6, 9)
results$Team_B <- substr(results$ID, 11, 14)

results <- results %>% 
  mutate(Team_A = as.numeric(Team_A),
         Team_B = as.numeric(Team_B))

team_names <- rbind(m_teams, w_teams)

games <- results %>% 
  left_join(team_names, by = c("Team_A" = "TeamID")) %>% 
  rename("Name_A" = "TeamName") %>% 
  left_join(team_names, by = c("Team_B" = "TeamID")) %>% 
  rename("Name_B" = "TeamName") %>% 
  arrange(Team_A, Team_B) %>% 
  filter(Usage == "Public") %>% 
  left_join(our_results, by = c("Name_A" = "Favorite", "Name_B" = "Underdog")) %>% 
  left_join(our_results, by = c("Name_A" = "Underdog", "Name_B" = "Favorite")) %>% 
  mutate(round = coalesce(Round.x, Round.y),
         Seed_Favorite = coalesce(Seed_Favorite.x, Seed_Favorite.y),
         Seed_Underdog = coalesce(Seed_Underdog.x, Seed_Underdog.y)) %>% 
  select(1:7, round, Seed_Favorite, Seed_Underdog) %>% 
  filter(round == 2)

