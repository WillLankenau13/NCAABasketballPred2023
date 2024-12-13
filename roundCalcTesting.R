library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")

#Load Files
download <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/finalData/tournament_data.csv")
m_seeds <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MNCAATourneySeeds.csv")
w_seeds <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/WNCAATourneySeeds.csv")
m_potential_games <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/finalData/m_games.csv") %>% 
  mutate(Season = 2023)
w_potential_games <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/finalData/w_games.csv") %>% 
  mutate(Season = 2023)

#potential games
potential_games <- rbind(m_potential_games, w_potential_games)

#Select
df <- download %>% 
  select(Season, Team_A, Team_B, Seed_A, Seed_B, round)

seeds <- rbind(m_seeds, w_seeds)
seeds$Seed <- substr(seeds$Seed, 2, 3)
seeds <- seeds %>% 
  mutate(Seed = as.numeric(Seed)) %>% 
  select(Season, TeamID, Region, Seed)


#Combine
df <- df %>% 
  left_join(seeds, by = c("Season", "Team_A" = "TeamID")) %>% 
  rename("Region_A" = "Region") %>% 
  rename("Seed_A1" = "Seed") %>% 
  left_join(seeds, by = c("Season", "Team_B" = "TeamID")) %>% 
  rename("Region_B" = "Region") %>% 
  rename("Seed_B1" = "Seed") %>% 
  rename("o_r" = "round")


#Round Calculation
df <- df %>% 
  mutate(round = ifelse(Region_A == "X" & Region_B == "W", 5, NA),
         round = ifelse(Region_A == "W" & Region_B == "X", 5, round),
         round = ifelse(Region_A == "Y" & Region_B == "Z", 5, round),
         round = ifelse(Region_A == "Z" & Region_B == "Y", 5, round),
         round = ifelse(Region_A != Region_B & is.na(round), 6, round),
         round = ifelse(Seed_A == Seed_B & is.na(round), 0, round))

df <- df %>% 
  mutate(round = ifelse(Seed_A + Seed_B == 17 & is.na(round), 1, round),
         t_Seed_A = ifelse(Seed_A > 8, 17 - Seed_A, Seed_A),
         t_Seed_B = ifelse(Seed_B > 8, 17 - Seed_B, Seed_B),
         round = ifelse(t_Seed_A + t_Seed_B == 9 & is.na(round), 2, round),
         t_Seed_A = ifelse(t_Seed_A > 4, 9 - t_Seed_A, t_Seed_A),
         t_Seed_B = ifelse(t_Seed_B > 4, 9 - t_Seed_B, t_Seed_B),
         round = ifelse(t_Seed_A + t_Seed_B == 5 & is.na(round), 3, round),
         t_Seed_A = ifelse(t_Seed_A > 2, 5 - t_Seed_A, t_Seed_A),
         t_Seed_B = ifelse(t_Seed_B > 2, 5 - t_Seed_B, t_Seed_B),
         round = ifelse(t_Seed_A + t_Seed_B == 3 & is.na(round), 4, round))
identical(df[['o_r']],df[['round']])




