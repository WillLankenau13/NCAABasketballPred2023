library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")
#


m_teams <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MTeams.csv") %>% 
  filter(LastD1Season == 2023) %>% 
  select(TeamID) %>% 
  mutate(c = 1)
w_teams <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/WTeamConferences.csv") %>% 
  filter(Season == 2023) %>% 
  select(TeamID) %>% 
  mutate(c = 1)

m_potential_games <- full_join(m_teams, m_teams, by = c("c"))
colnames(m_potential_games) <- c("Team_A", "c", "Team_B")
m_potential_games <- m_potential_games %>% 
  select(Team_A, Team_B) %>% 
  filter(Team_A < Team_B)

w_potential_games <- full_join(w_teams, w_teams, by = c("c"))
colnames(w_potential_games) <- c("Team_A", "c", "Team_B")
w_potential_games <- w_potential_games %>% 
  select(Team_A, Team_B) %>% 
  filter(Team_A < Team_B)


#Write csv files
write_csv(m_potential_games, "~/R Stuff/NCAABasketballPred2023/Data/finalData/m_games.csv")
write_csv(w_potential_games, "~/R Stuff/NCAABasketballPred2023/Data/finalData/w_games.csv")



