library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")

#Load Files
regular_detailed_games <- read.csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/regular_detailed.csv") %>% 
  select(-c(ConfAbbrev))
m_conferences <- read.csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MTeamConferences.csv")
w_conferences <- read.csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/WTeamConferences.csv")

#Combine Conferences
conferences <- rbind(m_conferences, w_conferences) %>% 
  mutate(mw = ifelse(TeamID > 2000, "w", "m"))

detailed_games <- left_join(regular_detailed_games, conferences, by = c("Season", "WTeamID" = "TeamID")) %>% 
  rename("WConf" = "ConfAbbrev") %>% 
  left_join(conferences, by = c("Season", "LTeamID" = "TeamID", "mw")) %>% 
  rename("LConf" = "ConfAbbrev") %>% 
  filter(WConf != LConf)

by_conf_w <- detailed_games %>%
  group_by(Season, WConf, mw) %>% 
  summarize(wins = n())

by_conf_l <- detailed_games %>%
  group_by(Season, LConf, mw) %>% 
  summarize(losses = n())

by_conf <- full_join(by_conf_w, by_conf_l, by = c("Season", "WConf" = "LConf", "mw")) %>% 
  rename("Conf" = "WConf") %>% 
  filter(Season > 2000) %>% 
  mutate(wins = ifelse(is.na(wins), 0, wins),
         losses = ifelse(is.na(losses), 0, losses)) %>% 
  mutate(win_per = wins / (losses + wins))

#Write csv
write_csv(by_conf, "~/R Stuff/NCAABasketballPred2023/Data/myCreated/conference_win_per.csv")









