library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")

#Load Files
odds <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/Original_NCAA_Odds.csv")
teams <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MTeamSpellings.csv")

#lowercase
odds <- odds %>% 
  mutate(Team = tolower(Team)) %>%
  mutate(Odds = ifelse(is.na(Odds), 30000, Odds))

combined <- left_join(odds, teams, by = c("Team" = "TeamNameSpelling")) %>% 
  mutate(pct = 100/(100+Odds)) %>% 
  group_by(Year) %>% 
  mutate(total_per = sum(pct),
         act_pct = pct/total_per) %>% 
  rename("Season" = "Year") %>% 
  rename("odds" = "Odds") %>% 
  select(Season, TeamID, odds, pct, act_pct)

by_year <- combined %>% 
  group_by(Season) %>% 
  summarize(count = n())

#write csv
write_csv(combined, "~/R Stuff/NCAABasketballPred2023/Data/myCreated/Odds.csv")





