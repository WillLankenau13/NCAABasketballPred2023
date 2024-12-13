library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")


#Load Files
df <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/results.csv") %>% 
  select(Season, DayNum, Team_A) %>% 
  filter(DayNum > 133)

#
m_results <- df %>% 
  filter(Team_A < 2000)
w_results <- df %>% 
  filter(Team_A > 2000)

data_2021 <- m_results %>% 
  filter(Season == 2021)
m_round <- m_results %>% 
  filter(Season != 2021) %>% 
  mutate(round = ifelse(DayNum == 134 | DayNum == 135, 0, NA),
         round = ifelse(DayNum == 136 | DayNum == 137, 1, round),
         round = ifelse(DayNum == 138 | DayNum == 139, 2, round),
         round = ifelse(DayNum == 143 | DayNum == 144, 3, round),
         round = ifelse(DayNum == 145 | DayNum == 146, 4, round),
         round = ifelse(DayNum == 152, 5, round),
         round = ifelse(DayNum == 154, 6, round))

data_2021 <- data_2021 %>% 
  mutate(round = ifelse(DayNum == 135 | DayNum == 136, 0, NA),
         round = ifelse(DayNum == 137 | DayNum == 138, 1, round),
         round = ifelse(DayNum == 139 | DayNum == 140, 2, round),
         round = ifelse(DayNum == 145 | DayNum == 146, 3, round),
         round = ifelse(DayNum == 147 | DayNum == 148, 4, round),
         round = ifelse(DayNum == 152, 5, round),
         round = ifelse(DayNum == 154, 6, round))

m_round <- rbind(m_round, data_2021) %>% 
  select(Season, DayNum, round) 

w_counts <- w_results %>% 
  group_by(Season, DayNum) %>% 
  summarize(count = n())

final_day_df <- w_counts %>% 
  filter(count == 1) %>% 
  select(Season, DayNum) %>% 
  rename("final_day" = "DayNum")

w_round <- full_join(w_results, w_counts, by = c("Season", "DayNum")) %>% 
  mutate(round = ifelse(count == 16, 1, NA),
         round = ifelse(count == 8, 2, round),
         round = ifelse(count == 4, 3, round),
         round = ifelse(count == 2 & DayNum < 150 & DayNum > 140, 4, round),
         round = ifelse(count == 2 & DayNum > 150 & DayNum > 140, 5, round),
         round = ifelse(count == 1, 6, round),
         round = ifelse(is.na(round), 0, round)) %>% 
  select(-c("count")) %>% 
  select(Season, DayNum, round)

test <- w_round %>% 
  group_by(Season, round) %>% 
  summarize(count = n())

m_round <- m_round %>% 
  distinct()
w_round <- w_round %>% 
  distinct()

#Write csv
write_csv(m_round, "~/R Stuff/NCAABasketballPred2023/Data/myCreated/m_round.csv")
write_csv(w_round, "~/R Stuff/NCAABasketballPred2023/Data/myCreated/w_round.csv")





