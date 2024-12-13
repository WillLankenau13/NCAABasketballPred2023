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
df_2023 <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/finalData/data2023.csv")

m_2023 <- df_2023 %>% 
  filter(Team_A < 2000)

w_2023 <- df_2023 %>% 
  filter(Team_A > 2000)


#Make predictions
m_2023$pred <- predict(m_model_29, m_2023, type = "response") 
w_2023$pred <- predict(w_model_31, w_2023, type = "response") 

#Combine
df_2023 <- rbind(m_2023, w_2023)

test <- df_2023 %>% 
  filter(!is.na(Seed_A)) %>% 
  filter(!is.na(Seed_B)) 

#No NAs
df_2023 <- df_2023 %>% 
  mutate(pred = ifelse(is.na(pred), 0.5, pred))


#Select rows
modA_2023 <- df_2023 %>% 
  select(ID, Team_A, Team_B, Seed_A, Seed_B, round, dif_Seed, fte_rating_A, fte_rating_B, dif_fte_rating, dif_def_rb_per, p_dif_median_g, fte_win_per, pred) %>% 
  mutate(dif = fte_win_per - pred)

test2 <- modA_2023 %>% 
  filter(!is.na(dif))

mean(test2$dif)
sd(test2$dif)
mean(test2$pred)
sd(test2$pred)

r1 <- test2 %>% 
  filter(round == 1)


#For Submission
sub_modA_pred <- df_2023 %>% 
  select(ID, pred)

#Write csv
write_csv(sub_modA_pred, "~/R Stuff/NCAABasketballPred2023/Predictions/log_reg_modA.csv")


