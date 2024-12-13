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

e_m_2023 <- m_2023 %>% 
  filter(round <= 3 | is.na(round))

l_m_2023 <- m_2023 %>% 
  filter(round > 3)

e_w_2023 <- w_2023 %>% 
  filter(round <= 3 | is.na(round))

l_w_2023 <- w_2023 %>% 
  filter(round > 3)


#Make predictions
e_m_2023$pred <- predict(m_model_45, e_m_2023, type = "response") 
l_m_2023$pred <- predict(m_model_77, l_m_2023, type = "response") 
e_w_2023$pred <- predict(w_model_94, e_w_2023, type = "response") 
l_w_2023$pred <- predict(w_model_72, l_w_2023, type = "response") 

#Manually calculating
# e_m_2023 <- e_m_2023 %>% 
#   mutate(y = round*m_model_44$coefficients[1] + dif_Seed*m_model_44$coefficients[2] + dif_fte_rating*m_model_44$coefficients[3] + dif_def_rb_per*m_model_44$coefficients[4] + p_dif_median_c*m_model_44$coefficients[5] + dif_avg_o_def_eff*m_model_44$coefficients[6],
#          man_pred = exp(y)/(1 + exp(y)))

# test <- e_m_2023 %>%
#   select(y, pred, man_pred)

#Combine
df_2023 <- rbind(e_m_2023, l_m_2023, e_w_2023, l_w_2023)

#No NAs
df_2023 <- df_2023 %>% 
  mutate(pred = ifelse(is.na(pred), 0.5, pred))

test <- df_2023 %>% 
  filter(!is.na(Seed_A)) %>% 
  filter(!is.na(Seed_B)) 

#Select rows
modB_2023 <- df_2023 %>% 
  select(ID, Team_A, Team_B, Seed_A, Seed_B, round, dif_Seed, fte_rating_A, fte_rating_B, dif_fte_rating, dif_def_rb_per, p_dif_median_g, fte_win_per, pred) %>% 
  mutate(dif = fte_win_per - pred) 

test2 <- modB_2023 %>% 
  filter(!is.na(dif))

mean(test2$dif)
sd(test2$dif)
mean(test2$pred)
sd(test2$pred)

r1 <- test %>% 
  filter(round == 1)


#For Submission
sub_modB_pred <- df_2023 %>% 
  select(ID, pred)

#Write csv
write_csv(df_2023, "~/R Stuff/NCAABasketballPred2023/forClass/log_model_full_data.csv")
write_csv(sub_modB_pred, "~/R Stuff/NCAABasketballPred2023/Predictions/log_reg_modB.csv")

