library("ggplot2")
library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")
library("modelr")
library(leaps)


year <- 2015

#Load files
w <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/finalData/r_tournament_data.csv") %>% 
  filter(Team_A > 2000) %>% 
  filter(Season > year) %>% 
  filter(round != 0) %>% 
  filter(Season != 2023)

train <- w %>% 
  filter(Season != 2022)

models <- function(df, c, n){
  # 
  # w <- sapply(n,function(x){ as.integer(intToBits(x))})
  # w1 <- as.numeric(substring(w[1], 1))
  # w2 <- as.numeric(substring(w[2], 1))
  # w3 <- as.numeric(substring(w[3], 1))
  # w4 <- as.numeric(substring(w[4], 1))
  # w5 <- as.numeric(substring(w[5], 1))
  # w6 <- as.numeric(substring(w[6], 1))
  # w7 <- as.numeric(substring(w[7], 1))
  # w8 <- as.numeric(substring(w[8], 1))
  # w9 <- as.numeric(substring(w[9], 1))
  # w10 <- as.numeric(substring(w[10], 1))
  # w11 <- as.numeric(substring(w[11], 1))
  # w12 <- as.numeric(substring(w[12], 1))
  # w13 <- as.numeric(substring(w[13], 1))
  # w14 <- as.numeric(substring(w[14], 1))
  # w15 <- as.numeric(substring(w[15], 1))
  # 
  # 
  if(c == 0){
    mod <- glm(result ~ round + round*dif_Seed + dif_Seed + dif_fte_rating + p_dif_l_14_win_pct + dif_l_14_win_pct + dif_a_win_pct + dif_off_efficiency + dif_def_efficiency + dif_eff_fg_per + dif_fgm_eff + dif_off_rb_per + dif_def_rb_per + dif_win_pct + dif_conf_win_per + p_dif_win_pct + dif_avg_o_off_eff + dif_avg_o_def_eff + dif_med_o_dif_eff + dif_to_eff + p_dif_conf_win_per + dif_two_pm_eff, data = df, family = "binomial")
  } else if(c == 1){
    mod <- glm(result ~ round + dif_Seed + dif_fte_rating + dif_l_14_win_pct + dif_a_win_pct + dif_off_efficiency + dif_def_efficiency + dif_eff_fg_per + dif_fgm_eff + dif_def_rb_per + dif_mean_g + dif_win_pct + dif_conf_win_per + p_dif_median_g + p_dif_win_pct + p_dif_act_pct + dif_avg_o_off_eff + dif_avg_o_def_eff, data = df, family = "binomial")
  } else if(c == 2){
    mod <- glm(result ~ round + dif_Seed + dif_fte_rating + dif_l_14_win_pct + dif_def_rb_per + dif_win_pct + dif_conf_win_per + p_dif_median_g + p_dif_win_pct + dif_avg_o_off_eff + dif_avg_o_def_eff, data = df, family = "binomial")
  } else if(c == 3){
    mod <- glm(result ~ round + dif_Seed + dif_fte_rating + dif_off_efficiency + dif_def_efficiency + dif_def_rb_per + dif_mean_g + p_dif_median_g + dif_conf_win_per + dif_med_o_dif_eff, data = df, family = "binomial")
  } else if(c == 4){
    mod <- glm(result ~ round + dif_Seed + dif_fte_rating + dif_dif_eff + dif_def_rb_per + dif_mean_g + dif_conf_win_per + dif_med_o_dif_eff, data = df, family = "binomial")
  } else if(c == 5){
    mod <- glm(result ~ dif_Seed + dif_fte_rating + dif_dif_eff + dif_def_rb_per + dif_mean_g + dif_conf_win_per + dif_med_o_dif_eff, data = df, family = "binomial")
  } else if(c == 6){
    mod <- glm(result ~ round + dif_Seed + dif_fte_rating + dif_def_rb_per + dif_win_pct + dif_conf_win_per + p_dif_median_g + p_dif_win_pct + dif_avg_o_off_eff, data = df, family = "binomial")
  } else if(c == 7){
    mod <- glm(result ~ round + dif_Seed + dif_fte_rating + dif_def_rb_per + dif_win_pct + dif_conf_win_per + p_dif_median_g + p_dif_win_pct, data = df, family = "binomial")
  } else if(c == 8){
    mod <- glm(result ~ round + dif_Seed + dif_fte_rating + dif_def_rb_per + dif_win_pct + p_dif_median_g + p_dif_win_pct, data = df, family = "binomial")
  } else if(c == 9){
    mod <- glm(result ~ round + dif_Seed + dif_fte_rating + dif_def_rb_per + dif_win_pct + p_dif_median_g, data = df, family = "binomial")
  } else if(c == 10){
    mod <- glm(result ~ round + dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_g, data = df, family = "binomial")
  } else if(c == 11){
    mod <- glm(result ~ dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_g, data = df, family = "binomial")
  } else if(c == 12){
    mod <- glm(result ~ dif_fte_rating + dif_def_rb_per + p_dif_median_g, data = df, family = "binomial")
  } else if(c == 13){
    mod <- glm(result ~ dif_def_rb_per + p_dif_median_g, data = df, family = "binomial")
  } else if(c == 14){
    mod <- glm(result ~ round + dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_g + p_dif_median_c + dif_conf_win_per, data = df, family = "binomial")
  } else if(c == 15){
    mod <- glm(result ~ dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_g + p_dif_median_c + dif_conf_win_per, data = df, family = "binomial")
  } else if(c == 16){
    mod <- glm(result ~ dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_c + dif_conf_win_per, data = df, family = "binomial")
  } else if(c == 17){
    mod <- glm(result ~ dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_c, data = df, family = "binomial")
  } else if(c == 18){
    mod <- glm(result ~ dif_Seed + dif_def_rb_per + p_dif_median_g, data = df, family = "binomial")
  } else if(c == 19){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_l_14_win_pct + dif_a_win_pct + dif_off_efficiency + dif_def_efficiency + dif_eff_fg_per + dif_fgm_eff + dif_off_rb_per + dif_def_rb_per + dif_mean_g + dif_win_pct + dif_conf_win_per + p_dif_median_g + p_dif_median_c + p_dif_mean_g + p_dif_win_pct + p_dif_act_pct + dif_avg_o_off_eff + dif_avg_o_def_eff + dif_med_o_dif_eff, data = df, family = "binomial")
  } else if(c == 20){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_l_14_win_pct + dif_a_win_pct + dif_off_efficiency + dif_def_efficiency + dif_eff_fg_per + dif_fgm_eff + dif_def_rb_per + dif_mean_g + dif_win_pct + dif_conf_win_per + p_dif_median_g + p_dif_win_pct + p_dif_act_pct + dif_avg_o_off_eff + dif_avg_o_def_eff, data = df, family = "binomial")
  } else if(c == 21){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_l_14_win_pct + dif_def_rb_per + dif_win_pct + dif_conf_win_per + p_dif_median_g + p_dif_win_pct + dif_avg_o_off_eff + dif_avg_o_def_eff, data = df, family = "binomial")
  } else if(c == 22){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_off_efficiency + dif_def_efficiency + dif_def_rb_per + dif_mean_g + p_dif_median_g + dif_conf_win_per + dif_med_o_dif_eff, data = df, family = "binomial")
  } else if(c == 23){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_dif_eff + dif_def_rb_per + dif_mean_g + dif_conf_win_per + dif_med_o_dif_eff, data = df, family = "binomial")
  } else if(c == 24){
    mod <- glm(result ~ 0 + dif_Seed + dif_fte_rating + dif_dif_eff + dif_def_rb_per + dif_mean_g + dif_conf_win_per + dif_med_o_dif_eff, data = df, family = "binomial")
  } else if(c == 25){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + dif_win_pct + dif_conf_win_per + p_dif_median_g + p_dif_win_pct + dif_avg_o_off_eff, data = df, family = "binomial")
  } else if(c == 26){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + dif_win_pct + dif_conf_win_per + p_dif_median_g + p_dif_win_pct, data = df, family = "binomial")
  } else if(c == 27){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + dif_win_pct + p_dif_median_g + p_dif_win_pct, data = df, family = "binomial")
  } else if(c == 28){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + dif_win_pct + p_dif_median_g, data = df, family = "binomial")
  } else if(c == 29){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_g, data = df, family = "binomial")
  } else if(c == 30){
    mod <- glm(result ~ 0 + dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_g, data = df, family = "binomial")
  } else if(c == 31){
    mod <- glm(result ~ 0 + dif_fte_rating + dif_def_rb_per, data = df, family = "binomial")
  } else if(c == 32){
    mod <- glm(result ~ 0 + dif_def_rb_per + p_dif_median_g, data = df, family = "binomial")
  } else if(c == 33){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_g + p_dif_median_c + dif_conf_win_per, data = df, family = "binomial")
  } else if(c == 34){
    mod <- glm(result ~ 0 + dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_g + p_dif_median_c + dif_conf_win_per, data = df, family = "binomial")
  } else if(c == 35){
    mod <- glm(result ~ 0 + dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_c + dif_conf_win_per, data = df, family = "binomial")
  } else if(c == 36){
    mod <- glm(result ~ 0 + dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_c, data = df, family = "binomial")
  } else if(c == 37){
    mod <- glm(result ~ 0 + dif_Seed + dif_def_rb_per + p_dif_median_g, data = df, family = "binomial")
  } else if(c == 38){
    mod <- glm(result ~ 0 + dif_a_win_pct + dif_Seed + dif_mean_g + dif_def_rb_per + p_dif_median_c, data = df, family = "binomial")
  } else if(c == 39){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + dif_win_pct + dif_conf_win_per + p_dif_median_g + p_dif_median_c + p_dif_win_pct + dif_avg_o_off_eff + dif_avg_o_def_eff, data = df, family = "binomial")
  } else if(c == 40){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + dif_win_pct + dif_conf_win_per + p_dif_median_g + p_dif_win_pct + dif_avg_o_off_eff + dif_avg_o_def_eff, data = df, family = "binomial")
  } else if(c == 41){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + dif_win_pct + dif_conf_win_per + p_dif_median_g + p_dif_win_pct + dif_avg_o_def_eff, data = df, family = "binomial")
  } else if(c == 42){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + dif_win_pct + p_dif_median_g + p_dif_win_pct + dif_avg_o_def_eff, data = df, family = "binomial")
  } else if(c == 43){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + dif_win_pct + p_dif_median_g + dif_avg_o_def_eff, data = df, family = "binomial")
  } else if(c == 44){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_c + dif_avg_o_def_eff, data = df, family = "binomial")
  } else if(c == 45){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_c, data = df, family = "binomial")
  } else if(c == 46){
    mod <- glm(result ~ 0 + dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_c, data = df, family = "binomial")
  } else if(c == 47){
    mod <- glm(result ~ 0 + dif_fte_rating + dif_def_rb_per + p_dif_median_c, data = df, family = "binomial")
  } else if(c == 48){
    mod <- glm(result ~ 0 + dif_def_rb_per + p_dif_median_c, data = df, family = "binomial")
  } else if(c == 49){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_g + dif_two_pm_eff, data = df, family = "binomial")
  } else if(c == 50){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + dif_two_pm_eff, data = df, family = "binomial")
  } else if(c == 51){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_two_pm_eff + p_dif_median_g, data = df, family = "binomial")
  } else if(c == 52){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_two_pm_eff + dif_def_rb_per + p_dif_median_g, data = df, family = "binomial")
  } else if(c == 53){
    mod <- glm(result ~ 0 + round + dif_two_pm_eff + dif_fte_rating + dif_def_rb_per + p_dif_median_g, data = df, family = "binomial")
  } else if(c == 54){
    mod <- glm(result ~ 0 + dif_two_pm_eff + dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_g, data = df, family = "binomial")
  } else if(c == 55){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_g + dif_dif_eff, data = df, family = "binomial")
  } else if(c == 56){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_g + round*dif_Seed, data = df, family = "binomial")
  } else if(c == 57){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + dif_win_pct + dif_conf_win_per + p_dif_median_g + round*dif_Seed, data = df, family = "binomial")
  } else if(c == 58){
    mod <- glm(result ~ 0 + dif_fte_rating + dif_def_rb_per + p_dif_median_g + round*dif_Seed, data = df, family = "binomial")
  } else if(c == 59){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + dif_def_rb_per + round*dif_Seed, data = df, family = "binomial")
  } else if(c == 60){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_fte_rating + p_dif_median_g + round*dif_Seed, data = df, family = "binomial")
  } else if(c == 61){
    mod <- glm(result ~ 0 + round + dif_Seed + dif_def_rb_per + p_dif_median_g + round*dif_Seed, data = df, family = "binomial")
  } else if(c == 62){
    mod <- glm(result ~ 0 + round + dif_fte_rating + dif_def_rb_per + p_dif_median_g + round*dif_Seed, data = df, family = "binomial")
  } else if(c == 63){
    mod <- glm(result ~ 0 + dif_Seed + dif_fte_rating + dif_def_rb_per + p_dif_median_g + round*dif_Seed, data = df, family = "binomial")
  } else if(c == 64){
    mod <- glm(result ~ 0 + round*dif_Seed + dif_fte_rating + p_dif_l_14_win_pct + dif_l_14_win_pct + dif_a_win_pct + dif_off_efficiency + dif_fgm_eff + dif_conf_win_per + p_dif_conf_win_per + dif_two_pm_eff, data = df, family = "binomial")
  } else if(c == 65){
    mod <- glm(result ~ 0 + round*dif_Seed + dif_fte_rating + p_dif_l_14_win_pct + dif_l_14_win_pct + dif_off_efficiency + dif_fgm_eff + dif_conf_win_per + p_dif_conf_win_per + dif_two_pm_eff, data = df, family = "binomial")
  } else if(c == 66){
    mod <- glm(result ~ 0 + round*dif_Seed + dif_fte_rating + p_dif_l_14_win_pct + dif_l_14_win_pct + dif_def_efficiency + dif_win_pct + dif_conf_win_per + p_dif_conf_win_per, data = df, family = "binomial")
  } else if(c == 67){
    mod <- glm(result ~ 0 + round*dif_Seed + dif_fte_rating + dif_l_14_win_pct + dif_def_efficiency + dif_win_pct + dif_conf_win_per + p_dif_conf_win_per, data = df, family = "binomial")
  } else if(c == 68){
    mod <- glm(result ~ 0 + round*dif_Seed + dif_fte_rating + dif_l_14_win_pct + dif_off_efficiency + dif_conf_win_per + p_dif_conf_win_per, data = df, family = "binomial")
  } else if(c == 69){
    mod <- glm(result ~ 0 + dif_fte_rating + dif_l_14_win_pct + dif_off_efficiency + dif_conf_win_per + p_dif_conf_win_per, data = df, family = "binomial")
  } else if(c == 70){
    mod <- glm(result ~ 0 + dif_fte_rating + dif_l_14_win_pct + dif_conf_win_per + p_dif_conf_win_per, data = df, family = "binomial")
  } else if(c == 71){
    mod <- glm(result ~ 0 + round*dif_Seed + dif_fte_rating + dif_win_pct, data = df, family = "binomial")
  } else if(c == 72){
    mod <- glm(result ~ 0 + dif_a_win_pct + dif_fte_rating, data = df, family = "binomial")
  } else if(c == 73){
    mod <- glm(result ~ 0 + dif_fte_rating, data = df, family = "binomial")
  } else if(c == 74){
    mod <- glm(result ~ 0 + fte_win_per, data = df, family = "binomial")
  } else if(c == 75){
    mod <- glm(result ~ 0 + round + dif_fte_rating, data = df, family = "binomial")
  } else if(c == 76){
    mod <- glm(result ~ 0 + dif_Seed + dif_fte_rating, data = df, family = "binomial")
  } else if(c == 77){
    mod <- glm(result ~ 0 + round*dif_Seed + dif_fte_rating, data = df, family = "binomial")
  } else if(c == 78){
    mod <- glm(result ~ 0 + round + dif_fte_rating + dif_def_rb_per, data = df, family = "binomial")
  } else if(c == 79){
    mod <- glm(result ~ 0 + dif_Seed + dif_fte_rating + dif_def_rb_per, data = df, family = "binomial")
  } else if(c == 80){
    mod <- glm(result ~ 0 + round*dif_Seed + dif_fte_rating + dif_def_rb_per, data = df, family = "binomial")
  } else if(c == 81){
    mod <- glm(result ~ dif_fte_rating, data = df, family = "binomial")
  } else if(c == 82){
    mod <- glm(result ~ fte_win_per, data = df, family = "binomial")
  } else if(c == 83){
    mod <- glm(result ~ round + dif_fte_rating, data = df, family = "binomial")
  } else if(c == 84){
    mod <- glm(result ~ dif_Seed + dif_fte_rating, data = df, family = "binomial")
  } else if(c == 85){
    mod <- glm(result ~ round*dif_Seed + dif_fte_rating, data = df, family = "binomial")
  } else if(c == 86){
    mod <- glm(result ~ round + dif_fte_rating + dif_def_rb_per, data = df, family = "binomial")
  } else if(c == 87){
    mod <- glm(result ~ dif_Seed + dif_fte_rating + dif_def_rb_per, data = df, family = "binomial")
  } else if(c == 88){
    mod <- glm(result ~ round*dif_Seed + dif_fte_rating + dif_def_rb_per, data = df, family = "binomial")
  } else if(c == 89){
    mod <- glm(result ~ 0 + dif_a_win_pct + dif_fte_rating + round*dif_Seed, data = df, family = "binomial")
  } else if(c == 90){
    mod <- glm(result ~ 0 + dif_a_win_pct + dif_fte_rating + dif_Seed, data = df, family = "binomial")
  } else if(c == 91){
    mod <- glm(result ~ 0 + dif_a_win_pct + dif_fte_rating + round, data = df, family = "binomial")
  } else if(c == 92){
    mod <- glm(result ~ 0 + round*dif_Seed + dif_fte_rating + dif_a_win_pct, data = df, family = "binomial")
  } else if(c == 93){
    mod <- glm(result ~ 0 + dif_fte_rating + dif_win_pct, data = df, family = "binomial")
  } else if(c == 94){
    mod <- glm(result ~ 0 + dif_fte_rating + dif_win_pct + dif_Seed, data = df, family = "binomial")
  } else if(c == 95){
    mod <- glm(result ~ 0 + dif_fte_rating + dif_win_pct + round, data = df, family = "binomial")
  } else if(c == 96){
    mod <- glm(result ~ 0 + dif_fte_rating + dif_win_pct + round*dif_Seed, data = df, family = "binomial")
  } else if(c == 97){
    mod <- glm(result ~ 0 + dif_fte_rating + dif_a_win_pct, data = df, family = "binomial")
  } else if(c == 98){
    mod <- glm(result ~ 0 + dif_fte_rating + dif_a_win_pct + dif_conf_win_per, data = df, family = "binomial")
  } else if(c == 99){
    mod <- glm(result ~ 0 + dif_fte_rating + dif_a_win_pct + dif_Seed + dif_conf_win_per, data = df, family = "binomial")
  }
  
  
  
  return(mod)
}

count <- 99
n_round <- 1
n_end_round <- 3


#dif_conf_win_per

result ~ round*dif_Seed + dif_fte_rating + p_dif_l_14_win_pct + dif_l_14_win_pct + dif_a_win_pct + dif_off_efficiency + dif_def_efficiency + dif_fgm_eff + dif_win_pct + dif_conf_win_per + p_dif_conf_win_per + dif_two_pm_eff

best_subset <- regsubsets(result ~ 0 + round + round*dif_Seed + dif_fte_rating + p_dif_l_14_win_pct + dif_l_14_win_pct + dif_a_win_pct + dif_off_efficiency + dif_def_efficiency + dif_fgm_eff + dif_win_pct + dif_conf_win_per + p_dif_conf_win_per + dif_two_pm_eff, data = w, nvmax = 10)
summary(best_subset)


modeling <- function(df, c, year){
  training <- df %>% 
    filter(Season != year)
  testing <- df %>% 
    filter(Season == year) %>%
    filter(round >= n_round) %>%
    filter(round <= n_end_round)
  
  training[is.na(training)] <- 0
  testing[is.na(testing)] <- 0
  
  model <- models(training, c)
  
  #print(summary(model))
  
  training$pred <- predict(model, training, type = "response")
  training <- training %>% 
    mutate(logloss = - (result*log(pred) + (1-result)*log(1-pred)),
           brier = (pred - result)^2)
  print(mean(training$logloss))
  print(mean(training$brier))
  
  testing$pred <- predict(model, testing, type = "response")
  testing <- testing %>% 
    mutate(logloss = - (result*log(pred) + (1-result)*log(1-pred)),
           brier = (pred - result)^2)
  print(mean(testing$logloss))
  print(mean(testing$brier))
  return(mean(testing$brier))
}

df <-  data.frame("model", "season", "score")
colnames(df) <- c("model", "season", "score")

scores <- function(df, c, season){
  df[nrow(df)+1, ] <- list(c, season, modeling(w, c, season))
  return(df)
}

i <- 0
y <- year + 1
while(i < count + 1){
  while(y < 2023){
    if(y != 2020){
      df <- scores(df, i, y)
    }
    y <- y + 1
  }
  y <- year + 1
  i <- i + 1
}

df <- df %>% 
  filter(model != "model") %>% 
  mutate(score = as.numeric(score))

w_grouped1 <- df %>% 
  group_by(model) %>% 
  summarize(mean = mean(score),
            median = median(score)) %>% 
  mutate(combined = mean + median)

# modeling(d, 0, 2018)
# modeling(d, 1, 2018)
# modeling(d, 2, 2018)
# modeling(d, 3, 2018)
# 
# modeling(d, 0, 2019)
# modeling(d, 1, 2019)
# modeling(d, 2, 2019)
# modeling(d, 3, 2019)
# 
# modeling(d, 0, 2021)
# modeling(d, 1, 2021)
# modeling(d, 2, 2021)
# modeling(d, 3, 2021)
# 
# modeling(d, 0, 2022)
# modeling(d, 1, 2022)
# modeling(d, 2, 2022)
# modeling(d, 3, 2022)

modeling(w, 73, 2022)
modeling(w, 31, 2022)
modeling(w, 47, 2016)
modeling(w, 30, 2016)
modeling(w, 36, 2016)
modeling(w, 46, 2016)

w_model_31 <- glm(result ~ 0 + dif_fte_rating + dif_def_rb_per, data = w, family = "binomial")
summary(w_model_31)

#Good for first half
w_model_94 <- glm(result ~ 0 + dif_fte_rating + dif_win_pct + dif_Seed, data = w, family = "binomial")
summary(w_model_94)

#Good for second half
w_model_72 <- glm(result ~ 0 + dif_a_win_pct + dif_fte_rating, data = w, family = "binomial")
summary(w_model_72)





