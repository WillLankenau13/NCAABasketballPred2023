library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")
library(corrr)


#Load Files
d_results <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/results.csv")
team_stats <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/team_stats.csv")
m_ap_pre <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/M_AP_preseason.csv")
m_tr_s <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/M_PreTournament_Simple_Rankings.csv") %>% 
  mutate(type = "n")
m_cr_s <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/M_PreConf_Simple_Rankings.csv") %>% 
  mutate(type = "c")
fte <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/FiveThirtyEight_Ratings.csv") %>% 
  mutate(type = "n")
seeds <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/Seeds.csv") %>% 
  mutate(type = "n")
odds <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/Odds.csv") %>% 
  mutate(type = "n")
m_round <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/m_round.csv") 
w_round <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/w_round.csv") 
conf <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/conference_win_per.csv")
m_conferences <- read.csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MTeamConferences.csv")
w_conferences <- read.csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/WTeamConferences.csv")

##Combined Rankings into one
m_r_s <- rbind(m_tr_s, m_cr_s)

#Ap Columns Names
colnames(m_ap_pre) <- c("Season", "TeamID", "AP_pre")

#Conferences
m_conf <- conf %>% 
  filter(mw == "m") %>% 
  left_join(m_conferences, by = c("Season", "Conf" = "ConfAbbrev"))
w_conf <- conf %>% 
  filter(mw == "w") %>% 
  left_join(w_conferences, by = c("Season", "Conf" = "ConfAbbrev"))

conf <- rbind(m_conf, w_conf) %>% 
  rename("conf_win_per" = "win_per") %>% 
  select(Season, TeamID, conf_win_per)

#### Join Function
join <- function(df, dfj, iftype){
  
  
  if(iftype == 1){
    n <- ncol(df)
    df <- df %>% 
      left_join(dfj, by = c("Team_A" = "TeamID", "Season", "type")) 
    colnames(df)[n+1:ncol(df)] <- paste(colnames(df)[n+1:ncol(df)], "_A", sep = "")
  
    n <- ncol(df)
    df <- df %>% 
      left_join(dfj, by = c("Team_B" = "TeamID", "Season", "type")) 
    colnames(df)[n+1:ncol(df)] <- paste(colnames(df)[n+1:ncol(df)], "_B", sep = "")
  } else {
    n <- ncol(df)
    df <- df %>% 
      left_join(dfj, by = c("Team_A" = "TeamID", "Season")) 
    colnames(df)[n+1:ncol(df)] <- paste(colnames(df)[n+1:ncol(df)], "_A", sep = "")
    
    n <- ncol(df)
    df <- df %>% 
      left_join(dfj, by = c("Team_B" = "TeamID", "Season")) 
    colnames(df)[n+1:ncol(df)] <- paste(colnames(df)[n+1:ncol(df)], "_B", sep = "")
  }

  
  return(df)
}



###### Combining

full_data <- join(d_results, seeds, 1) %>% 
  join(conf, 0) %>% 
  join(odds, 1) %>% 
  join(m_r_s, 1) %>% 
  join(m_ap_pre, 0) %>% 
  join(fte, 1) %>% 
  join(team_stats, 0)

m_data <- full_data %>% 
  filter(Team_A < 2000) %>% 
  left_join(m_round, by = c("Season", "DayNum"))
w_data <- full_data %>% 
  filter(Team_A > 2000) %>% 
  left_join(w_round, by = c("Season", "DayNum"))

full_data <- rbind(m_data, w_data)

#Differences
differences <- function(df, v){
  df[, paste("dif", v, sep = "_")] <- df[, paste(v, "A", sep = "_")] - df[, paste(v, "B", sep = "_")]
  return(df)
}

pct_differences <- function(df, v){
  df[, paste("p_dif", v, sep = "_")] <- (df[, paste(v, "A", sep = "_")] - df[, paste(v, "B", sep = "_")]) / (df[, paste(v, "A", sep = "_")] + df[, paste(v, "B", sep = "_")])
  return(df)
}


full_data <- differences(full_data, "Seed")
full_data <- differences(full_data, "fte_rating")
full_data <- differences(full_data, "fta_eff")
full_data <- differences(full_data, "ftm_eff")
full_data <- differences(full_data, "fga_eff")
full_data <- differences(full_data, "fgm_eff")
full_data <- differences(full_data, "three_pa_eff")
full_data <- differences(full_data, "three_pm_eff")
full_data <- differences(full_data, "two_pa_eff")
full_data <- differences(full_data, "two_pm_eff")
full_data <- differences(full_data, "ast_eff")
full_data <- differences(full_data, "to_eff")
full_data <- differences(full_data, "stl_eff")
full_data <- differences(full_data, "off_efficiency")
full_data <- differences(full_data, "def_efficiency")
full_data <- differences(full_data, "dif_eff")
full_data <- differences(full_data, "tempo")
full_data <- differences(full_data, "ft_per")
full_data <- differences(full_data, "ftr")
full_data <- differences(full_data, "ass_to_r")
full_data <- differences(full_data, "fga_pg")
full_data <- differences(full_data, "eff_fg_per")
full_data <- differences(full_data, "off_rb_per")
full_data <- differences(full_data, "def_rb_per")
full_data <- differences(full_data, "threepts_per_tot_pts")
full_data <- differences(full_data, "mean_c")
full_data <- differences(full_data, "median_c")
full_data <- differences(full_data, "MOR")
full_data <- differences(full_data, "POM")
full_data <- differences(full_data, "SAG")
full_data <- differences(full_data, "mean_g")
full_data <- differences(full_data, "median_g")
full_data <- differences(full_data, "var_ft_per")
full_data <- differences(full_data, "win_pct")
full_data <- differences(full_data, "l_14_win_pct")
full_data <- differences(full_data, "h_win_pct")
full_data <- differences(full_data, "a_win_pct")
full_data <- differences(full_data, "conf_win_per")
full_data <- differences(full_data, "avg_o_off_eff")
full_data <- differences(full_data, "avg_o_def_eff")
full_data <- differences(full_data, "med_o_dif_eff")
full_data <- differences(full_data, "mean_o_dif_eff")



full_data <- pct_differences(full_data, "mean_c")
full_data <- pct_differences(full_data, "median_c")
full_data <- pct_differences(full_data, "MOR")
full_data <- pct_differences(full_data, "POM")
full_data <- pct_differences(full_data, "SAG")
full_data <- pct_differences(full_data, "mean_g")
full_data <- pct_differences(full_data, "median_g")
full_data <- pct_differences(full_data, "win_pct")
full_data <- pct_differences(full_data, "l_14_win_pct")
full_data <- pct_differences(full_data, "h_win_pct")
full_data <- pct_differences(full_data, "a_win_pct")
full_data <- pct_differences(full_data, "conf_win_per")
full_data <- pct_differences(full_data, "act_pct")

full_data <- full_data %>% 
  mutate(fte_win_per = 1 - 1/(1+10^(dif_fte_rating*30.464/400)))

#Selecting
test <- full_data %>% 
  select(c(tot_poss_A:tot_o_dr_A, tot_ast_A:minutes_played_A, poss_pg_A:stl_pg_A, poss_o_pg_A:stl_o_pg_A, tot_poss_B:tot_o_dr_B, tot_ast_B:minutes_played_B, poss_pg_B:stl_pg_B, poss_o_pg_B:stl_o_pg_B))
#-c(40:56, 58:72, 86:97, 109:120, 166:182, 184:198, 212:223, 235:246)

full_data <- full_data %>% 
  select(-c(tot_poss_A:tot_o_dr_A, tot_ast_A:minutes_played_A, poss_pg_A:stl_pg_A, poss_o_pg_A:stl_o_pg_A, tot_poss_B:tot_o_dr_B, tot_ast_B:minutes_played_B, poss_pg_B:stl_pg_B, poss_o_pg_B:stl_o_pg_B))

test2 <- full_data %>% 
  select(c(games_A, wins_A, fga_pg_A, fgm_pg_A, h_games_A, h_wins_A, h_tot_poss_A:h_tot_o_pts_A, a_games_A, a_wins_A, a_tot_poss_A:a_tot_o_pts_A, l_14_games_A, l_14_wins_A, games_B, wins_B, fga_pg_B, fgm_pg_B, h_games_B, h_wins_B, h_tot_poss_B:h_tot_o_pts_B, a_games_B, a_wins_B, a_tot_poss_B:a_tot_o_pts_B, l_14_games_B, l_14_wins_B))

full_data <- full_data %>% 
  select(-c(games_A, wins_A, fga_pg_A, fgm_pg_A, h_games_A, h_wins_A, h_tot_poss_A:h_tot_o_pts_A, a_games_A, a_wins_A, a_tot_poss_A:a_tot_o_pts_A, l_14_games_A, l_14_wins_A, games_B, wins_B, fga_pg_B, fgm_pg_B, h_games_B, h_wins_B, h_tot_poss_B:h_tot_o_pts_B, a_games_B, a_wins_B, a_tot_poss_B:a_tot_o_pts_B, l_14_games_B, l_14_wins_B)) %>% 
  select(-c(NumOT))
#-c(37:38, 48:49, 76:77, 79:82, 86:87, 89:92, 96:97, 107:108, 118:119, 146:147, 149:152, 156:157, 166:167, 159:162)

full_data <- full_data %>% 
  filter(!is.na(result))

###### Some Filtering

tournament_data <- full_data %>%
  filter(type == "n")

conf_data <- full_data %>% 
  filter(type == "c")

m_tournament_data <- tournament_data %>% 
  filter(Team_A < 2000)

tc_data <- full_data %>% 
  filter(type == "n" | type == "c")

w_tournament_data <- tournament_data %>% 
  filter(Team_A > 2000)
w_tournament_data <- w_tournament_data[,colSums(is.na(w_tournament_data))<nrow(w_tournament_data)]

#Reduce Heavily
reduced <- full_data %>% 
  select(Season, Team_A, Team_B, Seed_A, Seed_B, type, result, round, dif_Seed, fte_rating_A, fte_rating_B, dif_fte_rating, fte_win_per, p_dif_l_14_win_pct, dif_l_14_win_pct, dif_h_win_pct, dif_a_win_pct, dif_off_efficiency, dif_def_efficiency, dif_dif_eff, dif_eff_fg_per, dif_fgm_eff, dif_off_rb_per, dif_def_rb_per, p_dif_median_g, p_dif_median_c, dif_mean_g, dif_mean_c, dif_median_c, dif_median_g, dif_win_pct, dif_conf_win_per, p_dif_mean_g, p_dif_mean_c, p_dif_win_pct, p_dif_act_pct, dif_avg_o_off_eff, dif_avg_o_def_eff, dif_med_o_dif_eff, dif_to_eff, dif_mean_o_dif_eff, p_dif_conf_win_per, dif_conf_win_per, dif_avg_o_off_eff, dif_fgm_eff, dif_dif_eff, dif_med_o_dif_eff, dif_two_pm_eff)

r_tournament_data <- reduced %>%
  filter(type == "n")

r_conf_data <- reduced %>% 
  filter(type == "c")

r_m_tournament_data <- tournament_data %>% 
  filter(Team_A < 2000)

r_tc_data <- reduced %>% 
  filter(type == "n" | type == "c")

r_w_tournament_data <- tournament_data %>% 
  filter(Team_A > 2000)
r_w_tournament_data <- r_w_tournament_data[,colSums(is.na(r_w_tournament_data))<nrow(r_w_tournament_data)]

#
m_cor <- m_tournament_data %>% 
  select_if(is.numeric)

m_cor <- m_cor %>%
  cor(method = "pearson", use = "complete.obs") 

w_cor <- w_tournament_data %>% 
  select_if(is.numeric)

w_cor <- w_cor %>%
  cor(method = "pearson", use = "complete.obs") 



#Write files

write_csv(full_data, "~/R Stuff/NCAABasketballPred2023/Data/finalData/full_data.csv")
write_csv(tournament_data, "~/R Stuff/NCAABasketballPred2023/Data/finalData/tournament_data.csv")
write_csv(conf_data, "~/R Stuff/NCAABasketballPred2023/Data/finalData/conf_data.csv")
write_csv(w_tournament_data, "~/R Stuff/NCAABasketballPred2023/Data/finalData/w_tournament_data.csv")
write_csv(m_tournament_data, "~/R Stuff/NCAABasketballPred2023/Data/finalData/m_tournament_data.csv")
write_csv(tc_data, "~/R Stuff/NCAABasketballPred2023/Data/finalData/conf_tour_data.csv")

write_csv(reduced, "~/R Stuff/NCAABasketballPred2023/Data/finalData/reduced.csv")
write_csv(r_tournament_data, "~/R Stuff/NCAABasketballPred2023/Data/finalData/r_tournament_data.csv")
write_csv(r_conf_data, "~/R Stuff/NCAABasketballPred2023/Data/finalData/r_conf_data.csv")
write_csv(r_w_tournament_data, "~/R Stuff/NCAABasketballPred2023/Data/finalData/r_w_tournament_data.csv")
write_csv(r_m_tournament_data, "~/R Stuff/NCAABasketballPred2023/Data/finalData/r_m_tournament_data.csv")
write_csv(r_tc_data, "~/R Stuff/NCAABasketballPred2023/Data/finalData/r_conf_tour_data.csv")















