library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")



#Load Files
team_stats <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/team_stats.csv") %>% 
  filter(Season == 2023)
m_ap_pre <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/M_AP_preseason.csv") %>% 
  filter(Season == 2023)
m_r_s <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/M_PreTournament_Simple_Rankings.csv") %>% 
  filter(Season == 2023)
fte <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/FiveThirtyEight_Ratings.csv") %>% 
  filter(Season == 2023)
seeds <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/Seeds.csv") %>% 
  filter(Season == 2023)
odds <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/Odds.csv") %>% 
  filter(Season == 2023)
conf <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/conference_win_per.csv") %>% 
  filter(Season == 2023)
m_conferences <- read.csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MTeamConferences.csv") %>% 
  filter(Season == 2023)
w_conferences <- read.csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/WTeamConferences.csv") %>% 
  filter(Season == 2023)
m_potential_games <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/finalData/m_games.csv") %>% 
  mutate(Season = 2023)
w_potential_games <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/finalData/w_games.csv") %>% 
  mutate(Season = 2023)
round <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/rounds2023.csv") %>% 
  mutate(Season = 2023)

#potential games
potential_games <- rbind(m_potential_games, w_potential_games)

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

t_data <- join(potential_games, seeds, 0) %>% 
  join(conf, 0) %>% 
  join(odds, 0) %>% 
  join(m_r_s, 0) %>% 
  join(m_ap_pre, 0) %>% 
  join(fte, 0) %>% 
  join(team_stats, 0)

#Round
t_data <- t_data %>% 
  left_join(round, by = c("Season", "Team_A", "Team_B", "Seed_A", "Seed_B"))

test <- t_data %>% 
  filter(!is.na(round))


#Differences
differences <- function(df, v){
  df[, paste("dif", v, sep = "_")] <- df[, paste(v, "A", sep = "_")] - df[, paste(v, "B", sep = "_")]
  return(df)
}

pct_differences <- function(df, v){
  df[, paste("p_dif", v, sep = "_")] <- (df[, paste(v, "A", sep = "_")] - df[, paste(v, "B", sep = "_")]) / (df[, paste(v, "A", sep = "_")] + df[, paste(v, "B", sep = "_")])
  return(df)
}


t_data <- differences(t_data, "Seed")
t_data <- differences(t_data, "fte_rating")
t_data <- differences(t_data, "fta_eff")
t_data <- differences(t_data, "ftm_eff")
t_data <- differences(t_data, "fga_eff")
t_data <- differences(t_data, "fgm_eff")
t_data <- differences(t_data, "three_pa_eff")
t_data <- differences(t_data, "three_pm_eff")
t_data <- differences(t_data, "two_pa_eff")
t_data <- differences(t_data, "two_pm_eff")
t_data <- differences(t_data, "ast_eff")
t_data <- differences(t_data, "to_eff")
t_data <- differences(t_data, "stl_eff")
t_data <- differences(t_data, "off_efficiency")
t_data <- differences(t_data, "def_efficiency")
t_data <- differences(t_data, "dif_eff")
t_data <- differences(t_data, "tempo")
t_data <- differences(t_data, "ft_per")
t_data <- differences(t_data, "ftr")
t_data <- differences(t_data, "ass_to_r")
t_data <- differences(t_data, "fga_pg")
t_data <- differences(t_data, "eff_fg_per")
t_data <- differences(t_data, "off_rb_per")
t_data <- differences(t_data, "def_rb_per")
t_data <- differences(t_data, "threepts_per_tot_pts")
t_data <- differences(t_data, "mean_c")
t_data <- differences(t_data, "median_c")
t_data <- differences(t_data, "MOR")
t_data <- differences(t_data, "POM")
t_data <- differences(t_data, "SAG")
t_data <- differences(t_data, "mean_g")
t_data <- differences(t_data, "median_g")
t_data <- differences(t_data, "var_ft_per")
t_data <- differences(t_data, "win_pct")
t_data <- differences(t_data, "l_14_win_pct")
t_data <- differences(t_data, "h_win_pct")
t_data <- differences(t_data, "a_win_pct")
t_data <- differences(t_data, "conf_win_per")
t_data <- differences(t_data, "avg_o_off_eff")
t_data <- differences(t_data, "avg_o_def_eff")
t_data <- differences(t_data, "med_o_dif_eff")
t_data <- differences(t_data, "mean_o_dif_eff")



t_data <- pct_differences(t_data, "mean_c")
t_data <- pct_differences(t_data, "median_c")
t_data <- pct_differences(t_data, "MOR")
t_data <- pct_differences(t_data, "POM")
t_data <- pct_differences(t_data, "SAG")
t_data <- pct_differences(t_data, "mean_g")
t_data <- pct_differences(t_data, "median_g")
t_data <- pct_differences(t_data, "win_pct")
t_data <- pct_differences(t_data, "l_14_win_pct")
t_data <- pct_differences(t_data, "h_win_pct")
t_data <- pct_differences(t_data, "a_win_pct")
t_data <- pct_differences(t_data, "conf_win_per")
t_data <- pct_differences(t_data, "act_pct")
t_data <- t_data %>% 
  mutate(fte_win_per = 1 - 1/(1+10^(dif_fte_rating*30.464/400)))

#Selecting
test <- t_data %>% 
  select(c(tot_poss_A:tot_o_dr_A, tot_ast_A:minutes_played_A, poss_pg_A:stl_pg_A, poss_o_pg_A:stl_o_pg_A, tot_poss_B:tot_o_dr_B, tot_ast_B:minutes_played_B, poss_pg_B:stl_pg_B, poss_o_pg_B:stl_o_pg_B))
#-c(40:56, 58:72, 86:97, 109:120, 166:182, 184:198, 212:223, 235:246)

t_data <- t_data %>% 
  select(-c(tot_poss_A:tot_o_dr_A, tot_ast_A:minutes_played_A, poss_pg_A:stl_pg_A, poss_o_pg_A:stl_o_pg_A, tot_poss_B:tot_o_dr_B, tot_ast_B:minutes_played_B, poss_pg_B:stl_pg_B, poss_o_pg_B:stl_o_pg_B))

test2 <- t_data %>% 
  select(c(games_A, wins_A, fga_pg_A, fgm_pg_A, h_games_A, h_wins_A, h_tot_poss_A:h_tot_o_pts_A, a_games_A, a_wins_A, a_tot_poss_A:a_tot_o_pts_A, l_14_games_A, l_14_wins_A, games_B, wins_B, fga_pg_B, fgm_pg_B, h_games_B, h_wins_B, h_tot_poss_B:h_tot_o_pts_B, a_games_B, a_wins_B, a_tot_poss_B:a_tot_o_pts_B, l_14_games_B, l_14_wins_B))

t_data <- t_data %>% 
  select(-c(games_A, wins_A, fga_pg_A, fgm_pg_A, h_games_A, h_wins_A, h_tot_poss_A:h_tot_o_pts_A, a_games_A, a_wins_A, a_tot_poss_A:a_tot_o_pts_A, l_14_games_A, l_14_wins_A, games_B, wins_B, fga_pg_B, fgm_pg_B, h_games_B, h_wins_B, h_tot_poss_B:h_tot_o_pts_B, a_games_B, a_wins_B, a_tot_poss_B:a_tot_o_pts_B, l_14_games_B, l_14_wins_B))
#-c(37:38, 48:49, 76:77, 79:82, 86:87, 89:92, 96:97, 107:108, 118:119, 146:147, 149:152, 156:157, 166:167, 159:162)


#Submission Column
t_data <- t_data %>% 
  mutate(ID = paste("2023", Team_A, Team_B, sep = "_"))
 
t_data <- t_data %>% 
  select(ncol(t_data), 1:(ncol(t_data)-1))


###### Some Filtering
m_tournament_data_2023 <- t_data %>% 
  filter(Team_A < 2000)

w_tournament_data_2023 <- t_data %>% 
  filter(Team_A > 2000)
w_tournament_data_2023 <- w_tournament_data_2023[,colSums(is.na(w_tournament_data_2023))<nrow(w_tournament_data_2023)]







write_csv(t_data, "~/R Stuff/NCAABasketballPred2023/Data/finalData/data2023.csv")

