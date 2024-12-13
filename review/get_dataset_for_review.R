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
ml <- read_csv("~/R Stuff/NCAABasketballPred2023/Predictions/ml_mod.csv") %>% 
  rename("pred" = "Pred")
log <- read_csv("~/R Stuff/NCAABasketballPred2023/Predictions/log_reg_modB.csv")
experts <- read_csv("~/R Stuff/NCAABasketballPred2023/Predictions/ExpertsMedianSubmission.csv") %>% 
  rename("pred" = "Pred")
data_2023 <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/finalData/data2023.csv")
m_teams <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MTeams.csv") %>% 
  select(TeamID, TeamName)
w_teams <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/WTeams.csv")
results <- read_csv("~/R Stuff/NCAABasketballPred2023/results.csv")
colnames(results) <- c("Favorite", "Seed_Favorite", "Underdog", "Seed_Underdog", "Winner", "result", "mw", "round")


#Eliminated Teams
r_0_m_elim <- list("SE Missouri St", "Mississippi St", "TX Southern", "Nevada")
r_0_w_elim <- list("Illinois", "Southern Univ", "Purdue", "Monmouth NJ")

#
losers <- results %>% 
  mutate(loser = ifelse(Winner == Favorite, Underdog, Favorite)) %>% 
  mutate(round = as.numeric(round)) %>% 
  select(mw, loser, round) %>% 
  mutate(mw = ifelse(mw == "M", "m", "w"))

get_loser_list <- function(round_num, mw_var){
  losers_df <- losers %>% 
    filter(mw == mw_var) %>% 
    filter(round <= round_num)
  elim <- apply(losers_df, 2, as.list)[[2]]
  if(mw_var == "m"){
    elim <- append(elim, r_0_m_elim)
  } else if(mw_var == "w"){
    elim <- append(elim, r_0_w_elim)
  }
  
  return(elim)
}

r_1_m_elim <- get_loser_list(1, "m")
r_1_w_elim <- get_loser_list(1, "w")

r_2_m_elim <- get_loser_list(2, "m")
r_2_w_elim <- get_loser_list(2, "w")

r_3_m_elim <- get_loser_list(3, "m")
r_3_w_elim <- get_loser_list(3, "w")

r_4_m_elim <- get_loser_list(4, "m")
r_4_w_elim <- get_loser_list(4, "w")


#Combine teams
team_names <- rbind(m_teams, w_teams)

get_games <- function(df){
  df <- data_2023
  
  #filter out non-tournament
  games <- df %>% 
    filter(!is.na(Seed_A)) %>% 
    filter(!is.na(Seed_B)) %>% 
    mutate(Team_A = as.numeric(Team_A),
           Team_B = as.numeric(Team_B)) 
  
  #Combine with team name
  games <- games %>% 
    left_join(team_names, by = c("Team_A" = "TeamID")) %>% 
    rename("Name_A" = "TeamName") %>% 
    left_join(team_names, by = c("Team_B" = "TeamID")) %>% 
    rename("Name_B" = "TeamName") %>% 
    arrange(Team_A, Team_B) 
  
  return(games)
}


#Favorite and Underdog
fav_und <- function(df, r, s, elim_list){
  df <- get_games()
  round <- df %>%
    filter(round == r) %>%
    select(Team_A, Name_A, Seed_A, Team_B, Name_B, Seed_B, round) %>%
    mutate(Favorite = ifelse(Seed_A > Seed_B, Name_B, Name_A),
           Seed_Favorite = ifelse(Seed_A > Seed_B, Seed_B, Seed_A),
           Underdog = ifelse(Seed_A > Seed_B, Name_A, Name_B),
           Seed_Underdog = ifelse(Seed_A > Seed_B, Seed_A, Seed_B))
  
  
  if(s == "m"){
    nice <- round %>%
      filter(Team_A < 2000) %>% 
      filter(!Name_A %in% elim_list) %>% 
      filter(!Name_B %in% elim_list) %>% 
      select(Favorite, Seed_Favorite, Underdog, Seed_Underdog) %>% 
      arrange(Seed_Favorite, Seed_Underdog, Favorite)
  } else if(s == "w"){
    nice <- round %>%
      filter(Team_A > 2000) %>% 
      filter(!Name_A %in% elim_list) %>% 
      filter(!Name_B %in% elim_list) %>% 
      select(Favorite, Seed_Favorite, Underdog, Seed_Underdog) %>% 
      arrange(Seed_Favorite, Seed_Underdog, Favorite)
  }
  
  return(nice)
}


m_1 <- fav_und(data_2023, 1, "m", r_0_m_elim)
m_2 <- fav_und(data_2023, 2, "m", r_1_m_elim)
m_3 <- fav_und(data_2023, 3, "m", r_2_m_elim)
m_4 <- fav_und(data_2023, 4, "m", r_3_m_elim)


m_2023 <- rbind(m_1, m_2, m_3, m_4)
