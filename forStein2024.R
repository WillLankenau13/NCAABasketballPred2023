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
library("data.table")

#CURRENT ROUND
current_round <- 5

#CLASS MODEL NAMES
df_names <- list("aw", "bb", "bb2", "bspn", "ddd", "ddd2", "ddg", "experts", "gq", "kake", "kake2", "log", "maria", "maria2", "ml", "raaa", "tpp", "tra", "wts")

#LIST OF SUBMISSION FILES
file_list <- list.files(path="~/R Stuff/NCAABasketballPred2023/classPredictions/")

#YEAR
year <- 2023

#NCAA BASKETBALL TEAMS/SCHOOLS
m_teams <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MTeams.csv") 
w_teams <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/WTeams.csv") 

#SEEDS
m_seeds <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MNCAATourneySeeds.csv")
w_seeds <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/WNCAATourneySeeds.csv")

#RESULTS FILE
results <- read_csv("~/R Stuff/NCAABasketballPred2023/results.csv")

#ROUND 0 ELIMINATED TEAMS
r_0_m_elim <- list("SE Missouri St", "Mississippi St", "TX Southern", "Nevada")
r_0_w_elim <- list("Illinois", "Southern Univ", "Purdue", "Monmouth NJ")




#################################################################
#################################################################


#Potential Games
m_teams_pot_games_file <- m_teams %>% 
  filter(LastD1Season == year) %>% 
  select(TeamID) %>% 
  mutate(c = 1)
w_teams_pot_games_file <- w_teams %>% 
  select(TeamID) %>% 
  mutate(c = 1)

m_potential_games <- full_join(m_teams_pot_games_file, m_teams_pot_games_file, by = c("c"), relationship = "many-to-many")
colnames(m_potential_games) <- c("Team_A", "c", "Team_B")
m_potential_games <- m_potential_games %>% 
  select(Team_A, Team_B) %>% 
  filter(Team_A < Team_B) %>% 
  mutate(Season = year)

w_potential_games <- full_join(w_teams_pot_games_file, w_teams_pot_games_file, by = c("c"), relationship = "many-to-many")
colnames(w_potential_games) <- c("Team_A", "c", "Team_B")
w_potential_games <- w_potential_games %>% 
  select(Team_A, Team_B) %>% 
  filter(Team_A < Team_B) %>% 
  mutate(Season = year)


#select m teams
m_teams <- m_teams %>% 
  select(TeamID, TeamName)

w_teams <- w_teams %>% 
  select(TeamID, TeamName)

#Submission files list
df_list <- list()

for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F) 
  temp_df <- data.frame(temp_data)
  colnames(temp_df) <- c("ID", "pred")
  temp_df <- temp_df %>% 
    select(ID, pred)
  df_list <- append(df_list, list(temp_df))
}

#Column names for results df
colnames(results) <- c("Favorite", "Seed_Favorite", "Underdog", "Seed_Underdog", "Winner", "result", "mw", "round")

#
base_col_names <- list("Favorite", "Seed_Favorite", "Underdog", "Seed_Underdog", "Round", "Mean", "Median")

#
losers <- results %>% 
  mutate(loser = ifelse(Winner == Favorite, Underdog, Favorite)) %>% 
  mutate(round = as.numeric(round)) %>% 
  select(mw, loser, round) %>% 
  mutate(mw = ifelse(mw == "M", "m", "w"))

get_loser_list <- function(round_num, mw_var){
  losers_df <- losers %>% 
    filter(mw == mw_var) %>% 
    filter(round < round_num)
  elim <- apply(losers_df, 2, as.list)[[2]]
  if(mw_var == "m"){
    elim <- append(elim, r_0_m_elim)
  } else if(mw_var == "w"){
    elim <- append(elim, r_0_w_elim)
  }
  
  return(elim)
}


#Combine teams
team_names <- rbind(m_teams, w_teams)

#Round Calculation
potential_games <- rbind(m_potential_games, w_potential_games)

#Select
df <- potential_games %>% 
  select(Season, Team_A, Team_B)

seeds <- rbind(m_seeds, w_seeds)
seeds$Region <- substr(seeds$Seed, 1, 1)
seeds$Seed <- substr(seeds$Seed, 2, 3)
seeds <- seeds %>% 
  mutate(Seed = as.numeric(Seed)) %>% 
  select(Season, TeamID, Region, Seed)


#Combine
df <- df %>% 
  left_join(seeds, by = c("Season", "Team_A" = "TeamID")) %>% 
  rename("Region_A" = "Region") %>% 
  rename("Seed_A" = "Seed") %>% 
  left_join(seeds, by = c("Season", "Team_B" = "TeamID")) %>% 
  rename("Region_B" = "Region") %>% 
  rename("Seed_B" = "Seed")


#Round Calculation
df <- df %>% 
  mutate(round = ifelse(Region_A == "X" & Region_B == "W", 5, NA),
         round = ifelse(Region_A == "W" & Region_B == "X", 5, round),
         round = ifelse(Region_A == "Y" & Region_B == "Z", 5, round),
         round = ifelse(Region_A == "Z" & Region_B == "Y", 5, round),
         round = ifelse(Region_A != Region_B & is.na(round), 6, round),
         round = ifelse(Seed_A == Seed_B & is.na(round), 0, round))

df <- df %>% 
  mutate(round = ifelse(Seed_A + Seed_B == 17 & is.na(round), 1, round),
         t_Seed_A = ifelse(Seed_A > 8, 17 - Seed_A, Seed_A),
         t_Seed_B = ifelse(Seed_B > 8, 17 - Seed_B, Seed_B),
         round = ifelse(t_Seed_A + t_Seed_B == 9 & is.na(round), 2, round),
         t_Seed_A = ifelse(t_Seed_A > 4, 9 - t_Seed_A, t_Seed_A),
         t_Seed_B = ifelse(t_Seed_B > 4, 9 - t_Seed_B, t_Seed_B),
         round = ifelse(t_Seed_A + t_Seed_B == 5 & is.na(round), 3, round),
         t_Seed_A = ifelse(t_Seed_A > 2, 5 - t_Seed_A, t_Seed_A),
         t_Seed_B = ifelse(t_Seed_B > 2, 5 - t_Seed_B, t_Seed_B),
         round = ifelse(t_Seed_A + t_Seed_B == 3 & is.na(round), 4, round))

round <- df %>% 
  select(Season, Team_A, Team_B, Seed_A, Seed_B, round) %>% 
  filter(!is.na(Seed_A),
         !is.na(Seed_B)) %>% 
  mutate(ID = paste(Season, "_", Team_A, "_", Team_B, sep = ""))

#get games
get_games <- function(df){
  df <- left_join(round, df, by = c("ID"))
  
  #as numeric
  games <- df %>% 
    mutate(Team_A = as.numeric(Team_A),
           Team_B = as.numeric(Team_B)) 
  
  #Combine with team name
  games <- games %>% 
    left_join(team_names, by = c("Team_A" = "TeamID")) %>% 
    rename("Name_A" = "TeamName") %>% 
    left_join(team_names, by = c("Team_B" = "TeamID")) %>% 
    rename("Name_B" = "TeamName") %>% 
    arrange(Team_A, Team_B) %>% 
    select(Team_A, Name_A, Seed_A, Team_B, Name_B, Seed_B, round, pred)
  
  return(games)
}


#Favorite and Underdog
fav_und <- function(df, r, s, elim_list){
  round <- df %>%
    filter(round == r) %>%
    select(Team_A, Name_A, Seed_A, Team_B, Name_B, Seed_B, round, pred) %>%
    mutate(Favorite = ifelse(Seed_A > Seed_B, Name_B, Name_A),
           Seed_Favorite = ifelse(Seed_A > Seed_B, Seed_B, Seed_A),
           Underdog = ifelse(Seed_A > Seed_B, Name_A, Name_B),
           Seed_Underdog = ifelse(Seed_A > Seed_B, Seed_A, Seed_B),
           Favorite_Probability = ifelse(Seed_A > Seed_B, 1 - pred, pred))
  
  if(s == "m"){
    nice <- round %>%
      filter(Team_A < 2000) %>% 
      filter(!Name_A %in% elim_list) %>% 
      filter(!Name_B %in% elim_list) %>% 
      select(Favorite, Seed_Favorite, Underdog, Seed_Underdog, Favorite_Probability) %>% 
      arrange(Seed_Favorite, Seed_Underdog, Favorite)
  } else if(s == "w"){
    nice <- round %>%
      filter(Team_A > 2000) %>% 
      filter(!Name_A %in% elim_list) %>% 
      filter(!Name_B %in% elim_list) %>% 
      select(Favorite, Seed_Favorite, Underdog, Seed_Underdog, Favorite_Probability) %>% 
      arrange(Seed_Favorite, Seed_Underdog, Favorite)
  }
  
  return(nice)
}

get_round_data <- function(round_num, mw, elim_list){
  df_s <- get_games(df_list[[1]])
  df <- fav_und(df_s, round_num, mw, elim_list) %>% 
    rename("Pred" = "Favorite_Probability")
  
  i <- 2
  while(i <= length(df_list)){
    print(i)
    games <- get_games(df_list[[i]])
    df_fav_und <- fav_und(games, round_num, mw, elim_list) %>% 
      rename("Pred" = "Favorite_Probability")
    df <- left_join(df, df_fav_und, by = c("Favorite", "Seed_Favorite", "Underdog", "Seed_Underdog"))
    i = i+1
  }
  
  df <- df %>% 
    mutate(round = round_num)
  df$mean <- rowMeans(subset(df, select = c(5:(ncol(df) - 1))), na.rm = TRUE)
  df$median = apply(df[,c(5:(ncol(df) - 2))], 1, median, na.rm = TRUE)
  df <- df %>% 
    select(1:4, (ncol(df) - 2):ncol(df), 5:(ncol(df) - 3))
  colnames(df) <- append(base_col_names, df_names)
  return(df)
}

m_combined <- data.frame()
w_combined <- data.frame()

z <- 1
while(z <= current_round){
  m_elim <- get_loser_list(z, "m")
  w_elim <- get_loser_list(z, "w")
  
  m_round <- get_round_data(z, "m", m_elim)
  w_round <- get_round_data(z, "w", w_elim)
  
  m_combined <- rbind(m_combined, m_round)
  w_combined <- rbind(w_combined, w_round)
  
  z <- z + 1
}



##################################################
##################################################

m_combined <- m_combined %>% 
  select(Favorite:Median, experts, ddd, bspn, wts, tra, aw, ml, ddg, kake, log, bb, gq, kake2, tpp, ddd2, maria, raaa, bb2, maria2)

w_combined <- w_combined %>% 
  select(Favorite:Median, experts, ddd, bspn, wts, tra, aw, ml, ddg, kake, log, bb, gq, kake2, tpp, ddd2, maria, raaa, bb2, maria2)


write_csv(m_combined, eval(paste("~/R Stuff/NCAABasketballPred2023/for2024/m_round_", current_round, ".csv" ,sep = "")))
write_csv(w_combined, eval(paste("~/R Stuff/NCAABasketballPred2023/for2024/w_round_", current_round, ".csv" ,sep = "")))

