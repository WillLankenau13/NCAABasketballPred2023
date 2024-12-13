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

#
aw <- read_csv("~/R Stuff/NCAABasketballPred2023/classPredictions/AnythingWorks.csv")
colnames(aw) <- c("ID", "pred")
bb <- read_csv("~/R Stuff/NCAABasketballPred2023/classPredictions/BernoulliBallers.csv")
colnames(bb) <- c("ID", "pred")
bb2 <- read_csv("~/R Stuff/NCAABasketballPred2023/classPredictions/BernoulliBallers2.csv")
colnames(bb2) <- c("ID", "pred")
bspn <- read_csv("~/R Stuff/NCAABasketballPred2023/classPredictions/BSPN.csv")
colnames(bspn) <- c("ID", "pred")
ddd2 <- read_csv("~/R Stuff/NCAABasketballPred2023/classPredictions/DoubleDunkDogs.csv")
colnames(ddd2) <- c("ID", "pred")
ddd <- read_csv("~/R Stuff/NCAABasketballPred2023/classPredictions/DoubleDunkDogs2.csv")
colnames(ddd) <- c("ID", "pred")
ddg <- read_csv("~/R Stuff/NCAABasketballPred2023/classPredictions/DunkDunkGoose.csv")
colnames(ddg) <- c("ID", "pred")
gq <- read_csv("~/R Stuff/NCAABasketballPred2023/classPredictions/GoodQuestion.csv")
colnames(gq) <- c("ID", "pred")
kake <- read_csv("~/R Stuff/NCAABasketballPred2023/classPredictions/KAKE01.csv")
colnames(kake) <- c("ID", "pred")
kake2 <- read_csv("~/R Stuff/NCAABasketballPred2023/classPredictions/KAKE012.csv")
colnames(kake2) <- c("ID", "pred")
tpp <- read_csv("~/R Stuff/NCAABasketballPred2023/classPredictions/ThreePointProphets.csv") %>% 
  select(id, pred)
colnames(tpp) <- c("ID", "pred")
tra <- read_csv("~/R Stuff/NCAABasketballPred2023/classPredictions/TRAVIS.csv")
colnames(tra) <- c("ID", "pred")
wts <- read_csv("~/R Stuff/NCAABasketballPred2023/classPredictions/WidgetToySet.csv")
colnames(wts) <- c("ID", "pred")
raaa <- read_csv("~/R Stuff/NCAABasketballPred2023/classPredictions/RAAA23.csv")
colnames(raaa) <- c("ID", "pred")
maria <- read_csv("~/R Stuff/NCAABasketballPred2023/classPredictions/MariaAndFriends.csv")
colnames(maria) <- c("ID", "pred")
maria2 <- read_csv("~/R Stuff/NCAABasketballPred2023/classPredictions/MariaAndFriends2.csv")
colnames(maria2) <- c("ID", "pred")

number_of_NA_models <- 0

#
df_list <- list(ddd, bspn, wts, tra, aw, ml, ddg, kake, log, bb, gq, kake2, tpp, ddd2, maria, raaa, bb2, maria2)
df_names <- list("experts", "ddd", "bspn", "wts", "tra", "aw", "ml", "ddg", "kake", "log", "bb", "gq", "kake2", "tpp", "ddd2", "maria", "raaa", "bb2", "maria2")
base_col_names <- list("Favorite", "Seed_Favorite", "Underdog", "Seed_Underdog", "Round", "Mean", "Median")

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
  df <- left_join(data_2023, df, by = c("ID"))
  
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

get_round_data <- function(starting_df, round_num, mw, elim_list){
  df_s <- get_games(starting_df)
  df <- fav_und(df_s, round_num, mw, elim_list) %>% 
    rename("Pred" = "Favorite_Probability")
  
  i <- 1
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
  df$mean <- rowMeans(subset(df, select = c(5:(ncol(df) - number_of_NA_models - 1))), na.rm = TRUE)
  df$median = apply(df[,c(5:(ncol(df) - number_of_NA_models - 2))], 1, median, na.rm = TRUE)
  df <- df %>% 
    select(1:4, (ncol(df) - 2):ncol(df), 5:(ncol(df) - 3))
  colnames(df) <- append(base_col_names, df_names)
  return(df)
}


m_round_1 <- get_round_data(experts, 1, "m", r_0_m_elim)
w_round_1 <- get_round_data(experts, 1, "w", r_0_w_elim)

m_round_2 <- get_round_data(experts, 2, "m", r_1_m_elim)
w_round_2 <- get_round_data(experts, 2, "w", r_1_w_elim)

m_round_3 <- get_round_data(experts, 3, "m", r_2_m_elim)
w_round_3 <- get_round_data(experts, 3, "w", r_2_w_elim)

m_round_4 <- get_round_data(experts, 4, "m", r_3_m_elim)
w_round_4 <- get_round_data(experts, 4, "w", r_3_w_elim)

m_round_5 <- get_round_data(experts, 5, "m", r_4_m_elim)
w_round_5 <- get_round_data(experts, 5, "w", r_4_w_elim)

m_round_6 <- get_round_data(experts, 6, "m", r_4_m_elim)
w_round_6 <- get_round_data(experts, 6, "w", r_4_w_elim)

m_c_round_2 <- rbind(m_round_1, m_round_2)
w_c_round_2 <- rbind(w_round_1, w_round_2)

m_c_round_3 <- rbind(m_round_1, m_round_2, m_round_3)
w_c_round_3 <- rbind(w_round_1, w_round_2, w_round_3)

m_c_round_4 <- rbind(m_round_1, m_round_2, m_round_3, m_round_4)
w_c_round_4 <- rbind(w_round_1, w_round_2, w_round_3, w_round_4)

m_c_round_5 <- rbind(m_round_1, m_round_2, m_round_3, m_round_4, m_round_5)
w_c_round_5 <- rbind(w_round_1, w_round_2, w_round_3, w_round_4, w_round_5)

m_c_round_6 <- rbind(m_round_1, m_round_2, m_round_3, m_round_4, m_round_5, m_round_6)
w_c_round_6 <- rbind(w_round_1, w_round_2, w_round_3, w_round_4, w_round_5, w_round_6)

write_csv(m_round_1, "~/R Stuff/NCAABasketballPred2023/forClass/m_round_1.csv")
write_csv(w_round_1, "~/R Stuff/NCAABasketballPred2023/forClass/w_round_1.csv")

write_csv(m_c_round_2, "~/R Stuff/NCAABasketballPred2023/forClass/m_round_2.csv")
write_csv(w_c_round_2, "~/R Stuff/NCAABasketballPred2023/forClass/w_round_2.csv")

write_csv(m_c_round_3, "~/R Stuff/NCAABasketballPred2023/forClass/m_round_3.csv")
write_csv(w_c_round_3, "~/R Stuff/NCAABasketballPred2023/forClass/w_round_3.csv")

write_csv(m_c_round_4, "~/R Stuff/NCAABasketballPred2023/forClass/m_round_4.csv")
write_csv(w_c_round_4, "~/R Stuff/NCAABasketballPred2023/forClass/w_round_4.csv")

write_csv(m_c_round_5, "~/R Stuff/NCAABasketballPred2023/forClass/m_round_5.csv")
write_csv(w_c_round_5, "~/R Stuff/NCAABasketballPred2023/forClass/w_round_5.csv")

write_csv(m_c_round_6, "~/R Stuff/NCAABasketballPred2023/forClass/m_round_6.csv")
write_csv(w_c_round_6, "~/R Stuff/NCAABasketballPred2023/forClass/w_round_6.csv")



