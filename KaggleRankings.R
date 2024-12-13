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


rank1 <- read_csv("~/R Stuff/NCAABasketballPred2023/Rankings/KaggleRankings1.csv")
rank2 <- read_csv("~/R Stuff/NCAABasketballPred2023/Rankings/KaggleRankings2.csv")
rank3 <- read_csv("~/R Stuff/NCAABasketballPred2023/Rankings/KaggleRankings3.csv")
rank4 <- read_csv("~/R Stuff/NCAABasketballPred2023/Rankings/KaggleRankings4.csv")
rank5 <- read_csv("~/R Stuff/NCAABasketballPred2023/Rankings/KaggleRankings5.csv")
rank6 <- read_csv("~/R Stuff/NCAABasketballPred2023/Rankings/KaggleRankings6.csv")
rank7 <- read_csv("~/R Stuff/NCAABasketballPred2023/Rankings/KaggleRankings7.csv")
rank8 <- read_csv("~/R Stuff/NCAABasketballPred2023/Rankings/KaggleRankings8.csv")
rank9 <- read_csv("~/R Stuff/NCAABasketballPred2023/Rankings/KaggleRankings9.csv")

df_list <- list(rank1, rank2, rank3, rank4, rank5, rank6, rank7, rank8, rank9)
col_names <- list("TeamName", "rank1", "rank2", "rank3", "rank4", "rank5", "rank6", "rank7", "rank8", "rank9")

combine <- function(v){
  df <- df_list[[1]] 
  colnames(df) <- c("class_rank", "kaggle_rank", "team", "score")
  df <- df %>% 
    select(team, v)
  
  i <- 2
  while(i <= length(df_list)){
    df_to_join <- df_list[[i]]
    colnames(df_to_join) <- c("class_rank", "kaggle_rank", "team", "score")
    df_to_join <- df_to_join %>% 
      select(team, v)
    df <- full_join(df, df_to_join, by = c("team"))
    
    i = i+1
  }
  colnames(df) <- col_names
  return(df)
  
}


kaggle_rank <- combine("kaggle_rank")
class_rank <- combine("class_rank")
score <- combine("score")


write_csv(kaggle_rank, "~/R Stuff/NCAABasketballPred2023/Rankings/kaggle_rankings.csv")
write_csv(class_rank, "~/R Stuff/NCAABasketballPred2023/Rankings/class_rankings.csv")
write_csv(score, "~/R Stuff/NCAABasketballPred2023/Rankings/score.csv")

