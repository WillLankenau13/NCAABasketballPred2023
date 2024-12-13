library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")



#Load Files
m_massey <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MMasseyOrdinals.csv")
m_seeds <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/MNCAATourneySeeds.csv")
w_seeds <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/WNCAATourneySeeds.csv")
m_fte <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/M538Ratings.csv")
w_fte <- read_csv("~/R Stuff/NCAABasketballPred2023/Data/Kaggle/W538Ratings.csv")

m_massey_stuff <- m_massey %>% 
  filter(SystemName == "MAS") %>% 
  distinct(Season)

#Gets the date the first AP rankings are released
ap_pre_day <- m_massey %>% 
  filter(SystemName == "AP") %>% 
  distinct(Season, RankingDayNum) %>% 
  group_by(Season) %>% 
  summarize(pre = min(RankingDayNum)) %>% 
  mutate(first = 1)

################ Select only some rankings

#Before NCAA Tournament
m_tr <- m_massey %>% 
  filter(RankingDayNum < 133 & RankingDayNum > 125) 
m_tr <- m_tr[!duplicated(m_tr[,c('Season','TeamID', 'SystemName')]),] %>% 
    distinct() %>% 
  select(Season, TeamID, SystemName, OrdinalRank) %>% 
  pivot_wider(names_from = SystemName, values_from = OrdinalRank)

#3 Reliable Metrics Before NCAA Tournament
m_tr_g <- m_massey %>% 
  filter(RankingDayNum < 133 & RankingDayNum > 125) %>% 
  filter(SystemName == "POM" | SystemName == "MOR" | SystemName == "SAG") 
m_tr_g <- m_tr_g[!duplicated(m_tr_g[,c('Season','TeamID', 'SystemName')]),] %>% 
  distinct() %>% 
  select(Season, TeamID, SystemName, OrdinalRank) %>% 
  pivot_wider(names_from = SystemName, values_from = OrdinalRank)

#Before Conference Tournament
m_cr <- m_massey %>% 
  filter(RankingDayNum < 114 & RankingDayNum > 106) 
m_cr <- m_cr[!duplicated(m_cr[,c('Season','TeamID', 'SystemName')]),] %>% 
  distinct() %>% 
  select(Season, TeamID, SystemName, OrdinalRank) %>% 
  pivot_wider(names_from = SystemName, values_from = OrdinalRank)

#3 Reliable Metrics Before Conference Tournament
m_cr_g <- m_massey %>% 
  filter(RankingDayNum < 114 & RankingDayNum > 106) %>% 
  filter(SystemName == "POM" | SystemName == "MOR" | SystemName == "SAG") 
m_cr_g <- m_cr_g[!duplicated(m_cr_g[,c('Season','TeamID', 'SystemName')]),] %>% 
  distinct() %>% 
  select(Season, TeamID, SystemName, OrdinalRank) %>% 
  pivot_wider(names_from = SystemName, values_from = OrdinalRank)

#Preseason AP Rankings
m_ap_pre <- m_massey %>% 
  filter(SystemName == "AP") %>% 
  full_join(ap_pre_day, by = c("Season", "RankingDayNum" = "pre")) %>% 
  filter(first == 1) %>% 
  select(Season, TeamID, OrdinalRank)


################# Means and Medians
mm_func <- function(df){
  df <- df[,colSums(is.na(df))<nrow(df)] %>% 
    mutate(mean = rowMeans(df[ , c(3:ncol(df))], na.rm=TRUE))
  df$median = apply(df[,-c(1:2, ncol(df))], 1, median, na.rm = TRUE)
  
  return(df)
}

m_tr <- mm_func(m_tr)
m_tr_g <- mm_func(m_tr_g)
m_cr <- mm_func(m_cr)
m_cr_g <- mm_func(m_cr_g)

#################

#Select only some columns from the big ones
m_tr_con <- m_tr %>% 
  select(Season, TeamID, mean, median)
m_cr_con <- m_cr %>% 
  select(Season, TeamID, mean, median)

#Join Same Dated Rankings
m_tr_s <- full_join(m_tr_con, m_tr_g, by = c("Season", "TeamID"), suffix = c("_c", "_g"))
m_cr_s <- full_join(m_cr_con, m_cr_g, by = c("Season", "TeamID"), suffix = c("_c", "_g"))

############# Seeds
seeds <- rbind(m_seeds, w_seeds)
seeds$Seed<- substr(seeds$Seed, 2, 3)
seeds <- seeds %>% 
  mutate(Seed = as.numeric(Seed)) %>% 
  select(Season, TeamID, Seed)

############# fte
fte <- rbind(m_fte, w_fte) %>% 
  select(Season, TeamID, 4) %>% 
  distinct()
colnames(fte) <- c("Season", "TeamID", "fte_rating")


### Write csv

#AP Preaseason
write_csv(m_ap_pre, "~/R Stuff/NCAABasketballPred2023/Data/myCreated/M_AP_preseason.csv")

#Tournament Simple
write_csv(m_tr_s, "~/R Stuff/NCAABasketballPred2023/Data/myCreated/M_PreTournament_Simple_Rankings.csv")
#Conf Tournament Simple
write_csv(m_cr_s, "~/R Stuff/NCAABasketballPred2023/Data/myCreated/M_PreConf_Simple_Rankings.csv")

#FiveThirtyEight
write_csv(fte, "~/R Stuff/NCAABasketballPred2023/Data/myCreated/FiveThirtyEight_Ratings.csv")

#Seeds
write_csv(seeds, "~/R Stuff/NCAABasketballPred2023/Data/myCreated/Seeds.csv")









