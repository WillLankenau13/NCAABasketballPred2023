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


#Import csv
men <- read_csv("~/R Stuff/NCAABasketballPred2023/forClass/m_round_6.csv")
women <- read_csv("~/R Stuff/NCAABasketballPred2023/forClass/w_round_6.csv")
pre_brier <- read_csv("~/R Stuff/NCAABasketballPred2023/brier_scores.csv") %>% 
  select(mod, brier)

#Seperate into rounds
men_5 <- men %>% 
  filter(Round == 5)
men_6 <- men %>% 
  filter(Round == 6)
women_5 <- women %>% 
  filter(Round == 5)
women_6 <- women %>% 
  filter(Round == 6)

#col names
col_names <- c("experts", "ddd", "bspn", "wts", "tra", "aw", "ml", "ddg", "kake", "log", "bb", "gq", "kake2", "tpp", "ddd2", "maria", "raaa", "bb2", "maria2")

#pivot longer
men_5 <- men_5 %>% 
  mutate(game = ifelse(Favorite == "Connecticut", 1, 2)) %>% 
  select(1:4, 27, 8:26) %>% 
  pivot_longer(cols = col_names, 
               names_to = "mod",
               values_to = "pred")
women_5 <- women_5 %>% 
  mutate(game = ifelse(Favorite == "South Carolina", 1, 2)) %>% 
  select(1:4, 27, 8:26) %>% 
  pivot_longer(cols = col_names, 
               names_to = "mod",
               values_to = "pred")

men_6 <- men_6 %>% 
  mutate(game = Seed_Favorite*Seed_Underdog)
women_6 <- women_6 %>% 
  mutate(game = Seed_Favorite*Seed_Underdog)

########
result_count <- 0
men <- data.frame(matrix(nrow = 0, ncol = 8))
colnames(men) <- c("Favorite", "Seed_Favorite", "Underdog", "Seed_Underdog", "mod", "pred", "brier", "index")

results <- sapply(result_count,function(x){ as.integer(intToBits(x))})
r1 <- as.numeric(substring(results[1], 1))
r2 <- as.numeric(substring(results[2], 1))
r3 <- as.numeric(substring(results[3], 1))

while(result_count < 8){
  results <- sapply(result_count,function(x){ as.integer(intToBits(x))})
  r1 <- as.numeric(substring(results[1], 1))
  r2 <- as.numeric(substring(results[2], 1))
  r3 <- as.numeric(substring(results[3], 1))
  
  seedA = 0
  seedB = 0
  
  if(r1 == 1){
    seedA = 4
  } else {
    seedA = 5
  }
  
  if(r2 == 1){
    seedB = 5
  } else {
    seedB = 9
  }
  
  gameID = seedA*seedB
  
  m6 <- men_6 %>% 
    filter(game == gameID) %>% 
    select(1:4, 8:26) %>% 
    pivot_longer(cols = col_names, 
                 names_to = "mod",
                 values_to = "pred") %>% 
    mutate(prob = abs(pred - (1 - r3)),
           brier = (pred - r3)^2) %>% 
    mutate(index = result_count)
  
  m5 <- men_5 %>% 
    mutate(prob = ifelse(game == 1, abs(pred - (1 - r1)), abs(pred - (1 - r2)))) %>% 
    mutate(brier = ifelse(game == 1, (pred - r1)^2, (pred - r2)^2)) %>% 
    select(-c('game')) %>% 
    mutate(index = result_count)
  
  men <- rbind(m5, m6, men)
  
  result_count = result_count + 1
}

m_experts <- men %>% 
  filter(mod == "experts") %>% 
  select(index, prob) %>% 
  group_by(index) %>% 
  summarize(prob = prod(prob))
colnames(m_experts) <- c("m_index", "m_prob")

men_brier <- men %>% 
  group_by(mod, index) %>% 
  summarize(sum = sum(brier))



###############
result_count <- 0
women <- data.frame(matrix(nrow = 0, ncol = 8))
colnames(women) <- c("Favorite", "Seed_Favorite", "Underdog", "Seed_Underdog", "mod", "pred", "brier", "index")

while(result_count < 8){
  results <- sapply(result_count,function(x){ as.integer(intToBits(x))})
  r1 <- as.numeric(substring(results[1], 1))
  r2 <- as.numeric(substring(results[2], 1))
  r3 <- as.numeric(substring(results[3], 1))
  
  seedA = 0
  seedB = 0
  
  if(r1 == 1){
    seedA = 1
  } else {
    seedA = 2
  }
  
  if(r2 == 1){
    seedB = 1
  } else {
    seedB = 3
  }
  
  gameID = seedA*seedB
  
  w6 <- women_6 %>% 
    filter(game == gameID) %>% 
    select(1:4, 8:26) %>% 
    pivot_longer(cols = col_names, 
                 names_to = "mod",
                 values_to = "pred") %>% 
    mutate(prob = abs(pred - (1 - r3)),
           brier = (pred - r3)^2) %>% 
    mutate(index = result_count)
  
  w5 <- women_5 %>% 
    mutate(prob = ifelse(game == 1, abs(pred - (1 - r1)), abs(pred - (1 - r2)))) %>% 
    mutate(brier = ifelse(game == 1, (pred - r1)^2, (pred - r2)^2)) %>% 
    select(-c('game')) %>% 
    mutate(index = result_count)
  
  women <- rbind(w5, w6, women)
  
  result_count = result_count + 1
}

w_experts <- women %>% 
  filter(mod == "experts") %>% 
  select(index, prob) %>% 
  group_by(index) %>% 
  summarize(prob = prod(prob))
colnames(w_experts) <- c("w_index", "w_prob")

women_brier <- women %>% 
  group_by(mod, index) %>% 
  summarize(sum = sum(brier))
#########

probability <- crossing(m_experts, w_experts) %>% 
  mutate(prob = m_prob*w_prob,
         index = m_index*8 + w_index) %>% 
  select(index, prob)

brier_scores <- men_brier %>% 
  full_join(women_brier, by = c("mod")) %>% 
  mutate(index = index.x*8 + index.y,
         brier = sum.x + sum.y) %>% 
  select(mod, index, brier)

pre_brier <- pre_brier %>% 
  mutate(p_brier = brier*120) %>% 
  select(mod, p_brier)

brier <- full_join(brier_scores, pre_brier, by = c("mod")) %>% 
  mutate(f_brier = (p_brier + brier)/126) %>% 
  select(mod, index, f_brier)

hafb <- brier %>% 
  filter(mod == "ml" | mod == "log") %>% 
  pivot_wider(names_from = mod, values_from = f_brier)
hafb$min = apply(hafb[,-c(1)], 1, min, na.rm = TRUE)
hafb <- hafb %>% 
  mutate(islog = ifelse(min == log, 1, 0)) %>% 
  select(index, min, islog) %>% 
  rename("f_brier" = "min") %>% 
  mutate(mod = "hafb")

hafb <-  hafb %>% 
  select(mod, index, f_brier)

maria <- brier %>% 
  filter(mod == "maria" | mod == "maria2") %>% 
  pivot_wider(names_from = mod, values_from = f_brier)
maria$min = apply(maria[,-c(1)], 1, min, na.rm = TRUE)
maria <- maria %>% 
  select(index, min) %>% 
  rename("f_brier" = "min") %>% 
  mutate(mod = "maria") %>% 
  select(mod, index, f_brier)

kake <- brier %>% 
  filter(mod == "kake" | mod == "kake2") %>% 
  pivot_wider(names_from = mod, values_from = f_brier)
kake$min = apply(kake[,-c(1)], 1, min, na.rm = TRUE)
kake <- kake %>% 
  select(index, min) %>% 
  rename("f_brier" = "min") %>% 
  mutate(mod = "kake") %>% 
  select(mod, index, f_brier)

team_brier <- brier %>% 
  filter(mod != "experts") %>% 
  filter(mod != "bb2") %>% 
  filter(mod != "kake2") %>% 
  filter(mod != "ddd2") %>% 
  filter(mod != "ml") %>% 
  filter(mod != "log") %>% 
  filter(mod != "maria") %>% 
  filter(mod != "maria2") %>% 
  filter(mod != "kake") %>% 
  filter(mod != "kake2")

t_brier <- rbind(team_brier, hafb, maria, kake)

team_brier <- left_join(t_brier, probability, by = c("index")) %>% 
  select(mod, index, f_brier, prob) %>% 
  mutate(m3 = index %/% 32,
         rem = index %% 32,
         m2 = rem %/% 16,
         rem = index %% 16,
         m1 = rem %/% 8,
         rem = index %% 8,
         w3 = rem %/% 4,
         rem = index %% 4,
         w2 = rem %/% 2,
         w1 = rem %% 2) %>% 
  select(!rem) 

#Comparison between two teams
##### 
comp <- team_brier %>% 
  pivot_wider(names_from = mod, values_from = f_brier) %>% 
  mutate(compa = ifelse(hafb > wts, 0, 1),
         prob_1 = prob*compa)

sum(comp$prob_1)

#####



#

team_brier <- team_brier %>% 
  filter(w2 == 0) %>% 
  filter(w1 == 0) %>% 
  filter(m1 == 1) %>% 
  filter(m2 == 1) %>% 
  filter(w3 == 0)

prob_sum <- sum(team_brier$prob)/13

team_brier <- team_brier %>% 
  mutate(prob = prob/prob_sum)

rank <- team_brier %>%
  arrange(index, f_brier) %>%
  group_by(index) %>%
  mutate(rank = rank(f_brier))

grouped_rank <- rank %>% 
  group_by(mod, rank) %>% 
  summarize(prob = sum(prob))

grouped_grade <- rank %>% 
  mutate(grade = ifelse(rank <= 2, 'A', NA),
         grade = ifelse(rank <= 5 & is.na(grade), 'B', grade),
         grade = ifelse(rank <= 8 & is.na(grade), 'C', grade),
         grade = ifelse(rank <= 11 & is.na(grade), 'D', grade),
         grade = ifelse(is.na(grade), 'E', grade)) %>% 
  group_by(mod, grade) %>% 
  summarize(prob = sum(prob))
  

vis <- rank %>% 
  filter(mod == "hafb")

group_vis <- vis %>% 
  group_by(rank) %>% 
  summarize(prob = sum(prob))

vis |> 
  ggplot() +
  aes(x = rank) +
  xlim(0.5, 13.5) +
  geom_histogram(binwidth = 1)

group_vis |> 
  ggplot() + 
  aes(x = rank, y = prob) + 
  geom_bar(stat='identity') + 
  coord_cartesian(xlim = c(1, 13)) + 
  geom_text(aes(label = round(prob*100, 1)))

colors <- c("red", "blue", "darkblue", "black", "pink", "purple", "lightblue", "grey", "brown", "green", "orange", "yellow", "darkgreen")

grouped_grade |> 
  ggplot() + 
  aes(x = grade, y = prob, fill = mod) + 
  geom_bar(stat='identity', position = position_dodge()) + 
  coord_cartesian(xlim = c(1, 5)) +
  scale_fill_manual(values = colors)


###With Results


#Set the results to games
team_brier_res <- team_brier %>% 
  filter(m3 == 1)
team_brier_res2 <- team_brier %>%
  filter(m3 == 0)

r_rank <- team_brier_res %>%
  arrange(index, f_brier) %>%
  group_by(index) %>%
  mutate(rank = rank(f_brier))

r_rank2 <- team_brier_res2 %>%
  arrange(index, f_brier) %>%
  group_by(index) %>%
  mutate(rank = rank(f_brier))

r_grouped_rank <- r_rank %>% 
  group_by(mod, rank) %>% 
  summarize(prob = sum(prob))

r_grouped_rank2 <- r_rank2 %>%
  group_by(mod, rank) %>%
  summarize(prob = sum(prob))


##Graphy graphs
grouped_rank |>
  ggplot() +
  aes(x = rank, y = prob, fill = mod) +
  geom_bar(stat='identity') +
  coord_cartesian(xlim = c(1, 13)) +
  scale_fill_manual(values = colors)

r_grouped_rank |> 
  ggplot() + 
  aes(x = rank, y = prob, fill = mod) + 
  geom_bar(stat='identity') + 
  coord_cartesian(xlim = c(1, 13)) +
  scale_fill_manual(values = colors)

r_grouped_rank2 |>
  ggplot() +
  aes(x = rank, y = prob, fill = mod) +
  geom_bar(stat='identity') +
  coord_cartesian(xlim = c(1, 13)) +
  scale_fill_manual(values = colors)



#Write Csv
write_csv(rank, "~/R Stuff/NCAABasketballPred2023/finalFour/rank.csv")
write_csv(team_brier, "~/R Stuff/NCAABasketballPred2023/finalFour/team_brier.csv")
