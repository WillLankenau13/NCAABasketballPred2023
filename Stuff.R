#Filter tournament games
m_tournament_detailed <- m_combined %>% 
  filter(type == "n")
#Filter conf games
m_conference_detailed <- m_combined %>% 
  filter(type == "c")
#Filter regular season games
m_regular_detailed <- m_combined %>% 
  filter(type == "r")

#Adding pre-tournament rankings to tournament results
m_tournament_detailed <- left_join(m_tournament_detailed, m_massey_avg_f, by = c("Team_A" = "TeamID", "Season")) %>% 
  rename("median_rank_A" = "median") %>% 
  rename("mean_rank_A" = "mean") %>% 
  left_join(m_massey_avg_f, by = c("Team_B" = "TeamID", "Season")) %>% 
  rename("median_rank_B" = "median") %>% 
  rename("mean_rank_B" = "mean") 

#Adding pre-conference tournament rankings to conference results
m_conference_detailed <- left_join(m_conference_detailed, m_massey_avg_c, by = c("Team_A" = "TeamID", "Season")) %>% 
  rename("median_rank_A" = "median") %>% 
  rename("mean_rank_A" = "mean") %>% 
  left_join(m_massey_avg_f, by = c("Team_B" = "TeamID", "Season")) %>% 
  rename("median_rank_B" = "median") %>% 
  rename("mean_rank_B" = "mean")


#NAs for regular season rankings
m_regular_detailed <- m_regular_detailed %>% 
  mutate(median_rank_A = NA,
         mean_rank_A = NA,
         median_rank_B = NA,
         mean_rank_B = NA)