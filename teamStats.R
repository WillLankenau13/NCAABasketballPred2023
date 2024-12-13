library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("readr")
library("dplyr")




#Load Files
regular_detailed_games <- read.csv("~/R Stuff/NCAABasketballPred2023/Data/myCreated/regular_detailed.csv")


#Split then Combine Data
w_detailed_games <- regular_detailed_games %>% 
  select(1:2, 3:4, 5:6, 7:8, 9:21, 22:34, 35) %>% 
  mutate(result = 1)
colnames(w_detailed_games) <- c("Season", "DayNum", "TeamID", "Score", "O_TeamID", "O_Score", "Loc", "OT", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA", "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF", "O_FGM", "O_FGA", "O_FGM3", "O_FGA3", "O_FTM", "O_FTA", "O_OR", "O_DR", "O_Ast", "O_TO", "O_Stl", "O_Blk", "O_PF", "Type", "result")

l_detailed_games <- regular_detailed_games %>% 
  select(1:2, 5:6, 3:4, 7:8, 22:34, 9:21, 35) %>% 
  mutate(WLoc = ifelse(WLoc == "H", "A", ifelse(WLoc == "A", "H", "N"))) %>% 
  mutate(result = 0)
colnames(l_detailed_games) <- c("Season", "DayNum", "TeamID", "Score", "O_TeamID", "O_Score", "Loc", "OT", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA", "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF", "O_FGM", "O_FGA", "O_FGM3", "O_FGA3", "O_FTM", "O_FTA", "O_OR", "O_DR", "O_Ast", "O_TO", "O_Stl", "O_Blk", "O_PF", "Type", "result")

detailed_games <- rbind(w_detailed_games, l_detailed_games)

write_csv(detailed_games, "~/R Stuff/NCAABasketballPred2023/Data/myCreated/detailed_games.csv")

#Last 14 Data
last_14 <- detailed_games %>% 
  filter(DayNum < 133 & DayNum > 117) %>% 
  group_by(Season, TeamID) %>% 
  summarize(l_14_games = n(),
            l_14_wins = sum(result)) %>% 
  mutate(l_14_win_pct = l_14_wins/l_14_games)

#Possesion data
detailed_games <- detailed_games %>% 
  mutate(poss = FGA - OR + TO + 0.475*FTA,
         o_poss = O_FGA - O_OR + O_TO + 0.475*O_FTA,
         ft_per = FTM/FTA)

#Home and Away
detailed_home <- detailed_games %>% 
  filter(Loc == "H")

detailed_away <- detailed_games %>% 
  filter(Loc == "A")

#Home
home <- detailed_home %>%
  group_by(Season, TeamID) %>% 
  summarize(h_games = n(),
            h_wins = sum(result),
            h_win_pct = h_wins/h_games,
            h_tot_poss = sum(poss),
            h_tot_pts = sum(Score),
            h_tot_o_poss = sum(o_poss),
            h_tot_o_pts = sum(O_Score))

home <- home %>% 
  mutate(h_off_efficiency = 100*h_tot_pts/h_tot_poss,
         h_def_efficiency = 100*h_tot_o_pts/h_tot_o_poss,
         h_dif_eff = h_off_efficiency - h_def_efficiency)

#Away
away <- detailed_away %>%
  group_by(Season, TeamID) %>% 
  summarize(a_games = n(),
            a_wins = sum(result),
            a_win_pct = a_wins/a_games,
            a_tot_poss = sum(poss),
            a_tot_pts = sum(Score),
            a_tot_o_poss = sum(o_poss),
            a_tot_o_pts = sum(O_Score))

away <- away %>% 
  mutate(a_off_efficiency = 100*a_tot_pts/a_tot_poss,
         a_def_efficiency = 100*a_tot_o_pts/a_tot_o_poss,
         a_dif_eff = a_off_efficiency - a_def_efficiency)

#Group By and totals
by_team <- detailed_games %>%
  group_by(Season, TeamID) %>% 
  summarize(games = n(),
            wins = sum(result),
            win_pct = wins/games,
            tot_poss = sum(poss),
            tot_pts = sum(Score),
            tot_o_poss = sum(o_poss),
            tot_o_pts = sum(O_Score),
            tot_OT = sum(OT),
            tot_fta = sum(FTA),
            tot_ftm = sum(FTM),
            tot_fga = sum(FGA),
            tot_fgm = sum(FGM),
            tot_3pa = sum(FGA3),
            tot_3pm = sum(FGM3),
            tot_2pa = tot_fga - tot_3pa,
            tot_2pm = tot_fgm - tot_3pm,
            tot_or = sum(OR),
            tot_dr = sum(DR),
            tot_o_or = sum(O_OR),
            tot_o_dr = sum(O_DR),
            var_ft_per = var(ft_per),
            tot_ast = sum(Ast),
            tot_to = sum(TO),
            tot_stl = sum(Stl),
            #defense
            tot_o_fta = sum(O_FTA),
            tot_o_ftm = sum(O_FTM),
            tot_o_fga = sum(O_FGA),
            tot_o_fgm = sum(O_FGM),
            tot_o_3pa = sum(O_FGA3),
            tot_o_3pm = sum(O_FGM3),
            tot_o_2pa = tot_o_fga - tot_o_3pa,
            tot_o_2pm = tot_o_fgm - tot_o_3pm,
            tot_o_ast = sum(Ast),
            tot_o_to = sum(TO),
            tot_o_stl = sum(Stl)) %>% 
  mutate(minutes_played = 40*games + 5*tot_OT)

#Stats
by_team <- by_team %>% 
  mutate(off_efficiency = 100*tot_pts/tot_poss,
         def_efficiency = 100*tot_o_pts/tot_o_poss,
         dif_eff = off_efficiency - def_efficiency,
         tempo = 40*tot_poss/minutes_played,
         ft_per = tot_ftm/tot_fta,
         ftr = tot_fta/tot_fga,
         ass_to_r = tot_ast/tot_to,
         fga_pg = tot_fga/games,
         fgm_pg = tot_fgm/games,
         eff_fg_per = (tot_2pm + 1.5*tot_3pm) / tot_fga,
         off_rb_per = tot_or/(tot_or+tot_o_dr),
         def_rb_per = tot_dr/(tot_dr+tot_o_or),
         threepts_per_tot_pts = tot_3pm*3/tot_pts) %>% 
  #Simple Stats
  mutate(poss_pg = tot_poss/games,
         pts_pg = tot_pts/games,
         o_ppts_pg = tot_o_pts/games,
         fta_pg = tot_fta/games,
         ftm_pg = tot_ftm/games,
         fga_pg = tot_fga/games,
         fgm_pg = tot_fgm/games,
         three_pa_pg = tot_3pa/games,
         three_pm_pg = tot_3pm/games,
         two_pa_pg = tot_2pa/games,
         two_pm_pg = tot_2pm/games,
         ast_pg = tot_ast/games,
         to_pg = tot_to/games,
         stl_pg = tot_stl/games,
         fta_eff = 100*tot_fta/tot_poss,
         ftm_eff = 100*tot_ftm/tot_poss,
         fga_eff = 100*tot_fga/tot_poss,
         fgm_eff = 100*tot_fgm/tot_poss,
         three_pa_eff = 100*tot_3pa/tot_poss,
         three_pm_eff = 100*tot_3pm/tot_poss,
         two_pa_eff = 100*tot_2pa/tot_poss,
         two_pm_eff = 100*tot_2pm/tot_poss,
         ast_eff = 100*tot_ast/tot_poss,
         to_eff = 100*tot_to/tot_poss,
         stl_eff = 100*tot_stl/tot_o_poss
         ) %>% 
  #Simple def stats
  mutate(poss_o_pg = tot_o_poss/games,
         fta_o_pg = tot_o_fta/games,
         ftm_o_pg = tot_o_ftm/games,
         fga_o_pg = tot_o_fga/games,
         fgm_o_pg = tot_o_fgm/games,
         three_pa_o_pg = tot_o_3pa/games,
         three_pm_o_pg = tot_o_3pm/games,
         two_pa_o_pg = tot_o_2pa/games,
         two_pm_o_pg = tot_o_2pm/games,
         ast_o_pg = tot_o_ast/games,
         to_o_pg = tot_o_to/games,
         stl_o_pg = tot_o_stl/games,
         fta_o_eff = 100*tot_o_fta/tot_o_poss,
         ftm_o_eff = 100*tot_o_ftm/tot_o_poss,
         fga_o_eff = 100*tot_o_fga/tot_o_poss,
         fgm_o_eff = 100*tot_o_fgm/tot_o_poss,
         three_pa_o_eff = 100*tot_o_3pa/tot_o_poss,
         three_pm_o_eff = 100*tot_o_3pm/tot_o_poss,
         two_pa_o_eff = 100*tot_o_2pa/tot_o_poss,
         two_pm_o_eff = 100*tot_o_2pm/tot_o_poss,
         ast_o_eff = 100*tot_o_ast/tot_o_poss,
         to_o_eff = 100*tot_o_to/tot_o_poss,
         stl_o_eff = 100*tot_o_stl/tot_poss)



################# Strength of Schedule ################# 
sos_measures <- by_team %>% 
  select(Season, TeamID, off_efficiency, def_efficiency, dif_eff, ftr, eff_fg_per, off_rb_per, def_rb_per)


sos_games <- detailed_games %>% 
  select(Season, TeamID, O_TeamID, result) %>% 
  left_join(sos_measures, by = c("Season", "O_TeamID" = "TeamID")) 

colnames(sos_games) <- c("Season", "TeamID", "O_TeamID", "result", "o_off_eff", "o_def_eff", "o_dif_eff", "o_ftr", "o_eff_fg_per", "o_off_rb_per", "o_def_rb_per")

sos <- sos_games %>% 
  group_by(Season, TeamID) %>% 
  summarize(games = n(),
            avg_o_off_eff = sum(o_off_eff)/games,
            avg_o_def_eff = sum(o_def_eff)/games,
            med_o_dif_eff = median(o_dif_eff, na.rm = TRUE),
            avg_o_ftr = sum(o_ftr)/games,
            avg_o_eff_fg_per = sum(o_eff_fg_per)/games,
            avg_o_off_rb_per = sum(o_off_rb_per)/games,
            avg_o_def_rb_per = sum(o_def_rb_per)/games
            ) %>% 
  mutate(mean_o_dif_eff = avg_o_off_eff - avg_o_def_eff)




####

by_team <- by_team %>% 
  left_join(home, by = c("Season", "TeamID")) %>% 
  left_join(away, by = c("Season", "TeamID")) %>% 
  left_join(last_14, by = c("Season", "TeamID")) %>% 
  left_join(sos, by = c("Season", "TeamID", "games"))

Concise <- by_team %>% 
  full_join(sos, by = c("Season", "TeamID", "games")) %>% 
  select(Season, TeamID, 22:34)
  
  

###

write_csv(by_team, "~/R Stuff/NCAABasketballPred2023/Data/myCreated/team_stats.csv")

