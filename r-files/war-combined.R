library(odbc)
library(DBI)
library(tidyverse)
library(png)
library(RColorBrewer)
con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "MySQL ODBC 8.0 Unicode Driver", 
                      Server = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com", 
                      Port = 3306,
                      UID = "admin",
                      PWD = "guerrillas",
                      Database = "callofduty")

match_info <- tbl(con, "match_info")
events_data <- tbl(con, "events") %>%
  left_join(match_info %>% select(match_id, mode), by = "match_id") %>%
  filter(type %in% c("death"), mode == "Hardpoint") %>%
  select(match_id, time_ms, id, attacker_id) %>%
  collect() 
roster_q <- tbl(con, "boxscores") %>%
  select(match_id, player_id = name, team)
boxscores <- tbl(con, "boxscores") %>%
  left_join(match_info %>% select(match_id, mode, duration_ms), by = "match_id") %>%
  filter(mode == "Hardpoint") %>%
  group_by(match_id) %>%
  summarise(ms_per_eng = (sum(kills)/mean(duration_ms/1000))) %>%
  ungroup() %>%
  summarise(ms_per_eng = mean(ms_per_eng))
  
OR2dProb <- function(OR, prob1) {
  
  prob2 <- (OR - 1)*(prob1/(1 - prob1))/
    (1 + (OR - 1)*(prob1/(1 - prob1)))
  
  return(prob2)
  
}

## Need to calculate kills added through 3 methods:
## 1. Extra Engagements
## 2. Extra Engagement Quality
## 3. Extra Engagement Winning Skill

## 1. Extra Engagements (PACE)
read_csv(here::here('pace_coefs.csv')) %>%
  janitor::clean_names() %>%
  select(player_id = name, AGAINST = 3, FOR = 4) %>%
  pivot_longer(c(FOR, AGAINST)) ->
  pace_coefs

# engagements added = player odds of eng * base_eng_rate (3s/eng in HP)
# kills added = enagements added * 0.5 (base win rate)
events_data %>%
  group_by(match_id) %>%
  # first_group(n = 50) %>%
  # select(-map) %>%
  mutate(time_ms =as.numeric(time_ms),
         event_id = 1:n()) %>%
  left_join(roster_data, by = 'match_id') %>%
  left_join(roster_data, c("attacker_id" = "player_id", 'match_id'),
            suffix = c('_all', "_killer")) %>%
  mutate(
    type=case_when(team_all == team_killer ~ "FOR", TRUE ~ "AGAINST")
  ) %>%
  select(-starts_with("team_")) ->
  pbp_data

pbp_data %>%
  # arrange(match_id, player_id) %>%
  group_by(match_id) %>%
  mutate(
    time_back = time_ms + ((id == player_id)*4000)
  ) %>%
  group_by(match_id, player_id) %>%
  mutate(alive = (cummax(lag(time_back, default = 0)) < time_back)*1) %>%
  group_by(match_id, player_id) %>%
  mutate(duration = time_ms - lag(time_ms, default = 0)) %>%
  filter(!is.na(duration)) %>%
  group_by(player_id, type) %>%
  summarise(toi = sum(duration*(alive == 1)),
            engs = n()) ->
  player_toi


pace_coefs %>%
  inner_join(player_toi, by = c('player_id', 'name' = 'type')) %>%
  mutate(
    # value = value - 1,
    base_pace = (engs/(toi/1000))/value,
    potential_engs = base_pace * (toi/1000)
    ) %>%
  group_by(player_id) %>%
  summarise(
    
    act_pm = sum((name == "FOR")*engs) - sum((name == "AGAINST")*engs),
    pot_pm = sum((name == "FOR")*potential_engs) -
      sum((name == "AGAINST")*potential_engs),
    pm_added_pace = act_pm - pot_pm) %>%
    left_join(roster_q %>%
              group_by(player_id) %>%
              count() %>%
              collect(), by = 'player_id') %>%
  mutate(
    # prob of eng/s x seconds played = engs
    # engs * 0.5 = kills expected 
    kapr_pace = pm_added_pace/n
  ) %>%
  select(player_id, pm_added_pace, kapr_pace) ->
  pm_added_pace


## 2. Extra Engagement Quality
read_csv(here::here('team_kill_influence_coefs.csv')) %>%
  janitor::clean_names() %>%
  select(player_id = name, AGAINST = 3, FOR = 4) %>%
  pivot_longer(c(FOR, AGAINST)) ->
  quality_coefs

# how many engs did they influence?

pbp_data %>%
  # filter(player_id == id | player_id == attacker_id) %>%
  group_by(player_id, type) %>%
  summarise(n= n(),
            games = length(unique(match_id))) ->
  player_k_d_season

quality_coefs %>%
  inner_join(player_k_d_season, by = c('player_id', 'name' = 'type')) %>%
  mutate(kills_added = case_when(name == "AGAINST" ~ -value * n,
                                 TRUE ~ value * n)) %>%
  group_by(player_id) %>%
  summarise(pm_added_total = sum(kills_added*2),
            kapr_quality = pm_added_total/games[1]) ->
  pm_added_eng_quality



## 3. Extra Eng Winning Skill
read_csv(here::here('shooting_skill_coefs.csv')) %>%
  janitor::clean_names() ->
  skill_coefs


pbp_data %>%
  filter(player_id == id | player_id == attacker_id) %>%
  group_by(player_id) %>%
  summarise(eng = n(),
            games = length(unique(match_id))) ->
  player_engs_season

skill_coefs %>%
  inner_join(player_engs_season, by = c("name"='player_id')) %>%
  mutate(kills_added_eng_skill = eng*prob - eng*0.5,
    pm_added_skill = kills_added_eng_skill*2 ,
    kapr_skill = pm_added_skill/games) %>%
  select(player_id = name, pm_added_skill, kapr_skill) ->
  pm_added_skill



### Put it all together
pm_added_pace %>%
  left_join(pm_added_eng_quality, by = 'player_id') %>%
  left_join(pm_added_skill, by = 'player_id') %>%
  mutate(kapr = kapr_pace  + kapr_quality  + kapr_skill,
         pm_total = pm_added_pace  + 
           pm_added_total  + pm_added_skill) %>%
  left_join(roster_q %>%
              group_by(player_id) %>%
              count() %>%
              collect(), by = 'player_id') %>%
  mutate(pm_g = pm_total/n ,
         wapr = pm_g * 0.18) %>%
  arrange(desc(pm_g)) %>%
  select(player_id, kapr, pm_g, pm_total, wapr) ->
  final
