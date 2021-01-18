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

## Useful additional functions
first_group = function(x, n = 1) x %>%
  select(group_cols()) %>%
  distinct %>%
  ungroup %>%
  slice(1:n) %>%
  { semi_join(x, .)}


## Need to divide the data into "shifts"
## each shift is, max, the time between engagements
## need to constantly track how long since each player died


## need roster info first
plist <-
  roster_q %>%
  pull(player_id)

roster_q %>%
  collect() ->
  roster_data

# create list of players that meet criteria
tibble(player_id = plist) %>%
  count(player_id) %>%
  # > 10 games in dataset
  filter(n > 100) ->
  player_df

## convert player kills into "team" kills with a row for each player
events_data %>%
  group_by(match_id) %>%
  # first_group(n = 50) %>%
  # select(-map) %>%
  mutate(time_ms =as.numeric(time_ms),
         event_id = 1:n()) %>%
  left_join(roster_data, by = 'match_id') %>%
  left_join(roster_data, c("attacker_id" = "player_id", 'match_id'),
            suffix = c('_all', "_killer")) %>%
  mutate(type=case_when(team_all == team_killer ~ "FOR", TRUE ~ "AGAINST")) %>%
  select(-starts_with("team_")) ->
  pbp_data


## label each interaction with the time since
## last engagement for the two participating players
events_data %>%
  mutate(time_ms = as.numeric(time_ms)) %>%
  group_by(match_id) %>%
  mutate(event_id = 1:n()) %>%
  pivot_longer(c(id, attacker_id),
               names_to = "type",
               values_to = "player") ->
  pbp_long

pbp_long %>%
  # arrange(match_id, player) %>%
  group_by(match_id, player) %>%
  mutate(tsla =  pmin(15000, time_ms - lag(time_ms, default = 0)),
         life = cumsum(lag(type, default = "id") == "id")) %>%
  group_by(life, .add = T) %>%
  mutate(streak = 1:n()-1,
         pdk = tsla < 5000 & streak == 0,
         last_kill_id = lag(event_id),
         last_kill_s = time_ms - lag(time_ms, default = 0),
         in_trade_eng = streak >= 1 & last_kill_s < 15000 & !pdk,
         win = type == "attacker_id") %>%
  ungroup() %>%
  select(match_id, event_id, type, player, pdk, last_kill_id, in_trade_eng, win) ->
  trade_kills

trade_kills %>%
  group_by(match_id, event_id) %>%
  mutate(trade_kill = any(in_trade_eng) & !all(in_trade_eng)) %>%
  filter(!pdk, trade_kill) ->
  only_trade_kills 

trade_kills %>%
  group_by(match_id, event_id) %>%
  mutate(trade_kill = any(in_trade_eng) & !all(in_trade_eng)) %>%
  filter(!pdk, !trade_kill) ->
  no_trade_kills 

only_trade_kills %>%
  filter(!is.na(last_kill_id)) %>%
  select(match_id, event_id, last_kill_id) %>%
  left_join(pbp_long %>% filter(type == "id"),
            by = c('match_id',"last_kill_id" = "event_id" )) ->
  dead_players

only_trade_kills %>%
  pivot_wider(id_cols = c(match_id, event_id),
              names_from = in_trade_eng, values_from = c(player)) %>%
  rename("TRADE" = `FALSE`, "AGAINST" = `TRUE`) %>%
  left_join(dead_players, by = c('match_id', 'event_id')) %>%
  rename("DEAD"=player) %>%
  select(-last_kill_id, -time_ms, -type) %>%
  left_join(pbp_long %>% filter(type == "attacker_id") %>%
              select(-time_ms, -type), by = c('match_id', 'event_id')) %>%
  mutate(win = player == TRADE) %>%
  select(-player) %>%
  pivot_longer(c(AGAINST, TRADE, DEAD),
               names_to = 'type',
               values_to = 'player') ->
  player_trades_long

player_trades_long %>%
  pivot_wider(id_cols = c(match_id, event_id, win),
              names_from = c(type, player),
              names_glue = "{player}_{type}",
              values_from = player,
              values_fn = is.character,
              values_fill = FALSE) ->
  player_indicator_matrix_trades
  
  
player_indicator_matrix_trades %>%
  left_join(pbp_with_state %>% select(match_id, event_id, state),
            by = c('match_id', "event_id")) %>%
  mutate(across(ends_with(c("TRADE", "AGAINST", "DEAD")), as.factor)) ->
  traded_with_state
  
### Look at each player trade success by state ######
library(ggtext)
library(ggrepel)
player_trades_long %>%
  left_join(pbp_with_state %>% select(match_id, event_id, state),
            by = c('match_id', "event_id")) %>%
  separate(state, into = c("first","sec"), sep = "v") %>%
  mutate(state = case_when(
    win & type == "TRADE"  ~ paste0(first,'v',sec),
     (win & type == "DEAD")~ paste0(first,'v',sec),
    win & type == "AGAINST" ~ paste0(sec,'v',first),
    (!win & type == "TRADE")  ~ paste0(sec,'v',first),
     (!win & type == "DEAD")~ paste0(sec,'v',first),
    !win & type == "AGAINST" ~ paste0(first,'v',sec)
  )) ->
  player_trades_long_state

# player_trades_long_state %>%
#   filter(state %in% c("4v4", "3v4")) %>%
  
  
player_trades_long_state %>%
  filter(state %in% c("4v4", "3v4")) %>%
  group_by(player, type) %>%
  summarise(w = sum(win),
            l = n() - w,
            w_pct = mean(win), 
            n = n(),
            sit_per_game = n/length(unique(match_id))) %>%
  filter(n > 500,  type == "TRADE")  %>%
  ggplot(aes(sit_per_game, w_pct, label = player))+
  geom_point(color = '#3d1866')+
  geom_hline(yintercept = player_trades_long_state %>%
               filter(state %in% c("4v4", "3v4")) %>%
               group_by(player, type) %>%
               summarise(w = sum(win),
                         l = n() - w,
                         w_pct = mean(win), 
                         n = n(),
                         sit_per_game = n/length(unique(match_id))) %>%
               filter(n > 200,  type == "TRADE") %>%
               ungroup() %>%
               summarise(m_win_pct = mean(w_pct)) %>%
               pull(m_win_pct), alpha = 0.5, size = 2, color= 'grey')+
  geom_text_repel(size =4)+
  labs(x = "Situations/Game", y = "Winning Trade, %")+
  ggtitle("I got your trade, trust!","Chance player got his teammate's trade\n4v4 situations only, min. 500 situations")+
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(panel.border = element_rect(color = '#3d1866', fill = NA),
        axis.text = element_text(face = 'bold', color = '#3d1866', size = 16),
        axis.title = element_text(face = 'bold', color = '#3d1866', size = 17),
        legend.text = element_text(face = 'bold', color = '#3d1866', size = 12),
        legend.title = element_text(face = 'bold', color = '#3d1866', size = 14),
        plot.title = element_text(face = 'bold', color = '#3d1866', size = 17),
        plot.subtitle = element_text( color = '#3d1866', size = 12),
        text = element_text(family = 'mono'))
  
player_trades_long_state %>%
  filter(state %in% c("4v4", "4v3")) %>%
  group_by(player, type) %>%
  summarise(w = sum(win),
            l = n() - w,
            w_pct = mean(win), 
            n = n(),
            sit_per_game = n/length(unique(match_id))) %>%
  filter(n > 500,  type == "AGAINST")  %>%
  ggplot(aes(sit_per_game, 1-w_pct, label = player))+
  geom_point(color = '#3d1866')+
  geom_hline(yintercept = player_trades_long_state %>%
               filter(state %in% c("4v4", "4v3")) %>%
               group_by(player, type) %>%
               summarise(w = sum(win),
                         l = n() - w,
                         w_pct = mean(win), 
                         n = n(),
                         sit_per_game = n/length(unique(match_id))) %>%
               filter(n > 500,  type == "AGAINST") %>%
               ungroup() %>%
               summarise(m_win_pct = 1-mean(w_pct)) %>%
               pull(m_win_pct), alpha = 0.5, size = 2, color= 'grey')+
  geom_text_repel(size =4)+
  labs(x = "Situations/Game", y = "Getting a second kill, %")+
  ggtitle("I got one, I got two!","Kill chance in engagement after getting a kill\n4v4 situations only, min. 500 situations")+
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(panel.border = element_rect(color = '#3d1866', fill = NA),
        axis.text = element_text(face = 'bold', color = '#3d1866', size = 16),
        axis.title = element_text(face = 'bold', color = '#3d1866', size = 17),
        legend.text = element_text(face = 'bold', color = '#3d1866', size = 12),
        legend.title = element_text(face = 'bold', color = '#3d1866', size = 14),
        plot.title = element_text(face = 'bold', color = '#3d1866', size = 17),
        plot.subtitle = element_text( color = '#3d1866', size = 12),
        text = element_text(family = 'mono'))

player_trades_long_state %>%
  filter(state %in% c("4v4", "4v3")) %>%
  group_by(player, type) %>%
  summarise(w = sum(win),
            l = n() - w,
            w_pct = mean(win), 
            n = n(),
            sit_per_game = n/length(unique(match_id))) %>%
  filter(n > 500,  type == "DEAD") %>%
  ggplot(aes(sit_per_game, 1-w_pct, label = player))+
  geom_point(color = '#3d1866')+
  geom_hline(yintercept = player_trades_long_state %>%
               filter(state %in% c("4v4", "4v3")) %>%
               group_by(player, type) %>%
               summarise(w = sum(win),
                         l = n() - w,
                         w_pct = mean(win), 
                         n = n(),
                         sit_per_game = n/length(unique(match_id))) %>%
               filter(n > 500,  type == "DEAD") %>%
               ungroup() %>%
               summarise(m_win_pct = 1-mean(w_pct)) %>%
               pull(m_win_pct), alpha = 0.5, size = 2, color= 'grey')+
  geom_text_repel(size =4)+
  labs(x = "Situations/Game", y = "Death Traded, %")+
  ggtitle("Can somebody get my trade?","Chance of a player's death being traded\n4v4 situations only, min. 500 situations")+
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(panel.border = element_rect(color = '#3d1866', fill = NA),
        axis.text = element_text(face = 'bold', color = '#3d1866', size = 16),
        axis.title = element_text(face = 'bold', color = '#3d1866', size = 17),
        legend.text = element_text(face = 'bold', color = '#3d1866', size = 12),
        legend.title = element_text(face = 'bold', color = '#3d1866', size = 14),
        plot.title = element_text(face = 'bold', color = '#3d1866', size = 17),
        plot.subtitle = element_text( color = '#3d1866', size = 12),
        text = element_text(family = 'mono'))

### Build ridge regression on players + state to determine coefs #####
library(glmnet)
library(tidymodels)
library(ranger)

filtered_data <-  
  traded_with_state %>%
  ungroup() %>%
  select(-match_id, -event_id) %>%
  mutate(win = as.factor(win))

impact_split <- initial_split(filtered_data)
impact_train <- training(impact_split)
impact_test <- testing(impact_split)

# make recipe
kill_impact_rec <- recipe(win ~ ., data = impact_train) %>%
  # update_role(c(match_id, event_id), new_role = "ID") %>%
  step_zv(all_predictors()) %>%
  # step_normalize(all_outcomes()) %>%
  prep()

## pre process data
pp_test <- 
  kill_impact_rec %>%
  bake(impact_test)

# "juice" the recipe to get the pp training data
pp_train <-
  juice(kill_impact_rec)

## Now set up a method to tune the ridge penalty instead of assuming 0.1
set.seed(5)
# data_boot <- bootstraps(pp_train)
data_cv <- vfold_cv(pp_train)
lr_model <- 
  logistic_reg(mixture = 0, penalty = 1E-9) %>%
  set_engine("glmnet")
  
best_wf <- workflow() %>%
  add_model(lr_model) %>%
  add_formula(win ~ .)

best_wf %>%
  fit(pp_train) ->
  best_fit
best_fit %>%
  tidy %>%
  separate(term, sep = "_", into = c('name', 'type')) %>%
  select(-penalty) %>%
  # group_by(name) %>%
  # summarise(estimate = sum(estimate))
  pivot_wider(names_from = type, values_from = estimate) %>%
  mutate(across(2:4, replace_na, 0)) %>%
  mutate(total = `NA` - AGAINST1 + FOR1) ->
  shot_coefs
  
  
  


