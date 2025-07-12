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
  group_by(match_id) %>%
  # first_group(n = 5) %>%
  mutate(time_ms = as.numeric(time_ms)) %>%
  mutate(event_id = 1:n()) %>%
  pivot_longer(c(id, attacker_id),
               names_to = "type",
               values_to = "player") ->
  pbp_long

# need to label each eng whether a trade was attempted or not
# first, check if eng is a trade to a previous kill
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
         in_trade_eng = streak >= 1 & last_kill_s < 5000 & !pdk,
         traded = type == "id") %>%
  group_by(match_id, event_id) %>%
  mutate(other_player = ifelse(type == 'id', lead(player), lag(player))) %>%
  ungroup() %>%
  select(match_id, event_id, type, player, other_player, last_kill_id, in_trade_eng, traded) ->
  trade_kills

events_data %>%
  mutate(time_ms = as.numeric(time_ms)) %>%
  group_by(match_id) %>%
  mutate(event_id = 1:n()) %>%
  left_join(trade_kills %>%
              arrange(match_id, event_id) %>%
              filter(in_trade_eng) %>%
              select(match_id, last_kill_id, player,other_player, in_trade_eng, traded),
            by = c("match_id", 'event_id' = 'last_kill_id')) %>%
  filter(!is.na(traded)) ->
  was_traded

was_traded %>%
  pivot_longer(c(id, attacker_id, other_player),
               names_to = 'player_type',
               values_to = "player_id") %>%
  mutate(p = case_when(
    player_type == "other_player" ~ "P3",
    player_type == "attacker_id"  ~ "P1",
    player_type == "id"  ~ "P2"
  )) %>%
  pivot_wider(id_cols = c(match_id, event_id, traded),
              names_from = c(player_id,p),
              names_glue = "{player_id}_{p}",
              values_from = player_id,
              values_fn = is.character,
              values_fill = FALSE)->
  was_traded_long


pbp_data %>%
  # arrange(match_id, player_id) %>%
  group_by(match_id) %>%
  mutate(
    time_back = time_ms + ((id == player_id)*4000)
  ) %>%
  group_by(match_id, player_id) %>%
  mutate(alive = (cummax(lag(time_back, default = 0)) < time_back)*1) %>%
  select(-attacker_id, -time_back) %>%
  tidyr::pivot_wider(
    names_from = c(player_id, type),
    names_glue = "{player_id}_{type}",
    values_from = alive,
    values_fn = sum,
    values_fill = 0
  ) %>%
  arrange(match_id, time_ms) ->
  pbp_data_wide

## add game state vars
pbp_data_wide %>%
  ungroup() %>%
  select(ends_with("FOR")) %>%
  rowSums() %>%
  as_tibble() %>%
  rename(for_alive = value) ->
  for_l

pbp_data_wide %>%
  ungroup() %>%
  select(ends_with("AGAINST")) %>%
  rowSums() %>%
  as_tibble %>%
  rename(against_alive = value) ->
  against_l

pbp_data_wide %>%
  left_join(match_info %>% collect() %>% select(match_id, mode, map),
            by = "match_id") %>%
  bind_cols(for_l) %>%
  bind_cols(against_l) %>%
  group_by(match_id) %>%
  mutate(state = as.factor(paste0(for_alive, "v", against_alive)),
         adv = factor(for_alive- against_alive)) %>%
  select(match_id, event_id, adv) ->
  state_by_events


## Combine state, who can get trade, did a trade happen
was_traded_long %>%
  left_join(state_by_events, by = c('match_id', "event_id")) ->
  # left_join(pbp_data_wide_traders, by = c('match_id', "event_id")) ->
  # mutate(across(ends_with("AGAINST"), as.factor)) ->
  traded_full 



library(glmnet)
library(tidymodels)
library(ranger)
library(caret)

traded_full %>%
  ungroup() %>% 
  filter(!is.na(adv)) %>%
  mutate(traded  = as.factor(traded)) %>%
  select(TRADE = traded , adv, starts_with(player_df$player_id), -starts_with("ALEXX")) ->
  trade_full_adv

expected_split <- initial_split(trade_full_adv)
expected_train <- training(expected_split)
expected_test <- testing(expected_split)

# make recipe
expected_rec <- recipe(TRADE ~ ., data = expected_train) %>%
  # update_role(c(match_id, event_id), new_role = "ID") %>%
  # step_string2factor(all_predictors(), -tsla) %>%
  step_zv(all_predictors()) %>%
  # step_num2factor(win, levels = c('0','1')) %>%
  # step_normalize(all_outcomes()) %>%
  prep()

## pre process data
pp_test <- 
  expected_rec %>%
  bake(expected_test)

# "juice" the recipe to get the pp training data
pp_train <-
  juice(expected_rec)

lr_model <- 
  # specify that the model is a random forest
  logistic_reg(penalty = 1E-9, mixture = 0) %>%
  # select the engine/package that underlies the model
  set_engine("glm") %>%
  # choose either the continuous regression or binary classification mode
  set_mode("classification")


wf <- workflow() %>%
  add_model(lr_model) %>%
  add_recipe(expected_rec)

fit_model <- 
  wf %>%
  fit(data = pp_train) 

fit_model %>%
  tidy() %>%
  mutate(estimate = exp(estimate)) ->
  proof

proof %>%
  separate(col = 'term',
           into = c('player', 'type' ),
           sep = "_") %>%
  mutate(type = str_remove(type, "TRUE")) %>%
  write_csv("eka-kill-coefs.csv")
  pivot_wider(id_cols = player, names_from = type, values_from = estimate) %>%
  ggplot(aes(P2, P3, label = player))+geom_label()
