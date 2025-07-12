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

#### Get xkills for each engagement ########
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

## Give every interaction an event_id
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
  mutate(tsla =  pmin(30000, time_ms - lag(time_ms, default = 0)),
         life = cumsum(lag(type, default = "id") == "id")) %>%
  group_by(life, .add = T) %>%
  mutate(streak = 1:n()-1,
         pdk = tsla < 5000 & streak == 0,
         pkk = streak > 0,
         last_kill_id = lag(event_id),
         last_kill_s = time_ms - lag(time_ms, default = 0),
         in_trade_eng = streak >= 1 & last_kill_s < 5000 & !pdk,
         win = type == "attacker_id")  %>%
  ungroup() %>%
  mutate(
    win = factor(type == "attacker_id"),
    
  ) %>%
  filter(!pdk) %>%
  select(match_id, event_id, player, pkk,tsla, win, in_trade_eng) ->
  kill_data

kill_data %>%
  group_by(match_id, event_id) %>%
  sample_n(size = 1) ->
  kill_sample_1

kill_data %>%
  left_join(kill_sample_1, by = c("match_id", "event_id")) %>%
  filter(win.x != win.y) %>%
  pivot_longer(c(player.x, player.y),
               names_to = "player_num",
               values_to = "player"
  ) ->
  kill_two_player_data

kill_two_player_data %>%
  mutate(.pred_odds = exp(0.254*(pkk.x==TRUE) + 2.79E-5*tsla.x +
                            -0.254*(pkk.y==TRUE) - 2.79E-5*tsla.y +
                            -0.0117*(pkk.x & pkk.y)),
         .pred_WIN = .pred_odds/(1+.pred_odds),
         .pred_WIN = ifelse(player_num == "player.x", .pred_WIN, 1-.pred_WIN),
         WIN = ifelse(player_num == "player.x", win.x== TRUE, win.y==TRUE),
         in_trade = ifelse(player_num == "player.x", in_trade_eng.x, in_trade_eng.y)) %>%
  select(match_id, event_id, player, .pred_WIN, WIN, in_trade) ->
  eng_xkill_results

### GET adv by event_id ####
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

events_data %>%
  mutate(time_ms = as.numeric(time_ms)) %>%
  group_by(match_id) %>%
  mutate(event_id = 1:n()) %>%
  bind_cols(for_l) %>%
  bind_cols(against_l) %>%
  group_by(match_id) %>%
  mutate(state = as.factor(paste0(for_alive, "v", against_alive)),
         adv = for_alive- against_alive) %>%
  select(match_id, event_id, adv)->
  pbp_with_state
  
#### Combine state and xkills #######
eng_xkill_results %>%
  left_join(pbp_with_state, by = c('match_id', 'event_id')) %>%
  group_by(match_id, event_id) %>%
  mutate(trade_sit = case_when(
    all(in_trade) ~ "BOTH",
    in_trade ~ "SOLO",
    any(in_trade) ~ "OTHER",
    TRUE ~ "NONE"
  )) %>%
  mutate(adv = ifelse(WIN, adv, -adv)) ->
  events_combined

events_combined %>%
  ungroup() %>%
  pivot_wider(names_from = player,
              names_glue = "{player}_ENG",
              values_from = player,
              values_fn = is.character,
              values_fill = FALSE
  ) %>%
  mutate(WIN = as.factor(WIN),
         adv = as.factor(adv)) ->
  events_combined_factors

# split data so that p1 and p2 are evenly the killer
library(glmnet)
library(tidymodels)
library(ranger)
library(caret)

events_combined_factors %>%
  filter(!is.na(adv), trade_sit != "NONE") %>%
  select(WIN, .pred_WIN, adv, starts_with(player_df$player_id), -starts_with("ALEXX")) ->
  events_filtered

expected_split <- initial_split(events_filtered)
expected_train <- training(expected_split)
expected_test <- testing(expected_split)

# make recipe
expected_rec <- recipe(WIN ~ ., data = expected_train) %>%
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
