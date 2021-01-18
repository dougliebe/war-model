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

## okay, so now we have a model for xKills
## now we need to create a ridge regression of xKill ~ state + players
## this will give us the coefs for player impact on kill win outcome,
## Not accouting for the skill of the individual, as xKill is 
## the league average chance to win any engagement 

## First, get xKill by event_id
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
  mutate(streak = 1:n()-1) %>%
  mutate(pdk = tsla < 5000 & streak == 0,
         pkk = streak > 0,
         # tsla = pkk * tsla + (10000*!pkk),
         pdk = replace_na(pdk, FALSE)) ->
  player_context

player_context %>%
  bind_cols(predict(lr_model,
                    expected_rec %>%
                      bake(player_context),type = "prob")) %>%
  group_by(match_id, event_id) %>%
  mutate(.pred_TRUE = .pred_TRUE/sum(.pred_TRUE)) %>%
  filter(type == "id") %>%
  select(player, .pred_TRUE) ->
  xKill_events

##### Next, get same events with indicators for state and each player ###

## filter out players with too few games to get coefs
## need roster info 
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
  left_join(xKill_events, by = c('match_id', 'event_id')) %>%
  mutate(
    type=case_when(team_all == team_killer ~ "FOR", TRUE ~ "AGAINST")
   ) %>%
  select(-starts_with("team_")) ->
  pbp_data

## Need to decide if each player_id is alive, not the dead id in last 4000ms
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
  # ungroup() %>%
  mutate(across(ends_with(c("FOR", "AGAINST")), as.factor)) %>%
  mutate(across(c('map', 'mode'), as.factor),
         shift_index = cumsum(state != lag(state, default = "4v4"))) %>%
  ungroup() %>%
  distinct(match_id, shift_index, .keep_all = T) %>%
  select(-for_alive, -against_alive, -id, -time_ms, -map, -mode, -state) ->
  pbp_with_state

# remove the players with so little data as to mess up everything
pbp_with_state %>%
  filter(!is.na(.pred_TRUE)) %>%
  select(xkill = .pred_TRUE, adv, starts_with(player_df$player_id)) ->
  pbp_filtered

### Build ridge regression on players + state to determine coefs

impact_split <- initial_split(pbp_filtered)
impact_train <- training(impact_split)
impact_test <- testing(impact_split)

# make recipe
kill_impact_rec <- recipe(xkill ~ ., data = impact_train) %>%
  # update_role(c(mode, map), new_role = "ID") %>%
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
  linear_reg(mixture = 0, penalty = tune()) %>%
  set_engine("glmnet") 
# %>%
  # fit(.pred_TRUE ~ ., data = pp_train) 
coefs <- lr_model %>%
  tidy()
  predict(pp_train)
  

tune_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine('glmnet') 
lambda_grid <- grid_regular(penalty(), levels = 10)

wf <- workflow() %>%
  add_model(lr_model) %>%
  add_formula(xkill ~.)

lasso_grid <-
  tune_grid(
    wf,
    resamples = data_cv,
    grid = lambda_grid
  )

lasso_grid %>% 
  collect_metrics() %>%
  # mutate(penalty = factor(penalty)) %>%
  ggplot(aes(penalty, mean, color = .metric))+
  geom_line()

best_model <- linear_reg(mixture = 0, penalty = 1E-9) %>%
  set_engine("glmnet")

best_wf <- workflow() %>%
  add_model(best_model) %>%
  add_formula(xkill ~ .)

best_wf %>%
  fit(kill_impact_rec %>%
        bake(pbp_filtered)) ->
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


coefs %>%
  janitor::clean_names() %>%
  mutate(total = for1 - against1) %>%
  # pivot_longer(c(against1, for1), names_to = 'type') %>%
  ggplot(aes(for1, against1))+
  scale_y_reverse() +
  geom_hline(yintercept = coefs %>%
               janitor::clean_names() %>%
               pull(against1) %>% mean(na.rm = T)) +
  geom_vline(xintercept = coefs %>%
               janitor::clean_names() %>%
               pull(for1) %>% mean(na.rm = T)) +
  geom_label(aes(label = name))

coefs %>%
  janitor::clean_names() %>%
  mutate(total = for1 - against1) %>%
  arrange(desc(total))
