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

## okay, so now that we have a model for xKills
## we can predict ind. gunskill, conditional on xkill
## kill ~ xkill + state + player_indicator for shooter only
## need a row for each killer and die-er 

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
  mutate(win = ifelse(type == 'id', 0, 1)) %>%
  filter(!is.na(.pred_TRUE)) %>%
    select(match_id, event_id, player, .pred_TRUE, win) ->
  event_indices


#### Get state adv by event_id #####
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
  # left_join(match_info %>% collect() %>% select(match_id, mode, map),
  #           by = "match_id") %>%
  bind_cols(for_l) %>%
  bind_cols(against_l) %>%
  group_by(match_id) %>%
  mutate(state = as.factor(paste0(for_alive, "v", against_alive)),
         adv = factor(for_alive- against_alive)) %>%
  select(match_id, event_id, adv) ->
  state_by_event

## Combine state and player + kill_prob
event_indices %>%
  rename(kill_prob = .pred_TRUE) %>%
  pivot_wider(
    names_from = player,
    values_from = player,
    values_fn = length,
    values_fill = 0
    ) %>%
  ungroup() %>%
  select(kill_prob, win, starts_with(player_df$player_id)) %>%
  mutate(across(-kill_prob, as.factor),
         win = as.factor(win)) ->
  player_with_state

### Build ridge regression on player + state + kill_prob to determine coefs
set.seed(5)
skill_split <- initial_split(player_with_state)
skill_train <- training(skill_split)
skill_test <- testing(skill_split)
folds <- vfold_cv(skill_train, v = 10)

# make recipe
skill_rec <- recipe(win ~ ., data = skill_train) %>%
  # step_num2factor(all_predictors(), -kill_prob, levels = c("0","1")) %>%
  # update_role(c(mode, map), new_role = "ID") %>%
  step_zv(all_predictors()) %>%
  # step_normalize(all_outcomes()) %>%
  prep()

## pre process data
pp_test <- 
  skill_rec %>%
  bake(impact_test)

# "juice" the recipe to get the pp training data
pp_train <-
  juice(skill_rec)

## Now set up a method to tune the ridge penalty instead of assuming 0.1

# data_boot <- bootstraps(pp_train)
lr_model <- 
  logistic_reg(mixture = 0, penalty = 0) %>%
  set_engine("glmnet") 


wf <- workflow() %>%
  add_model(lr_model) %>%
  add_formula(win ~.)

wf_cv_fit <-
  wf %>%
  fit_resamples(folds) %>%
  metrics(mn_log_loss)

wf_cv_fit %>% 
  collect_predictions()
  mn_log_loss(win, .pred_TRUE)
  collect_metrics() %>%
  # mutate(penalty = factor(penalty)) %>%
  ggplot(aes(penalty, mean, color = .metric))+
  geom_line()

best_model <- logistic_reg(mixture = 0, penalty = 1E-9) %>%
  set_engine("glmnet")

best_wf <- workflow() %>%
  add_model(best_model) %>%
  add_formula(win ~ .)

best_fit <- best_wf %>%
  fit(skill_rec %>%
        bake(player_with_state)) 
best_fit %>%
  tidy %>%
  separate(term, sep = "1", into = c('name')) %>%
  mutate(odds = exp(estimate),
         prob = odds/(1+odds)) %>%
  select(-penalty) ->
  skill_coefs


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


