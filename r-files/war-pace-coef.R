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
  mutate(duration = time_ms - lag(time_ms, default = NA),
         epm = 1/((duration)/1000)*60,
         shot = 1,
         state = as.factor(paste0(for_alive, "v", against_alive))) %>%
  # # ungroup() %>%
  # filter( !is.na(epm), !is.infinite(epm)) %>%
  # mutate(across(ends_with(c("FOR", "AGAINST")), as.factor)) %>%
  # mutate(across(c('map', 'mode'), as.factor),
  #        shift_index = cumsum(state != lag(state, default = "4v4"))) %>%
  # group_by(shift_index, .add = T) %>%
  # mutate(duration = sum(duration),
  #        epm = n()/((duration)/1000)*60) %>%
  # ungroup() %>%
  # distinct(match_id, shift_index, .keep_all = T) %>%
  select(-for_alive, -against_alive, -id, -match_id, -time_ms) ->
  pbp_with_state


### Building up the ridge regression
library(glmnet)
library(tidymodels)

# remove the players with so little data as to mess up everything
pbp_with_state %>%
  ungroup() %>%
  # group_by(match_id)
  # mutate()
  filter(!is.na(duration)) %>%
  mutate(duration = duration + (duration == 0)*1) %>%
  select(duration, shot, state, starts_with(player_df$player_id)) ->
  pbp_filtered



data_split <- initial_split(pbp_filtered)
data_train <- training(data_split)
data_test <- testing(data_split)

# make recipe
for_against_rec <- recipe(duration ~ ., data = data_train) %>%
  update_role(shot, new_role = "ID") %>%
  step_zv(all_predictors()) %>%
  # step_normalize(all_outcomes()) %>%
  prep()

## pre process data
pp_test <- 
  for_against_rec %>%
  bake(data_test)

# "juice" the recipe to get the pp training data
pp_train <-
  juice(for_against_rec)

## Now set up a method to tune the LASSO penalty instead of assuming 0.1
set.seed(5)
# data_boot <- bootstraps(pp_train)
data_cv <- vfold_cv(pp_train)

# data_boot <- bootstraps(pp_train)
lr_model <- 
  logistic_reg(mixture = 0, penalty = 0) %>%
  
  set_engine("glmnet") 


wf <- workflow() %>%
  add_model(lr_model) %>%
  add_formula(win ~.)
surv <- survival::Surv(pp_train$duration, data_train$shot)
cox_model <- cv.glmnet(x = model.matrix( ~. , pp_train %>%
                                          select(-duration, -shot)),
             y = surv,
             family = "cox",
             nfolds = 4,
             alpha = 0,
             parallel = FALSE
)
coef(cox_model) %>%
  # rownames()
  exp() %>%
  as.matrix() %>%
  as_tibble() %>%
    bind_cols(coef(cox_model) %>%
                rownames()) %>%
  rename(odds = 1, name = 2) %>%
  select(name, odds) ->
  pace_coefs

pace_coefs %>%
  separate(name, sep = "_", into = c('name', 'type')) %>%
  pivot_wider(names_from = type, values_from = odds) %>%
  mutate(across(2:4, replace_na, 0)) ->
  pace_coefs

test_out %>%
  group_by(state) %>%
  mutate(n = n()) %>%
  filter(n > 100) %>%
  ggplot(aes(.pred, epm, color = state))+geom_point()+geom_abline(slope = 1)
  # head()

## Now we need a dataset of rate stats from "boxscores"
boxscore_q <- tbl(con, "boxscores") %>%
  mutate(extra_kill_lives = )
  select(match_id, player_id = name, kills, deaths, snd_firstbloods, snd_firstdeaths) %>%
  left_join(tbl(con, 'match_info') %>% select(match_id, duration_ms, rounds), by = "match_id")
  collect()

  
  
