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
  mutate(streak = 1:n()-1) %>%
  mutate(first_eng = as.factor(streak == 0),
         pdk = tsla < 5000 & streak == 0,
         pkk = streak > 0,
         # tsla = pkk * tsla + (10000*!pkk),
         pdk = replace_na(pdk, FALSE))  %>%
  ungroup() %>%
  mutate(
         win = factor(type == "attacker_id")
         ) %>%
  filter(!pdk) %>%
  select(match_id, event_id, pkk,tsla, win) ->
  kill_data

# split data so that p1 and p2 are evenly the killer
library(glmnet)
library(tidymodels)
library(ranger)
library(caret)

kill_data %>%
  group_by(match_id, event_id) %>%
  sample_n(size = 1) ->
  kill_sample_1

kill_data %>%
  left_join(kill_sample_1, by = c("match_id", "event_id")) %>%
  filter(win.x != win.y) ->
  kill_two_player_data

### Building up the ridge regression
kill_two_player_data %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  select(-win.y, -match_id, -event_id) ->
  filtered_kills

expected_split <- initial_split(filtered_kills)
expected_train <- training(expected_split)
expected_test <- testing(expected_split)

# make recipe
expected_rec <- recipe(win.x ~ 0 + ., data = expected_train) %>%
  step_interact(terms = ~ pkk.x:pkk.y) %>%
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

# ## Build to eng win% model
# set.seed(5)
# # data_boot <- bootstraps(pp_train)
# data_cv <- vfold_cv(pp_train)

lr_model <- 
  # specify that the model is a random forest
  logistic_reg(penalty = 1E-9, mixture = 0) %>%
  # select the engine/package that underlies the model
  set_engine("glm") %>%
  # choose either the continuous regression or binary classification mode
  set_mode("classification")

# rf_model <- 
#   # specify that the model is a random forest
#   rand_forest() %>%
#   # specify that the `mtry` parameter needs to be tuned
#   set_args(trees = 100, mtry = 2 , min_n = 5) %>%
#   # select the engine/package that underlies the model
#   set_engine("ranger") %>%
#   # choose either the continuous regression or binary classification mode
#   set_mode("classification") 


wf <- workflow() %>%
  add_model(lr_model) %>%
  add_recipe(expected_rec)

fit_model <- 
  wf %>%
  fit(data = pp_train) 

fit_model %>%
  tidy()


predict(fit_model, pp_test, type = "prob") %>%
  bind_cols(pp_test) %>%
  arrange(desc(.pred_TRUE))
  mn_log_loss(win.x, .pred_TRUE)

predict(fit_model, pp_test, type = "prob") %>%
  bind_cols(pp_test) %>%
  ggplot(aes(.pred_TRUE, fill = win.x))+
  geom_density(alpha = 0.5)+
  lims(x = c(0,1))

# generate predictions from the test set
test_predictions <- starter %>% collect_predictions()
test_predictions %>% 
  conf_mat(truth = win, estimate = .pred_class)
test_predictions %>%
  select(.pred_TRUE) %>%
  bind_cols(pp_test) %>%
  ggplot() +
  geom_density(aes(x = .pred_TRUE, fill = win), 
               alpha = 0.5)+
  facet_wrap(~adv)

test_predictions %>%
  select(.pred_TRUE) %>%
  bind_cols(pp_test) %>%
  arrange(desc(.pred_TRUE)) %>%
  ggplot() +
  geom_point(aes(y = .pred_TRUE, x = tsla), 
               alpha = 0.5)+
  facet_wrap(~adv)

expand.grid(
  adv = factor(c(-2,-1,0,1,2)),
  streak = factor(c(0,1)),
  ttl = seq(0, 5000, 100)
  ) ->
  test_data

predict(final_model,expected_rec %>%
          bake(test_data))

### Save a model for xkills
parsed <- tidypredict::parse_model(final_model)
