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


# Look at kills on team-level
# how much time do they play at each advantage?



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

## now label each event by the teams
events_data %>%
  mutate(time_ms = as.numeric(time_ms)) %>%
  group_by(match_id) %>%
  mutate(event_id = 1:n(),
         event_len = time_ms - lag(time_ms, default = 0)) %>%
  pivot_longer(c(id, attacker_id),
               names_to = "type",
               values_to = "player") %>%
  left_join(roster_data, by = c("match_id", "player" = 'player_id'))->
  pbp_long

## Combine pbp_long with adv at times
pbp_long %>%
  left_join(state_by_events, by = c("match_id", "event_id")) %>%
  mutate(adv = as.numeric(as.character(adv)),
         adv = ifelse(type == "id", -adv, adv)) %>%
  group_by(team, .add = T) %>%
  mutate(previous_adv = lag(adv, default = 0)) ->
  adv_with_team

adv_with_team %>%
  group_by(match_id, team, adv) %>%
  summarise(time = sum(event_len, na.rm = T)) %>%
  pivot_wider(c(match_id, team), names_from = 'adv', values_from = 'time') ->
  games_by_time

## Find some games to look at
tbl(con, "boxscores") %>%
  group_by(match_id, team) %>%
  summarise(engs = sum(kills, na.rm = TRUE)+sum(deaths, na.rm = TRUE),
            pm = sum(kills, na.rm = TRUE)-sum(deaths, na.rm = TRUE)) %>%
  left_join(tbl(con, "match_scores") %>%
              select(match_id, team = name, score, is_victor),
            by = c("match_id", 'team')) %>%
  collect() ->
  team_pm

games_by_time %>%
  left_join(team_pm, by = c("match_id", "team")) ->
  games_time_score

games_time_score %>%
  ungroup() %>%
  filter(pm == 22) %>%
  arrange(pm) %>%
  slice(1) %>% pull(match_id)

match_interest <- "da2b9833-5556-5dd4-8602-ef5002b27a87"
match_interest <- "11361229-faf6-5b60-8804-8a74587c53b3"
games_by_time %>%
  filter(match_id == match_interest) %>%
  pivot_longer(cols = c(-match_id, -team),
               names_to = 'adv',
               values_to = 'time') %>%
  mutate(adv = as.numeric(adv)) %>%
  filter(!is.na(time), adv >0) %>%
  ggplot(aes(adv, time/1000, fill = team))+geom_col(position = 'dodge')+
  labs(x = "Man-Advantage", y = "Time (s)")+
  scale_fill_manual(values = c("OPTIC GAMING" = "#9DC73B", "SPLYCE" = 'black'))+
  scale_x_continuous(breaks = c(1,2,3), labels = c("+1", "+2", "+3"))+
  ggtitle("OG outslays +22, loses 235-250", "Time at each advantage") +
  theme_bw() +
  theme(panel.border = element_rect(color = '#3d1866', fill = NA),
        axis.text = element_text(face = 'bold', color = '#3d1866', size = 16),
        axis.title = element_text(face = 'bold', color = '#3d1866', size = 17),
        legend.text = element_text(face = 'bold', color = '#3d1866', size = 12),
        legend.title = element_text(face = 'bold', color = '#3d1866', size = 14),
        plot.title = element_text(face = 'bold', color = '#3d1866', size = 17),
        plot.subtitle = element_text( color = '#3d1866', size = 12),
        text = element_text(family = 'mono'))

match_info %>%
  filter(match_id == match_interest)
tbl(con, "match_scores")  %>%
  filter(match_id == match_interest)

adv_with_team %>%
  filter(team == "OPTIC GAMING", match_id == match_interest) %>%
  ggplot(aes(time_ms/1000, cumsum(adv)))+geom_step()
