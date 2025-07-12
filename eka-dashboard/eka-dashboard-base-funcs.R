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
boxscore_q <- tbl(con, "boxscores")

## create a table of stats for each player in HP
boxscore_q %>%
  inner_join(match_info, by = 'match_id') %>%
  group_by(name) %>%
  summarise(kd = sum(kills)/sum(deaths),
            time_s = sum(duration_ms/1000),
            kpm = sum(kills)/sum(duration_ms/1000)*60,
            epm = sum(kills+deaths)/sum(duration_ms/1000)*60
            ) %>%
  filter(time_s > 100*60) %>%
  collect() %>%
  pivot_longer(-name,
               names_to = 'stat',
               values_to = 'value') %>%
  group_by(stat) %>%
  mutate(percentile = percent_rank(value)) ->
  boxscore_tbl


## get kill trade success, death trade success,
## % of kill trades att, % of death trades att
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

## this gives us everything we need about each engagement
events_data %>%
  mutate(time_ms = as.numeric(time_ms)) %>%
  group_by(match_id) %>%
  mutate(event_id = 1:n()) %>%
  left_join(trade_kills %>%
              arrange(match_id, event_id) %>%
              filter(in_trade_eng) %>%
              select(match_id, last_kill_id,other_player,
                     trade_att = in_trade_eng, trade_win = traded),
            by = c("match_id", 'event_id' = 'last_kill_id'))  ->
  was_traded

## now we can get counts of how often each situation happens,
## combine that with boxscore data on time played to get rates
was_traded %>%
  pivot_longer(c(id, attacker_id, other_player),
               names_to = 'type',
               values_to = 'player_id') %>%
  group_by(player_id, type, trade_att, trade_win) %>%
  count() ->
  trade_counts

trade_counts %>%
  replace_na(list(trade_att = FALSE, trade_win = FALSE)) %>%
  group_by(player_id) %>%
  summarise(
    kill_pct = sum(n*(type == "attacker_id"))/sum(n*(type %in% c("attacker_id", 'id'))),
    death_pct = 1 - kill_pct,
    trade_att_kill_pct = sum(n*(type == "attacker_id" & trade_att))/sum(n*(type == "attacker_id")),
    no_trade_att_kill_pct = 1 - trade_att_kill_pct,
    traded_kill_pct = sum(n*(type == "attacker_id" & trade_att & trade_win))/sum(n*(type == "attacker_id")),
    two_piece_pct = sum(n*(type == "attacker_id" & trade_att & !trade_win))/sum(n*(type == "attacker_id")),
    trade_att_death_pct = sum(n*(type == "id" & trade_att))/sum(n*(type == "id")),
    no_trade_att_death_pct = 1 - trade_att_death_pct,
    traded_death_pct = sum(n*(type == "id" & trade_att & trade_win))/sum(n*(type == "id")),
    failed_trade_death_pct = sum(n*(type == "id" & trade_att & !trade_win))/sum(n*(type == "id")),
  #   kills_untraded_pct = 1 - (sum(n*(type == "attacker_id")*trade_att)/sum(n*(type == "attacker_id"))),
  #   deaths_traded_pct = sum(n*(type == "id")*trade_att)/sum(n*(type == "id")),
  #   got_two_pct = sum(n*(type == "attacker_id")*trade_att*!trade_win)/
  #     sum(n*(type == "attacker_id")*trade_att),
  #   death_traded = sum(n*(type == "id")*trade_att*trade_win)/
  #     sum(n*(type == "id")*trade_att),
    n = sum(n)
  ) %>%
  filter(n > 200) %>%
  pivot_longer(-player_id,
               names_to = 'stat') %>%
  group_by(stat) %>%
  mutate(percentile = percent_rank(value)) ->
  player_trade_tbl

## 

