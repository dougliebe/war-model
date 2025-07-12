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





### To get expected +/-
## For each engagement, for each player, there should be:
# win eng?
# traded?
# team win trade?

### We know set things about each player:
# 1. Beta_1 = chance to win eng
# 2. B_2 = chance to get into trade | kill
# 3. B_3 = chance teammate can get into trade | death
# 4. B4 = chance to win trade | kill
# 5. B5 = chance teammate wins trade | death


## First, look at each engagement alone, define vars about state ####
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
  count()

## put all betas together
ind_coefs <- read_csv(here::here("coefs","shooting_skill_coefs.csv")) %>%
  select(player_id = name, beta_1 = estimate) %>%
  mutate(beta_1 = exp(beta_1+-3.88 + 0.5*7.64))
# since trades are predicted with killer, die-er, and deathteam
# we need to add average coefs for the parts of the equation not included in beta
# i.e. for trade likelihood beta = player beta + intercept + avg(P2) + avg(P3)
trade_coefs <- read_csv(here::here('coefs', 'eka-trade-coefs.csv')) %>%
  separate(term, into = c("player_id", "type"), sep = "_") %>%
  pivot_wider(id_cols = player_id, names_from = type, values_from = estimate) %>%
  select(player_id, beta_2 = KILLER, beta_3 = DEAD) %>%
  mutate(beta_2 = exp(log(beta_2)+log(0.909)+log(1.13) +log(0.828)),
         beta_3 = exp(log(beta_3)+log(0.909)+log(1.13) +log(1.4))
         )
trade_kill_coefs <- read_csv(here::here('coefs', "eka-kill-coefs.csv")) %>%
  pivot_wider(id_cols = player, names_from = type, values_from = estimate) %>%
  select(player_id = player, beta_4 = P1, beta_5 = P2) %>%
  mutate(beta_4 = exp(log(beta_4)+log(1.68)+log(1.17)+log(1.01)),
         beta_5 = exp(log(beta_5)+log(1.68)+log(0.91)+log(1.01))
  )

all_coefs <- ind_coefs %>%
  left_join(trade_coefs, by = "player_id") %>%
  left_join(trade_kill_coefs, by = 'player_id')

# all_coefs %>%
#   mutate(p_kill = beta_1/(1+beta_1),
#          p_death = 1- p_kill,
#          p_trade_att_kill = beta_2/(1+beta_2),
#          p_no_trade_att_kill = 1 - p_trade_att_kill,
#          # chance of two-piece = coef * base_2piece_rate * trade att %
#          p_traded_kill = (beta_4/(1+beta_4) * p_trade_att_kill) * p_kill,
#          p_two_piece = ((1- (beta_4/(1+beta_4) )) * p_trade_att_kill) * p_kill,
#          p_trade_att_death = beta_3/(1+beta_3),
#          p_no_trade_att_death = 1 - p_trade_att_death,
#          p_fail_trade = (beta_5/(1+beta_5) * p_trade_att_death) * p_death,
#          p_traded_death = ((1- (beta_5/(1+beta_5))) * p_trade_att_death) * p_death,
#   ) ->
#   update_coefs

## assuming P(kill / Death) = 1 for events after engagement
all_coefs %>%
  mutate(p_kill = beta_1/(1+beta_1),
         p_death = 1- p_kill,
         p_trade_att_kill = beta_2/(1+beta_2),
         p_no_trade_att_kill = 1 - p_trade_att_kill,
         # chance of two-piece = coef * base_2piece_rate * trade att %
         p_traded_kill = (beta_4/(1+beta_4) * p_trade_att_kill) * 1,
         p_two_piece = ((1- (beta_4/(1+beta_4) )) * p_trade_att_kill) * 1,
         p_trade_att_death = beta_3/(1+beta_3),
         p_no_trade_att_death = 1 - p_trade_att_death,
         p_fail_trade = (beta_5/(1+beta_5) * p_trade_att_death) * 1,
         p_traded_death = ((1- (beta_5/(1+beta_5))) * p_trade_att_death) * 1,
  ) ->
  update_coefs


update_coefs %>%
  mutate(exp_eng = 
           p_trade_att_kill * 1 +
           p_two_piece * 2 - 
           p_trade_att_death - 
           p_fail_trade * 2) %>%
  select(player_id, exp_eng) ->
  est_kill_add

est_kill_add %>%
  left_join(pbp_long %>%
              group_by(player) %>%
              count(), by = c("player_id" = "player")) %>%
  mutate(eka = exp_eng * n) %>%
  left_join(tbl(con, "boxscores") %>%
              left_join(match_info, by = 'match_id') %>%
              filter(mode == "Hardpoint") %>%
              group_by(name) %>%
              summarise(time_s = sum(duration_ms/1000)) %>%
              collect(),
            by = c("player_id"='name')) %>%
  mutate(kapm = eka/time_s*60) %>%
  arrange((kapm)) %>%
  ggplot(aes(kapm))+geom_histogram()+
  geom_vline(xintercept = est_kill_add %>%
               left_join(pbp_long %>%
                           group_by(player) %>%
                           count(), by = c("player_id" = "player")) %>%
               mutate(eka = exp_eng * n) %>%
               left_join(tbl(con, "boxscores") %>%
                           left_join(match_info, by = 'match_id') %>%
                           filter(mode == "Hardpoint") %>%
                           group_by(name) %>%
                           summarise(time_s = sum(duration_ms/1000)) %>%
                           collect(),
                         by = c("player_id"='name')) %>%
               mutate(kapm = eka/time_s*60) %>%
               ungroup() %>%
               summarise(m = mean(kapm, na.rm = T)) %>%
               pull(m))


