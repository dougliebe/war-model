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
match_info <- tbl(con, 'match_info') %>%
  collect

boxscore <- tbl(con, "boxscores") %>%
  collect()

output <- boxscore %>%
  left_join(match_info, by= 'match_id')
## Compare HP stats in WWII to BO4

output %>%
  mutate(title = case_when(
    lubridate::as_datetime(start_time_s) < "2017-10-01" ~ "IW",
    lubridate::as_datetime(start_time_s) < "2018-10-01" ~ "WWII",
    lubridate::as_datetime(start_time_s) < "2019-10-01" ~ "BO4"
  ),
  year_no = case_when(
    lubridate::as_datetime(start_time_s) < "2017-10-01" ~ 1,
    lubridate::as_datetime(start_time_s) < "2018-10-01" ~ 2,
    lubridate::as_datetime(start_time_s) < "2019-10-01" ~ 3
  )) ->
  title_data

title_data %>%
  mutate(player = tolower(name)) %>%
  group_by(year_no,mode, player) %>%
  summarise(kills = sum(kills),
            deaths = sum(deaths),
            rounds = sum(rounds),
            # score = sum(score),
            n = n()) %>%
  mutate(kd = kills/deaths,
         # kpr = kills/score,
         # win_pct = score/rounds,
         # score_pg = score/n
  ) %>%
  group_by(year_no) %>%
  summarise(sum(n))
  filter(n > 20) %>%
  select(year_no, player, kd, kpr, score_pg) %>%
  pivot_longer(c(-year_no,-player)) %>%
  group_by(player, name) %>%
  mutate(next_year = lead(value)) %>%
  # group_by(player, year_no) %>%
  # mutate(score_pg = sum(next_year*(name == "score_pg"))) %>%
  group_by(name) %>%
  summarise(cor = cor(value, next_year, use = "pairwise.complete"))
