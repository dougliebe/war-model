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
boxscores <- tbl(con, "boxscores") %>%
  left_join(match_info %>% select(match_id, mode, duration_ms), by = "match_id") %>%
  filter(mode == "Hardpoint") %>%
  group_by(match_id, team) %>%
  summarise(adv = sum(kills)-sum(deaths),
            eng = sum(kills)+sum(deaths)) %>%
  left_join(tbl(con, 'match_scores') %>% 
              select(match_id, team = name, is_victor),
            by = c("match_id", 'team')) %>%
  collect()
  

boxscores %>%
  mutate(bin = cut(adv, breaks = seq(-10, 10))) %>%
  group_by(bin) %>%
  summarise(win_pct = mean(is_victor == TRUE),
            n = n()) %>%
  ggplot(aes(bin, win_pct))+geom_col()

m1 <- glm((is_victor==TRUE)*1 ~ adv, data = boxscores, family = binomial())

summary(m1)
