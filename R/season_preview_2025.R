# 2026 Season Draft Guide


library(tidyverse)
library(baseballr)
library(lubridate)
library(kableExtra)
library(purrr)
library(DT)

# Pull 2025 Batters and Pitchers from bRef - summary stats for a time window
# 2025 Stathead pulls are not working right now
# Update 3/5 - it's working

# YEAR-MONTH-DAY

allBatters <- bref_daily_batter(t1 = "2024-03-01", t2 = "2024-10-01")

allPitchers <- bref_daily_pitcher(t1 = "2024-03-01", t2 = "2024-10-01")

# Sort the top 100 pitchers and top 100 batters

# What if we use the MLB scraper for 2025

pitchers <- mlb_stats(stat_type = 'season', player_pool = "all", stat_group = 'pitching', season = 2025)

hitters <- mlb_stats(stat_type = 'season', player_pool = "all", stat_group = 'hitting', season = 2025)

pitchers <- pitchers %>%
  filter(position_type == "Pitcher",
         as.numeric(games_started) > 5) %>%
  select(position_abbreviation, player_full_name, team_name, age, strike_outs, era, whip, innings_pitched, games_started) %>%
  arrange(as.numeric(whip)) %>%
  slice(1:200)

hitters <- hitters %>%
  filter(position_type != "Pitcher",
         position_abbreviation != "X",
         games_played > 50) %>%
  select(position_abbreviation, player_full_name, team_name, age, rbi, ops, avg, home_runs, stolen_bases, games_played) %>%
  group_by(position_abbreviation) %>%
  arrange(-as.numeric(ops)) %>%
  slice(1:50)
