# Melville St. Fantasy League

library(tidyverse)
library(baseballr)
library(lubridate)
library(kableExtra)
library(purrr)
library(DT)

# Pull Batters and Pitchers from bRef - summary stats for a time window

# YEAR-MONTH-DAY

allBatters <- bref_daily_batter(t1 = "2025-03-01", t2 = Sys.Date())

allPitchers <- bref_daily_pitcher(t1 = "2025-03-01", t2 = Sys.Date())

# Initialize Rosters

rosters <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTLS6FHq2re1Xk31L9jMfWUNWWcLx7d-q1p2oPhec5mcUX05elElSH1Frwi86qk3KoppCFkgM4g36cw/pub?gid=0&single=true&output=csv")

# Consolidate rosters by owner, position, roster status

# Batters

battersList <- rosters %>%
  filter(batter_pitcher == "Batter")

# Pitchers

pitchersList <- rosters %>%
  filter(batter_pitcher == "Pitcher")

# Filter batter list for drafted players


batter_roster <- left_join(battersList, allBatters, by = c("Name" = "Name"))

stats_batting <- batter_roster %>%
  filter(Status == "Starter") %>%
  group_by(Owner) %>%
  summarize(H = sum(H, na.rm = TRUE),
            HR = sum(HR, na.rm = TRUE),
            BA = sum(H, na.rm = TRUE)/sum(AB, na.rm = TRUE),
            RBI = sum(RBI, na.rm = TRUE),
            SB = sum(SB, na.rm = TRUE),
            OPS = (
                      (sum(H, na.rm = TRUE) + 
                      sum(BB, na.rm = TRUE)+ 
                      sum(HBP, na.rm = TRUE)) / 
                     sum(PA, na.rm = TRUE)
                     )+
              (
                sum(X1B, na.rm = TRUE) + 
                 (2*sum(X2B, na.rm = TRUE)) + 
                 (3*sum(X3B, na.rm = TRUE))+
                 (4*sum(HR, na.rm = TRUE)
                  )
                ) /
              sum(AB, na.rm = TRUE)
            ) %>%
  mutate(BA = signif(BA, 3),
         OPS = signif(OPS, 3)) %>%
  select(-H)

# Filter pitcher list for drafted players

pitcher_roster <- left_join(pitchersList, allPitchers, by = c("Name" = "Name"))

stats_pitching <- pitcher_roster %>%
  filter(Status == "Starter") %>%
  group_by(Owner) %>%
  summarize(IP = sum(IP, na.rm = TRUE),
            SO = sum(SO, na.rm = TRUE),
            ERA = (sum(ER, na.rm = TRUE)/sum(IP, na.rm = TRUE))*9,
            WHIP = (sum(BB, na.rm = TRUE)+sum(HBP, na.rm = TRUE)+sum(H, na.rm = TRUE))/sum(IP, na.rm = TRUE)
  ) %>%
  mutate(ERA = signif(ERA, 3),
         WHIP = signif(WHIP, 4))

# Put the columns together into long form standings in order to calculate leader

stats_batting_points <- stats_batting %>%
  pivot_longer(cols = -Owner, names_to = "Stat", values_to = "value") %>%
  group_by(Stat) %>%
  arrange(value) %>%
  mutate(points = row_number())

# Pitching stats - ERA and WHIP - more points for lower, all else, more points for higher (e.g. Strikeouts)

stats_pitching_points <- stats_pitching %>%
  pivot_longer(cols = -Owner, names_to = "Stat", values_to = "value") %>%
  filter(Stat %in% c("ERA", "WHIP")) %>%
  group_by(Stat) %>%
  arrange(-value) %>%
  mutate(points = row_number()) %>%
  rbind(., stats_pitching %>%
  pivot_longer(cols = -Owner, names_to = "Stat", values_to = "value") %>%
  filter(!Stat %in% c("ERA", "WHIP")) %>%
  group_by(Stat) %>%
  arrange(value) %>%
  mutate(points = row_number()))

# all stats

all_stats <- rbind(stats_batting_points, stats_pitching_points)

# Create a points table

points_table <- all_stats %>%
  group_by(Owner) %>%
  summarize(Points = sum(points))


# Function to bold the appropriate value in row-based standings in kable
bold_stat <- function(x, col) {
  if (col %in% c("ERA", "WHIP")) {
    ifelse(x == min(x), cell_spec(x, bold = TRUE), x) # Bold lower value
  } else {
    ifelse(x == max(x), cell_spec(x, bold = TRUE), x) # Bold higher value
  }
}

