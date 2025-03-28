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

positionBatters <- c("C", "1B", "2B", "SS", "3B", 
              "OF1", "OF2", "OF3", "Util/DH", 
              "C_Bench", "IF_Bench", "OF_Bench")

positionPitchers <- c("P1", "P2", "P3",  "P4", "P_Bench")

statusBatters <- c("Starter", "Starter", "Starter", "Starter", "Starter",
             "Starter", "Starter", "Starter", "Starter",
             "Bench", "Bench", "Bench")

statusPitchers <- c("Starter", "Starter", "Starter", "Starter", "Bench")

# List Rosters - In order of positions listed in positionBatters and positionPitchers
# Player names are case and character sensitive - compare them with the lists pulled in `allBatters` and `allPitchers`

battersList_Isaac <- c("J.T. Realmuto", "Freddie Freeman", "Mookie Betts", "Elly De La Cruz", "José Ramírez",
                  "Fernando Tatis Jr.", "Jarren Duran", "Steven Kwan", "Shohei Ohtani",
                  "Salvador Perez", "Xander Bogaerts", "Ronald Acuña Jr.")

battersList_Dad <- c("Adley Rutschman", "Vladimir Guerrero Jr.", "Marcus Semien", "Bobby Witt Jr.", "Alec Bohm",
                "Aaron Judge", "Juan Soto", "Yordan Alvarez", "Pete Alonso",
                "Will Smith", "Bryce Harper", "Kyle Tucker")

pitchersList_Isaac <- c("Paul Skenes", "Yoshinobu Yamamoto", "Sandy Alcántara", "Aaron Nola", "Corbin Burnes")

pitchersList_Dad <- c("Zack Wheeler", "Tanner Houck", "Tarik Skubal", "Chris Sale", "Gerrit Cole")

# Consolidate rosters by owner, position, roster status

# Batters

battersList <- cbind(positionBatters, battersList_Isaac, statusBatters) %>%
  as.data.frame() %>%
  rename(Name = battersList_Isaac) %>%
  mutate(Owner = "Isaac") %>%
  rbind(., cbind(positionBatters, battersList_Dad, statusBatters) %>%
          as.data.frame() %>%
          rename(Name = battersList_Dad) %>%
          mutate(Owner = "Dad"))

# Pitchers

pitchersList <- cbind(positionPitchers, pitchersList_Isaac, statusPitchers) %>%
  as.data.frame() %>%
  rename(Name = pitchersList_Isaac) %>%
  mutate(Owner = "Isaac") %>%
  rbind(., cbind(positionPitchers, pitchersList_Dad, statusPitchers) %>%
          as.data.frame() %>%
          rename(Name = pitchersList_Dad) %>%
          mutate(Owner = "Dad"))

# Filter batter list for drafted players

batter_roster <- left_join(battersList, allBatters, by = c("Name" = "Name"))

stats_batting <- batter_roster %>%
  filter(statusBatters == "Starter") %>%
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
         OPS = signif(OPS, 3))

# Filter pitcher list for drafted players

pitcher_roster <- left_join(pitchersList, allPitchers, by = c("Name" = "Name"))

stats_pitching <- pitcher_roster %>%
  filter(statusPitchers == "Starter") %>%
  group_by(Owner) %>%
  summarize(IP = sum(IP, na.rm = TRUE),
            SO = sum(SO, na.rm = TRUE),
            ERA = (sum(ER, na.rm = TRUE)/sum(IP, na.rm = TRUE))*9,
            WHIP = (sum(BB, na.rm = TRUE)+sum(HBP, na.rm = TRUE)+sum(H, na.rm = TRUE))/sum(IP, na.rm = TRUE)
  ) %>%
  mutate(ERA = signif(ERA, 3),
         WHIP = signif(WHIP, 4))

# Put the columns together into long form standings in order to calculate leader

points_table <- stats_batting %>%
  pivot_longer(cols = -Owner, names_to = "Stat", values_to = "value") %>%
  pivot_wider(names_from = Owner, values_from = value) %>%
rbind(.,
stats_pitching %>%
  pivot_longer(cols = -Owner, names_to = "Stat", values_to = "value") %>%
  pivot_wider(names_from = Owner, values_from = value)
) %>%
  mutate(Leader = case_when(Dad > Isaac & Stat %in% c("ERA", "WHIP") ~ "Isaac",
                            Dad < Isaac & Stat %in% c("ERA", "WHIP") ~ "Dad",
                            Dad > Isaac & !Stat %in% c("ERA", "WHIP") ~ "Dad" ,
                            Dad < Isaac & !Stat %in% c("ERA", "WHIP")~ "Isaac",
                            Dad == Isaac ~ "Tie")) %>%
  group_by(Leader) %>%
  tally() %>%
  rename(Points = n,
         Owner = Leader)

# Function to bold the appropriate value in row-based standings in kable
bold_stat <- function(x, col) {
  if (col %in% c("ERA", "WHIP")) {
    ifelse(x == min(x), cell_spec(x, bold = TRUE), x) # Bold lower value
  } else {
    ifelse(x == max(x), cell_spec(x, bold = TRUE), x) # Bold higher value
  }
}

