# Scrape MLB injury list, semi_join with rosters and produce a table of injuries.

library(tidyverse)
library(rvest)
library(janitor)
library(stringi)

url <- "https://www.espn.com/mlb/injuries"


page <- read_html(url)

# Extract each ESPN responsive table block
injured_table <- page %>%
  html_elements(".ResponsiveTable") %>%
  map_dfr(function(x) {
    
    team <- x %>%
      html_element(".Table__Title") %>%
      html_text2()
    
    tbl <- x %>%
      html_element("table") %>%
      html_table(fill = TRUE) %>%
      clean_names()
    
    tbl %>%
      mutate(team = team, .before = 1)
  }) %>%
  rename(player = name) %>%
  mutate(
    player_key = player %>%
      str_to_lower() %>%
      stri_trans_general("Latin-ASCII") %>%
      str_replace_all("[^a-z ]", "") %>%
      str_squish()
  )


clean_player_name <- function(x) {
  x %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^a-z ]", "") %>%
    str_squish()
}

injured_table <- injured_table %>%
  mutate(player_key = clean_player_name(player))

the_list_clean <- pitcher_roster %>%
  select(Owner, Name, Position) %>%
  rbind(., batter_roster %>%
          select(Owner, Name, Position)) %>%
  mutate(player_key = clean_player_name(Name))

injured_on_list <- injured_table %>%
  inner_join(
    the_list_clean,
    by = "player_key"
  ) %>%
  filter(status != "Day-To-Day") %>%
  select(Name, Owner, team, pos, Position, status, est_return_date, comment) %>%
  rename('Player' = Name,
         Team = team,
         `Roster Position` = Position,
         `Position` = pos,
         Status = status,
         `Estimated Return` = est_return_date,
         Details = comment) %>%
  arrange(Owner)
