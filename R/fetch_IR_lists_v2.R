# Scrape MLB injury list, semi_join with rosters and produce a table of injuries.

library(tidyverse)
library(rvest)
library(janitor)
library(stringi)
library(httr)

# Cached injury file
injury_cache_file <- "data/injured_on_list.rds"

if (!dir.exists("data")) {
  dir.create("data")
}

clean_player_name <- function(x) {
  x %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^a-z ]", "") %>%
    str_squish()
}

injured_on_list <- tryCatch({
  
  url <- "https://www.espn.com/mlb/injuries"
  
  resp <- httr::RETRY(
    verb = "GET",
    url = url,
    httr::user_agent("Mozilla/5.0"),
    times = 12,
    pause_base = 30,
    pause_cap = 300
  )
  
  page <- read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
  
  injured_table <- page %>%
    html_elements(".ResponsiveTable") %>%
    map_dfr(function(x) {
      team <- x %>% html_element(".Table__Title") %>% html_text2()
      
      tbl <- x %>%
        html_element("table") %>%
        html_table(fill = TRUE) %>%
        clean_names()
      
      tbl %>% mutate(team = team, .before = 1)
    }) %>%
    rename(player = name) %>%
    mutate(player_key = clean_player_name(player))
  
  the_list_clean <- pitcher_roster %>%
    select(Owner, Name, Position) %>%
    bind_rows(
      batter_roster %>% select(Owner, Name, Position)
    ) %>%
    mutate(player_key = clean_player_name(Name))
  
  injured_table %>%
    inner_join(the_list_clean, by = "player_key") %>%
    filter(status != "Day-To-Day") %>%
    select(Name, Owner, team, pos, Position, status, est_return_date, comment) %>%
    rename(
      Player = Name,
      Team = team,
      `Roster Position` = Position,
      Position = pos,
      Status = status,
      `Estimated Return` = est_return_date,
      Details = comment
    ) %>%
    arrange(Owner)
  
}, 

error = function(e) {
  message("Could not fetch ESPN injury table: ", e$message)
  
  if (file.exists(injury_cache_file)) {
    message("Using cached injury table: ", injury_cache_file)
    readRDS(injury_cache_file)
  } else {
    message("No injury cache found. Returning empty injury table.")
    
    tibble(
      Player = character(),
      Owner = character(),
      Team = character(),
      Position = character(),
      `Roster Position` = character(),
      Status = character(),
      `Estimated Return` = character(),
      Details = character()
    )
  }
})

if (nrow(injured_on_list) > 0) {
  saveRDS(
  list(
    timestamp = Sys.time(),
    injuries = injured_on_list
  ),
  injury_cache_file
)
  message("Saved injury cache: ", injury_cache_file)
}