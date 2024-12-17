# data - womens tennis - wta

library("tidyverse")
library("janitor")
library("here")
library("countrycode")
library("tidymodels")

w_path_to_file <- here("data/wta")

# load matches

w_files_matches <- fs::dir_ls(path = w_path_to_file, glob = "*wta_matches_*csv")

w_pre_matches <- read_csv(w_files_matches, id = "path") %>% select(contains("tourney"), contains("seed"), contains("id"), winner_name, loser_name,
                                                               winner_rank, loser_rank, round, score) %>%
  filter(tourney_level == "G") %>%
  mutate(date = ymd(tourney_date)) %>%
  select(-tourney_date, -tourney_level) %>%
  filter(date > ymd(19680301)) %>%
  relocate(date, .after = tourney_name) %>%
  relocate(tourney_id, .after = score) %>%
  mutate(
    tourney_id = case_when(
      str_detect(tourney_id, "FRA") ~ str_c(str_extract(tourney_id, "[0-9]{4}"), "-520"),
      str_detect(tourney_id, "GBR") ~ str_c(str_extract(tourney_id, "[0-9]{4}"), "-540"),
      str_detect(tourney_id, "AUS") & date == ymd(19770103) ~ "1977-580",
      str_detect(tourney_id, "AUS") & date == ymd(19771219) ~ "1977-581",
      str_detect(tourney_id, "AUS") ~ str_c(str_extract(tourney_id, "[0-9]{4}"), "-580"),
      str_detect(tourney_id, "USA") ~ str_c(str_extract(tourney_id, "[0-9]{4}"), "-560"),
      .default = tourney_id))

# load player info

w_players_info <- read_csv(here("data/wta/wta_players.csv")) %>%
  mutate(player = str_c(name_first, " ", name_last)) %>%
  select(-contains("name"), -wikidata_id)

# load rankings info from Sackmann

w_files_rankings <- fs::dir_ls(path = w_path_to_file, glob = "*wta_rankings_*csv")

w_rankings <- read_csv(w_files_rankings, id = "path") %>%
  rename(player_id = player) %>%
  mutate(date = ymd(ranking_date)) %>%
  select(-path, -points, -ranking_date) %>%
  left_join(w_players_info, by = join_by(player_id)) %>%
  select(rank, date, player, player_id)

# matches with missing rank info

w_pre_missing <- w_pre_matches %>%
  filter(is.na(winner_rank)) %>%
  bind_rows(w_pre_matches %>% filter(is.na(loser_rank))) %>%
  distinct()

# add ranking to matches

w_matches <- w_pre_matches %>%
  left_join(w_rankings %>% select(-date), by = join_by(winner_name == player, tourney_id), relationship = "many-to-many") %>%
  mutate(
    winner_rank = case_when(
      !is.na(winner_seed) ~ winner_seed,
      !is.na(winner_rank) ~ winner_rank,
      is.na(rank) ~ elo_rank,
      .default = rank)) %>%
  select(-rank, -elo_rank) %>%
  left_join(rankings_update %>% select(-date), by = join_by(loser_name == player, tourney_id), relationship = "many-to-many") %>%
  left_join(elo_rankings %>% select(-date), by = join_by(loser_name == player, tourney_id), relationship = "many-to-many") %>%
  mutate(
    loser_rank = case_when(
      !is.na(loser_seed) ~ loser_seed,
      !is.na(loser_rank) ~ loser_rank,
      is.na(rank) ~ elo_rank,
      .default = rank),
    tourney_name = case_when(
      tourney_name == "Us Open" ~ "US Open",
      tourney_name == "Australian Open-2" ~ "Australian Open",
      .default = tourney_name)) %>%
  select(-rank, -elo_rank) %>%
  select(-contains("seed")) %>%
  mutate(round = factor(round, levels = c("R128", "R64", "R32", "R16", "QF", "SF", "F"))) %>% 
  arrange(date, round) %>%
  distinct()

