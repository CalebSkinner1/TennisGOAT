# Data Compilation

library("tidyverse")
library("janitor")
library("here")
library("countrycode")
library("tidymodels")

path_to_file <- here("data")

# load matches

files_matches <- fs::dir_ls(path = path_to_file, glob = "*atp_matches_*csv")

pre_matches <- read_csv(files_matches, id = "path") %>% select(contains("tourney"), contains("seed"), contains("id"), winner_name, loser_name,
                                                           winner_rank, loser_rank, round, score) %>%
  filter(tourney_level == "G") %>%
  mutate(date = ymd(tourney_date)) %>%
  select(-tourney_date, -tourney_level) %>%
  filter(date > ymd(19680301)) %>%
  relocate(date, .after = tourney_name) %>%
  relocate(tourney_id, .after = score)

# load player info

players_info <- read_csv(here("data/atp_players.csv")) %>%
  mutate(player = str_c(name_first, " ", name_last)) %>%
  select(-contains("name"), -wikidata_id)

# load rankings info from Sackmann

files_rankings <- fs::dir_ls(path = path_to_file, glob = "*atp_ranking_*csv")

rankings <- read_csv(files_rankings, id = "path") %>%
  rename(player_id = player) %>%
  mutate(date = ymd(ranking_date)) %>%
  select(-path, -points, -ranking_date) %>%
  left_join(players_info, by = join_by(player_id)) %>%
  select(rank, date, player, player_id)

# matches with missing rank info

pre_missing <- pre_matches %>%
  filter(is.na(winner_rank)) %>%
  bind_rows(pre_matches %>% filter(is.na(loser_rank))) %>%
  distinct()

# load rank info from ultimate tennis statistics

files_rankings2 <- fs::dir_ls(path = path_to_file, glob = "*atp_rankings*csv")

rankings2 <- read_csv(files_rankings2, id = "path") %>%
  select(rank, name, bestRankDate) %>%
  mutate(
    date = mdy(bestRankDate),
    date = if_else(date > ymd(20500101), date - period("100 years"), date)) %>%
  arrange(date, rank) %>%
  rename(player = "name") %>%
  select(-bestRankDate)

rankings_comb <- rankings %>% bind_rows(rankings2) %>%
  arrange(date, rank) %>%
  distinct() %>%
  select(-player_id)

# single ranking applied to multiple gs tournaments (French and Wimbledon)

rankings_first <- rankings_comb %>%
  filter(date == ymd(19730402) | date == ymd(19720327) | date == ymd(19710329) | date == ymd(19700330) | date == ymd(19690331) | date == ymd(19680401)) %>%
  mutate(temp = 2)

# link rank to corresponding tournament

rankings_update <- rankings_comb %>%
  mutate(
    temp = 1) %>%
  bind_rows(rankings_first) %>%
  mutate(
    tourney_id = case_when(
      date == ymd(19680101) ~ "1968-580",
      date == ymd(19680401) & temp == 1 ~ "1968-520",
      date == ymd(19680401) & temp == 2 ~ "1968-540",
      date == ymd(19680715) ~ "1968-560",
      
      date == ymd(19681230) ~ "1969-580",
      date == ymd(19690331) & temp == 1 ~ "1969-520",
      date == ymd(19690331) & temp == 2 ~ "1969-540",
      date == ymd(19690714) ~ "1969-560",
      
      date == ymd(19691229) ~ "1970-580",
      date == ymd(19700330) & temp == 1 ~ "1970-520",
      date == ymd(19700330) & temp == 2 ~ "1970-540",
      date == ymd(19700713) ~ "1970-560",
      
      date == ymd(19701228) ~ "1971-580",
      date == ymd(19710329) & temp == 1 ~ "1971-520",
      date == ymd(19710329) & temp == 2 ~ "1971-540",
      date == ymd(19710712) ~ "1971-560",
      
      date == ymd(19711227) ~ "1972-580",
      date == ymd(19720327) & temp == 1 ~ "1972-520",
      date == ymd(19720327) & temp == 2 ~ "1972-540",
      date == ymd(19720717) ~ "1972-560",
        
      date == ymd(19730402) & temp == 1 ~ "1973-520",
      date == ymd(19730402) & temp == 2 ~ "1973-540",
      
      date == ymd(19750630) ~ "1975-560",
      
      date == ymd(19760531) ~ "1976-520",
      date == ymd(19760621) ~ "1976-540",
      date == ymd(19760830) ~ "1976-560",
      
      date == ymd(19761231) ~ "1977-580",
      date == ymd(19770516) ~ "1977-520",
      date == ymd(19770619) ~ "1977-540",
      date == ymd(19770830) ~ "1977-560",
      date == ymd(19771212) ~ "1977-581",
      
      date == ymd(19780522) ~ "1978-520",
      
      date == ymd(19810518) ~ "1981-520",
      date == ymd(19810824) ~ "1981-560",
      
      date == ymd(19820517) ~ "1982-520",
      date == ymd(19820823) ~ "1982-560",
      date == ymd(19821129) ~ "1982-580",
      
      date == ymd(19830613) ~ "1983-540",
      date == ymd(19830822) ~ "1983-560",
      
      date == ymd(19840521) ~ "1984-520",
      .default = NA)) %>%
  select(-temp)

# load elo rankings from ultimate tennis statistics

files_elo <- fs::dir_ls(path = path_to_file, glob = "*elo*csv")

# link elo to tournament

elo_rankings <- read_csv(files_elo, id = "path") %>%
  select(rank, name, bestRankDate, country_name) %>%
  mutate(
    date = mdy(bestRankDate),
    date = if_else(date > ymd(20500101), date - period("100 years"), date)) %>%
  arrange(date, rank) %>%
  rename(
    player = "name",
    elo_rank = "rank") %>%
  select(-bestRankDate) %>%
  mutate(
    tourney_id = case_when(
      date == ymd(19680524) ~ "1968-520",
      date == ymd(19680624) ~ "1968-540",
      date == ymd(19680829) ~ "1968-560",
      date == ymd(19690120) ~ "1969-580",
      date == ymd(19690528) ~ "1969-520",
      date == ymd(19690621) ~ "1969-540",
      date == ymd(19690827) ~ "1969-560",
      date == ymd(19700117) ~ "1970-580",
      date == ymd(19700428) ~ "1970-520",
      date == ymd(19700622) ~ "1970-540",
      date == ymd(19700902) ~ "1970-560",
      date == ymd(19710228) ~ "1971-580",
      date == ymd(19710521) ~ "1971-520",
      date == ymd(19710621) ~ "1971-540",
      date == ymd(19710829) ~ "1971-560",
      date == ymd(19711220) ~ "1972-580",
      date == ymd(19720521) ~ "1972-520",
      date == ymd(19720624) ~ "1972-540",
      date == ymd(19720828) ~ "1972-560",
      date == ymd(19721220) ~ "1973-580",
      date == ymd(19730521) ~ "1973-520",
      date == ymd(19730624) ~ "1973-540",
      date == ymd(19731224) ~ "1974-580",
      date == ymd(19750825) ~ "1975-560",
      date == ymd(19760531) ~ "1976-520",
      date == ymd(19760621) ~ "1976-540",
      date == ymd(19760901) ~ "1976-560",
      date == ymd(19770523) ~ "1977-520",
      date == ymd(19770620) ~ "1977-540",
      date == ymd(19770830) ~ "1977-560",
      date == ymd(19771219) ~ "1977-581",
      date == ymd(19780529) ~ "1978-520",
      date == ymd(19780828) ~ "1978-560",
      date == ymd(19781219) ~ "1978-580",
      date == ymd(19790528) ~ "1979-520",
      date == ymd(19790625) ~ "1979-540",
      date == ymd(19790827) ~ "1979-560",
      date == ymd(19791224) ~ "1979-580",
      date == ymd(19800526) ~ "1980-520",
      date == ymd(19800623) ~ "1980-540",
      date == ymd(19800825) ~ "1980-560",
      date == ymd(19810525) ~ "1981-520",
      date == ymd(19810620) ~ "1981-540",
      date == ymd(19810824) ~ "1981-560",
      date == ymd(19811221) ~ "1981-580",
      date == ymd(19820524) ~ "1982-520",
      date == ymd(19820621) ~ "1982-540",
      date == ymd(19820823) ~ "1982-560",
      date == ymd(19821129) ~ "1982-580",
      date == ymd(19830523) ~ "1983-520",
      date == ymd(19830620) ~ "1983-540",
      date == ymd(19830822) ~ "1983-560",
      date == ymd(19831128) ~ "1983-580",
      date == ymd(19840528) ~ "1984-520",
      .default = NA),
    # fix discrepancy of two John Austin's
    remove = case_when(
      player == "John Austin" & country_name == "United States" & tourney_id != "1980-580" & tourney_id != "1979-580" ~ 1,
      player == "John Austin" & country_name == "Ireland" & tourney_id == "1979-580" ~ 1,
      .default = 0)) %>%
  filter(remove != 1) %>%
  select(-remove, -country_name)

# add ranking to matches

matches <- pre_matches %>%
  left_join(rankings_update %>% select(-date), by = join_by(winner_name == player, tourney_id), relationship = "many-to-many") %>%
  left_join(elo_rankings %>% select(-date), by = join_by(winner_name == player, tourney_id), relationship = "many-to-many") %>%
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

# show matches missing ranking

missing <- matches %>%
  filter(is.na(winner_rank)) %>%
  bind_rows(matches %>% filter(is.na(loser_rank))) %>%
  distinct()

missing %>% group_by(date, tourney_id) %>%
  summarize(count = n()) %>%
  filter(count > 25) %>%
  arrange(desc(count))

# create players csv

players_results0 <- matches %>%
  rename(
    "player_id" = winner_id,
    "opp_id" = loser_id,
    "name" = winner_name,
    "opp_name" = loser_name,
    "rank" = winner_rank,
    "opp_rank" = loser_rank) %>%
  mutate(result = "winner") %>%
  bind_rows(
    matches %>%
      rename(
        "player_id" = loser_id,
        "opp_id" = winner_id,
        "name" = loser_name,
        "opp_name" = winner_name,
        "rank" = loser_rank,
        "opp_rank" = winner_rank) %>%
      mutate(result = "loser"))

write_csv(players_results0, here("data/player_results.csv"))

# win rate model
win_rate_data <- players_results0 %>%
  filter(between(rank, 1, 100)) %>%
  group_by(rank) %>%
  summarize(
    wins = sum(result == "winner"),
    losses = sum(result == "loser")) %>%
  mutate(
    win_rate = wins/(wins + losses))

lm_spec <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

wr_rec <- recipe(win_rate ~ rank, data = win_rate_data) %>%
  step_mutate(rank2 = sqrt(rank))

wr_wf <- workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(wr_rec)

wr_fit <- wr_wf %>% fit(win_rate_data)

win_rate_data_update <- win_rate_data %>%
  mutate(
    rank2 = sqrt(rank),
    fitted_values = wr_fit$fit$fit$fit$fitted.values,
    win_points = (fitted_values - min(fitted_values))*99/(max(fitted_values) - min(fitted_values)) + 1,
    lose_points = -.5*rank)

win_rate_data_update %>%
  ggplot() +
  geom_point(aes(x = rank, y = win_rate), color = "indianred3") +
  geom_line(aes(x = rank, y = fitted_values), color = "cadetblue4")

# finish player results
  
players_results <- players_results0 %>%
  mutate(
    points = case_when(
      score == "W/O" ~ 0,
      is.na(opp_rank) & result == "winner" ~ 1,
      is.na(opp_rank) & result == "loser" ~ -50,
      result == "winner" & opp_rank < 100 ~ win_rate_data_update$win_points[opp_rank],
      result == "winner" & opp_rank >= 100 ~ 1,
      result == "loser" & opp_rank < 100 ~ win_rate_data_update$lose_points[opp_rank],
      result == "loser" & opp_rank >= 100 ~ -50,
      .default = NA),
    bonus = case_when(
      result == "winner" & round == "R16" ~ 10,
      result == "winner" & round == "QF" ~ 20,
      result == "winner" & round == "SF" ~ 30,
      result == "winner" & round == "F" ~ 40,
      .default = 0)) %>%
  mutate(round = factor(round, levels = c("R128", "R64", "R32", "R16", "QF", "SF", "F"))) %>% 
  arrange(date, round) %>%
  left_join(players_info, join_by(player_id, name == player)) %>%
  mutate(
    dob = ymd(dob),
    ioc = case_when(
      name == "Robert Lutz" ~ "USA",
      name == "V Kazarevitch" ~ "RUS",
      name == "Anthony Hammond" ~ "AUS",
      name == "Ove Nils Bengtson" ~ "SWE",
      name == "William Higgins" ~ "USA",
      name == "Franklin Robbins" ~ "USA",
      name == "Robert Mckinley" ~ "USA",
      name == "Michael Sprenglemeyer" ~ "USA",
      name == "Luis Garcia" ~ "MEX",
      name == "Henry Hank Irvine" ~ "RHO",
      name == "Robert Kreiss" ~ "USA",
      name == "Michael Phillips" ~ "AUS",
      name == "William Durham" ~ "AUS",
      name == "Charles Owens" ~ "USA",
      name == "Bernard Mitton" ~ "RSA",
      name == "Anthony Fawcett" ~ "RHO",
      name == "Michael Wayman" ~ "GBR",
      name == "Christophe Roger Vasselin" ~ "FRA",
      name == "Gene Stuart Malin" ~ "USA",
      name == "Edouard Roger Vasselin" ~ "FRA",
      .default = ioc),
    ioc = countrycode(ioc, origin = 'iso3c', destination = 'country.name',
                      custom_match = c('ALG' = 'Algeria',
                                       'BAH' = 'The Bahamas',
                                       'BAR' = "Barbados",
                                       'BUL' = 'Bulgaria',
                                       'CHI' = 'Chile',
                                       'CRC' = 'Costa Rica',
                                       'CRO' = 'Croatia',
                                       'DEN' = 'Denmark',
                                       'GER' = 'Germany',
                                       'GRE' = 'Greece',
                                       'HAI' = 'Haiti',
                                       'INA' = 'Indonesia',
                                       'IRI' = 'Iran',
                                       'LAT' = 'Latvia',
                                       'MON' = 'Monaco',
                                       'NED' = 'Netherlands',
                                       'PAR' = 'Paraguay',
                                       'NGR' = 'Nigeria',
                                       'PHI' = 'Philippines',
                                       'POR' = 'Portugal',
                                       'PUR' = 'Puerto Rico',
                                       'RSA' = 'South Africa',
                                       'SLO' = 'Slovenia',
                                       'SUI' = 'Switzerland',
                                       'TCH' = 'Czechia',
                                       'TPE' = 'Taiwan',
                                       'URS' = 'Soviet Union',
                                       'URU' = 'Uruguay',
                                       'YUG' = 'Yugoslavia',
                                       'ZIM' = 'Zimbabwe',
                                       'RHO' = 'Rhodesia')))

# write csvs

# write_csv(matches, here("data/gs_matches.csv"))
# 
# write_csv(players_results, here("data/gs_players.csv"))




