# Calculations

library("tidyverse")
library("here")

matches <- read_csv(here("data/gs_matches.csv"))
players <- read_csv(here("data/gs_players.csv"))

# players page

players_sum <- players %>%
  group_by(name, dob, ioc, height, hand) %>%
  # group_by(name) %>%
  summarize(
    total_points = sum(points) + sum(bonus),
    victory_points = sum(points),
    bonus_points = sum(bonus),
    victories = sum(points > 0),
    defeats = sum(points < 0),
    win_perc = victories/(victories + defeats)) %>%
  rename(country = ioc) %>%
  arrange(desc(total_points)) %>%
  print(n = 150)

top10 <- players_sum %>% ungroup() %>% slice(1:10) %>% select(name) %>% pull()
top25 <- players_sum %>% ungroup() %>% slice(1:25) %>% select(name) %>% pull()
top50 <- players_sum %>% ungroup() %>% slice(1:50) %>% select(name) %>% pull()

# players by Grand Slam

players %>%
  group_by(name, dob, ioc, height, hand, tourney_name) %>%
  summarize(
    total_points = sum(points) + sum(bonus),
    victory_points = sum(points),
    bonus_points = sum(bonus),
    victories = sum(points > 0),
    defeats = sum(points < 0),
    win_perc = victories/(victories + defeats)
  ) %>%
  arrange(desc(total_points)) %>%
  print(n = 50)

# Grand Slams

players %>%
  group_by(name, dob, ioc, height, hand) %>%
  summarize(
    championships = sum(result == "winner" & round == "F"),
    finals = sum(round == "F"),
    semifinals = sum(round == "SF"),
    quarterfinals = sum(round == "QF"),
    "round of 16" = sum(round == "R16"),
    "round of 32" = sum(round == "R32"),
    "round of 64" = sum(round == "R64"),
    "round of 128" = sum(round == "R128"),
  ) %>%
  arrange(desc(championships), desc(finals), desc(semifinals), desc(quarterfinals),
          desc(`round of 16`), desc(`round of 32`), desc(`round of 64`), desc(`round of 128`))

# players by year

players %>%
  group_by(name, tourney_id) %>%
  summarize(
    total_points = sum(points) + sum(bonus))

# top players over time
players %>%
  group_by(name) %>%
  mutate(
    career_points = cumsum(points + bonus)) %>%
  filter(name %in% top10) %>%
  ggplot() +
  geom_line(aes(x = date, y = career_points, color = name))

# top players over time per tournament
players %>%
  group_by(name, tourney_name) %>%
  mutate(
    career_points = cumsum(points + bonus)) %>%
  filter(name %in% top10) %>%
  ggplot() +
  geom_line(aes(x = date, y = career_points, color = name)) +
  facet_wrap(~tourney_name)



gs_table <- players %>%
  group_by(name, dob, ioc, height, hand) %>%
  summarize(
    Championships = sum(result == "winner" & round == "F"),
    Finals = sum(round == "F"),
    Semifinals = sum(round == "SF"),
    Quarterfinals = sum(round == "QF"),
    "Round of 16" = sum(round == "R16"),
    "Round of 32" = sum(round == "R32"),
    "Round of 64" = sum(round == "R64"),
    "Round of 128" = sum(round == "R128")) %>%
  arrange(desc(Championships), desc(Finals), desc(Semifinals), desc(Quarterfinals),
          desc(`Round of 16`), desc(`Round of 32`), desc(`Round of 64`), desc(`Round of 128`)) %>%
  ungroup() %>%
  mutate(
    Rank = row_number(),
    `Birth Year` = year(dob)) %>%
  relocate(Rank, .before = name) %>%
  rename(
    Country = ioc,
    Name = name,
    Height = height,
    Hand = hand) %>%
  relocate(`Birth Year`, .before = Country) %>%
  select(-dob) %>%
  slice(1:25)


