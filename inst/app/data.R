library(ffoptim)
library(tidyverse)
library(glue)
library(gt)
library(powerjoin)
library(gtExtras)
library(RColorBrewer)

TEAM <- 'Black Wilson'
pal <- RColorBrewer::brewer.pal(n = 8, name = "Set2")
RColorBrewer::display.brewer.pal(n = 8, name = "Set2")
pgreen <- "#A6D854"
pred <- "#FC8D62"
# color_scale <- colorRamp(c(pred, pgreen))(100)


# Pull Lookups #
all_owned_assets <- FFDATA$all_rostered_assets %>%
  select(-c(roster, picks)) %>%
  unnest(all_assets) %>%
  mutate(
    player_id = if_else(is.na(player_id), glue("raw_pick--{raw_pick}"), player_id),
    player_name = if_else(is.na(player_id), player_id, player_name)
  )
all_owned_players <- all_owned_assets %>%
  filter(!str_detect(player_name, "^raw_pick--")) %>%
  mutate(scaled_value = scales::rescale(value_2qb))
all_owned_draft_picks <-
  anti_join(all_owned_assets, all_owned_players, by = join_by(player_id)) %>%
  janitor::remove_empty('cols')
all_unowned_players <- FFDATA$non_rostered %>%
  rename(player_id = id,
         player_name = player)
all_league_players <-
  bind_rows(all_owned_players, all_unowned_players) %>%
  janitor::remove_empty('cols')
all_league_assets <-
  bind_rows(all_league_players, all_owned_draft_picks) %>%
  janitor::remove_empty('cols')

# Current Team Lookups #
cur_roster <- all_owned_players %>%
  filter(franchise_name == TEAM)
cur_lineup <- opt_lineup(id = cur_roster$player_id,
                         pos = cur_roster$pos,
                         val = cur_roster$value_2qb) %>%
  rename(player_id = id) %>%
  power_left_join(y = {
    cur_roster
  },
  by = join_by(player_id),
  conflict = coalesce_xy)

# Addatives #
player_lookups <- nflreadr::load_ff_playerids() %>%
  select(player_id = sleeper_id,
         player_name = name,
         team,
         gsis_id) %>%
  left_join(y = {
    nflreadr::load_players() %>%
      select(gsis_id,
             headshot) %>%
      drop_na()
  })

# create lineups from rosters

all_lineups <- all_owned_players %>%
  group_by(franchise_name) %>%
  nest() %>%
  mutate(opt_lineups = map(
    data,
    \(x) opt_lineup(
      id = x$player_id,
      pos = x$pos,
      val = x$value_2qb
    )
  )) %>%
  ungroup() %>%
  select(-data) %>%
  unnest(opt_lineups) %>%
  mutate(slot_rank = scales::rescale(val), .by = slot) %>%
  mutate(starter_rank = scales::rescale(val), .by = starter) %>%
  mutate(pos_rank = scales::rescale(val), .by = pos) %>%
  left_join(y = {
    select(player_lookups, id = player_id, player_name, headshot)
  }) %>%
  relocate(player_name, headshot, .after = id) %>%
  mutate(row_id = row_number()) %>%
  group_by(franchise_name, slot) %>%
  arrange(-val, .by_group = T) %>%
  ungroup() %>%
  mutate(row_id = if_else(is.na(slot), na_int, row_id)) %>%
  arrange(row_id, -val) %>%
  select(-row_id)
