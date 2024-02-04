#' Make Traade Candidates
#'
#' @description
#' each trade based on 1:1 basis:
#   - one asset for another asset
# values are defined as:
#   - self value ~ valuation system of owning team
#   - alt value ~ valuation system of opposing team
#   - For now, this will all be the value_2qb before the desirability functions are stable
# trade is evaluated if:
#   - opposing asset y has more self value than owned asset x
#   - owned asset x more alt value than opposing asset y
#   - thus, creating a mutually beneficial asset swap
# degree of fleece is set as max allowable alt value

#' @param ...
#' @param scale Boolean indicating values to be scaled.
#' @param window Value difference needed for trade consideration. Respect the scale.
#'
#' @return
#' @export
make_all_trade_candidates <-
  function(dataclass = ffoptim::FFDATA,
           present_values = ffoptim::PRESENT_VALUES,
           cur_team = "Black Wilson",
           fleece_degree = .05,
           window = .1,
           scale = F) {

  # Cue up Data #
  all_assets <-
    select(FFDATA$all_rostered_assets, franchise_name, all_assets) %>%
    tidyr::unnest(cols = c(all_assets)) %>%
    left_join(select(present_values, -pos),
              by = c('fp_id')) %>%
    mutate(across(c(present_val, value_2qb), \(x) if_else(is.na(x), 1, x)),
           player_id = if_else(!is.na(raw_pick), glue("raw_pick--{raw_pick}"), player_id),
           player_name = if_else(!is.na(raw_pick), player_id, player_name))

  # Assign Player Values #
  # self_val_fn <-
  #   build_desirability_fn(
  #     present_values = all_assets$present_val,
  #     future_values = all_assets$value_2qb,
  #     present_scale = 1,
  #     future_scale = 1
  #   )
  # alt_val_fn <-
  #   build_desirability_fn(
  #     present_values = all_assets$present_val,
  #     future_values = all_assets$value_2qb,
  #     present_scale = 1,
  #     future_scale = 1
  #   )
  # all_assets$self_val <- self_val_fn(present_value = all_assets$present_val,
  #                                    future_value = all_assets$value_2qb)
  # all_assets$alt_val <- alt_val_fn(present_value = all_assets$present_val,
  #                                  future_value = all_assets$value_2qb)
  if (scale) {
  all_assets$self_val <- scales::rescale(all_assets$value_2qb)
  all_assets$alt_val <- scales::rescale(all_assets$value_2qb)
  } else {
  all_assets$self_val <- all_assets$value_2qb
  all_assets$alt_val <- all_assets$value_2qb
  }

  # Create Swap Candidates #
  # find similar self value swaps
  similar_assets <-
    ffoptim::locate_similar_players(
      players = all_assets$player_id,
      point_values = all_assets$self_val,
      i = all_assets$franchise_name == cur_team,
      window = window,
      filter_pos_ev = T
    )
  # append points to owning and opposing
  similar_assets <- tibble::enframe(similar_assets,
                                    name = "owning_id",
                                    value = "opposing_id") %>%
    tidyr::unnest_longer(opposing_id) %>%
    left_join(
      y = select(all_assets, player_id, player_name, self_val, alt_val),
      by = c('owning_id' = 'player_id')
    ) %>%
    left_join(
      y = select(all_assets, player_id, player_name, self_val, alt_val),
      by = c('opposing_id' = 'player_id')
    )

  # filter where owned asset has more alt value than opposing asset
  # swap_candidates <- filter(similar_assets,
  #                                 alt_val.x >= alt_val.y)

  # Calculate Raw Gained Value #
  swap_candidates <- mutate(
    similar_assets,
    self_raw_gain = self_val.y - self_val.x,
    alt_raw_gain = alt_val.x - alt_val.y
  )


  # Clean Up #
  swap_candidates <- swap_candidates %>%
    # join back to owning franchises
    left_join(y = {
      select(all_assets, franchise_name.x = franchise_name, owning_id = player_id)
    }) %>%
    left_join(y = {
      select(all_assets, franchise_name.y = franchise_name, opposing_id = player_id)
    }) %>%
    # remove pick swaps
    filter(!(str_detect(owning_id, "raw_pick--") & str_detect(opposing_id, "raw_pick--")))

      return(swap_candidates)

  }









