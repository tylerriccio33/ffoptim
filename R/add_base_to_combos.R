#' @export
add_base_to_combos <- function(trade_packages, base_id, base_val, base_player_name) {
  assert_cols(trade_packages, combo, id, val, player_name)

  new_rows <- tibble(combo = unique(trade_packages$combo),
                     id = base_id,
                     val = base_val,
                     player_name = base_player_name)

  # add new rows
  trade_packages <- vec_rbind(new_rows, trade_packages) %>%
    arrange(combo)

  return(trade_packages)
}
