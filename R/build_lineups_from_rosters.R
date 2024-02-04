#' Build Lineups From Rosters
#'
#' @description Use a tibble of owned players to create lineups.
#'
#' @param owned_players
#'
#' @return Tibble with a slot, id and scaled ranks.
#' @export
#'
#' @examples
build_lineups_from_rosters <- function(owned_players) {

  assert_cols(owned_players, franchise_name, player_id, pos, value_2qb)
  owned_players$player_id <- as.character(owned_players$player_id)

  # Nest Picks and Optimize Lineup #
  optimized_lineups <- owned_players %>%
    nest(.by = "franchise_name") %>%
    mutate(opt_lineups = map(
      data,
      \(x) opt_lineup(
        id = x$player_id,
        pos = x$pos,
        val = x$value_2qb
      )
    )) %>%
    select(-data) %>%
    unnest(opt_lineups)

  # Derive Ranks #
  ranked_players <- optimized_lineups %>%
    mutate(slot_rank = scales::rescale(val), .by = slot) %>%
    mutate(starter_rank = scales::rescale(val), .by = starter) %>%
    mutate(pos_rank = scales::rescale(val), .by = pos)

  # Arrange #
  # lineups come back in the correct order ie. qb, rb, wr, etc.
  # this tracks the original order, then orders the NAs
  # the end result is the properly ordered lineup with value ordered NAs
  arranged_lineups <- ranked_players %>%
    mutate(row_id = row_number()) %>%
    group_by(franchise_name, slot) %>%
    arrange(-val, .by_group = T) %>%
    ungroup() %>%
    mutate(row_id = if_else(is.na(slot), na_int, row_id)) %>%
    arrange(row_id, -val) %>%
    select(-row_id)

  # Add Back #
  arranged_lineups <- power_left_join(x = arranged_lineups,
                                      y = owned_players,
                                      conflict = coalesce_xy,
                                      by = c("id" = "player_id"))


return(arranged_lineups)

}
