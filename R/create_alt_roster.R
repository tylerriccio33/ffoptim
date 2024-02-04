#' create_alt_roster
#'
#' @param lineup_lists named list of lineups containing a slot, id and value
#' @param sim_lookup ?
#'
#' @return
#' @export
#'
#' @importFrom purrr list_rbind
#' @importFrom vctrs vec_assign vec_slice
#' @importFrom tidyr unnest_longer
#' @importFrom tibble enframe
#'
create_alt_roster <- function(lineup_lists, sim_lookup) {
  full_lookup <- purrr::list_rbind(lineup_lists, names_to = "team") %>%
    select(-slot)

  switch_lookup <- sim_lookup %>%
    tibble::enframe(name = "player", value = "alt") %>%
    tidyr::unnest_longer(alt)

  players_to_switch <- switch_lookup$player
  alt_players <- switch_lookup$alt

  alt_universe <- vector(mode = "list", length = length(players_to_switch))
  for (i in seq_along(players_to_switch)) {
    cur_player <- players_to_switch[i]
    cur_alt <- alt_players[i]

    cur_player_i <- full_lookup$id == cur_player
    cur_player_row <-
      vec_slice(full_lookup, cur_player_i)
    cur_player_team <- cur_player_row$team
    cur_player_id <- cur_player_row$id
    cur_player_val <- cur_player_row$val

    cur_alt_i <- full_lookup$id == cur_alt
    cur_alt_row <-
      vec_slice(full_lookup, cur_alt_i)
    cur_alt_team <- cur_alt_row$team
    cur_alt_id <- cur_alt_row$id
    cur_alt_val <- cur_alt_row$val

    # Swap Ids and Values #
    # assign alt id to real id
    cur_lookup <- full_lookup
    cur_lookup$id <- vec_assign(
      x = full_lookup$id,
      i = cur_player_i,
      value = cur_alt_id
    )
    cur_lookup$val <- vec_assign(
      x = full_lookup$val,
      i = cur_player_i,
      value = cur_alt_val
    )
    cur_lookup$id <- vec_assign(
      x = full_lookup$id,
      i = cur_alt_i,
      value = cur_player_id
    )
    cur_lookup$val <- vec_assign(
      x = full_lookup$val,
      i = cur_alt_i,
      value = cur_player_val
    )

    # filter for only effected teams
    effected_teams <- c(cur_player_team, cur_alt_team)
    cur_lookup <- vec_slice(cur_lookup, i = cur_lookup$team %in% effected_teams)

    alt_universe[[i]] <- cur_lookup
  }

  return(alt_universe)
}
