#' locate_similar_players
#'
#' @param players vector of player IDs
#' @param point_values vector of corresponding point values
#' @param i logical vector indicating players to search from
#' @param window points allowed above or below player's point value
#' @param filter_pos_ev logical indicating detainment of only positive value propositions
#'
#' @return a dictionary of players (keys) and vectors of similar players (values)
#' @export
#'
#' @example
#' if(F) {
#' similar_player_list <- locate_similar_players(
#' players = dt$id,
#' point_values = dt$val,
#' i = dt$team == "Black Wilson",
#' window = 20,
#' filter_pos_ev = T
#' ) }
locate_similar_players <- function(players,
                                   point_values,
                                   i,
                                   window = .20,
                                   filter_pos_ev = T) {
  players <- as.character(players)

  if (length(players) == 1) {
    cli_bullets(
      c("x" = "One player passed into player.",
        ">" = "A number of players, as a vector should be passed here.",
        "i" = "This function is vectorized. Did you accidently call it in map?")
    )
  }

  if (any(vec_detect_missing(players)) |
      any(vec_detect_missing(point_values))) {
    cli_abort("Missing players or point values passed to `locate_similar_players()`")
  }

  if (!is_logical(i)) {
    cli_abort(
      c("x" = "Argument `i` must be a logical vector.",
        "i" = "`i` is used to find similar players for only a certain subset.")
    )
  }

  lookup <- set_names(x = point_values, nm = players)

  players_to_eval <- vec_slice(lookup, i)
  ids <- vector(mode = "list", length = length(players_to_eval))
  for (i in seq_along(players_to_eval)) {
    # loc similar i
    cur_player <- names(players_to_eval[i])
    cur_val <- players_to_eval[[i]]
    lower_val <- cur_val - window
    upper_val <- cur_val + window
    in_window <-
      between(x = point_values,
              left = lower_val,
              right = upper_val)
    if (filter_pos_ev) {
      # remove anything below cur_val
      in_window <- if_else(point_values < cur_val, F, in_window)
    }
    players_in_window <- vec_slice(x = players, # include all players
                                   i = in_window)
    # remove owned players
    players_in_window <-
      vec_slice(players_in_window,
                !vec_in(players_in_window, names(players_to_eval)))
    ids[[i]] <- players_in_window
  }
  names(ids) <- names(players_to_eval)
  ids <- discard(ids, is_empty)
  return(ids)
}
