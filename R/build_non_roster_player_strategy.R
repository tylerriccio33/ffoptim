#' Title
#'
#' @param ordered_roster
#' @param non_rostered_data
#' @param max_spots
#'
#' @return
#' @export
#'
#' @examples
build_non_roster_player_strategy <-
  function(ordered_roster, non_rostered_data = FFDATA$non_rostered, max_spots = 30) {
    assert_cols(ordered_roster, slot, player_id, val, pos, player_name)
    assert_cols(non_rostered_data, player_id, pos, val, player_name)
    # TODO: all non_rostered cols should be in ordered roster

    # Sort #
    sorted_roster <- arrange(ordered_roster, by = -val)
    sorted_non <- arrange(non_rostered_data, by = -val)

    # Traverse Non-Rostered #
    # for each player move up the roster
    new_roster <- sorted_roster
    cur_spots <- nrow(new_roster)
    for (i in seq_len(nrow(sorted_non))) {
      cur_non_roster_row <- vctrs::vec_slice(sorted_non, i = i)
      roster_spot <- new_roster$val < cur_non_roster_row$val
      if (!any(roster_spot)) {
        # no more roster spots
        break
      }
      roster_spot <- min(which(roster_spot))
      new_roster <- add_row(new_roster,
        cur_non_roster_row,
        .before = roster_spot
      )
      new_roster <- slice_head(new_roster, n = max_spots)
    }

    # describe total strategy
    new_players <- vctrs::vec_slice(new_roster$player_name, !new_roster$player_id %in% sorted_roster$player_id)
    drop_players <- vctrs::vec_slice(sorted_roster$player_name, !sorted_roster$player_id %in% new_roster$player_id)

    # return cls
    cls <-
      list(
        "add_players" = new_players,
        "drop_players" = drop_players,
        "new_roster" = new_roster,
        "old_roster" = sorted_roster
      )

    # build message
    drop_order <- rev(drop_players)
    purrr::walk2(
      new_players,
      drop_order,
      \(x, y) cli_alert_info("Add {x} and drop {y}")
    )

    return(cls)
  }









