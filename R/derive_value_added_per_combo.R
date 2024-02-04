#' Derive Value Added Per Combo
#'
#' @description Convenience/helper function to execute combos as transactions and derive value added from the transaction.
#'
#' @param cur_trade_packages Tibble containing `combo`, `id` (player) and `val` (player).
#' @param roster A roster class with the `slot`, `id`, `val`, `pos` and `starter`.
#' @param other_roster  A roster class with the `slot`, `id`, `val`, `pos` and `starter`.
#' @param cur_other_player The ID of the target player.
#' @param pre_cur Named list with `total_value` and `starter_value`, result of `derive_value_added_per_combo()`.
#' @param pre_other Named list with `total_value` and `starter_value`, result of `derive_value_added_per_combo()`.
#'
#' @return Named list, where the names are the combos. The values for each combo is another named list, with `starter_value`, `other_starter_value`, `total_value` and `other_total_value`.
#' @export
#'
derive_value_added_per_combo <-
  function(cur_trade_packages,
           roster,
           other_roster,
           cur_other_player,
           pre_cur,
           pre_other) {
    # assert_instance(roster, 'roster')
    # assert_instance(other_roster, 'roster')
    assert_cols(cur_trade_packages, combo, id, val)

    # for each trade package, execute the trade and calculate values
    trade_package_combos <-
      nest(cur_trade_packages, .by = .data$combo) %>%
      deframe()
    combo_values <-
      vector(mode = 'list', length = length(trade_package_combos))
    for (i in seq_along(combo_values)) {
      cur_dt <- trade_package_combos[[i]]
      # create post roster
      post_rosters <-
        execute_player_transaction(
          x = roster,
          y = other_roster,
          x_transaction = cur_dt$id,
          y_transaction = cur_other_player
        ) %>%
        # ? extract this method to convenience function
        map(\(x) mutate(x, pos = if_else(is.na(pos), 'PICK', pos))) %>%
        # optimize lineup
        map(\(x) opt_lineup(
          id = x$id,
          pos = x$pos,
          val = x$val
        ))

      # calculate total values of post
      post_values <-
        map(post_rosters, \(x) calc_total_roster_values(x))
      list2env(post_values, envir = current_env())   # add x and y to env for ease

      # calculate value added by transaction
      combo_values[[i]] <-
        list(
          "starter_value" = x$starter_value - pre_cur$starter_value,
          "other_starter_value" = y$starter_value - pre_other$starter_value,
          "total_value" = x$total_value - pre_cur$total_value,
          "other_total_value" = y$total_value - pre_other$total_value
        )
    }
    names(combo_values) <- names(trade_package_combos)
    return(combo_values)
  }
