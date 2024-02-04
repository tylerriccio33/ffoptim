#' Create Trade Package
#'
#' @description Create combos of players on the roster that sum to the value_needed within the window.
#'
#' @param value_needed
#' @param window
#' @param roster_lookup Tibble with `id`, `player_name` and `val` serving as the elements to make combinations of.
#' @param max_combn An int vector passed to `m` of `combn` determining the number of elements that can make up the value.
#'
#' @return
#' @export
#'
#' @examples
create_trade_packages <-
  function(value_needed,
           window,
           roster_lookup,
           max_combn = 2) {

    assert_cols(roster_lookup, player_name, id, val)
    if (!assertthat::is.number(max_combn)) {
      cli_abort("{.var max_combn} must be an scalar number.")
    }

    val_id_lookup <- select(roster_lookup, id, val)

    # create all id combos
    combinations <- vector(mode = 'list', length = max_combn)
    for (i in seq_along(combinations)) {
      cur_comb <- combn(x = roster_lookup$id, m = i)
      combinations[[i]] <- cur_comb
    }

    combinations <- map(combinations, as_tibble)
    combinations <- imap(combinations, ~ {
      # create unique names, much faster to do it manually
      colnames(.x) <- str_c(1:ncol(.x), glue("--{.y}"))
      return(.x)
    })
    # make sure each has correct rows
    max_rows <- max_combn
    combinations <- map(combinations, ~ {
      cur_rows <- nrow(.x)
      rows_needed <- max_rows - cur_rows
      if (rows_needed != 0) {
        empty_frame <- tibble(empty__col = "foo")
        bind_these <- rep(list(empty_frame), rows_needed)
        .x <- vec_rbind(.x, list_rbind(bind_these))
        .x <- select(.x, -empty__col)
        return(.x)
      }
      return(.x)
    })


    combinations <- list_cbind(combinations)
    # pivot longer
    pivoted <- pivot_longer(combinations,
                            cols = everything(),
                            names_to = "combo",
                            values_to = "id") %>%
      drop_na()     # combos with less than the max will have empty rows

      # join to values
    pivoted <- left_join(pivoted, val_id_lookup, by = "id")

    # filter to results within window
    lower <- value_needed - window
    upper <- value_needed + window
    # ? use .by instead of group-ungroup
    valid_combos <- group_by(pivoted, combo) %>%
      mutate(total_val = sum(val)) %>%
      filter(between(x = .data$total_val, left = lower, right = upper)) %>%
      ungroup() %>%
      select(-total_val)

    # join back to roster
    valid_combos <- left_join(valid_combos,
                              y = {
                                select(roster_lookup, id, player_name)
                              },
                              by = 'id')
    return(valid_combos)


  }
