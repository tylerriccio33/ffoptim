#' execute_player_transaction
#'
#' @param x dataset x, containing an `id` column.
#' @param y dataset y, containing an `id` column.
#' @param x_transaction a vector of ids to send to `y`.
#' @param y_transaction a vector of ids to send to `x`.
#'
#' @return a list containing `x` and `y` datasets with the swapped rows.
#' @export
#'
execute_player_transaction <-
  function(x, y, x_transaction, y_transaction) {
    # Validate #
    # `id` in both datasets
    common_cols <- intersect(colnames(x), colnames(y))
    if (!"id" %in% common_cols) {
      cli_abort(
        c(
          "{.var id} column must be in both datasets.",
          i = "Did you by chance pass in vectors instead of tibbles?",
          "i" = "Did you forget the id column or rename it something else?"
        )
      )
    }

    # Store and remove x rows
    x_switch_i <- vec_match(needles = x_transaction, haystack = x$id)
    x_switch_rows <- vec_slice(x, i = x_switch_i)
    x_keep_i <- vec_set_difference(x = seq(nrow(x)), y = x_switch_i)
    x <- vec_slice(x, i = x_keep_i)
    # Store and remove x rows
    y_switch_i <- vec_match(needles = y_transaction, haystack = y$id)
    y_switch_rows <- vec_slice(y, i = y_switch_i)
    y_keep_i <- vec_set_difference(x = seq(nrow(y)), y = y_switch_i)
    y <- vec_slice(y, i = y_keep_i)

    # Append
    x <- vec_rbind(x, y_switch_rows)
    y <- vec_rbind(y, x_switch_rows)

    # Return lists
    swapped <- list(
      "x" = x,
      "y" = y
    )

    return(swapped)
  }
