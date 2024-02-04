#' Calculate Total Roster Values
#'
#' @description Convenience function to calculate the starter and total value of a roster.
#'
#' @param roster Tibble with a `val` and `starter` column.
#' @param na.rm Boolean passed to `sum()`.
#'
#' @return Named list with `total_value` and `starter_value`.
#' @export
#'
calc_total_roster_values <- function(roster, na.rm = F) {
  assert_cols(roster, val, starter)

  if (any(vec_detect_missing(roster$starter))) {
    cli_abort(
      c("All values of {.var starter} must be complete.",
        i = "Was there a non-coalesced join somewhere?")
    )
  }

  partial_sum <- partial(sum, na.rm = na.rm)

  total_val <- partial_sum(roster$val)

  starter_filtered <- vec_slice(roster, i = roster$starter)
  starter_val <- partial_sum(starter_filtered$val)

  values <- list("total_value" = total_val,
                  "starter_value" = starter_val)

  return(values)

}
