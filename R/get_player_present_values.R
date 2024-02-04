#' Title
#'
#' @param ...
#' @param clean
#'
#' @return
#' @export
#'
#' @examples
get_player_present_values <- function(..., clean = T) {
  present_values <-
    ffpros::fp_rankings(page = "consensus-cheatsheets", sport = "nfl") %>%
    arrange(rank) %>%
    mutate(scaled_rank = rev(.data$rank),
           scaled_rank = scales::rescale(.data$scaled_rank),
           present_val = scaled_rank * player_owned_avg,
           present_val = scales::rescale(.data$present_val))

  if (clean) {
    present_values <- select(present_values,
                             fp_id = fantasypros_id,
                             name = player_name,
                             pos,
                             present_val)
    cli_alert(c(i = "{.var clean} is true so {.var ...} is ignored."))
  } else {
    dots_empty <- (rlang::list2(...) %>% length()) == 0
    if (!dots_empty) {
      present_values <- select(present_values, ...)
    }
  }
  return(present_values)
}


