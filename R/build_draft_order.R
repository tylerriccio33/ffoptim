#' Handle Drafting
#'
#' @export
#'
build_draft_order <- function(con, ..., clean = T) {
  # h2h_winpct then points_for
  # ... is additional tie breakers
  standings <- ffscrapr::ff_standings(con)

  ordered <- standings %>%
    arrange(.data$h2h_winpct, .data$points_for, ...)

  if (clean) {
    ordered <- ordered %>%
      select(franchise_id, franchise_name) %>%
      mutate(order = row_number())
  }

  return(ordered)
}

#' @export
build_draft_template <- function(rounds, picks) {
  draft_order <- tibble(
    round = integer(rounds * picks),
    pick = integer(rounds * picks)
  )

  counter <- 1

  for (round in 1:rounds) {
    if (round %% 2 == 1) {
      draft_order$round[counter:(counter + picks - 1)] <- round
      draft_order$pick[counter:(counter + picks - 1)] <- 1:picks
    } else {
      draft_order$round[counter:(counter + picks - 1)] <- round
      draft_order$pick[counter:(counter + picks - 1)] <- picks:1
    }
    counter <- counter + picks
  }

  # add raw pick
  draft_order$raw_pick <- seq_len(nrow(draft_order))
  return(draft_order)
}

derive_raw_pick_from_vals <- function(vals, rm_og_pick_col = T) {
  # the values are formatted as 2023 Pick 1.01
  # this must be converted into a raw pick
  vals <- vals %>%
    mutate(picks = str_sub(pick, start = -4)) %>%
    arrange(picks) %>%
    mutate(raw_pick = row_number())
  if (rm_og_pick_col) {
    vals <- select(vals, -pick)
  } else {
    cli_alert_danger("The value pick column ('2023 Pick 1.01') wasn't removed. You probably meant to remove this.")
  }
}
