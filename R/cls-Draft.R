#' Title
#'
#' @param lg
#' @param .season
#' @param vals
#'
#' @return
#' @export
#'
#' @examples
Draft <- function(lg, .season, vals) {

  # Get Picks #
  # tibble of the current owning assets
  present_pick_assets <- ffscrapr::ff_draftpicks(lg) %>%
    dplyr::filter(.data$season == .season + 1) %>%
    dplyr::select(franchise_id, franchise_name, original_franchise_id, round)

  # Get Draft Values #
  season_expr <- glue::glue("^{.season} Pick")
  draft_vals <- vals %>%
    dplyr::filter(stringr::str_detect(.data$player, season_expr)) %>%
    dplyr::rename(pick = player) %>%
    dplyr::select(-fp_id) %>%
    derive_raw_pick_from_vals(rm_og_pick_col = T)

  # Build Full Draft #
  original_draft_order <- build_draft_order(lg, clean = T)
  max_rounds <- max(present_pick_assets$round)
  n_teams <- dplyr::n_distinct(present_pick_assets$franchise_id)
  draft_template <-
    build_draft_template(rounds = max_rounds, picks = n_teams)
  # append team to template using original order
  original_draft <- left_join(x = draft_template,
                              y = original_draft_order,
                              by = c('pick' = 'order'))

  # Join Pick to Current Assets #
  # use the round + original_franchise_id to get the raw pick
  present_pick_assets <- left_join(
    x = present_pick_assets,
    y = select(original_draft, round, pick, raw_pick, original_franchise_id = franchise_id),
    by = c("round", "original_franchise_id")
  )

  # Join Value to Round-Pick #
  present_pick_assets <- left_join(x = present_pick_assets,
                                   y = draft_vals,
                                   by = "raw_pick") %>%
    select(-c(pos))


  # Return Class #
  cls <-
    list(
      "original_order" = original_draft_order,
      "vals" = draft_vals,
      "present_pick_assets" = present_pick_assets
    )


  return(cls)

}
