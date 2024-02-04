#' Title
#'
#' @param platform
#' @param league_id
#' @param season
#' @param n_qb
#' @param draft_order_agnostic
#'
#' @return
#' @export
#'
FFData <- function(platform = "sleeper",
                            league_id = "985411219362246656",
                            season = 2023,
                            n_qb = 2,
                            draft_order_agnostic = T) {

  # Connect League #
  lg <- ffscrapr::ff_connect(platform = "sleeper",
                             league_id = "985411219362246656",
                             season = season)

  # Collect Rosters #
  rosters <- ffscrapr::ff_rosters(lg) %>%
    select(-c(team, age, franchise_id))

  # Collect Values #
  qb_expr <- glue::glue("{n_qb}qb$")
  vals <- ffscrapr::dp_values() %>%
    select(player, pos, fp_id, matches(qb_expr))

  # Collect IDs #
  id_col <- dplyr::case_match(platform,
                              "sleeper" ~ "sleeper_id",
                              .default = rlang::na_chr)
  ids <- ffscrapr::dp_playerids() %>%
    select(id = {{id_col}}, fp_id = fantasypros_id)

  # Join #
  joined_rosters <- dplyr::left_join(x = rosters,
                                     y = ids,
                                     by = c('player_id' = 'id')) %>%
    left_join(y = select(vals, -c(player, pos)),
              by = 'fp_id')

  # Nest Players into Team #
  nested_rosters <- dplyr::group_by(joined_rosters, franchise_name) %>%
    tidyr::nest(.key = "roster")


  # Collect Draft Picks #
  draft <- Draft(lg = lg,
                 .season = season,
                 vals = vals)
  draft_assets <- draft$present_pick_assets

  nested_picks <- draft_assets %>%
    dplyr::group_by(franchise_name) %>%
    tidyr::nest(.key = "picks")

  # Join Picks and Rosters #
  all_assets <- dplyr::left_join(x = nested_rosters,
                                 y = nested_picks,
                                 by = 'franchise_name') %>%
    dplyr::ungroup() %>%
    dplyr::mutate(all_assets = purrr::map2(roster, picks, \(x, y) vctrs::vec_rbind(x, y)))

  # Get Non-Rostered #
  non_rostered <- dplyr::anti_join(x = ids,
                                   y = rosters,
                                   by = c('id' = 'player_id')) %>%
    tidyr::drop_na() %>%
    dplyr::inner_join(y = vals,
                      by = 'fp_id') %>%
    dplyr::select(-fp_id)


  # Set up Class #
  cls <- list(
    "lg" = lg,
    "non_rostered" = non_rostered,
    "all_rostered_assets" = all_assets
  )

  # Unnest Assets Method #
  unnest_roster_element <- function(self, elem) {
    valid_elems <- c('roster','pick','all_assets')
    string_elem <- rlang::enexpr(elem) %>% as.character()
    if(!string_elem %in% valid_elems) {
      cli::cli_abort(c(
        "Elem {.var string_elem} must be in {valid_elems}"
      ))
    }
    dt <- self$all_rostered_assets[[1]]
    unnested <- dplyr::select(dt, franchise_name, {{elem}}) %>%
      tidyr::unnest()
    return(unnested)
  }
  cls[["unnest_roster_element"]] <- unnest_roster_element

  return(cls)

}



