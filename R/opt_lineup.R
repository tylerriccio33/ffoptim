#' opt_lineup
#'
#' @param id vector of ids
#' @param pos vector of full caps positions for the IDs. Picks are denoted as `PICKS`.
#' @param val vector of point values to optimize, corresponding to id
#'
#' @return a tibble of lineup slot, id and value for the whole team
#' @export
#'
#' @importFrom vctrs vec_slice vec_c
#' @importFrom purrr list_c discard map
#' @importFrom dplyr left_join
#' @importFrom tibble enframe
opt_lineup <- function(id, pos, val) {
  val <- dplyr::if_else(is.na(val), 0, val)
  data <- tibble(
    pos = pos,
    id = id,
    val = val
  )

  if(!assertthat::noNA(data)) {
    cli_abort("Some arguments contained missing values passed in.")
  }

  picks <- vec_slice(data, data$pos == "PICK")$id
  qbs <- vec_slice(data, data$pos %in% c("QB", "PICK"))$id
  rbs <- vec_slice(data, data$pos %in% c("RB", "PICK"))$id
  wrs <- vec_slice(data, data$pos %in% c("WR", "PICK"))$id
  tes <- vec_slice(data, data$pos %in% c("TE", "PICK"))$id
  flexs <- vec_c(rbs, wrs, tes, picks) %>% unique()
  supers <- vec_c(flexs, qbs, picks) %>% unique()
  all_pools <- list(
    "qb" = qbs,
    "rb" = rbs,
    "wr" = wrs,
    "te" = tes,
    "flex" = flexs,
    "super" = supers,
    "picks" = picks
  )

  avail_players <- purrr::list_c(all_pools)

  value_lookup <- data$val
  names(value_lookup) <- data$id

  positions <- c(
    "qb1" = NA_character_,
    "rb1" = NA_character_,
    "rb2" = NA_character_,
    "wr1" = NA_character_,
    "wr2" = NA_character_,
    "te1" = NA_character_,
    "flex1" = NA_character_,
    "flex2" = NA_character_,
    "super1" = NA_character_
  )

  for (position in names(positions)) {
    # Set Pool #
    # ? could make this case_when
    if (position == "qb1") {
      id_pool <- all_pools["qb"]
    } else if (str_detect(position, "^rb")) {
      id_pool <- all_pools["rb"]
    } else if (str_detect(position, "^wr")) {
      id_pool <- all_pools["wr"]
    } else if (position == "te1") {
      id_pool <- all_pools["te"]
    } else if (str_detect(position, "^flex")) {
      id_pool <- all_pools["flex"]
    } else if (position == "super1") {
      id_pool <- all_pools["super"]
    } else {
      cli_abort("Breakdown in position logic.")
    }

    # sometimes there are none of a position (or pick)
    if (is_empty(id_pool[[1]])) {
      next
    }


    # Sort Pool by Values #
    id_pool <- unlist(purrr::map(id_pool, as.character))
    cur_values <- value_lookup[c(id_pool)]
    ordered <- vec_sort(cur_values, direction = "desc")
    best_player <- names(ordered[1])

    # Insert Best Option #
    positions[position] <- best_player

    # Remove Player From All Avail #
    avail_players <-
      purrr::discard(avail_players, \(x) x == best_player)

    # Reconcile Avail w/Pools #
    # ? just use base
    all_pools <- all_pools %>%
      purrr::map( ~ {
        # discard where best player
        vec_players <- .x
        vec_players <-
          purrr::discard(vec_players, \(x) x == best_player)
        return(vec_players)
      })
  }


  # Pivot to Points + Players #
  avail_players <- unique(avail_players)
  # some players will be dupes (wrs also flex/super)
  non_starting_players <-
    tibble::as_tibble_col(as.character(avail_players), column_name = "id")


  pivoted <- tibble::enframe(positions,
                             name = "slot",
                             value = "id") %>%
    # add non-starting players
    bind_rows(non_starting_players) %>%
    left_join(tibble::enframe(value_lookup, name = "id", value = "val"),
              by = "id") %>%
    # extract rank
    mutate(
      rank = stringr::str_sub(.data$slot, start = -1) %>% as.integer(),
      starter = !is.na(rank)
    ) %>%
    # put back position
    left_join(y = {
      select(data, id, pos)
    }, by = "id")

  return(pivoted)
}
