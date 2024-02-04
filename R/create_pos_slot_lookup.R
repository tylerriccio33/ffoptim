#' Title
#'
#' @param qb
#' @param rb
#' @param wr
#' @param te
#' @param flex
#' @param super
#'
#' @return
#' @export
#'
#' @examples
create_pos_slot_lookup <- function(qb = 1, rb = 2, wr = 2, te = 1, flex = 2, super = 1) {

  # create proper number of slots
  slot_lookup <- list("QB" = qb,
                      "RB" = rb,
                      "WR" = wr,
                      "TE" = te,
                      "flex" = flex,
                      "super" = super) %>%
    enframe(name = "pos",
            value = "slot_count") %>%
    mutate(slot = map2(pos, slot_count, \(x, y) rep(x = x, y))) %>%
    select(-slot_count) %>%
    unnest(slot) %>%
    mutate(slot = glue("{slot}{row_number()}"), .by = "pos")

  # add allowable positions
  multiple_pos_lookup <-
    list("flex" = c("RB", "WR", "TE"),
         "super" = c("RB", "WR", "TE", "QB")) %>%
    enframe(name = "pos",
            value = "positions")

  # join positions to pos
  left_join(x = slot_lookup,
            y = multiple_pos_lookup,
            by = "pos") %>%
    replace_na(list(positions = list(na_chr))) %>%
    unnest(positions) %>%
    mutate(positions = if_else(is.na(positions), pos, positions))


}
