#' vec_nest
#'
#' @param data
#' @param ...
#'
#' @return
#'
vec_nest <- function(data, ...) {
  suppressMessages({
    nest_cols <- dplyr::select(data, ...) %>% colnames()
  })

  res <-
    vec_split(
      x = data[setdiff(colnames(data), nest_cols)],
      by = data[nest_cols]
    )

  vec_cbind(res$key, tibble::new_tibble(list(data = res$val)))
}
