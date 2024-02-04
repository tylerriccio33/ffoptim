#' Nest Assets
#'
#' @description Convenience function that takes all owned assets and nests them by roster.
#'
#' @param all_owned_assets Tibble of assets associated with a `franchise_name`.
#' @param ... Args passed to `dplyr::select()` prior to nesting. If empty, it's `everything()`.
#' @param .key The `.key` argument of `tidyr::nest()`.
#'
#' @return
#' @export
#'
#' @examples
nest_assets <- function(all_owned_assets, ..., .key = "data") {

  assert_cols(all_owned_assets, franchise_name)

  dots <- enexprs(...)
  empty_dots <- length(dots) == 0
  if (empty_dots) {
    select_args <- exprs(everything())
  } else {
    select_args <- dots
  }

  clean_assets <- select(all_owned_assets, franchise_name, !!!select_args)

  nested <- nest(clean_assets, .by = "franchise_name", .key = .key)

  return(nested)

}

