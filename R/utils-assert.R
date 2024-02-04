assert_cols <- function(data, ...) {
  cols <- rlang::enexprs(...) %>% as.character()
  for (col in cols) {
    in_data <- col %in% colnames(data)
    if (!in_data) {
      cli::cli_abort(c(
        "{.var {col}} passed to {.var ...} must exist in {.var data}."
      ))
    }
  }
}
