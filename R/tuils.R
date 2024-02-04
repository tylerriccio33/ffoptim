#' @export
isinstance <- function(obj, ...) {
  classes <- class(obj)
  any(...) %in% classes
}

#' @export
assert_instance <- function(obj, ...) {
  res <- isinstance(obj = obj, ...)
  if (!res) {
    cli_abort(c(
      "Object of {.cls {class(obj)}} must be one of class {.cls {...}}."
    ))
  }
}
