#' Partial Desirability Converter Function
#'
#' @description This function creates a new desirability converter that takes a new present and future value(s) and converts it to a desirability between 0 and 1.
#'
#' @param present_values Vector of present values.
#' @param future_values Vector of future values.
#' @param present_scale Scalar which multiplies the importance of the present value.
#' @param future_scale Scalar which multiplies the importance of the future value.
#'
#' @return A function, partially completed which takes the argument `present_value` and `future_value`. This partialed function returns the desirability of those two values. This function is vectorized.
#' @export
#'
build_desirability_fn <-
  function(present_values,
           future_values,
           present_scale = 1,
           future_scale = 1) {

    present_values <-
      if_else(is.na(present_values), 0, present_values)
    future_values <- if_else(is.na(future_values), 0, future_values)
    # TODO: Should probably put a message that explains this in more detail

    low_present <- min(present_values)
    high_present <- max(present_values)
    low_future <- min(future_values)
    high_future <- max(future_values)

    desirability_fn <- function(present_value,
                                future_value,
                                low_present,
                                high_present,
                                low_future,
                                high_future,
                                present_scale,
                                future_scale) {

      present_value <- if_else(is.na(present_value), 0, present_value)
      future_value <- if_else(is.na(future_value), 0, future_value)

      if (present_scale != 0) {
        present_d <-
          desirability2::d_max(
            x = present_value,
            low = low_present,
            high = high_present,
            scale = present_scale
          )
      }
      if (future_scale != 0) {
        future_d <-
          desirability2::d_max(
            x = future_value,
            low = low_future,
            high = high_future,
            scale = future_scale
          )
      }

      if (present_scale == 0) {
        return(future_d)
      }
      if (future_scale == 0) {
        return(present_d)
      }

      overall_d <- desirability2::d_overall(present_d,
                                            future_d,
                                            geometric = T)
      overall_d <- round(overall_d, digits = 5)

      return(overall_d)
    }

    desirability_fn <- purrr::partial(
      desirability_fn,
      low_present = low_present,
      high_present = high_present,
      low_future = low_future,
      high_future = high_future,
      present_scale = present_scale,
      future_scale = future_scale
    )

    return(desirability_fn)
  }
