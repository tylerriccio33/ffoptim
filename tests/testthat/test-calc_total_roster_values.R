


test_that("function works as intended", {
  test_data <- tibble(val = c(1, 1, 1),
                      starter = c(T, T, F))

  result <- calc_total_roster_values(test_data)
  x_result <- c("total_value" = 3,
                "starter_value" = 2)

  expect_equal(result,
               x_result)

})
