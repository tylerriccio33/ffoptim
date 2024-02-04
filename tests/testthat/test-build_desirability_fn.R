

test_that("check simple examples", {
  # 1:9 is really 10 units
  fn <- build_desirability_fn(
    present_values = seq(1:9),
    future_values = seq(1:9),
    present_scale = 1,
    future_scale = 1
  )

  # perfect even
  result <- fn(present_value = 5, future_value = 5)
  expect_equal(result, .5)

  # 0
  result <- fn(present_value = 1, future_value = 1)
  expect_equal(result, 0)

  # .25
  # present value of 1 would really be a perfect 0
  # present value of 2 scales to 1
  result <- fn(present_value = 2, future_value = 5)
  expect_equal(result, .25)

  # 1
  result <- fn(present_value = 15, future_value = 15)
  expect_equal(result, 1)

})


test_that("check scales work", {
  # 1:9 is really 10 units
  fn <- build_desirability_fn(
    present_values = seq(1:9),
    future_values = seq(1:9),
    present_scale = 1,
    future_scale = 1/2
  )
  even_fn <- build_desirability_fn(
    present_values = seq(1:9),
    future_values = seq(1:9),
    present_scale = 1,
    future_scale = 1
  )

  result <- fn(present_value = 5, future_value = 5)
  expect_gt(result, even_fn(present_value = 5, future_value = 5))



})










