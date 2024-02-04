

test_that("function works as intended", {
  test_data <- tibble(
    franchise_name = c('foo', 'bar'),
    val = c(1, 2),
    foo = c(1, 2)
  )
  result <- nest_assets(test_data, .key = 'roster')
  x_result <- tibble(franchise_name = c('foo', 'bar'),
                     roster = list(tibble(val = 1,
                                        foo = 1),
                                 tibble(val = 2,
                                        foo = 2)))
  expect_equal(result, x_result)


})
test_that("function respects ...", {
  test_data <- tibble(
    franchise_name = c('foo', 'bar'),
    val = c(1, 2),
    foo = c(1, 2)
  )
  result <- nest_assets(test_data, new_val = val, .key = 'roster')
  x_result <- tibble(franchise_name = c('foo', 'bar'),
                     roster = list(tibble(new_val = 1),
                                 tibble(new_val = 2)))
  expect_equal(result, x_result)


})
