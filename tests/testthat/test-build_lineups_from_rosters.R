

test_that("function keeps all original columns", {

  test_data <- tibble(franchise_name = 'foo',
                      player_id = 1,
                      player_name = 'foo',
                      value_2qb = 1,
                      pos = 'foo')

  suppressWarnings({
    result <- build_lineups_from_rosters(test_data)
  })

  # the data is joined using `c('id' = 'player_id')`
  # so player id will be missing from the result

  expect_true(object = {
    cols_needed <- discard(colnames(test_data), \(x) x == 'player_id')
    all(cols_needed %in% colnames(result))

  })

})


