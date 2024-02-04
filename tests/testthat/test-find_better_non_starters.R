

test_that("function works as intended", {

  test_data <- tibble(
    franchise_name = c('foo','foo','foo','bar','bar'),
    id = seq(5),
    val = c(5, 2, 3, 4, 5),
    starter = c(T, T, F, T, F),
    pos = c('QB','RB','WR', 'QB','WR'),
    player_name = seq(5)
  )


  result <- find_better_players(test_data,
                                cur_team = "foo",
                                non_starter_only = F)

  x_result <- tibble(
    franchise_name = 'foo',
    id = 3,
    val = 3,
    starter = F,
    pos = 'WR',
    player_name = 3,
    other_franchise_name = 'bar',
    other_id = 5,
    other_val = 5,
    other_player_name = 5
  )

  expect_equal(result, x_result)


  })

test_that("function works as intended", {

  test_data <- tibble(
    franchise_name = c('foo','foo','foo','bar','bar'),
    id = seq(5),
    val = c(3, 2, 3, 4, 5),
    starter = c(T, T, F, T, F),
    pos = c('QB','RB','WR', 'QB','WR'),
    player_name = seq(5)
  )


  result <- find_better_players(test_data,
                                cur_team = "foo",
                                non_starter_only = F)

  x_result <- tibble(
    franchise_name = c('foo', 'foo'),
    id = c(1, 3),
    val = c(3, 3),
    starter = c(T, F),
    pos = c('QB','WR'),
    player_name = c(1, 3),
    other_franchise_name = c('bar', 'bar'),
    other_id = c(4, 5),
    other_val = c(4, 5),
    other_player_name = c(4, 5)
  )

  expect_equal(result, x_result)


  })

test_that("function works as intended", {

  test_data <- tibble(
    franchise_name = c('foo','foo','foo','bar','bar'),
    id = seq(5),
    val = c(3, 2, 3, 4, 5),
    starter = c(T, T, F, T, F),
    pos = c('QB','QB','WR', 'QB','QB'),
    player_name = seq(5)
  )


  result <- find_better_players(test_data,
                                cur_team = "foo",
                                non_starter_only = F)

  # should have 4 total rows
  # 2 for each id
  expect_true(nrow(result) == 4)
  expect_true(n_distinct(result$id) == 2)
  expect_true(n_distinct(result$other_id) == 2)


  })

