
test_that("function works as inteded simply", {
  dt <- tibble(
    id = seq(5),
    val = c(1, 2, 1 , 4 , 5),
    my_team = c(T, T, F, F, F)
  )
  x <- list(`1` = c(3) %>% as.character(),
            `2` = c(3) %>% as.character())

  result <- locate_similar_players(
    players = dt$id,
    point_values = dt$val,
    i = dt$my_team,
    window = 1,
    filter_pos_ev = F
  )

  expect_length(result, 2)

  expect_equal(result, x)

})

test_that("another random example", {
  dt <- tibble(
    id = seq(5),
    val = c(5, 2, 1, 1, 6),
    my_team = c(T, T, F, F, F)
  )
  x <- list(`1` = c(5) %>% as.character(),
            `2` = c(3, 4) %>% as.character())

  result <- locate_similar_players(
    players = dt$id,
    point_values = dt$val,
    i = dt$my_team,
    window = 1,
    filter_pos_ev = F
  )

  expect_length(result, 2)

  expect_equal(result, x)

})

test_that("that roschon example", {
  dt <- tibble(
    id = c(10219, 10235) %>% as.character(),
    val = c(.0282, .297),
    my_team = c(T, F)
  )
  x <- list()

  result <- locate_similar_players(
    players = dt$id,
    point_values = dt$val,
    i = dt$my_team,
    window = .2,
    filter_pos_ev = F
  )

  expect_equal(unname(result), x)

})

test_that("filters to pos ev only", {
  dt <- tibble(
    id = seq(5),
    val = c(1, 2, 1 , 4 , 5),
    my_team = c(T, T, F, F, F)
  )
  x <- list(`1` = c(3) %>% as.character())

  result <- locate_similar_players(
    players = dt$id,
    point_values = dt$val,
    i = dt$my_team,
    window = 1,
    filter_pos_ev = T
  )

  expect_length(result, 1)

  expect_equal(result, x)

})

