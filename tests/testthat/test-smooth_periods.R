test_that("misspecification of arguments trigger error", {
  x <- data.frame()
  expect_error(smooth_periods(x))
  x <- data.table::data.table(start = 1)
  expect_error(smooth_periods(x))
  x <- data.table::data.table(start = "1")
  expect_error(smooth_periods(x))
  x <- data.table::data.table(start = 1, end = "1")
  expect_error(smooth_periods(x))
  x <- data.table::data.table(start1 = 1, end = 1)
  expect_error(smooth_periods(x))
  x <- data.table::data.table(start = 1, end1 = 1)
  expect_error(smooth_periods(x))
  x <- data.table::data.table(start = 1, end = 1)
  expect_error(smooth_periods(x, group = "group"))
  expect_error(smooth_periods(x, group = NA))
  expect_error(smooth_periods(x, max_gap = "1"))
  expect_error(smooth_periods(x, max_gap = 1.1))
  expect_error(smooth_periods(x, max_gap = -1))
})

test_that("data lines are correctly smoothed", {
  x <- data.table::data.table(
    t(matrix(c(
      1, 2,
      2, 3,
      4, 5),
      nrow = 2))
  )
  names(x) <- c("start", "end")
  smoothed_correct <- data.table(t(matrix(c(1,5))))
  names(smoothed_correct) <- c("start", "end")

  expect_equal(smooth_periods(x), smoothed_correct)
})

test_that("'group' argument works correctly", {
  x <- data.table::data.table(
    t(matrix(c(
      1, 1, 2,
      1, 2, 3,
      2, 4, 5,
      2, 6, 7),
      nrow = 3))
  )
  names(x) <- c("group", "start", "end")
  smoothed_correct <- data.table(
    t(matrix(c(
      1, 1, 3,
      2, 4, 7),
      nrow = 3))
  )
  names(smoothed_correct) <- c("group", "start", "end")

  expect_equal(smooth_periods(x, group = "group"), smoothed_correct)
})

test_that("'max_gap' argument works correctly", {
  x <- data.table::data.table(
    t(matrix(c(
      1, 2,
      2, 3,
      4, 5),
      nrow = 2))
  )
  names(x) <- c("start", "end")

  smoothed_correct0 <- data.table(
    t(matrix(c(
      1, 3,
      4, 5),
      nrow = 2))
  )
  smoothed_correct1 <- data.table(
    t(matrix(c(
      1, 5),
      nrow = 2))
  )
  names(smoothed_correct0) <- c("start", "end")
  names(smoothed_correct1) <- c("start", "end")

  expect_equal(smooth_periods(x, max_gap = 0), smoothed_correct0)
  expect_equal(smooth_periods(x, max_gap = 1), smoothed_correct1)
})
