test_that("multiplication works", {

  x <- data.table::as.data.table(
    t(matrix(c(
      1,  1,  2,
      1,  2,  4,
      1,  4,  5,
      2,  5,  6,
      3,  7, 10,
      3, 10, 11,
      4,  2,  4,
      4,  6,  8,
      4,  7,  9,
      4,  7, 10),
      nrow = 3)))
  names(x) <- c("group", "start_date", "end_date")


  test <- smooth_periods(x, "start_date", "end_date", "group")



  start = "start"
  end = "end"
  group = "group"
  keep = NULL
  max_gap = 1

  set.seed(1)
  n_row <- 1e7
  x <- data.table::data.table(
    group = ceiling(n_row * runif(n_row) / 10) ,
    start = ceiling(runif(n_row) * 10)
  )
  x$end <- x$start + rbinom(n_row, 5, 0.3)

  system.time(
    test <- smooth_periods(x, "start", "end", "group")
  )


  expect_equal(2 * 2, 4)
})
