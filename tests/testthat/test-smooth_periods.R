test_that("multiplication works", {

  x <- as.data.frame(
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


  vec <- c("start_date", "end", NULL)
start = "start_date"
end = "end_date"
group = "group"
keep = NULL
max_gap = 1

  test <- smooth_periods(x, "start_date", "end_date", "group")

  expect_equal(2 * 2, 4)
})
