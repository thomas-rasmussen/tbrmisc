test_that("is_date works", {
  expect_identical(is_date(as.Date("2000-01-01")), TRUE)
  expect_identical(is_date("2000-01-01"), FALSE)
})
