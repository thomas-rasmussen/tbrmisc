test_that("non-numeric input triggers error", {
  expect_error(is_integer("1"))
  expect_error(is_integer(FALSE))
})

test_that("integer(ish) input work", {
  expect_equal(is_integer(1), TRUE)
  expect_equal(is_integer(1.0), TRUE)
  expect_equal(is_integer(-1), TRUE)
})

test_that("various tests", {
  expect_equal(is_integer(1L), TRUE)
  expect_equal(is_integer(-1.5), FALSE)
  expect_equal(is_integer(c(-4, 2.0, 6.6)), c(TRUE, TRUE, FALSE))

})

test_that("return obejct is logical vector", {
  expect_equal(typeof(is_integer(1)), "logical")
  expect_equal(typeof(is_integer(c(-4, 2.0, 6.6))), "logical")
})
