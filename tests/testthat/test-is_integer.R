test_that("is_integer works", {
  expect_equal(is_integer(1), TRUE)
  expect_equal(is_integer(1L), TRUE)
  expect_equal(is_integer(1.0), TRUE)
  expect_equal(is_integer(-1), TRUE)
  expect_equal(is_integer(-1.5), FALSE)
  expect_equal(is_integer(FALSE), TRUE)
  expect_equal(is_integer(TRUE), TRUE)
  expect_error(is_integer("1"), "non-numeric argument to mathematical function")
  expect_equal(is_integer(c(TRUE, -4, 2.0, 6.6)), c(TRUE, TRUE, TRUE, FALSE))

})

test_that("return obejct is logical vector", {
  expect_equal(typeof(is_integer(1)), "logical")
  expect_equal(typeof(is_integer(c(TRUE, -4, 2.0, 6.6))), "logical")
})
