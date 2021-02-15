test_that("is_valid_variable_name works as intended", {
  expect_equal(is_valid_variable_name("valid_name"), TRUE)
  expect_equal(is_valid_variable_name(var <- "var"), TRUE)
  expect_equal(is_valid_variable_name("_invalid"), FALSE)
  expect_equal(is_valid_variable_name(".valid"), TRUE)
  expect_equal(is_valid_variable_name("..valid"), TRUE)
  expect_equal(is_valid_variable_name("if"), FALSE)
  expect_equal(is_valid_variable_name("while"), FALSE)
  expect_equal(is_valid_variable_name("..."), FALSE)
  expect_equal(is_valid_variable_name("...", allow_reserved = FALSE), FALSE)
  expect_equal(is_valid_variable_name("...", allow_reserved = TRUE), TRUE)
  expect_equal(is_valid_variable_name(c("..1", "..2")), c(FALSE, FALSE))
  expect_equal(is_valid_variable_name(c("var", "var")), c(TRUE, TRUE))
  expect_equal(
    is_valid_variable_name(c("var", "var"), unique = TRUE), c(TRUE, FALSE)
  )
  expect_equal(
    is_valid_variable_name(c("valid", "_invalid", "while", "...")),
    c(TRUE, FALSE, FALSE, FALSE)
  )
})
