test_that("invalid input trigger error", {
  expect_error(chr_to_regex(NULL))
  expect_error(chr_to_regex(NA))
  expect_error(chr_to_regex(1))
  expect_error(chr_to_regex("a", regex_type = "not_option"))
  expect_error(chr_to_regex(character(1)))
  expect_error(chr_to_regex(c("abc", "")))
})

test_that("basic use of function works as expected", {
  expect_equal(chr_to_regex(character(0)), character(0))
  expect_equal(chr_to_regex("abc"), "^abc")
  expect_equal(chr_to_regex(c("abc", "def")), "^abc|^def")
})

test_that("'regex_type' arguments works correctly", {
  expect_equal(chr_to_regex("abc", regex_type = "exact"), "^abc$")
  expect_equal(chr_to_regex(c("abc", "def"), regex_type = "exact"), "^abc$|^def$")
  expect_equal(chr_to_regex(c("abc", "def"), regex_type = "ends_with"), "abc$|def$")
})
