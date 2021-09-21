# Used "CPR-numbers" in tests have been checked to not fulfill the modulus 11
# test to make it almost certain they are not real CPR-numbers.
test_that("misspecification of arguments result in error.", {

  expect_error(cpr_extract(0101001234))
  expect_error(cpr_extract("0101001234", extract = "not_option"))
  expect_error(cpr_extract("0101001234", test_modulus11 = 1))
  expect_error(cpr_extract("101001234"))
})

test_that("extracted information is correct", {
  x <- c("0101001234", "0101001235")
  expect_equal(cpr_extract(x, extract = "sex"), c(0, 1))
  expect_equal(
    cpr_extract(x, extract = "birthdate"),
    c(as.Date("1900-01-01"), as.Date("1900-01-01"))
  )
})

test_that("input with impossible day/month of birth results in error", {
  expect_error(cpr_extract("4040001234"))
})

test_that("modulus 11 test is correct", {
  # We can't/shouldn't check if real CPR-numbers pass the modulus 11 test,
  # since CPR-numbers are person-sensitive data. The function has been
  # informally tested on the author's own CPR-number to check that it passes
  # the test. Technically, not all real CPR numbers pass this test, but the
  # author's is known to do so. For more information on the modulus 11 test,
  # see the references.
  expect_error(cpr_extract("0101001234", extract = "sex", test_modulus11 = TRUE))
})

test_that("input CPR-numbers are cleaned correctly", {
  expect_equal(cpr_extract("010100-1234"), as.Date("1900-01-01"))
  expect_equal(cpr_extract("010100 1234", extract = "sex"), 0)
})


test_that("century of birth algorithm works as intended", {
  expect_equal(cpr_extract("0101001234"), as.Date("1900-01-01"))
  expect_equal(cpr_extract("0101002234"), as.Date("1900-01-01"))
  expect_equal(cpr_extract("0101003234"), as.Date("1900-01-01"))

  expect_equal(cpr_extract("0101004234"), as.Date("2000-01-01"))
  expect_equal(cpr_extract("0101604234"), as.Date("1960-01-01"))

  expect_equal(cpr_extract("0101005234"), as.Date("2000-01-01"))
  expect_equal(cpr_extract("0101605234"), as.Date("1860-01-01"))

  expect_equal(cpr_extract("0101006234"), as.Date("2000-01-01"))
  expect_equal(cpr_extract("0101606234"), as.Date("1860-01-01"))

  expect_equal(cpr_extract("0101007234"), as.Date("2000-01-01"))
  expect_equal(cpr_extract("0101607234"), as.Date("1860-01-01"))

  expect_equal(cpr_extract("0101008234"), as.Date("2000-01-01"))
  expect_equal(cpr_extract("0101608234"), as.Date("1860-01-01"))

  expect_equal(cpr_extract("0101009234"), as.Date("2000-01-01"))
  expect_equal(cpr_extract("0101609234"), as.Date("1960-01-01"))
})
