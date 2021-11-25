test_that(".tidy_codes works", {

  # include more tests, especially some stuff that results in errors
  expect_error(tidy_codes("A - B"))
  expect_error(tidy_codes("A#C"))
  expect_equal(tidy_codes(""), character(0))
  expect_equal(tidy_codes("A2"), "A2")
  expect_equal(tidy_codes("A19-A30"), c("A19", "A2", "A30"))
  expect_equal(tidy_codes("A9-B1"), c("A9", "B0", "B1"))
  expect_equal(tidy_codes("A:B"), c("A", "B"))
  expect_equal(tidy_codes("A.9-B0"), c("A9", "B0"))
  expect_equal(tidy_codes("A-b"), c("A", "B"))
  expect_equal(tidy_codes("A00-B89"), c("A", paste0("B", 0:8)))
})

test_that(".expand_code_range works", {
  expect_error(.expand_code_range("B-A"))
  expect_equal(.expand_code_range("0-2"), c("0", "1", "2"))
  expect_equal(.expand_code_range("A-C"), c("A", "B", "C"))
  expect_equal(.expand_code_range("A8-B1"), c("A8", "A9", "B0", "B1"))
  expect_equal(.expand_code_range("7Y-8A"), c("7Y", "7Z", "8A"))
  expect_equal(.expand_code_range("A1A-A1C"), c("A1A", "A1B", "A1C"))
})

test_that(".next_code works", {
  # Basic tests
  expect_equal(.next_code("A"), "B")
  expect_equal(.next_code("1"), "2")
  expect_equal(.next_code("AB"), "AC")
  expect_equal(.next_code("A1"), "A2")
  expect_equal(.next_code("1A"), "1B")
  # We want the function to increase the chapter code
  # if the next code logically belongs to a new chapter
  expect_equal(.next_code("A9"), "B0")
  expect_equal(.next_code("8Z"), "9A")
  expect_equal(.next_code("09"), "10")
  expect_equal(.next_code("AZ"), "BA")
  expect_equal(.next_code("AZZ"), "BAA")
  expect_equal(.next_code("AZ2"), "AZ3")
  # If it is not clear what the next code is, return error
  expect_error(.next_code("Z"))
  expect_error(.next_code("9"))
  expect_error(.next_code("9Z"))
  expect_error(.next_code("Z9"))
  expect_error(.next_code("ZZ"))
  expect_error(.next_code("99"))
})

test_that(".collapse_codes works", {
  expect_equal(.collapse_codes(c(paste0("A", 0:9), "B", "B1")), c("A", "B"))
  expect_equal(.collapse_codes(c(paste0("A", 0:12), "B1")), c("A", "B1"))
  expect_equal(.collapse_codes(paste0("1", LETTERS)), "1")
  expect_equal(.collapse_codes(c("A", paste0("A0",0:12))), "A")
  expect_equal(.collapse_codes(paste0("1", 0:9)), "1")
  expect_equal(.collapse_codes(paste0("A", LETTERS)), "A")
  expect_equal(.collapse_codes(c("A", "A")), "A")
  expect_equal(.collapse_codes(c(paste0("0A", 0:9))), "0A")
  expect_equal(.collapse_codes(c(paste0("A0B1", LETTERS))), "A0B1")
})

test_that(".validate_code_range works", {
  expect_error(.validate_code_range(1))
  expect_error(.validate_code_range(c("a", "b")))
  expect_error(.validate_code_range("a"))
  expect_error(.validate_code_range("1:2"))
  expect_error(.validate_code_range("AB123"))
  expect_error(.validate_code_range("B-A"))
  expect_error(.validate_code_range("20-18"))
  expect_error(.validate_code_range("A20-A18"))
  expect_error(.validate_code_range("20-100"))
  expect_error(.validate_code_range("A1-2B"))
  expect_error(.validate_code_range("A-"))
  expect_error(.validate_code_range("A-B-D"))
  expect_error(.validate_code_range("11A-A12"))
  expect_equal(.validate_code_range("AB2-AC1"), "AB2-AC1")
  expect_equal(.validate_code_range("1:2", range_sep = ":"), "1:2")
  expect_equal(.validate_code_range("A-B"), "A-B")
  expect_equal(.validate_code_range("12-13"), "12-13")
  expect_equal(.validate_code_range("AB1-AB3"), "AB1-AB3")
  expect_equal(.validate_code_range("1A1-2B3"), "1A1-2B3")
  })

test_that(".clean_codes works", {
  expect_error(.clean_codes(123))
  expect_equal(.clean_codes("A123"), "A123")
  expect_equal(.clean_codes("A1 A2"), c("A1", "A2"))
  expect_equal(.clean_codes(c("A", "B-C")), c("A", "B-C"))
  expect_equal(.clean_codes(c("A.1", "B:C|D;E")), c("A1", "B-C", "D", "E"))
  expect_error(.clean_codes("A$B"))
  expect_equal(.clean_codes("A$B", check_if_clean = FALSE), "A$B")
  expect_equal(.clean_codes("a-B, c"), c("A-B", "C"))
})

