#### define_all_variables ####

.add_dat <- function(id, code, date, code_type, result  = NA, x = NULL) {
  new_row <- data.frame(
    id = id,
    code = code,
    date = as.Date(date),
    code_type = code_type,
    result = result
  )
  if (is.null(x)) {
    return(new_row)
  } else {
    return(rbind(x, new_row))
  }
}

.make_def <- function(name, def_type, def_period, def_length, inc = list(), exc = list()) {
  def <- def_template()
  def$name <- name
  def[["def_type"]] <- def_type
  def[["def_period"]] <- def_period
  def[["def_length"]] <- def_length
  for (i in seq_along(inc)) {
    i_type <- names(inc)[i]
    def[["codes_include"]][[i_type]] <- inc[[i]]
    def[["codes_exclude"]][[i_type]] <- exc[[i]]
  }
  def
}

test_that("define_all_variables throws error with improper input", {
  # studypop does not have required variables
  var_dat <- .add_dat(1, "A", "2000-01-01", "icd10")
  codelist <- list(.make_def("var", "first_date", "before", 0))
  studypop <- data.frame(id = 1, index_date = as.Date("2000-01-01"))
  expect_error(define_all_variables(studypop, var_dat, codelist, id_var = "id1"))
  studypop$id <- NULL
  expect_error(define_all_variables(studypop, var_dat, codelist))
  # var_dat does not have required variables
  var_dat <- .add_dat(1, "A", "2000-01-01", "icd10")
  codelist <- list(.make_def("var", "first_date", "before", 0))
  studypop <- data.frame(id = 1, index_date = as.Date("2000-01-01"))
  expect_error(define_all_variables(studypop, var_dat, codelist, date_var = "date1"))
  var_dat$date <- NULL
  expect_error(define_all_variables(studypop, var_dat, codelist))
})

test_that("define_all_variables correctly defines variables", {
  codelist <- list()
  codelist$cov1 <- .make_def(
    "cov1", "last_date", "before", 365, list(icd10 = "A"), list(icd10 = "")
  )
  codelist$cov2 <- .make_def(
    "cov2", "median_result", "before", 90, list(bio_npu = "NPU123"), list(bio_npu = "")
  )
  codelist$out1 <- .make_def(
    "out1", "first_date", "after", 10 * 365, list(atc = "B"), list(atc = "")
  )
  studypop <- data.frame(id = 1:4, index_date = rep(as.Date("2000-01-01"), 4))
  var_dat <- .add_dat(1, "A1", "1999-10-01", "icd10")
  var_dat <- .add_dat(2, "NPU123", "1999-11-01", "bio_npu", 1, x = var_dat)
  var_dat <- .add_dat(3, "NPU1234", "1999-11-01", "bio_npu", 2, x = var_dat)
  var_dat <- .add_dat(4, "B1", "2005-01-01", "atc", x = var_dat)
  expect_equal(
    as.data.frame(define_all_variables(studypop, var_dat, codelist)),
    data.frame(
      id = 1:4,
      cov1 = c(as.Date("1999-10-01"), as.Date(NA), as.Date(NA), as.Date(NA)),
      cov2 = c(NA, 1, NA, NA),
      out1 = c(as.Date(NA), as.Date(NA), as.Date(NA), as.Date("2005-01-01"))
    )
  )


})

#### define_variable ####

.add_row <- function(id, index_date, code, date, code_type, result  = NA, x = NULL) {
  new_row <- data.frame(
    id = id,
    index_date = as.Date(index_date),
    code = code,
    date = as.Date(date),
    code_type = code_type,
    result = result
  )
  if (is.null(x)) {
    return(new_row)
  } else {
    return(rbind(x, new_row))
  }
}

.make_def <- function(def_type, def_period, def_length, inc = list(), exc = list()) {
  def <- def_template()
  def[["def_type"]] <- def_type
  def[["def_period"]] <- def_period
  def[["def_length"]] <- def_length
  for (i in seq_along(inc)) {
    i_type <- names(inc)[i]
    def[["codes_include"]][[i_type]] <- inc[[i]]
    def[["codes_exclude"]][[i_type]] <- exc[[i]]
  }
  def
}

test_that("define_variable throws error when improper input", {
  # 'x' does not have the required variables
  x <- .add_row(1, "2000-01-01", "A", "2000-01-01", "icd10")
  def <- .make_def("first_date", "before", 0)
  expect_error(define_variable(x, def), NA)
  expect_error(define_variable(x, def, id_var = "invalid"))
  x <- x[, !names(x) == "id"]
  expect_error(define_variable(x, def))
  # def is a manual definition
  x <- .add_row(1, "2000-01-01", "A", "2000-01-01", "icd10")
  def <- .make_def("first_date", "before", 0)
  def$manual_def <- TRUE
  expect_error(define_variable(x, def))
  # Unrecognized definition type
  x <- .add_row(1, "2000-01-01", "A", "2000-01-01", "icd10")
  def <- .make_def("invalid", "before", 0)
  expect_error(define_variable(x, def))
  # Invalid definition period
  x <- .add_row(1, "2000-01-01", "A", "2000-01-01", "icd10")
  def <- .make_def("first_date", "invalid", 0)
  expect_error(define_variable(x, def))
})

test_that("define_variables works with non-default variable names", {
  x <- .add_row(1, "2000-01-01", "A", "2000-01-01", "icd10")
  names(x) <- c("id1", "index_date1", "code1", "date1", "code_type1", "result1")
  def <- .make_def("first_date", "before", 0)
  expect_error(define_variable(
    x,
    def,
    id_var = "id1",
    index_date_var = "index_date1",
    code_var = "code1",
    date_var = "date1",
    code_type_var = "code_type1",
    result_var = "result1"
  ), NA)
})

test_that("define_variable names the variable using the definition name", {
  x <- .add_row(1, "2000-01-01", "A", "2000-01-01", "icd10")
  def <- .make_def("first_date", "before", 0)
  expect_equal(names(define_variable(x, def)), c("id", ""))
  def$name <- "var"
  expect_equal(names(define_variable(x, def)), c("id", "var"))
})

# first_date
test_that("define_variable 'first_date' definition types works correctly", {
  def <- .make_def("first_date", "after", 365, list(icd8 = "1"), list(icd8 = "12"))
  def$name <- "var"
  x <- .add_row(1, "2000-01-01", "1", "2000-01-01", "icd8")
  x <- .add_row(2, "2000-01-01", "1", "2000-01-01", "icd8", x = x)
  x <- .add_row(2, "2000-01-01", "1", "2000-01-02", "icd8", x = x)
  x <- .add_row(3, "2000-01-01", "1", "1999-01-01", "icd8", x = x)
  x <- .add_row(4, "2000-01-01", "1", "2002-01-01", "icd8", x = x)
  x <- .add_row(5, "2000-01-01", "12", "2000-01-01", "icd8", x = x)
  x <- .add_row(5, "2000-01-01", "1", "2000-01-02", "icd8", x = x)
  expect_equal(
    as.data.frame(define_variable(x, def)),
    data.frame(
      id = 1:5,
      var = c(
        as.Date("2000-01-01"),
        as.Date("2000-01-01"),
        as.Date(NA),
        as.Date(NA),
        as.Date("2000-01-02"))
    )
  )
})

# last_date
test_that("define_variable 'last_date' definition types works correctly", {
  def <- .make_def("last_date", "before", 100, list(icd8 = "1"), list(icd8 = "12"))
  def$name <- "var"
  x <- .add_row(1, "2000-01-01", "1", "1999-11-01", "icd8")
  x <- .add_row(2, "2000-01-01", "1", "1999-11-01", "icd8", x = x)
  x <- .add_row(2, "2000-01-01", "1", "1999-11-02", "icd8", x = x)
  x <- .add_row(3, "2000-01-01", "1", "1999-01-01", "icd8", x = x)
  x <- .add_row(4, "2000-01-01", "1", "2002-01-01", "icd8", x = x)
  x <- .add_row(5, "2000-01-01", "12", "1999-12-01", "icd8", x = x)
  x <- .add_row(5, "2000-01-01", "1", "1999-11-01", "icd8", x = x)
  expect_equal(
    as.data.frame(define_variable(x, def)),
    data.frame(
      id = 1:5,
      var = c(
        as.Date("1999-11-01"),
        as.Date("1999-11-02"),
        as.Date(NA),
        as.Date(NA),
        as.Date("1999-11-01"))
    )
  )
})

# first_result
test_that("define_variable 'first_result' definition types works correctly", {
  def <- .make_def("first_result", "after", 365, list(icd8 = "1"), list(icd8 = "12"))
  def$name <- "var"
  x <- .add_row(1, "2000-01-01", "1", "2000-01-01", "icd8", 1)
  x <- .add_row(2, "2000-01-01", "1", "2000-01-01", "icd8", 1, x = x)
  x <- .add_row(2, "2000-01-01", "1", "2000-01-02", "icd8", 2, x = x)
  x <- .add_row(3, "2000-01-01", "1", "1999-01-01", "icd8", 1, x = x)
  x <- .add_row(4, "2000-01-01", "1", "2002-01-01", "icd8", 1, x = x)
  x <- .add_row(5, "2000-01-01", "12", "2000-01-01", "icd8", 1, x = x)
  x <- .add_row(5, "2000-01-01", "1", "2000-01-02", "icd8", 2, x = x)
  expect_equal(
    as.data.frame(define_variable(x, def)),
    data.frame(id = 1:5, var = c(1, 1, NA, NA, 2))
  )
})

# last_result
test_that("define_variable 'last_result' definition types works correctly", {
  def <- .make_def("last_result", "before", 100, list(icd8 = "1"), list(icd8 = "12"))
  def$name <- "var"
  x <- .add_row(1, "2000-01-01", "1", "1999-11-01", "icd8", 1)
  x <- .add_row(2, "2000-01-01", "1", "1999-11-01", "icd8", 1, x = x)
  x <- .add_row(2, "2000-01-01", "1", "1999-11-02", "icd8", 2, x = x)
  x <- .add_row(3, "2000-01-01", "1", "1999-01-01", "icd8", 1, x = x)
  x <- .add_row(4, "2000-01-01", "1", "2002-01-01", "icd8", 1, x = x)
  x <- .add_row(5, "2000-01-01", "12", "1999-12-01", "icd8", 1,  x = x)
  x <- .add_row(5, "2000-01-01", "1", "1999-11-01", "icd8", 2, x = x)
  expect_equal(
    as.data.frame(define_variable(x, def)),
    data.frame(id = 1:5, var = c(1, 2, NA, NA, 2))
  )
})

# median_result
test_that("define_variable 'median_result' definition types works correctly", {
  def <- .make_def("median_result", "after", 365, list(icd8 = "1"), list(icd8 = "12"))
  def$name <- "var"
  x <- .add_row(1, "2000-01-01", "1", "2000-01-01", "icd8", 1)
  x <- .add_row(2, "2000-01-01", "1", "2000-01-01", "icd8", 1, x = x)
  x <- .add_row(2, "2000-01-01", "1", "2000-01-02", "icd8", 2, x = x)
  x <- .add_row(2, "2000-01-01", "1", "2000-01-02", "icd8", 6, x = x)
  x <- .add_row(3, "2000-01-01", "1", "1999-01-01", "icd8", 1, x = x)
  x <- .add_row(4, "2000-01-01", "1", "2002-01-01", "icd8", 1, x = x)
  x <- .add_row(5, "2000-01-01", "12", "2000-01-01", "icd8", 1, x = x)
  x <- .add_row(5, "2000-01-01", "1", "2000-01-02", "icd8", 2, x = x)
  expect_equal(
    as.data.frame(define_variable(x, def)),
    data.frame(id = 1:5, var = c(1, 2, NA, NA, 2))
  )
})

# mean_result
test_that("define_variable 'mean_result' definition types works correctly", {
  def <- .make_def("mean_result", "after", 365, list(icd8 = "1"), list(icd8 = "12"))
  def$name <- "var"
  x <- .add_row(1, "2000-01-01", "1", "2000-01-01", "icd8", 1)
  x <- .add_row(2, "2000-01-01", "1", "2000-01-01", "icd8", 1, x = x)
  x <- .add_row(2, "2000-01-01", "1", "2000-01-02", "icd8", 2, x = x)
  x <- .add_row(2, "2000-01-01", "1", "2000-01-02", "icd8", 6, x = x)
  x <- .add_row(3, "2000-01-01", "1", "1999-01-01", "icd8", 1, x = x)
  x <- .add_row(4, "2000-01-01", "1", "2002-01-01", "icd8", 1, x = x)
  x <- .add_row(5, "2000-01-01", "12", "2000-01-01", "icd8", 1, x = x)
  x <- .add_row(5, "2000-01-01", "1", "2000-01-02", "icd8", 2, x = x)
  expect_equal(
    as.data.frame(define_variable(x, def)),
    data.frame(id = 1:5, var = c(1, 3, NA, NA, 2))
  )
})

# max_result
test_that("define_variable 'max_result' definition types works correctly", {
  def <- .make_def("max_result", "after", 365, list(icd8 = "1"), list(icd8 = "12"))
  def$name <- "var"
  x <- .add_row(1, "2000-01-01", "1", "2000-01-01", "icd8", 1)
  x <- .add_row(2, "2000-01-01", "1", "2000-01-01", "icd8", 1, x = x)
  x <- .add_row(2, "2000-01-01", "1", "2000-01-02", "icd8", 2, x = x)
  x <- .add_row(2, "2000-01-01", "1", "2000-01-02", "icd8", 6, x = x)
  x <- .add_row(3, "2000-01-01", "1", "1999-01-01", "icd8", 1, x = x)
  x <- .add_row(4, "2000-01-01", "1", "2002-01-01", "icd8", 1, x = x)
  x <- .add_row(5, "2000-01-01", "12", "2000-01-01", "icd8", 1, x = x)
  x <- .add_row(5, "2000-01-01", "1", "2000-01-02", "icd8", 2, x = x)
  expect_equal(
    as.data.frame(define_variable(x, def)),
    data.frame(id = 1:5, var = c(1, 6, NA, NA, 2))
  )
})

# min_result
test_that("define_variable 'min_result' definition types works correctly", {
  def <- .make_def("min_result", "after", 365, list(icd8 = "1"), list(icd8 = "12"))
  def$name <- "var"
  x <- .add_row(1, "2000-01-01", "1", "2000-01-01", "icd8", 1)
  x <- .add_row(2, "2000-01-01", "1", "2000-01-01", "icd8", 1, x = x)
  x <- .add_row(2, "2000-01-01", "1", "2000-01-02", "icd8", 2, x = x)
  x <- .add_row(2, "2000-01-01", "1", "2000-01-02", "icd8", 6, x = x)
  x <- .add_row(3, "2000-01-01", "1", "1999-01-01", "icd8", 1, x = x)
  x <- .add_row(4, "2000-01-01", "1", "2002-01-01", "icd8", 1, x = x)
  x <- .add_row(5, "2000-01-01", "12", "2000-01-01", "icd8", 1, x = x)
  x <- .add_row(5, "2000-01-01", "1", "2000-01-02", "icd8", 2, x = x)
  expect_equal(
    as.data.frame(define_variable(x, def)),
    data.frame(id = 1:5, var = c(1, 1, NA, NA, 2))
  )
})


#### .filter_var_def_dat ####

.add_row <- function(id, index_date, code, date, code_type, misc_var = NA, x = NULL) {
  new_row <- data.frame(
    id = id,
    index_date = as.Date(index_date),
    code = code,
    date = as.Date(date),
    code_type = code_type,
    misc_var = misc_var
  )
  if (is.null(x)) {
    return(new_row)
  } else {
    return(rbind(x, new_row))
  }
}

.make_def <- function(def_period, def_length, inc = list(), exc = list()) {
  def <- def_template()
  def[["def_period"]] <- def_period
  def[["def_length"]] <- def_length
  for (i in seq_along(inc)) {
    i_type <- names(inc)[i]
    def[["codes_include"]][[i_type]] <- inc[[i]]
    def[["codes_exclude"]][[i_type]] <- exc[[i]]
  }
  def
}

test_that(".filter_var_def_dat works", {
  def <- .make_def("after", "10", list(icd8 = "1"), list(icd8 = ""))
  var_dat <- .add_row(1, "2000-01-01", "1", "2000-01-01", "icd8")
  # Check that the return object is a data.table
  expect_true(data.table::is.data.table(.filter_var_def_dat(var_dat, def)))
  # Check that the number of columns is the same as in var_dat
  expect_equal(ncol(.filter_var_def_dat(var_dat, def)), ncol(var_dat))
  # Check that rows outside the period of interest are removed
  def <- .make_def("after", "10", list(icd8 = "1"), list(icd8 = ""))
  var_dat <- .add_row(1, "2000-01-01", "1", "2000-01-01", "icd8")
  var_dat <- .add_row(1, "2000-01-01", "1", "1999-01-01", "icd8", x = var_dat)
  var_dat <- .add_row(1, "2000-01-01", "1", "2000-01-11", "icd8", x = var_dat)
  var_dat <- .add_row(1, "2000-01-01", "1", "2000-01-12", "icd8", x = var_dat)
  var_dat <- .add_row(1, "2000-01-01", "1", "2000-01-12", "icd8", x = var_dat)
  expect_equal(nrow(.filter_var_def_dat(var_dat, def)), 2)
  # Check that rows with irrelevant codes are removed
  def <- .make_def("after", "10", list(icd8 = "12"), list(icd8 = "123"))
  var_dat <- .add_row(1, "2000-01-01", "12", "2000-01-01", "icd8")
  var_dat <- .add_row(1, "2000-01-01", "123", "2000-01-01", "icd8", x = var_dat)
  var_dat <- .add_row(1, "2000-01-01", "12", "2000-01-01", "icd10", x = var_dat)
  var_dat <- .add_row(1, "2000-01-01", "234", "2000-01-01", "icd10", x = var_dat)
  expect_equal(nrow(.filter_var_def_dat(var_dat, def)), 1)
  # Check only period restriction works
  def <- .make_def("after", "10")
  var_dat <- .add_row(1, "2000-01-01", "12", "2000-01-01", "icd8")
  var_dat <- .add_row(1, "2000-01-01", "12", "2000-02-01", "icd8", x = var_dat)
  expect_equal(nrow(.filter_var_def_dat(var_dat, def)), 1)
  # Check return empty data.table with same columns if no relevant rows in
  # 'var_dat'
  def <- .make_def("after", "10")
  var_dat <- .add_row(1, "2000-01-01", "1", "2001-01-01", "icd8")
  expect_true(data.table::is.data.table(.filter_var_def_dat(var_dat, def)))
  expect_equal(nrow(.filter_var_def_dat(var_dat, def)), 0)
  expect_equal(ncol(.filter_var_def_dat(var_dat, def)), ncol(var_dat))
})


#### .period_filter_exp ####

.make_def <- function(def_period, def_length) {
  def <- def_template()
  def[["def_period"]] <- def_period
  def[["def_length"]] <- def_length
  def
}

test_that(".period_filter_exp works", {
  # Test def_period values
  def <- .make_def("before", 0)
  expect_equal(
    .period_filter_exp(def),
    "date < index_date & index_date <= (date + 0)"
  )
  def <- .make_def("before", 10)
  expect_equal(
    .period_filter_exp(def),
    "date < index_date & index_date <= (date + 10)"
  )
  def <- .make_def("after", 0)
  expect_equal(
    .period_filter_exp(def),
    "index_date <= date & date <= (index_date + 0)"
  )
  def <- .make_def("after", 10)
  expect_equal(
    .period_filter_exp(def),
    "index_date <= date & date <= (index_date + 10)"
  )
  # Variable names
  def <- .make_def("before", 0)
  expect_equal(
    .period_filter_exp(def, index_date = "var1", date = "var2"),
    "var2 < var1 & var1 <= (var2 + 0)"
  )
  def <- .make_def("after", 0)
  expect_equal(
    .period_filter_exp(def, index_date = "var1", date = "var2"),
    "var1 <= var2 & var2 <= (var1 + 0)"
  )
})


#### .code_filter_exp ####

.make_def <- function(inc = list(), exc = list()) {
  def <- def_template()
  for (i in seq_along(inc)) {
    i_type <- names(inc)[i]
    def[["codes_include"]][[i_type]] <- inc[[i]]
    def[["codes_exclude"]][[i_type]] <- exc[[i]]
  }
  def
}

test_that(".code_filter_exp works", {
  # No codes
  def <- .make_def()
  expect_equal(.code_filter_exp(def), "")
  # One code type, one inclusion codes
  def <- .make_def(list(icd8 = "1"), list(icd8 = ""))
  expect_equal(
    .code_filter_exp(def), "(code_type == 'icd8' & grepl('^1', code))"
  )
  # One code type, multiple inclusion codes
  def <- .make_def(list(icd8 = c("1", "2")), list(icd8 = ""))
  expect_equal(
    .code_filter_exp(def),
    "(code_type == 'icd8' & grepl('^1|^2', code))"
  )
  # One code type, one inclusion and exclusion code
  def <- .make_def(list(icd8 = "1"), list(icd8 = "2"))
  expect_equal(
    .code_filter_exp(def),
    "(code_type == 'icd8' & grepl('^1', code) & !grepl('^2', code))"
  )
  # Check biomarker NPU and analysis codes uses exact matching
  def <- .make_def(list(bio_npu = "NPU123"), list(bio_npu = ""))
  expect_equal(
    .code_filter_exp(def),
    "(code_type == 'bio_npu' & grepl('^NPU123$', code))"
  )
  def <- .make_def(list(bio_ana = "123"), list(bio_ana = ""))
  expect_equal(
    .code_filter_exp(def),
    "(code_type == 'bio_ana' & grepl('^123$', code))"
  )
  # Multiple code types
  def <- .make_def(
    list(icd10 = "A", bio_ana = "123"),
    list(icd10 = "B", bio_ana = "")
  )
  expect_equal(
    .code_filter_exp(def),
    paste0("(code_type == 'icd10' & grepl('^A', code) & ",
      "!grepl('^B', code)) | (code_type == 'bio_ana' & ",
      "grepl('^123$', code))")
  )
  # Variable names
   def <- .make_def(list(icd8 = "1"), list(icd8 = ""))
   expect_equal(
     .code_filter_exp(def, code_type_var = "var1", code_var = "var2"),
     "(var1 == 'icd8' & grepl('^1', var2))"
   )
})
