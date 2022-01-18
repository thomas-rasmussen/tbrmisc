test_that("input must have class 'tbl_summary'", {
  if (requireNamespace("gtsummary", quietly = TRUE)) {
    dat <- data.frame(var = 1:10)
    expect_error(mask_tbl(dat))
    x <- gtsummary::tbl_summary(dat)
    expect_error(mask_tbl(x), NA)
  }
})

test_that("continuous variables are not masked", {
  if (requireNamespace("gtsummary", quietly = TRUE)) {
    dat <- data.frame(
      cont_var = c(1, 2, 3)
    )
    x <- gtsummary::tbl_summary(dat, type = list(cont_var ~ "continuous"))
    x_masked <- mask_tbl(x)
    expect_equal(x, x_masked)
  }
})

test_that("masking binary variables works", {
  if (requireNamespace("gtsummary", quietly = TRUE)) {
    dat <- data.frame(
      bin_var1 = c(rep(1, 5), rep(0, 5)),
      bin_var2 = c(rep(1, 4), rep(0, 6))
    )
    x <- gtsummary::tbl_summary(dat)
    x_masked <- mask_tbl(x)
    expect_equal(x_masked$table_body$stat_0, c("5 (50%)", "<5"))
  }
})

test_that("masking categorical variables works", {
  if (requireNamespace("gtsummary", quietly = TRUE)) {
    dat <- data.frame(
      cat_var1 = c(rep(1, 4), rep(2, 8)),
      cat_var2 = c(rep(1, 2), rep(2, 2), rep(3, 8)),
      cat_var3 = c(rep(1, 1), rep(2, 5), rep(3, 6))
    )
    x <- gtsummary::tbl_summary(dat)
    x_masked <- mask_tbl(x)
    expect_equal(
      x_masked$table_body$stat_0,
      c(NA, "<5", "<5",
        NA, "<5", "<5", "8 (67%)",
        NA, "<5", "<5", "6 (50%)")
    )
  }
})


test_that("using tbl_summary() by-argument works", {
  if (requireNamespace("gtsummary", quietly = TRUE)) {
    dat <- data.frame(
      group = c(rep(1, 5), rep(2, 5)),
      cont_var = 0:1,
      bin_var = 0:1,
      cat_var = 0:1
    )
    x <- gtsummary::tbl_summary(
      dat,
      by = "group",
      type = list(cont_var ~ "continuous", bin_var ~ "dichotomous", cat_var ~ "categorical")
    )
    x_masked <- mask_tbl(x)
    expect_equal(
      x_masked$table_body$stat_1,
      c("0 (0, 1)", "<5", NA, "<5", "<5")
    )
    expect_equal(
      x_masked$table_body$stat_2,
      c("1 (0, 1)", "<5", NA, "<5", "<5")
    )
  }
})

