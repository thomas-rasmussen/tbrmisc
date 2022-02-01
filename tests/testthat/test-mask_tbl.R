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
      c(NA, "<5", "*",
        NA, "<5", "<5", "8 (67%)",
        NA, "<5", "*", "6 (50%)")
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

test_that("categorical variables does not get unnecesarily masked", {
  if (requireNamespace("gtsummary", quietly = TRUE)) {
    dat <- data.frame(var = c(rep(1, 10), rep(2, 10))
    )
    x <- gtsummary::tbl_summary(dat)
    x_masked <- mask_tbl(x)
    expect_equal(x, x_masked)
  }
})

test_that("factor with missing levels is masked correctly", {
  if (requireNamespace("gtsummary", quietly = TRUE)) {
    dat <- data.frame(var = (c(rep("A", 4), rep("E", 10))))
    dat$var <- factor(dat$var, levels = c("A","B", "C", "D", "E"))

    x <- gtsummary::tbl_summary(dat)
    x_masked <- mask_tbl(x)
    expect_equal(
      x_masked$table_body$stat_0,
      c(NA, "<5", "*", "0 (0%)", "0 (0%)", "10 (71%)")
    )
  }
})

test_that("missing rows for continuous variables are properly masked", {
  if (requireNamespace("gtsummary", quietly = TRUE)) {
    dat <- data.frame(
      var1 = c(rep(1, 10), rep(NA, 4)),
      var2 = c(rep(1, 8), rep(NA, 6))
      )

    x <- gtsummary::tbl_summary(
      dat, type = list(var1 ~ "continuous", var2 ~ "continuous")
      )
    x_masked <- mask_tbl(x)
    expect_equal(
      x_masked$table_body$stat_0,
      c("1.0000 (1.0000, 1.0000)", "<5",
      "1.0000 (1.0000, 1.0000)", "6")
    )
  }
})

test_that("values masked to avoid backwards counting", {
  if (requireNamespace("gtsummary", quitely = TRUE)) {
    dat <- data.frame(var = c(1, rep(2, 5), rep(3, 10)))
    x <- gtsummary::tbl_summary(dat)
    x_masked <- mask_tbl(x)
    expect_equal(
      x_masked$table_body$stat_0,
      c(NA, "<5", "*", "10 (62%)")
    )
    dat <- data.frame(var = c(1, 2, rep(3, 10)))
    x <- gtsummary::tbl_summary(dat)
    x_masked <- mask_tbl(x)
    expect_equal(
      x_masked$table_body$stat_0,
      c(NA, "<5", "<5", "10 (83%)")
    )
  }
})
