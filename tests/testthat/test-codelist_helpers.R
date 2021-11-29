test_that("def_template works", {
  expect_error(def_template(), NA)
  expect_error(def_template(manual_def = TRUE), NA)
  expect_error(def_template(extra_info = "Something extra"), NA)
})

test_that("add_def works", {
  codelist <- list()
  template <- def_template()
  expect_error(add_def(codelist, template, "def"), NA)
  expect_error(add_def(codelist, template, "def", manual_def = TRUE), NA)
  expect_error(add_def(codelist, template, "def", test = "test"), NA)
})

test_that("update_def works", {
  codelist <- list() |>
    add_def(def_template(), "def")
  expect_error(update_def(codelist, "def", "manual_def", TRUE), NA)
})

test_that("update_codes works", {
  codelist <- list() |>
    add_def(def_template(), "def")
  expect_error(update_codes(codelist, "def", "icd8", c("123", "124")), NA)
})

test_that("codelist_to_df works", {
  template <- def_template()
  template["def_type"] <- "yes_no"
  template["def_period"] <- "before"
  codelist <- list() |>
    add_def(template, "def1") |>
    add_def(template, "def2") |>
    update_codes("def1", "icd8", c("123", "124")) |>
    update_codes("def2", "icd10", c("A", "B", "C"), include = FALSE)

  expect_error(codelist_to_df(codelist), NA)
})
