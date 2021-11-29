# Codelist helper functions

#' Make a Definition Template
#'
#' Make a definition template. Intended to be used together with the other
#' codelist helper functions to facilitate creating a codelist.
#'
#' @param code_types list.
#' @param ... Update/add definition elements.
#'
#' @return list
#' @export
#'
#' @examples
#' def_template()
def_template <- function(
    code_types = list(icd8 = "", icd10 = "", atc = "", bio_npu = "", bio_ana = ""),
    ...) {

  args <- list(...)

  x <- list(
    name = "",
    label = "",
    manual_def = FALSE,
    notes = "",
    def_type = c(
      "yes_no",
      "most_recent_date",
      "most_recent_result",
      "median_result",
      "mean_result",
      "max_result",
      "min_result"
    ),
    def_period = c("before", "after"),
    def_length = 0L,
    def_length_unit = c("days"),
    codes_include = code_types,
    codes_exclude = code_types
  )

  for (i in seq_along(args)) {
    x[names(args[i])] <- args[i]
  }

  x
}


#' Add Definition to Codelist
#'
#' Add a definition to a codelist based on a given definition template.
#'
#' @param x list. Codelist definition is added to.
#' @param template list. Codelist template used to set default values.
#' @param name string. Name of definition.
#' @param ... Add/update definition elements.
#'
#' @return list
#' @export
#'
#' @examples
#' codelist <- list() |>
#'   add_def(def_template(), "def")
add_def <- function(x, template, name, ...) {

  args <- list(...)

  x[[name]] <- template
  x[[name]][["name"]] <- name

  for (i in seq_along(args)) {
    x[[name]][names(args[i])] <- args[i]
  }

  x
}


#' Update Codelist Definition
#'
#' Update a definition in a codelist.
#'
#' @param x list. Codelist in which a definition is to be updated
#' @param name string. Name of definition to update.
#' @param var string. Definition element to update.
#' @param value Value assigned to `var`.
#'
#' @return list
#' @export
#'
#' @examples
#' codelist <- list() |>
#'   add_def(def_template(), "def") |>
#'   update_def("def", "manual_def", TRUE)
update_def <- function(x, name, var, value) {
  x[[name]][[var]] <- value
  x
}


#' Update Codes
#'
#' Update a specific type of codes used in a codelist definition.
#'
#' @param x list. Codelist in which a definition is to be updated.
#' @param name string. Name of definition to update.
#' @param type string. Type of codes to update.
#' @param codes Character. Vector of codes.
#' @param include Boolean. Are the codes to update the inclusion codes (TRUE) or
#' the exclusion codes (FALSE)
#'
#' @return list
#' @export
#'
#' @examples
#' codelist <- list() |>
#'   add_def(def_template(), "def") |>
#'   update_codes("def", "icd8", c("123", "124"))
update_codes <- function(x, name, type, codes, include = TRUE) {
  if (include) {
    include_exclude = "codes_include"
  } else {
    include_exclude = "codes_exclude"
  }
  x[[name]][[include_exclude]][[type]] <- codes
  x
}


#' Convert Codelist to data.frame
#'
#' Converts a codelist into a data.frame
#'
#' @param x list. Codelist.
#'
#' @return data.frame
#' @export
#'
#' @examples
#'  template <- def_template()
#'  template["def_type"] <- "yes_no"
#'  template["def_period"] <- "before"
#'  codelist <- list() |>
#'    add_def(template, "def1") |>
#'    add_def(template, "def2") |>
#'    update_codes("def1", "icd8", c("123", "124")) |>
#'    update_codes("def2", "icd10", c("A", "B", "C"), include = FALSE) |>
#'    codelist_to_df()

codelist_to_df <- function(x) {

  for (i in seq_along(names(x))) {
    include_col <- as.data.frame(lapply(x[[i]]$codes_include, paste, collapse = " "))
    names(include_col) <- paste0("codes_include_", names(include_col))
    exclude_col <- as.data.frame(lapply(x[[i]]$codes_exclude, paste, collapse = " "))
    names(exclude_col) <- paste0("codes_exclude", names(exclude_col))
    i_df <- as.data.frame(x[[i]][!names(x[[i]]) %in% c("codes_include", "codes_exclude")])
    i_df <- cbind(i_df, include_col, exclude_col)
    if (i == 1) df <- i_df
    else df <- rbind(df, i_df)
  }

  df
}
