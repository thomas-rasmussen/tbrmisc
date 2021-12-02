# Quiets concerns of R CMD check regarding 'no visible binding
# for global variable ...'
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(".id", ".index_date", ".date", ".code", ".code_type", ".result", ".var_name")
  )
}

#' Define All Variables
#'
#' Define all variables in a codelist using a study population and
#' variable data.
#'
#' @param studypop data.frame. Study population.
#' @param var_dat data.frame. Data used to define variables.
#' @param codelist list. Codelist.
#' @param id_var string. Name of id variable in `studypop` and `var_dat`.
#' @param index_date_var string. Name of index date variable in `studypop`.
#' @param date_var string. Name of variable in `var_dat` with dates
#' corresponding `code_var`.
#' @param code_var string. Name of code variable in in `var_dat`.
#' @param code_type_var string. Name of code type variable in `var_dat`.
#' @param result_var string. Name of result variable in in `var_dat`.
#'
#' @return data.table
#' @export
#'
#' @examples
#' \dontrun{
#' TODO: add example
#' }
define_all_variables <- function(studypop,
                                var_dat,
                                codelist,
                                id_var = "id",
                                index_date_var = "index_date",
                                date_var = "date",
                                code_var = "code",
                                code_type_var = "code_type",
                                result_var = "result") {

  # Check that studypop has required variables
  if (!all(c(id_var, index_date_var) %in% names(studypop))) {
    stop(
      paste("studypop does not have all variables:", id_var, index_date_var),
      call. = FALSE)
  }

  # Check that var_dat has required variables
  if (!all(c(id_var, date_var, code_var, code_type_var, result_var) %in% names(var_dat))) {
    stop(
      paste("var_dat does not have all variables:",
            id_var, date_var, code_var, code_type_var, result_var),
      call. = FALSE)
  }

  studypop <- as.data.table(studypop)
  var_dat <- as.data.table(var_dat)
  setnames(studypop, c(id_var, index_date_var), c(".id", ".index_date"))
  setnames(
    var_dat,
    c(id_var, date_var, code_var, code_type_var, result_var),
    c(".id", ".date", ".code", ".code_type", ".result"))
  studypop <- studypop[, list(.id, .index_date)]
  var_dat <- var_dat[, list(.id, .date, .code, .code_type, .result)]

  # Merge index date to variable data
  var_dat <- merge(var_dat, studypop, by = ".id", all.x = TRUE)

  var_def <- studypop[, list(.id)]
  for (i in seq_along(codelist)) {
    i_def <- define_variable(
      x = var_dat,
      def = codelist[[i]],
      index_date_var = ".index_date",
      id_var = ".id",
      date_var = ".date",
      code_var = ".code",
      code_type_var = ".code_type",
      result_var = ".result"
    )

    var_def <- merge(var_def, i_def, by = ".id", all.x = TRUE)
  }

  setnames(var_def, ".id", id_var)
  var_def
}


#' Define Variable
#'
#' Defines a variable based on variable data and a variable definition.
#'
#' @param x data.frame. Variable data.
#' @param def list. Codelist definition.
#' @param id_var string. Name of id variable in `x`.
#' @param index_date_var string. Name of index date variable in `x`.
#' @param date_var string. Name of variable in `x` with dates
#' corresponding `code_var`.
#' @param code_var string. Name of code variable in `x`.
#' @param code_type_var string. Name of code type variable in `x`.
#' @param result_var string. Name of result variable in `x`.
#'
#' @return data.table
#' @export
#'
#' @examples
#' \dontrun{
#' TODO: add example
#' }
define_variable <- function(x,
                            def,
                            id_var = "id",
                            index_date_var = "index_date",
                            date_var = "date",
                            code_var = "code",
                            code_type_var = "code_type",
                            result_var = "result") {

  # Check 'x' has required variables
  if (!all(c(id_var, index_date_var, date_var, code_var, result_var) %in% names(x))) {
    stop(paste("'x' does not include required variables:",
          id_var, index_date_var, date_var, code_var, result_var), call. = FALSE)
  }

  # Make sure the definition is not a manual definition
  if (def$manual_def) {
    stop("'def' is a manual definition", call. = FALSE)
  }

  # Check that the definition type is recognized
  if (!def$def_type %in% c(
      "first_date", "last_date", "first_result", "last_result", "median_result",
      "mean_result", "max_result", "min_result")) {
    stop(
      paste0("def_type == '", def$def_type, "' is not recognized definition type"),
      call. = FALSE
    )
  }

  # Check valid definition period
  if (!def$def_period %in% c("before", "after")) {
    stop(
      paste0("def_period == '", def$def_period, "' is not valid definition period"),
      call. = FALSE
    )
  }

  # Convert 'x' to a data.table
  x <- data.table::as.data.table(x)

  # Rename variables
  setnames(
    x,
    c(id_var, index_date_var, date_var, code_var, code_type_var, result_var),
    c(".id", ".index_date", ".date", ".code", ".code_type", ".result"))

  # Make a data.table with unique id values.
  x_id <- unique(x[, list(.id)])

  # Restrict data to rows with relevant codes in the relevant period.
  x_filter <- .filter_var_def_dat(
    var_dat = x,
    def = def,
    index_date = ".index_date",
    date_var = ".date",
    code_var = ".code",
    code_type_var = ".code_type"
  )

  # Define variable according to definition type.
  # first_date
  if (def$def_type == "first_date") {
    x_out <- x_filter[, .SD[which.min(.date)], by = .id
      ][, list(.id, .var_name = .date)]

  # last_date
  } else if (def$def_type == "last_date") {
    x_out <- x_filter[, .SD[which.max(.date)], by = .id
      ][, list(.id, .var_name = .date)]
  # first_result
  } else if(def$def_type == "first_result") {
    x_out <- x_filter[, .SD[which.min(.date)], by = .id
      ][, list(.id, .var_name = .result)]
  # last_result
  } else if(def$def_type == "last_result") {
    x_out <- x_filter[, .SD[which.max(.date)], by = .id
      ][, list(.id, .var_name = .result)]
  # median_result
  } else if(def$def_type == "median_result") {
    x_out <- x_filter[, .var_name := stats::median(.result), by = .id
      ][, list(.id, .var_name)][, .SD[1], by = .id]
  # mean_result
  } else if(def$def_type == "mean_result") {
    x_out <- x_filter[, .var_name := mean(.result), by = .id
      ][, list(.id, .var_name)][, .SD[1], by = .id]
  # max_result
  } else if(def$def_type == "max_result") {
    x_out <- x_filter[, .var_name := max(.result), by = .id
      ][, list(.id, .var_name)][, .SD[1], by = .id]
  # min_result
  } else if(def$def_type == "min_result") {
    x_out <- x_filter[, .var_name := min(.result), by = .id
      ][, list(.id, .var_name)][, .SD[1], by = .id]
  }

  # Merge variable to id data.frame.
  x_out <- merge(x_id, x_out, by = ".id", all.x = TRUE)
  setnames(x_out, c(".id", ".var_name"), c(id_var, def$name))

  x_out[]
}


#' Filter Variable Data
#'
#' Takes a variable definition and data on the variable and restricts the
#' data to only include rows that are relevant for defining the variable.
#'
#' @param var_dat data.frame. Date on variable
#' @param def list. Codelist definition.
#' @param index_date string. Name of index date variable in `var_dat`.
#' @param date_var string. Name of variable in `var_dat` with dates
#' corresponding `code_var`.
#' @param code_var string. Name of code variable in `var_dat`.
#' @param code_type_var string. Name of code type variable `var_dat`.
#'
#' @return data.table
#' @keywords internal
#'
#' @examples
#' \dontrun{TODO: add example}
.filter_var_def_dat <- function(var_dat,
                                def,
                                index_date = "index_date",
                                date_var = "date",
                                code_var = "code",
                                code_type_var = "code_type") {

  # Make sure 'var_dat' is a data.table
  var_dat <- data.table::as.data.table(var_dat)

  # Make filter string expressions for restricting 'var_dat' to rows
  # with codes that are relevant to the definition of the variable,
  # that are also in the correct period.
  period_exp <- .period_filter_exp(
    def,
    index_date = index_date,
    date_var = date_var
  )
  code_exp <- .code_filter_exp(
    def,
    code_var = code_var,
    code_type_var = code_type_var)

  # Combine the string filter expressions, and filter the data.
  if (code_exp == "") {
    text <- paste0("var_dat[", period_exp, "]")
  } else {
    text <- paste0(
      "var_dat[(",
      period_exp,
      ") & (",
      code_exp,
      ")]"
    )
  }

  eval(parse(text = text))
}


#' Make Period Filter String Expression
#'
#' Takes a codelist definition and creates a string that can be used as an
#' expression restrict variable data to rows with codes relevant for the
#' definition.
#'
#' @param def list. codelist definition.
#' @param index_date string. Name of index date variable.
#' @param date_var string. Name of variable with code dates.
#'
#' @return string
#' @keywords internal
#'  def <- list() |>
#'  add_def(def_template(), "var") |>
#'    update_def("var", "def_period", "before") |>
#'    update_def("var", "def_length", 10)
#'  def <- def$var
#'  .period_filter_exp(def)
#' @examples
#' \dontrun{
#' TODO: add example
#' }
.period_filter_exp <- function(def,
                              index_date = "index_date",
                              date_var = "date") {
  if(def$def_period == "before") {
    paste0(
      date_var,
      " < ",
      index_date,
      " & ",
      index_date,
      " <= (",
      date_var,
      " + ",
      def$def_length,
      ")"
    )
  } else if (def$def_period == "after") {
    paste0(
      index_date,
      " <= ",
      date_var,
      " & ",
      date_var,
      " <= (",
      index_date,
      " + ",
      def$def_length,
      ")"
    )
  }
}


#' Make Code Filter String Expression
#'
#' Takes a codelist definition and creates a string that can be used as an
#' expression to restrict variable data to rows with codes relevant for
#' the definition.
#'
#' @param def list. codelist definition.
#' @param code_type_var string. Name of code type variable.
#' @param code_var string. Name of code variable
#'
#' @return string
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'  def <- list() |>
#'  add_def(def_template(), "var") |>
#'    update_codes("var", "icd8", c("1", "2"))
#'  def <- def$var
#'  .code_filter_exp(def)
#' }
.code_filter_exp <- function(def,
                            code_type_var = "code_type",
                            code_var = "code") {

  # Build the expression, one code type at a time
  codes_inc <- def$codes_include
  codes_exc <- def$codes_exclude
  exp <- ""
  for (i in seq_along(codes_inc)) {
    i_type <- names(codes_inc)[i]
    # For most kind of codes we also want all subcodes, but for some types of
    # codes, we want exact matches.
    if (i_type %in% c("bio_npu", "bio_ana")) {
      regex_type <- "exact"
    } else {
      regex_type <- "starts_with"
    }
    i_inc <- codes_inc[[i_type]]
    i_exc <- codes_exc[[i_type]]

    # Build code type part
    i_type_exp <- paste0(code_type_var, " == ", "'", i_type, "'")

    # Build inclusion codes part
    if (paste0(i_inc, collapse = " ") != "") {
      i_inc_exp <- paste0(
        "grepl('", chr_to_regex(i_inc, regex_type = regex_type),
        "', ",
        code_var,
        ")"
      )
    } else {
      i_inc_exp <- ""
    }

    # Build exclusion codes part
    if (paste0(i_exc, collapse = " ") != "") {
      i_exc_exp <- paste0(
        "!grepl('", chr_to_regex(i_exc, regex_type = regex_type),
        "', ",
        code_var,
        ")"
      )
    } else {
      i_exc_exp <- ""
    }

    # Combine parts
    if (i_inc_exp == "" & i_exc_exp == "") {
      next()
    } else if (i_inc_exp == "") {
      i_exp <- paste0(
        "(",
        i_type_exp,
        " & ",
        i_exc_exp,
        ")"
      )
    } else if (i_exc_exp == "") {
      i_exp <- paste0(
        "(",
        i_type_exp,
        " & ",
        i_inc_exp,
        ")"
      )
    } else {
      i_exp <- paste0(
        "(",
        i_type_exp,
        " & ",
        i_inc_exp,
        " & ",
        i_exc_exp,
        ")"
      )
    }

    if (exp == "") exp <- i_exp
    else exp <- paste0(exp, " | ", i_exp)
  }

  exp
}
