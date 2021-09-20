#' Smooth periods
#'
#' Smooth data with time periods, so that data lines with periods in
#' continuation of each others are combined into a single data line.
#'
#' @param x data.table
#' @param start string. Variable name of numeric/date variable with start dates.
#' @param end string. Variable names of numeric/date variable with end dates.
#' @param group character vector. Names of grouping variables.
#' @param max_gap Non-negative integer. Maximum allowed gap between time-periods
#'   when smoothing.
#'
#' @return data.table
#' @export
#' @import data.table
#'
#' @examples
#' 2 + 2
smooth_periods <- function(x,
                           start = "start",
                           end = "end",
                           group = NULL,
                           max_gap = 1L) {

  #### Input checks ####

  if (!data.table::is.data.table(x)) {
    stop("'x' is not a data.table")
  }
  if (!(is.character(start) & length(start) == 1)) {
    stop("'start' must be a character vector of length 1")
  }
  if (!is.numeric(x[[start]]) & !is_date(x[[start]])) {
    stop("'start' must refer to a a numeric/date variable in 'x'")
  }
  if (!(is.character(end) & length(end) == 1)) {
      stop("'end' must be a character vector of length 1")
  }
  if (!is.numeric(x[[end]]) & !is_date(x[[end]])) {
      stop("'end' must refer to a numeric/date variable in 'x'")
  }
  if (!is.null(group)) {
    if (!is.character(group)) {
      stop("'group' must be a character vector")
    }
    if (!all(group %in% names(x))) {
      error_msg <- paste0(
        "One or more variable names specified in 'group' does not ",
        "exist in 'x'"
        )
      stop(error_msg, call. = FALSE)
    }
  }
  if (!(length(max_gap) == 1 & is_integer(max_gap) & max_gap >= 0)) {
    stop("'max_gap' must be a non-negative integer")
  }


  #### Prepare data ####

  # Restrict to necessary variables
  x <- x[, c(group, start, end), with = FALSE]

  # Sort data according to grouping and date variables
  data.table::setorderv(x, c(group, start, end))

  # Construct function call to add variables identifying rows with the
  # start/end of a series of periods that are in continuation of each other
  group_conds <- character(0)
  for (i in seq_along(group)) {
    group_conds = paste0(
      group_conds,
      " | ",
      group[i],
      " != data.table::shift(",
      group[i],
      ")"
    )
  }

  txt <- paste0(
    "x[, `:=`(.new_start = ",
    "is.na(data.table::shift(",
    end,
    ")) | ",
    start,
    " > (data.table::shift(",
    end,
    ") + ",
    max_gap,
    ")",
    group_conds,
    ")][, .new_end := data.table::shift(.new_start, n = -1L, fill = TRUE)]"
  )

  eval(parse(text = txt))

  #### Smooth ####

  # Construct call to data.table() to make smoothed data
  txt <- paste0(
    start,
    " = x$",
    start,
    "[x$.new_start]",
    ", ",
    end,
    " = x$",
    end,
    "[x$.new_end]"
  )

  for (i in seq_along(group)) {
    txt <- paste0(group[i], " = x$", group[i], "[x$.new_start], ", txt)
  }

  txt <- paste0("data.table::data.table(", txt, ")")

  eval(parse(text = txt))
}
