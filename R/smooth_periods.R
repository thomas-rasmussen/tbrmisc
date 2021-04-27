#' Smooth periods
#'
#' Smooth data with time periods, so that data lines with periods in
#' continuation of each others a combined into a single data line.
#'
#' @param x data.table
#' @param start string. Variable name with start dates.
#' @param end string. Variable names with end dates.
#' @param group character vector. Names of grouping variables.
#' @param keep character vector. Names of auxiliary variables to keep.
#' @param max_gap Positive numeric value. Maximum gap allowed when
#'   smoothing lines.
#'
#' @return data.table
#' @export
#'
#' @examples
#' 2 + 2
smooth_periods <- function(x,
                           start,
                           end,
                           group = NULL,
                           keep = NULL,
                           max_gap = 1L) {

  #### Input checks ####
  if (!data.table::is.data.table(x)) {
    stop("`x` is not a data.frame")
  }
  if (!(is.character(start) & length(start) == 1)) {
    stop("`start` must be a character vector of length 1")
  }
  if (!is.numeric(x[[start]])) {
    stop("`start` must refer to a a numeric variable in `x`")
  }
  if (!(is.character(end) & length(end) == 1)) {
      stop("`start` must be a character vector of length 1")
  }
  if (!is.numeric(x[[end]])) {
      stop("`end` must refer to a numeric variable in `x`")
  }
  if (!is.null(group)) {
    if (!(is.character(group) & length(group) >= 1)) {
      stop("`group` must be a character vector of length >= 1")
    }
    if (!all(group %in% names(x))) {
      error_msg <- paste0(
        "One or more variable names specified in `group` does not ",
        "exist in `x`"
        )
      stop(error_msg)
    }
  }
  if (!is.null(keep)) {
    if (!(is.character(keep) & length(keep) >= 1)) {
      stop("`group` must be a character vector of length >= 1")
    }
    if (!all(keep %in% names(x))) {
      error_msg <- paste0(
        "One or more variable names specified in `keep` does not ",
        "exist in `x`"
      )
      stop(error_msg)
    }
  }
  if (!(length(max_gap) == 1 & is.numeric(max_gap) & max_gap > 0)) {
    stop("`max_gap` must be a positive numeric value")
  }


  #### Prepare data ####

  # Restrict to necessary variables
  dat <- x[, c(group, start, end, keep), with = FALSE]


  # Sort data according to grouping and date variables
  if (is.null(group)) {
    order_vars <- c(start, end)
  } else {
    order_vars <- c(group, start, end)
  }
  data.table::setorderv(dat, order_vars)

  # Construct function call to add variables identifying rows with the
  # start/end of a series of periods that are in continuation of each other
  group_conds <- ""
  for (i in 1:length(group)) {
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
    "dat[, `:=`(.new_start = ",
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
    " = dat$",
    start,
    "[dat$.new_start]",
    ", ",
    end,
    " = dat$",
    end,
    "[dat$.new_end]"
  )
  if (!is.null(group)) {
    for (i in 1:length(group)) {
      txt <- paste0(group[i], " = dat$", group[i], "[dat$.new_start], ", txt)
    }
  }
  if (!is.null(keep)) {
    txt <- paste0(txt, ", ", keep, " = dat$", keep, "[dat$.new_start]" )
  }
  txt <- paste0("data.table::data.table(", txt, ")")

  eval(parse(text = txt))
}
