#' Make regular expression from character vector
#'
#' Makes a regular expression searching for strings given in the input vector.
#' By default, the regular expression search for strings beginning with the
#' strings from the input vector, but it can also by specified to look for exact
#' matches or strings ending with the strings from the input vector.
#'
#' @param x character vector.
#' @param regex_type Type of regex to make from the input vector.
#'
#' @return string.
#' @export
#'
#' @examples
#' chr_to_regex(c("abc", "def"))
chr_to_regex <- function(
    x,
    regex_type = c("starts_with", "exact", "ends_with")) {

  if (!is.character(x)) {
    stop("'x' must be a character vector", call. = FALSE)
  }
  for (i in seq_along(x)) {
    if (x[i] == "") {
      stop("One or more element of 'x'is character(1), which is not allowed!", call. = FALSE)
    }
  }
  regex_type <- match.arg(regex_type)

  if (length(x) == 0) return(x)

  if (regex_type == "starts_with") {
    x <- paste0(x, collapse = "|^")
    x <- paste0("^", x)
  } else if (regex_type == "exact") {
    x <- paste0(x, collapse = "$|^")
    x <- paste0("^", x, "$")
  } else if (regex_type == "ends_with") {
    x <- paste0(x, collapse = "$|")
    x <- paste0(x, "$")
  }

  x
}

