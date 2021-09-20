#' Check if object is a Date object
#'
#' @param x object
#'
#' @return logical
#' @export
#'
#' @examples
#' is_date(as.Date("2000-01-01"))
is_date <- function(x) methods::is(x, "Date")

