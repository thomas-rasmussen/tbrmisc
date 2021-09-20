#' Check if integer(ish)
#'
#' Check if each element of a numeric vector can be interpreted as integer
#' numbers.
#'
#' @param x a numeric vector
#' @param tol tolerance
#'
#' @return logical vector
#' @export
#'
#' @examples
#' is_integer(1)
#' is_integer(1.1)
is_integer <- function(x, tol = .Machine$double.eps^0.5){
  if(!is.numeric(x)) {
    stop("'x' must be numeric", call. = FALSE)
  }
  abs(x - round(x)) < tol
}
