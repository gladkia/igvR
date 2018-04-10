#----------------------------------------------------------------------------------------------------
#' printf
#'
#' \code{printf} emulates the standard C library function of the same name
#'
#' @rdname printf
#'
#' @param ... a format string followed by a matching number of scalar values
#'
#' @return a string
#'
#' @export
#'
#' @examples
#'   printf("%s: %04d, %10.8f", "emulating standard C library", 10, pi)
#'

printf <- function(...) print(noquote(sprintf(...)))
