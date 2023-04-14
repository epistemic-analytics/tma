#' simple window decay
#'
#' @param x numeric vector
#' @param args TBD
#'
#' @return numeric vector
#' @export
simple_window <- function(x, args = NULL) { 
  (x <= args$window_size) * 1 
}

#' decay
#'
#' @param what function to use as decay
#' @param ... named parameter list to pass to `what`
#'
#' @return TB
#' @export
decay <- function(what, ...) {
  args <- list(...);
  
  function(x) {
    do.call(what, list(x = x, args = args))
  }
}
