#' Create linear model
#'
#' @param data data.frame
#' @param x column 1
#' @param y column 2
#' @param degree number of degrees
#'
#' @return result of lm(x ~ y + I(y ^ 1) + ... + I(y ^ N)) where N == degree
#' @export
data_to_lm <- function(data, x, y, degree = 4) {
  lm(as.formula(paste0(x, " ~ ", y, " + ", paste("I(", y, paste( "^", seq(degree)[-1]), ")", collapse = " + "))), data = data)
}


#' Convert lm to useable equation
#'
#' @param x results of lm()
#' @param with_assignment logical, default is FALSE, if TRUE (or character string) will return a `y = ` to the left-hand
#' side of the equation
#' @param digits NULL (default) performs no reduction in precision, if an integer, rounds to that decimal place
#'
#' @return character string representing equation
#' @export
lm_to_eq <- function(x, with_assignment = FALSE, digits = NULL) {
  coefs <- x$coefficients;
  
  return(paste0(
    ifelse(with_assignment != FALSE, ifelse(is.logical(with_assignment), "y = ", with_assignment), ""),
    paste(sapply(length(coefs):2, function(coef) {
      paste0(
        "(", format(coefs[coef], digits = digits), " * x", ifelse(coef-1 > 1, paste0("^",coef-1), ""), ")"
      )
    }), collapse = " + "), " + ", format(coefs[1], digits = digits)))
}