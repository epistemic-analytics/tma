#' Default Decay
#'
#' @description #default decay function, falls from 1-0 over time 4
#' this is used if someone does not specify a decay function, or doesn't know 
#' how to calculate one
#' we could get more creative and let someone specify how long they want the
#' decay to go and then give them a function based on that
#'
#' @param x TBD
#' 
#' @return TBD
#' @export
default.decay.function = function(x) {
  ifelse(
    x < 4,
    return(max(0, (0.0508*x^3 - 0.3035*x^2 + 0.1532*x + 1))),
    return(0)
  )
}