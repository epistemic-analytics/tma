#' TMA wrapper function
#'
#' @param data TBD
#' @param units TBD
#' @param codes TBD
#' @param hoo.rules TBD 
#' @param weight.by TBD
#' @param return.ena.set TBD
#' @param time.column TBD
#' @param decay.function TBD
#' @param mode.column TBD
#' @param time.limit TBD
#' @param mask TBD
#' @param meta.data TBD
#'
#' @return TBD
#' @export
tma <- function(
  data,
  units,
  codes,
  hoo.rules,
  weight.by = sqrt,
  return.ena.set = TRUE,
  time.column = NA,  #if no time column is given, use model with equal time = 1 between lines in a hoo
  decay.function = NA,
  mode.column = NA,
  time.limit = NA,
  mask = NA,
  meta.data = NA
) {
  # hoo rules must be list of lists
  #note that options here are...
  #   ...<UNIT>, which is replaced by the value of the individual unit
  #               for which context is being constructed
  #   ...any regex
  #generate contexts for this data: TMA
  hoos.TMA = get.contexts(units = units, data = data, hoo.rules = hoo.rules);
  # browser()
  accumulation = accumulate.units(
    context.object = hoos.TMA,
    codes = codes,
    time.column = time.column,
    decay.function = decay.function,
    mode.column = mode.column,
    weight.by = weight.by,
    time.limit = time.limit,
    meta.data = meta.data,
    mask = mask,
    return.ena.set = return.ena.set
  )
  return(accumulation);
}