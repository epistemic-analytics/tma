#' @title tma accumulates data
#' @description TMA is used to accumulate data for ENA models. and. A more complete description of the methods will be provided with the next release.
#' @name tma
#' @import rENA
#' @importFrom rlang exprs
#' @import data.table
NULL

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
#' @param beta TBD
#' @param indices.only TBD
#'
#' @return TBD
#' @export
tma_orig <- function(
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
  meta.data = NA,
  beta = FALSE,
  indices.only = FALSE
) {
  # hoo rules must be list of lists
  #note that options here are...
  #   ...<UNIT>, which is replaced by the value of the individual unit
  #               for which context is being constructed
  #   ...any regex
  #generate contexts for this data: TMA
  if(beta == FALSE) {
    hoos.TMA = get.contexts(units = units, data = data, hoo.rules = hoo.rules);
  
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
  }
  else {
    # browser()
    # hoos.TMA = get.contexts_dt(units = units, data = data, hoo.rules = hoo.rules);
    if ( is.data.frame(units) ) {
      hoos.TMA = get_contexts(x = data, units_by = colnames(units), rules = hoo.rules, indices.only = indices.only);
    }
    else {
      hoos.TMA = get_contexts(x = data, units_by = units, rules = hoo.rules, indices.only = indices.only);
    }
  
    accumulation = accumulate_units(
      x = data.table::as.data.table(data),
      # context.object = hoos.TMA,
      # units = data.table::as.data.table(units),
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
  }
  return(accumulation);
}

#' Title
#'
#' @param data TBD
#' @param units.by TBD
#' @param hoo.rules TBD
#' @param code.cols TBD
#' @param split.rules TBD
#' @param time.column TBD
#' @param weight.by TBD
#' @param return.ena.set TBD
#' @param decay.functions TBD
#' @param mode.column TBD
#' @param time.limit TBD
#' @param meta.data TBD
#'
#' @return TBD
#' @export
tma <- function(
  data,
  units.by,
  hoo.rules,
  code.cols,
  split.rules = NULL,
  time.column = NULL,
  weight.by = function(x) { x },
  return.ena.set = TRUE,
  decay.functions = list(),
  mode.column = 'Context',
  time.limit = NULL,
  meta.data = units.by
) {
  #model <- units(x = data, by = units.by);
  model <- contexts(
    x = data,
    units = units.by,
    hoo_rules = hoo.rules,
    split_rules = split.rules
  );
  model <- accumulate_contexts(
    x = model, 
    codes = code.cols,
    time.column = time.column,
    weight.by = weight.by,
    return.ena.set = return.ena.set,
    decay.function = decay.functions,
    mode.column = mode.column,
    time.limit = time.limit,
    meta.data = meta.data
  );
  
  return(model);
}