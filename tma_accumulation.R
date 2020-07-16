require(dplyr)
library(dplyr)
require(optimbase)
library(optimbase)

# helpers.R includes the following functions:
  # convert.time
  # call_eq
  # merge.on
  # get.unique.col.values
  # test.df
  # get.quoted.columns
source('./helpers.R');
# decay_functions.R includes the following functions:
  # step_function
  # get.decay
  # get.decay.modes
source('./decay_functions.R');
# contexts.R includes the following functions:
  # get.contexts
source('./contexts.R');
# masks.R includes the following functions:
  # mask
source('./masks.R');
# accumulation.R includes the following functions:
  # accumulate.units
  # default.decay.function
source('./accumulation.R');


tma_accumulation = function(
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