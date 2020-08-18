my.hoo.TMA = list()
my.hoo.TMA[[1]] = list(Condition = "<UNIT>", GroupName = "<UNIT>", Context = "Public")
my.hoo.TMA[[2]] = list(Condition = "<UNIT>", UserName = "<UNIT>")
my.units = unique(d[,c("Condition", "UserName")])
codes = c('A','B','C')
modal.decay.function = list()
modal.decay.function$Private = step_function(window_size = 2);
modal.decay.function$Public = step_function(window_size = 2);
accumulation_window = tma(
  data = d,
  units = my.units,
  codes = codes,
  hoo.rules = my.hoo.TMA,
  weight.by = function(x) {return(x)},
  return.ena.set = TRUE,
  time.column = NA,
  decay.function = modal.decay.function,
  mode.column = 'Context',
  time.limit = NA,
  meta.data = c('Condition'),
  beta = FALSE
)

my.hoo.TMA = list()
my.hoo.TMA[[1]] = list(Condition = "*", GroupName = "*", Context = "Public")
my.hoo.TMA[[2]] = list(Condition = "*", UserName = "*")
my.units = unique(d[,c("Condition", "UserName")])
codes = c('A','B','C')
modal.decay.function = list()
modal.decay.function$Private = step_function(window_size = 2);
modal.decay.function$Public = step_function(window_size = 2);
accumulation_window_2 = tma(
  data = d,
  units = my.units,
  codes = codes,
  hoo.rules = my.hoo.TMA,
  weight.by = function(x) {return(x)},
  return.ena.set = TRUE,
  time.column = NA,
  decay.function = modal.decay.function,
  mode.column = 'Context',
  time.limit = NA,
  meta.data = c('Condition'),
  beta = TRUE
)
