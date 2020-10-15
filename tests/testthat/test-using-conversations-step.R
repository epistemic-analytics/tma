testthat::context("Test Conversation Parameter with Step Function")
RS.data <- read.csv(system.file("extdata/rs.data.csv", package = "tma"), stringsAsFactors = FALSE)
RS.data$Context <- "Public"
RS.data$Row <- seq(nrow(RS.data))

my.hoo.TMA = list(
  list(Condition = "<UNIT>", GroupName = "<UNIT>", Context = "Public")
)
my.units = RS.data[,c("Condition", "UserName"), drop = FALSE]
codes <- c("Data", "Technical.Constraints", "Performance.Parameters",
  "Client.and.Consultant.Requests", "Design.Reasoning", "Collaboration");
weighted_step_func <- function(window_size) {
  return(Vectorize(function(x) {
    scalar = 1;
    if (x == 0) {
      return(1*scalar);
    } else if( x < window_size && x > 0) {
      return(1*scalar);
    } else {
      return(0);
    }
  }, "x"))
}
step_func <- function(window_size) {
  return(Vectorize(function(x) {
    if (x == 0) {
      return(1);
      # return(0.5);
    } else if( x < window_size && x > 0) {
      return(1);
    } else {
      return(0);
    }
  }, "x"))
}

test_window_size = 4
my.conversations = c("Condition", "GroupName")

# weighted_decays = list(
#   Private = weighted_step_func(test_window_size),
#   Public = weighted_step_func(test_window_size)
# )
reg_decays = list(
  # Private = step_func(test_window_size),
  Public = step_func(test_window_size)
)
# tma_reg = tma(
#   data = RS.data,
#   units = unique(my.units),
#   codes = codes,
#   hoo.rules = my.hoo.TMA,
#   weight.by = function(x) {return((x))},
#   return.ena.set = TRUE,
#   time.column = NA,
#   decay.function = reg_decays,
#   mode.column = 'Context',
#   time.limit = NA,
#   # mask = NA,
#   meta.data = c('Condition')
#   # ,conversations = my.conversations
# )
tma_conv = tma(
  data = RS.data,
  units = unique(my.units),
  codes = codes,
  hoo.rules = my.hoo.TMA,
  weight.by = function(x) {return((x))},
  return.ena.set = TRUE,
  time.column = NA,
  decay.function = reg_decays,
  mode.column = 'Context',
  time.limit = NA,
  # mask = NA,
  meta.data = c('Condition')
  ,conversations = my.conversations
)

# tma_reg_set <- ena.make.set(tma_reg)
tma_conv_set <- ena.make.set(tma_conv)

accum = ena.accumulate.data(
  units = my.units,
  conversation = RS.data[, my.conversations, drop = FALSE],
  codes = RS.data[, codes, drop = FALSE],
  model = "EndPoint",
  window.size.back = test_window_size
  ,weight.by = "product"
);
accum_set <- ena.make.set(accum)


test_that("verify results", {
  testthat::expect_equal(as.matrix(tma_conv_set$points), as.matrix(accum_set$points))
  testthat::expect_equal(as.matrix(tma_conv_set$line.weights), as.matrix(accum_set$line.weights))
  testthat::expect_equal(as.matrix(tma_conv_set$rotation$nodes), as.matrix(accum_set$rotation$nodes))
})
