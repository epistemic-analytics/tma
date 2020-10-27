testthat::context("Test Conversation Parameter with Step Function")
# data(test_rsdata)
# RS.data <- test_rsdata
# 
# RS.data$Context <- "Public"
# RS.data$Row <- seq(nrow(RS.data))
# 
# CommunicationType = sample(x = c("chat","view"), size = nrow(RS.data), replace = TRUE, prob = c(.8, .2))
# RS.data$CommunicationType = CommunicationType
# write.csv(x = RS.data, file = "inst/extdata/rs.data-modes.csv")

# RS.modes <- read.csv("inst/extdata/rs.data-modes.csv", stringsAsFactors = FALSE);
test.modes <- read.csv("../../inst/extdata/test_data_2.csv", stringsAsFactors = FALSE);

my.hoo.TMA = list(
  list(Condition = "<UNIT>", GroupName = "<UNIT>", Mode = "Chat"),
  list(Condition = "<UNIT>", UserName = "<UNIT>", Mode = "View")
  #,list(Split.on = "Day")
  #list(Day = "<UNIT>") # Multiple values auto-split?
)

#someone in my group, I can see chat and facial expressions (but not computer)
# my.hoo.TMA[[2]] = list(Condition = "<UNIT>", GroupName = "<UNIT>", CommunicationType = "face")

#i can see my own computer and chat (but not facial expression)
# my.hoo.TMA[[2]] = list(Condition = "<UNIT>", UserName = "<UNIT>", Mode = "chat")

# my.hoo.TMA[[5]] = list(Condition = "<UNIT>", UserName = "<UNIT>", CommunicationType = "view|chat")
# my.hoo.TMA[[6]] = list(Condition = "<UNIT>", UserName = "<UNIT>", CommunicationType = "!face")
# my.hoo.TMA[[7]] = list(Activity = "*Day")
# my.hoo.TMA[[8]] = list(split = list (Activity = "*Day"))

my.units = test.modes[,c("Condition", "UserName"), drop = FALSE]
# codes <- c(
#   "Data", "Technical.Constraints", "Performance.Parameters",
#   "Client.and.Consultant.Requests", "Design.Reasoning", "Collaboration"
# );
codes <- LETTERS[1:3]
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

# my.conversations = c("Condition", "GroupName")

test_window_size = 4
reg_decays = list(
  Private = step_func(test_window_size),
  Public = step_func(test_window_size)
)


testthat::test_that("context methods equal", {
  contexts_beta <- get_contexts(x = test.modes, units_by = colnames(my.units), rules = my.hoo.TMA[1:2], indices.only = FALSE);
  contexts_orig <- get.contexts(units = unique(my.units), data = test.modes, hoo.rules = my.hoo.TMA[1:2])
  
  testthat::expect_true(all(
    data.table(contexts_orig$contexts$`FirstGame::User1`$context) == 
    contexts_beta$contexts$`FirstGame::User1`$context[[1]]
  ))
})

testthat::test_that("tma methods equal", {
  weight_func <- function(x) {return((x))}
  
  #microbenchmark::microbenchmark(
  tma_conv_orig = tma(
    data = test.modes,
    units = unique(my.units),
    codes = codes,
    hoo.rules = my.hoo.TMA[1:2],
    weight.by = weight_func,
    return.ena.set = TRUE,
    time.column = NA,
    decay.function = reg_decays,
    mode.column = 'Context',
    time.limit = NA,
    # mask = NA,
    meta.data = c('Condition')
    ,beta = FALSE
  ) #,
  tma_conv_beta = tma(
    data = test.modes,
    units = unique(my.units),
    codes = codes,
    hoo.rules = my.hoo.TMA[1:2],
    weight.by = weight_func,
    return.ena.set = TRUE,
    time.column = NA,
    decay.function = reg_decays,
    mode.column = 'Context',
    time.limit = NA,
    # mask = NA,
    meta.data = c('Condition')
    ,beta = TRUE
  )
  #,times = 10)
  
  testthat::expect_equal(tma_conv_orig$meta.data, tma_conv_beta$meta.data)
})