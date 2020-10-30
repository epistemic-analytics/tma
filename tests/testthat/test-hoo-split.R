testthat::context("Test Updated TMA wrapper")

# test.mode.path <- system.file(package = "tma", "extdata/test_data_2.csv");
# test.modes <- read.csv("./inst/extdata/test_data_2.csv", stringsAsFactors = FALSE);
data("test_data_2")
test.modes <- test_data_2; #read.csv(test.mode.path, stringsAsFactors = FALSE);
test.modes$ID = seq(nrow(test.modes))

my.hoo.TMA = list(
  list(Condition = "<UNIT>", GroupName = "<UNIT>", Mode = "Chat"),
  list(Condition = "<UNIT>", UserName = "<UNIT>", Mode = "View")
)

my.units = test.modes[,c("Condition", "UserName"), drop = FALSE]
codes <- LETTERS[1:3]

test_window_size = 4
reg_decays = list(
  Private = step_function(test_window_size),
  Public = step_function(test_window_size)
)

testthat::test_that("context methods equal", {
  contexts_orig <- get.contexts(units = unique(my.units), data = test.modes, hoo.rules = my.hoo.TMA[1:2])
  
  contexts_beta <- contexts(
    x = test.modes,
    units = colnames(my.units),
    hoo_rules = rules(
      (UserName == UNIT$UserName) & (Mode == "View"),
      (GroupName %in% UNIT$GroupName) & (Mode == "Chat")
    )
  );
  
  testthat::expect_true(all(
    data.table(contexts_orig$contexts$`FirstGame::User1`$context) == 
    contexts_beta$model$contexts$`FirstGame::User1`[, colnames(contexts_orig$contexts$`FirstGame::User1`$context), with = FALSE]
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
  tma_conv_beta = tma2(
    data = test.modes,
    units = colnames(my.units),
    code.cols = codes,
    hoo.rules = rules(
      (UserName == UNIT$UserName) & (Mode == "View"),
      (GroupName %in% UNIT$GroupName) & (Mode == "Chat")
    ),
    weight.by = weight_func,
    return.ena.set = TRUE,
    time.column = NA,
    decay.function = reg_decays,
    mode.column = 'Context',
    time.limit = NA,
    meta.data = c('Condition')
  )
  #,times = 10)
  
  cols <- tma_conv_orig$model$contexts$`FirstGame::User1`$context;
  testthat::expect_equal(
    as.data.table(tma_conv_orig$model$contexts$`FirstGame::User1`$context),
    tma_conv_beta$model$contexts$`FirstGame::User1`[, c(cols)]
  )
})
