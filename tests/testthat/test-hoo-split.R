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
simple_decay <- function(x) { (x - 3 <= 0) * 1 }

testthat::test_that("context methods equal", {
  contexts_orig <- get.contexts(units = unique(my.units), data = test.modes, hoo.rules = my.hoo.TMA[1:2])
  
  contexts_beta <- contexts(
    x = test.modes,
    units = colnames(my.units),
    hoo_rules = rules(
      (UserName %in% UNIT$UserName) & (Mode   %in% "View"),
      (GroupName %in% UNIT$GroupName) & (Mode %in% "Chat")
    )
  );
  
  testthat::expect_true(all(
    contexts_orig$contexts$`FirstGame::User1`$context$ID ==
    contexts_beta$model$contexts$`FirstGame::User1`
  ))
})

testthat::test_that("tma methods equal", {
  weight_func <- function(x) {return((x))}
  
  tma_conv_orig = tma_orig(
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
  );
  tma_conv_beta = tma(
    data = test.modes,
    units = colnames(my.units),
    code.cols = codes,
    hoo.rules = rules(
      (UserName %in% UNIT$UserName) & (Mode %in% "View"),
      (GroupName %in% UNIT$GroupName) & (Mode %in% "Chat")
    ),
    weight.by = weight_func,
    return.ena.set = TRUE,
    time.column = NA,
    decay.function = simple_decay,
    mode.column = 'Context',
    time.limit = NA,
    meta.data = c('Condition')
  );
  
  cols <- tma_conv_orig$model$contexts$`FirstGame::User1`$context;
  testthat::expect_equivalent(
    tma_conv_orig$model$contexts$`FirstGame::User1`$context$ID,
    tma_conv_beta$model$contexts$`FirstGame::User1`
  )
  testthat::expect_equivalent(tma_conv_beta$connection.counts, tma_conv_beta$connection.counts)
})
