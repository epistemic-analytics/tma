# test.modes <- data.table::fread("./inst/extdata/test_data_2.csv", stringsAsFactors = FALSE);
data("test_data_2")
test.modes <- test_data_2;
tma_test <- units( x = test.modes, by = c("Condition", "UserName") );
tma_test <- contexts(
  x = tma_test, 
  units = c("Condition", "UserName"),
  hoo_rules = rules(
    (UserName == UNIT$UserName) & (Mode == "View"),
    (GroupName %in% UNIT$GroupName) & (Mode == "Chat")
    ,(((To == UNIT$UserName) | UserName == UNIT$UserName) & (Mode == "DM"))
  )
  # ,split_rules = Day
)

test_window_size <- 4
reg_decays <- list(
  Private = step_function(test_window_size),
  Public = step_function(test_window_size)
)
simple_window <- function(x) { (x <= 3) * 1 }

testthat::test_that("simple_decay", {
  tma_test1 <- accumulate_contexts(
      x = tma_test,
      codes = LETTERS[1:3],
      weight.by = function(x) { x },
      return.ena.set = TRUE,
      decay.function = simple_window,
      mode.column = 'Context'
  );
  tma_test2 <- accumulate_contexts(
      x = tma_test,
      codes = LETTERS[1:3],
      weight.by = function(x) { x },
      return.ena.set = TRUE,
      decay.function = reg_decays,
      mode.column = 'Context'
  );
  
  testthat::expect_equal(tma_test1$connection.counts, tma_test2$connection.counts);
})

testthat::test_that("using_wrapper", {
  tma_test2 <- accumulate_contexts(
      x = tma_test,
      codes = LETTERS[1:3],
      weight.by = function(x) { x },
      return.ena.set = TRUE,
      decay.function = reg_decays,
      mode.column = 'Context'
  );
  tma_set2 <- tma(
    data = test.modes,
    units.by = c("Condition", "UserName"),
    code.cols = LETTERS[1:3],
    hoo.rules = rules(
      (UserName == UNIT$UserName) & (Mode == "View"),
      (GroupName %in% UNIT$GroupName) & (Mode == "Chat"),
      (((To == UNIT$UserName) | UserName == UNIT$UserName) & (Mode == "DM"))
    ),
    decay.functions = simple_window
  );
  
  testthat::expect_equal(tma_set2$connection.counts, tma_test2$connection.counts);
})
