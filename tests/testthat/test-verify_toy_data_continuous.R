testthat::context("Test continuous data")

test_that("verify models", {
  data("test_data", package = "tma")
  d = tma::test_data
  d$ID <- seq(nrow(d))
  my.hoo.TMA = list()
  my.hoo.TMA[[1]] = list(Condition = "<UNIT>", GroupName = "<UNIT>", Context = "Public")  
  my.hoo.TMA[[2]] = list(Condition = "<UNIT>", UserName = "<UNIT>")
  my.units = unique(d[,c("Condition", "UserName")])
  codes = c('A','B','C')
  modal.decay.function = list()
  modal.decay.function$Private = continuous_decay_function_window_3_approx;
  modal.decay.function$Public = continuous_decay_function_window_3_approx;
  accumulation_window_3 = tma_orig(
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
    # mask = NA,
    meta.data = c('Condition')
  )
  
  # hoo_rules = rules(
  #   (UserName %in% UNIT$UserName) & (Mode %in% "View"),
  #   (GroupName %in% UNIT$GroupName) & (Mode %in% "Chat")
  # )
  accumulation_window_beta = tma(
    data = d,
    units.by = colnames(my.units),
    code.cols = codes,
    hoo.rules = rules(
      (GroupName %in% UNIT$GroupName) & (Condition == UNIT$Condition) & (Context == "Public"),
      (Condition == UNIT$Condition) & (UserName == UNIT$UserName) & (Context == "Private")
    ),
    weight.by = function(x) {return(x)},
    return.ena.set = TRUE,
    time.column = NA,
    decay.function = modal.decay.function,
    mode.column = 'Context',
    time.limit = NA,
    meta.data = c('Condition')
  )
  
  dimnames = list(LETTERS[1:3], LETTERS[1:3])
  User1_directed = matrix(c(
    0,0,1,
    0,0,.5,
    0,0,.5
  ),3,3, dimnames=dimnames, byrow = TRUE)
  User2_directed = matrix(c(
    0,.5,0,
    0,0,0.5,
    0,1.5,0
  ),3,3, dimnames=dimnames, byrow = TRUE)
  User3_directed = matrix(c(
    0,0,0.5,
    2,0,1,
    2,0,.5
  ),3,3, dimnames=dimnames, byrow = TRUE)
  
  User1_undirected = matrix(c(
    0,0,1,
    NA,0,.5,
    NA,NA,.5
  ),3,3, dimnames=dimnames, byrow = TRUE)
  User2_undirected = matrix(c(
    0,.5,0,
    NA,0,2,
    NA,NA,0
  ),3,3, dimnames=dimnames, byrow = TRUE)
  User3_undirected = matrix(c(
    0,2,2.5,
    NA,0,1,
    NA,NA,.5
  ),3,3, dimnames=dimnames, byrow = TRUE)

  testthat::expect_equal(
    User1_directed, accumulation_window_3$model$contexts$`FirstGame::User1`$directed.adjacency
  )
  testthat::expect_equal(
    User2_directed, accumulation_window_3$model$contexts$`FirstGame::User2`$directed.adjacency
  )
  testthat::expect_equal(
    User3_directed, accumulation_window_3$model$contexts$`FirstGame::User3`$directed.adjacency
  )

  testthat::expect_equal(
    User1_undirected, accumulation_window_3$model$contexts$`FirstGame::User1`$undirected.adjacency
  )
  testthat::expect_equal(
    User2_undirected, accumulation_window_3$model$contexts$`FirstGame::User2`$undirected.adjacency
  )
  testthat::expect_equal(
    User3_undirected, accumulation_window_3$model$contexts$`FirstGame::User3`$undirected.adjacency
  )
  
  cols <- colnames(accumulation_window_3$model$contexts$`FirstGame::User1`$context);
  testthat::expect_equivalent(
    accumulation_window_3$model$contexts$`FirstGame::User1`$context$ID,
    accumulation_window_beta$model$contexts$`FirstGame::User1`
  )
  testthat::expect_equivalent(
    accumulation_window_3$model$contexts$`FirstGame::User2`$context$ID,
    accumulation_window_beta$model$contexts$`FirstGame::User2`
  )
  testthat::expect_equivalent(
    accumulation_window_3$model$contexts$`FirstGame::User3`$context$ID,
    accumulation_window_beta$model$contexts$`FirstGame::User3`
  )
})
