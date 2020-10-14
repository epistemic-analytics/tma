data("test_data", package = "tma")
d = tma::test_data

d$time = convert.time( as.character(d$Timestamp), units = "minutes");
my.hoo.TMA = list()
my.hoo.TMA[[1]] = list(Condition = "<UNIT>", GroupName = "<UNIT>", Context = "Public")  
my.hoo.TMA[[2]] = list(Condition = "<UNIT>", UserName = "<UNIT>")
my.units = unique(d[,c("Condition", "UserName")])
codes = c('A','B','C')
modal.decay.function = list()
modal.decay.function$Private = continuous.decay.function.over.ten.minutes;
modal.decay.function$Public = continuous.decay.function.over.ten.minutes;
accumulation_with_time_column = tma(
  data = d,
  units = my.units,
  codes = codes,
  hoo.rules = my.hoo.TMA,
  weight.by = function(x) {return(x*1)},
  return.ena.set = TRUE,
  time.column = 'time',
  decay.function = modal.decay.function,
  mode.column = 'Context',
  time.limit = 7,
  # mask = NA,
  meta.data = c('Condition')
)

test_that("directed_continuous_decay_use_time_column_test", {
  User1_directed = t(matrix(c(
    0,0,1,
    0,0,.5,
    0,0,.5
  ),3,3))
  colnames(User1_directed) = rownames(User1_directed) = c('A','B','C')
  testthat::expect_equal(
    User1_directed, accumulation_with_time_column$model$contexts$`FirstGame::User1`$directed.adjacency
  )
  
  User2_directed = t(matrix(c(
    0,.5,0,
    0,0,0.5,
    0,1.5,0
  ),3,3))
  colnames(User2_directed) = rownames(User2_directed) = c('A','B','C')
  testthat::expect_equal(
    User2_directed, accumulation_with_time_column$model$contexts$`FirstGame::User2`$directed.adjacency
  )
  
  User3_directed = t(matrix(c(
    0,0,0.5,
    2,0,1,
    2,0,.5
  ),3,3))
  colnames(User3_directed) = rownames(User3_directed) = c('A','B','C')
  testthat::expect_equal(
    User3_directed, accumulation_with_time_column$model$contexts$`FirstGame::User3`$directed.adjacency
  )
})

test_that("undirected_continuous_decay_use_time_column_test", {
  User1_undirected = t(matrix(c(
    0,0,1,
    NA,0,.5,
    NA,NA,.5
  ),3,3))
  colnames(User1_undirected) = rownames(User1_undirected) = c('A','B','C')
  testthat::expect_equal(
    User1_undirected, accumulation_with_time_column$model$contexts$`FirstGame::User1`$undirected.adjacancy
  )
  
  User2_undirected = t(matrix(c(
    0,.5,0,
    NA,0,2,
    NA,NA,0
  ),3,3))
  colnames(User2_undirected) = rownames(User2_undirected) = c('A','B','C')
  testthat::expect_equal(
    User2_undirected, accumulation_with_time_column$model$contexts$`FirstGame::User2`$undirected.adjacancy
  )
  
  User3_undirected = t(matrix(c(
    0,2,2.5,
    NA,0,1,
    NA,NA,.5
  ),3,3))
  colnames(User3_undirected) = rownames(User3_undirected) = c('A','B','C')
  testthat::expect_equal(
    User3_undirected, accumulation_with_time_column$model$contexts$`FirstGame::User3`$undirected.adjacancy
  )
})
