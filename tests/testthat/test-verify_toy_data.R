library(testthat)
data("test_data", package = "tma")
d = tma::test_data

my.hoo.TMA = list(
  list(Condition = "<UNIT>", GroupName = "<UNIT>", Context = "Public"),
  list(Condition = "<UNIT>", UserName = "<UNIT>")
)
my.units = unique(d[,c("Condition", "UserName")])
codes = c('A','B','C')
modal.decay.function = list(
  "Private" = step_function(window_size = 2),
  "Public" = step_function(window_size = 2)
)

accumulation_window_2 = tma::tma(
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
accumulation_window_beta = tma::tma(
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
  meta.data = c('Condition'),
  beta = TRUE
)

dimnames = list(LETTERS[1:3], LETTERS[1:3])
User1_directed = matrix(c(
  0,0,1,
  0,0,0,
  0,0,0
),3,3, dimnames=dimnames, byrow = TRUE)
User2_directed = matrix(c(
  0,0,0,
  0,0,0.5,
  0,1.5,0
),3,3, dimnames=dimnames, byrow = TRUE)
User3_directed = matrix(c(
  0,0,0.5,
  2,0,1,
  1.5,0,0
),3,3, dimnames=dimnames, byrow = TRUE)

User1_undirected = matrix(c(
  0,0,1,
  NA,0,0,
  NA,NA,0
),3,3, dimnames=dimnames, byrow = TRUE)

User2_undirected = matrix(c(
  0,0,0,
  NA,0,2,
  NA,NA,0
),3,3, dimnames=dimnames, byrow = TRUE)

User3_undirected = matrix(c(
  0,2,2,
  NA,0,1,
  NA,NA,0
),3,3, dimnames=dimnames, byrow = TRUE)

test_that("directed adjacency", {
  testthat::expect_equal(
    User1_directed, accumulation_window_2$model$contexts$`FirstGame::User1`$directed.adjacency
  )
  testthat::expect_equal(
    User2_directed, accumulation_window_2$model$contexts$`FirstGame::User2`$directed.adjacency
  )
  testthat::expect_equal(
    User3_directed, accumulation_window_2$model$contexts$`FirstGame::User3`$directed.adjacency
  )
})

test_that("directed adjacency new get_context", {
  testthat::expect_equal(
    User1_directed, accumulation_window_beta$model$contexts$`FirstGame::User1`$directed.adjacency
  )
  testthat::expect_equal(
    User2_directed, accumulation_window_beta$model$contexts$`FirstGame::User2`$directed.adjacency
  )
  testthat::expect_equal(
    User3_directed, accumulation_window_beta$model$contexts$`FirstGame::User3`$directed.adjacency
  )
})

test_that("undirected adjacency", {
  testthat::expect_equal(
    User1_undirected, accumulation_window_2$model$contexts$`FirstGame::User1`$undirected.adjacancy
  )
  testthat::expect_equal(
    User2_undirected, accumulation_window_2$model$contexts$`FirstGame::User2`$undirected.adjacancy
  )
  testthat::expect_equal(
    User3_undirected, accumulation_window_2$model$contexts$`FirstGame::User3`$undirected.adjacancy
  )
})

test_that("undirected adjacency new get context", {
  testthat::expect_equal(
    User1_undirected, accumulation_window_beta$model$contexts$`FirstGame::User1`$undirected.adjacancy
  )
  testthat::expect_equal(
    User2_undirected, accumulation_window_beta$model$contexts$`FirstGame::User2`$undirected.adjacancy
  )
  testthat::expect_equal(
    User3_undirected, accumulation_window_beta$model$contexts$`FirstGame::User3`$undirected.adjacancy
  )
})
