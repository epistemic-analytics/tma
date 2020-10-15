unit_cols <- c("Condition", "GroupName", "UserName")

d <- tma::test_data
my.units = unique(d[, unit_cols])

d_grp2 <- data.table::copy(d)
d_grp2$GroupName <- "Group_2"
rbind(d, d_grp2)
d2 <- rbind(d, d_grp2)

my.units2 = unique(d2[, unit_cols])

codes = c('A','B','C')
modal.decay.function = list()
modal.decay.function$Private = step_function(window_size = 2);
modal.decay.function$Public = step_function(window_size = 2);

weight_by_func <- function(x) {return(x)}

my.hoo.TMA = list()
my.hoo.TMA[[1]] = list(Condition = "<UNIT>", GroupName = "<UNIT>", Context = "Public")
my.hoo.TMA[[2]] = list(Condition = "<UNIT>", GroupName = "<UNIT>", UserName = "<UNIT>")
accumulation_window = tma(
  data = d2,
  units = my.units2,
  codes = codes,
  hoo.rules = my.hoo.TMA,
  weight.by = weight_by_func,
  return.ena.set = TRUE,
  time.column = NA,
  decay.function = modal.decay.function,
  mode.column = 'Context',
  time.limit = NA,
  meta.data = c('Condition'),
  beta = FALSE
)

wc.hoo.TMA = list()
wc.hoo.TMA[[1]] = list(Condition = "*", GroupName = "*", Context = "Public")
wc.hoo.TMA[[2]] = list(Condition = "*", GroupName = "*", UserName = "*")
accumulation_window_2 = tma(
  data = d2,
  units = my.units2,
  codes = codes,
  hoo.rules = wc.hoo.TMA,
  weight.by = weight_by_func,
  return.ena.set = TRUE,
  time.column = NA,
  decay.function = modal.decay.function,
  mode.column = 'Context',
  time.limit = NA,
  meta.data = c('Condition'),
  beta = TRUE
)


test_that("directed adjacencys", {
  User1_directed = t(matrix(c(
    0,0,1,
    0,0,0,
    0,0,0
  ),3,3))
  colnames(User1_directed) = rownames(User1_directed) = c('A','B','C')
  testthat::expect_equal(
    User1_directed, accumulation_window$model$contexts$`FirstGame::Group_1::User1`$directed.adjacency
  )
  testthat::expect_equal(
    User1_directed, accumulation_window_2$model$contexts$`FirstGame::Group_1::User1`$directed.adjacency
  )
  
  User2_directed = t(matrix(c(
    0,0,0,
    0,0,0.5,
    0,1.5,0
  ),3,3))
  colnames(User2_directed) = rownames(User2_directed) = c('A','B','C')
  testthat::expect_equal(
    User2_directed, accumulation_window$model$contexts$`FirstGame::Group_1::User2`$directed.adjacency
  )
  testthat::expect_equal(
    User2_directed, accumulation_window_2$model$contexts$`FirstGame::Group_1::User2`$directed.adjacency
  )
  
  User3_directed = t(matrix(c(
    0,0,0.5,
    2,0,1,
    1.5,0,0
  ),3,3))
  colnames(User3_directed) = rownames(User3_directed) = c('A','B','C')
  testthat::expect_equal(
    User3_directed, accumulation_window$model$contexts$`FirstGame::Group_1::User3`$directed.adjacency
  )
  testthat::expect_equal(
    User3_directed, accumulation_window_2$model$contexts$`FirstGame::Group_1::User3`$directed.adjacency
  )
})


test_that("undirected adjacency", {
  User1_undirected = t(matrix(c(
    0,0,1,
    NA,0,0,
    NA,NA,0
  ),3,3))
  colnames(User1_undirected) = rownames(User1_undirected) = c('A','B','C')
  testthat::expect_equal(
    User1_undirected, accumulation_window_2$model$contexts$`FirstGame::Group_1::User1`$undirected.adjacancy
  )
  
  User2_undirected = t(matrix(c(
    0,0,0,
    NA,0,2,
    NA,NA,0
  ),3,3))
  colnames(User2_undirected) = rownames(User2_undirected) = c('A','B','C')
  testthat::expect_equal(
    User2_undirected, accumulation_window_2$model$contexts$`FirstGame::Group_1::User2`$undirected.adjacancy
  )
  
  User3_undirected = t(matrix(c(
    0,2,2,
    NA,0,1,
    NA,NA,0
  ),3,3))
  colnames(User3_undirected) = rownames(User3_undirected) = c('A','B','C')
  testthat::expect_equal(
    User3_undirected, accumulation_window_2$model$contexts$`FirstGame::Group_1::User3`$undirected.adjacancy
  )
})
