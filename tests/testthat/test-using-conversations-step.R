testthat::context("Test Conversation Parameter with Step Function")
data(test_rsdata)
RS.data <- data.table::as.data.table(test_rsdata)
RS.data$Context <- "Public"
RS.data$ID <- seq(nrow(RS.data))

codes <- c("Data", "Technical.Constraints", "Performance.Parameters",
  "Client.and.Consultant.Requests", "Design.Reasoning", "Collaboration");

test_window_size = 4
conversations.by = c("Condition", "GroupName");
units.by = c("Condition", "UserName");

my.units = RS.data[, ..units.by]
reg_decays = list(
  Public = step_function(test_window_size)
)
rs_set <- units(RS.data, by = units.by)
rs_set <- contexts(
  x = RS.data,
  units = c("Condition", "UserName"),
  hoo_rules = rules(
    (GroupName %in% UNIT$GroupName) & (Condition == UNIT$Condition)
  )
  ,split_rules = GroupName
)
rs_set <- accumulate_units(
  x = rs_set,
  codes = codes,
  weight.by = function(x) {return((x))},
  return.ena.set = TRUE,
  time.column = "ID",
  decay.function = reg_decays,
  mode.column = 'Context',
  time.limit = NA,
  meta.data = units.by
)
rs_set <- rENA::ena.make.set(rs_set)


accum = rENA::ena.accumulate.data(
  units = my.units,
  conversation = RS.data[, ..conversations.by],
  codes = RS.data[, c(codes), with = FALSE],
  model = "EndPoint",
  window.size.back = test_window_size,
  weight.by = "product"
);
accum_set <- rENA::ena.make.set(accum)
rs_set$connection.counts[1,]
accum$connection.counts[1,]

testthat::test_that("verify results", {
  testthat::expect_equal(as.matrix(rs_set$points), as.matrix(accum_set$points))
  testthat::expect_equal(as.matrix(rs_set$line.weights), as.matrix(accum_set$line.weights))
  testthat::expect_equal(as.matrix(rs_set$rotation$nodes), as.matrix(accum_set$rotation$nodes))
})
