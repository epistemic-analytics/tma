testthat::context("Test Conversation Parameter with Step Function")
data(test_rsdata)
RS.data <- data.table::as.data.table(test_rsdata)
RS.data$Context <- "Public"
# RS.data$ID <- seq(nrow(RS.data))

codes <- c("Data", "Technical.Constraints", "Performance.Parameters",
  "Client.and.Consultant.Requests", "Design.Reasoning", "Collaboration");

test_window_size = 4
conversations.by = c("Condition", "GroupName");
units.by = c("Condition", "UserName");

my.units = RS.data[, ..units.by]
reg_decays = list(
  Public = function(x) {
    if( x >= 0 && x < 4) {
      return(1);
    } else {
      return(0);
    }
  }
)
simple_decay <- function(x) { (x - 3 <= 0) * 1 }

rs_set <- units(RS.data, by = units.by)
rs_set <- contexts(
  x = rs_set,
  units = c("Condition", "UserName"),
  hoo_rules = rules(
    (GroupName %in% UNIT$GroupName) & (Condition %in% UNIT$Condition)
  )
  # ,split_rules = ActivityNumber
);
rs_set <- accumulate_contexts(
  x = rs_set,
  codes = codes,
  mode.column = 'Context',
  weight.by = function(x) {return((x))},
  decay.function = simple_decay,
  return.ena.set = TRUE
)
rs_set <- rENA::ena.make.set(rs_set);

accum = rENA::ena.accumulate.data(
  units = my.units,
  conversation = RS.data[, ..conversations.by],
  codes = RS.data[, c(codes), with = FALSE],
  model = "EndPoint",
  window.size.back = test_window_size,
  weight.by = "product"
);
accum_set <- rENA::ena.make.set(accum);

testthat::test_that("verify results", {
  testthat::expect_equal(as.matrix(rs_set4$points), as.matrix(accum_set$points))
  testthat::expect_equal(as.matrix(rs_set4$line.weights), as.matrix(accum_set$line.weights))
  testthat::expect_equal(as.matrix(rs_set4$rotation$nodes), as.matrix(accum_set$rotation$nodes))
})
