testthat::context("Test conversation rule == warning");

data("RS.data");
units_by <- c("Condition", "UserName");

count_warnings <- function(expr) {
  msgs <- character(0);
  withCallingHandlers(expr, warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w));
    invokeRestart("muffleWarning")
  });
  msgs
}

test_that("unit_equality_columns finds UNIT columns compared with ==", {
  expect_equal(tma:::unit_equality_columns(quote(a == UNIT$a & UNIT[["b"]] == b & c %in% UNIT$c)), c("a", "b"));
  expect_equal(tma:::unit_equality_columns(quote((a %in% UNIT$a))), character(0));
  expect_equal(tma:::unit_equality_columns(rlang::exprs((a == UNIT$a))), "a");
})

test_that("== against a multi-valued UNIT column warns once per rule", {
  # Some RS.data users appear in two GroupNames, so UNIT$GroupName has 2 values.
  rules <- conversation_rules((Condition == UNIT$Condition & GroupName == UNIT$GroupName));
  msgs <- count_warnings(contexts(RS.data, units_by = units_by, hoo_rules = rules));
  expect_length(msgs, 1);
  expect_match(msgs, "`GroupName`");
  expect_match(msgs, "%in%", fixed = TRUE);
})

test_that("%in% rules and single-valued == comparisons do not warn", {
  rules_in <- conversation_rules((Condition %in% UNIT$Condition & GroupName %in% UNIT$GroupName));
  expect_length(count_warnings(contexts(RS.data, units_by = units_by, hoo_rules = rules_in)), 0);
  # Condition is part of the unit, so UNIT$Condition always has a single value.
  rules_cond <- conversation_rules((Condition == UNIT$Condition));
  expect_length(count_warnings(contexts(RS.data, units_by = units_by, hoo_rules = rules_cond)), 0);
})

test_that("the warning is also raised when data.table rejects an unparenthesized ==", {
  rules <- conversation_rules(GroupName == UNIT$GroupName);
  msgs <- character(0);
  expect_error(withCallingHandlers(
    contexts(RS.data, units_by = units_by, hoo_rules = rules),
    warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") }
  ));
  expect_length(msgs, 1);
  expect_match(msgs, "`GroupName`");
})
