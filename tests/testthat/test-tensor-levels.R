testthat::context("Tensor factor levels are looked up in the tensor, not per context");

# accumulate() must map each sender / receiver / mode value to the tensor slice
# for that level. It used to re-encode character-valued columns with
# as.factor() inside each unit's context, so a context containing only the
# second level used the FIRST level's weight and window.

rs_accum <- function(df, tensor) {
  horizon <- c("Condition", "GroupName");
  rules <- conversation_rules((Condition %in% UNIT$Condition & GroupName %in% UNIT$GroupName));
  ctx <- contexts(df, units_by = c("Condition", "UserName"), hoo_rules = rules,
                  split_rules = function(unit, unit_context) split(unit_context, by = horizon));
  codes <- c('Data','Technical.Constraints','Performance.Parameters',
             'Client.and.Consultant.Requests','Design.Reasoning','Collaboration');
  accumulate(ctx, codes, tensor)
}
sender_tensor <- function(df) {
  t <- context_tensor(df, sender_cols = "GameHalf", receiver_cols = NULL, mode_column = c(),
                      default_window = 4, default_weight = 1);
  nd <- length(dim(t));
  first <- slice.index(t, 1) == 1;                 # the first (sorted) GameHalf level
  t[first & slice.index(t, nd) == 1] <- 0.5;       # weight
  t[first & slice.index(t, nd) == 2] <- 2;         # window
  t
}
counts <- function(accum) unname(as.matrix(accum$connection.counts));

test_that("character-valued and numeric sender columns give identical networks (RS.data)", {
  data("RS.data");
  df_chr <- data.table::as.data.table(RS.data);
  df_num <- data.table::copy(df_chr);
  df_num[, GameHalf := as.numeric(factor(GameHalf, levels = c("First", "Second")))];

  a_chr <- rs_accum(df_chr, sender_tensor(df_chr));
  a_num <- rs_accum(df_num, sender_tensor(df_num));
  expect_equal(counts(a_chr), counts(a_num));

  # Every unit context here holds a single GameHalf level, so the "First"
  # weight/window must not leak into contexts that only contain "Second".
  a_default <- rs_accum(df_chr, context_tensor(df_chr, sender_cols = "GameHalf", receiver_cols = NULL,
                                               mode_column = c(), default_window = 4, default_weight = 1));
  expect_false(isTRUE(all.equal(counts(a_chr), counts(a_default))));
})

# Two conversations, each with a single sender level. With level "a" weighted
# 0 and "b" weighted 1, only conversation "2" (all "b") contributes.
toy <- data.frame(
  u = c("x", "x", "y", "y"),
  c = c("1", "1", "2", "2"),
  s = c("a", "a", "b", "b"),
  A = c(1, 1, 1, 1),
  B = c(1, 1, 1, 1)
);
toy_rules <- conversation_rules((c %in% UNIT$c));
toy_accum <- function(df) {
  ctx <- contexts(df, units_by = "u", hoo_rules = toy_rules);
  t <- context_tensor(df, sender_cols = "s", receiver_cols = NULL, mode_column = c(),
                      default_window = 2, default_weight = 1);
  nd <- length(dim(t));
  t[slice.index(t, 1) == 1 & slice.index(t, nd) == 1] <- 0;   # level 1 ("a" / 0) weight 0
  accumulate(ctx, c("A", "B"), t)
}

test_that("a context with only the second level uses that level's weight", {
  a <- toy_accum(toy);
  cc <- as.matrix(a$connection.counts);
  rownames(cc) <- as.character(a$connection.counts$ENA_UNIT);
  expect_equal(unname(cc["x", ]), 0);        # "a" rows: weight 0
  expect_gt(unname(cc["y", ]), 0);           # "b" rows: weight 1
})

test_that("a numeric sender column coded 0/1 is looked up by level, not used as an index", {
  toy01 <- toy;
  toy01$s <- ifelse(toy$s == "a", 0, 1);
  expect_equal(counts(toy_accum(toy01)), counts(toy_accum(toy)));
})

test_that("values missing from the tensor's levels give a clear error", {
  ctx <- contexts(toy, units_by = "u", hoo_rules = toy_rules);
  t <- context_tensor(toy[toy$s == "a", ], sender_cols = "s", receiver_cols = NULL, mode_column = c(),
                      default_window = 2, default_weight = 1);
  expect_error(accumulate(ctx, c("A", "B"), t), "not levels of the tensor");
})
