testthat::context("Test accumulate weight_by");

# One unit, one conversation, window 3; code counts > 1 so the weight models
# differ. Reference values are rENA's legacy ena.accumulate.data(weight.by = ...)
# with window.size.back = 3 (connection order A & B, A & C, B & C).
toy <- data.frame(
  u = c("x", "x", "x"),
  c = c("1", "1", "1"),
  A = c(2, 1, 0),
  B = c(0, 3, 1),
  C = c(1, 0, 2)
);
toy_codes <- c("A", "B", "C");
toy_rules <- conversation_rules((c == UNIT$c));

toy_accum <- function(...) {
  ctx <- contexts(toy, units_by = "u", hoo_rules = toy_rules);
  tensor <- context_tensor(toy, sender_cols = NULL, receiver_cols = NULL,
                           default_window = 3, default_weight = 1);
  accumulate(context_model = ctx, codes = toy_codes, tensor = tensor, ...)
}
unit_counts <- function(accum) unname(as.matrix(accum$connection.counts)[1, ]);

test_that("weight_by matches rENA ena.accumulate.data weight.by (unordered)", {
  expect_equal(unit_counts(toy_accum(weight_by = "binary")),  c(2, 3, 2));
  expect_equal(unit_counts(toy_accum(weight_by = "product")), c(12, 9, 12));
  expect_equal(unit_counts(toy_accum(weight_by = "sqrt")),
               c(4.73205080756888, 4.86370330515627, 4.73205080756888), tolerance = 1e-12);
  expect_equal(unit_counts(toy_accum(weight_by = "log1p")),
               c(3.68887945411394, 3.73766961828337, 3.68887945411394), tolerance = 1e-12);
  expect_equal(unit_counts(toy_accum(weight_by = "log")), unit_counts(toy_accum(weight_by = "log1p")));
})

test_that("weight_by defaults follow the binary flag", {
  expect_equal(unit_counts(toy_accum()),                unit_counts(toy_accum(weight_by = "binary")));
  expect_equal(unit_counts(toy_accum(binary = FALSE)),  unit_counts(toy_accum(weight_by = "product")));
  expect_equal(toy_accum(weight_by = "sqrt")$`_function.params`$weight_by, "sqrt");
  expect_equal(toy_accum(binary = FALSE)$`_function.params`$weight_by, "product");
})

test_that("weight_by rejects unknown or non-string values", {
  expect_error(toy_accum(weight_by = "cube"), "Unknown weight model");
  expect_error(toy_accum(weight_by = sqrt), "must be one of");
})

test_that("row.connection.counts stay raw regardless of weight_by", {
  raw <- toy_accum(weight_by = "product")$model$row.connection.counts;
  expect_equal(toy_accum(weight_by = "sqrt")$model$row.connection.counts, raw);
  expect_equal(toy_accum(weight_by = "binary")$model$row.connection.counts, raw);
})

test_that("ordered: binary keeps raw directed counts; sqrt / log1p apply per directed cell", {
  rcc <- as.matrix(toy_accum(ordered = TRUE)$model$row.connection.counts);
  expect_equal(unit_counts(toy_accum(ordered = TRUE, weight_by = "binary")),  unname(colSums(rcc)));
  expect_equal(unit_counts(toy_accum(ordered = TRUE, weight_by = "product")), unname(colSums(rcc)));
  expect_equal(unit_counts(toy_accum(ordered = TRUE, weight_by = "sqrt")),    unname(colSums(sqrt(rcc))));
  expect_equal(unit_counts(toy_accum(ordered = TRUE, weight_by = "log1p")),   unname(colSums(log1p(rcc))));
})

test_that("weight_by applies per line on RS.data (sqrt of each line, not of the unit total)", {
  data("RS.data");
  rs_codes <- c('Data','Technical.Constraints','Performance.Parameters',
                'Client.and.Consultant.Requests','Design.Reasoning','Collaboration');
  # Same context setup as rENA::accumulate(): `%in%` (a unit can span several
  # groups, so UNIT$GroupName may hold more than one value) and one context per
  # conversation via split_rules.
  horizon <- c("Condition", "GroupName");
  rs_rules <- conversation_rules(
    (Condition %in% UNIT$Condition & GroupName %in% UNIT$GroupName)
  );
  ctx <- contexts(RS.data, units_by = c("Condition", "UserName"), hoo_rules = rs_rules,
                  split_rules = function(unit, unit_context) split(unit_context, by = horizon));
  tensor <- context_tensor(RS.data, sender_cols = NULL, receiver_cols = NULL,
                           default_window = 4, default_weight = 1);
  prod <- accumulate(ctx, rs_codes, tensor, weight_by = "product");
  sq   <- accumulate(ctx, rs_codes, tensor, weight_by = "sqrt");

  # Per-line reference from the (folded, raw) row counts: sqrt each line, sum by unit.
  rows <- sq$model$row.connection.counts;
  conn_cols <- setdiff(colnames(rows), c("QEID", "QEUNIT", "Condition", "UserName"));
  expected <- rows[, lapply(.SD, function(x) sum(sqrt(x))), by = "QEUNIT", .SDcols = conn_cols];
  expect_equal(unname(as.matrix(sq$connection.counts)), unname(as.matrix(expected[, ..conn_cols])));

  # and it is not sqrt of the product totals
  expect_false(isTRUE(all.equal(as.matrix(sq$connection.counts), sqrt(as.matrix(prod$connection.counts)))));
})
