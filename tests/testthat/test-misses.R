testthat::context("Test accumulate misses (missed co-occurrences)");

# Same toy as test-weight-by.R: one unit, window 3, weight 1.
#   row 1: A=2 B=0 C=1   ground = 0                 -> no misses
#   row 2: A=1 B=3 C=0   ground = (2,0,1); C absent -> A->C 2, C->C 1
#   row 3: A=0 B=1 C=2   ground = (3,3,1); A absent -> A->A 3, B->A 3, C->A 1
# Directed layout ground i -> response j at j*n+i (column-major n x n).
toy <- data.frame(
  u = c("x", "x", "x"),
  c = c("1", "1", "1"),
  A = c(2, 1, 0),
  B = c(0, 3, 1),
  C = c(1, 0, 2)
);
toy_codes <- c("A", "B", "C");
toy_rules <- conversation_rules((c %in% UNIT$c));

toy_accum <- function(...) {
  ctx <- contexts(toy, units_by = "u", hoo_rules = toy_rules);
  tensor <- context_tensor(toy, sender_cols = NULL, receiver_cols = NULL,
                           default_window = 3, default_weight = 1);
  accumulate(context_model = ctx, codes = toy_codes, tensor = tensor, ...)
}
co_cols <- function(x) unname(as.matrix(x[, sapply(x, is, "ena.co.occurrence"), with = FALSE]));
unordered_cols <- function(x) unname(as.matrix(x[, c("A & B", "A & C", "B & C"), with = FALSE]));

row_misses <- rbind(c(0, 0, 0, 0, 0, 0, 0, 0, 0),
                    c(0, 0, 0, 0, 0, 0, 2, 0, 1),
                    c(3, 3, 1, 0, 0, 0, 0, 0, 0));

test_that("apply_tensor(misses = TRUE) returns ground-present / response-absent counts", {
  codes <- as.matrix(toy[, toy_codes]);
  r <- apply_tensor(tensor = c(1, 3), dims = 2L, dims_sender = integer(0),
                    dims_receiver = integer(0), dims_mode = integer(0),
                    context_lookup = matrix(0L, 3, 0), unit_rows = 0:2,
                    codes = codes, times = c(0, 1, 2), ordered = TRUE, misses = TRUE);
  expect_equal(r$row_miss_counts, row_misses);
  expect_equal(as.numeric(r$miss_counts), colSums(row_misses));

  plain <- apply_tensor(tensor = c(1, 3), dims = 2L, dims_sender = integer(0),
                        dims_receiver = integer(0), dims_mode = integer(0),
                        context_lookup = matrix(0L, 3, 0), unit_rows = 0:2,
                        codes = codes, times = c(0, 1, 2), ordered = TRUE);
  expect_null(plain$row_miss_counts);
  expect_equal(plain$row_connection_counts, r$row_connection_counts);
})

test_that("accumulate(misses = TRUE) stores row and unit misses; hits unchanged", {
  m <- toy_accum(ordered = TRUE, misses = TRUE);
  expect_equal(co_cols(m$model$row.miss.counts), row_misses);
  expect_equal(co_cols(m$miss.counts)[1, ], colSums(row_misses));
  expect_equal(colnames(m$miss.counts), colnames(m$connection.counts));
  expect_true(m$`_function.params`$misses);

  plain <- toy_accum(ordered = TRUE);
  expect_null(plain$miss.counts);
  expect_equal(co_cols(m$connection.counts), co_cols(plain$connection.counts));
})

test_that("weight_by applies to misses per line", {
  m <- toy_accum(ordered = TRUE, misses = TRUE, weight_by = "sqrt");
  expect_equal(co_cols(m$miss.counts)[1, ], colSums(sqrt(row_misses)));
  expect_equal(co_cols(m$model$row.miss.counts), row_misses);  # rows stay raw
})

# Unordered: fold each line (i & j = i->j + j->i, diagonal dropped), then weight.
#   row 2: A->C 2         -> A&C 2
#   row 3: B->A 3, C->A 1 -> A&B 3, A&C 1
# Columns A & B, A & C, B & C.
test_that("unordered misses fold per line like connection counts", {
  m <- toy_accum(misses = TRUE, weight_by = "product");
  expect_equal(unordered_cols(m$miss.counts)[1, ], c(3, 3, 0));
  expect_equal(co_cols(m$model$row.miss.counts),
               rbind(c(0, 0, 0), c(0, 2, 0), c(3, 1, 0)));
  expect_equal(colnames(m$miss.counts), colnames(m$connection.counts));

  expect_equal(unordered_cols(toy_accum(misses = TRUE)$miss.counts)[1, ], c(1, 2, 0));  # binary per line
  expect_equal(unordered_cols(toy_accum(misses = TRUE, weight_by = "sqrt")$miss.counts)[1, ],
               c(sqrt(3), sqrt(2) + 1, 0));

  plain <- toy_accum(weight_by = "product");
  expect_equal(unordered_cols(m$connection.counts), unordered_cols(plain$connection.counts));
  expect_equal(class(m$miss.counts), class(plain$connection.counts));
})
