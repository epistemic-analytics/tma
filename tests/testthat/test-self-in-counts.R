testthat::context("Test self_connections = TRUE (self-connections in connection.counts)");

# Same toy as test-self-connections.R: unit x, window 3, weight 1.
sc_toy <- data.frame(
  u = c("x", "x", "x", "y", "y", "z", "w", "w"),
  c = c("1", "1", "1", "2", "2", "3", "4", "4"),
  A = c(2, 1, 0, 1, 1, 1, 0, 1),
  B = c(0, 3, 1, 1, 0, 1, 1, 1),
  C = c(1, 0, 2, 0, 1, 0, 1, 0)
);
sc_codes <- c("A", "B", "C");
diag_cols <- c("A & A", "A & B", "B & B", "A & C", "B & C", "C & C");
sc_accum <- function(...) {
  ctx <- contexts(sc_toy, units_by = "u", hoo_rules = conversation_rules((c %in% UNIT$c)));
  tensor <- context_tensor(sc_toy, sender_cols = NULL, receiver_cols = NULL,
                           default_window = 3, default_weight = 1);
  accumulate(context_model = ctx, codes = sc_codes, tensor = tensor, ...)
}
row_x <- function(tbl, cols) unname(unlist(as.data.frame(tbl)[1, cols]));

test_that("adjacency_key with diagonal: A & A, A & B, B & B, ...", {
  expect_equal(colnames(adjacency_key(sc_codes, TRUE, diagonal = TRUE)), diag_cols);
  expect_equal(colnames(adjacency_key(sc_codes, TRUE)), c("A & B", "A & C", "B & C"));
  expect_equal(colnames(adjacency_key(sc_codes, TRUE, diagonal = TRUE)),
               libqe::connection_names(sc_codes, TRUE));
})

test_that("self_connections = TRUE puts the diagonal into connection.counts", {
  a <- sc_accum(weight_by = "product", self_connections = TRUE);
  expect_equal(colnames(a$rotation$adjacency.key), diag_cols);
  expect_true(all(diag_cols %in% colnames(a$connection.counts)));
  expect_equal(row_x(a$connection.counts, diag_cols), c(2, 12, 3, 9, 12, 2));
  expect_true(a$`_function.params`$self_connections);
  # the diagonal columns are the self vector; the pairs are unchanged
  expect_equal(row_x(a$connection.counts, c("A & A", "B & B", "C & C")),
               row_x(a$self.connection.counts, sc_codes));
  plain <- sc_accum(weight_by = "product");
  expect_equal(row_x(a$connection.counts, c("A & B", "A & C", "B & C")),
               row_x(plain$connection.counts, c("A & B", "A & C", "B & C")));
  # rows: raw per line, same layout
  expect_equal(tail(colnames(a$model$row.connection.counts), 6), diag_cols);
})

test_that("weight_by applies to the self columns per line, like the others", {
  for (w in c("binary", "sqrt")) {
    a <- sc_accum(weight_by = w, self_connections = TRUE);
    expect_equal(row_x(a$connection.counts, c("A & A", "B & B", "C & C")),
                 row_x(a$self.connection.counts, sc_codes), info = w);
  }
  expect_equal(row_x(sc_accum(self_connections = TRUE)$connection.counts, diag_cols),
               c(1, 2, 1, 3, 2, 1));   # binary
})

test_that("misses get the diagonal too; default and ordered are unchanged", {
  a <- sc_accum(weight_by = "product", self_connections = TRUE);
  expect_equal(row_x(a$miss.counts, c("A & A", "B & B", "C & C")),
               row_x(a$self.miss.counts, sc_codes));
  expect_false(sc_accum()$`_function.params`$self_connections);
  expect_equal(colnames(sc_accum()$connection.counts), colnames(sc_accum(self_connections = FALSE)$connection.counts));
  o1 <- sc_accum(ordered = TRUE, weight_by = "product", self_connections = TRUE);
  o0 <- sc_accum(ordered = TRUE, weight_by = "product");
  expect_equal(as.data.frame(o1$connection.counts), as.data.frame(o0$connection.counts));
  expect_false(o1$`_function.params`$self_connections);
})
