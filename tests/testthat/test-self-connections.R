testthat::context("Test self-connections (ground i x response i)");

# One unit (x), window 3, weight 1 -- the test-misses.R toy plus three units
# in their own conversations.
#   row 2: ground (2,0,1), response (1,3,0) -> self A 2;  C absent -> self-miss C 1
#   row 3: ground (3,3,1), response (0,1,2) -> self B 3, C 2; A absent -> self-miss A 3
self_toy <- data.frame(
  u = c("x", "x", "x", "y", "y", "z", "w", "w"),
  c = c("1", "1", "1", "2", "2", "3", "4", "4"),
  A = c(2, 1, 0, 1, 1, 1, 0, 1),
  B = c(0, 3, 1, 1, 0, 1, 1, 1),
  C = c(1, 0, 2, 0, 1, 0, 1, 0)
);
self_codes <- c("A", "B", "C");

self_accum <- function(...) {
  ctx <- contexts(self_toy, units_by = "u", hoo_rules = conversation_rules((c %in% UNIT$c)));
  tensor <- context_tensor(self_toy, sender_cols = NULL, receiver_cols = NULL,
                           default_window = 3, default_weight = 1);
  accumulate(context_model = ctx, codes = self_codes, tensor = tensor, ...)
}
unit_x <- function(tbl) unname(unlist(as.data.frame(tbl)[1, self_codes]));

test_that("finalize/aggregate_self_connections read each row's diagonal", {
  rc <- rbind(c(1, 2, 3, 4), c(5, 0, 0, 2));   # 2 codes: diagonal at 1 and 4
  expect_equal(finalize_self_connections(rc, 2L, TRUE, "product"), rbind(c(1, 4), c(5, 2)));
  expect_equal(finalize_self_connections(rc, 2L, FALSE, "binary"), rbind(c(1, 1), c(1, 1)));
  expect_equal(finalize_self_connections(rc, 2L, TRUE, "binary"), rbind(c(1, 4), c(5, 2)));  # ordered: raw
  expect_equal(aggregate_self_connections(rc, 2L, FALSE, "sqrt"), c(1 + sqrt(5), 2 + sqrt(2)));
  expect_error(finalize_self_connections(rc, 3L, TRUE, "product"), "n_codes\\^2");
})

test_that("self.connection.counts per unit, weighted per line", {
  expect_equal(unit_x(self_accum(weight_by = "product")$self.connection.counts), c(2, 3, 2));
  expect_equal(unit_x(self_accum(weight_by = "binary")$self.connection.counts),  c(1, 1, 1));
  expect_equal(unit_x(self_accum(weight_by = "sqrt")$self.connection.counts),    c(sqrt(2), sqrt(3), sqrt(2)));
  # ordered "binary" keeps raw counts, as for connection counts
  expect_equal(unit_x(self_accum(ordered = TRUE)$self.connection.counts),        c(2, 3, 2));
})

test_that("ordered self-connections equal the diagonal of the ordered connection counts", {
  a <- self_accum(ordered = TRUE, weight_by = "product");
  cc <- as.data.frame(a$connection.counts);
  self <- as.data.frame(a$self.connection.counts);
  expect_equal(unname(as.matrix(self[, self_codes])),
               unname(as.matrix(cc[, c("A & A", "B & B", "C & C")])));
})

test_that("row.self.connection.counts are raw per line", {
  rows <- as.data.frame(self_accum(weight_by = "sqrt")$model$row.self.connection.counts);
  expect_equal(unname(as.matrix(rows[rows$QEUNIT == "x", self_codes])),
               rbind(c(0, 0, 0), c(2, 0, 0), c(0, 3, 2)));
})

test_that("self.connection.counts rows and columns line up with connection.counts", {
  a <- self_accum();
  expect_equal(as.data.frame(a$self.connection.counts)$ENA_UNIT,
               as.data.frame(a$connection.counts)$ENA_UNIT);
  expect_equal(tail(colnames(a$self.connection.counts), 3), self_codes);
  expect_s3_class(a$self.connection.counts, "self.connections");
})

test_that("self-misses with misses = TRUE only", {
  expect_null(self_accum()$self.miss.counts);
  expect_null(self_accum()$model$row.self.miss.counts);
  a <- self_accum(misses = TRUE, weight_by = "product");
  expect_equal(unit_x(a$self.miss.counts), c(3, 0, 1));
  expect_equal(unit_x(self_accum(misses = TRUE)$self.miss.counts), c(1, 0, 1));   # binary
  rows <- as.data.frame(a$model$row.self.miss.counts);
  expect_equal(unname(as.matrix(rows[rows$QEUNIT == "x", self_codes])),
               rbind(c(0, 0, 0), c(0, 0, 1), c(3, 0, 0)));
})

test_that("self-connections do not change the connection counts", {
  # Same accumulation either way; the self vectors are only added alongside.
  a <- self_accum(weight_by = "product");
  expect_equal(unname(unlist(as.data.frame(a$connection.counts)[1, c("A & B", "A & C", "B & C")])),
               c(12, 9, 12));
})
