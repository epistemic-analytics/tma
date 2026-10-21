# Reference for python/tests/test_contexts_split.py: R tma's accumulate() on
# data whose conversations are interleaved and whose units span several
# conversations -- unsplit (one context per unit; time = row id within the
# context) and split by conversation (split_rules) -- for unordered binary,
# unordered product and ordered product, windows 2 and 3.
# From the repo root:  Rscript python/tests/fixtures/contexts-split.R
suppressMessages(devtools::load_all(quiet = TRUE))
set.seed(7)
n <- 40
toy <- data.frame(
  u = sample(c("ann", "bob", "cat", "dan"), n, replace = TRUE),
  c = sample(c("k1", "k2", "k3"), n, replace = TRUE),   # interleaved conversations
  A = rbinom(n, 1, 0.5), B = rbinom(n, 1, 0.5), C = rbinom(n, 1, 0.4)
)
codes <- c("A", "B", "C")
run <- function(split, window, ...) {
  ctx <- if (split) {
    contexts(toy, units_by = "u", hoo_rules = conversation_rules((c %in% UNIT$c)),
             split_rules = function(unit, unit_context) split(unit_context, by = "c"))
  } else {
    contexts(toy, units_by = "u", hoo_rules = conversation_rules((c %in% UNIT$c)))
  }
  acc <- accumulate(ctx, codes, context_tensor(toy, NULL, NULL, default_window = window,
                                               default_weight = 1), misses = FALSE, ...)
  cc <- as.matrix(acc$connection.counts)
  list(units = as.character(acc$meta.data$u), counts = unname(cc[, colnames(cc) != "QEUNIT"]))
}
cases <- list()
for (split in c(FALSE, TRUE)) for (window in c(2, 3)) {
  for (variant in list(list(name = "binary", args = list()),
                       list(name = "product", args = list(weight_by = "product")),
                       list(name = "ordered", args = list(ordered = TRUE, weight_by = "product")))) {
    key <- paste(if (split) "split" else "unsplit", window, variant$name, sep = "_")
    cases[[key]] <- do.call(run, c(list(split = split, window = window), variant$args))
  }
}
jsonlite::write_json(list(toy = toy, codes = codes, cases = cases),
                     "python/tests/fixtures/contexts-split.json", digits = NA, auto_unbox = TRUE,
                     dataframe = "columns", matrix = "rowmajor", pretty = TRUE)
