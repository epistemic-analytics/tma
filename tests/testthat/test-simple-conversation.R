testthat::context("Test updated get context approach")

# d <- tma::test_data
# d <- rbind(d, d)
# d$Row <- seq(1:nrow(d))
# d$Day <- c(rep(1, 6), rep(2, 6))
# d[7, "Context"] <- "Public"
# d[8, "Context"] <- "Private"
#
# d <- data.table::data.table(d);
# d_format <- "%m/%d/%y %H:%M";
# converted_dates <- as.POSIXct(d$Timestamp[1:6], format = d_format);
# pushed_dates <- as.POSIXct(d$Timestamp[7:12], format = d_format) + 3600;
# d$Timestamp <- c(converted_dates, pushed_dates);

test_that("verify results", {
  data(test_rsdata)
  RS.data <- test_rsdata
  RS.data$Context <- "Public"
  RS.data$Row <- seq(nrow(RS.data))
  
  units_by <- c("Condition", "UserName");
  hoo_rules_old = list(
    list(Condition = "<UNIT>", UserName = "<UNIT>"),
    list(Condition = "<UNIT>", GroupName = "<UNIT>", Context = "Public")
  )
  hoo_rules = list(
    list(Condition = TRUE, GroupName = TRUE, Context = "Public")
    # ,list(Condition = "<UNIT>", UserName = "<UNIT>")
  )
  rs_contexts_tables <- get_contexts(RS.data, units_by = c("Condition", "UserName"), rules = hoo_rules, indices.only = FALSE)
  all_units_rs <- unique(RS.data[, units_by])
  rs_contexts_old <- get.contexts(all_units_rs, data = as.data.frame(RS.data), hoo.rules = hoo_rules_old);

  testthat::expect_true(all(sapply(names(rs_contexts_tables$contexts), function(n) {
    all.equal(data.frame(rs_contexts_tables$contexts[[n]]$context), rs_contexts_old$contexts[[n]]$context, check.attributes = FALSE)
  })))
})

# bench_results <- microbenchmark::microbenchmark(
#   rs_contexts <- get_contexts(RS.data, units_by = c("Condition", "UserName"), rules = hoo_rules, indices.only = FALSE),
#   rs_contexts_old <- get.contexts(all_units_rs, data = as.data.frame(RS.data), hoo.rules = hoo_rules_old),
#   times = 10
# )
# print(bench_results)