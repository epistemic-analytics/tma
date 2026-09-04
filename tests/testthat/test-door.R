context("test-door")

test_that("lookback door pools co-occurrences correctly", {
  df <- data.frame(
    UserName = c("A", "A", "A", "A", "B", "B"),
    Index = c(1, 2, 3, 4, 1, 2),
    `code1 & code2` = c(1, 0, 1, 0, 0, 1),
    `code2 & code3` = c(0, 1, 0, 1, 1, 0),
    check.names = FALSE
  )

  res <- door(
    data = df,
    method = "Lookback",
    unit_col = "UserName",
    time_col = "Index",
    lookback_size = 2L,
    aggregate = "sum",
    weighting = "equal"
  )

  expect_true("door_code1 & code2" %in% names(res))
  expect_true("door_code2 & code3" %in% names(res))

  # Unit A, lookback 2 sum:
  # row 1 (idx 1): c12=1, c23=0
  # row 2 (idx 2): c12=1+0=1, c23=0+1=1
  # row 3 (idx 3): c12=0+1=1, c23=1+0=1
  # row 4 (idx 4): c12=1+0=1, c23=0+1=1
  expect_equal(res$`door_code1 & code2`[1:4], c(1, 1, 1, 1))
  expect_equal(res$`door_code2 & code3`[1:4], c(0, 1, 1, 1))
})

test_that("ema door applies exponential smoothing", {
  df <- data.frame(
    UserName = c("A", "A", "A", "A"),
    Index = c(1, 2, 3, 4),
    `code1 & code2` = c(1, 0, 1, 0),
    check.names = FALSE
  )

  res <- door(
    data = df,
    method = "EMA",
    unit_col = "UserName",
    time_col = "Index",
    alpha = 0.5
  )

  expect_equal(res$`door_code1 & code2`[1], 1.0)
  expect_equal(res$`door_code1 & code2`[2], 0.5)
  expect_equal(res$`door_code1 & code2`[3], 0.75)
  expect_equal(res$`door_code1 & code2`[4], 0.375)
})

test_that("replace_door updates connection counts in accumulation object", {
  conn <- data.table::data.table(
    QEUNIT = c("A", "B"),
    `code1 & code2` = c(0, 0),
    check.names = FALSE
  )
  accum <- list(connection.counts = conn)

  door_df <- data.frame(
    ENA_UNIT = c("A", "B"),
    `door_code1 & code2` = c(5, 10),
    check.names = FALSE
  )

  updated <- replace_door(accum, door_df)
  expect_equal(as.numeric(updated$connection.counts$`code1 & code2`), c(5, 10))
})

test_that("door_accumulation runs door and replacement in one step", {
  conn <- data.table::data.table(
    QEUNIT = c("A", "A", "A"),
    UserName = c("A", "A", "A"),
    Index = c(1, 2, 3),
    `code1 & code2` = c(1, 0, 1),
    check.names = FALSE
  )
  accum <- list(connection.counts = conn)

  updated <- door_accumulation(
    accum,
    method = "Lookback",
    unit_col = "UserName",
    time_col = "Index",
    lookback_size = 2L,
    aggregate = "sum",
    weighting = "equal"
  )

  expect_equal(as.numeric(updated$connection.counts$`code1 & code2`), c(1, 1, 1))
})
