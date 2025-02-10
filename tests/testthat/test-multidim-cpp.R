# TODO: make sure to change the below to deal with accum_multidim_c
testthat::context("Test C++ accumulation with multidimensional array")
data(test_mockdata, package = "tma")
mock_data <- test_mockdata;

test_that("no senders, no receivers, modality", {
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # Initialize TMA units and contexts
  unit_cols <- c("userID", "condition")
  codes <- c("A", "B", "C")
  HOO_rules_model <- tma:::rules(
    modality %in% "chat" & chatGroup %in% UNIT$chatGroup & condition %in% UNIT$condition, modality %in% "resource" & userID %in% UNIT$userID & condition %in% UNIT$condition
  )
  
  context_model <- tma:::contexts(
    x = mock_data,
    units = unit_cols,
    hoo_rules = HOO_rules_model
  )
  
  time_column = "timeStamp"
  
  # Get new TMA results 
  mode_column = "modality"
  
  multidim_arr <- make_multidim_arr(mock_data, sender_cols = c(), receiver_cols = c(), mode_column, 0, 0)
  
  multidim_arr["chat", "weight"] <- 1
  multidim_arr["resource", "weight"] <- 2
  multidim_arr["chat", "window"] <- 360
  multidim_arr["resource", "window"] <- 180

  result_old <- accum_multidim(
    mock_data,
    multidim_arr, 
    time_column,
    codes,
    context_model
  )
  result_new <- accum_multidim_c(
    context_model = context_model, multidim_arr = multidim_arr, time_column = time_column, codes = codes
  )
  
  # Compare vectors across all 3 units 
  
  user1_old <- as.numeric(unname(as.vector(result_old$connection.counts[1,4:length(colnames(result_old$connection.counts))])))
  user1_new <- as.numeric(unname(as.vector(result_new$connection.counts[1,4:length(colnames(result_new$connection.counts))])))
  user2_old <- as.numeric(unname(as.vector(result_old$connection.counts[2,4:length(colnames(result_old$connection.counts))])))
  user2_new <- as.numeric(unname(as.vector(result_new$connection.counts[2,4:length(colnames(result_new$connection.counts))])))
  user3_old <- as.numeric(unname(as.vector(result_old$connection.counts[3,4:length(colnames(result_old$connection.counts))])))
  user3_new <- as.numeric(unname(as.vector(result_new$connection.counts[3,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_equal(user1_old, user1_new)
  testthat::expect_equal(user2_old, user2_new)
  testthat::expect_equal(user3_old, user3_new)
})

test_that("sender, no receivers, no modality", {
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # Initialize TMA units and contexts
  unit_cols <- c("userID", "condition")
  codes <- c("A", "B", "C")
  HOO_rules_model <- tma:::rules(
    modality %in% "chat" & chatGroup %in% UNIT$chatGroup & condition %in% UNIT$condition, modality %in% "resource" & userID %in% UNIT$userID & condition %in% UNIT$condition
  )
  
  context_model <- tma:::contexts(
    x = mock_data,
    units = unit_cols,
    hoo_rules = HOO_rules_model
  )
  
  time_column = "timeStamp"
  
  
  # Get new TMA results
  sender_cols = c("role")
  
  multidim_arr <- make_multidim_arr(mock_data, sender_cols = sender_cols, receiver_cols = list(), mode_column = list(), 0, 0)
  
  multidim_arr["Mentor", "weight"] <- 1
  multidim_arr["Player", "weight"] <- 2
  multidim_arr["Mentor", "window"] <- 360
  multidim_arr["Player", "window"] <- 180
  
  result_old <- accum_multidim(
    mock_data,
    multidim_arr, 
    time_column,
    codes,
    context_model
  )
  result_new <- accum_multidim_c(
    context_model = context_model, multidim_arr = multidim_arr, time_column = time_column, codes = codes
  )
  
  # Compare vectors across all 3 units 
  
  user1_old <- as.numeric(unname(as.vector(result_old$connection.counts[1,4:length(colnames(result_old$connection.counts))])))
  user1_new <- as.numeric(unname(as.vector(result_new$connection.counts[1,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user1_old == user1_new))
  
  user2_old <- as.numeric(unname(as.vector(result_old$connection.counts[2,4:length(colnames(result_old$connection.counts))])))
  user2_new <- as.numeric(unname(as.vector(result_new$connection.counts[2,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user2_old == user2_new))
  
  user3_old <- as.numeric(unname(as.vector(result_old$connection.counts[3,4:length(colnames(result_old$connection.counts))])))
  user3_new <- as.numeric(unname(as.vector(result_new$connection.counts[3,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user3_old == user3_new))
})

test_that("no senders, receiver, no modality", {
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # Initialize TMA units and contexts
  unit_cols <- c("userID", "condition")
  codes <- c("A", "B", "C")
  HOO_rules_model <- tma:::rules(
    modality %in% "chat" & chatGroup %in% UNIT$chatGroup & condition %in% UNIT$condition, modality %in% "resource" & userID %in% UNIT$userID & condition %in% UNIT$condition
  )
  
  context_model <- tma:::contexts(
    x = mock_data,
    units = unit_cols,
    hoo_rules = HOO_rules_model
  )
  
  time_column = "timeStamp"
  
  
  # Get new TMA results
  receiver_cols = c("language")
  
  multidim_arr <- make_multidim_arr(mock_data, sender_cols = list(), receiver_cols = receiver_cols, mode_column = list(), 0, 0)
  
  multidim_arr["L1", "weight"] <- 1
  multidim_arr["L2", "weight"] <- 5
  multidim_arr["L1", "window"] <- 360
  multidim_arr["L2", "window"] <- 180
  
  result_old <- accum_multidim(
    mock_data,
    multidim_arr, 
    time_column,
    codes,
    context_model
  )
  result_new <- accum_multidim_c(
    context_model = context_model, multidim_arr = multidim_arr, time_column = time_column, codes = codes
  )
  
  # Compare vectors across all 3 units 
  
  user1_old <- as.numeric(unname(as.vector(result_old$connection.counts[1,4:length(colnames(result_old$connection.counts))])))
  user1_new <- as.numeric(unname(as.vector(result_new$connection.counts[1,4:length(colnames(result_new$connection.counts))])))
  
  user2_old <- as.numeric(unname(as.vector(result_old$connection.counts[2,4:length(colnames(result_old$connection.counts))])))
  user2_new <- as.numeric(unname(as.vector(result_new$connection.counts[2,4:length(colnames(result_new$connection.counts))])))
  
  user3_old <- as.numeric(unname(as.vector(result_old$connection.counts[3,4:length(colnames(result_old$connection.counts))])))
  user3_new <- as.numeric(unname(as.vector(result_new$connection.counts[3,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_equal(user1_old, user1_new)
  testthat::expect_equal(user2_old, user2_new)
  testthat::expect_equal(user3_old, user3_new)
  
})

test_that("sender, no receiver, modality", {
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # Initialize TMA units and contexts
  unit_cols <- c("userID", "condition")
  codes <- c("A", "B", "C")
  HOO_rules_model <- tma:::rules(
    modality %in% "chat" & chatGroup %in% UNIT$chatGroup & condition %in% UNIT$condition, modality %in% "resource" & userID %in% UNIT$userID & condition %in% UNIT$condition
  )
  
  context_model <- tma:::contexts(
    x = mock_data,
    units = unit_cols,
    hoo_rules = HOO_rules_model
  )
  
  time_column = "timeStamp"
  
  # Get new TMA results
  sender_cols = c("role")
  mode_column = "modality"
  
  multidim_arr <- make_multidim_arr(mock_data, sender_cols = sender_cols, receiver_cols = list(), mode_column = mode_column, 0, 0)
  
  multidim_arr["Mentor", "chat", "weight"] <- 3
  multidim_arr["Mentor", "resource", "weight"] <- 1
  multidim_arr["Player", "chat", "weight"] <- 2
  multidim_arr["Player", "resource", "weight"] <- 5
  
  multidim_arr["Mentor", "chat", "window"] <- 180
  multidim_arr["Mentor", "resource", "window"] <- 60
  multidim_arr["Player", "chat", "window"] <- 180
  multidim_arr["Player", "resource", "window"] <- 360
  
  
  result_old <- accum_multidim(
    mock_data,
    multidim_arr, 
    time_column,
    codes,
    context_model
  )
  result_new <- accum_multidim_c(
    context_model = context_model, multidim_arr = multidim_arr, time_column = time_column, codes = codes
  )
  
  
  
  # Compare vectors across all 3 units 
  
  user1_old <- as.numeric(unname(as.vector(result_old$connection.counts[1,4:length(colnames(result_old$connection.counts))])))
  user1_new <- as.numeric(unname(as.vector(result_new$connection.counts[1,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user1_old == user1_new))
  
  user2_old <- as.numeric(unname(as.vector(result_old$connection.counts[2,4:length(colnames(result_old$connection.counts))])))
  user2_new <- as.numeric(unname(as.vector(result_new$connection.counts[2,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user2_old == user2_new))
  
  user3_old <- as.numeric(unname(as.vector(result_old$connection.counts[3,4:length(colnames(result_old$connection.counts))])))
  user3_new <- as.numeric(unname(as.vector(result_new$connection.counts[3,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user3_old == user3_new))
  
  
})

test_that("no sender, receiver, modality", {
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # Initialize TMA units and contexts
  unit_cols <- c("userID", "condition")
  codes <- c("A", "B", "C")
  HOO_rules_model <- tma:::rules(
    modality %in% "chat" & chatGroup %in% UNIT$chatGroup & condition %in% UNIT$condition, modality %in% "resource" & userID %in% UNIT$userID & condition %in% UNIT$condition
  )
  
  context_model <- tma:::contexts(
    x = mock_data,
    units = unit_cols,
    hoo_rules = HOO_rules_model
  )
  
  time_column = "timeStamp"
  
  # Get new TMA results
  receiver_cols = c("language")
  mode_column = "modality"
  
  multidim_arr <- make_multidim_arr(mock_data, sender_cols = list(), receiver_cols = receiver_cols, mode_column = mode_column, 0, 0)
  
  multidim_arr["L1", "chat", "weight"] <- 5
  multidim_arr["L1", "resource", "weight"] <- 1
  multidim_arr["L2", "chat", "weight"] <- 3
  multidim_arr["L2", "resource", "weight"] <- 2
  
  multidim_arr["L1", "chat", "window"] <- 180
  multidim_arr["L1", "resource", "window"] <- 360
  multidim_arr["L2", "chat", "window"] <- 60
  multidim_arr["L2", "resource", "window"] <- 120
  
  result_old <- accum_multidim(
    mock_data,
    multidim_arr, 
    time_column,
    codes,
    context_model
  )
  result_new <- accum_multidim_c(
    context_model = context_model, multidim_arr = multidim_arr, time_column = time_column, codes = codes
  )
  
  # Compare vectors across all 3 units 
  
  user1_old <- as.numeric(unname(as.vector(result_old$connection.counts[1,4:length(colnames(result_old$connection.counts))])))
  user1_new <- as.numeric(unname(as.vector(result_new$connection.counts[1,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user1_old == user1_new))
  
  user2_old <- as.numeric(unname(as.vector(result_old$connection.counts[2,4:length(colnames(result_old$connection.counts))])))
  user2_new <- as.numeric(unname(as.vector(result_new$connection.counts[2,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user2_old == user2_new))
  
  user3_old <- as.numeric(unname(as.vector(result_old$connection.counts[3,4:length(colnames(result_old$connection.counts))])))
  user3_new <- as.numeric(unname(as.vector(result_new$connection.counts[3,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user3_old == user3_new))
  
})

test_that("sender, receiver, no modality", {
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # Initialize TMA units and contexts
  unit_cols <- c("userID", "condition")
  codes <- c("A", "B", "C")
  HOO_rules_model <- tma:::rules(
    modality %in% "chat" & chatGroup %in% UNIT$chatGroup & condition %in% UNIT$condition, modality %in% "resource" & userID %in% UNIT$userID & condition %in% UNIT$condition
  )
  
  context_model <- tma:::contexts(
    x = mock_data,
    units = unit_cols,
    hoo_rules = HOO_rules_model
  )
  
  time_column = "timeStamp"
  
  
  
  # Get new TMA results
  sender_cols = c("role")
  receiver_cols = c("language")
  
  multidim_arr <- make_multidim_arr(mock_data, sender_cols = sender_cols, receiver_cols = receiver_cols, mode_column = list(), 0, 0)
  
  multidim_arr["Mentor", "L1", "weight"] <- 1
  multidim_arr["Mentor", "L2", "weight"] <- 5
  multidim_arr["Player", "L1", "weight"] <- 3
  multidim_arr["Player", "L2", "weight"] <- 1
  
  multidim_arr["Mentor", "L1", "window"] <- 360
  multidim_arr["Mentor", "L2", "window"] <- 180
  multidim_arr["Player", "L1", "window"] <- 250
  multidim_arr["Player", "L2", "window"] <- 200
  
  result_old <- accum_multidim(
    mock_data,
    multidim_arr, 
    time_column,
    codes,
    context_model
  )
  result_new <- accum_multidim_c(
    context_model = context_model, multidim_arr = multidim_arr, time_column = time_column, codes = codes
  )
  
  
  
  # Compare vectors across all 3 units 
  
  user1_old <- as.numeric(unname(as.vector(result_old$connection.counts[1,4:length(colnames(result_old$connection.counts))])))
  user1_new <- as.numeric(unname(as.vector(result_new$connection.counts[1,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user1_old == user1_new))
  
  user2_old <- as.numeric(unname(as.vector(result_old$connection.counts[2,4:length(colnames(result_old$connection.counts))])))
  user2_new <- as.numeric(unname(as.vector(result_new$connection.counts[2,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user2_old == user2_new))
  
  user3_old <- as.numeric(unname(as.vector(result_old$connection.counts[3,4:length(colnames(result_old$connection.counts))])))
  user3_new <- as.numeric(unname(as.vector(result_new$connection.counts[3,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user3_old == user3_new))
})

test_that("sender, receiver, modality", {
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # Initialize TMA units and contexts
  unit_cols <- c("userID", "condition")
  codes <- c("A", "B", "C")
  HOO_rules_model <- tma:::rules(
    modality %in% "chat" & chatGroup %in% UNIT$chatGroup & condition %in% UNIT$condition, modality %in% "resource" & userID %in% UNIT$userID & condition %in% UNIT$condition
  )
  
  context_model <- tma:::contexts(
    x = mock_data,
    units = unit_cols,
    hoo_rules = HOO_rules_model
  )
  
  time_column = "timeStamp"
  
  # Get new TMA results
  sender_cols = c("role")
  receiver_cols = c("language")
  mode_column = "modality"
  
  multidim_arr <- make_multidim_arr(mock_data, sender_cols = sender_cols, receiver_cols = receiver_cols, mode_column = mode_column, 0, 0)
  
  multidim_arr["Mentor", "L1", "chat", "weight"] <- 5
  multidim_arr["Mentor", "L1", "resource", "weight"] <- 5
  multidim_arr["Mentor", "L2", "chat", "weight"] <- 7
  multidim_arr["Mentor", "L2", "resource", "weight"] <- 2
  
  multidim_arr["Player", "L1", "chat", "weight"] <- 3
  multidim_arr["Player", "L1", "resource", "weight"] <- 5
  multidim_arr["Player", "L2", "chat", "weight"] <- 5
  multidim_arr["Player", "L2", "resource", "weight"] <- 1
  
  multidim_arr["Mentor", "L1", "chat", "window"] <- 800
  multidim_arr["Mentor", "L1", "resource", "window"] <- 500
  multidim_arr["Mentor", "L2", "chat", "window"] <- 20
  multidim_arr["Mentor", "L2", "resource", "window"] <- 310
  
  multidim_arr["Player", "L1", "chat", "window"] <- 320
  multidim_arr["Player", "L1", "resource", "window"] <- 200
  multidim_arr["Player", "L2", "chat", "window"] <- 150
  multidim_arr["Player", "L2", "resource", "window"] <- 360
  
  result_old <- accum_multidim(
    mock_data,
    multidim_arr, 
    time_column,
    codes,
    context_model
  )
  result_new <- accum_multidim_c(
    context_model = context_model, multidim_arr = multidim_arr, time_column = time_column, codes = codes
  )
  
  # Compare vectors across all 3 units 
  
  user1_old <- as.numeric(unname(as.vector(result_old$connection.counts[1,4:length(colnames(result_old$connection.counts))])))
  user1_new <- as.numeric(unname(as.vector(result_new$connection.counts[1,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user1_old == user1_new))
  
  user2_old <- as.numeric(unname(as.vector(result_old$connection.counts[2,4:length(colnames(result_old$connection.counts))])))
  user2_new <- as.numeric(unname(as.vector(result_new$connection.counts[2,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user2_old == user2_new))
  
  user3_old <- as.numeric(unname(as.vector(result_old$connection.counts[3,4:length(colnames(result_old$connection.counts))])))
  user3_new <- as.numeric(unname(as.vector(result_new$connection.counts[3,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user3_old == user3_new))
  
})

test_that("senders, receivers, modality", {
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # Initialize TMA units and contexts
  unit_cols <- c("userID", "condition")
  codes <- c("A", "B", "C")
  HOO_rules_model <- tma:::rules(
    modality %in% "chat" & chatGroup %in% UNIT$chatGroup & condition %in% UNIT$condition, modality %in% "resource" & userID %in% UNIT$userID & condition %in% UNIT$condition
  )
  
  context_model <- tma:::contexts(
    x = mock_data,
    units = unit_cols,
    hoo_rules = HOO_rules_model
  )
  
  time_column = "timeStamp"
  
  # Get new TMA results
  sender_cols <- c("role", "language")
  receiver_cols <- c("role", "handwriting")
  mode_column <- "modality"
  
  multidim_arr <- make_multidim_arr(mock_data, sender_cols = sender_cols, receiver_cols = receiver_cols, mode_column = mode_column, 0, 0)
  
  
  multidim_arr["Player", "L1", "Player", "poor", "chat", "weight"] <- 1
  multidim_arr["Player", "L1", "Player", "poor", "resource", "weight"] <- 1
  multidim_arr["Player", "L1", "Player", "good", "chat", "weight"] <- 1
  multidim_arr["Player", "L1", "Player", "good", "resource", "weight"] <- 8
  multidim_arr["Player", "L1", "Mentor", "poor", "chat", "weight"] <- 1
  multidim_arr["Player", "L1", "Mentor", "poor", "resource", "weight"] <- 1
  multidim_arr["Player", "L1", "Mentor", "good", "chat", "weight"] <- 1
  multidim_arr["Player", "L1", "Mentor", "good", "resource", "weight"] <- 7
  multidim_arr["Player", "L2", "Player", "poor", "chat", "weight"] <- 7
  multidim_arr["Player", "L2", "Player", "poor", "resource", "weight"] <- 4
  multidim_arr["Player", "L2", "Player", "good", "chat", "weight"] <- 1
  multidim_arr["Player", "L2", "Player", "good", "resource", "weight"] <- 1
  multidim_arr["Player", "L2", "Mentor", "poor", "chat", "weight"] <- 1
  multidim_arr["Player", "L2", "Mentor", "poor", "resource", "weight"] <- 3
  multidim_arr["Player", "L2", "Mentor", "good", "chat", "weight"] <- 3
  multidim_arr["Player", "L2", "Mentor", "good", "resource", "weight"] <- 12
  multidim_arr["Mentor", "L1", "Player", "poor", "chat", "weight"] <- 1
  multidim_arr["Mentor", "L1", "Player", "poor", "resource", "weight"] <- 10
  multidim_arr["Mentor", "L1", "Player", "good", "chat", "weight"] <- 1
  multidim_arr["Mentor", "L1", "Player", "good", "resource", "weight"] <- 9
  multidim_arr["Mentor", "L1", "Mentor", "poor", "chat", "weight"] <- 1
  multidim_arr["Mentor", "L1", "Mentor", "poor", "resource", "weight"] <- 1
  multidim_arr["Mentor", "L1", "Mentor", "good", "chat", "weight"] <- 7
  multidim_arr["Mentor", "L1", "Mentor", "good", "resource", "weight"] <- 7
  multidim_arr["Mentor", "L2", "Player", "poor", "chat", "weight"] <- 7
  multidim_arr["Mentor", "L2", "Player", "poor", "resource", "weight"] <- 1
  multidim_arr["Mentor", "L2", "Player", "good", "chat", "weight"] <- 6
  multidim_arr["Mentor", "L2", "Player", "good", "resource", "weight"] <- 2
  multidim_arr["Mentor", "L2", "Mentor", "poor", "chat", "weight"] <- 1
  multidim_arr["Mentor", "L2", "Mentor", "poor", "resource", "weight"] <- 3
  multidim_arr["Mentor", "L2", "Mentor", "good", "chat", "weight"] <- 5
  multidim_arr["Mentor", "L2", "Mentor", "good", "resource", "weight"] <- 5
  
  multidim_arr["Player", "L1", "Player", "poor", "chat", "window"] <- 360
  multidim_arr["Player", "L1", "Player", "poor", "resource", "window"] <- 10 
  multidim_arr["Player", "L1", "Player", "good", "chat", "window"] <- 360
  multidim_arr["Player", "L1", "Player", "good", "resource", "window"] <- 310
  multidim_arr["Player", "L1", "Mentor", "poor", "chat", "window"] <- 360
  multidim_arr["Player", "L1", "Mentor", "poor", "resource", "window"] <- 300
  multidim_arr["Player", "L1", "Mentor", "good", "chat", "window"] <- 360
  multidim_arr["Player", "L1", "Mentor", "good", "resource", "window"] <- 320
  multidim_arr["Player", "L2", "Player", "poor", "chat", "window"] <- 360
  multidim_arr["Player", "L2", "Player", "poor", "resource", "window"] <- 350
  multidim_arr["Player", "L2", "Player", "good", "chat", "window"] <- 360
  multidim_arr["Player", "L2", "Player", "good", "resource", "window"] <- 189
  multidim_arr["Player", "L2", "Mentor", "poor", "chat", "window"] <- 121
  multidim_arr["Player", "L2", "Mentor", "poor", "resource", "window"] <- 782
  multidim_arr["Player", "L2", "Mentor", "good", "chat", "window"] <- 333
  multidim_arr["Player", "L2", "Mentor", "good", "resource", "window"] <- 343
  multidim_arr["Mentor", "L1", "Player", "poor", "chat", "window"] <- 845
  multidim_arr["Mentor", "L1", "Player", "poor", "resource", "window"] <- 800
  multidim_arr["Mentor", "L1", "Player", "good", "chat", "window"] <- 500
  multidim_arr["Mentor", "L1", "Player", "good", "resource", "window"] <- 20
  multidim_arr["Mentor", "L1", "Mentor", "poor", "chat", "window"] <- 70
  multidim_arr["Mentor", "L1", "Mentor", "poor", "resource", "window"] <- 10
  multidim_arr["Mentor", "L1", "Mentor", "good", "chat", "window"] <- 20
  multidim_arr["Mentor", "L1", "Mentor", "good", "resource", "window"] <- 100
  multidim_arr["Mentor", "L2", "Player", "poor", "chat", "window"] <- 360
  multidim_arr["Mentor", "L2", "Player", "poor", "resource", "window"] <- 200
  multidim_arr["Mentor", "L2", "Player", "good", "chat", "window"] <- 360
  multidim_arr["Mentor", "L2", "Player", "good", "resource", "window"] <- 350
  multidim_arr["Mentor", "L2", "Mentor", "poor", "chat", "window"] <- 360
  multidim_arr["Mentor", "L2", "Mentor", "poor", "resource", "window"] <- 270
  multidim_arr["Mentor", "L2", "Mentor", "good", "chat", "window"] <- 180
  multidim_arr["Mentor", "L2", "Mentor", "good", "resource", "window"] <- 360
  
  
  result_old <- accum_multidim(
    mock_data,
    multidim_arr, 
    time_column,
    codes,
    context_model
  )
  result_new <- accum_multidim_c(
    context_model = context_model, multidim_arr = multidim_arr, time_column = time_column, codes = codes
  )
  
  # Compare vectors accross all 3 units 
  
  # Compare vectors across all 3 units 
  
  user1_old <- as.numeric(unname(as.vector(result_old$connection.counts[1,4:length(colnames(result_old$connection.counts))])))
  user1_new <- as.numeric(unname(as.vector(result_new$connection.counts[1,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user1_old == user1_new))
  
  user2_old <- as.numeric(unname(as.vector(result_old$connection.counts[2,4:length(colnames(result_old$connection.counts))])))
  user2_new <- as.numeric(unname(as.vector(result_new$connection.counts[2,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user2_old == user2_new))
  
  user3_old <- as.numeric(unname(as.vector(result_old$connection.counts[3,4:length(colnames(result_old$connection.counts))])))
  user3_new <- as.numeric(unname(as.vector(result_new$connection.counts[3,4:length(colnames(result_new$connection.counts))])))
  
  testthat::expect_true(all(user3_old == user3_new))
})
