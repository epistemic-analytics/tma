context("test-trajectory")

test_that("tma_fit_poly_curve and derivatives work accurately", {
  # Generate quadratic trajectory x(t) = t, y(t) = t^2
  t_vals <- seq(0, 1, length.out = 21)
  pts <- data.frame(
    RowID = seq_along(t_vals),
    MR1 = t_vals,
    MR2 = t_vals^2
  )

  curve <- tma_fit_poly_curve(pts, x_col = "MR1", y_col = "MR2", max_degree = 2L, fit_curve_with_zero_pts = TRUE)
  expect_equal(curve$degree, 2L)

  prof <- tma_poly_derivative_profile(curve, n_eval = 50L)
  expect_equal(nrow(prof), 50L)

  # Check velocity at t = 0.5: dx/dt = 1, dy/dt = 2*t = 1.0
  mid_idx <- 25L
  expect_equal(prof$dx[mid_idx], 1.0, tolerance = 1e-3)
  expect_equal(prof$dy[mid_idx], 2 * prof$t[mid_idx], tolerance = 1e-3)
})

test_that("tma_curve_distance computes integrated distance", {
  pts_a <- data.frame(
    RowID = 1:5,
    MR1 = c(0, 0.25, 0.5, 0.75, 1.0),
    MR2 = c(0, 0, 0, 0, 0)
  )
  pts_b <- data.frame(
    RowID = 1:5,
    MR1 = c(0, 0.25, 0.5, 0.75, 1.0),
    MR2 = c(1, 1, 1, 1, 1)
  )

  c_a <- tma_fit_poly_curve(pts_a, x_col = "MR1", y_col = "MR2", degree = 1L, fit_curve_with_zero_pts = TRUE)
  c_b <- tma_fit_poly_curve(pts_b, x_col = "MR1", y_col = "MR2", degree = 1L, fit_curve_with_zero_pts = TRUE)

  dist <- tma_curve_distance(c_a, c_b)
  expect_equal(dist, 1.0, tolerance = 1e-5)
})

test_that("turn lag sweep identifies best following lag", {
  # Leader: y=0, x follows t
  # Follower: trails leader by 2 turns
  times <- 1:10
  pts_ldr <- data.frame(index = times, MR1 = times * 1.0, MR2 = rep(0, 10))
  pts_fol <- data.frame(index = times, MR1 = (times - 2) * 1.0, MR2 = rep(0, 10))

  res <- tma_sweep_signed_turn_lags(pts_fol, pts_ldr, x_col = "MR1", y_col = "MR2", time_col = "index", max_lag = 4)
  expect_equal(res$best_lag, 2L)
  expect_equal(res$min_mean_distance, 0.0, tolerance = 1e-6)
})
