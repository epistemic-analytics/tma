# =============================================================================
# tma_trajectory.R — Trajectory curve fitting, derivatives, distance, and following
# =============================================================================

#' Fit polynomial parametric curve to ETM/TMA points normalized to t in [0, 1]
#'
#' @param pts data.frame or matrix of points
#' @param x_col Column name for x coordinate
#' @param y_col Column name for y coordinate
#' @param time_col Column name for time ordering
#' @param degree Fixed polynomial degree (optional)
#' @param max_degree Maximum degree to evaluate for LOOCV-2D (default 3)
#' @param criterion "loocv" or "aic"
#' @param fit_curve_with_zero_pts Logical; whether to retain (0,0) points
#' @param zero_eps Tolerance for detecting (0,0) points
#' @return List with fitted curve parameters, prediction closures, and metadata
#' @export
tma_fit_poly_curve <- function(
    pts,
    x_col = NULL,
    y_col = NULL,
    time_col = "RowID",
    degree = NULL,
    max_degree = 3L,
    criterion = c("loocv", "aic"),
    fit_curve_with_zero_pts = FALSE,
    zero_eps = 1e-8) {
  criterion <- match.arg(criterion)
  pts <- as.data.frame(pts)

  if (is.null(x_col) || !(x_col %in% names(pts))) {
    x_col <- intersect(c("SVD1", "MR1", "GMR1", "X1", "x"), names(pts))[1]
  }
  if (is.null(y_col) || !(y_col %in% names(pts))) {
    y_col <- intersect(c("SVD2", "MR2", "GMR2", "X2", "y"), names(pts))[1]
  }
  if (is.na(x_col) || is.na(y_col)) {
    stop("Cannot find dimension columns (SVD1/MR1/GMR1 and SVD2/MR2/GMR2) in pts.")
  }

  if (time_col %in% names(pts)) {
    pts <- pts[order(pts[[time_col]]), , drop = FALSE]
  }

  if (!isTRUE(fit_curve_with_zero_pts)) {
    keep <- (abs(pts[[x_col]]) > zero_eps) | (abs(pts[[y_col]]) > zero_eps)
    pts <- pts[keep, , drop = FALSE]
  }

  n <- nrow(pts)
  if (n < 2L) {
    stop("Need at least 2 non-zero points to fit a curve.")
  }

  points_mat <- as.matrix(pts[, c(x_col, y_col), drop = FALSE])
  fixed_deg <- if (!is.null(degree)) as.integer(degree) else 0L

  fit_res <- libqe::fit_trajectory_poly(
    points = points_mat,
    t = numeric(0),
    max_degree = as.integer(max_degree),
    fixed_degree = fixed_deg,
    criterion = criterion
  )

  coeffs_x <- fit_res$coeffs_x
  coeffs_y <- fit_res$coeffs_y

  pred_x <- function(t_new) {
    t_v <- as.numeric(t_new)
    res <- libqe::eval_trajectory_curve(coeffs_x, coeffs_y, t_v)
    res[, 1L]
  }

  pred_y <- function(t_new) {
    t_v <- as.numeric(t_new)
    res <- libqe::eval_trajectory_curve(coeffs_x, coeffs_y, t_v)
    res[, 2L]
  }

  structure(
    list(
      pred_x = pred_x,
      pred_y = pred_y,
      coeffs_x = coeffs_x,
      coeffs_y = coeffs_y,
      degree = fit_res$degree,
      cv_error = fit_res$cv_error,
      aic = fit_res$aic,
      t = fit_res$t,
      fitted_points = fit_res$fitted_points,
      n = n,
      x_col = x_col,
      y_col = y_col,
      time_col = time_col,
      pts = pts
    ),
    class = "tma_poly_curve"
  )
}

#' Evaluate analytical derivatives of a fitted polynomial curve
#'
#' @param curve Output of \code{tma_fit_poly_curve()}
#' @param n_eval Number of evaluation points in [0, 1]
#' @return Data frame of evaluated positions, velocity, speed, acceleration, heading turn rate, curvature
#' @export
tma_poly_derivative_profile <- function(curve, n_eval = 1000L) {
  if (is.null(curve$coeffs_x) || is.null(curve$coeffs_y)) {
    stop("`curve` must be a valid tma_poly_curve object.")
  }
  n_eval <- max(10L, as.integer(n_eval))
  t_eval <- seq(0, 1, length.out = n_eval)

  d <- libqe::eval_trajectory_derivatives(curve$coeffs_x, curve$coeffs_y, t_eval)

  # Also evaluate exact x and y coordinates
  pts <- libqe::eval_trajectory_curve(curve$coeffs_x, curve$coeffs_y, t_eval)

  data.frame(
    t = d$t,
    x = pts[, 1L],
    y = pts[, 2L],
    dx = d$vx,
    dy = d$vy,
    speed = d$speed,
    d2x = d$ax,
    d2y = d$ay,
    omega = d$heading_rate,
    kappa = d$curvature,
    stringsAsFactors = FALSE
  )
}

#' Locate critical points (heading turns, velocity zeros, inflections, curvature peaks)
#'
#' @param profile Output of \code{tma_poly_derivative_profile()}
#' @param speed_tol Minimum speed threshold for heading turns
#' @param zero_tol Numerical zero threshold
#' @param top_curvature Max curvature peaks to return
#' @return List of critical points
#' @export
tma_poly_critical_points <- function(
    profile,
    speed_tol = 1e-6,
    zero_tol = 1e-8,
    top_curvature = 5L) {

  .crossings <- function(t, z, x, y, label, speed = NULL, speed_tol = 1e-6) {
    n <- length(t)
    if (n < 3L) {
      return(data.frame(
        type = character(), t = numeric(), x = numeric(), y = numeric(),
        value = numeric(), stringsAsFactors = FALSE
      ))
    }
    rows <- list()
    for (i in seq_len(n - 1L)) {
      z1 <- z[i]; z2 <- z[i + 1L]
      if (is.na(z1) || is.na(z2)) next
      if (abs(z1) < zero_tol) z1 <- 0
      if (abs(z2) < zero_tol) z2 <- 0
      if (z1 == 0 && z2 == 0) next
      if (z1 * z2 > 0) next
      if (!is.null(speed) && (speed[i] < speed_tol || speed[i + 1L] < speed_tol)) next
      denom <- z2 - z1
      frac <- if (abs(denom) < 1e-15) 0.5 else (-z1) / denom
      frac <- min(max(frac, 0), 1)
      ti <- t[i] + frac * (t[i + 1L] - t[i])
      xi <- x[i] + frac * (x[i + 1L] - x[i])
      yi <- y[i] + frac * (y[i + 1L] - y[i])
      rows[[length(rows) + 1L]] <- data.frame(
        type = label, t = ti, x = xi, y = yi,
        value = 0,
        stringsAsFactors = FALSE
      )
    }
    if (!length(rows)) {
      return(data.frame(
        type = character(), t = numeric(), x = numeric(), y = numeric(),
        value = numeric(), stringsAsFactors = FALSE
      ))
    }
    do.call(rbind, rows)
  }

  inner <- profile$t > 0 & profile$t < 1
  prof <- profile[inner, , drop = FALSE]

  heading_turns <- .crossings(
    prof$t, prof$omega, prof$x, prof$y, "heading_turn",
    speed = prof$speed, speed_tol = speed_tol
  )
  x_velocity_zeros <- .crossings(
    prof$t, prof$dx, prof$x, prof$y, "x_velocity_zero"
  )
  y_velocity_zeros <- .crossings(
    prof$t, prof$dy, prof$x, prof$y, "y_velocity_zero"
  )
  x_inflections <- .crossings(
    prof$t, prof$d2x, prof$x, prof$y, "x_second_deriv_zero"
  )
  y_inflections <- .crossings(
    prof$t, prof$d2y, prof$x, prof$y, "y_second_deriv_zero"
  )

  # Curvature peaks
  ak <- abs(prof$kappa)
  is_peak <- rep(FALSE, nrow(prof))
  if (nrow(prof) >= 3L) {
    for (i in 2:(nrow(prof) - 1L)) {
      if (prof$speed[i] < speed_tol) next
      if (ak[i] > ak[i - 1L] && ak[i] > ak[i + 1L]) is_peak[i] <- TRUE
    }
  }
  idx <- which(is_peak & prof$t > 0 & prof$t < 1)
  curvature_peaks <- if (length(idx) > 0L) {
    ord <- order(ak[idx], decreasing = TRUE)
    idx <- idx[ord]
    top_n <- min(length(idx), as.integer(top_curvature))
    idx <- idx[seq_len(top_n)]
    data.frame(
      type = "curvature_peak",
      t = prof$t[idx],
      x = prof$x[idx],
      y = prof$y[idx],
      value = prof$kappa[idx],
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      type = character(), t = numeric(), x = numeric(), y = numeric(),
      value = numeric(), stringsAsFactors = FALSE
    )
  }

  list(
    heading_turns = heading_turns,
    x_velocity_zeros = x_velocity_zeros,
    y_velocity_zeros = y_velocity_zeros,
    x_inflections = x_inflections,
    y_inflections = y_inflections,
    curvature_peaks = curvature_peaks,
    all = rbind(
      heading_turns, x_velocity_zeros, y_velocity_zeros,
      x_inflections, y_inflections, curvature_peaks
    )
  )
}

#' Integrated Euclidean curve distance between two fitted trajectories
#'
#' @param curve_a First fitted trajectory
#' @param curve_b Second fitted trajectory
#' @param t_start Starting time in [0, 1]
#' @param t_end Ending time in [0, 1]
#' @return Distance value
#' @export
tma_curve_distance <- function(curve_a, curve_b, t_start = 0.0, t_end = 1.0) {
  libqe::integrated_trajectory_distance(
    curve_a$coeffs_x, curve_a$coeffs_y,
    curve_b$coeffs_x, curve_b$coeffs_y,
    as.numeric(t_start), as.numeric(t_end)
  )
}

#' Integrated lagged Euclidean distance between two fitted trajectories
#'
#' @param curve_a First trajectory
#' @param curve_b Second trajectory
#' @param lag Normalized time lag in [0, 1)
#' @param direction "a_follows_b", "b_follows_a", or "symmetric"
#' @return List with distance metrics
#' @export
tma_curve_distance_lag <- function(
    curve_a,
    curve_b,
    lag = 0.0,
    direction = c("a_follows_b", "b_follows_a", "symmetric")) {
  direction <- match.arg(direction)
  lag <- as.numeric(lag)[1L]
  if (lag < 0 || lag >= 1) {
    return(list(D = NA_real_, lag = lag, direction = direction))
  }

  d_ab <- libqe::lagged_trajectory_distance(
    curve_a$coeffs_x, curve_a$coeffs_y,
    curve_b$coeffs_x, curve_b$coeffs_y,
    lag
  )

  d_ba <- libqe::lagged_trajectory_distance(
    curve_b$coeffs_x, curve_b$coeffs_y,
    curve_a$coeffs_x, curve_a$coeffs_y,
    lag
  )

  d_val <- switch(
    direction,
    a_follows_b = d_ab,
    b_follows_a = d_ba,
    symmetric = mean(c(d_ab, d_ba))
  )

  list(
    lag = lag,
    direction = direction,
    D = d_val,
    D_a_follows_b = d_ab,
    D_b_follows_a = d_ba
  )
}

#' Discrete turn-lag distance between point sets of two conversants
#'
#' @param pts_a ETM point rows for agent A
#' @param pts_b ETM point rows for agent B
#' @param x_col Column name for x coordinate
#' @param y_col Column name for y coordinate
#' @param time_col Column name for turn index / timestamp
#' @param delta Signed integer turn lag
#' @return List with mean_distance and valid_count
#' @export
tma_signed_turn_lag <- function(
    pts_a,
    pts_b,
    x_col = "MR1",
    y_col = "MR2",
    time_col = "index",
    delta = 0L) {
  pa <- as.matrix(pts_a[, c(x_col, y_col)])
  pb <- as.matrix(pts_b[, c(x_col, y_col)])
  ta <- as.numeric(pts_a[[time_col]])
  tb <- as.numeric(pts_b[[time_col]])

  libqe::signed_turn_lag(pa, pb, ta, tb, as.integer(delta))
}

#' Discrete turn-lag optimization sweep across signed lags
#'
#' @param pts_a ETM point rows for agent A
#' @param pts_b ETM point rows for agent B
#' @param x_col Column name for x coordinate
#' @param y_col Column name for y coordinate
#' @param time_col Column name for turn index / timestamp
#' @param max_lag Max integer lag to sweep
#' @return List with best_lag, min_mean_distance, lags, and mean_distances
#' @export
tma_sweep_signed_turn_lags <- function(
    pts_a,
    pts_b,
    x_col = "MR1",
    y_col = "MR2",
    time_col = "index",
    max_lag = 15L) {
  pa <- as.matrix(pts_a[, c(x_col, y_col)])
  pb <- as.matrix(pts_b[, c(x_col, y_col)])
  ta <- as.numeric(pts_a[[time_col]])
  tb <- as.numeric(pts_b[[time_col]])

  libqe::sweep_signed_turn_lags(pa, pb, ta, tb, as.integer(max_lag))
}

#' Distance-distance Pearson correlation between two coordinate configurations
#'
#' @param X Matrix of coordinates 1
#' @param Y Matrix of coordinates 2
#' @return Pearson correlation r in [-1, 1]
#' @export
tma_dist_dist_correlation <- function(X, Y) {
  libqe::dist_dist_correlation(as.matrix(X), as.matrix(Y))
}
