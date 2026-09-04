# =============================================================================
# tma_stability.R — Hyperparameter stability recommendation grid
# =============================================================================

#' Evaluate hyperparameter stability grid for door pooling
#'
#' Computes pairwise trajectory distance matrices across a grid of lookback sizes
#' or alpha values, then computes the matrix correlation between adjacent parameter
#' settings to find the "elbow" where trajectories stabilize.
#'
#' @param accum Object from \code{tma::accumulate()}.
#' @param unit_col Name of column identifying unit/speaker.
#' @param time_col Name of column identifying monotonic time.
#' @param param_type "lookback" or "ema".
#' @param param_values Numeric vector of parameter values to evaluate.
#' @param x_col Column name for x coordinate (default auto).
#' @param y_col Column name for y coordinate (default auto).
#' @param max_degree Polynomial degree for trajectory curves (default 3).
#' @return Data frame of parameter values and stability correlations.
#' @export
tma_stability_grid <- function(
    accum,
    unit_col,
    time_col,
    param_type = c("lookback", "ema"),
    param_values = NULL,
    x_col = NULL,
    y_col = NULL,
    max_degree = 3L) {
  param_type <- match.arg(param_type)

  if (is.null(param_values)) {
    param_values <- if (param_type == "lookback") {
      seq(5L, 50L, by = 5L)
    } else {
      seq(0.05, 0.5, by = 0.05)
    }
  }

  raw_df <- accum$model$row.connection.counts
  if (is.null(raw_df)) {
    raw_df <- accum$connection.counts
  }

  pair_cols <- cooccurrence_pair_column_names(raw_df)
  if (!length(pair_cols)) {
    stop("No co-occurrence columns found in accumulation.")
  }

  # Accumulate / replace door for each parameter value
  models <- lapply(param_values, function(pv) {
    d_df <- if (param_type == "lookback") {
      door(
        data = raw_df,
        method = "Lookback",
        unit_col = unit_col,
        time_col = time_col,
        lookback_size = as.integer(pv),
        verbose = FALSE
      )
    } else {
      door(
        data = raw_df,
        method = "EMA",
        unit_col = unit_col,
        time_col = time_col,
        alpha = as.numeric(pv),
        verbose = FALSE
      )
    }

    acc_mod <- replace_door(accum, d_df)
    acc_mod
  })

  # Compute stability correlations between adjacent parameter values
  n_params <- length(param_values)
  corrs <- numeric(n_params)
  corrs[1L] <- 1.0

  if (n_params > 1L) {
    for (i in 2:n_params) {
      m1 <- models[[i - 1L]]$connection.counts
      m2 <- models[[i]]$connection.counts

      # Select numeric connection columns
      c1 <- as.matrix(m1[, pair_cols, with = FALSE])
      c2 <- as.matrix(m2[, pair_cols, with = FALSE])

      corrs[i] <- libqe::dist_dist_correlation(c1, c2)
    }
  }

  data.frame(
    param = param_values,
    correlation = corrs,
    param_type = param_type,
    stringsAsFactors = FALSE
  )
}
