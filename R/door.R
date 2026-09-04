# =============================================================================
# door.R — Apply the ETM door function to co-occurrence trajectories
# =============================================================================

COOCCURRENCE_NAME_REGEX <- "\\s*&\\s*"

cooccurrence_pair_column_names <- function(data) {
  nm <- names(data)
  is_pair <- grepl(COOCCURRENCE_NAME_REGEX, nm)
  is_num <- vapply(data, is.numeric, logical(1))
  nm[is_pair & is_num]
}

prefix_door_columns <- function(df_door, original_names) {
  stopifnot(ncol(df_door) == length(original_names))
  out <- as.data.frame(df_door, stringsAsFactors = FALSE)
  names(out) <- paste0("door_", original_names)
  out
}

attach_door_columns <- function(source_rows, cols_of_interest, door_only) {
  meta <- source_rows[, setdiff(names(source_rows), cols_of_interest), drop = FALSE]
  cbind(meta, door_only)
}

ensure_ena_unit_column <- function(data) {
  if ("ENA_UNIT" %in% names(data)) {
    return(data)
  }
  if ("QEUNIT" %in% names(data)) {
    data$ENA_UNIT <- as.character(data$QEUNIT)
    return(data)
  }
  data
}

assert_door_keys <- function(data, unit_col, time_col) {
  if (!unit_col %in% names(data)) {
    stop("`unit_col` not found: ", unit_col, ". Available: ",
         paste(names(data), collapse = ", "))
  }
  if (!time_col %in% names(data)) {
    stop("`time_col` not found in `data`: ", time_col,
         ". Include a monotonic row index in accumulation units.")
  }
  if (!is.character(data[[unit_col]]) && !is.factor(data[[unit_col]])) {
    stop("`unit_col` must be character or factor.")
  }
  if (!(is.numeric(data[[time_col]]) ||
        inherits(data[[time_col]], c("Date", "POSIXct", "POSIXt")))) {
    stop("`time_col` must be numeric, Date, or POSIXct. Convert before calling.")
  }
  if (any(is.na(data[[unit_col]]))) {
    bad <- which(is.na(data[[unit_col]]))[seq_len(min(5L, sum(is.na(data[[unit_col]]))))]
    stop("`unit_col` has NA (e.g. rows: ", paste(bad, collapse = ", "), ").")
  }
  if (any(is.na(data[[time_col]]))) {
    bad <- which(is.na(data[[time_col]]))[seq_len(min(5L, sum(is.na(data[[time_col]]))))]
    stop("`time_col` has NA (e.g. rows: ", paste(bad, collapse = ", "), ").")
  }
}

prepare_units_time_ordered <- function(data, unit_col, time_col) {
  data$ENA_UNIT_base <- trimws(as.character(data[[unit_col]]))
  if (!"ENA_UNIT" %in% names(data)) {
    data$ENA_UNIT <- data$ENA_UNIT_base
  }

  do.call(rbind, lapply(split(data, data$ENA_UNIT_base), function(df) {
    tvec <- df[[time_col]]
    if (any(duplicated(tvec))) {
      idx <- which(duplicated(tvec) | duplicated(tvec, fromLast = TRUE))
      show <- head(idx, 5L)
      stop("Duplicate ", time_col, " within unit '", df$ENA_UNIT_base[1L],
           "'. Example row positions: ", paste(show, collapse = ", "),
           ". Use a unique time key per row (e.g. Index).")
    }
    if (is.unsorted(tvec, strictly = TRUE)) {
      badpos <- which(diff(tvec) <= 0L)[1L]
      stop("Non-increasing ", time_col, " within unit '", df$ENA_UNIT_base[1L],
           "' near sorted positions ", badpos, " and ", badpos + 1L, ".")
    }
    df[order(tvec), , drop = FALSE]
  }))
}

.resolve_lookback_per_unit <- function(lookback_size, data, unit_col) {
  units <- unique(trimws(as.character(data[[unit_col]])))
  if (length(lookback_size) == 1L && is.numeric(lookback_size)) {
    return(stats::setNames(rep(as.integer(lookback_size), length(units)), units))
  }
  if (is.data.frame(lookback_size)) {
    uc <- intersect(c(unit_col, "unit", "UserName"), names(lookback_size))[1]
    lc <- intersect(c("lookback", "lookback_size", "door"), names(lookback_size))[1]
    if (is.na(uc) || is.na(lc)) {
      stop("lookback_size data.frame needs unit and lookback columns")
    }
    out <- stats::setNames(
      as.integer(lookback_size[[lc]]),
      as.character(lookback_size[[uc]])
    )
    return(out)
  }
  if (is.numeric(lookback_size) && !is.null(names(lookback_size))) {
    nm <- trimws(names(lookback_size))
    out <- structure(as.integer(lookback_size), names = nm)
    missing <- setdiff(units, nm)
    if (length(missing)) {
      stop("lookback_size missing units: ", paste(head(missing, 5L), collapse = ", "))
    }
    return(out)
  }
  stop("`lookback_size` must be a single integer, a named vector, or a data.frame")
}

#' Apply lookback door sliding window pooling to co-occurrence data
#'
#' @param data Data frame with co-occurrence columns
#' @param cols_of_interest Character vector of code-pair columns
#' @param lookback_size Window lookback size (single integer, named vector, or data.frame)
#' @param unit_col Name of column identifying unit
#' @param aggregate "sum" or "mean"
#' @param weighting "equal" or "linear"
#' @param segment_col Optional segment column to reset window
#' @return Data frame with door_* columns attached
#' @export
lookback_door <- function(
    data,
    cols_of_interest,
    lookback_size = 100L,
    unit_col = "ENA_UNIT_base",
    aggregate = c("sum", "mean"),
    weighting = c("equal", "linear"),
    segment_col = NULL) {
  aggregate <- match.arg(aggregate)
  weighting <- match.arg(weighting)

  if (!is.null(segment_col) && !(segment_col %in% names(data))) {
    stop("`segment_col` not found in `data`: ", segment_col)
  }

  lookback_map <- .resolve_lookback_per_unit(lookback_size, data, unit_col)
  units <- unique(data$ENA_UNIT_base)
  out <- vector("list", length(units))
  ui <- 0L

  for (u in units) {
    u_key <- as.character(u)
    idx <- match(u_key, names(lookback_map))
    if (is.na(idx)) {
      stop("No lookback_size for unit: ", u_key)
    }
    lb_u <- lookback_map[idx]
    unit_data <- as.data.frame(data[data$ENA_UNIT_base == u, , drop = FALSE])

    segments <- if (is.null(segment_col)) {
      list(unit_data)
    } else {
      split(unit_data, unit_data[[segment_col]], drop = TRUE)
    }

    seg_results <- lapply(segments, function(seg_df) {
      M <- as.matrix(seg_df[, cols_of_interest, drop = FALSE])
      n <- nrow(M)
      if (n == 0L) return(NULL)

      # Use libqe C++ acceleration if available
      segs_vec <- if (!is.null(segment_col)) as.integer(as.factor(seg_df[[segment_col]])) else integer(0)
      S <- libqe::door_lookback_block(
        block = M,
        lookback_size = as.integer(lb_u),
        aggregate_mean = (aggregate == "mean"),
        weighting_linear = (weighting == "linear"),
        segment_ids = segs_vec
      )

      door_mat <- prefix_door_columns(as.data.frame(S), cols_of_interest)
      attach_door_columns(seg_df, cols_of_interest, door_mat)
    })

    ui <- ui + 1L
    out[[ui]] <- do.call(rbind, seg_results)
  }

  do.call(rbind, out)
}

#' Apply EMA smoothing to co-occurrence data
#'
#' @param data Data frame with co-occurrence columns
#' @param cols_of_interest Character vector of code-pair columns
#' @param alpha Smoothing factor (0, 1]
#' @return Data frame with door_* columns attached
#' @export
ema_door <- function(data, cols_of_interest, alpha = 0.3) {
  mat_by_unit <- function(unit_data) {
    mat <- as.matrix(unit_data[, cols_of_interest, drop = FALSE])
    n <- nrow(mat)
    if (n == 0L) return(unit_data)

    out <- libqe::door_ema_block(block = mat, alpha = as.numeric(alpha))
    door_only <- prefix_door_columns(as.data.frame(out), cols_of_interest)
    attach_door_columns(unit_data, cols_of_interest, door_only)
  }

  units <- unique(data$ENA_UNIT_base)
  pieces <- lapply(units, function(u) {
    mat_by_unit(as.data.frame(data[data$ENA_UNIT_base == u, , drop = FALSE]))
  })
  do.call(rbind, pieces)
}

#' Apply LOESS smoothing to co-occurrence data
#'
#' @param data Data frame with co-occurrence columns
#' @param cols_of_interest Character vector of code-pair columns
#' @param time_col Strictly increasing time column
#' @param span LOESS span parameter
#' @param degree LOESS degree parameter (1 or 2)
#' @param family "gaussian" or "symmetric"
#' @return Data frame with door_* columns attached
#' @export
loess_door <- function(
    data,
    cols_of_interest,
    time_col,
    span = 0.3,
    degree = 1L,
    family = c("gaussian", "symmetric")) {
  family <- match.arg(family)
  units <- unique(data$ENA_UNIT_base)
  out <- vector("list", length(units))
  ui <- 0L

  for (u in units) {
    unit_data <- as.data.frame(data[data$ENA_UNIT_base == u, , drop = FALSE])
    x <- as.numeric(unit_data[[time_col]])
    n <- length(x)
    S <- matrix(NA_real_, nrow = n, ncol = length(cols_of_interest))

    for (j in seq_along(cols_of_interest)) {
      y <- unit_data[[cols_of_interest[j]]]
      if (all(is.na(y))) {
        S[, j] <- NA_real_
        next
      }
      fit <- tryCatch({
        stats::predict(
          stats::loess(
            y ~ x,
            span = span,
            degree = degree,
            family = family,
            control = stats::loess.control(
              surface = "direct",
              trace.hat = "approximate",
              iterations = 1L
            ),
            na.action = stats::na.exclude
          ),
          newdata = data.frame(x = x)
        )
      }, error = function(e) rep(NA_real_, n))
      S[, j] <- as.numeric(fit)
    }

    door_only <- prefix_door_columns(as.data.frame(S), cols_of_interest)
    ui <- ui + 1L
    out[[ui]] <- attach_door_columns(unit_data, cols_of_interest, door_only)
  }

  do.call(rbind, out)
}

.normalize_door_method <- function(method) {
  method <- as.character(method)
  allowed <- c("Lookback", "EMA", "LOESS")
  bad <- setdiff(method, allowed)
  if (length(bad)) {
    stop(
      "Unknown method: ", paste(bad, collapse = ", "),
      ". Use: Lookback, EMA, LOESS"
    )
  }
  unique(method)
}

.apply_post_norm <- function(res, pair_cols, post_norm) {
  if (post_norm != "l2") {
    return(res)
  }
  door_cols <- paste0("door_", pair_cols)
  door_cols <- door_cols[door_cols %in% names(res)]
  if (!length(door_cols)) {
    return(res)
  }
  M <- as.matrix(res[, door_cols, drop = FALSE])
  res[, door_cols] <- libqe::normalize_networks(M)
  res
}

.run_door_method <- function(
    data,
    method,
    pair_cols,
    time_col,
    unit_col,
    lookback_size,
    aggregate,
    weighting,
    segment_col,
    alpha,
    loess_span,
    loess_degree,
    loess_family,
    post_norm,
    verbose) {
  if (isTRUE(verbose)) {
    if (method == "Lookback") {
      lb_msg <- if (length(lookback_size) == 1L && is.numeric(lookback_size)) {
        as.character(lookback_size)
      } else {
        paste0("per-unit (", length(.resolve_lookback_per_unit(lookback_size, data, unit_col)), " units)")
      }
      message(
        "Lookback: lookback_size=", lb_msg,
        ", aggregate=", aggregate, ", weighting=", weighting,
        ", segment_col=", if (is.null(segment_col)) "NULL" else segment_col
      )
    } else if (method == "EMA") {
      message("EMA: alpha=", alpha)
    } else if (method == "LOESS") {
      message(
        "LOESS: span=", loess_span,
        ", degree=", loess_degree, ", family=", loess_family
      )
    }
  }

  res <- switch(
    method,
    Lookback = lookback_door(
      data,
      cols_of_interest = pair_cols,
      lookback_size = lookback_size,
      unit_col = unit_col,
      aggregate = aggregate,
      weighting = weighting,
      segment_col = segment_col
    ),
    EMA = ema_door(
      data,
      cols_of_interest = pair_cols,
      alpha = alpha
    ),
    LOESS = loess_door(
      data,
      cols_of_interest = pair_cols,
      time_col = time_col,
      span = loess_span,
      degree = loess_degree,
      family = loess_family
    ),
    stop("Unknown method: ", method)
  )

  .apply_post_norm(res, pair_cols, post_norm)
}

#' Apply the ETM door function to code-pair co-occurrence rows
#'
#' @param data Data frame from \code{accum$model$row.connection.counts}. If only
#'   \code{QEUNIT} is present (TMA default), it is copied to \code{ENA_UNIT} for
#'   alignment with \code{replace_door()}.
#' @param method One or more of \code{"Lookback"}, \code{"EMA"}, \code{"LOESS"}.
#' @param unit_col Column grouping the trajectory (e.g. \code{"UserName"}).
#' @param time_col Strictly increasing time column within each unit (e.g. \code{"Index"}).
#' @param post_norm \code{"none"} or \code{"l2"} row normalization on \code{door_*}
#'   columns after pooling.
#' @param lookback_size Lookback only — max rows in the backward pool. A single integer
#'   applies to all units; a named vector or data.frame (\code{unit} + \code{lookback})
#'   sets per-unit lookback sizes.
#' @param aggregate Lookback only — \code{"sum"} or \code{"mean"}.
#' @param weighting Lookback only — \code{"equal"} or \code{"linear"} ramp.
#' @param segment_col Lookback only — if set, the pool resets at each new segment level.
#' @param alpha EMA only — blend weight on the current row (0–1).
#' @param loess_span,loess_degree,loess_family LOESS only — passed to \code{stats::loess}.
#' @param verbose Print method settings.
#' @return A data frame when \code{length(method) == 1}; otherwise a named list of
#'   data frames.
#' @export
door <- function(
    data,
    method = "Lookback",
    unit_col,
    time_col,
    post_norm = c("none", "l2"),
    lookback_size = 100L,
    aggregate = c("sum", "mean"),
    weighting = c("equal", "linear"),
    segment_col = NULL,
    alpha = 0.3,
    loess_span = 0.3,
    loess_degree = 1L,
    loess_family = c("gaussian", "symmetric"),
    verbose = FALSE) {
  post_norm <- match.arg(post_norm)
  aggregate <- match.arg(aggregate)
  weighting <- match.arg(weighting)
  loess_family <- match.arg(loess_family)

  if (inherits(data, "data.table")) {
    data <- as.data.frame(data)
  }
  data <- ensure_ena_unit_column(data)

  assert_door_keys(data, unit_col, time_col)
  data <- prepare_units_time_ordered(data, unit_col, time_col)

  pair_cols <- cooccurrence_pair_column_names(data)
  if (length(pair_cols) == 0L) {
    stop("No numeric code-pair columns (names matching ' & ') found.")
  }

  methods <- .normalize_door_method(method)

  if (length(methods) == 1L) {
    return(.run_door_method(
      data = data,
      method = methods[[1L]],
      pair_cols = pair_cols,
      time_col = time_col,
      unit_col = unit_col,
      lookback_size = lookback_size,
      aggregate = aggregate,
      weighting = weighting,
      segment_col = segment_col,
      alpha = alpha,
      loess_span = loess_span,
      loess_degree = loess_degree,
      loess_family = loess_family,
      post_norm = post_norm,
      verbose = verbose
    ))
  }

  results <- vector("list", length(methods))
  names(results) <- methods
  for (m in methods) {
    results[[m]] <- .run_door_method(
      data = data,
      method = m,
      pair_cols = pair_cols,
      time_col = time_col,
      unit_col = unit_col,
      lookback_size = lookback_size,
      aggregate = aggregate,
      weighting = weighting,
      segment_col = segment_col,
      alpha = alpha,
      loess_span = loess_span,
      loess_degree = loess_degree,
      loess_family = loess_family,
      post_norm = post_norm,
      verbose = verbose
    )
  }
  results
}
